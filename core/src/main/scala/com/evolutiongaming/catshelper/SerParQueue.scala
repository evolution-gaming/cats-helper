package com.evolutiongaming.catshelper

import cats.effect.kernel.{Deferred, Ref}
import cats.effect.syntax.all._
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.{Applicative, Monad, Parallel}

import scala.annotation.tailrec
import cats.effect.kernel.Async

/**
  * The idea behind this queue is to parallelize some parts of otherwise serial queue
  * There are two types of tasks: one which has key `defined` - Some(key) (keyful) and those that have `None` (keyless)
  * In case of enqueueing keyless task it will be run after all tasks enqueued before.
  * Also tasks enqueued after will wait until `keyless` task is completed.
  *
  * In case of enqueueing keyful task it will be run after all keyless or same key tasks enqueued before
  * Also keyful tasks or the one with matching key enqueued after will wait until `keyless` task is completed.
  *
  * It also means that enqueued one after another keyful tasks with different keys will be run in parallel
  *
  * In short, this queue enforces:
  * * order per unique key for keyful tasks
  * * global order for keyless tasks
  *
  * Example:
  *   enqueued operations of
  *     (a.some, 0), (none, 1), (a.some, 2), (b.some, 3), (c.some, 4), (none, 5)
  *   will be run in the following order
  *                                 (a.some, 2)
  *     (a.some, 0) => (none, 1) => (b.some, 3) =>  (none, 5)
  *                                 (c.some, 4)
  */
trait SerParQueue[F[_], -K] {

  def apply[A](key: Option[K])(task: F[A]): F[F[A]]
}

object SerParQueue {

  def empty[F[_]: Applicative, K]: SerParQueue[F, K] = new SerParQueue[F, K] {
    def apply[A](key: Option[K])(task: F[A]) = task.map { _.pure[F] }
  }

  def of[F[_]: Async: Parallel, K]: F[SerParQueue[F, K]] = {

    val void = ().pure[F]

    type Task = F[Unit]

    object Task {
      val empty: Task = void
    }

    sealed trait In

    object In {

      sealed trait ParOrEmpty extends In { self =>
        def add(task: Task): Ser = Ser(task, self)
      }

      sealed trait SerOrEmpty extends In { self =>
        def add(key: K, task: Task): Par = In.Par(key, task, self)
      }

      sealed trait ParOrSer extends In

      case object Empty extends ParOrEmpty with SerOrEmpty

      final case class Par(head: Map[K, Task], tail: SerOrEmpty = Empty) extends ParOrEmpty with ParOrSer { self =>

        def add(key: K, task: Task): Par = {
          val tasks = self
            .head
            .getOrElse(key, void)
            .add(task)
          val head = self.head.updated(key, tasks)
          self.copy(head)
        }
      }

      object Par {
        def apply(key: K, task: Task, tail: SerOrEmpty): Par = apply(Map((key, task)), tail)
      }

      final case class Ser(head: Task, tail: ParOrEmpty = Empty) extends SerOrEmpty with ParOrSer {
        def add(task: Task): Ser = copy(head.add(task))
      }
    }

    sealed trait S

    object S {

      val empty: S = Empty

      case object Empty extends S

      final case class Par(in: In = In.Empty, out: Map[K, Task] = Map.empty) extends S

      object Par {
        def apply(key: K): Par = apply(out = Map((key, Task.empty)))
      }

      final case class Ser(in: In = In.Empty, out: Task = Task.empty) extends S
    }

    val stop = ().asRight[Task]

    def combine(in: In.Ser): Task = {

      @tailrec def loop(in: In, task: Task): F[Unit] = {
        in match {
          case In.Empty     => task
          case In.Par(h, t) => loop(t, h.values.toList.parSequence_.add(task))
          case In.Ser(h, t) => loop(t, h.add(task))
        }
      }

      Sync[F].defer { loop(in, Task.empty) }
    }

    Ref[F]
      .of(S.empty)
      .map { ref =>
        new SerParQueue[F, K] {
          def apply[A](key: Option[K])(task0: F[A]) = {

            def start(task: Task)(f: S => (S, F[Either[Task, Unit]])): F[Unit] = {
              task
                .tailRecM { task =>
                  for {
                    _ <- task
                    a <- ref.modify(f)
                    a <- a
                  } yield a
                }
                .start
                .void
            }

            def start0(key: K, task: Task): F[Unit] = start(task) {
              case S.Empty => (S.empty, stop.pure[F]) // should not happen

              case S.Par(in, out) =>
                out.getOrElse(key, Task.empty) match {
                  case Task.empty =>
                    val map = out - key
                    if (map.isEmpty) {
                      in match {
                        case In.Empty =>
                          (S.empty, stop.pure[F])

                        case In.Par(head, In.Empty) =>
                          (S.Ser(In.Par(head)), start1(Task.empty).as(stop))

                        case In.Par(head, tail: In.Ser) =>
                          val task = combine(tail)
                          (S.Ser(In.Par(head)), start1(task).as(stop))

                        case in: In.Ser =>
                          val task = combine(in)
                          (S.Ser(), start1(task).as(stop))
                      }
                    } else {
                      (S.Par(in, map): S, stop.pure[F])
                    }
                  case task =>
                    val state: S = S.Par(in, out.updated(key, Task.empty))
                    (state, task.asLeft[Unit].pure[F])
                }

              case s: S.Ser =>
                (s, stop.pure[F])
            }

            def start1(task: F[Unit]) = start(task) {
              case S.Empty =>
                (S.empty, stop.pure[F]) // should not happen

              case S.Ser(In.Empty, Task.empty) =>
                (S.empty, stop.pure[F])

              case S.Ser(in: In.Ser, Task.empty) =>
                val task = combine(in)
                (S.Ser(), task.asLeft[Unit].pure[F])

              case S.Ser(In.Par(head, In.Empty), Task.empty) =>
                val state = S.Par(out = head.map { case (k, _) => k -> Task.empty })
                val effect = Sync[F].defer {
                  head
                    .foldLeft(void) { case (result, (key, task)) => result *> start0(key, task) }
                    .as(stop)
                }
                (state, effect)

              case S.Ser(In.Par(head, tail: In.Ser), Task.empty) =>
                val task = combine(tail)
                (S.Ser(In.Par(head)), task.asLeft[Unit].pure[F])

              case S.Ser(in, task) =>
                (S.Ser(in), task.asLeft[Unit].pure[F])

              case s: S.Par =>
                (s, stop.pure[F]) // should not happen
            }

            Concurrent[F].uncancelable { _ =>
              for {
                d <- Deferred.apply[F, Either[Throwable, A]]
                task = for {
                  a <- task0.attempt
                  _ <- d.complete(a)
                } yield {}
                a <- key match {
                  case Some(key) =>
                    ref.modify {
                      case S.Empty => (S.Par(key), start0(key, task))

                      case S.Par(In.Empty, out) =>
                        out.get(key) match {
                          case Some(tasks) =>
                            (S.Par(out = out.updated(key, tasks.add(task))), void)
                          case None =>
                            (S.Par(out = out.updated(key, Task.empty)), start0(key, task))
                        }

                      case S.Par(in: In.Par, out)        => (S.Par(in.add(key, task), out), void)
                      case S.Par(in: In.Ser, out)        => (S.Par(in.add(key, task), out), void)
                      case S.Ser(in: In.Par, out)        => (S.Ser(in.add(key, task), out), void)
                      case S.Ser(in: In.SerOrEmpty, out) => (S.Ser(in.add(key, task), out), void)
                    }

                  case None =>
                    ref.modify {
                      case S.Empty                       => (S.Ser(), start1(task))
                      case S.Par(in: In.Ser, out)        => (S.Par(in.add(task), out), void)
                      case S.Par(in: In.ParOrEmpty, out) => (S.Par(in.add(task), out), void)
                      case S.Ser(In.Empty, out)          => (S.Ser(In.Empty, out.add(task)), void)
                      case S.Ser(in: In.Ser, out)        => (S.Ser(in.add(task), out), void)
                      case S.Ser(in: In.Par, out)        => (S.Ser(in.add(task), out), void)
                    }
                }
                _ <- a
              } yield for {
                a <- d.get
                a <- a.liftTo[F]
              } yield a
            }
          }
        }
      }
  }

  private implicit class Ops[F[_], A](val self: F[A]) extends AnyVal {
    def add(task: F[Unit])(implicit F: Monad[F]): F[Unit] = self *> task
  }
}
