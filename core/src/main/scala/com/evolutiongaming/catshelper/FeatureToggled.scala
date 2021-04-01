package com.evolutiongaming.catshelper

import cats.effect.implicits._
import cats.effect.{Concurrent, Resource, Spawn}
import cats.effect.{Deferred, Ref, Temporal}
import cats.syntax.all._

import scala.concurrent.duration._

/**
 * Given a `Resource[F, A]` produces `Resource[F, Resource[F, Option[A]]]` which,
 * while being in `use`, manages lifecycle of `A` according the state of a boolean
 * toggle, providing access to `A` only when the toggle is `true`.
 *
 * @define ra the base resource.
 *
 * @define gracePeriod defines an interval which is counted from the moment the toggle
 *         goes off. During this interval the managed resource `A` is kept alive while
 *         there is at least one "user" active. After `gracePeriod` expires the resource
 *         will be released unconditionally. It is ''up to the client'' to resolve any
 *         errors that may come from a forceful termination of a resource that is still
 *         in use.
 */
object FeatureToggled {

  /**
   * Periodically polls given flag `F[Boolean]` making given resource `ra` available
   * when the flag is `true`.
   *
   * @param ra $ra
   * @param enabled a feature flag
   * @param pollInterval an interval between consecutive polls
   * @param gracePeriod $gracePeriod
   */
  def polling[F[_]: Temporal, A](
    ra: Resource[F, A],
    enabled: F[Boolean],
    pollInterval: FiniteDuration,
    gracePeriod: FiniteDuration = Duration.Zero,
  ): Resource[F, Resource[F, Option[A]]] = {

    of(ra, gracePeriod) { toggle =>
      Schedule(Duration.Zero, pollInterval)(enabled.flatMap(toggle)).use(_ => Spawn[F].never)
    }
  }

  /**
   * Gives you manual control over a feature-toggle. Makes given resource `ra` available
   * while the toggle is `true`.
   *
   * @param ra $ra
   * @param gracePeriod $gracePeriod
   * @param toggleControl a manual toggle control. Use provided `Boolean => F[Unit]` to
   *        push the toggle state. `toggleControl` will be cancelled when the outer resource
   *        gets released.
   */
  def of[F[_]: Temporal, A](
    ra: Resource[F, A],
    gracePeriod: FiniteDuration = Duration.Zero,
  )(
    toggleControl: (Boolean => F[Unit]) => F[Unit],
  ): Resource[F, Resource[F, Option[A]]] = Resource.suspend {

    sealed trait State
    case class Active(a: A, awaitTermination: F[Unit]) extends State
    case object Empty extends State

    case class ToggleState(on: Boolean, next: Deferred[F, ToggleState])
    def toggleStateOf(on: Boolean): F[ToggleState] = Deferred[F, ToggleState].map(ToggleState(on, _))

    for {
      stateRef <- Ref.of[F, State](Empty)
      rwLock   <- ReadWriteRef[F].of(())
      flagRef  <- toggleStateOf(false).flatMap(Ref[F].of)

      featureToggledResource = {
        val toggle = (v: Boolean) => toggleStateOf(v).flatMap { ts =>
          flagRef.getAndSet(ts).flatMap(_.next.complete(ts)).uncancelable.void
        }

        val waitFor = (v: Boolean) => flagRef.get.tailRecM { get =>
          get.map(ts => if (ts.on == v) ().asRight else ts.next.get.asLeft)
        }

        // This is what we emit to clients.
        val access: Resource[F, Option[A]] = Resource.suspend {
          stateRef.get map {
            case Active(a, awaitTermination) =>
              // This will hold the lock until either a client is done using `a`,
              // or until termination is forced.
              runManaged(rwLock.read.use(_ => awaitTermination)) as a.some

            case Empty =>
              none[A].pure[Resource[F, *]]
          }
        }

        // 1) wait for "on", 2) expose resource, 3) wait for "off", 4) cleanup.
        val loopOnce: F[Unit] = waitFor(true) *> ra.use { a =>
          for {
            terminated <- Deferred[F, Unit]
            expose      = stateRef.set(Active(a, terminated.get))

            // As soon as we "expose" our resource we MUST guarantee it's cleaned up after use.
            _ <- (expose <* waitFor(false)).guarantee {
              (
                stateRef.set(Empty),
                (Temporal[F].sleep(gracePeriod) *> terminated.complete(())).start,
                rwLock.write.use(_ => ().pure[F]),
                ).tupled.void
            }
          } yield ()
        }

        val resourceManagementLoop = loopOnce.foreverM

        runManaged(resourceManagementLoop) *> runManaged(toggleControl(toggle)) as access
      }
    } yield featureToggledResource
  }

  private def runManaged[F[_]: Concurrent](f: F[_]): Resource[F, Unit] =
    Resource.make(f.start)(_.cancel).void
}
