package cats
package data

import cats.functor.Bifunctor

final case class Biprod[F[_, _], G[_, _], A, B](first: F[A, B], second: G[A, B])

object Biprod extends BiprodInstances

private[data] sealed abstract class BiprodInstances {
  implicit def biprodBifunctor[F[_, _]: Bifunctor, G[_, _]: Bifunctor]: Bifunctor[Biprod[F, G, ?, ?]] =
    new BiprodBifunctor[F, G] {
      val F = Bifunctor[F]
      val G = Bifunctor[G]
    }

  implicit def biprodBifoldable[F[_, _]: Bifoldable, G[_, _]: Bifoldable]: Bifoldable[Biprod[F, G, ?, ?]] =
    new BiprodBifoldable[F, G] {
      val F = Bifoldable[F]
      val G = Bifoldable[G]
    }
}

private[data] sealed abstract class BiprodBifunctor[F[_, _], G[_, _]] extends Bifunctor[Biprod[F, G, ?, ?]] {
  def F: Bifunctor[F]
  def G: Bifunctor[G]

  def bimap[A, B, C, D](fab: Biprod[F, G, A, B])(f: A => C, g: B => D): Biprod[F, G, C, D] =
    Biprod(F.bimap(fab.first)(f, g), G.bimap(fab.second)(f, g))
}

private[data] sealed abstract class BiprodBifoldable[F[_, _], G[_, _]] extends Bifoldable[Biprod[F, G, ?, ?]] {
  def F: Bifoldable[F]
  def G: Bifoldable[G]

  def bifoldLeft[A, B, C](fab: Biprod[F, G, A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C = {
    val first = F.bifoldLeft(fab.first, c)(f, g)
    G.bifoldLeft(fab.second, first)(f, g)
  }

  def bifoldRight[A, B, C](fab: Biprod[F, G, A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] = {
    val first = F.bifoldRight(fab.first, c)(f, g)
    G.bifoldRight(fab.second, first)(f, g)
  }
}
