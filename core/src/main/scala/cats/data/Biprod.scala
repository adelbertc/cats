package cats
package data

import cats.functor.Bifunctor

final case class Biprod[F[_, _], G[_, _], A, B](first: F[A, B], second: G[A, B])

object Biprod extends BiprodInstances

private[data] sealed abstract class BiprodInstances extends BiprodInstances1 {
  implicit def biprodEq[F[_, _], G[_, _], A, B](implicit FAB: Eq[F[A, B]], GAB: Eq[G[A, B]]): Eq[Biprod[F, G, A, B]] =
    new Eq[Biprod[F, G, A, B]] {
      def eqv(x: Biprod[F, G, A, B], y: Biprod[F, G, A, B]): Boolean =
        FAB.eqv(x.first, y.first) && GAB.eqv(x.second, y.second)
    }

  implicit def biprodBitraverse[F[_, _]: Bitraverse, G[_, _]: Bitraverse]: Bitraverse[Biprod[F, G, ?, ?]] =
    new BiprodBitraverse[F, G] {
      val F = Bitraverse[F]
      val G = Bitraverse[G]
    }
}

private[data] sealed abstract class BiprodInstances1 {
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

private[data] sealed trait BiprodBifunctor[F[_, _], G[_, _]] extends Bifunctor[Biprod[F, G, ?, ?]] {
  def F: Bifunctor[F]
  def G: Bifunctor[G]

  def bimap[A, B, C, D](fab: Biprod[F, G, A, B])(f: A => C, g: B => D): Biprod[F, G, C, D] =
    Biprod(F.bimap(fab.first)(f, g), G.bimap(fab.second)(f, g))
}

private[data] sealed trait BiprodBifoldable[F[_, _], G[_, _]] extends Bifoldable[Biprod[F, G, ?, ?]] {
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

private[data] sealed abstract class BiprodBitraverse[F[_, _], G[_, _]]
    extends Bitraverse[Biprod[F, G, ?, ?]]
    with BiprodBifoldable[F, G]
    with BiprodBifunctor[F, G] {
  def F: Bitraverse[F]
  def G: Bitraverse[G]

  def bitraverse[H[_], A, B, C, D](fab: Biprod[F, G, A, B])(f: A => H[C], g: B => H[D])(implicit H: Applicative[H]): H[Biprod[F, G, C, D]] = {
    val first = F.bitraverse(fab.first)(f, g)
    val second = G.bitraverse(fab.second)(f, g)
    H.map2(first, second) { case (fcd, gcd) => Biprod(fcd, gcd) }
  }

  override def bimap[A, B, C, D](fab: Biprod[F, G, A, B])(f: A => C, g: B => D): Biprod[F, G, C, D] =
    Biprod(F.bimap(fab.first)(f, g), G.bimap(fab.second)(f, g))
}
