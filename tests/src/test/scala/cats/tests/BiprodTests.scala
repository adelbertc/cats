package cats
package tests

import cats.data.{Biprod, Xor}
import cats.laws.discipline._

class BiprodTests extends CatsSuite {
  checkAll("Biprod[Xor, Tuple2, ?, ?]", BifunctorTests[Biprod[Xor, Xor, ?, ?]])
}
