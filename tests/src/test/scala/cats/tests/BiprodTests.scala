package cats
package tests

import cats.data.{Biprod, Xor}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq.tuple2Eq

class BiprodTests extends CatsSuite {
  checkAll("Biprod[Xor, Tuple2, ?, ?]", BitraverseTests[Biprod[Xor, Tuple2, ?, ?]].bitraverse[Option, Int, Int, Int, String, String, String])
  checkAll("Bitraverse[Biprod[Xor, Tuple2, ?, ?]]", SerializableTests.serializable(Bitraverse[Biprod[Xor, Tuple2, ?, ?]]))
}
