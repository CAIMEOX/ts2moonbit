///|
extern "js" fn i() -> U2 = "() => 1"

///|
extern "js" fn j() -> U2 = "() => 'a'"

///|
pub fn callFromToplevel() -> String {
  "top level"
}

///|
typealias U2 = @js.Union2[String, Int]

///| Test Union Types
test {
  inspect!(i().to1(), content="Some(1)")
  inspect!(j().to0(), content="Some(\"a\")")
  inspect!(i().to0(), content="None")
}
