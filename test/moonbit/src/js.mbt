///|
extern "js" fn i() -> U2 = "() => 1"

///|
extern "js" fn j() -> U2 = "() => 'a'"

///|
pub fn callFromToplevel() -> String {
  "top level"
}

///|
typealias @js.Union2[String, Int] as U2

///| Test Union Types
test {
  inspect(i().to1(), content="Some(1)")
  inspect(j().to0(), content="Some(\"a\")")
  inspect(i().to0(), content="None")
}
