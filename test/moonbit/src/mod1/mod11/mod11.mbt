///|
pub fn helloFromMod11() -> String {
  "hello from mod 11".to_string()
}

///|
test {
  inspect!(@top.internal_fn(), content="internal")
}

///|
pub typealias Ref1[T] = @top.Reference[T]

///|
test {
  let r : Ref1[Int] = @top.createRef(11)
  r.showRef()
}
