///|
pub fn helloFromMod11() -> String {
  "hello from mod 11".to_string()
}

test {
  inspect!(@top.internal_fn(), content="internal")
}