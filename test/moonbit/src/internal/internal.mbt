///|
pub fn internal_fn() -> String {
  "internal"
}

///| Extern type with generic
extern type Reference[T]

///|
extern "js" fn create_ref_prim(v : @js.Value) -> @js.Value = "v => ({ current: v })"

///|
extern "js" fn console_log_prim(v : @js.Value) -> Unit = "v => console.log(v)"

///|
pub fn createRef[T](v : T) -> Reference[T] {
  create_ref_prim(@js.Value::cast_from(v)).cast()
}

///|
pub fn showRef[T](self : Reference[T]) -> Unit {
  @js.Value::cast_from(self) |> console_log_prim
}

///|
test {
  let r = createRef(1)
  let x = createRef("hello internal")
  r.showRef()
  x.showRef()
}
