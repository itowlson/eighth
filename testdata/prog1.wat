(module
  (func $add_squares (export "add_squares") (param $a i32) (param $b i32) (result i32)
    local.get $a
    local.get $a
    i32.mul
    local.get $b
    local.get $b
    i32.mul
    i32.add
  )
)
