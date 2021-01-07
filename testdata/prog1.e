(module
  (func add_squares ($a i32) ($b i32) -> i32 {
    local.get $a
    local.get $a
    i32.mul
    local.get $b
    local.get $b
    i32.mul
    i32.add
  })
)
