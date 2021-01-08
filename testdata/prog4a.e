func square [i32] -> [i32] {
    $0 $0 *
}

func add_squares [i32 i32] -> [i32] {
    $0 square
    $1 square
    +
}

func sub_squares [i32 i32] -> [i32] {
    $0 square $1 square -
}
