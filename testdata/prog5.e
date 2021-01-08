struct pt {
    $x i32
    $y i32
}

func translate [pt i32 i32] -> [pt] {
    $0.$x $1 +
    $0.$y $2 +
}
