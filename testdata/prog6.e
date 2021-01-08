struct pt {
    $x i32
    $y i32
}

struct sz {
    $w i32
    $h i32
}

struct rect {
    $pos pt
    $size sz
}

func br [rect] -> [pt] {
    $0.$pos.$x $0.$size.$w +
    $0.$pos.$y $0.$size.$h +
}
