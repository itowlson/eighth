(module
  (func $triangle (export "triangle") (param $a i32) (result i32)
    (local $lc i32)
    i32.const 0
    local.set $lc
    i32.const 0  ;; ...0
    block (param i32) (result i32)
        loop (param i32) (result i32)
            local.get $lc  ;; ...r, lc
            local.get $lc  ;; ...r, lc, lc
            i32.const 1    ;; ...r, lc, lc, 1
            i32.add        ;; ...r, lc, lc + 1
            local.set $lc  ;; ...r, lc
            i32.add        ;; r + lc
            local.get $a   ;; r + lc, a
            local.get $lc  ;; r + lc, a, lc
            i32.lt_u       ;; r + lc, t/f
            br_if 1        ;; r + lc
            br 0
        end
    end
  )
)
