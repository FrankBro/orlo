(let ((x 1)) x)
#; (the int 1)

(let ((x 1) (y 2)) (+ x y))
#; (the int 3)

(let loop ((i 0))
  (if (> i 5)
      i
      (loop (+ i 1))))
#; (the int 6)

; we don't overwrite top variables AND they don't live on
(define x 0)
(let ((x 1) (y 2)) (set! x 3))
x
#; (the int 0)
y
#; "Unbound variable: y"
