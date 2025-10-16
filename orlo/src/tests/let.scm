(let ((x 1)) x)
#; (the int 1)

(let ((x 1) (y 2)) (+ x y))
#; (the int 3)

(let loop ((i 0))
  (if (> i 5)
      i
      (loop (+ i 1))))
#; (the int 6)
