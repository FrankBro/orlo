(define res 0)
(for ((x [1 2 3]))
  (set! res (+ res x)))
res
#; (the int 6)

(define arr [])
(for ((x [1 2]))
  (push! arr x))
arr
#; (the [int] [1 2])

(define arr [])
(for ((x [2 1]) (y [1 0]))
  (push! arr (- x y)))
arr
#; (the [int] [1 2 0 1])
