(define i 0)
(define arr [])
(while (< i 5)
  (push! arr i)
  (set! i (+ i 1)))
arr
#; (the [int] [0 1 2 3 4])
