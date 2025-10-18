; simple
(define-macro (unless cond then else)
  `(if ,cond ,else ,then))
(unless #f 1 2)
#; (the int 1)
(unless #t 1 2)
#; (the int 2)

; arithmetic
(define-macro (square x)
  `(* ,x ,x))
(square 5)
#; (the int 25)
(square (+ 2 3))
#; (the int 25)
; nested
(square (square 2))
#; (the int 16)

; list manpulation
(define-macro (first lst) `(car ,lst))
(first '(1 2 3))
#; (the int 1)

; vararg
(define-macro (when cond . body)
  `(if ,cond (begin ,@body) 0))
(when #t (+ 1 2))
#; (the int 3)
(when #t (+ 1 2) (* 3 4))
#; (the int 12)
(when #f (+ 1 2))
#; (the int 0)

; expand
(expand (square 3))
#; (the (symbol int int) (* 3 3))
(expand (unless #f 1 2))
#; (the (symbol bool int int) (if #f 2 1))
(expand (when #t (+ 1 2) (* 3 4)))
#; (the (symbol bool int int) (if #t (begin (+ 1 2) (* 3 4)) 0))
