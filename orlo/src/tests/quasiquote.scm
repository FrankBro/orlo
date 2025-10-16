; basic
`a
#; (the symbol a)
`(1 2 3)
#; (the (int int int) (1 2 3))
(let ((x 2)) `(1 ,x 3))
#; (the (int int int) (1 2 3))

; unquote-splicing
(let ((lst '(2 3))) `(,@lst))
#; (the (int int) (2 3))
(let ((lst '(2 3))) `(,@lst 4))
#; (the (int int int) (2 3 4))
(let ((lst '(2 3))) `(1 ,@lst))
#; (the (int int int) (1 2 3))
(let ((lst '(2 3))) `(1 ,@lst 4))
#; (the (int int int int) (1 2 3 4))

; multiple
(let ((a '(1 2)) (b '(3 4))) `(,@a ,@b))
#; (the (int int int int) (1 2 3 4))

; nested
`((1 2) (3 4))
#; (the ((int int) (int int)) ((1 2) (3 4)))

; empty
`()
#; (the () ())
(let ((x '())) `(1 ,@x 2))
#; (the (int int) (1 2))

; unquote and unquote-splicing
(let ((x 1) (lst '(2 3))) `(,x ,@lst 4))
#; (the (int int int int) (1 2 3 4))

; deeply nested
(let ((x 1)) `(a (b ,x)))
#; (the (symbol (symbol int)) (a (b 1)))

`#t
#; (the bool #t)
`"hello"
#; (the string "hello")
`42
#; (the int 42)

; with eval
(let ((x 5)) `(1 ,(+ x 2) 3))
#; (the (int int int) (1 7 3))
(let ((lst '(1 2))) `(a ,(car lst)))
#; (the (symbol int) (a 1))
