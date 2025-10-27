(the int 1)
#; (the int 1)
(the (lambda (a) a) (lambda (x) x))
#; (the (lambda (a) a) (lambda (x) 'body))
(the (lambda (b) b) (lambda (x) x))
#; "the expected type '(lambda (b) b)' does not match the inferred type '(lambda (a) a)'"
