(.a 1)
#; (the ( .a int . a) ( .a 1 ))

(match (.a 1)
  ((.a a) a))
#; (the int 1)

(lambda (a) (match a
  ((.a a) a)))
#; (the (lambda ((.a a)) a) (lambda (a) 'body))

(lambda (a) (match a
  ((.a a) a)
  ((.b b) b)))
#; (the (lambda ((.a a .b a)) a) (lambda (a) 'body))

(match (.a 1) ((.a a) a) ((.b b) b))
#; (the int 1)
