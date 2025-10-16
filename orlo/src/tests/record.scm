{}
#; (the {} {})
{ .a 1 }
#; (the { .a int } { .a 1 })
{ .a 1 .b #t }
#; (the { .a int .b bool } { .a 1 .b #t })

(. { .a 1 } a)
#; (the int 1)
(. [{ .a 1 }] 0 a)
#; (the int 1)

(define rec {})
#; (the {} {})
{ .a 1 . rec }
#; (the { .a int } { .a 1 })

(define (get-a rec) (. rec a))
#; (the (lambda ({ .a a . b }) a) (lambda (rec) 'body))
