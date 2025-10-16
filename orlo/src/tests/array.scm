[]
#; (the [~a] [])
[1]
#; (the [int] [1])
[1 2]
#; (the [int] [1 2])

(define arr [])
#; (the [~a] [])
(push! arr 1)
#; (the [int] [1])
(. [1] 0)
#; (the int 1)
(. [1 2 3] -1)
#; (the int 3)
(. [1 2 3] -2)
#; (the int 2)
(. [1 2 3] -3)
#; (the int 1)
