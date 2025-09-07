* Actually make a UnboundList and GenericList, I don't like that `(lambda (a b) (cons a b))` is `(λ a b -> (a . b))`, should be `(λ a la -> (a . la))` or something
