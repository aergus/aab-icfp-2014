_accList =
(\r accList x l -> (IF (== l 0) (CAR x) (accList (CDR x) (- l 1))))

_lastTuple =
(\r lastTuple x l -> (IF (< l 2) (CDR x) (lastTuple (CDR x) (- l 1))))

_foldr =
(\r foldr f base xs ->
    (IF (NIL xs) base (f (CAR xs) (foldr f base (CDR xs)))))

_map =
(\r map f xs -> (IF (NIL xs) 0 (: (f (CAR xs)) (map f (CDR xs)))))

_elem =
(\ x xs -> (_foldr (\ y v -> (_or (== x y) v)) 0 xs))

_mod =
(\r n m -> (- n (/ n m)))

_and =
(\ a b -> (IF (== a 1) (IF (== b 1) 1 0) 1))

_or =
(\ a b -> (IF (== a 0) (IF (== b 0) 0 1) 1))

_not =
(\ a -> (IF (== a 0) 1 0))
