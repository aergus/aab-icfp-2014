accList:
(\r accList x l (IF (== l 0) (CAR x) (accList (CRD x) (- l 1))))

lastTuple:
(\r lastTuple x l (IF (< l 2) (CRD x) (lastTuple (CRD x) (- l 1))))

foldr:
(\r foldr f base xs
    (IF (NIL xs) base (f (CAR x) (foldr f base (CRD x)))))

map:
(\r map f xs (IF (NIL xs) 0 (: (f (CAR x)) (map f (CRD xs)))))

elem:
(\ x xs (_foldr (\ y v (or (== x y) v)) 0 xs))

and:
(\ a b (IF (== a 1) (IF (== b 1) 1 0) 1))

or:
(\ a b (IF (== a 0) (IF (== b 0) 0 1) 1))
