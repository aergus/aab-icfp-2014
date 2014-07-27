; accList :: [a] -> Int -> a
_accList =
(\r accList x l -> (IF (== l 0) (CAR x) (accList (CDR x) (- l 1))))

; _lastTuple :: ?
_lastTuple =
(\r lastTuple x l -> (IF (< l 2) (CDR x) (lastTuple (CDR x) (- l 1))))

; _foldr :: (a -> b -> b) -> b -> [a] -> b
_foldr =
(\ f base xs ->
    (IF (NIL xs) base (f (CAR xs) (_foldr f base (CDR xs)))))

; _map :: (a -> b) [a] -> [b]
_map =
(\ f xs -> (IF (NIL xs) 0 (: (f (CAR xs)) (_map f (CDR xs)))))

; _filter :: (a -> Int) -> [a] -> [a]
_filter =
(\ p l -> (IF (NIL l) 0 (IF (p (CAR l))
                            (: (CAR l) (_filter p (CDR l)))
                            (_filter p (CDR l)))))

; _elem :: Eq a => a -> [a] -> Int
_elem =
(\ x xs -> (IF (NIL xs) 0 (IF (== x (CAR xs)) 1 (_elem x (CDR xs)))))

; _mod :: Int -> Int -> Int
_mod =
(\ n m -> (- n (* m (/ n m))))

; _and :: Int -> Int -> Int
_and =
(\ a b -> (IF (== a 0) 0 (IF (== b 0) 0 1)))

; _or :: Int -> Int -> Int
_or =
(\ a b -> (IF (== a 0) (IF (== b 0) 0 1) 1))

; _not :: Int -> Int
_not =
(\ a -> (IF (== a 0) 1 0))
