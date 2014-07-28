-- accList :: [a] -> Int -> a
_accList =
(\ x l -> (IF (== l 0) (CAR x) (_accList (CDR x) (- l 1))))

-- _accMatrix :: [[a]] -> (Int, Int) -> a
_accMatrix =
(\ xss pos -> (_accList (_accList xss (CDR pos)) (CAR pos)))

-- _lastTuple :: ?
_lastTuple =
(\r lastTuple x l -> (IF (< l 2) (CDR x) (lastTuple (CDR x) (- l 1))))

-- _accAndModList :: [a] -> Int -> a -> (a, [a])
_accAndModList =
(\ xs n x ->
   (IF (== n 0)
       (: (CAR xs) (: x (CDR xs)))
       { pr <- (_accAndModList (CDR xs) (- n 1) x);
         (: (CAR pr) (: (CAR xs) (CDR pr)))
       }
   )
)

-- _accAndModMatrix :: [[a]] -> (Int, Int) -> a -> (a, [[a]])
_accAndModMatrix =
(\ xss pos x ->
  (IF (== (CDR pos) 0)
      { pr <- (_accAndModList (CAR xss) (CAR pos) x);
        (: (CAR pr) (: (CDR pr) (CDR xss)))
      }
      { pr <- (_accAndModMatrix (CDR xss) (: (CAR pos) (- (CDR pos) 1)) x);
        (: (CAR pr) (: (CAR xss) (CDR pr)))
      }
  )
)

-- _listSearchR :: Eq a => a -> [a] -> Int -> Int
_listSearchR =
(\ x xs k ->
   (IF (NIL xs)
       -1
       (IF (== (CAR xs) x)
           k
           (_listSearchR x (CDR xs) (+ k 1))
       )
   )
)

-- _matrixSearchR :: Eq a => a -> [[a]] -> Int -> (Int, Int)
_matrixSearchR =
(\ x xss k ->
  (IF (NIL xss)
      (: -1 -1)
      { i <- (_listSearchR x (CAR xss) 0);
        (IF (<= 0 i)
            (: i k)
            (_matrixSearchR x (CDR xss) (+ k 1))
        )
      }
  )
)

-- _foldr :: (a -> b -> b) -> b -> [a] -> b
_foldr =
(\ f base xs ->
    (IF (NIL xs) base (f (CAR xs) (_foldr f base (CDR xs)))))

-- _map :: (a -> b) [a] -> [b]
_map =
(\ f xs -> (IF (NIL xs) 0 (: (f (CAR xs)) (_map f (CDR xs)))))

-- _filter :: (a -> Int) -> [a] -> [a]
_filter =
(\ p l -> (IF (NIL l) 0 (IF (p (CAR l))
                            (: (CAR l) (_filter p (CDR l)))
                            (_filter p (CDR l)))))

-- _eqPair :: (Eq a, Eq b) -> (a, b) -> (a, b) -> Int
_eqPair =
(\ x y ->
   (_and (== (CAR x) (CAR y)) (== (CDR x) (CDR y)))
)

-- _elemEq :: a -> [a] -> (a -> Int) -> Int
_elemEq =
(\ x xs eq -> (IF (NIL xs) 0 (IF (eq x (CAR xs)) 1 (_elemEq x (CDR xs) eq))))

-- _elem :: Eq a => a -> [a] -> Int
_elem =
(\ x xs eq -> (IF (NIL xs) 0 (IF (== x (CAR xs)) 1 (_elem x (CDR xs)))))

-- _dist :: Int -> Int -> Int -> Int -> Int
_dist =
(\ x y v w ->
   (_max (_absDiff x v) (_absDiff y w))
)

-- _absDiff :: Int -> Int -> Int
_absDiff =
(\ x y ->
  (_max (- x y) (- y x))
)

-- _max :: Int -> Int -> Int
_max =
(\ x y ->
  (IF (< y x) x y)
)

-- _mod :: Int -> Int -> Int
_mod =
(\ n m -> (- n (* m (/ n m))))

-- _and :: Int -> Int -> Int
_and =
(\ a b -> (IF (== a 0) 0 (IF (== b 0) 0 1)))

-- _or :: Int -> Int -> Int
_or =
(\ a b -> (IF (== a 0) (IF (== b 0) 0 1) 1))

-- _not :: Int -> Int
_not =
(\ a -> (IF (== a 0) 1 0))
