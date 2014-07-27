-- Never do the same thing for a third time!
main =
(: 0
   (\ ai world ->
      ((\r handle gameMap pos dir hist k ->
           (IF (_or (_and (_accList (_accList gameMap (CDR (_applyDir pos dir)))
                                    (CAR (_applyDir pos dir))
                          )
                          (_or (<= ((\r count x y z list ->
                                        (IF (NIL list)
                                            0
                                            ((\ t c ->
                                                (IF (_and (== x (CAR t))
                                                          (_and
                                                            (== y (CAR (CDR t)))
                                                            (== z (CDR (CDR t)))
                                                          )
                                                    )
                                                    (+ 1 c)
                                                    c
                                                )
                                             ) (CAR list)
                                               (count x y z (CDR list))
                                            )
                                        )
                                    ) dir (CAR pos) (CDR pos) hist
                                   )
                                   1
                               )
                               (<= k 4)
                          )
                    )
                    (_not k)
               )
               (: (: (: dir pos) hist) dir)
               (handle gameMap pos (_mod (+ dir 1) 4) hist (- k 1))
          )
       ) (CAR world) (_myPos world) (_myDir world) ai 8
      )
   )
)

-- _myDir :: WorldState -> Int
_myDir =
(\ world ->
  (CAR (CDR (CDR (CAR (CDR world)))))
)

-- _myPos :: WorldState -> (Int, Int)
_myPos =
(\ world ->
  (CAR (CDR (CAR (CDR world))))
)

-- _applyDir :: (Int, Int) -> Int -> (Int, Int)
_applyDir =
(\ pos dir ->
   (IF (== dir 0)
       (: (CAR pos) (- (CDR pos) 1))
       (IF (== dir 1)
           (: (+ (CAR pos) 1) (CDR pos))
           (IF (== dir 2)
               (: (CAR pos) (+ (CDR pos) 1))
               (: (- (CAR pos) 1) (CDR pos))
           )
       )
   )
)

-- accList :: [a] -> Int -> a
_accList =
(\r accList x l -> (IF (== l 0) (CAR x) (accList (CDR x) (- l 1))))

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
