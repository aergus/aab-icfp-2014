; Always keep your distance from the ghosts.
main =
(: 0
   (\ ai world ->
      ((\r handle gameMap ghosts pos dir hist k ->
           (IF (_or (_and ((\ x y ->
                              (_and (_accList (_accList gameMap y) x)
                                   (
; SORRY for this indentation chaos!
(\r ghostsClear ghosts x y ->
    (IF (NIL ghosts)
        1
        ((\ g ->
            (IF (<= ((\ a b c d ->
                        ((\ i j k l mx ad ->
                            (mx (ad i k mx) (ad j l mx))
                         ) a b c d (\ m n -> (IF (< n m) m n)) (\ m n mx ->
                                                                  (mx (- m n)
                                                                  (- n m)
                                                                 )
                                                               )
                        )
                     ) x y (CAR (CAR (CDR g))) (CDR (CAR (CDR g)))
                    )
                    2
                )
                (IF (== (CAR g) 1)
                    1
                    0
                )
                (ghostsClear (CDR ghosts) x y)
             )
         ) (CAR ghosts)
        )
    )
)
; back to normal indentation:
                                    ghosts x y)
                              )
                           ) (CAR (_applyDir pos dir)) (CDR (_applyDir pos dir))
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
               (handle gameMap ghosts pos (_mod (+ dir 1) 4) hist (- k 1))
          )
       ) (CAR world) (_allGhosts world) (_myPos world) (_myDir world) ai 8
      )
   )
)

; _allGhosts :: WorldState -> [Ghost]
_allGhosts =
(\ world ->
   (CAR (CDR (CDR world)))
)

; _myDir :: WorldState -> Int
_myDir =
(\ world ->
  (CAR (CDR (CDR (CAR (CDR world)))))
)

; _myPos :: WorldState -> (Int, Int)
_myPos =
(\ world ->
  (CAR (CDR (CAR (CDR world))))
)

; _applyDir :: (Int, Int) -> Int -> (Int, Int)
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

; accList :: [a] -> Int -> a
_accList =
(\r accList x l -> (IF (== l 0) (CAR x) (accList (CDR x) (- l 1))))

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
