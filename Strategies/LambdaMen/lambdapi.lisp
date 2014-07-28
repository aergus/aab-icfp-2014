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

-- _myPos :: WorldState -> (Int, Int)
_myPos =
(\ world ->
  (CAR (CDR (CAR (CDR world))))
)

-- _allGhosts :: WorldState -> [Ghost]
_allGhosts =
(\ world ->
   (CAR (CDR (CDR world)))
)

-- _ghostsClear :: [Ghost] -> Int -> Int -> Int
_ghostsClear =
(\ ghosts x y ->
   (IF (NIL ghosts)
       1
       ((\ g ->
           (IF (<= (_dist x y (CAR (CAR (CDR g))) (CDR (CAR (CDR g))))
                   2
               )
               (IF (== (CAR g) 1)
                   1
                   0
               )
               (_ghostsClear (CDR ghosts) x y)
            )
        ) (CAR ghosts)
       )
   )
)

-- _accAndModMap :: [[Int]] -> (Int, Int) -> Int -> (Int, [[Int]])
_accAndModMap =
(\ xss pos x ->
  (IF (== (CDR pos) 0)
      { pr <- (_accAndModLine (CAR xss) (CAR pos) x);
        (: (CAR pr) (: (CDR pr) (CDR xss)))
      }
      { pr <- (_accAndModMap (CDR xss) (: (CAR pos) (- (CDR pos) 1)) x);
        (: (CAR pr) (: (CAR xss) (CDR pr)))
      }
  )
)

-- _accAndModLine :: [Int] -> Int -> Int -> (Int, [Int])
_accAndModLine =
(\ xs n x ->
   (IF (== n 0)
       (: (CAR xs)
          (: (IF (_and (CAR xs) (<= (CAR xs) 6)) x (CAR xs)) (CDR xs))
       )
       { pr <- (_accAndModLine (CDR xs) (- n 1) x);
         (: (CAR pr) (: (CAR xs) (CDR pr)))
       }
   )
)
