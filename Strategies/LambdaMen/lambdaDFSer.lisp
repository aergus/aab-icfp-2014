-- IMPORT helper.lisp

-- seek it, go to it
main =
(: (_computeRoute (_matrixSearchR 4 (CAR WORLDSTATE) 0)
                  (_myPos WORLDSTATE)
                  (CAR WORLDSTATE)
   )
   (\ ai world ->
      (IF (NIL ai)
          { route <- (_computeRoute (_matrixSearchR 3 (CAR world) 0)
                                    (_myPos world)
                                    (CAR world)
                     );
            (IF (NIL route) (: 0 0) (: (CDR route) (CAR route)))
          }
          (: (CDR ai) (CAR ai))
       )
   )
)

-- _computeRoute :: (Int, Int) -> (Int, Int) -> [[Int]] -> Int
_computeRoute =
(\ start target map ->
   (_findDirsR target
               (_lambdaDFSR target ([] start) (CDR (_accAndModMap map start 8)))
               0
   )
)

-- _findDirsR :: (Int, Int) -> [[Int]] -> Int ->  Int
_findDirsR =
(\ target map dirs ->
    { state <- (_accMatrix map target);
      pos <- (_applyDir target (_mod (+ (- state 9) 2) 4));
      next <- (_accMatrix map pos);
      (IF (== next 8)
          (: (- state 9) dirs)
          (_findDirsR pos map (: (- state 9) dirs))
      )
    }
)

-- _lambdaDFSR :: (Int, Int) -> [(Int, Int)] -> [[Int]] -> Int
_lambdaDFSR =
(\ target stack map ->
  (IF (NIL stack)
      map
      (IF (_eqPair target (CAR stack))
          map
          { cur <- (CAR stack);
            pr <- (_handleDirR map cur 3);
            newStack <- (IF (== -1 (CAR pr))
                            (CDR stack)
                            (: (_applyDir cur (CAR pr)) stack)
                        );
            (_lambdaDFSR target newStack (CDR pr))
          }
      )
  )
)

-- _handleDirR :: [[Int]] -> (Int, Int) -> Int -> Int
_handleDirR =
(\ map pos k ->
   (IF (== -1 k)
       (: -1 map)
       { pr <- (_accAndModMap map (_applyDir pos k) (+ k 9));
         (IF (_and (CAR pr) (<= (CAR pr) 6))
             (: k (CDR pr))
             (_handleDirR (CDR pr) pos (- k 1))
         )
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
