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
