; rotate clockwise if couldn't move in the last round
main =
(: (: 0 0)
   (\ ai world ->
      ((\ pos dir ->
          (: pos
             (IF (_and (==  (CAR pos) (CAR ai)) (==  (CDR pos) (CDR  ai)))
               (_mod (+ dir 1) 4)
               dir
             )
          )
       ) (_myPos world) (_myDir world)
      )
   )
)

; _myPos :: WorldState -> (Int, Int)
_myPos =
(\ world ->
  (CAR (CDR (CAR (CDR world))))
)

; _myDir :: WorldState -> Int
_myDir =
(\ world ->
  (CAR (CDR (CDR (CAR (CDR world)))))
)

; _mod :: Int -> Int -> Int
_mod = (\ n m -> (- n (* m (/ n m))))

; _and :: Int -> Int -> Int
_and = (\ a b -> (IF (== a 1) (IF (== b 1) 1 0) 0))
