-- IMPORT dfs.lisp lambdapi.lisp helper.lisp

-- be aware of *everything* and act accordingly (easier said than done)
main =
(: (: (_computeRoute (_myPos WORLDSTATE)
                     (_matrixSearchR 3 (CAR WORLDSTATE) 0)
                     (CAR WORLDSTATE)
      )
      (: 3 (: 0 0))
   )
   (\ai world ->
     (IF (IF (NIL (CAR ai))
             1
             (IF (_not (== (CAR (CDR ai)) (_myLives world)))
                 1
                 (_ghostsAhead (_myVitality world)
                               (_allGhosts world)
                               (_myPos world)
                               (CAR ai)
                 )
             )
         )
         (_generateRoute ai world)
         (: (: (CDR (CAR ai)) (CDR ai)) (CAR (CAR ai)))
     )
   )
)

_generateRoute =
(\ ai world ->
   { lives <- (_myLives world);
     preTarget <- (_matrixSearchR 3 (CAR world) 0);
     target <- (IF (== (CAR preTarget) -1)
                   (_matrixSearchR 2 (CAR world) 0)
                   preTarget
               );
     map <- (_foldr (\ g mp ->
                       (CDR (_accAndModMap mp (CAR (CDR g)) 7))
                    )
                    (CAR world)
                    (_allGhosts world)
            );
     route <- (_computeRoute (_myPos world) target (CAR world));
     (: (: (CDR route) (: lives (CDR (CDR ai))))
        (CAR route)
     )
  }
)