-- IMPORT dfs.lisp lambdapi.lisp helper.lisp

-- seek it, go to it
main =
(: (_computeRoute (_myPos WORLDSTATE)
                  (_matrixSearchR 4 (CAR WORLDSTATE) 0)
                  (CAR WORLDSTATE)
   )
   (\ ai world ->
      (IF (NIL ai)
          { route <- (_computeRoute (_myPos world)
                                    (_matrixSearchR 3 (CAR world) 0)
                                    (CAR world)
                     );
            (IF (NIL route) (: 0 0) (: (CDR route) (CAR route)))
          }
          (: (CDR ai) (CAR ai))
       )
   )
)
