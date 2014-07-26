_dfs:
(\r dfs gameMap target dfsState ->
    (IF (NIL (CAR dfsState))
       (dfsState)
       (IF (__and (== (CAR target) (CAR (CAR (CAR dfsState))))
                  (== (CDR target) (CDR (CAR (CAR dfsState)))))
           (dfsState)
           (_dfs gameMap
                 target
                 (_updateState (CAR (CAR dfsState))
                 (dfsState))))))

_updateState:
(\ gameMap cell state ->
    (IF (__and (_cellFree gameMap (CAR cell) (- (CDR cell) 1))
               (_notVisited state (CAR cell) (- (CDR cell) 1)))
        (_addNewCell (CAR cell) (- (CDR cell) 1) cell state)
        (IF (__and (_cellFree gameMap (+ (CAR cell) 1) (CDR cell))
                   (_notVisited state (+ (CAR cell) 1) (CDR cell)))
            (_addNewCell (+ (CAR cell) 1) (CDR cell) cell state)
            (IF (__and (_cellFree gameMap (CAR cell) (+ (CDR cell) 1))
                       (_notVisited state (CAR cell) (+ (CDR cell) 1)))
                (_addNewCell (CAR cell) (- (CDR cell) 1) cell state)
                (IF (__and (_cellFree gameMap (- (CAR cell) 1) (CDR cell))
                           (_notVisited state (- (CAR cell) 1) (CDR cell)))
                    (_addNewCell (- (CAR cell) 1) (CDR cell) cell state)
                    (: (CAR (CDR state)) (CDR state)))))))

_cellFree:
(\ gameMap x y -> (__elem (_getCell gameMap x y) ([] 1 2 3 4)))

_notVisited:
(\ state x y -> (not (__elem (: x y) (CDR (CAR state)))))

_addNewCell:
(\ gameMap y oldCell state ->
    (: (: (: x y) (CAR state))
       (: (: (: x y) (CDR (CAR state)))
          (: (: oldCell (: x y)) (CDR (CDR state))))))

_getCell:
(\ gameMap x y -> (__accList (__accList gameMap x) y))
