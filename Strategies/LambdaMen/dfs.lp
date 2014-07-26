(\r dfs map target dfsState =
    if (__nil (CAR dfsState)) (dfsState)
       if ((== (CAR target) (CAR (CAR (CAR dfsState)))) * (== (CDR target) (CDR (CAR (CAR dfsState))))) (dfsState)
          (dfs map target (updateState (CAR (CAR dfsState)) (dfsState))))

updateState:
(\ map cell state
            = if ((_cellFree (CAR cell) (- (CDR cell) 1)) * (_notVisited state (CAR cell) (- (CDR cell) 1)))
                 (_addNewCell (CAR cell) (- (CDR cell) 1) cell state)
                 (if ((_cellFree (+ (CAR cell) 1) (CDR cell)) * (_notVisited state (+ (CAR cell) 1) (CDR cell)))
                     (_addNewCell (+ (CAR cell) 1) (CDR cell) cell state)
                     (if ((_cellFree (CAR cell) (+ (CDR cell) 1)) * (_notVisited state (CAR cell) (+ (CDR cell) 1)))
                         (_addNewCell (CAR cell) (- (CDR cell) 1) cell state)
                         (if ((_cellFree (- (CAR cell) 1) (CDR cell)) * (_notVisited state (- (CAR cell) 1) (CDR cell)))
                              (_addNewCell (- (CAR cell) 1) (CDR cell) cell state)
                              (: (CAR (CDR state)) (CDR state)))))))

addNewCell
(\ map x y oldCell state =
  (: (: (: x y) (CAR state)) (: (: (: x y) (CDR (CAR state))) (: (: oldCell (: x y)) (CDR (CDR state))))))

getCell:
(\ map x y = _accList (_accList map x) y)

notVisited:

cellFree:
