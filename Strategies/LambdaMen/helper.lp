(\r accList x l (If (== l 0) (Car x) (arrList (Crd x) (- l 1))))
(\r lastTuple x l (If (< l 2) (Crd x) (lastTuple (Crd x) (- l 1))))
