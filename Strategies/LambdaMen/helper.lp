(\r accList x l (If (== l 0) (Car x) (accList (Crd x) (- l 1))))
(\r lastTuple x l (If (< l 2) (Crd x) (lastTuple (Crd x) (- l 1))))
(\r foldr f base xs (If (Nil xs) base (f (Car x) (foldr f base (Crd
x)))))
(\r map f xs (If (Nil xs) 0 (Cons (f (Car x)) (map f (Crd xs)))))
