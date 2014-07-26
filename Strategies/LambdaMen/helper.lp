(\r accList x l (If (== l 0) (Car x) (accList (Cdr x) (- l 1))))
(\r lastTuple x l (If (< l 2) (Cdr x) (lastTuple (Cdr x) (- l 1))))
(\r foldr f base xs (If (Nil xs) base (f (Car x) (foldr f base (Cdr
x)))))
(\r map f xs (If (Nil xs) 0 (Cons (f (Car xs)) (map f (Cdr xs)))))
;get cell
(Car worldstate)
(\ x y (accList (accList (Car worldstate) 

((\r accList x l (If (== l 0) (Car x) (accList (Cdr x) (- l 1)))) ([] 13 37 42) 2)
