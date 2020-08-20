data MyList a = Empty | Cons a (MyList a)

myMap :: (a -> b) -> MyList a -> MyList b
myMap _ Empty = Empty
myMap f (Cons x xs) = Cons (f x) (myMap f xs)