type 'a Lazy = unit -> 'a

fun force (f: 'a Lazy) = f ()

datatype 'a Seq = Nil | Cons of 'a * 'a Seq Lazy

fun take 0 s   = []
  | take n Nil = []
  | take n (Cons (h,t)) = h :: take (n-1) (force t)

fun maps f Nil = Nil
  | maps f (Cons (h,t)) = Cons (f h, fn () => maps f (force t))

fun filters p Nil = Nil
  | filters p (Cons (h,t)) = if p h
                             then Cons (h, fn () => filters p (force t))
                             else filters p (force t)
