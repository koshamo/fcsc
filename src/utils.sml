(* -- general functions -- *)
(*  zip2 : 'a list -> 'b list -> ('a * 'b) list  *)
fun zip2 a b = ListPair.zip(a,b)

(*  first : 'a * 'b -> 'a  *)
fun first (a,b) = a

(*  second : 'a * 'b -> 'b  *)
fun second (a,b) = b

(*  lfold : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b  *)
fun lfold f u [] = u
  | lfold f u (x::xs) = lfold f (f u x) xs

(*  rfold : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b  *)
fun rfold f u [] = u
  | rfold f u (x::xs) = f x (rfold f u xs)

(*  mapAccuml : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list  *)
fun mapAccuml f acc []      = (acc, [])
  | mapAccuml f acc (x::xs) = let 
                                val (acc1, x') = f acc x
                                val (acc2, xs') = mapAccuml f acc1 xs
                              in
                                (acc2, x'::xs')
                              end

(*  sort 'a list -> 'a list  *)
fun sort [] = []
  | sort (x::xs) = let val lt = List.filter (fn y => y < x) xs
                       val gte = List.filter (fn y => y >= x) xs
                   in sort lt @ [x] @ sort gte
                   end

(*  space : int -> string  *)
fun space n = let fun ps c l = if c > 0 then " " ^ ps (c-1) l else l
              in ps n "" 
              end

(* -- temporary constants -> TODO: implementing lazy structures -- *)
val maxHeap = 100
(* --- *)

(* -- association list -- *)
type ('a,'b) Assoc = ('a * 'b) list

(*  aLookup : Assoc -> 'a -> 'b -> 'b  *)
fun aLookup []          key def = def
  | aLookup ((k,v)::bs) key def = if k = key then v
                                  else aLookup bs key def

(*  aDomain : ('a,'b) Assoc -> 'a list  *)
fun aDomain (alist : ('a,'b) Assoc) = map #1 alist

(*  aRange : ('a,'b) Assoc -> 'b list  *)
fun aRange (alist : ('a,'b) Assoc) = map #2 alist

(*  aEmpty : ('a,'b) Assoc  *)
val aEmpty = []


(* -- heap -- *)
(*  remove : (int * 'a) list -> 'a -> (int * 'a) list  *)
fun remove [] ad = raise Fail ("Attempt to update or free nonexistent address #"
                               ^ Int.toString ad)
  | remove ((a,e)::xs) ad = if a = ad then xs 
                            else (a,e):: remove xs ad

(*  makeAddrList : int * int list -> int list  *)
fun makeAddrList (0, xs) = 0::xs
  | makeAddrList (n, xs) = makeAddrList (n-1, n::xs)


(*   'a Heap : (1) number of object 
               (2) list of unused addresses 
               (3) assoc list mapping addresses to objects *)
type 'a Heap = int * int list * (int * 'a) list
type Addr = int

(*  hInitial : 'a Heap  *)
fun hInitial size = (0, makeAddrList (size,[]), []) 

(*  hAlloc : 'a Heap -> 'a -> 'a Heap * Addr  *)
fun hAlloc (size, (next::free), cts) n = 
           ((size+1, free, (next,n) :: cts), next)

(*  hUpdate : 'a Heap -> Addr -> 'a -> 'a Heap  *)
fun hUpdate (size, free, cts) a n = (size, free, (a,n) :: remove cts a)

(*  hFree : 'a Heap -> Addr -> 'a Heap  *)
fun hFree (size, free, cts) a = (size-1, a::free, remove cts a)

(*  showaddr : Addr -> string (* or [char]?*) *)
fun showaddr a = "#" ^ Int.toString a

(*  hLookup : 'a Heap -> Addr -> 'a  *)
fun hLookup (size, free, cts) a =
        aLookup cts a (raise Fail 
                        ("can't find node " ^ showaddr a ^ " in heap"))

(*  hAddresses : 'a Heap -> Addr list  *)
fun hAddresses (size, free, cts : (int * 'a) list) = map #1 cts

(*  hSize : 'a Heap -> int  *)
fun hSize (size, free, cts) = size

(*  hNull : Addr  *)
val hNull = 0

(*  hIsnull : Addr -> bool  *)
fun hIsnull a = a = 0


(* -- generating unique names -- *)
type NameSupply = int
val initialNameSupply = 0 : NameSupply

(*  makeName : string -> n -> string  *)
fun makeName pref ns = pref ^ "_" ^ Int.toString ns

(*  makeNameList : string list -> n -> string list  *)
fun makeNameList [] _ = []
  | makeNameList (p::ps) n = makeName p n :: makeNameList ps (n+1)

(*  getName : NameSupply -> string -> NameSupply * string  *)
fun getName nameSup pref = (nameSup+1, makeName pref nameSup)

(*  getNames : NameSupply -> string list -> NameSupply * string list  *)
fun getNames nameSup prefs = (nameSup + length prefs, makeNameList prefs
  nameSup)
  

(* -- sets -- *)
type 'a Set = 'a list

fun getOrder (a,b) = if a < b then LESS else
                     if a = b then EQUAL else
                     GREATER

(*  setFromList : 'a list -> 'a Set  *)
val setFromList  = let fun rmdup [] = []
                          | rmdup [x] = [x]
                          | rmdup (x::y::xs) = if x = y then rmdup (y::xs)
                                               else x :: rmdup (y::xs)
                    in rmdup o sort 
                    end

(*  setToList : 'a Set -> 'a list  *)
fun setToList xs = xs

(*  setUnion : 'a Set -> 'a Set -> 'a Set  *)
fun setUnion [] [] = []
  | setUnion [] ys = ys
  | setUnion xs [] = xs
  | setUnion (x::xs) (y::ys) = case getOrder(x,y) 
                                 of LESS    => x :: setUnion xs (y::ys)
                                  | EQUAL   => x :: setUnion xs ys
                                  | GREATER => y :: setUnion (x::xs) ys
                                    
(*  setIntersection : 'a Set -> 'a Set -> 'a Set  *)
fun setIntersection [] [] = []
  | setIntersection [] ys = []
  | setIntersection xs [] = []
  | setIntersection (x::xs) (y::ys) = 
                    case getOrder(x,y)
                      of LESS    => setIntersection xs (y::ys)
                       | EQUAL   => x :: setIntersection xs ys
                       | GREATER => setIntersection (x::xs) ys

(*  setSubtraction : 'a Set -> 'a Set -> 'a Set  *)
fun setSubtraction [] [] = []
  | setSubtraction [] ys = []
  | setSubtraction xs [] = xs
  | setSubtraction (x::xs) (y::ys) = 
                   case getOrder(x,y)
                     of LESS    => x :: setSubtraction xs (y::ys)
                      | EQUAL   => setSubtraction xs ys
                      | GREATER => setSubtraction (x::xs) ys

(*  setElementOf : 'a -> 'a Set -> bool  *)
fun setElementOf x []      = false
  | setElementOf x (y::ys) = x = y orelse 
                             if x > y then setElementOf x ys
                             else false

(*  setEmpty : 'a Set  *)
val setEmpty = []

(*  setIsEmpty : 'a Set -> bool  *)
fun setIsEmpty s = null s

(*  setSingleton : 'a -> 'a Set  *)
fun setSingleton x = [x]

(*  setUnionList : 'a Set list -> 'a Set  *)
val setUnionList = lfold setUnion setEmpty
