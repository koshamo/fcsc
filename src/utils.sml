(* -- general functions -- *)
(*  zip2 : 'a list * 'b list -> ('a * 'b) list  *)
val zip2 = ListPair.zip

(*  lfold : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b  *)
fun lfold f u [] = u
  | lfold f u (x::xs) = lfold f (f u x) xs

(*  rfold : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b  *)
fun rfold f u [] = u
  | rfold f u (x::xs) = f x (rfold f u xs)

(*  sort 'a list -> 'a list  *)
fun sort [] = []
  | sort (x::xs) = let val lt = List.filter (fn y => y < x) xs
                       val gte = List.filter (fn y => y >= x) xs
                   in sort lt @ [x] @ sort gte
                   end

(* -- temporary constants -> TODO: implementing lazy structures -- *)
val maxHeap = 100
(* --- *)

(* -- association list -- *)
type ('a,'b) assoc = ('a * 'b) list

(*  aLookup : assoc -> 'a -> 'b -> 'b  *)
fun aLookup []          key def = def
  | aLookup ((k,v)::bs) key def = if k = key then v
                                  else aLookup bs key def

(*  aDomain : ('a,'b) assoc -> 'a list  *)
fun aDomain (alist : ('a,'b) assoc) = map #1 alist

(*  aRange : ('a,'b) assoc -> 'b list  *)
fun aRange (alist : ('a,'b) assoc) = map #2 alist

(*  aEmpty : ('a,'b) assoc  *)
val aEmpty = []


(* -- heap -- *)
(*  remove : (int * 'a) list -> 'a -> (int * 'a) list  *)
fun remove [] ad = raise Fail ("Attempt to update or free nonexistent address #"
                               ^ Int.toString ad)
  | remove ((a,e)::xs) ad = if a = ad then xs 
                            else (a,e):: remove xs ad
(*  makeAddrList : int -> int list -> int list  *)
fun makeAddrList (0, xs) = 0::xs
  | makeAddrList (n, xs) = makeAddrList (n-1, n::xs)


(*   'a heap : (1) number of object 
               (2) list of unused addresses 
               (3) assoc list mapping addresses to objects *)
type 'a heap = int * int list * (int * 'a) list
type addr = int

(*  hInitial : 'a heap  *)
fun hInitial size = (0, makeAddrList (size,[]), []) 

(*  hAlloc : 'a heap -> 'a -> 'a heap * addr  *)
fun hAlloc (size, (next::free), cts) n = 
           ((size+1, free, (next,n) :: cts), next)

(*  hUpdate : 'a heap -> addr -> 'a -> 'a heap  *)
fun hUpdate (size, free, cts) a n = (size, free, (a,n) :: remove cts a)

(*  hFree : 'a heap -> addr -> 'a heap  *)
fun hFree (size, free, cts) a = (size-1, a::free, remove cts a)

(*  showaddr : addr -> string (* or [char]?*) *)
fun showaddr a = "#" ^ Int.toString a

(*  hLookup : 'a heap -> addr -> 'a  *)
fun hLookup (size, free, cts) a =
        aLookup cts a (raise Fail 
                        ("can't find node " ^ showaddr a ^ " in heap"))

(*  hAddresses : 'a heap -> addr list  *)
fun hAddresses (size, free, cts : (int * 'a) list) = map #1 cts

(*  hSize : 'a heap -> int  *)
fun hSize (size, free, cts) = size

(*  hNull : addr  *)
val hNull = 0

(*  hIsnull : addr -> bool  *)
fun hIsnull a = a = 0


(* -- generating unique names -- *)
type nameSupply = int
val initialNameSupply = 0 : nameSupply

(*  makeName : string -> n -> string  *)
fun makeName pref ns = pref ^ "_" ^ Int.toString ns

(*  makeNameList : string list -> n -> string list  *)
fun makeNameList [] _ = []
  | makeNameList (p::ps) n = makeName p n :: makeNameList ps (n+1)

(*  getName : nameSupply -> string -> nameSupply * string  *)
fun getName nameSup pref = (nameSup+1, makeName pref nameSup)

(*  getNames : nameSupply -> string list -> nameSupply * string list  *)
fun getNames nameSup prefs = (nameSup + length prefs, makeNameList prefs
  nameSup)
  

(* -- sets -- *)
type 'a set = 'a list

fun getOrder (a,b) = if a < b then LESS else
                     if a = b then EQUAL else
                     GREATER

(*  setFromList : 'a list -> 'a set  *)
val setFromList  = let fun rmdup [] = []
                          | rmdup [x] = [x]
                          | rmdup (x::y::xs) = if x = y then rmdup (y::xs)
                                               else x :: rmdup (y::xs)
                    in rmdup o sort 
                    end

(*  setToList : 'a set -> 'a list  *)
fun setToList xs = xs

(*  setUnion : 'a set -> 'a set -> 'a set  *)
fun setUnion [] [] = []
  | setUnion [] ys = ys
  | setUnion xs [] = xs
  | setUnion (x::xs) (y::ys) = case getOrder(x,y) 
                                 of LESS    => x :: setUnion xs (y::ys)
                                  | EQUAL   => x :: setUnion xs ys
                                  | GREATER => y :: setUnion (x::xs) ys
                                    
(*  setIntersection : 'a set -> 'a set -> 'a set  *)
fun setIntersection [] [] = []
  | setIntersection [] ys = []
  | setIntersection xs [] = []
  | setIntersection (x::xs) (y::ys) = 
                    case getOrder(x,y)
                      of LESS    => setIntersection xs (y::ys)
                       | EQUAL   => x :: setIntersection xs ys
                       | GREATER => setIntersection (x::xs) ys

(*  setSubtraction : 'a set -> 'a set -> 'a set  *)
fun setSubtraction [] [] = []
  | setSubtraction [] ys = []
  | setSubtraction xs [] = xs
  | setSubtraction (x::xs) (y::ys) = 
                   case getOrder(x,y)
                     of LESS    => x :: setSubtraction xs (y::ys)
                      | EQUAL   => setSubtraction xs ys
                      | GREATER => setSubtraction (x::xs) ys

(*  setElementOf : 'a -> 'a set -> bool  *)
fun setElementOf x []      = false
  | setElementOf x (y::ys) = x = y orelse 
                             if x > y then setElementOf x ys
                             else false

(*  setEmpty : 'a set  *)
val setEmpty = []

(*  setIsEmpty : 'a set -> bool  *)
fun setIsEmpty s = null s

(*  setSingleton : 'a -> 'a set  *)
fun setSingleton x = [x]

(*  setUnionList : 'a set list -> 'a set  *)
val setUnionList = lfold setUnion setEmpty
