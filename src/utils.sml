(*  zip2 : 'a list * 'b list -> ('a * 'b) list  *)
val zip2 = ListPair.zip

(* --- *)
val maxHeap = 100

(*  makeAddrList : int -> int list -> int list  *)
fun makeAddrList (0, xs) = 0::xs
  | makeAddrList (n, xs) = makeAddrList (n-1, n::xs)

(*  remove : (int * 'a) list -> 'a -> (int * 'a) list  *)
fun remove [] _ = []
  | remove ((a,e)::xs) ad = if a = ad then xs 
                            else (a,e):: remove xs ad

(* --- *)

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


