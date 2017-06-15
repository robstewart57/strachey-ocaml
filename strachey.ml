include Sys;;
include Printf;;

let rec makevar n = "a"^string_of_int n;;

let rec gen n =
 if n=0
 then []
 else (makevar n,n)::gen (n-1);;

let rec show t s =
 print_string s;
 print_string ": ";
 printf "%7.4f" t;
 print_string "\n";;

let rec update key value list =
 match list with
  [] -> [(key,value)] |
  (key1,value1)::rest ->
    if key=key1
    then (key,value)::rest
    else (key1,value1)::update key value rest;;

let rec updates l =
 match l with
 [] -> [] |
 (k,v)::r -> update k v (updates r);;

let rec tupdates l t c n =
if c = 0
then t/.n
else
 let t1 = Sys.time()
 in
  let _ = updates l
  in
   let t2 = Sys.time()
   in tupdates l (t+.t2-.t1) (c-1) n;;

let rec search key list =
 match list with
  [] ->  raise (Failure "search") |
  (key1,value1)::rest ->
   if key=key1
   then value1
   else search key rest;;

let rec searches l1 l2 =
 match l1 with
 [] -> () |
 (k,_)::r ->
  let _ = search k l2
  in searches r l2;;

let rec tsearches l1 l2 t c n =
if c = 0
then t/.n
else
 let t1 = Sys.time()
 in
  let _ = searches l1 l2
  in
   let t2 = Sys.time()
   in tsearches l1 l2 (t+.t2-.t1) (c-1) n;;

let sUpdate key value rest = (key,value)::rest;;

let rec sUpdates l =
 match l with
 [] -> [] |
 (k,v)::r -> (k,v)::sUpdates r;;

let rec tsUpdates l t c n =
if c = 0
then t/.n
else
 let t1 = Sys.time()
 in
  let _ = sUpdates l
  in
   let t2 = Sys.time()
   in tsUpdates l (t+.t2-.t1) (c-1) n;;

let rec pSearch key =
 if key="a"
 then 1
 else
  if key="b"
  then 2
  else raise (Failure "search");;

let rec strUpdate ident value store  =
 fun id -> if id=ident then value else store id;;

let strEmpty id = raise (Failure "search");;

let rec strUpdates l =
 match l with
 [] -> strEmpty |
 (k,v)::r -> strUpdate k v (strUpdates r);;

let rec tstrUpdates l t c n =
if c = 0
then t/.n
else
 let t1 = Sys.time()
 in
  let _ = strUpdates l
  in
   let t2 = Sys.time()
   in tsUpdates l (t+.t2-.t1) (c-1) n;;

let rec strSearches l store =
 match l with
 [] -> () |
 (k,_)::r ->
  let _ = store k
  in strSearches r store;;

let rec tstrSearches l store t c n =
if c = 0
then t/.n
else
 let t1 = Sys.time()
 in
  let _ = strSearches l store
  in
   let t2 = Sys.time()
   in tsUpdates l (t+.t2-.t1) (c-1) n;;

let rec lqUpdate k v l =
 match l with
 (key,value)::rest ->
  if k=key
  then (value,l)
  else
   let (newvalue,newrest) = lqUpdate k v rest
   in (newvalue,(key,value)::newrest) |
  [] -> (v,[(k,v)]);;

let rec lqUpdates l =
 match l with
 [] -> [] |
 (k,v)::r ->
  let (_,l1) = lqUpdate k v (lqUpdates r)
  in l1;;

let rec tlqUpdates l t c n =
if c = 0
then t/.n
else
 let t1 = Sys.time()
 in
  let _ = lqUpdates l
  in
   let t2 = Sys.time()
   in tlqUpdates l (t+.t2-.t1) (c-1) n;;

let rec qUpdate key value rest =
 fun k v -> if k=key
            then (value,qUpdate key value rest)
            else
             let (newvalue,newrest) = rest k v
             in (newvalue,qUpdate key value newrest) ;;

let rec qEmpty key value = (value,qUpdate key value qEmpty );;

let rec qUpdates l =
 match l with
 [] -> qEmpty |
 (k,v)::r -> qUpdate k v (qUpdates r);;

let rec tqUpdates l t c n =
if c = 0
then t/.n
else
 let t1 = Sys.time()
 in
  let _ = qUpdates l
  in
   let t2 = Sys.time()
   in tqUpdates l (t+.t2-.t1) (c-1) n;;

let rec qSearches l q =
 match l with
 [] -> () |
 (k,v)::r ->
  let (_,f) = q k v
  in qSearches r f;;

let rec tqSearches l q t c n =
if c = 0
then t/.n
else
 let t1 = Sys.time()
 in
  let _ = qSearches l q
  in
   let t2 = Sys.time()
   in tqSearches l q (t+.t2-.t1) (c-1) n;;

let rec gens i p s c n =
 if i>p
 then ()
 else
  let l = gen i
  in
  print_int i; print_string " & ";
  printf "%7.4f" (tupdates l 0.0 c n); print_string " & ";
  let ll = updates l
  in
  printf "%7.4f" (tsearches l ll 0.0 c n); print_string " & ";
  printf "%7.4f" (tsUpdates l 0.0 c n); print_string " & ";
  printf "%7.4f" (tstrUpdates l 0.0 c n); print_string " & ";
  let store = strUpdates l
  in
  printf "%7.4f" (tstrSearches l store 0.0 c n); print_string "\n ";
  gens (i+s) p s c n;;

let rec gens1 i p s c n l =
  printf "%7.4f" (tlqUpdates l 0.0 c n); print_string " & ";
  printf "%7.4f" (tqUpdates l 0.0 c n); print_string " & ";
  let q = qUpdates l
  in
  printf "%7.4f" (tqSearches l q 0.0 c n); print_string "\\\\\n";
  gens1 (i+s) p s c n;;

let p = 6000;;
let s = 300;;
let i = 300;;

let c = 5;;
let n = 5.0;;

print_string "%";;
print_int p;;
print_string " key/value pairs maximum\n";;
print_string "%";;
print_int s;;
print_string " steps\n";;
print_string "%";;
print_int c;;
print_string " repeats\n";;
print_string "p & ";
print_string "list update & ";;
print_string "list search & ";;
print_string "list stack update & ";;
print_string "Strachey update & ";;
print_string "Strachey search & ";;
print_string "list queue update & ";;
print_string "queue update & ";;
print_string "queue search \\\\\n";;

gens i p s c n;;

let rec oUpdate key value rest =
 fun k v -> if k=key
            then (value,oUpdate key value rest)
            else
             if k<key
             then (v,oUpdate k v (oUpdate key value rest))
             else
              let (newvalue,newrest) = rest k v
              in (newvalue,oUpdate key value newrest);;

let rec oEmpty key value = (value,oUpdate key value oEmpty);;

let rec cUpdate key value rest =
 fun search k v ->
            if k=key
            then
             if search
             then (value,cUpdate key value rest)
             else (v,cUpdate key v rest)
            else
             let (newvalue,newrest) = rest search k v
             in (newvalue,cUpdate key value newrest);;

let rec cEmpty search key value = (value,cUpdate key value cEmpty);;

let rec tUpdate key value left right =
 fun k v -> if k=key
            then (value,tUpdate key value left right)
            else
             if k<key
             then
              let (newvalue,newleft) = left k v
              in (newvalue,tUpdate key value newleft right)
             else
              let (newvalue,newright) = right k v
              in (newvalue,tUpdate key value left newright);;

let rec tEmpty key value = (value,tUpdate key value tEmpty tEmpty);;
