datatype 'a list = Empty
                 | Cons of 'a * 'a list

datatype box = Bacon
             | Ix of int

fun is_bacon(Bacon) = true
  | is_bacon(Ix(n)) = false;

(is_bacon : box -> bool);

exception No_bacon of int

fun where_is(Empty) = raise No_bacon(0)
  | where_is(Cons(a_box,rest)) = if is_bacon(a_box)
                                 then 1
                                 else 1 + where_is(rest);

(where_is : box list -> int);

(where_is(Cons(Ix(5),Cons(Ix(13),Cons(Ix(8),Empty))))
    handle No_bacon(an_int) => an_int);

exception Out_of_range

fun eq_int(n:int,m:int) = n = m;

fun list_item(n,Empty) = raise Out_of_range
  | list_item(n,Cons(abox,rest)) = if eq_int(n,1)
                                   then abox
                                   else list_item(n - 1,rest);

(list_item : int * box list -> box);

fun find(n,boxes) = (check(n,boxes,list_item(n,boxes))
  handle Out_of_range => find(n div 2,boxes))
and check(n,boxes,Bacon) = n
  | check(n,boxes,Ix(i)) = find(i,boxes);

(find : (int * (box list)) -> int);
(check : (int * (box list) * box) -> int);

val t = Cons(Ix(5),Cons(Ix(4),Cons(Bacon,Cons(Ix(2),Cons(Ix(7),Empty)))));

fun path(n,boxes) = Cons(n,(check(n,boxes,list_item(n,boxes))
  handle Out_of_range => path(n div 2,boxes)))
and check(n,boxes,Bacon) = Empty
  | check(n,boxes,Ix(i)) = path(i,boxes);

(path : (int * (box list)) -> (int list));
(check : (int * (box list) * box) -> (int list));

fun path(n,boxes) = Cons(n,(check(boxes,list_item(n,boxes))
  handle Out_of_range => path(n div 2,boxes)))
and check(boxes,Bacon) = Empty
  | check(boxes,Ix(i)) = path(i,boxes);

(path : (int * (box list)) -> (int list));
(check : ((box list) * box) -> (int list));
