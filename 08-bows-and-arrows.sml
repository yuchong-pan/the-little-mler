datatype 'a list = Empty
                 | Cons of 'a * 'a list

datatype orapl = Orange
               | Apple

fun eq_orapl(Orange,Orange) = true
  | eq_orapl(Apple,Apple) = true
  | eq_orapl(one,another) = false;

(eq_orapl : (orapl * orapl) -> bool);

fun eq_int(n:int,m:int) = n = m;

fun subst_int(n,a,Empty) = Empty
  | subst_int(n,a,Cons(e,t)) = if eq_int(a,e)
                               then Cons(n,subst_int(n,a,t))
                               else Cons(e,subst_int(n,a,t));

(subst_int : (int * int * int list) -> int list);

fun subst_orapl(n,a,Empty) = Empty
  | subst_orapl(n,a,Cons(e,t)) = if eq_orapl(a,e)
                                 then Cons(n,subst_orapl(n,a,t))
                                 else Cons(e,subst_orapl(n,a,t));

(subst_orapl : (orapl * orapl * orapl list) -> orapl list);

fun subst(rel,n,a,Empty) = Empty
  | subst(rel,n,a,Cons(e,t)) = if rel(a,e)
                               then Cons(n,subst(rel,n,a,t))
                               else Cons(e,subst(rel,n,a,t));

(subst : ((('b * 'a) -> bool) * 'a * 'b * 'a list) -> 'a list);

fun less_than(n:int,m:int) = n<m;

fun in_range((small,large),x) = if less_than(small,x)
                                then less_than(x,large)
                                else false;

(in_range : ((int * int) * int) -> bool);

fun subst_pred(pred,n,Empty) = Empty
  | subst_pred(pred,n,Cons(e,t)) = if pred(e)
                                     then Cons(n,subst_pred(pred,n,t))
                                     else Cons(e,subst_pred(pred,n,t));

(subst_pred : (('a -> bool) * 'a  * 'a list) -> 'a list);

fun is_15(n) = eq_int(n,15);

(is_15 : int -> bool);

fun in_range_c(small,large)(x) = if less_than(small,x)
                                 then less_than(x,large)
                                 else false;

(in_range_c : (int * int) -> (int -> bool));

fun in_range_c_11_16(x) = if less_than(11,x)
                          then less_than(x,16)
                          else false;

(* in_range_c(11,16) *)

fun subst_c(pred)(n,Empty) = Empty
  | subst_c(pred)(n,Cons(e,t)) = if pred(e)
                                 then Cons(n,subst_c(pred)(n,t))
                                 else Cons(e,subst_c(pred)(n,t));

(subst_c : ('a -> bool) -> (('a * 'a list) -> 'a list));

fun subst_c(pred) = fn (n,Empty) => Empty
                     | (n,Cons(e,t)) => if pred(e)
                                        then Cons(n,subst_c(pred)(n,t))
                                        else Cons(e,subst_c(pred)(n,t));

fun combine(Empty,Empty) = Empty
  | combine(Empty,Cons(b,l2)) = Cons(b,combine(Empty,l2))
  | combine(Cons(a,l1),Empty) = Cons(a,combine(l1,Empty))
  | combine(Cons(a,l1),Cons(b,l2)) = Cons(a,combine(l1,Cons(b,l2)));

(combine : 'a list * 'a list -> 'a list);

fun combine(Empty,l2) = l2
  | combine(Cons(a,l1),l2) = Cons(a,combine(l1,l2));

(combine : 'a list * 'a list -> 'a list);

fun combine_c(Empty)(l2) = l2
  | combine_c(Cons(a,l1))(l2) = Cons(a,combine_c(l1)(l2));

(combine_c : 'a list -> ('a list -> 'a list));

fun base(l2) = l2;

fun combine_s(Empty) = base
  | combine_s(Cons(a,l1)) = make_cons(a,combine_s(l1))
and make_cons(a,f)(l2) = Cons(a,f(l2));

(combine_s : 'a list -> ('a list -> 'a list));
(make_cons : (('a * ('a list -> 'a list)) -> ('a list -> 'a list)));

fun prefixer_123(l2) = Cons(1,Cons(2,Cons(3,l2)));

(prefixer_123 : int list -> int list);

fun prefix_3(l2) = Cons(3,base(l2));

(prefix_3 : int list -> int list);

fun prefix_23(l2) = Cons(2,prefix_3(l2));

(prefix_23 : int list -> int list);

fun prefix_123(l2) = Cons(1,prefix_23(l2));

(prefix_123 : int list -> int list);
