fun identity(x) = x;

(identity : 'a -> 'a);

fun true_maker(x) = true;

(true_maker : 'a -> bool);

datatype bool_or_int = Hot of bool
                     | Cold of int

fun hot_maker(x) = Hot;

(hot_maker : 'a -> (bool -> bool_or_int));

fun help(f) = Hot(true_maker(if true_maker(5)
                             then f
                             else true_maker));

(help : ('a -> bool) -> bool_or_int);

datatype chain = Link of (int * (int -> chain));

fun ints(n) = Link(n + 1,ints);

(ints : int -> chain);

fun skips(n) = Link(n + 2,skips);

(skips : int -> chain);

fun divides_evenly(n,c) = (n mod c) = 0;

(divides_evenly : (int * int) -> bool);

fun is_mod_5_or_7(n) = if divides_evenly(n,5)
                       then true
                       else divides_evenly(n,7);

(is_mod_5_or_7 : int -> bool);

fun some_ints(n) = if is_mod_5_or_7(n + 1)
                   then Link(n + 1,some_ints)
                   else some_ints(n + 1);

(some_ints : int -> chain);

fun eq_int(n:int,m:int) = n = m;

fun chain_item(n,Link(i,f)) = if eq_int(n,1)
                              then i
                              else chain_item(n - 1,f(i));

(chain_item : (int * chain) -> int);

fun is_prime(n) = has_no_divisors(n,n-1)
and has_no_divisors(n,c) = if c=1
                           then true
                           else
                             if divides_evenly(n,c)
                             then false
                             else has_no_divisors(n,c - 1);

(is_prime : int -> bool);
(has_no_divisors : (int * int) -> bool);

fun primes(n) = if is_prime(n + 1)
                then Link(n + 1,primes)
                else primes(n + 1);

(primes : int -> chain);

fun fibs(n)(m) = Link(n + m,fibs(m));

(fibs : int -> (int -> chain));

fun fibs_1(m) = Link(1 + m,fibs(m));

fun fibs_2(m) = Link(2 + m,fibs(m));
