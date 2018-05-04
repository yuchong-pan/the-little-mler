signature Ysig
	  =
	  sig
	    val Y : (('a -> 'a) -> ('a -> 'a)) -> ('a -> 'a);
	  end;

functor Yfunc()
	:>
	Ysig
	=
	struct
	  datatype 'a T = Into of 'a T -> 'a;
	  fun Y(f) = H(f)(Into(H(f)))
	  and H(f)(a) = f(G(a))
	  and G(Into(a))(x) = a(Into(a))(x);
	end;

structure Ystruct = Yfunc();

fun mk_fact(fact)(n) = if (n = 0)
		       then 1
		       else n * fact(n - 1);
