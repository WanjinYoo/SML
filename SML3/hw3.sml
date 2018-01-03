(* Assign 03 Provided Code *)

(*  Version 1.0 *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

(* Description of g:  g takes 2 functions and pattern and returns an integer.
For the value of r,Its third parameter has not defined yet. It will be defined if case p is tuple or constructor.
Otherwise we are not going to use r since we are only dealing with one number or string.
First function does not take any parameters. It returns an integer if and only if p is a wildcard.
Second function takes string and returns an integer if and only if p is a variable of string.
If pattern is Unit or ConstP, It is going to give you zero.
If pattern is TurpleP(TurpleP takes a list of patterns), It returns sum of its matching values in the list.
If pattern is Constructor, val r will be g f1 f2 p. it returns r (we do not care about the constructor name in this case)
*)

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(**** put all your code after this line ****)



fun only_capitals(xs : string list) =
  List.filter(fn y => Char.isUpper(String.sub(y, 0))) xs

fun longest_string1(xs : string list) =
  List.foldl(fn(x,y) => if String.size(x) > String.size(y) then x else y) "" xs
  
fun longest_string2(xs : string list) =
  List.foldl(fn(x,y) => if String.size(x) >= String.size(y) then x else y) "" xs
  
fun longest_string_helper f xs = 
  List.foldl(fn(x,y) =>if (f((String.size x),(String.size y))) then x else y) "" xs
  
fun longest_string3(xs : string list) =
  longest_string_helper(fn (x,y) => x > y) xs
  
fun longest_string4(xs : string list) =
  longest_string_helper(fn (x,y) => x >= y) xs
  
fun longest_capitalized(xs : string list) =
 (longest_string1 o only_capitals) xs
 
fun rev_string(xs : string) =
   (String.implode o List.rev o String.explode) xs
    
fun first_answer f a =
 case a of
   [] => raise NoAnswer
  | x :: x' => case f x of
       NONE => first_answer f x'
      |SOME y => y

fun all_answers f a =
let fun answers(x,y) =
   case x of
   [] => SOME y
  | q::q' => case f q of
   NONE => NONE
  | SOME p => answers(q',p @ y )
 in answers(a,[])
 end
  
(* fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end *)
    
fun count_wildcards x = 
   g (fn _=> 1) (fn a => 0) x

fun count_wild_and_variable_lengths(x) =
  g (fn a =>1) (fn b => String.size(b)) x

fun count_some_var(x,y) =
  g (fn a =>0) (fn b => if b=x then 1 else 0)  y
  
fun check_pat (x) =
 let fun card(x) =
    case x of 
      Wildcard          => []
    | Variable x        => x::[]
    | TupleP ps         => List.foldl (fn(a,b) => b@card(a)) [] ps
    | ConstructorP(_,p) => card(p)
    | _                 => []

 fun check(xs) =
     case xs of
      [] => true
      | a::a' => if List.exists (fn k => a=k) a' then false 
      else check(a')
   in check(card(x))
   end
  
fun match(x,y) = 
 case (x,y) of
  
 (_,Wildcard) => SOME []
 |(Unit,UnitP) => SOME []
 |(a,Variable b) => SOME [(b,a)] 
 |(Const q,ConstP p) => 
 if q = p then  SOME [] else NONE
 | (Constructor(t,t'),ConstructorP(u,u')) => 
 if (t = u) then match(t',u') else NONE
 |(Tuple a2,TupleP b2) =>if (List.length a2 = List.length b2)
  then all_answers match (ListPair.zip(a2,b2))
   else NONE
 |(_,_) => NONE
 
fun first_match a b =
  ( SOME (first_answer (fn x => match(a,x))b))   handle NoAnswer => NONE
  

 
  
  