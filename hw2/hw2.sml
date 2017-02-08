(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)
exception notFound
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for Part 1 here *)

fun all_except_option (a : string,b : string list) = 
 case b of 
    [] => NONE
  |  x :: x' => 
    case same_string(x, a) of 
      true  => SOME x'
    | false => case all_except_option(a, x') of 
                 NONE   => NONE
               | SOME y => SOME (x::y)
               
               
val test1_0 = all_except_option("~1",["5","2","~1","3"]) = SOME ["5","2","3"];

fun get_substitutions1(a : string list list,b : string) =

 case a of 
   [] => []
  | x :: x' => 
  case all_except_option(b,x) of
  NONE => get_substitutions1(x',b)
 |SOME x => x @ get_substitutions1(x',b)

val test2_0 = get_substitutions1([["5","wanjin","Jack","Daniel"],["wanjin"],[]],"wanjin") = ["5","Jack","Daniel"];    

fun get_substitutions2(a : string list list,b : string) =

 let fun tailrecursive(remainstring : string list list, matchstring : string list) =
 case remainstring of 
   [] => matchstring
  | x :: x' => 
  case all_except_option(b,x) of
  NONE => tailrecursive(x',matchstring)
 |SOME x => tailrecursive(x',matchstring @ x) 
 in tailrecursive(a,[])
 end
 
val test3_0 = get_substitutions2([["wanjin","jack"],["wanjin","jack"],["wanjin","jack"]],"jack") = ["wanjin","wanjin","wanjin"];

fun similar_names( a : string list list,{first:string,middle:string,last:string }) = 
let
val alt_list = first :: get_substitutions2(a,first)  
fun similar( list : string list) =
case list of
 [] => []
| x::x' => {first =x,middle = middle,last = last} :: similar (x') 
in similar(alt_list)
end

val test4_0 = similar_names([["wanjin", "wan"],["wanjin", "jin","Dereck"],["England", "Kim"]], 
             {first="wanjin", middle = "wanny", last="jin"}) =
            [{first="wanjin",last="jin",middle="wanny"},
             {first="wan",last="jin",middle="wanny"},{first="jin",last="jin",middle="wanny"},
             {first="Dereck",last="jin",middle="wanny"}];
(************************************************************************)
(* Game  *)

(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw
val testcard = [(Clubs, Ace),(Clubs, Num 10), (Clubs, Num 2),(Clubs, Ace)];

exception IllegalMove

(* put your solutions for Part 2 here *)

fun card_color((suit,rank):card) =
case suit of
        Clubs => Black
     | Spades => Black
     | Hearts => Red
     | Diamonds => Red
     
val test5_0= card_color((Clubs,King)) = Black;

fun card_value((suit,rank):card) =
case rank of
  Jack => 10
|Queen => 10
|King => 10
|Ace => 11
|Num x => x

val test6_0= card_value((Diamonds,Jack)) = 10;

fun remove_card(cs :card list,c,e) =
case cs of
  [] => raise e
| x::x' => if c = x then x'
else x::remove_card(x',c,e)
  
val test7_0 = remove_card(testcard, (Clubs, Ace), notFound) = [(Clubs, Num 10), (Clubs, Num 2),(Clubs, Ace)];


fun all_same_color(cs :card list) =
case cs of 
  [] => true
  |x :: [] => true
  |x :: x' :: x'' => if (card_color(x) = card_color(x')) then all_same_color(x'::x'') else false
  
val test8_0 = all_same_color(testcard) = true;

fun sum_cards(cs :card list) =

let fun summ(sum : int ,c' : card list) =
case c' of
[] => sum
| x :: x' => summ(sum+card_value(x),x')
in summ(0,cs)
end

val test9_0 = sum_cards(testcard) = 34;

fun score(cs :card list, goal : int) =
let 
val x = sum_cards(cs)
val y = if  x > goal then 2 *(x - goal) else (goal - x)
in if (all_same_color(cs) = true) then (y div 2) else y
end

val test10_0 = score(testcard,20) = 14;


fun officiate(cs,ms,goal) =

 let fun gamestart(rcs, hcs ,rms) =
 case rms of 
 [] => score(hcs,goal)
 | x :: x' =>  case x of 
Discard y => gamestart(rcs,(remove_card(hcs,y,IllegalMove)),x')
|DRAW => case rcs of
  [] => score(hcs,goal)
  |a :: a' => if (sum_cards(a::hcs) > goal) 
  then score(a::hcs,goal)
  else gamestart(a',a::hcs,x')


 in gamestart(cs,[],ms)
 end
 
 val test11_0 = officiate(testcard,[Draw,Draw,Draw,Draw,Discard(Clubs,Ace)],20) = 1;

