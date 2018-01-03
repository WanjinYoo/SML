(*  Assignment #1 *)

type DATE = (int * int * int)
exception InvalidParameter

(* This file is where your solutions go *)

fun is_older(d1: DATE, d2: DATE): bool =
  let
      val date1 = (#1(d1) * 365) + (#2(d1) * 30) + (#3(d1))
      val date2 = (#1(d2) * 365) + (#2(d2) * 30) + (#3(d2))
   in  
      if date1 < date2 
       then 
          true
      else 
          false
   end
   
fun number_in_month(x : DATE list, y :int) =
  if null x then 0
  else
   let
      val check = (#2(hd x))
    in
      if check = y
        then
         1 + number_in_month(tl x,y)
      else
          number_in_month(tl x,y)
     end
     
fun number_in_months(x : DATE list, y :int list) =
  if null y then 0
  else
  let
    val first = number_in_month(x,hd y)
  in 
     first + number_in_months(x,tl y)
  end
  
fun dates_in_month(x : DATE list, y :int) =
   if null x then []
   else
   if (#2(hd x)) = y then hd x :: dates_in_month(tl x,y)
   else
   dates_in_month(tl x,y)
   
fun dates_in_months(x : DATE list, y :int list) =
  
  if null y then []
  else
  let
    val list = dates_in_month(x,hd y)
  in 
     list @ dates_in_months(x,tl y)
  end
  
fun get_nth(x, y :int ) = 
 if null x then raise InvalidParameter
  else if y = 0 then raise InvalidParameter
  else if y = 1 then hd x else get_nth(tl x,y-1)
  
val monthnames = ["January","February","March","April","May","June","July","August","September","October","November","December"]

fun date_to_string(x : DATE) :string =
  get_nth(monthnames,#2 x)^" "^(Int.toString(#3 x))^", "^(Int.toString (#1 x))
  
fun number_before_reaching_sum(x :int, y : int list) =
 if x <= hd y then 0
  else 1+number_before_reaching_sum(x - hd y,tl y)
  
 val monthlist =[31,28,31,30,31,30,31,31,30,31,30,31]
  
fun what_month(x : int) =
  1+number_before_reaching_sum(x,monthlist)
  
fun month_range(x : int, y : int) =
  if y < x then []
 else if y = x then [what_month(y)]
 else what_month(x) :: month_range(x+1,y)
 
fun oldest(x : DATE list) =
  if null x then NONE
  else 
   let
   val older = oldest(tl x)
   in if isSome older andalso is_older(valOf older,hd x) = true then older
   else SOME (hd x)
   end
(* should handle leap years *)

fun reasonable_date(x :DATE):bool =
 if (#1 x) <= 0 orelse (#2 x) <= 0 orelse (#2 x) > 12 orelse (#3 x) <= 0 then false
 else if (#1 x) mod 4 = 0 andalso (#1 x) mod 100 <> 0 
 then
 let
 val leaplist = [31,29,31,30,31,30,31,31,30,31,30,31]
 val days = get_nth(leaplist,(#2 x))
 in if days < (#3 x) then false
 else true
 end
 else if get_nth(monthlist,(#2 x)) < (#3 x) then false
 else true
 
  
   
    
    


(* Add your other functions here *)