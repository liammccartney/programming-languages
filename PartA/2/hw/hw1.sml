(* is_older
* Return true if and only if date1 occurs before date2, false otherwise
* Type: ( (int * int * int) * (int * int * int) -> bool) *)
fun is_older (date1 : int*int*int, date2 : int*int*int) =
  if date1 = date2
  then
    false
  else if #1 date1 < #1 date2
  then
      true
  else if #1 date1 = #1 date2 andalso #2 date1 < #2 date2
  then
      true
  else if #1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2
  then
      true
  else
    false

(* number_in_months
* Return count of dates in a list that include the provided month value
* Type: ( (int * int * int) list * int -> int) *)
fun number_in_month (dates : (int*int*int) list, month : int) =
  if null dates
  then 0
  else if #2 (hd dates) = month
  then 1 + number_in_month(tl dates, month)
  else number_in_month(tl dates, month)


(* number_in_months
* Return count of dates in a list any of which include any of the provided list of months
* Type: ( (int * int * int) list * int list -> int) *)
fun number_in_months (dates : (int*int*int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)


(* dates_in_month
* Return list of dates that include a specific month
* Type: (int * int * int) list * int -> (int * int * int) list *)
fun dates_in_month (dates : (int*int*int) list, month: int) =
  if null dates
  then []
  else if #2 (hd dates) = month
  then (hd dates)::dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month)


(* dates_in_months
* Return list of dates that include any one of a list months
* Type: (int * int * int) list * int list -> (int * int * int) list *)
fun dates_in_months (dates : (int*int*int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


(* get_nth
* Returns nth element of a list of strings
* Type: string list * int -> string *)
fun get_nth (strings : string list, n : int) =
  if n = 1
  then hd strings
  else get_nth(tl strings, n - 1)


(* date_to_string
* Formats a date object as a string with the month's name
* Type: (int*int*int) -> string *)
fun date_to_string (date : (int * int * int)) =
  let
    val months = ["January", "February", "March",
                  "April", "May", "June",
                  "July", "August", "September",
                  "October", "November", "December"]
  in
    get_nth (months, #2 date) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
  end


(* number_before_reaching_sum
* Returns n where nums[0] + ... nums[n - 1] < sum
* Type: int * int list -> int *)
fun number_before_reaching_sum (sum : int, nums : int list) =
let
  fun sum_list (nums : int list) =
    if null nums
    then 0
    else hd(nums) + sum_list(tl(nums))
in
  if null nums
  then 0
  else if sum_list(nums) < sum
  then length nums
  else number_before_reaching_sum(sum, rev(tl(rev(nums))))
end

(* what_month
* Returns which month a day of the year falls in where day is between 1 and 365
* Type: int -> int *)
fun what_month (day : int) =
  let
    val last_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, last_days) + 1
  end


(* month_range
* Returns a list of months in which each day between two days fall
* Type: int * int -> int list *)
fun month_range (day1 : int, day2 : int) =
  let
    fun count_up_between (d : int)=
      if day2 < d
      then []
      else if d = day2
      then day2::[]
      else d::count_up_between(d + 1)

    fun map_to_months(days : int list) =
      if null days
      then days
      else what_month(hd days)::map_to_months(tl days)
  in
    map_to_months(count_up_between(day1))
  end


(* oldest
* Returns the oldest date from a list of dates
* Type: (int * int * int) list -> (int * int * int) *)
fun oldest (dates : (int * int * int) list) =
  if null dates
  then NONE
  else
    let val tl_oldest = oldest(tl dates)
    in if isSome tl_oldest andalso is_older(valOf tl_oldest, hd dates)
       then tl_oldest
       else SOME(hd dates)
    end


fun number_in_months_challenge () = 
  "hello"
