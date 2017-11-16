(* Jastaj Virdee - 260689027 *)

(* Q1: A Rose by any Other Name Would Smell as Sweet *)

type 'a rose_tree = Node of 'a * ('a rose_tree) list

(* Find with exceptions *)

exception BackTrack

(* Q1.1 write a function that finds an element of the tree using backtracking with exceptions *)

let rec find_e (p : 'a -> bool) (t : 'a rose_tree) : 'a = (* Function with exceptions *)
  match t with
  | Node (a, []) -> if (p a) then a else raise BackTrack
  | Node (a, children) -> if (p a) then a
    else
      let rec check_children c = match c with
        | h::t -> (try find_e p h with BackTrack -> check_children t)
        | [] -> raise BackTrack
      in check_children children

(* Q1.1: write this function and it helper functions *)
let find (p : 'a -> bool)  (t : 'a rose_tree) : 'a option = (* call find_e and handle the exceptions *)
  try Some (find_e p t) with BackTrack -> None

(* Find with failure continuations *)

let rec find_k (p : 'a -> bool) (t : 'a rose_tree) (k : unit -> 'a option) : 'a option =
  match t with
    | Node (a, []) -> if (p a) then Some(a) else k ()
    | Node (a, children) -> if (p a) then Some(a) else
      let rec check_children c = match c with
        | h::t -> find_k p h (fun () -> check_children t)
        | [] -> k ()
      in check_children children

(* Q1.2: write this function and it helper functions *)
let find' (p : 'a -> bool)  (t : 'a rose_tree) : 'a option = find_k p t (fun () -> None)

(* Find all with continuations *)

let rec find_all_k  (p : 'a -> bool) (t : 'a rose_tree) (k : 'a list -> 'b) : 'b =
  match t with
    | Node (a, []) -> if (p a) then (k [a]) else k []
    | Node (a, children) ->
       let rec check_children c = match c with
         | h::t -> find_all_k p h (fun l -> (check_children t)@l) (*<-- This is sketchy*)
         | [] -> k [] in
       if (p a) then (check_children children)@(k [a])
       else check_children children

(* Q1.3: write this function and it helper functions *)
let find_all p t = find_all_k p t (fun l -> l)

(* An example to use *)

let example = Node (7, [ Node (1, [])
                         ; Node (15, [Node (6, [])])
                         ; Node (15, [])
                         ; Node (7, [])
                         ; Node (11, [])
                         ; Node (15, [])
                         ])

let is_big x =  x > 10

(* Q2 : Rational Numbers Two Ways *)

type fraction = int * int

module type Arith =
  sig
    type t
    val epsilon : t             (* A suitable tiny value, like epsilon_float for floats *)

    val plus : t -> t -> t      (* Addition *)
    val minus : t -> t -> t     (* Substraction *)
    val prod : t -> t -> t      (* Multiplication *)
    val div : t -> t -> t       (* Division *)
    val abs : t -> t            (* Absolute value *)
    val lt : t -> t -> bool     (* < *)
    val le : t -> t -> bool     (* <= *)
    val gt : t -> t -> bool     (* > *)
    val ge : t -> t -> bool     (* >= *)
    val eq : t -> t -> bool     (* = *)
    val from_fraction : fraction -> t (* conversion from a fraction type *)
    val to_string : t -> string        (* generate a string *)
  end

module FloatArith : Arith =
struct
  type t = float
  let epsilon = epsilon_float
  let from_fraction (num, den) = float_of_int num /. float_of_int den

  let plus = (+.)
  let minus = (-.)
  let prod = ( *. )
  let div = ( /. )
  let abs = abs_float
  let lt = (<)
  let le = (<=)
  let gt = (>)
  let ge = (>=)
  let eq = (=)
  let to_string x = string_of_float x
end

(* Q2.1: Implement the Arith module using rational numbers (t = fraction) *)

let rec gcd (u : int) (v : int) : int =
  if v <> 0 then (gcd v (u mod v))
  else (abs u)

let lcm (m : int) (n : int) : int  =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)

module FractionArith : Arith =
struct
  type t = fraction
  let epsilon = (1,1000000)
  let from_fraction (num, den) = (num, den)

  let plus (n1, d1) (n2, d2) = if d1=d2 then (n1+n2, d1)
                               else
                                 let den = lcm d1 d2 in
                                 let mult1 = den/d1 in
                                 let mult2 = den/d2 in
                                 (mult1*n1 + mult2*n2, den)
  let minus (n1, d1) (n2, d2) = if d1=d2 then (n1-n2, d1)
                                else
                                  let den = lcm d1 d2 in
                                  let mult1 = den/d1 in
                                  let mult2 = den/d2 in
                                  (mult1*n1 - mult2*n2, den)
  let prod (n1, d1) (n2, d2) = (n1*n2, d1*d2)
  let div (n1, d1) (n2, d2) = (n1*d2, n2*d1)
  let abs (num, den) = (abs num, abs den)
  let lt (n1, d1) (n2, d2) = if d1=d2 then n1 < n2
                             else
                               let den = lcm d1 d2 in
                               let mult1 = den/d1 in
                               let mult2 = den/d2 in
                               mult1*n1 < mult2*n2
  let le (n1, d1) (n2, d2) = if d1=d2 then n1 <= n2
                             else
                               let den = lcm d1 d2 in
                               let mult1 = den/d1 in
                               let mult2 = den/d2 in
                               mult1*n1 < mult2*n2
  let gt (n1, d1) (n2, d2)= if d1=d2 then n1 > n2
                            else
                              let den = lcm d1 d2 in
                              let mult1 = den/d1 in
                              let mult2 = den/d2 in
                              mult1*n1 > mult2*n2
  let ge (n1, d1) (n2, d2) = if d1=d2 then n1 >= n2
                             else
                               let den = lcm d1 d2 in
                               let mult1 = den/d1 in
                               let mult2 = den/d2 in
                               mult1*n1 >= mult2*n2
  let eq (n1, d1) (n2, d2) = if d1=d2 then n1 = n2
                             else
                               let den = lcm d1 d2 in
                               let mult1 = den/d1 in
                               let mult2 = den/d2 in
                               mult1*n1 = mult2*n2
  let to_string (num, den) = (string_of_int num)^"/"^(string_of_int den)
end

module type NewtonSolver =
  sig
    type t

    val square_root : t -> t
  end

(* Q2.2: Implement a function that approximates the square root using  the Newton-Raphson method *)

module Newton (A : Arith) : (NewtonSolver with type t = A.t) =
struct
 type t = A.t

 let rec findroot x acc approx =
    let next_approx = (A.div (A.plus (A.div x approx)  approx) (A.from_fraction(2,1))) in
      let current_accuracy = (A.abs (A.minus next_approx approx)) in
        if (A.le current_accuracy acc) then next_approx else
        begin
          print_string(A.to_string(current_accuracy));
          print_string("\n");
          findroot x acc next_approx;
        end

 let square_root n = findroot n A.epsilon (A.from_fraction(1,1));

end

(* Examples *)

module FloatNewton = Newton (FloatArith)
module RationalNewton = Newton (FractionArith)

let sqrt2 = FloatNewton.square_root (FloatArith.from_fraction (2, 1))
let sqrt2_r = RationalNewton.square_root (FractionArith.from_fraction (2, 1))

(* Q3 : Real Real Numbers, for Real! *)

type 'a stream = { head : 'a  ; tail : unit -> 'a stream}

let rec nth z = function
  | 0 -> z.head
  | n -> nth (z.tail()) (n - 1)

let rec constant x = {head = x ; tail = fun () -> constant x }

(* Some examples *)

let sqrt2 =
  {head = 1 ; tail = fun () -> constant 2} (* 1,2,2,2,2... *)

let golden_ratio = constant 1   (* 1,1,1,1,1,1 *)

let rec take n z =
  if n = 1 then [z.head]
  else z.head::(take (n-1) (z.tail()))

(* Q3.1: implement the function q as explained in the pdf *)
let rec q z n = match n with
  | 0 -> 1
  | 1 -> nth z 1
  | _ -> (nth z n) * (q z (n-1)) + (q z (n-2))

(* Q3.2: implement the function r as in the notes *)
let rec r z n = match n with
  | 0 -> float_of_int (nth z 0)
  | _ -> match n mod 2 with
    | 0 -> r z (n-1) -. 1. /. float_of_int ((q z n)*(q z (n-1)))
    | _ -> r z (n-1) +. 1./. float_of_int ((q z n)*(q z (n-1)))
(* Q3.3: implement the error function *)
let error z n = match n with
  | 0 -> let qn = float_of_int(q z n) in
         1.0 /. (qn *. qn)
  | _ -> let qn = float_of_int(q z n) in
         let qnminus1 = float_of_int(q z (n-1)) in
         1.0 /. (qn *. (qn +. qnminus1))

(* Q3.4: implement a function that computes a rational approximation of a real number *)
let rat_of_real z approx =
  let rec getApprox z approx n =
    if (error z n) <= approx then
      r z n
    else
      getApprox z approx (n+1)
  in getApprox z approx 0

let real_of_int n = { head = n ; tail = fun () -> constant 0}

(* Q3.5: implement a function that computes the real representation of a rational number   *)
let rec real_of_rat r = assert false


(* Examples *)

(* Approximations of the  irrational numbers we have *)

(* let sqrt_2_rat = rat_of_real sqrt2 1.e-5 *)
(* let golden_ratio_rat = rat_of_real golden_ratio 1.e-5 *)

(* To test the representation of rationals we can try this *)
(* let to_real_and_back n = rat_of_real (real_of_rat n) 0.0001 *)

(* e1 should be very close to 10 (it is exactly 10 in the model solution) *)
(* let e1 = to_real_and_back 10.0 *)

(* this is the float approximation of pi, not the real number pi *)
(* let not_pi = 2. *. acos 0. *)

(* This should share the same 4 decimals with not_pi *)
(* let not_pi' = to_real_and_back not_pi *)
