let l = [3; 5; 1; 4; 6];;

(* TRI FUSION *)
let rec partition (l : 'a list) : 'a list * 'a list =
  match l with
  | [] -> [], []
  | [a] -> [a], []
  | a :: b :: q -> let (l1, l2) = partition q in (a :: l1, b :: l2);;

let rec fusion (l1 : 'a list)(l2 : 'a list) : 'a list =
  match l1, l2 with
  | [], _ -> l2
  | _, [] -> l1
  | t1 :: q1, t2 :: q2 when t1 < t2 -> t1 :: fusion q1 l2
  | t1 :: q1, t2 :: q2 -> t2 :: fusion l1 q2;;

let rec tri_fusion (l : 'a list) : 'a list =
  match l with
  | [] | [_] -> l
  | _ -> let (l1, l2) = partition l
         in fusion (tri_fusion l1)(tri_fusion l2);;

tri_fusion l;;

(* TRI FUSION *)

let rec taille_liste (l : 'a list) : int =
  match l with
  | [] -> 0
  | t :: q -> 1 + taille_liste q;;

let rec pour_tous (f : 'a -> bool)(l : 'a list) : bool =
  match l with
  | [] -> true
  | t :: q -> if f t then pour_tous f q else false;;

