open kcolor


let lire_graphe (filename : string) : graphe =
  let x = open_in filename in
  let n = ref 0 in
  let edges = ref [] in
  try
    while true do
      let line = input_line x in
      match line.[0] with
      | 'c' -> () (* Ligne de commentaire, ignorée *)
      | 'p' -> 
        let line_list = List.filter (fun s -> s <> "") (String.split_on_char ' ' line) in
        n := int_of_string (List.nth line_list 2) (* Nombre de sommets *)
      | 'e' -> 
        let line_list = List.filter (fun s -> s <> "") (String.split_on_char ' ' line) in
        let u = int_of_string (List.nth line_list 1) - 1 in (* Convertir en base 0 *)
        let v = int_of_string (List.nth line_list 2) - 1 in
        edges := (u, v) :: !edges
      | _ -> ()
    done;
    [||] (* Jamais atteint, sauf si la boucle est interrompue *)
  with
  | End_of_file -> 
    close_in x;
    (* Construire le graphe *)
    let g = Array.make !n [] in
    List.iter (fun (u, v) ->
      g.(u) <- v :: g.(u) (* Ajouter l’arête u -> v *)
    ) !edges;
    g
;;

let graphe5 = lire_graphe "test2.col";;
let graphe6 = desorienter_graphe graphe5;;

let graphe_simple = lire_graphe "test/test_simple3.col";;
let graphe_simpleNO = desorienter_graphe graphe_simple;;

dsatur graphe_simpleNO;;
dsatur_branch_and_bound graphe_simpleNO;;
dsatur_mem graphe_simpleNO;;

dsatur_mem graphe6;;
