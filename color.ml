#load "unix.cma"

(* Type pour représenter un graphe *)
type voisin = int list

type graphe = voisin array

type coloration = int array

(* Trouve le sommet avec le DSAT maximal (ou, en cas d'égalité, le degré maximal) *)
let sommet_saturation_max_mem (g : graphe) (c : coloration) (dsat : int array) : int =
  let n = Array.length g in
  let max_sommet = ref (-1) in
  let max_dsat = ref (-1) in
  let max_degre = ref (-1) in
  for i = 0 to n - 1 do
    if c.(i) = -1 then (* Considérer uniquement les sommets non coloriés *)
      let dsat_i = dsat.(i) in
      let degre_i = List.length g.(i) in
      if dsat_i > !max_dsat || (dsat_i = !max_dsat && degre_i > !max_degre) then begin
        max_sommet := i;
        max_dsat := dsat_i;
        max_degre := degre_i;
      end
  done;
  !max_sommet

let sommet_saturation_max (g : graphe) (c : coloration) (dsat : int array) : int =
  let max_sommet = ref (-1) in
  let max_dsat = ref (-1) in
  let max_degre = ref (-1) in
  Array.iteri (fun i voisins ->
    if c.(i) = -1 then
      let dsat_i = dsat.(i) in
      let degre_i = List.length voisins in
      if dsat_i > !max_dsat || (dsat_i = !max_dsat && degre_i > !max_degre) then begin
        max_sommet := i;
        max_dsat := dsat_i;
        max_degre := degre_i;
      end
  ) g;
  !max_sommet



let update_dsat (g : graphe) (c : coloration) (dsat : int array)
                (voisins_colores : int list array) (sommet : int) (couleur : int) : (int * int) list =
  (* Met à jour les DSAT et voisins_colores, en mémorisant les modifications *)
  let modifications = ref [] in
  List.iter (fun voisin ->
    if c.(voisin) = -1 then begin
      (* Si la couleur n'est pas déjà dans les voisins colorés *)
      if not (List.mem couleur voisins_colores.(voisin)) then begin
        voisins_colores.(voisin) <- couleur :: voisins_colores.(voisin);
        modifications := (voisin, dsat.(voisin)) :: !modifications;
        dsat.(voisin) <- dsat.(voisin) + 1;
      end
    end
  ) g.(sommet);
  !modifications

let revert_dsat (dsat : int array) (voisins_colores : int list array)
                (modifications : (int * int) list) (sommet : int) (couleur : int) =
  List.iter (fun (voisin, old_dsat) ->
    dsat.(voisin) <- old_dsat;
    voisins_colores.(voisin) <- List.filter ((<>) couleur) voisins_colores.(voisin)
  ) modifications


let dsatur_branch_and_bound (g : graphe) : int =
  let start_time = Unix.gettimeofday () in

  let n = Array.length g in
  let c = Array.make n (-1) in             (* Tableau des couleurs assignées *)
  let dsat = Array.make n 0 in            (* Tableau des degrés de saturation *)
  let voisins_colores = Array.make n [] in (* Liste des couleurs des voisins pour chaque sommet *)
  let chi = ref n in                      (* Borne supérieure pour le nombre de couleurs *)
  let sommets_colories = ref 0 in         (* Compteur de sommets coloriés *)

  (* Fonction récursive de backtracking avec pruning *)
  let rec branch_and_bound (k : int) =
    if k >= !chi then () (* Pruning : on ne continue pas si k dépasse chi *)
    else if !sommets_colories = n then
      chi := min !chi k (* Mise à jour de la borne supérieure *)
    else begin
      let s = sommet_saturation_max g c dsat in
      if s <> -1 then begin
        (* Créer la liste des couleurs interdites pour ce sommet *)
        let couleurs_interdites = List.fold_left (fun acc voisin ->
          if c.(voisin) <> -1 then c.(voisin) :: acc else acc
        ) [] g.(s) in

        (* Essayer toutes les couleurs admissibles *)
        for couleur = 0 to k do
          if not (List.mem couleur couleurs_interdites) then begin
            (* Assigner la couleur au sommet s *)
            c.(s) <- couleur;
            incr sommets_colories;

            (* Mise à jour et mémorisation des modifications *)
            let modifications = update_dsat g c dsat voisins_colores s couleur in

            (* Appel récursif avec la nouvelle valeur de k *)
            branch_and_bound (max k (couleur + 1));

            (* Backtracking : annuler l'affectation et les modifications *)
            revert_dsat dsat voisins_colores modifications s couleur;
            c.(s) <- -1;
            decr sommets_colories;
          end
        done;
      end
    end
  in

  (* Lancement de la recherche *)
  branch_and_bound 0;

  (* Temps d'exécution *)
  let end_time = Unix.gettimeofday () in
  Printf.printf "Temps d'exécution : %.6f secondes\n" (end_time -. start_time);
  !chi


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

let desorienter_graphe (g : graphe) : graphe =
  let n = Array.length g in
  let g_non_oriente = Array.make n [] in
  for i = 0 to n - 1 do
    List.iter (fun voisin -> (* jouter l'arrete i -> voisin *)
      if not (List.exists (fun x -> x = voisin) g_non_oriente.(i)) then
        g_non_oriente.(i) <- voisin :: g_non_oriente.(i); (* Ajouter l'arrete voisin -> i *)
      if not (List.exists (fun x -> x = i) g_non_oriente.(voisin)) then
        g_non_oriente.(voisin) <- i :: g_non_oriente.(voisin)
    ) g.(i)
  done;
  g_non_oriente;;



(* TEST *)

(* Graphe complet à 5 sommets *)
let graphe_complet_k5 = [|
  [1; 2; 3; 4]; (* 0 *)
  [0; 2; 3; 4]; (* 1 *)
  [0; 1; 3; 4]; (* 2 *)
  [0; 1; 2; 4]; (* 3 *)
  [0; 1; 2; 3]  (* 4 *)
|]

(* Graphe biparti complet K3,3 *)
let graphe_biparti_k33 = [|
  [3; 4; 5]; (* 0 *)
  [3; 4; 5]; (* 1 *)
  [3; 4; 5]; (* 2 *)
  [0; 1; 2]; (* 3 *)
  [0; 1; 2]; (* 4 *)
  [0; 1; 2]  (* 5 *)
|]

(* Graphe cycle impair C5 *)
let graphe_cycle_c5 = [|
  [1; 4]; (* 0 *)
  [0; 2]; (* 1 *)
  [1; 3]; (* 2 *)
  [2; 4]; (* 3 *)
  [0; 3]  (* 4 *)
|]

(* Graphe en étoile à 6 sommets *)
let graphe_etoile = [|
  [1; 2; 3; 4; 5]; (* 0 *)
  [0];             (* 1 *)
  [0];             (* 2 *)
  [0];             (* 3 *)
  [0];             (* 4 *)
  [0]              (* 5 *)
|]

(* Graphe arbre binaire *)
let graphe_arbre_binaire = [|
  [1; 2]; (* 0 *)
  [0; 3; 4]; (* 1 *)
  [0; 5; 6]; (* 2 *)
  [1];       (* 3 *)
  [1];       (* 4 *)
  [2];       (* 5 *)
  [2]        (* 6 *)
  |]

let test_graphes () =
  let tester_graphe nom graphe =
    Printf.printf "Test du graphe : %s\n" nom;
    let nombre_couleurs = dsatur_branch_and_bound graphe in
    Printf.printf "Nombre de couleurs utilisé : %d\n" nombre_couleurs;
    Printf.printf "-------------------------------------\n"
  in
  tester_graphe "Graphe complet K5" graphe_complet_k5;
  tester_graphe "Graphe biparti K3,3" graphe_biparti_k33;
  tester_graphe "Cycle impair C5" graphe_cycle_c5;
  tester_graphe "Graphe en étoile" graphe_etoile;
  tester_graphe "Graphe arbre binaire" graphe_arbre_binaire
;;

(* Appel des tests *)
test_graphes ();;

let graphe = lire_graphe "test/test3.col"
let grapheNO = desorienter_graphe graphe;;

dsatur_branch_and_bound grapheNO;;
