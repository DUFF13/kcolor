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

(* Met à jour les valeurs DSAT après avoir colorié un sommet *)
let update_dsat (g : graphe) (c : coloration) (dsat : int array) (voisins_colores : bool array array)
                (sommet : int) (couleur : int) : unit =
  List.iter (fun voisin ->
    if c.(voisin) = -1 then (* Si le voisin n'est pas encore colorié *)
      if not voisins_colores.(voisin).(couleur) then begin
        voisins_colores.(voisin).(couleur) <- true;
        dsat.(voisin) <- dsat.(voisin) + 1;
      end
    ) g.(sommet)



(* Fonction principale pour DSATUR avec Branch and Bound *)
let dsatur_branch_and_bound_optimized (g : graphe) : int =
  let start_time = Unix.gettimeofday () in (* Démarre le chronomètre *)

  let n = Array.length g in
  let c = Array.make n (-1) in (* Tableau des couleurs, -1 pour non colorié *)
  let dsat = Array.make n 0 in (* Tableau des saturations *)
  let voisins_colores = Array.init n (fun _ -> Array.make n false) in (* Tableau des voisins colorés *)
  let chi = ref n in (* Nombre chromatique initial *)
  let sommets_colories = ref 0 in (* Compteur des sommets coloriés *)

  (* Fonction récursive pour effectuer le backtracking *)
  let rec dsatur_recursive (k : int) =
    if !sommets_colories = n then ( (* Tous les sommets sont coloriés *)
      chi := min !chi k; (* Met à jour le nombre chromatique minimal trouvé *)
    ) else (
      let s = sommet_saturation_max_mem g c dsat in
      (* Vérifie si un sommet valide a été trouvé *)
      if s <> -1 then (
        for couleur = 0 to k do
          if couleur < !chi then (
            (* Vérifie si la couleur est valide pour ce sommet *)
            let couleur_possible =
              List.for_all (fun voisin -> c.(voisin) <> couleur) g.(s)
            in
            if couleur_possible then (
              c.(s) <- couleur;
              incr sommets_colories; (* Incrémente le compteur de sommets coloriés *)

              (* Met à jour les saturations dynamiquement *)
              update_dsat g c dsat voisins_colores s couleur;

              (* Appel récursif *)
              dsatur_recursive (max k couleur);

              (* Backtracking : défaire les changements *)
              c.(s) <- -1;
              decr sommets_colories; (* Décrémente le compteur de sommets coloriés *)

              List.iter (fun voisin ->
                if voisin >= 0 && voisin < n then (
                  voisins_colores.(voisin).(couleur) <- false;
                  dsat.(voisin) <- dsat.(voisin) - 1
                )
              ) g.(s)
            )
          )
        done
      )
    )
  in

  (* Appel initial à la fonction récursive *)
  dsatur_recursive 0;

  (* Mesure du temps d'exécution *)
  let end_time = Unix.gettimeofday () in
  let execution_time = end_time -. start_time in
  Printf.printf "Temps d'exécution : %.6f secondes\n" execution_time;

  (* Retourne le nombre chromatique optimal *)
  !chi

let update_dsat (g : graphe) (c : coloration) (dsat : int array) (voisins_colores : int list array)
                (sommet : int) (couleur : int) : unit =
  List.iter (fun voisin ->
    if c.(voisin) = -1 then begin
      (* Si le voisin n'est pas encore colorié et la couleur n'est pas dans ses voisins colorés *)
      if not (List.mem couleur voisins_colores.(voisin)) then begin
        voisins_colores.(voisin) <- couleur :: voisins_colores.(voisin);
        dsat.(voisin) <- dsat.(voisin) + 1;
      end
    end
  ) g.(sommet)

let dsatur_branch_and_bound (g : graphe) : int =
  let start_time = Unix.gettimeofday () in

  let n = Array.length g in
  let c = Array.make n (-1) in
  let dsat = Array.make n 0 in
  let voisins_colores = Array.make n [] in
  let chi = ref n in
  let sommets_colories = ref 0 in

  (* Fonction récursive pour le backtracking *)
  let rec branch_and_bound (k : int) =
    if !sommets_colories = n then
      chi := min !chi k
    else
      let s = sommet_saturation_max g c dsat in
      if s <> -1 then
        for couleur = 0 to k do
          if couleur < !chi && not (List.mem couleur voisins_colores.(s)) then begin
            c.(s) <- couleur;
            incr sommets_colories;

            (* Met à jour les DSAT et les couleurs des voisins *)
            update_dsat g c dsat voisins_colores s couleur;

            (* Appel récursif *)
            branch_and_bound (max k (couleur + 1));

            (* Backtracking *)
            c.(s) <- -1;
            decr sommets_colories;

            (* Retire la couleur des voisins *)
            List.iter (fun voisin ->
              if c.(voisin) = -1 then
                voisins_colores.(voisin) <- List.filter ((<>) couleur) voisins_colores.(voisin)
            ) g.(s)
          end
        done
  in

  (* Lancement de la recherche *)
  branch_and_bound 0;

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

let graphe = lire_graphe "test/test.col"
let grapheNO = desorienter_graphe graphe;;

dsatur_branch_and_bound grapheNO;;

let graphe2 = lire_graphe "test/test_simp
