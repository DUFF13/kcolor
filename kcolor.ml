#load "unix.cma";;


type voisin = int list;;




type graphe = voisin array;;

type coloration = int array;;


let rec taille_liste (l : 'a list) : int = (* O(n) *)
  match l with
  | [] -> 0
  | t :: q -> 1 + taille_liste q;;

let rec pour_tous (f : 'a -> bool)(l : 'a list) : bool =
  match l with
  | [] -> true
  | t :: q -> if f t then pour_tous f q else false;;


let max_array arr =
  let max = ref arr.(0) in
  for i = 1 to Array.length arr - 1 do
    if arr.(i) > !max then
      max := arr.(i)
  done;
  !max;;

let trier_sommets_par_degre (g : graphe) : int array =
  let n = Array.length g in
  let sommets = Array.init n (fun i -> i) in
  Array.sort (fun i j -> compare (List.length g.(j)) (List.length g.(i))) sommets;
  sommets
;;


let coloration_correcte (g : graphe)(c : coloration) : bool = (* O(|A|
  + |S| *)
  let n = Array.length g in
  let rec aux (i : int) =
    match i with
    | i when i = n -> true
    | i -> if pour_tous (fun x -> c.(x) <> c.(i)) g.(i)
           then aux (i + 1)
           else false
  in aux 0;;

let degre_sortant (g : graphe) (noeud : int) : int = List.length g.(noeud);;

let degre_entrant (g : graphe) (noeud : int) : int =
  Array.fold_left
    (fun acc voisins -> if List.mem noeud voisins then acc + 1 else acc)
    0 g

let degres_sommet (g : graphe) (noeud : int) : int =
  (degre_sortant g noeud) + (degre_entrant g noeud)

let noeud_degre_max (g : graphe) : int = 
  let n = Array.length g in
  let max_noeud = ref 0 in
  let max_degre = ref (List.length g.(0)) in
  for i = 1 to n - 1 do
    let degre_i = List.length g.(i) in
    if degre_i > !max_degre then begin
      max_degre := degre_i;
      max_noeud := i
    end
  done;
  !max_noeud;;


let rec exists (f : 'a -> bool)(l : 'a list) : bool =
  match l with
  | [] -> false
  | t :: q -> if f t then true else exists f q;;


let dsat (g : graphe)(c : coloration)(s : int) : int =
  let rec aux (l : int list) =
    match l with
    | [] -> 0
    | t :: q when c.(t) = -1 -> aux q
    | t :: q when exists (fun j -> c.(j) = c.(t)) q -> aux q
    | _ :: q -> 1 + aux q
  in aux g.(s);;


let trier_noeuds_par_dsat (g : graphe)(c : coloration) : int array =
  let n = Array.length g in
  let degres = Array.init n (fun i -> dsat g c i) in
  let indices = Array.init n (fun i -> i) in
  Array.sort (fun i j ->
      let dsat_i = degres.(i) in
      let dsat_j = degres.(j) in
      if dsat_i <> dsat_j then
        compare dsat_j dsat_i 
      else
        compare (degres_sommet g j) (degres_sommet g i)) indices;
    indices;; 

let sommet_saturation_max (g : graphe) (c : coloration) : int = (* considere le cas d'égalité *)
  let n = Array.length g in
  let max_sommet = ref (-1) in
  let max_dsat = ref (-1) in
  let max_degre = ref (-1) in

  for i = 0 to n - 1 do
    if c.(i) = -1 then 
      let dsat_i = dsat g c i in
      let degre_i = degres_sommet g i in
      if dsat_i > !max_dsat || (dsat_i = !max_dsat && degre_i > !max_degre) then begin
        max_sommet := i;
        max_dsat := dsat_i;
        max_degre := degre_i;
      end
  done;

  !max_sommet;;
   
let find_first_available_color (g : graphe) (c : coloration) (s : int) : int =
  let voisins = g.(s) in
  let n = Array.length c in
  let couleurs_voisines = Array.make n false in
  List.iter (fun voisin ->
      if c.(voisin) <> -1 then
        couleurs_voisines.(c.(voisin)) <- true
    ) voisins;
  let couleur = ref 0 in
  while !couleur < n && couleurs_voisines.(!couleur) do
    incr couleur
  done;
  !couleur
;;


let dsatur (g : graphe) : int * coloration =
  let start_time = Unix.gettimeofday () in

  let n = Array.length g in
  let c = Array.make n (-1) in (* Initialisation des couleurs à -1 *)

  for _step = 0 to n - 1 do
    let s = sommet_saturation_max g c in
    let couleur = find_first_available_color g c s in
    c.(s) <- couleur
  done;


  let end_time = Unix.gettimeofday () in
  let execution_time = end_time -. start_time in
  Printf.printf "Temps d'exécution : %.6f secondes\n" execution_time;
  
  max_array c + 1, c;;

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


(* Utilisation du tableau pour mettre à jour les sommets *)

let update_dsat (g : graphe) (c : coloration) (dsat : int array) (voisins_colores : bool array array) (sommet : int) (couleur : int) =
  List.iter (fun voisin ->
    if c.(voisin) = -1 then (* Si le voisin n'est pas encore coloré *)
      if not voisins_colores.(voisin).(couleur) then begin
        voisins_colores.(voisin).(couleur) <- true;
        dsat.(voisin) <- dsat.(voisin) + 1
      end
    ) g.(sommet);;

let update_dsat (g : graphe) (c : coloration) (dsat : int array) (voisins_colores : bool array array)
                (sommet : int) (couleur : int) : unit =
  List.iter (fun voisin ->
    if voisin >= 0 && voisin < Array.length c && c.(voisin) = -1 then (* Si le voisin n'est pas encore colorié *)
      if not voisins_colores.(voisin).(couleur) then begin
        voisins_colores.(voisin).(couleur) <- true;
        dsat.(voisin) <- dsat.(voisin) + 1;
        if dsat.(voisin) < 0 then failwith "DSAT incorrect (valeur négative)";
      end
  ) g.(sommet);
;;

let sommet_saturation_max_mem (g : graphe) (c : coloration) (dsat : int array) : int =
  let n = Array.length g in
  let max_sommet = ref (-1) in
  let max_dsat = ref (-1) in
  let max_degre = ref (-1) in
  for i = 0 to n - 1 do
    if c.(i) = -1 then (* Considérer uniquement les sommets non colorés *)
      let dsat_i = dsat.(i) in
      let degre_i = List.length g.(i) in
      if dsat_i > !max_dsat || (dsat_i = !max_dsat && degre_i > !max_degre) then begin
        max_sommet := i;
        max_dsat := dsat_i;
        max_degre := degre_i;
      end
  done;
  !max_sommet;;

let sommet_saturation_max_mem (g : graphe) (c : coloration) (dsat : int array) : int =
  let n = Array.length g in
  let max_sommet = ref (-1) in
  let max_dsat = ref (-1) in
  let max_degre = ref (-1) in
  for i = 0 to n - 1 do
    if c.(i) = -1 then (* Considérer uniquement les sommets non colorés *)
      let dsat_i = dsat.(i) in
      let degre_i = List.length g.(i) in
      if dsat_i > !max_dsat || (* Priorité à DSAT *)
         (dsat_i = !max_dsat && degre_i > !max_degre) then begin (* Égalité : prioriser le degré *)
        max_sommet := i;
        max_dsat := dsat_i;
        max_degre := degre_i;
      end
  done;
  if !max_sommet = -1 then failwith "Aucun sommet non colorié trouvé dans sommet_saturation_max_mem";
  (* Printf.printf "Sommet choisi : %d, DSAT : %d, Degré : %d\n" !max_sommet !max_dsat !max_degre; *)
  !max_sommet
;;



let dsatur_mem (g : graphe) : int * coloration =
  let start_time = Unix.gettimeofday () in

  let n = Array.length g in
  let c = Array.make n (-1) in (* Initialisation des couleurs à -1 *)
  let dsat = Array.make n 0 in (* Initialisation des saturations à 0 *)
  let voisins_colores = Array.init n (fun _ -> Array.make n false) in (* Tableau des couleurs voisines *)

  for _step = 0 to n - 1 do
    let s = sommet_saturation_max_mem g c dsat in
    let couleur = find_first_available_color g c s in
    c.(s) <- couleur;
    update_dsat g c dsat voisins_colores s couleur
  done;

  let end_time = Unix.gettimeofday () in
  let execution_time = end_time -. start_time in
  Printf.printf "Temps d'exécution : %.6f secondes\n" execution_time;

  max_array c + 1, c;;


let dsatur_nombre_chromatique (g : graphe) : int =
  let start_time = Unix.gettimeofday () in

  let n = Array.length g in
  let dsat = Array.make n 0 in 
  let used_colors = ref 0 in 
  let c = Array.make n (-1) in 
  let voisins_colores = Array.init n (fun _ -> Array.make n false) in 

  let sommets_triees = trier_sommets_par_degre g in

  (* Coloration des sommets *)
  Array.iter (fun s ->
    let couleur = find_first_available_color g c s in (* Réutilisation de la fonction auxiliaire *)
    c.(s) <- couleur;
    if couleur = !used_colors then incr used_colors; (* maj si une nouvelle couleur est utilisée *)

    update_dsat g c dsat voisins_colores s couleur
  ) sommets_triees;


  let end_time = Unix.gettimeofday () in
  let execution_time = end_time -. start_time in
  Printf.printf "Temps d'exécution : %.6f secondes\n" execution_time;

  !used_colors
;;

(* Tri des sommets restants par DSAT décroissant et degré décroissant *)
let trier_sommets_dsat (g : graphe) (dsat : int array) (sommets_restants : int list) : int list =
  List.sort (fun i j ->
    let dsat_i = dsat.(i) and dsat_j = dsat.(j) in
    if dsat_i <> dsat_j then compare dsat_j dsat_i
    else compare (List.length g.(j)) (List.length g.(i))
  ) sommets_restants
;;

let dsatur_branch_and_bound (g : graphe) : int =
  let start_time = Unix.gettimeofday () in

  
  let n = Array.length g in
  let c = Array.make n (-1) in (* Tableau des couleurs, -1 pour non colorié *)
  let chi = ref n in (* Nombre chromatique initial  *)

  (* Fonction récursive pour effectuer le backtracking *)
  let rec dsatur_recursive (k : int) =
    if Array.for_all (fun color -> color <> -1) c then
      chi := min !chi k
    else
      let s = sommet_saturation_max g c in
      for couleur = 1 to k + 1 do
        if couleur <= !chi then (* éviter d'explorer des solutions inutiles *)
          let couleur_possible =
            List.for_all (fun voisin -> c.(voisin) <> couleur) g.(s)
          in
          if couleur_possible then (
            c.(s) <- couleur; 
            dsatur_recursive (max k couleur);
            c.(s) <- -1 
          )
      done
  in

  dsatur_recursive 0;

  let end_time = Unix.gettimeofday () in
  let execution_time = end_time -. start_time in
  Printf.printf "Temps d'exécution : %.6f secondes\n" execution_time;
  
  !chi;;


let dsatur_branch_and_bound (g : graphe) : int =
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
      for couleur = 0 to k do
        if couleur < !chi then (
          (* Vérifie si la couleur est valide pour ce sommet *)
          let couleur_possible =
            List.for_all (fun voisin -> c.(voisin) <> couleur) g.(s)
          in
          if couleur_possible then (
            c.(s) <- couleur;
            incr sommets_colories; (* Incrémente le compteur de sommets coloriés *)

            (* Log pour débogage *)
            Printf.printf "Sommet %d colorié avec %d. Sommets coloriés : %d\n" s couleur !sommets_colories;

            (* Met à jour les saturations dynamiquement *)
            update_dsat g c dsat voisins_colores s couleur;

            (* Appel récursif *)
            dsatur_recursive (max k couleur);

            (* Backtracking : défaire les changements *)
            c.(s) <- -1;
            decr sommets_colories; (* Décrémente le compteur de sommets coloriés *)
            Printf.printf "Backtracking : sommet = %d. Sommets coloriés : %d\n" s !sommets_colories;

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
  in

  (* Appel initial à la fonction récursive *)
  dsatur_recursive 0;

  (* Mesure du temps d'exécution *)
  let end_time = Unix.gettimeofday () in
  let execution_time = end_time -. start_time in
  Printf.printf "Temps d'exécution : %.6f secondes\n" execution_time;

  (* Retourne le nombre chromatique optimal *)
  !chi
;;

let rec dsatur_recursion (g : graphe) (c : coloration) (dsat : int array) (voisins_colores : bool array array)
                     (feasible_colors : bool array array) (chi : int ref) : unit =
  let n = Array.length g in

  (* Cas de base : tous les sommets sont coloriés *)
  if Array.for_all (fun color -> color <> -1) c then (
    let k = Array.fold_left max 0 c + 1 in
    if k < !chi then (
      chi := k;
      Printf.printf "Nouvelle solution : %d couleurs\n" k;
    )
  ) else (
    (* Calcul de la borne inférieure locale *)
    let local_lower_bound = Array.fold_left max 0 dsat in
    if local_lower_bound >= !chi then (
      (* Abandonner la branche *)
      ()
    ) else (
      (* Sélection d’un sommet avec DSAT maximal *)
      let s = sommet_saturation_max_mem g c dsat in
      assert (s >= 0 && s < n); (* Vérifie que le sommet est valide *)

      (* Essayer toutes les couleurs possibles *)
      for couleur = 0 to !chi - 1 do
        if List.for_all (fun voisin -> c.(voisin) <> couleur) g.(s) then (
          (* Colorier le sommet *)
          c.(s) <- couleur;

          (* Mettre à jour les saturations *)
          update_dsat g c dsat voisins_colores s couleur;
          (* Après chaque mise à jour de DSAT *)
          Printf.printf "DSAT après mise à jour : [%s]\n"
            (String.concat "; " (Array.to_list (Array.map string_of_int dsat)));

          (* Suivi de la coloration *)
          Printf.printf "Coloration actuelle : [%s]\n"
            (String.concat "; " (Array.to_list (Array.map string_of_int c)));

          (* Descente récursive *)
          dsatur_recursion g c dsat voisins_colores feasible_colors chi;

          (* Backtracking *)
          c.(s) <- -1;
          List.iter (fun voisin ->
              assert (voisin >= 0 && voisin < Array.length voisins_colores); (* Vérifie que le voisin est valide *)
            if c.(voisin) = -1 then (
              voisins_colores.(voisin).(couleur) <- false;
              dsat.(voisin) <- dsat.(voisin) - 1;
            )
          ) g.(s)
        )
      done;

      (* Essayer une nouvelle couleur si nécessaire *)
      if Array.fold_left max 0 c + 1 < !chi then (
        c.(s) <- Array.fold_left max 0 c + 1;
        update_dsat g c dsat voisins_colores s (Array.fold_left max 0 c);
        dsatur_recursion g c dsat voisins_colores feasible_colors chi;
        c.(s) <- -1
      )
    )
  )
;;

(* Fonction principale pour branch and bound avec DSATUR *)
let dsatur_branch_and_bound (g : graphe) : int =
  let n = Array.length g in
  let c = Array.make n (-1) in (* Initialisation des couleurs *)
  let dsat = Array.make n 0 in (* Tableau des saturations *)
  let voisins_colores = Array.init n (fun _ -> Array.make n false) in
  let feasible_colors = Array.init n (fun _ -> Array.make n false) in (* Couleurs possibles *)
  let chi = ref n in (* Borne supérieure initiale *)

  (* Appel de la récursion *)
  dsatur_recursion g c dsat voisins_colores feasible_colors chi;

  (* Retourne le nombre chromatique optimal *)
  !chi
;;

let graphe_test = [|
  [1; 2];
  [0; 2];
  [0; 1]
|];;

let chi = dsatur_branch_and_bound graphe_test;;
Printf.printf "Nombre chromatique optimal : %d\n" chi;;



let graphe5 = lire_graphe "test2.col";;
let graphe6 = desorienter_graphe graphe5;;

let graphe_simple = lire_graphe "test/test_simple.col";;
let graphe_simpleNO = desorienter_graphe graphe_simple;;

dsatur graphe_simpleNO;;
dsatur_branch_and_bound graphe_simpleNO;;
dsatur_mem graphe_simpleNO;;

dsatur_mem graphe6;;
