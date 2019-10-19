open Printf
(*type                                          *)
type t_arbre= N of int * t_arbre list

type tree = {
  iMax           : int;
  larbre         : t_arbre list;
}

let tree_vide={iMax=0;larbre=[]}

(*Calcul du nombre de noeud de l'arbre          *)
let rec nbNoeud larbre =
 let rec nbNoeudIter inbNoeud = function
   | N(ival,larbre)    -> (inbNoeud+1) + (nbNoeud larbre)
 in
 List.fold_left ( fun inbNoeud arbre ->
   nbNoeudIter inbNoeud  arbre
 ) 0 larbre

(* nombre de feuille                       *)
let rec nbFeuille = function
              [] -> 0
    |N(_,[])::tl -> 1 + (nbFeuille tl)
    |N(_,larbreN)::tl -> (nbFeuille larbreN) + (nbFeuille tl)

(* affichage de l'arbre *)
let rec print_arbre iDecalage = function
   N(ival,larbre)    ->
   printf "%*s" iDecalage "";
   printf "%d\n" ival;flush stdout;
   List.iter ( fun arbre-> print_arbre (iDecalage+1) arbre ) larbre

let print tree =
  List.iter (   fun arbre ->
    print_arbre 0 arbre;
    (*Printf.printf "\n"*)
  )tree.larbre

(* fold_left                                    *)
let fold_left f a tree =

  let rec fold_left acc a =function
    | N(ival,[])     -> 
        f a (List.rev (ival::acc))  
    | N(ival,larbre) -> 
        List.fold_left (fold_left (ival::acc)) a larbre
  in 

  List.fold_left (fold_left []) a tree.larbre

(* iter                                         *)
let iter f tree =
  ignore (fold_left (fun a t -> f t;a) [] tree )

(*Recherche d'un ident sous forme de string dans un arbre            *)
let bFindTree iMax sident larbre =

  let rec bFindTreeIter iListComplet larbre =
   match iListComplet with
     []         -> true
    | iHead::tl ->
       List.exists (fun (N(i,larbreSuiv)) ->
        i=iHead
           &&
        bFindTreeIter tl larbreSuiv
       )larbre
  in

  let iList=Outils.to_liste_int sident in
  (* on ajoute des 0 devant si necessaire *)
  let iListComplet=Outils.complete iList iMax 0 in
  bFindTreeIter iListComplet larbre

(*Ecriture de l'arbre dans un fichier Marshal   *)
let to_marshal tree sNom = 
  let fd=Unix.openfile sNom [Unix.O_WRONLY;Unix.O_TRUNC;Unix.O_CREAT] 511 in
  let db=Unix.out_channel_of_descr fd in
  set_binary_mode_out db true;
  Marshal.to_channel db tree [];
  close_out db

(*lecture de l'arbre a partir d'un fichier Marshal *)
let of_marshal sNom =
  try (
    let fd=Unix.openfile sNom [Unix.O_RDONLY] 511 in
    let db=Unix.in_channel_of_descr fd in
    set_binary_mode_in db true;
    let tree=(Marshal.from_channel db: tree) in
    close_in db;
    (true,tree)
  ) with _ -> (false,tree_vide)

(* sort                                         *)
let sort larbre =
  List.sort (fun (N(i1,_)) (N(i2,_)) -> i1-i2  )larbre

(* ajout d'une liste d'int à un arbre           *)
let rec ajout larbre = function
  | []        -> larbre
  | ihd::tl ->
     (*Printf.printf "(%d)\n" ihd;*)
     let (bTrouve,larbreS)=
      (* on regarde si un N de la liste contient ihd *)
      List.fold_left ( fun (bTrouve,larbreS) (N(iN,larbreN) as arbre) ->
       if bTrouve then (true,sort (arbre::larbreS))
       else (
            if iN=ihd then
             (true,sort (N(iN,sort (ajout larbreN tl))::larbreS))
            else
             (false,sort (arbre::larbreS))
       )
      ) (false,[]) larbre
     in
     (*Printf.printf "(%b)\n" bTrouve;*)
     if bTrouve then
      larbreS
     else (
       sort (N(ihd,ajout [] tl)::larbre)
     )
 
