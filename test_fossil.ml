
open Printf
open Fossil

(**************************************************)
(* Tests unitaires de Outils                 *)
(**************************************************)

 let sRepTest="./Test"

 
(**************************************************)
(* Utilitaire de test                             *)
(**************************************************)
  let debut  iFct =
    printf "Module=%s Fonction=tu_%02d() Test en cours\n" __MODULE__ iFct ;
    flush stdout

  let fin  iFct =
    printf "Module=%s Fonction=tu_%02d() Test ok\n" __MODULE__ iFct ;
    flush stdout

  let rejet iFct iAnomalie =
    let sMsg=sprintf "Module=%s Fonction=tu_%02d() Anomalie=%d" __MODULE__ iFct iAnomalie in
    failwith sMsg 

 (*************************************************)
 (* tu_01                                         *)
 (* creation d'un fichier fossile TREE_FOSSILE    *)
 (* contenant [1411;1422;1433;2544;2545;2656;3767]*)
 (*************************************************)
  let tu_01 () =
    debut 01;

    let sFich_Fossile=sRepTest^"/TREE_FOSSILE" in
    Outils.unlink sFich_Fossile;

    let iMax=4 in   (* longueur de tous les idents *)

    (* creation de l'arbre *)
    let larbre=
      List.fold_left ( fun larbre ident ->
        (* conversion sous la forme d'une chaine *)
        let s=sprintf "%d" ident in
        (*printf "%s\n" s; *)
        (* chaine vers liste d'entier blanc et remplacé par un 0 *)
        let iList=Outils.to_liste_int_0 s in
        (* on ajoute des 0 devant si necessaire pour obtenir la taille iMax *)
        let iListComplet=Outils.complete iList iMax 0 in
  
        Tree.ajout larbre iListComplet
      ) [] [1411;1422;1433;2544;2545;2656;3767];
    in

    (* creation du fossile *)
    to_fossile larbre iMax sFich_Fossile;

    (* ouverture du fichier fossile *)
    let fib=open_in sFich_Fossile in

    (* test sur des nombres du fossile *)
    List.iteri ( fun iRg ident ->
      let sident=sprintf "%d" ident in
      if not (bFind fib sident) then rejet 01 01
    ) [1411;1422;1433;2544;2545;2656;3767];

    (* test sur des nombres n'appartenant pas au fossile *)
    List.iteri ( fun iRg ident ->
      let sident=sprintf "%d" ident in
      if (bFind fib sident) then rejet 01 02
    ) [1412;1522;1463];

    close_in fib; 
  
    (* visualisation du fossile *) 
    (*
    dump sFich_Fossile;
    *)
    fin 01

 (*************************************************)
 (* tu_02                                         *)
 (* creation d'un fichier fossile TREE_FOSSILE    *)
 (* contenant les nombres premiers sur 4 chiffres *)
 (*************************************************)

  module Si    = Set.Make ( struct
    type t                = int 
    let compare           = compare
  end)

  let tu_02 () =
    debut 02;

    let sFich_Fossile=sRepTest^"/TREE_FOSSILE" in
    Outils.unlink sFich_Fossile;

    let iMax=4 in   (* nbre de chiffres des nombres premiers *)
    let iDeb=int_of_float(10.0**(float_of_int(iMax-1))) in
    let iFin=int_of_float(10.0**(float_of_int(iMax)))-1 in

    (* Donne la liste des nombres premiers entre iDeb et iFin *)
    let liElt=Prime.l_prime (iDeb,iFin) in 

    (* creation de l'arbre *)
    let larbre=
      List.fold_left ( fun larbre ident ->
        (* conversion sous la forme d'une chaine *)
        let s=sprintf "%d" ident in
        (*printf "%s\n" s; *)
        (* chaine vers liste d'entier blanc et remplacé par un 0 *)
        let iList=Outils.to_liste_int_0 s in
  
        Tree.ajout larbre iList
      ) [] liElt;
    in

    (* creation du fossile *)
    to_fossile larbre iMax sFich_Fossile;

    (* ouverture du fichier fossile *)
    let fib=open_in sFich_Fossile in

    (* conversion sous la forme d'ensemble *)
    let s = List.fold_right Si.add liElt Si.empty in

    (* test sur tous les nombres entre iDeb et iFin *)
    for iCour=iDeb to iFin 
    do

      let bPrime=Si.mem iCour s in

      let sident=sprintf "%d" iCour in
      let bF=bFind fib sident in

      if bPrime && not(bF) || not(bPrime) && bF then rejet 02 01
    done;

    fin 02


;;
(**************************************************)
(* Enchainement des tests unitaires               *)
(**************************************************)
   tu_01 ();
   tu_02 ();
(*
   tu_03 ();
   tu_04 ();
   tu_05 ();
   tu_06 ();
   tu_07 ();
   tu_08 ();
   tu_09 ();
   tu_10 ();
   tu_11 ();
   tu_12 ();
   tu_13 ();
   tu_14 ();
   tu_15 ();
   tu_16 ();
   tu_17 ();
   tu_18 ();
   tu_19 ();
   tu_20 ();
   tu_21 ();
   tu_22 ();
*)
