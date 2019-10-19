
open Printf
open Outils
open Prime
(**************************************************)
(* Tests unitaires de Outils                 *)
(**************************************************)

 let sRepTest="./Test"

 
(**************************************************)
(* Utilitaire de test                             *)
(**************************************************)
  let debut  iFct =
    printf "Module=%s Fonction=tu_%02d() Test en cours\n" __MODULE__ iFct 

  let fin  iFct =
    printf "Module=%s Fonction=tu_%02d() Test ok\n" __MODULE__ iFct 

  let rejet iFct iAnomalie =
    let sMsg=sprintf "Module=%s Fonction=tu_%02d() Anomalie=%d" __MODULE__ iFct iAnomalie in
    failwith sMsg 

 (************************************************)
 (* tu_01                               *)
 (************************************************)
  let tu_01 () =
    debut 01;

    List.iter ( fun i ->
      if not (is_prime i) then rejet 01 i
    ) [2;5;11;131;3623;3251;2441];

    List.iter ( fun i ->
      if (is_prime i) then rejet 01 i
    ) [4;375;319;333];

    fin 01

 (************************************************)
 (* tu_02                               *)
 (************************************************)
 (* test de l'opérateur ope *)
  let tu_02 () =
    debut 02;

    (* Somme des i de 1 à 2 *)
    let ret=ope (+) id [1;2] in
    if ret<>(Some 3) then rejet 02 1;

    (* Produit pour i de 1 à 2 des (i+1) *)
    let ret=ope ( * ) (fun x -> x+1) [1;2] in
    if ret<>(Some 6) then rejet 02 2;

    (* Somme des i^2 de 1 à 2 *)
    let ret=ope (+) (fun x -> x*x) [1;2] in
    if ret<>(Some 5) then rejet 02 3;

    (* Concaténation de chaine en ajoutant le séparateur , *)
    let ret=ope (sprintf "%s,%s") id ["1";"2"] in
    if ret<>(Some "1,2") then rejet 02 4;

    let ret=ope (sprintf "%s,%s") id [] in
    if ret<>None then rejet 02 5;

    let ret=ope (sprintf "%s,%s") id ["1"] in
    if ret<>(Some "1") then rejet 02 6;

    fin 02

 (************************************************)
 (* tu_03                               *)
 (************************************************)
 (* to_liste_char *)
  let tu_03 () =
    debut 03;

    let lc=to_liste_char "123" in
    if lc<>['1'; '2'; '3'] then rejet 03 1;

    fin 03


 (************************************************)
 (* tu_04                               *)
 (************************************************)
 (* test de list_to_string_add_sep *) 
  let tu_04 () =
    debut 04;

    let s=list_to_string_add_sep "," (sprintf "%d") [1;2;3] in
    if s<>"1,2,3" then rejet 04 1;

    let s=list_to_string_add_sep "," (sprintf "%d") [] in
    if s<>"" then rejet 04 2;

    fin 04

 (************************************************)
 (* tu_05                               *)
 (************************************************)
 (* to_liste_int *)
  let tu_05 () =
    debut 05;

    let li=to_liste_int "12980123" in
    if li<>[1; 2; 9; 8; 0; 1; 2; 3] then rejet 05 1;

    fin 05

 (************************************************)
 (* tu_06                               *)
 (************************************************)
 (* to_liste_int *)
  let tu_06 () =
    debut 06;

    let li=to_liste_int_0 "  1298" in
    if li<>[0; 0; 1; 2; 9; 8] then rejet 06 1;

    fin 06

 (************************************************)
 (* tu_07                               *)
 (************************************************)
 (* int_to_liste_int *)
  let tu_07 () =
    debut 07;

    let li=int_to_liste_int 0 in
    if li<>[0] then rejet 07 1;

    let li=int_to_liste_int 12 in
    if li<>[1;2] then rejet 07 2;

    let li=int_to_liste_int 129808976897 in
    if li<>[1; 2; 9; 8; 0; 8; 9; 7; 6; 8; 9; 7] then rejet 07 3;
    fin 07

 (************************************************)
 (* tu_08                               *)
 (************************************************)
 (* int_of_liste_int *)
  let tu_08 () =
    debut 08;

    let i=int_of_liste_int [1; 2; 9; 8; 0; 8; 9; 7; 6; 8; 9; 7] in
    if i<> 129808976897 then rejet 08 1;

    fin 08

 (************************************************)
 (* tu_09                               *)
 (************************************************)
 (* complete *) 
  let tu_09 () =
    debut 09;

    let li=complete [1;2] 5 0 in
    if li<> [0; 0; 0; 1; 2] then rejet 09 1;

    fin 09

 (************************************************)
 (* tu_10                               *)
 (************************************************)
 (* string_of_char *) 
  let tu_10 () =
    debut 10;

    let s=string_of_char [] in
    if s<>"" then rejet 10 1;

    let s=string_of_char ['a';'1';'e'] in
    if s<>"a1e" then rejet 10 2;

    fin 10

 (************************************************)
 (* tu_11                               *)
 (************************************************)
 (* enumFor *) 
  let tu_11 () =
    debut 11;

    let li=enumFor 0 10 in
    if li<>[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10] then rejet 11 1;

    let li=enumFor 5 1 in
    if li<>[5; 4; 3; 2; 1] then rejet 11 2;

    let li=enumFor 2 2 in
    if li<>[2] then rejet 11 3;

    fin 11

 (************************************************)
 (* tu_12                               *)
 (************************************************)
 (* position *) 
  let tu_12 () =
    debut 12;

    let i=position [1;2;8;9] 9 in
    if i<>Some 3 then rejet 12 1;

    let i=position [1] 3 in
    if i<>None then rejet 12 2;

    fin 12

 (************************************************)
 (* tu_13                               *)
 (************************************************)
 (* dedouble *) 
  let tu_13 () =
    debut 13;

    let l=dedouble [] in
    if l<>[] then rejet 13 1;

    let l=dedouble [1] in
    if l<>[] then rejet 13 2;

    let l=dedouble [1;2] in
    if l<>[(1, 2)] then rejet 13 3;

    let l=dedouble [1;2;3] in
    if l<>[(1, 2); (2, 3)] then rejet 13 4;

    let l=dedouble [1;2;3;4] in
    if l<>[(1, 2); (2, 3);(3, 4)] then rejet 13 5;

    fin 13

 (************************************************)
 (* tu_14                               *)
 (************************************************)
 (* reduce *) 
  let tu_14 () =
    debut 14;

    let i=reduce (+) [1;2;8;9]  in
    if i<>Some 20 then rejet 14 1;

    fin 14

 (************************************************)
 (* tu_15                               *)
 (************************************************)
 (* numerote *) 
  let tu_15 () =
    debut 15;

    let li=numerote [1;2;8;9]  in
    if li<>[(0,1);(1,2);(2,8);(3,9)]  then rejet 15 1;

    fin 15

 (************************************************)
 (* tu_16                               *)
 (************************************************)
 (* doublon *) 
  let tu_16 () =
    debut 16;

    let li=doublon [1;2;8;2;9]  in
    if li<>[1;8;2;9]  then rejet 16 1;

    fin 16


(**************************************************)
(* Enchainement des tests unitaires               *)
(**************************************************)
let ()= 
   tu_01 ();
   tu_02 ();
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
   () 
(*
   tu_17 ();
   tu_18 ();
   tu_19 ();
   tu_20 ();
   tu_21 ();
   tu_22 ();
*)
