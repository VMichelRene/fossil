
open Printf
open Bit

(**************************************************)
(* Tests unitaires de bits                 *)
(**************************************************)

 
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
 (* shift to left                                 *)
 (*************************************************)
  let tu_01 () =
    debut 01;

    let r=pos_l (BitPos [0;2]) 3 in
    if r<> BitPos [3; 5] then rejet 01 01;

    fin 01

 (*************************************************)
 (* tu_02                                         *)
 (* shift to right                                *)
 (*************************************************)
  let tu_02 () =
    debut 02;

    let r=pos_r (BitPos [3;5]) 3 in
    if r<> BitPos [2; 0] then rejet 02 01;

    let r=pos_r (BitPos [3;5]) 4 in
    if r<> BitPos [1] then rejet 02 02;

    fin 02

 (*************************************************)
 (* tu_03                                         *)
 (*************************************************)
  let tu_03 () =
    debut 03;

    (* 2^0+2^4+2^7 = 1 + 16 + 128 *)
    let li=pos_to_puiss (BitPos [0;4;7]) in
    if li<> BitPuiss [1;16;128] then rejet 03 01;

    fin 03

 (*************************************************)
 (* tu_04                                         *)
 (*************************************************)
  let tu_04 () =
    debut 04;

    (* 2^0+2^4+2^7 = 145 *)
    let i=puiss_to_int (BitPuiss [1;16;128]) in
    if i<> 145 then rejet 04 01;

    fin 04

 (*************************************************)
 (* tu_05                                         *)
 (*************************************************)
  let tu_05 () =
    debut 05;

    (* 2^1+2^3+2^9 = 522 *)
    let i=pos_to_int (BitPos [1;3;9]) in
    if i<> 522 then rejet 05 01;

    fin 05

 (*************************************************)
 (* tu_06                                         *)
 (*************************************************)
  let tu_06 () =
    debut 06;

    (* we keep 4 bits on right *)
    let lgld=bloc_decoupe ([1;16;128]) 4 in
    if lgld<>([128;16], [1]) then rejet 06 01;

    fin 06

;;
(**************************************************)
(* Enchainement des tests unitaires               *)
(**************************************************)
   tu_01 ();
   tu_02 ();
   tu_03 ();
   tu_04 ();
   tu_05 ();
   tu_06 ();
(*
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
