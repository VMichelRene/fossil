
open Printf
open File_bit
(**************************************************)
(* Tests unitaires de File_bit                 *)
(* Gestion bit à bit d'un fichier en lecture ecriture seek *)
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
 (* Controle de l'entrée sortie sur un fichier binaire *)
 (* tu_01                               *)
 (* test d'écriture d'un octet en plusieurs fois *)
 (* lecture d'un octet                           *)
 (************************************************)
  let tu_01 () =
    debut 01;

    let sNom=sRepTest^"/Fich_01" in
    Outils.unlink sNom;

    let io=ouvrir sNom in 
    write io 2 3;
    write io 2 1;
    write io 4 9;
    close io;

    let io=ouvrir sNom in 
    let a=read io 8 in
    if a<>217 then rejet 01 1;

    Outils.unlink sNom;
    fin 01

 (************************************************)
 (* Controle de l'entrée sortie sur un fichier binaire *)
 (* tu_02                               *)
 (* test d'écriture de 10bits  en plusieurs fois *)
 (* lecture d'un octet                           *)
 (************************************************)
  let tu_02 () =
    debut 02;

    let sNom=sRepTest^"/Fich_02" in
    Outils.unlink sNom;

    let io=ouvrir sNom in 
    write io 2 3;
    write io 2 1;
    write io 4 9;
    write io 2 2;
    close io;

    let io=ouvrir sNom in 
    let a=read io 10 in
    if a<>870 then rejet 02 1;

    Outils.unlink sNom;
    fin 02 

 (************************************************)
 (* Controle de l'entrée sortie sur un fichier binaire *)
 (* tu_03                               *)
 (* test d'écriture de 10bits  en plusieurs fois *)
 (* lecture de 10 bits en 2 fois                 *)
 (*
    1 1 0 1 1 0 0 1 1 0 0 0 0 0 0 0 
    --- --- ------- ---
     2   2     4     2                   en ecriture
    --- ---------------
     2         8                         en lecture 
 *)
 (************************************************)
  let tu_03 () =
    debut 03;

    let sNom=sRepTest^"/Fich_03" in
    Outils.unlink sNom;

    let io=ouvrir sNom in 
    write io 2 3;
    write io 2 1;
    write io 4 9;
    write io 2 2;
    close io;

    let io=ouvrir sNom in 
    let a=read io 2 in
    if a<>3 then rejet 03 1;
    let a=read io 8 in

    if a<>102 then rejet 03 2;

    Outils.unlink sNom;
    fin 03


 (************************************************)
 (* Controle de l'entrée sortie sur un fichier binaire *)
 (* tu_04                               *)
 (* test de charge                               *)
 (************************************************)
  let tu_04 () =
    debut 04;

    let sNom=sRepTest^"/Fich_04" in
    Outils.unlink sNom;

    let io=ouvrir sNom in 
    write io 10 999;
    write io 10 682;
    close io;

    let io=ouvrir sNom in 
    let a=read io 10 in
    if a<>999 then rejet 04 1; 
    let a=read io 10 in
    if a<>682 then rejet 04 2;

    Outils.unlink sNom;
    fin 04 


 (************************************************)
 (* Controle de l'entrée sortie sur un fichier binaire *)
 (* tu_05                               *)
 (* test du write rewind read                    *)
 (************************************************)
  let tu_05 () =
    debut 05;

    let sNom=sRepTest^"/Fich_05" in
    Outils.unlink sNom;

    let io=ouvrir sNom in 
    write io 10 999;
    rewind io; 
    let a=read io 10 in
    if a<>999 then rejet 05 1;

    close io;

    Outils.unlink sNom;
    fin 05


 (************************************************)
 (* Controle de l'entrée sortie sur un fichier binaire *)
 (* tu_06                               *)
 (* test du write seek read                    *)
 (*
   (999)base 10= (1 1 1 1 1 0 0 1 1 1) base 2 
 *)
 (************************************************)
  let tu_06 () =
    debut 06;

    let sNom=sRepTest^"/Fich_06" in
    Outils.unlink sNom;

    let io=ouvrir sNom in 
    write io 10 999;
    seek io 2; 
    let a=read io 8 in
    if a<>231 then rejet 06 1;
    close io;

    Outils.unlink sNom;
    fin 06


 (************************************************)
 (* Controle de l'entrée sortie sur un fichier binaire *)
 (* tu_07                               *)
 (* test du write seek write                     *)
 (*
   (999)base 10= (1 1 1 1 1 0 0 1 1 1) base 2 
   1 1 1 1  1 0 0 1  1 1 0 0  0 0 0 0     = (371)octal (300)octal / write 10 999

   1 1 0 1  0 1 0 1  0 1 0 1  1 1 1 1     = (325)octal (137)octal = (54623)base 10
       ------------------------------ write 14 5471
            ----------------
 *)
 (************************************************)
  let tu_07 () =
    debut 07;

    let sNom=sRepTest^"/Fich_07" in
    Outils.unlink sNom;

    let io=ouvrir sNom in 
    write io 10 999;
    seek io 2; 
    write io 14 5471;
    close io;
    let io=ouvrir sNom in 
    let a=read io 16 in
    if a<>54623 then rejet 07 1;
    close io;

    Outils.unlink sNom;
    fin 07


 (************************************************)
 (* Controle de l'entrée sortie sur un fichier binaire *)
 (* tu_08                               *)
 (* test du write seek write                     *)
 (*
   (999)base 10= (1 1 1 1 1 0 0 1 1 1) base 2 
   1 1 1 1  1 0 0 1  1 1 0 0  0 0 0 0     = (371)octal (300)octal / write 10 999

   1 1 0 1  0 1 0 1  0 1 0 1  1 1 1 1     = (325)octal (137)octal = (54623)base 10
       ------------------------------ write 14 5471
            ----------------
                              ------- write 4 9
   1 1 0 1  0 1 0 1  0 1 0 1  1 0 0 1     = (325)octal (131)octal = (54617)base 10
 *)
 (************************************************)
  let tu_08 () =
    debut 08;

    let sNom=sRepTest^"/Fich_08" in
    Outils.unlink sNom;

    let io=ouvrir sNom in 
    write io 10 999;
    seek io 2; 
    write io 14 5471;
    rewind io;
    let a=read io 16 in
    if a<>54623 then rejet 08 1; 
    seek io 12; 
    write io 4 9;
    close io;
    let io=ouvrir sNom in 
    let a=read io 16 in
    if a<>54617 then rejet 08 2;
    close io;

    Outils.unlink sNom;
    fin 08

 (************************************************)
 (* Controle de l'entrée sortie sur un fichier binaire *)
 (* tu_09                               *)
 (* test d'ecriture bit à bit                    *)
 (************************************************)
  let tu_09 () =
    debut 09;
    
    let sNom=sRepTest^"/Fich_09" in
    Outils.unlink sNom;

    let io=ouvrir sNom in 
    write io 16 0;

    List.iter ( fun iPos ->
      seek io iPos;
      if iPos mod 2 = 0 then write io 1 1;
    ) (Outils.enumFor 0 15);

    rewind io;
    let a=read io 16 in
    if a<>43690 then rejet 09 1; 

    close io;

    Outils.unlink sNom;
    fin 09

 (************************************************)
 (* Controle de l'entrée sortie sur un fichier binaire *)
 (* tu_10                               *)
 (* test d'ecriture bit à bit                    *)
 (* 
    1 0 1 0 1 0 1 0  1 0 1 0 1 0 1 0   = (43690)base 10
    1 0 1 0 1 0 1 0  0 0 1 0 1 0 1 0   = seek 8, write 1 0 = (252)octal (52)octal = (43562)base 10
                     ^ 
 *)
 (************************************************)
  let tu_10 () =
    debut 10;
    
    let sNom=sRepTest^"/Fich_10" in
    Outils.unlink sNom;

    let io=ouvrir sNom in 
    write io 16 0;

    List.iter ( fun iPos ->
      seek io iPos;
      if iPos mod 2 = 0 then write io 1 1;
    ) (Outils.enumFor 0 15);

    seek io 8;
    write io 1 0;
    rewind io;
    let a=read io 16 in
    if a<>43562 then rejet 10 1; 
    close io;

    Outils.unlink sNom;
    fin 10 


 (************************************************)
 (* Controle de l'entrée sortie sur un fichier binaire *)
 (* tu_11                               *)
 (* test d'ecriture bit à bit                    *)
 (* avec un crible d'Eratosthène                 *)
 (************************************************)
  let tu_11 () =
    let iMax=1000 in
    let iMilieu=iMax/2 -1 in

    let rec cocher io iValBase iValCour=
      let iVal=iValCour+iValBase in
      if iVal < (iMax+2) then (
        let iPos=iVal-2 in 
        seek io iPos;
        write io 1 1;
        cocher io iValBase iVal
      ) 
    in 

    debut 11;
    
    let sNom=sRepTest^"/Fich_11" in
    Outils.unlink sNom;

    let io=ouvrir sNom in 
    write io iMax 0;

    List.iter ( fun iPos ->
      let iVal=iPos+2 in 

      seek io iPos;
      let a=read io 1 in
      if a=0 then cocher io iVal iVal;
    ) (Outils.enumFor 0 iMilieu);

    close io;

    let io=ouvrir sNom in 
    List.iter ( fun iPos ->
      let iVal=iPos+2 in 

      seek io iPos;
      let a=read io 1 in

      let bprime=Prime.is_prime iVal in
      if a=0 && not bprime then rejet 11 1; 
      if a=1 &&     bprime then rejet 11 2; 
    ) (Outils.enumFor 0 (iMax-1));
    close io;

    Outils.unlink sNom;
    fin 11 


 (************************************************)
 (* Controle de l'entrée sortie sur un fichier binaire *)
 (* tu_12                               *)
 (* test d'ecriture bit à bit                    *)
 (* 
    1 1 0 0 0 0 0 0   
 *)
 (************************************************)
  let tu_12 () =
    debut 12;
    
    let sNom=sRepTest^"/Fich_12" in
    Outils.unlink sNom;

    let io=ouvrir sNom in 
    write io 2 3;
    close io;
    let stats=Unix.stat sNom in

    if stats.Unix.st_size<>1 then rejet 12 1; 

    Outils.unlink sNom;
    fin 12 


 (************************************************)
 (* Controle de l'entrée sortie sur un fichier binaire *)
 (* tu_13                               *)
 (* test d'ecriture bit à bit                    *)
 (* 
   1 1 0 0 0 0 0 0   
   tu de la taille du fichier qui ne doit contenir qu'un octet.
 *)
 (************************************************)
  let tu_13 () =
    debut 13;
    
    let sNom=sRepTest^"/Fich_13" in
    Outils.unlink sNom;

    let io=ouvrir sNom in 
    write io 2 3;
    seek io 0;
    let _=read io 2 in
    close io;
    let stats=Unix.stat sNom in
    if stats.Unix.st_size<>1 then rejet 13 1;

    Outils.unlink sNom;
    fin 13

 (************************************************)
 (* Controle de l'entrée sortie sur un fichier binaire *)
 (* Test :
      seek
      write
      read                              
 *) 
 (************************************************)

  let tu_14 () =
    (* 
       0 0 0 0 0 0 0 0  0 0 0 0 0 1 0 0 
  n°   0 1 2 3 4 5 6 7  8 9               numéro du bit 
    *)

    debut 14;

    let sNom=sRepTest^"/Fich_14" in
    Outils.unlink sNom;

    let io=ouvrir sNom in
    (* on se positionne sur le 8 ième bits *)
    seek io 8;
    (* on ecrit le nombre 4 sur 8 bits *)
    write io 8 4;

    seek io 8;
    let a=read io 8 in 
    if a<>4 then rejet 14 1; 

    (* on se positionne sur le 8 ième bits *)
    seek io 8;
    (* on lit 6 bits que l'on transforme en entier *)
    let a=read io 6 in 
    if a<>1 then rejet 14 2; 

    (* lecture d'une zone non renseignée par l'utilisateur *)
    rewind io;
    let a=read io 8 in 
    (* une zone non renseignée contient par défaut 0 *)
    if a<>0 then rejet 14 3; 

    Outils.unlink sNom;
    fin 14


 (************************************************)
 (* seek sur une position qui n'est pas un multiple de 8 *)
 (************************************************)

  let tu_15 () =
    (* 
       0 0 1 0  0 0 1 0 0 1 0 0 

  n°   0 0 0 0  0 0 0 0 0 0 1 1            numéro du bit
       0 1 2 3  4 5 6 7 8 9 0 1            

    Pour visualiser en octal : od -b Test_15 

[mv@calculus IO_a]$ od -b Test_15
0000000 042 100
0000002

  (042)octal=(0 0 1 0 0 0 1 0)base2
  (100)octal=(0 1 0 0 0 0 0 0)base2
On ecrit nécessairement par bloc de 8 bits 
  On ne gère pas une fin de fichier en nombre de bits non multiple de 8.

       0 0 1 0  1 0 1 0 0 1 0 0 

  n°   0 0 0 0  0 0 0 0 0 0 1 1            numéro du bit
       0 1 2 3  4 5 6 7 8 9 0 1            

[mv@calculus IO_a]$ od -b Test_15
0000000 052 100

    *)
    debut 15;

    let sNom=sRepTest^"/Fich_15" in
    Outils.unlink sNom;

    let io=ouvrir sNom in 
    write io 12 (Bit.pos_to_int (Bit.BitPos [2;5;9]));
    close io;
 
    let io=ouvrir sNom in 
    (* on se positionne sur le 4 ième bits *)
    seek io 4;
    (* on ecrit le nombre 164 sur 8 bits *)
    write io 8 (Bit.pos_to_int (Bit.BitPos [2;5;7]));
    close io;

    let io=ouvrir sNom in 
    seek io 4;
    let a=read io 8 in 
    if a<>164 then rejet 15 1;

    Outils.unlink sNom;
    fin 15


 (************************************************)
 (* seek sur une position qui n'est pas un multiple de 8 *)
 (* comme tu_15 mais sans les closes     *)
 (* un write suivi d'un read *)
 (************************************************)

  let tu_16 () =

    debut 16;

    let sNom=sRepTest^"/Fich_16" in
    Outils.unlink sNom;

    let io=ouvrir sNom in 
    write io 12 (Bit.pos_to_int (Bit.BitPos [2;5;9]));
    (* on se positionne sur le 4 ième bits *)
    seek io 4;
    (* on ecrit le nombre 164 sur 8 bits *)
    write io 8 (Bit.pos_to_int (Bit.BitPos [2;5;7]));
    seek io 4;
    let a=read io 8 in 
    if a<>164 then rejet 16 1;

    Outils.unlink sNom;
    fin 16



 (************************************************)
 (* seek sur une position qui n'est pas un multiple de 8 *)
 (************************************************)
  let tu_17 () =
    (* 
       0 0 1 1  0 1 0 0 
  n°   0 1 2 3  4 5 6 7                numéro du bit 

       0 0 1 0  1 1 1 1 
  n°   0 1 2 3  4 5 6 7                numéro du bit 

    Pour visualiser en octal : od -b Test_4 

    *)

    debut  17;

    let sNom=sRepTest^"/Fich_17" in
    Outils.unlink sNom;

    let io=ouvrir sNom  in
    write io 8 52;
    seek io 3;
    write io 5 15;

    seek io 2;
    let a=read io 6 in 
    if a<>47 then rejet 17 1; 

    Outils.unlink sNom;
    fin 17

 (************************************************)
 (* seek et read au cas ou les données ne sont pas sur le disque *)
 (* 0 0 1 0  0 0 0 0                             *)
 (************************************************)
  let tu_18 () =
    debut 18;

    let sNom=sRepTest^"/Fich_18" in
    Outils.unlink sNom;

    let io=ouvrir sNom  in
    write io 1 1;
    rewind io;
    let a=read io 16 in 
    if a<>32768 then rejet 18 1;

    Outils.unlink sNom;
    fin 18 

 (************************************************)
 (* seek et write                                *) 
 (* 0 0 1 0  0 0 0 0                             *)
 (************************************************)
  let tu_19 () =
    debut 19;

    let sNom=sRepTest^"/Fich_19" in
    Outils.unlink sNom;

    let io=ouvrir sNom  in
    seek io 2;
    write io 1 1;
    close io;

    (* controle *)
    let io=ouvrir sNom  in
    let a=read io 3 in 
    if a<>1 then rejet 19 1; 

    Outils.unlink sNom;
    fin 19 

 (************************************************)
 (* seek et write                                *) 
 (* 1 0 0 0  0 1 0 0  0 0 1 0  0 0 0 0           *)
 (*
    od -b Test_20
    0000000 204 040

 *)
 (************************************************)
  let tu_20 () =
    debut  20;

    let sNom=sRepTest^"/Fich_20" in
    Outils.unlink sNom;

    let io=ouvrir sNom  in
    seek io 0;
    write io 1 1;

    seek io 5;
    write io 1 1;

    seek io 10;
    write io 1 1;

    close io;

    (* controle *)
    let io=ouvrir sNom  in
    let a=read io 12 in 
    if a<>2114 then rejet 20 1;

    Outils.unlink sNom;
    fin 20

 (************************************************)
 (* test de recouvrement                         *) 
 (* 
    1 0 1 0  1 0 1 0  1 0 1 0  1 0 1 0  1 0 1 0  1 0 1 0 

    0 1 0 1        
                   1  0 1 
                               0 1 0 1  1 1 1 1

    Dans le fichier on doit avoir
    0 1 0 1  1 0 1 1  0 1 1 0  0 1 0 1  1 1 1 1  1 0 1 0 
 
    od -b Test_21
0000000 133 145 372
 *)
 (************************************************)
  let tu_21 () =
    debut  21;

    let sNom=sRepTest^"/Fich_21" in
    Outils.unlink sNom;

    let io=ouvrir sNom  in
    write io 8 170;
    write io 8 170;
    write io 8 170;

    rewind io;
    write io 4 5;

    seek io 7;
    write io 3 5;

    seek io 12;
    write io 8 95;

    close io;

    (* controle *)
    let io=ouvrir sNom  in
    let a=read io 8 in 
    if a<>91 then rejet 21 1;
    let a=read io 8 in 
    if a<>101 then rejet 21 2;
    let a=read io 8 in 
    if a<>250 then rejet 21 3;

    Outils.unlink sNom;
    fin 21

 (************************************************)
 (* test de recouvrement                         *) 
 (* 
    1 0 1 0  1 0 1 0  1 0 1 0  1 0 1 0  1 0 1 0  1 0 1 0 

    0 1 0 1  0 0 0 1  0 1 0 0  0 1 0 1  1 1 1 1

    Dans le fichier on doit avoir
    0 1 0 1  0 0 0 1  0 1 0 0  0 1 0 1  1 1 1 1  1 0 1 0 
 
    od -b Test_22
0000000 121 105 372
   
 *)
 (************************************************)
  let tu_22 () =
    debut  22;

    let sNom=sRepTest^"/Fich_22" in
    Outils.unlink sNom;

    let io=ouvrir sNom  in
    write io 8 170;
    write io 8 170;
    write io 8 170;

    rewind io;
    write io 20 332895;

    close io;

    (* controle *)
    let io=ouvrir sNom  in
    let a=read io 8 in 
    if a<>81 then rejet 22 1;
    let a=read io 8 in 
    if a<>69 then rejet 22 2;
    let a=read io 8 in 
    if a<>250 then rejet 22 3;

    Outils.unlink sNom;
    fin 22

 (************************************************)
 (* tu_23                                        *)
 (* lecture après la fin de fichier              *)
 (* 0 0 0 0 0 1 0 1                              *)
 (************************************************)
 (* Par defaut on lit des 0 en fin de fichier.
    Cela n'est pas génant, car généralement la structure 
    du fichier est connu.    
    Dans le cas :
      d'un arbre ou d'un tableau on connait la fin.
      d'une machine de Turing le ruban est sans fin.
 *)
  let tu_23 () =
    debut  23;

    let sNom=sRepTest^"/Fich_23" in
    Outils.unlink sNom;

    let io=ouvrir sNom  in
    (* lecture de 5 Bits après la fin de fichier *)
    let a=read io 5 in 
    if a<>0 then rejet 23 1;
    (* ecriture de 1 Bit à 1 *)
    write io 1 1;
    write io 2 1;

    rewind io;
    let a=read io 8 in 
    if a<>5 then rejet 23 2;

    seek io 5;
    let a=read io 3 in 
    if a<>5 then rejet 23 3;

    close io;

    Outils.unlink sNom;
    fin 23

 (************************************************)
 (* tu_24                                        *)
 (* seekCur                                      *)
 (* 0 0 0 0  0 0 0 0  1 0 1                      *)
 (************************************************)
  let tu_24 () =
    debut  24;

    let sNom=sRepTest^"/Fich_24" in
    Outils.unlink sNom;

    let io=ouvrir sNom  in
    (* positionnement sur le deuxième octets *)
    seek io 8;
    write io 3 5;

    seek io 8;
    seekCur io 1;
    let a=read io 2 in 
    if a<>1 then rejet 24 1;
   
    close io;

    Outils.unlink sNom;
    fin 24


 (************************************************)
 (* tu_25                                        *)
 (* Positionnement après la fin de fichier       *)
 (************************************************)
 (* Utile au ruban de la machine de Turing 
 *)
  let tu_25 () =
    debut  25;

    let sNom=sRepTest^"/Fich_25" in
    Outils.unlink sNom;

    let io=ouvrir sNom  in

    seek io 23;
    write io 1 1;

    seek io 16;
    seekCur io 7;
    let a=read io 1 in 
    if a<>1 then rejet 25 1;

    close io;

    Outils.unlink sNom;
    fin 25

  (************************************************)
  (*tu_26 *)
  (************************************************)
  (*
    n°   0 0 0 0 0 0 0 0  0 0 1 1 1 1 1 1 
         0 1 2 3 4 5 6 7  8 9 0 1 2 3 4 5 
         1 1 1 1 1 1 1 1  1 0 1 0 1 1 1 1
      
         Quand on se positionne sur le bits n° 13
         on doit avoir posPrecFaiblePartiel = (1 0 1 0 1)binaire = (4,2,0) pos
             
  *)
  let  tu_26 () =
    debut 26;

    let sNom=sRepTest^"/Fich_26" in
    Outils.unlink sNom;

    (* creation du fichier *)
    let io=ouvrir sNom in
    write io 8 255;
    write io 8 175;
    close io;

    (* lecture pour contrôle *)
    let io=ouvrir sNom in
    seek io 13 ;

    let a=read io 3 in
    (* on doit lire : 1 1 1 *)
    if a<>7 then rejet 26 1;

    close io;
    fin 26


  (************************************************)
  (*tu_27                                         *)
  (************************************************)
  (* 
     Controle de l'entrée sortie sur un fichier binaire
     Test d'écriture et de lecture d'une liste d'octets 
  *)
  let  tu_27 () =
    debut 27;

    let sNom=sRepTest^"/Fich_27" in
    Outils.unlink sNom;

    let liTest=[ 254;227;0;1;12] in
    (* creation du fichier *)
    let io=ouvrir sNom in
    List.iter ( fun iTest ->
      write io 8 iTest;
    ) liTest;
    close io;

    (* lecture pour contrôle *)
    let io=ouvrir sNom in
    let liRes=
      List.map ( fun _ ->
        read io 8
      )liTest
    in
    close io;
    if liRes<>liTest then rejet 27 1;
    fin 27

  (************************************************)
  (*tu_28                                         *)
  (************************************************)
  (* test d'écriture d'une liste de bits                *)
  (************************************************)
  let  tu_28 () =
    debut 28;

    let sNom=sRepTest^"/Fich_28" in
    Outils.unlink sNom;

    (*let liTest=[1;0;1;1;0;0;0;1] in*)
    let iTest=177 in

    (* creation du fichier *)
    let io=ouvrir sNom in
    write_pos io 8 (Bit.int_to_pos iTest);
    close io;

    (* lecture pour contrôle *)
    let io=ouvrir sNom in
    let liRes=
      List.map ( fun iNbBits ->
        let i=read io iNbBits in
        i
      ) [2;3;2;1]
    in
    close io;

    if liRes<>[2;6;0;1] then rejet 28 1;

    fin 28

  (************************************************)
  (*tu_29 *)
  (************************************************)
  (* Controle de l'entrée sortie sur un fichier binaire 
        On se limite à un octet
     test :
       seekCur   seek à partir de la position courante 
       rewind
       seek      seek à partir du debut du fichier     
  *)
  let  tu_29 () =
    debut 29; 

    let sNom=sRepTest^"/Fich_29" in
    Outils.unlink sNom;
    (* 
       1 0 | 1 1 0 | 0 0 1
  n°   0 1   2 3 4   5 6 7     numéro du bit 
    *)
    let iTest=177 in

    (* creation du fichier *)
    let io=ouvrir sNom in
    write_pos io 8 (Bit.int_to_pos iTest);
    close io;

    (* seek et lecture pour contrôle *)
    let io=ouvrir sNom in

    let a=read io 2 in
    (* on doit lire : 1 0 *)
    if a<>2 then rejet 29 1;
    (* on saute 3 bits : 1 1 0 *)
    seekCur io 3; 
    (* on doit lire : 0 0 1 *)
    let a=read io 3 in
    if a<>1 then rejet 29 2;

    (* on rembioine io *)
    rewind io;
    let a=read io 3 in
    (* on doit lire : 1 0 1 *)
    if a<>5 then rejet 29 3;

    (* on rembioine io *)
    rewind io;
    (* on se positionne sur le 3 ième bits *)
    seek io 3; 
    (* on doit lire : 1 0 0 0 1 *)
    let a=read io 5 in
    if a<>17 then rejet 29 4;

    seek io 1; 
    (* on doit lire : 0 1 1 0 0 0 1 *)
    let a=read io 7 in
    if a<>49 then rejet 29 5;

    close io;

    fin 29 

  (************************************************)
  (*tu_30 *)
  (************************************************)
  (* Controle de l'entrée sortie sur un fichier binaire 
        On se limite à un octet
     On realise deux ecritures partielles 
     test :
       write
  *)
  let  tu_30 () =
    debut 30;

    let sNom=sRepTest^"/Fich_30" in
    Outils.unlink sNom;
    (* 
       1 0 | 1 1 0 | 0 0 1
  n°   0 1   2 3 4   5 6 7     numéro du bit 
    *)
    (* creation du fichier *)
    let io=ouvrir sNom in
    write_pos io 2 (Bit.int_to_pos 2);
    write_pos io 3 (Bit.int_to_pos 6);
    write_pos io 3 (Bit.int_to_pos 1);
    close io;

    (* seek et lecture pour contrôle *)
    let io=ouvrir sNom in

    let a=read io 2 in
    (* on doit lire : 1 0 *)
    if a<>2 then rejet 30 1;

    (* on se positionne sur le 2 ième bits *)
    seek io 2; 
    (* on doit lire : 1 1 0 *)
    let a=read io 3 in
    if a<>6 then rejet 30 2;

    (* on se positionne sur le 0 ième bits *)
    seek io 1; 
    (* on doit lire : 0 1 1 0 0 0 1 *)
    let a=read io 7 in
    if a<>49 then rejet 30 3;

    close io;

    fin 30

  (************************************************)
  (*tu_31 *)
  (************************************************)
  (* On ecrase une partie                         *)
  let  tu_31 () =
    debut 31;

    let sNom=sRepTest^"/Fich_31" in
    Outils.unlink sNom;
    (* 
       1 1 | 1 1 1 | 1 1 1
  n°   0 1   2 3 4   5 6 7     numéro du bit 

       0 1 | 1 1 1 | 1 1 1
  n°   0 1   2 3 4   5 6 7     numéro du bit 
    *)

    (* creation du fichier *)
    let io=ouvrir sNom in
    write io 8 255;

    rewind io;
    write io 2 1;
    close io;

    (* seek et lecture pour contrôle *)
    let io=ouvrir sNom in
    let a=read io 8 in
    (* 
      Cette opération ne fonctionne pas mais ruban ne l'utilise pas. 
    *)
    if a<>127 then rejet 31 1;
    close io;

    fin 31

  (************************************************)
  (*tu_32 *)
  (************************************************)
  (* On ecrase une partie mais apres une ouverture *)
  let  tu_32 () =
    debut 32;

    let sNom=sRepTest^"/Fich_32" in
    Outils.unlink sNom;
    (* 
       1 1 | 1 1 1 | 1 1 1
  n°   0 1   2 3 4   5 6 7     numéro du bit 

       0 1 | 1 1 1 | 1 1 1
  n°   0 1   2 3 4   5 6 7     numéro du bit 
    *)

    (* creation du fichier *)
    let io=ouvrir sNom in
    write io 8 255;
    close io;

    let io=ouvrir sNom in
    write io 2 1;
    close io;

    (* seek et lecture pour contrôle *)
    let io=ouvrir sNom in
    let a=read io 8 in
    if a<>127 then rejet 32 1;
    close io;

    fin 32


;;
(**************************************************)
(* Enchainement des tests unitaires               *)
(**************************************************)
   tu_01 (); tu_02 (); tu_03 (); tu_04 ();
   tu_05 (); tu_06 (); tu_07 (); tu_08 ();
   tu_09 (); tu_10 (); tu_11 (); tu_12 ();
   tu_13 (); tu_14 (); tu_15 (); tu_16 ();
   tu_17 (); tu_18 (); tu_19 (); tu_20 ();
   tu_21 (); tu_22 (); tu_23 (); tu_24 ();
   tu_25 (); tu_26 (); tu_27 (); tu_27 ();
   tu_28 (); tu_29 (); tu_30 (); tu_31 (); 
   tu_32 ();
