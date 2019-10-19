open Printf
open Bit
(************************************************)
(* manipulation d'un octet sous forme de bits   *)
(************************************************)

(*type                                          *)

type my_posByte=
  {
    iNbBitsG    : int;
    posG        : my_bitpos;

    iNbBitsD    : int;
    posD        : my_bitpos;
  }

type my_posB=
  {
    mutable opt_posByte : my_posByte option
  }

(*sprintB                                       *)
let sprintB posB= 
  match posB.opt_posByte with 
   | None -> sprintf "None"
   | Some posByte ->
       sprintf "iNbBitsG=%d posG=%s | iNbBitsD=%d posD=%s" 
         posByte.iNbBitsG (sprint_pos posByte.posG)
         posByte.iNbBitsD (sprint_pos posByte.posD)


(* initialise la partie droite sur 8 bits *)
let set posB pos=
  posB.opt_posByte <- 
    Some {
      iNbBitsG=0;
      posG=BitPos [];
      iNbBitsD=8;
      posD=pos;
    }

(* on récupère la partie gauche qui doit contenir 8 bits *)
let get posB =
  match posB.opt_posByte with 
    | None         ->  failwith "Bit get : posB est vide"  
    | Some posByte ->  
        if posByte.iNbBitsG<>8 then failwith "Bit get : la partie G ne contient pas 8 bits";
        let pos=posByte.posG in
        posB.opt_posByte <- None;
        pos  

(* lecture de iNbBits au niveau de la partie Droite *) 
(* indique le nombre restant de bits non lu     *)
let read posB iNbBits =
  match posB.opt_posByte with 
    | None         ->   
        (iNbBits,Bit.BitPos [])
    | Some posByte ->  
        (* nombre de Bits pouvant être lu *)
        let iNbBitsALire=Pervasives.min iNbBits posByte.iNbBitsD in
        let iNbBits'=iNbBits-iNbBitsALire in


        let  iNbBitsG =posByte.iNbBitsG+iNbBitsALire in
        let  iNbBitsD =posByte.iNbBitsD-iNbBitsALire in

        (* Traitement de posD *)
        let (posDFort,posD)=pos_preleve posByte.posD iNbBitsD in  
        let posDFortDec    =pos_r posDFort iNbBitsD in

        (* Traitement de posG : Décale posG vers la gauche puis on ajoute pos*)
        let posGDec=pos_l posByte.posG iNbBitsALire in
        let posG   =pos_concat posGDec posDFortDec in

        posB.opt_posByte <-
          Some {
            iNbBitsG; 
            posG;
            iNbBitsD;
            posD; 
          }
       ;
       (iNbBits',posDFortDec)

(* indique le nombre restant de bits non ecrit  *)
(*
  A chaque ecriture on ajoute pos à posG sur les bits de poids faibles
  et on enlève à posD sur les bits de poids forts l'équivalent de pos.

  Exemple :
    write  (2,[1,0])
    iNbBitsG=2 posG=1,0       iNbBitsD=6 posD=

    write  (2,[0]);
    iNbBitsG=4 posG=3,2,0     iNbBitsD=4 posD=

    write  (4,[3;0]);
    iNbBitsG=8 posG=7,6,4,3,0 iNbBitsD=0 posD=
*)
let write posB (iNbBits,pos)=

  let iNbBitsAEcrire = 
    match posB.opt_posByte with 
      | None         -> 
          Pervasives.min iNbBits 8
      | Some posByte ->  
          Pervasives.min iNbBits (8 - posByte.iNbBitsG) 
  in
  (* on segmente pos *)
  let iNbBits'=iNbBits-iNbBitsAEcrire in

  (* on prelève sur pos iNbBitsManque au niveau des poids forts *)
  let (posG,pos')=pos_preleve pos iNbBits' in  
  (* on décale vers la droite posG *)
  let posGDec=pos_r posG iNbBits' in
  (
    match posB.opt_posByte with 
      | None ->   
          let  iNbBitsG =   iNbBitsAEcrire in
          let  iNbBitsD = 8-iNbBitsAEcrire in
          posB.opt_posByte <-
            Some {
              iNbBitsG; 
              posG=posGDec;
              iNbBitsD;
              posD=BitPos [];
            }
      
      | Some posByte ->  
          let  iNbBitsG =posByte.iNbBitsG+iNbBitsAEcrire in
          let  iNbBitsD =posByte.iNbBitsD-iNbBitsAEcrire in
  
          (* Traitement de posG : Décale posG vers la gauche puis on ajoute pos*)
          let posGDec'=pos_l posByte.posG iNbBitsAEcrire in
          let posG=pos_concat posGDec' posGDec in
          (* Traitement de posD *)
          let (_,posD)=pos_preleve posByte.posD iNbBitsD in  
          posB.opt_posByte <-
            Some {
              iNbBitsG; 
              posG;
              iNbBitsD;
              posD; 
            }
  );
  (iNbBits',pos')

(* siComplet                                    *)
let siComplet posB =  
  match posB.opt_posByte with 
    | None         -> false 
    | Some posByte -> posByte.iNbBitsG=8 

(* siNone                                    *)
let siNone posB =  
  match posB.opt_posByte with 
    | None         -> true 
    | Some posByte -> false 
   
 
