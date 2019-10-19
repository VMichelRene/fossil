open Printf
open Bit
(***************************************************)
(*Ecriture et lecture sur un fichier binaire       *)
(***************************************************)

type my_io =
  {
    (* nom du fichier *)
    sNom                     : string;
    fd                       : Unix.file_descr; 
    posB                     : Byte.my_posB;
    (* le pointeur de fichier se trouve après posB *) 
    mutable bAfter           : bool; 

    (* position dans le fichier en Bits par rapport au debut du fichier *)
    mutable seekBits         : int;

  }

let info io =
  printf "posB      =%s\n" (Byte.sprintB io.posB);
  printf "bAfter    =%b\n" io.bAfter;
  printf "seekBits  =%d\n" io.seekBits
 

(************************************************)
(* writeB                                       *)
(************************************************)
(* 
  soit le seek me positionne sur octet |g g d d d d d d|
                                                        ^
                                                        Pointeur fichier
       si je realise un write 2 (10)b  |g g g g d d d d| 
                                            1 0 
       je dois revenir un octet dans le fichier pour ecrire.
       Par simplicité même si je ne realise par de write 
        je peux ecrire l'octet (toujours en revenant en arrière d'une position).

 soit l'octet est positionné après la fin de fichier |g g d d d d d d|
                                                      ^
                                                      Pointeur fichier
      auquel cas je dois l'écrire dans le fichier sans revenir en arrière.  
*)

let writeB io  =

  let transfert io =
    
    (* lecture des 8 bits de Gauche de io.posB *)
    let posDisque=Byte.get io.posB in 
(*
    printf "Avant read\n";
    let (iNbBitsDisque,posDisque)=Byte.read io.posB 8 in
    if iNbBitsDisque <> 8 then failwith "writeB : la partie G ne contient pas 8 bits";
*)
    (* on ecrit un octet *)
    let buf=Bit.pos_to_s posDisque in 
    ignore (Unix.write io.fd buf 0 1) 
  in 
  let bNone=Byte.siNone io.posB in
  if not bNone then (
    if io.bAfter then (
      (* le pointeur de fichier est après posB *)
      (* on doit reculer d'un octet avant d'écrire *)
      ignore (Unix.lseek io.fd (-1) Unix.SEEK_CUR )
    );

    (* on bascule les bits de Droite vers la Gauche afin d'avoir un posB prêt *)
    let _ =Byte.read io.posB 8 in
    transfert io
  );
  io.posB.Byte.opt_posByte<-None;
  io.bAfter<-false
   

(************************************************)
(* pos_read                                     *)
(*
  Par exemple, peut avoir iNbBits = 0 ou 5 ou 100
   Une lecture de 100 bits nécessite de lire 13 octets du fichiers.
    sur le dernier octet du fichier il restera un reliquat 
*)
(************************************************)
let rec pos_read io posAccu iNbBits = 

  (* on charge un octets *)
  let buf = Bytes.create 1 in
  let inbOctet=Unix.read io.fd buf 0 1 in
 
  let posDisque= 
    if inbOctet=1 then ( 
      (* le pointeur de fichier va être après posB *)
      io.bAfter<-true ;
      (* cas ou une lecture sur disque est possible *)
      let iByte=Pervasives.int_of_char buf.[0] in
      Bit.int_to_pos iByte
    )
    else (
     io.bAfter<-false ;
     (BitPos []) 
    )
 in 

 if iNbBits < 8 then (
   (* s'il reste moins de 8 bits alors on charge posB *)
   Byte.set io.posB posDisque;
   let (_,posRead)=Byte.read io.posB iNbBits in
   Bit.pos_sort (Bit.pos_concat (Bit.pos_l posAccu iNbBits) posRead)
 )
 else (
   (* on cumule posDisque sur posAccu *)
   let posAccu=Bit.pos_sort (Bit.pos_concat (Bit.pos_l posAccu 8) posDisque) in 
   pos_read io posAccu (iNbBits-8)
 )
(************************************************)
(* pos_write                                    *)
(* ecriture sur io de nBits de pos en partant des bits de poids forts  *)
(************************************************)
let rec pos_write io (iNbBits,pos) =

  if iNbBits>0 then (
    (* iNbBits' = iNbBits - "le nombre de bits ecrit sur io.posB" *)
    let (iNbBits',pos')=
      Byte.write io.posB (iNbBits,pos) 
    in
    let bComplet=Byte.siComplet io.posB in
(*
    printf "pos_write (%d %d) %b\n" iNbBits iNbBits' bComplet;
    info io;
*)
    if bComplet then (
      (* si io.posB est complet alors on peut l'écrire sur le fichier *)
      writeB io;
      (* on doit charger posB avec l'octet suivant sinon pos' risque d'écraser les informations suivante *)
      let buf = Bytes.create 1 in
      let inbOctet=Unix.read io.fd buf 0 1 in
      if inbOctet=1 then ( 
        (* le pointeur de fichier va être après posB *)
        io.bAfter<-true ;
        let iByte=Pervasives.int_of_char buf.[0] in
        let posDisque=Bit.int_to_pos iByte in
        Byte.set io.posB posDisque
      )
      else (
       io.bAfter<-false ;
      );
          
      pos_write io (iNbBits',pos')
    ); 
  )



(***************************************************)
(*seek                                             *)
(***************************************************)
let seek io iNbBits =

  io.seekBits <-iNbBits;
 
  (* ecriture de posB *)
  writeB io; 

  let inbOctets=iNbBits/8 in
  let inbBitsReliquats=iNbBits mod 8 in

  (* on se positionne sur fd *)
  let inbOctets'=Unix.lseek io.fd inbOctets Unix.SEEK_SET in 
(*
  printf "inbOctets=%d inbBitsReliquats=%d inbOctets'=%d\n" inbOctets inbBitsReliquats inbOctets';
*)
  if inbOctets'<>inbOctets then failwith "seek Unix.lseek";

  (* chargement de l'octet suivant s'il existe *)
  ignore (pos_read io (Bit.BitPos []) inbBitsReliquats)

(***************************************************)
(*Positionnement en nombre de bits par rapport à la position courante    *)
(***************************************************)
let seekCur io iNbBits =
  let iNbBitsDebFich=io.seekBits+iNbBits in
  seek io iNbBitsDebFich

(***************************************************)
(*rewind                                           *)
(***************************************************)
let rewind io =
  seek io 0 

(***************************************************)
(* ouvrir                                          *)
(***************************************************)

let ouvrir sNom =

  let fd=Unix.openfile sNom [Unix.O_RDWR;Unix.O_CREAT] 0o666 in

  let io=
    {
      sNom;
      fd;
      posB      ={Byte.opt_posByte=None};
      (* le pointeur de fichier se trouve avant posB *)
      bAfter    =false;
      seekBits  = 0;
    } 
  in
  (* juste pour charger posB si le fichier n'est pas vide *)
  rewind io;
  io

(***************************************************)
(*read_pos                                         *)
(* lecture sous forme de positions binaires BitPos *)
(***************************************************)
let read_pos io iNbBits =

  (* on peut toujours lire le fichier même en cas de dépassement *)
  io.seekBits    <- io.seekBits+iNbBits; 
  (* preparation de posAccu *)
  let (iNbBits',posAccu)= Byte.read io.posB iNbBits in 
(*
  printf " iNbBits=%d iNbBits'=%d  posAccu (%s)\n" iNbBits iNbBits' (Bit.sprint_pos posAccu) ;
*)
  let pos= 
    if iNbBits' = 0 then 
      posAccu
    else  
      (* il reste des bits à lire que l'on va lire sur disque à l'aide de pos_read *)
      pos_read io posAccu iNbBits' 
  in
  pos
(***************************************************)
(*read                                             *)
(***************************************************)
let read io iNbBits =
  let pos=read_pos io iNbBits in
  Bit.pos_to_int pos

(***************************************************)
(*read_lposition                                   *)
(* lecture sous la forme d'une liste de position   *)
(* si read_pos retourne BitPos [0;1;6]
   alors read_lpositon retourne [0;1;6]            *)
(***************************************************)
let read_lposition io iNbBits =
  let pos=read_pos io iNbBits in
  Bit.pos_to_lposition pos

(***************************************************)
(*close                                            *)
(***************************************************)
let close io =
  writeB io;
  Unix.close io.fd

(***************************************************)
(*write                                            *)
(* L'ecriture sur le fichier respecte l'ordre bits forts vers bits faibles
   Exemple : 
    write io 12 1023;
    close io; 

    on ecrit 12 bits de (1023)base 10 = (0011 1111 1111)base 2
    on a dans le fichier deux octets :
      (00111111)base 2=(077)base 8
      (11110000)base 2=(360)base 8

   od -b Test_1
   0000000 077 360

*)
(***************************************************)

(***************************************************)
(*write_pos                                        *)
(***************************************************)
let write_pos io iNbBits pos =
  (* verification que i ne depasse pas la capacité *)
  if not (Bit.pos_verif iNbBits pos) then (
(*
    printf "write_pos iNbBits=%d pos=%s\n" iNbBits (Bit.sprint_pos pos);
*)
    printf "write_pos iNbBits=%d pos=%s\n" iNbBits (Bit.sprint_pos pos);
    raise (Invalid_argument (__FILE__^",write_pos"));
  );

  io.seekBits        <- io.seekBits+iNbBits;

  pos_write io (iNbBits,pos)  

(***************************************************)
(*write                                            *)
(***************************************************)
let write io iNbBits i =
  let pos=Bit.int_to_pos i in
  write_pos io iNbBits pos 
(***************************************************)
(*write_lposition                                  *)
(***************************************************)
let write_lposition io iNbBits lposition =
  let pos=Bit.pos_of_lposition lposition in
  write_pos io iNbBits pos 
(***************************************************)
(*size                                             *)
(*  nombre d'octets du fichiers y compris posB     *)
(***************************************************)

let size io =
  let stat =Unix.stat io.sNom in
  let size =stat.Unix.st_size in
  let bNone=Byte.siNone io.posB in
  if bNone then size else size+1

