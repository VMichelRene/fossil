open Printf

(************************************************)
(*type                                          *)
(************************************************)
(* position des bits d'un entier 
  (5)base 10 = (101)base 2 -> BitPos [0;2]
*)
type my_bitpos= BitPos of (int list)

(* décomposition d'un entier en puissance de 2 
  5 -> BitPuiss [1;4]
*)
type my_bitpuiss= BitPuiss of (int list)



(* Preleve                                      *)
let pos_preleve (BitPos l) s =
  let (lFort,lFaible)=
    List.fold_left ( fun (lFort,lFaible) e ->
      if e >=s then (e::lFort,lFaible) else (lFort,e::lFaible) 
    ) ([],[]) l
  in
  (BitPos lFort,BitPos lFaible)

(* Décalage de i position vers la gauche       *)
let pos_l (BitPos l) i =
  BitPos (List.map ((+)i) l)

(* Décalage de i position vers la droite       *)
let pos_r (BitPos l) i =
  BitPos (
    List.fold_left ( fun r e ->
      if e<i then r else e-i::r 
    ) [] l 
  )

(* pos_concat                                   *)
let pos_concat (BitPos l1) (BitPos l2) = BitPos (l1@l2) 

(*sprint_pos                                    *)
let sprint_pos (BitPos l) = 
  Outils.list_to_string_add_sep "," (sprintf "%d") l

(*Conversion d'un entier en position binaire    *)
let int_to_pos  i = 
  let rec int_to_pos iPos i = 
    if i=0 then []
    else (
      if (i land 1) = 1 then iPos::(int_to_pos (iPos+1) (i lsr 1))
      else int_to_pos (iPos+1) (i lsr 1)
    ) 
  in
  BitPos (int_to_pos 0 i)

(*Conversion d'un pos vers une liste de position*)
let pos_to_lposition pos =
  let BitPos lposition=pos in 
  lposition

(*Conversion d'une liste de position vers un pos*)
let pos_of_lposition lposition = BitPos lposition 

(*Conversion de  position binaire  en puissance *)
let pos_to_puiss (BitPos l) = 
  BitPuiss (List.map (fun e -> 1 lsl e) l)

(* Conversion d'une liste de puissance 2 en entier *)
let puiss_to_int (BitPuiss l) = 
  List.fold_left (+) 0 l

(* Conversion d'une liste de position en entier *)
let pos_to_int pos = 
  puiss_to_int (pos_to_puiss pos )

(* Conversion d'une liste de position en une chaine de caractère de un caractère 
   pos doit représenter un entier [0,..,255]
*)

let pos_to_s pos =
  let i=pos_to_int pos in
  let c=Pervasives.char_of_int i in
  String.make 1 c

(* Decoupage d'un bloc en deux parties          *)
let bloc_decoupe l i = 
  let rec bloc_decoupe l fr = 
    List.fold_left ( fun (lG,lD) e ->
      if e>=fr then (e::lG,lD) else (lG,e::lD)
    ) ([],[]) l 
  in
  let fr = 1 lsl i in   (* frontière *)
  bloc_decoupe l fr


(*                                              *)
let pos_sort (BitPos lpos) = BitPos (List.sort compare lpos)

(*Nombre utile de bit d'un pos                  *)
let pos_nbBitUtile (BitPos lipos) =
  let iposMax=
    List.fold_left ( fun nb ipos ->
      if ipos> nb then ipos else nb
    ) 0 lipos 
  in
  iposMax+1

(*Nombre utile de bit d'un entier               *)
let int_nbBitUtile i =
  let pos=int_to_pos i in
  pos_nbBitUtile pos

(* verification que pos ne depasse pas la capacité de iNbBits *)
let pos_verif iNbBits (BitPos lipos) =
  List.for_all ((>) iNbBits) lipos
