open Printf

(*                                              *)
let id  x = x
let cst x =fun _ -> x

(*                                              *)
let value ~default = function
  | None   -> default
  | Some x -> x

(* reduce                             *)
  let reduce f l =
    let rec aux h = function
      | []       -> None 
      | x::[]    -> Some (h x)
      | x::tl    -> aux (f (h x)) tl
    in
    aux id l 

(* ope                                          *)
  let ope f g l =
        List.map g l
    |>  reduce f 

(* List.init                                     *)
let list_init n f = Array.(init n f |> to_list)

(* dedouble                                     *)
let dedouble l =
  let rec aux acc h' = function
    | []    -> List.rev acc
    | h::tl -> aux ((h',h)::acc) h tl
  in
  match l with 
    | []    -> []
    | h::tl -> aux [] h tl

(* numérotation d'une liste en respectant l'ordre à partir de 0 *)
let numerote lobj =
  List.mapi ( fun rg obj -> (rg,obj)) lobj

(* position du premier element                  *)
let position le iPos =
  let lnum_e=numerote le in
  try (
    let (n,_)=
      List.find ( fun (_,e) -> e=iPos
      ) lnum_e
    in
    Some n
  ) with Not_found -> None

(* enlève les doublons                          *)
let doublon lx =
  List.fold_right  (
    fun x l -> if List.exists ( (=) x ) l then l else x::l 
  )  lx []

(*Projection canonique                          *)
let pi_1=fst
let pi_2=snd

(*                                              *)
let proj1_sur_2 list1 = 
 List.map pi_1 list1 

let proj2_sur_2 list1 = 
 List.map pi_2 list1 

(* enumFor                                      *) 
let rec enumFor iDeb iFin =
  let rec aux acc iCpt =
    if iCpt<iDeb then  acc
    else aux (iCpt::acc) (iCpt-1) 
  in
  if iDeb <= iFin 
  then aux [] iFin
  else List.rev (enumFor iFin iDeb)

(* Conversion d'un string en liste de char       *)
let to_liste_char s =
  list_init (String.length s) (fun i -> s.[i])

(*Conversion d'un string en liste de int       *)
let to_liste_int s =
  let ascii_0=int_of_char '0' in

  to_liste_char s
  |> List.map (fun c -> int_of_char c -ascii_0 ) 

(*Conversion d'un string en liste de int mais les 
blancs sont remplacé par des 0       *)
let to_liste_int_0 s =
  let ascii_0=int_of_char '0' in

  to_liste_char s
  |> List.map (
       fun c -> 
         if c=' ' then 0 else int_of_char c -ascii_0 
       ) 

(*Conversion d'un int en liste de int           *)
let int_to_liste_int i =
  sprintf "%d" i 
  |> to_liste_int 

(*int_of_liste_int                              *)
let int_of_liste_int li =
  List.fold_left (fun l i -> l*10+i) 0 li

(*Ajout de a devant une liste afin d'atteindre la taille iMax *) 
let complete iList iMax c =
  let rec aux iList iManque =
    if iManque=0 then iList
    else aux (c::iList) (iManque-1) 
  in
  let iTaille=List.length iList in
  if iTaille>=iMax then iList 
  else aux iList (iMax-iTaille)

(*Conversion d'une liste de caractères en string*)
let rec string_of_char lc = 
  List.fold_left (fun s c -> s^(String.make 1 c) ) "" lc
        
(*ajoute un caractère apres chaque element sauf le dernier *)
let list_to_string_add_sep s f le = 
     ope (fun a b -> sprintf "%s%s%s" a s b) f le
  |> value ~default:"" 

(*Destruction d'un fichier                      *)
let unlink sNom =
 try (Unix.unlink sNom) with _ -> ()

(*Outils Gaux  evite de compiler avec gtk       *)
let may f = function
    None   -> ()
  | Some o -> f o 
