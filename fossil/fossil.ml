open Printf

exception DepassementWrite

(* calcul du nombre de bit de codage à partir de nbloc *)
(*
# intervalle [(0,100);(1,101);(3,103)] 2;;
- : int = 101
# intervalle [(0,100);(1,101);(3,103)] 5;;
- : int = 103
# intervalle [(0,100);(1,101);(3,103)] 0;;
- : int = 100
# intervalle [(0,100);(1,101);(3,103)] 1;;
- : int = 101
*)
let intervalle lCodage nblocFind =
    List.fold_left ( fun nbBitsDecCour' (nbloc,nbBitsDecCour) ->
      if nblocFind >=nbloc then 
        nbBitsDecCour
      else 
        nbBitsDecCour'
    ) 0   lCodage 

(*extraction de lCodage entre [a  b]            *)
let extract lCodage a b = 

  (* extremité d'arrivée *)
  let nbBitsDec_b=intervalle lCodage b  in

  if a=b then (nbBitsDec_b,[]) 
  else (
    (* extremité de départ *)
    let nbBitsDec_a=intervalle lCodage a  in

    (* extraction des codes entre ]a b[ *)
    let lCodageFilter=
      List.filter ( fun (nbloc,_) -> nbloc >a && nbloc <b
      ) lCodage
    in
    (nbBitsDec_b,(a,nbBitsDec_a)::lCodageFilter @ [(b,nbBitsDec_b)])
 )

(*calcul du nombre de bits entre ]a b[          *)
let nbBitsBetween lCodage a b =
  if a+1 > b-1 then 0
  else (
    (* cumul des nblocs de 10 bits *)
    let nbBitsBloc=(b-a-1)*10 in

    let (nbBitsDec_arr,lCodageFilter)=extract lCodage (a+1) (b-1) in
(*
    List.iter ( fun (a,b) -> 
      printf "(%d,%d) " a b  
    ) lCodageFilter;
    printf "\n";
*)
    let lCodageInter=Outils.dedouble lCodageFilter in
    (* cumul des nDecal *)
    List.fold_left ( fun r ((a,nb_a),(b,nb_b)) ->
        r+(b-a)*nb_a
    ) (nbBitsBloc+nbBitsDec_arr) lCodageInter 
  )

(*Visualisation du contenu                      *)
let rec visu ib lCodage nbloc (opt_inbr,opt_inbr') =
(*
  (
  match (opt_inbr,opt_inbr') with 
   |(Some a,Some b) -> printf "To read=(%d,%d)\n" a b
   | _ -> ()
  );
*)
  if (opt_inbr,opt_inbr')<> (Some 0,Some 0) then (
    let pos=File_bit.read_pos ib 10 in
    Printf.printf "[" ;
    let (Bit.BitPos lpos)= pos in
    List.iter  ( fun ipos ->
      Printf.printf "%d " ipos;
    ) lpos;
    Printf.printf "]";
  
    (* lecture du decalage *)
    let nbBitsCodage=intervalle lCodage nbloc in
    let pos=File_bit.read_pos ib nbBitsCodage in
    let ipos=Bit.pos_to_int pos in
    Printf.printf "(%d)\n" ipos;
    
    let length_lpos=if ipos=0 then 0 else List.length lpos in

    let (opt_inbr,opt_inbr')= 
      match (opt_inbr,opt_inbr') with
        | (None     ,   _      ) -> (Some length_lpos          ,Some 0) 
        | (Some 1   ,Some inbr') -> (Some (inbr' + length_lpos),Some 0)
        | (Some inbr,Some inbr') -> (Some (inbr-1)             ,Some (inbr' + length_lpos))
    in

    (* on passe au bloc suivant *)
    visu ib lCodage (nbloc+1) (opt_inbr,opt_inbr')
  )

let dump sNom = 
  (* ouverture en lecture d'un fichier binaire *)
  let ib=File_bit.ouvrir sNom in

  (* lecture de iMax code sur 2 octets *)
  let iMax=File_bit.read ib (2*8) in 

  (* lecture de la taille du tableau sur 4 octets *)
  let iLengthCodage=File_bit.read ib (4*8) in 


  let lCodage=
    List.fold_left ( fun lCodage iCpt ->
      (* lecture de nbloc sur 8 octets *)
      let nbloc=File_bit.read ib (8*8) in 
      (* lecture de nbBitsDecCour sur 1 octet *)
      let nbBitsDecCour=File_bit.read ib (1*8) in

      (nbloc,nbBitsDecCour)::lCodage

    ) [] (Outils.enumFor 1 iLengthCodage); 
  in
  let lCodage=List.rev lCodage in

  printf "iMax=%d inbElt=%d\n" iMax iLengthCodage;
  printf "nbloc nbBits\n";
  List.iter ( fun (nbloc,nbBitsDecCour) ->
      printf "%d %d\n" nbloc nbBitsDecCour;
  )lCodage;
 
  visu ib lCodage 0 (None,None);
  File_bit.close ib
  
(*statistique sur les décalages                    *)
let rec statDec ((nbloc,nDecal,nbBitsDecPrec,lCodage)as arg) iMax larbre niveauRecherche niveauCourant =

  if niveauCourant =niveauRecherche then (
    let nbBitsDecCour=
      if niveauCourant<>iMax-1 then 
        Bit.int_nbBitUtile (nDecal-nbloc)
      else 0
    in 
    (* complement pour le positionnement *)
    let nbBits=List.length larbre in
 
    if nbBitsDecPrec <> nbBitsDecCour then (
      (nbloc+1,nDecal+nbBits,nbBitsDecCour,(nbloc,nbBitsDecCour)::lCodage)
    )
    else 
      (nbloc+1,nDecal+nbBits,nbBitsDecCour,lCodage)
  )
  else (
    if niveauCourant < niveauRecherche then (
       List.fold_left (fun arg (Tree.N(_,larbreN)) -> 
         statDec arg iMax larbreN niveauRecherche (niveauCourant+1)
       ) arg larbre
    )
    else arg
  )

let stat larbre iMax =
  let (_,_,_,lCodage)=
    List.fold_left ( fun arg niveauRecherche -> 
     statDec arg iMax larbre niveauRecherche 0  
    ) (0,1,0,[]) (Outils.enumFor 0 (iMax-1))
  in
  List.iter ( fun (nbloc,nbBitsDecCour) ->
    (* print du nombre de bit necessaire décalage *) 
    printf "(nbloc=%d nbBits=%d)\n"  nbloc nbBitsDecCour;
    flush stdout;
  ) (List.rev lCodage)

(*compactage de l'arbre                            *)
let rec compact_fossile ob iMax lCodage ((nbloc,nDecal)as arg) larbre niveauRecherche niveauCourant =

  if niveauCourant =niveauRecherche then (
    let lBloc=
       List.map (fun (Tree.N(ihdN,_)) -> ihdN
       )larbre
    in
    (* ecriture de lBloc sous la forme de 10bits *)
    File_bit.write_pos ob 10  (Bit.BitPos lBloc); 
   
    (* complement pour le positionnement *)
    let nbBits=List.length lBloc in
  
    (* calcul du nombre de bit du codage *)
    let nbBitsCodage=intervalle lCodage nbloc in

    (* Au niveau des feuilles le nDecal n'a pas de sens *)
    if niveauCourant<>iMax-1 then (
      (* ecriture sur nbBitsCodage bits *)
      File_bit.write ob nbBitsCodage  (nDecal-nbloc)
    ); 
    (nbloc+1,nDecal+nbBits)
  )
  else (
    if niveauCourant < niveauRecherche then (
       List.fold_left (fun arg (Tree.N(_,larbreN)) -> 
         compact_fossile ob iMax lCodage arg larbreN niveauRecherche (niveauCourant+1)
       ) arg larbre
    )
    else arg
  )

(* Test 
type t_arbre = N of int * t_arbre list
# let la=[
  N(1,
   [N(4,[N(8,[N(15,[])]);N(9,[N(16,[])]);N(10,[N(17,[])])])]
  );
  N(2,
   [N(5,[N(11,[N(18,[]);N(19,[])])]);N(6,[N(12,[N(20,[])]);N(13,[N(21,[])])])]
  );
  N(3,
   [N(7,[N(14,[N(22,[])])])]
  )
  ];;

# let la=[N(1,[N(2,[]);N(3,[])])] 

let rec f larbre niveauRecherche niveauCourant =

  if niveauCourant =niveauRecherche then (
      let lBloc=
         List.map (fun (N(ihdN,_)) -> ihdN
         )larbre
      in
      if lBloc<>[] then (
        List.iter (Printf.printf "(%d)" )lBloc ; 
        Printf.printf "\n";
      )
  )
  else (
    if niveauCourant < niveauRecherche then (
       List.iter (fun (N(_,larbreN)) -> f larbreN niveauRecherche (niveauCourant+1)
       ) larbre
    )
  )


;;
f la 0 0 ;;
*)
 
(*transfert de l'arbre vers un fossile             *)
let to_fossile larbre iMax sNom  =

  (* ouverture en ecriture d'un fichier binaire *)
  let ob=File_bit.ouvrir sNom in 

  (* ecriture de iMax sur 2 octets *)
  File_bit.write ob (2*8) iMax;

  (* calcul du tableau de codage des décalages *)
  let (_,_,_,lCodage)=
    List.fold_left ( fun arg niveauRecherche -> 
     statDec arg iMax larbre niveauRecherche 0  
    ) (0,1,0,[]) (Outils.enumFor 0 (iMax-1))
  in
  let lCodage=List.rev lCodage in

  (* ecriture de la taille du tableau sur 4 octets *)
  File_bit.write ob (4*8) (List.length lCodage); 

  List.iter ( fun (nbloc,nbBitsDecCour) ->
    (* ecriture de nbloc sur 8 octets *)
    File_bit.write ob (8*8) nbloc; 
    (* ecriture de nbBitsDecCour sur 1 octet *)
    File_bit.write ob (1*8) nbBitsDecCour; 
  ) lCodage;

  (* compactage de l'arbre *)
  let _ =
    List.fold_left ( fun arg niveauRecherche -> 
     compact_fossile ob iMax lCodage arg larbre niveauRecherche 0  
    ) (0,1) (Outils.enumFor 0 (iMax-1))
  in
  (* ecriture du reliquat dans le fichier *)
  File_bit.close ob 



(***************************************************)
(*            Lecture                             *)
(***************************************************)
type my_fib =
  {
   ib              : File_bit.my_io;
   iMax            : int;
   lCodage         : (int * int) list;
   sizeHeadBits    : int                 (* taille de l'entete en Bits *)
  }

let open_in sNom =
  try (
    (* ouverture en lecture d'un fichier binaire *)
    let ib=File_bit.ouvrir sNom in
  
    (* lecture de iMax code sur 2 octets *)
    let iMax=File_bit.read ib (2*8) in
  
    (* lecture de la taille du tableau sur 4 octets *)
    let iLengthCodage=File_bit.read ib (4*8) in
  
    let lCodage=
      List.fold_left ( fun lCodage iCpt ->
        (* lecture de nbloc sur 8 octets *)
        let nbloc=File_bit.read ib (8*8) in
        (* lecture de nbBitsDecCour sur 1 octet *)
        let nbBitsDecCour=File_bit.read ib (1*8) in
  
        (nbloc,nbBitsDecCour)::lCodage
  
      ) [] (Outils.enumFor 1 iLengthCodage);
    in
    let lCodage=List.rev lCodage in
    (* nous sommes au debut de l'arbre *)
    Some {ib;iMax;lCodage;sizeHeadBits=(2+4+iLengthCodage*9)*8}
  ) with _ -> None

(*Positionnement après l'entete                    *)
let afterHead_in fib =
  File_bit.rewind fib.ib;
  File_bit.seekCur fib.ib fib.sizeHeadBits


(*Recherche d'un element                           *)
let bFind opt_fib s =
  let rec bFind fib nbloc = function
      []         -> true
    | iPos::[]   ->  (* feuille *) 
        (* lecture des 10 bits de la position courante *)
        let Bit.BitPos lBloc=File_bit.read_pos fib.ib 10 in

        (* position en partant de 0 de iPos dans lBloc *)
        let opt=Outils.position lBloc iPos in

        if opt=None then false else true
    | iPos::tl   ->
        (* lecture des 10 bits de la position courante *)
        let Bit.BitPos lBloc=File_bit.read_pos fib.ib 10 in
(*
        printf "iPos=(%d)\n" iPos;
        List.iter ( fun e ->
          printf "%d," e
        ) lBloc; 
        printf "\n";
*)
        (* position en partant de 0 de iPos dans lBloc *)
        let opt=Outils.position lBloc iPos in

        if opt=None then false
        else ( 
          let Some r=opt in
(*          
          printf "iPos=%d r=%d \n" iPos r; 
*)
          (* calcul de la taille en Bits du Décalage du bloc *) 
          let nbBitsDec=intervalle fib.lCodage nbloc  in
          (* lecture du Décalage *)
          let nDecal=File_bit.read fib.ib nbBitsDec in

          (* calcul du numéro du bloc Suivant qui contient le prochain noeud *) 
          let nblocSuiv=nbloc+nDecal+r in

          (* nombre de Bits entre le ]nbloc  nblocSuiv[ *)
          let nbBitsBetween=nbBitsBetween fib.lCodage nbloc nblocSuiv in 
(*
          printf "Between ]%d %d[ nbBits(%d) nDecal(%d)\n" nbloc nblocSuiv nbBitsBetween nDecal;
*)
          (* avance de nBits pour se positionner sur nblocSuiv *)
          File_bit.seekCur fib.ib nbBitsBetween;

          bFind fib nblocSuiv tl
        )
  in
 
  if opt_fib=None then false
  else (     
    let Some fib=opt_fib in
    let li=Outils.to_liste_int_0 s in
    (* on ajoute les 0 *)
    let li=Outils.complete li fib.iMax 0 in
    (* on se positionne après l'entete *)
    afterHead_in fib;
    bFind fib 0 li
  )

(*close_in                                         *)
let close_in =
  Outils.may ( fun fib ->
    File_bit.close fib.ib
  ) 



