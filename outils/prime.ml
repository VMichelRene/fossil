(* test de primalité                            *)
 let is_prime n =
    let n = abs n in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
    n <> 1 && is_not_divisor 2

(* Liste des nombres premiers entre deux nombres *)
 let l_prime (iDeb,iFin) =
   let rec aux l iCour =
     if iCour>iFin then List.rev l
     else if is_prime iCour then 
       aux (iCour::l) (iCour+1)
     else aux l (iCour+1)
   in
   let iDeb=max iDeb 0 in
   aux [] iDeb
