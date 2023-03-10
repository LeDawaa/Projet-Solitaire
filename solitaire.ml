open Solitaire_defs
(* Q0 remplacer par vos nom, prénom et adresse mail OBLIGATOIREMENT de l'université.
      ne pas enlever les guillemets !

   0.5 point
 *)
let nom = "PINEAU"
let premom = "Florian"
let email = "florian.pineau1@universite-paris-saclay.fr"


(* Q1
    affiche_tas : carte list -> unit
    Affiche soit un emplacement vide (en utilisant la variable globale zone_vide) si le tas est vide
            soit la première carte du tas s'il est non vide. On peut convertir la carte en chaîne avec
            la fonction string_of_carte (qui s'occupe d'ajouter les couleurs correctement).

   0.75 point
*)

let affiche_tas tas =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   match tas with 
    [] -> Printf.printf("%s") zone_vide
  | [ e ] -> Printf.printf("%s") (string_of_carte e)
  | e :: ttas -> Printf.printf("%s") (string_of_carte e)

   (* Help_solitaire.affiche_tas tas *)
;;

(* Q2
   affiche_piles : (carte liste * carte list) list -> unit
   Affiche la liste des piles. Chaque pile est précédée de son numéro, en
   commençant par 1. Pour chaque pile (lvisibles, lcachees) on affiche :
     - son numéro, suivi de deux points ":"
     - les cartes de lvisibles, chacune suivie d'un espace. Comme précédemment
       les cartes sont à convertir avec string_of_carte.
     - pour chaque carte de lcachees, on affiche la chaine contenue dans
       la variable globale carte_cachee, suivie d'un espace
     - deux retour à la ligne

   Une fonction utile est la fonction List.iteri: (int -> 'a -> unit) -> 'a list -> unit
   Elle est semblable à la fonction List.iter vue en cours mais la fonction f passée en argument
   reçoit en plus la position dans la liste, en plus de la valeur. Attention la position
   commence à 1.

   Par exemple
     List.iteri (fun i c -> Printf.printf "%d:%s " i c) ["a"; "b"; "c"]
   affiche:
     0:a 1:b 2:c

   1.5 point
*)

let affiche_piles piles =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   let rec display pile text=
  match pile with
    ([], []) -> ""
  | ([], p :: ppile) ->   text ^ carte_cachee ^ " " ^ (display ([], ppile) text)
  | (p :: ppile, pilee) ->   text ^ string_of_carte p ^ " " ^ (display (ppile, pilee) text)
  in
  List.iteri (fun i c -> Printf.printf "%d:%s\n\n" (i + 1) (display c  "")) piles;; 

   (* Help_solitaire.affiche_piles piles *)
;;

(* Q3
    affiche_pioche: carte list -> unit
    Affiche la pioche. Si celle ci est vide, on affiche la chaîne contenue dans la variable globale
    zone_vide sinon on affiche celle contenue dans la variable globale carte_cachee.

   0.5 point
*)

let affiche_pioche pioche =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   match pioche with 
    [] -> Printf.printf("%s") zone_vide
  | [ e ] -> Printf.printf("%s") carte_cachee
  | e :: ppioche -> Printf.printf("%s") carte_cachee

   (* Help_solitaire.affiche_pioche pioche *)
;;


(* Q4
    affiche_jeu : jeu -> unit
    Affiche l'état du jeu. On utilise affiche_tas pour afficher dans l'ordre le tas de cœur, de pique, de trefle et de
    carreau chacun séparé de 2 espaces.
    Puis on affiche 4 espaces.
    Puis on utilise affiche_tas pour afficher la défausse.
    Puis on affiche 1 espace.
    Puis on affiche la pioche avec affiche_pioche.
    Puis on affiche deux retours à la ligne.
    Puis on affiche les piles avec affiche_piles
    Puis on affiche un retour à la ligne.

    1 point
*)

let affiche_jeu jeu =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   affiche_tas jeu.coeur; Printf.printf("%s") ("  ");
   affiche_tas jeu.pique; Printf.printf("%s") ("  ");
   affiche_tas jeu.trefle; Printf.printf("%s") ("  ");
   affiche_tas jeu.carreau; Printf.printf("%s") ("    ");
   affiche_tas jeu.defausse; Printf.printf("%s") (" ");
   affiche_pioche jeu.pioche; Printf.printf("%s") ("\n\n");
   affiche_piles jeu.piles; Printf.printf("%s") ("\n");

   (* Help_solitaire.affiche_jeu jeu *)
;;


let valeurs = [ Valeur 1; Valeur 2; Valeur 3; Valeur 4;
                Valeur 5; Valeur 6 ; Valeur 7; Valeur 8;
                Valeur 9 ; Valeur 10; Valet; Dame; Roi ]

(* Q5
    init_cartes : couleur -> carte list
    Utilise la variable globale valeurs pour renvoyer la liste de toutes
    les cartes de la couleur donnée en argument.

   1 point
*)

let init_cartes couleur =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   let rec init_cart couleur vals=
   match vals with
   [] -> []
  | e :: vvals -> {couleur=couleur; valeur=e} :: (init_cart couleur vvals);
  in
  init_cart couleur valeurs

   (* Help_solitaire.init_cartes couleur *)
;;

(* Le paquet de carte initiale, où les 13 cartes de chaque couleur sont présentes dans l'ordre *)
let paquet_initial = (init_cartes Coeur) @ (init_cartes Pique) @ (init_cartes Carreau) @ (init_cartes Trefle)
;;

(* Q6
    melange : carte list -> carte list
    Mélange aléatoirement la liste de carte donnée en argument. On peut utiliser l'algorithme suivant :
    On calcule une liste de paires d'un entier aléatoire (entre 0 1000, obtenu par Random.int 1000)
    et d'une carte.
    Puis on trie cette liste en utilisant List.sort avec la fonction de comparaison generique
    compare de la bibliothèque standard.
    Puis sur cette liste triée, on retire les entiers aléatoires pour obtenir une liste de cartes

    1.25 point
*)
let melange paquet =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   let rec tuple paq=
   match paq with
   [] -> []
  | e :: ppaq -> (Random.int 1000, e) :: (tuple ppaq);
  in
  let sorted = List.sort compare (tuple paquet) in
  let rec untuple paq=
  match paq with
  [] -> []
  | e :: ppaq -> (snd e) :: (untuple ppaq);
  in
  untuple sorted

   (* Help_solitaire.melange paquet *)
;;

(* Q7
   n_premieres: carte list -> int -> carte list * carte list
   Cette fonction récursive prend en argument une liste de cartes et un entier n.
   Elle renvoie une paire de listes (lprem, lreste) où :
   lprem est la liste des n premières cartes de la liste initiale et
   lreste est la liste des cartes restantes

   Si la liste est trop courte, lève l'erreur :
   failwith "npremiere: pas assez de cartes"

   1.5 point
*)

let rec n_premieres paquet n =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

  let rec part_one paquet n=
  if n = 0 then []
  else match paquet with
     [] -> failwith "npremiere: pas assez de cartes"
   | e :: ppaquet -> e :: (part_one ppaquet (n-1));
   in
   let rec part_two paquet n=
   if n > 0 then match paquet with e :: ppaquet -> (part_two ppaquet (n-1))
  else match paquet with
     [] -> []
   | e :: ppaquet -> e :: (part_two ppaquet (n-1));
   in
   (part_one paquet n, part_two paquet n)

   (* Help_solitaire.n_premieres paquet n *)
;;


(*
   Q8
   init_piles: carte list -> (carte list * carte list)
   Cette fonction prend en argument une liste de cartes l.
   Elle prend les 7 premières cartes de l et crée une pile
   [ ([c], l6) ] où c est la carte visible et l6 la liste des 6 cartes cachées.
   Puis elle fait de même sur les cartes restantes pour ajouter :
   [ ([c], l5) ; ([c], l6) ]
   Puis ainsi de suite jusqu à former
   [ ([c], []); ...; ([c], l7) ]


   La fonction est donnée, ne pas la modifier.
   Question : Quel est le type de la fonction interne 'init_pile' ?
   Réponse : 'a list -> 'a list * 'a list

   0.5 point
*)

let init_piles paquet =
    let init_pile cartes =
        match cartes with
            [] -> failwith "init_pile: pas assez de cartes"
        | p :: r -> [p], r
    in
    List.fold_left (fun (acc_piles, acc_reste) n ->
            let tas, acc_reste = n_premieres acc_reste n in
            let pile = init_pile tas in
            (pile :: acc_piles, acc_reste))
            ([], paquet) [ 7;6;5;4;3;2;1 ]

;;

(* La fonction qui initialise le jeu, ne pas modifier *)
let init_jeu () =
    let cartes = melange paquet_initial in
    let piles, reste = init_piles cartes in
    { coeur = [];
      pique = [];
      carreau = [];
      trefle = [];
      defausse = [];
      pioche = reste;
      piles = piles;
      }
;;

(* Q9: pioche : jeu -> jeu
   La fonction qui pioche une carte et renvoie le nouveau jeu. Pour cela :
   - si la défausse ET la pioche sont toutes les deux vides, alors lever l'erreur
     failwith "pioche: plus de carte à piocher"
   - si la pioche est vide, alors
        créer un nouveau jeu où la pioche contient les cartes de la défausse renversée
        et la défausse est vide, puis piocher dedans
   - sinon prendre la première carte de la pioche et la poser sur la défausse

   0.75 point
*)

let pioche jeu =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   match jeu.pioche, jeu.defausse with
   [], [] -> failwith "pioche: plus de carte à piocher"
   | [], _ -> { coeur=jeu.coeur;
                pique=jeu.pique;
                carreau=jeu.carreau;
                trefle=jeu.trefle;
                piles=jeu.piles;
                defausse=[];
                pioche=List.rev jeu.defausse}
   | p :: ppioche, _ -> { coeur=jeu.coeur;
                pique=jeu.pique;
                carreau=jeu.carreau;
                trefle=jeu.trefle;
                piles=jeu.piles;
                defausse=p :: jeu.defausse;
                pioche=ppioche }

   (* Help_solitaire.pioche jeu *)
;;

(* Q10
   couleur_compatible: couleur -> couleur -> bool
   Renvoie vrai si les couleurs c1 et c2 sont compatibles
    (i.e. c1 est Pique ou Trefle et c2 est Coeur ou Carreau, et vice-versa.
   Renvoie faux dans les autres cas

   0.75 point
*)
let couleur_compatible c1 c2 =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   match c1, c2 with
   (Trefle | Pique), (Coeur | Carreau) -> true
   | (Trefle | Pique), (Trefle | Pique) -> false

   | (Coeur | Carreau), (Coeur | Carreau) -> false
   | (Coeur | Carreau), (Trefle | Pique) -> true

   (* Help_solitaire.couleur_compatible *)
;;

(* Q11 renvoie la valeur suivante à celle donnée en argument.
       Si v vaut Roi, lève l'erreur
       failwith "valeur_suivante: appel sur un roi"

    0.75 point
*)

let valeur_suivante v =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   match v with
   Valeur 1 -> Valeur 2
   | Valeur 2 -> Valeur 3
   | Valeur 3 -> Valeur 4
   | Valeur 4 -> Valeur 5
   | Valeur 5 -> Valeur 6
   | Valeur 6 -> Valeur 7
   | Valeur 7 -> Valeur 8
   | Valeur 8 -> Valeur 9
   | Valeur 9 -> Valeur 10
   | Valeur 10 -> Valet
   | Valet -> Dame
   | Dame -> Roi
   | Roi -> failwith "valeur_suivante: appel sur un roi"

   (* Help_solitaire.valeur_suivante v *)
;;

(* Q12
   est_suivante_meme_couleur: carte -> carte -> bool
   Renvoie vrai si est seulement si suiv est la carte juste après c et que les deux sont de même couleur
   (i.e. toutes les deux du Pique ou toutes les deux du Coeur. Coeur et Carreau ne sont pas la même couleur)
   Si ce n'est pas le cas la fonction renvoie faux.

   0.5 point
 *)
let est_suivante_meme_couleur c suiv =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   (suiv.valeur = (valeur_suivante c.valeur)) && (suiv.couleur = c.couleur)

   (* Help_solitaire.est_suivante_meme_couleur c suiv *)
;;

(* Q13
   est_suivante_couleur_diff c suiv
   Renvoie vrai si et seulement si suiv est la carte juste après c et les deux cartes sont de couleur compatible

   0.5 point
*)
let est_suivante_couleur_diff c suiv =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   (suiv.valeur = (valeur_suivante c.valeur)) && (couleur_compatible c.couleur suiv.couleur)

   (* Help_solitaire.est_suivante_couleur_diff c suiv *)
;;

(* Q14
   cherche_index_pour_suivante: carte list -> carte -> int
   Étant donnée une carte suiv, renvoie la position (à partir de 1) de la carte c telle que
   est_suivante_couleur_diff c suiv, si elle existe.
   Si ce n'est pas le cas, renvoie 0.

   Par exemple, si suiv est le 9 de Pique, cherche_index_pour_suivante l suiv, renvoie la
   position du 8 de cœur ou du 8 de carreau si l'un des deux est présent dans l, et 0 sinon.
   On peut supposer qu'au plus une de ces cartes est dans l.

   Il peut être judicieux de faire une fonction recursive auxiliaire interne.

   1.5 point
*)

let cherche_index_pour_suivante tas suiv =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   let rec finder tas suiv acc=
   match tas with
   [] -> 0
   | t :: ttas -> if (est_suivante_couleur_diff t suiv) then acc else finder ttas suiv (acc+1)
   in
   finder tas suiv 1

   (* Help_solitaire.cherche_index_pour_suivante tas suiv *)
;;

(* Q15
   retire_defausse: jeu -> carte * jeu
   Prend en argument un jeu et renvoie la carte se trouvant au sommet de la defausse
   et le jeu sans cette carte.

   Lève l'erreur :
       failwith "retire_defausse: pas de carte"
   si la défausse est vide.

  0.75 point
*)

let retire_defausse jeu =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   match jeu.defausse with
   [] -> failwith "retire_defausse: pas de carte"
   | d :: ddefausse -> (d,
               { coeur=jeu.coeur;
                pique=jeu.pique;
                carreau=jeu.carreau;
                trefle=jeu.trefle;
                piles=jeu.piles;
                defausse=ddefausse;
                pioche=jeu.pioche })

   (* Help_solitaire.retire_defausse jeu *)
;;

(* Q16
   nieme: 'a list -> n -> 'a
   renvoie le nième élément d'une liste. Doit lever l'erreur
   failwith "nieme: pas assez d'éléments" s'il n'y a pas assez d'éléments.

   On ne peut donc pas utiliser simplement List.nth, car il faut renvoyer
   une erreur différente en cas d'erreur.

   Les indices commencent à partir de 0.

   1 point
*)


let rec nieme l n =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   match l with
   [] -> failwith "nieme: pas assez d'éléments"
   | f :: ll -> if n = 0 then f else nieme ll (n-1)

   (* Help_solitaire.nieme l n *)
;;

(* Q17
   remplace_nieme: 'a list -> int -> 'a -> 'a list
   remplace la nieme valeur de l par v et renvoie la nouvelle liste.
   Lève l'erreur
       failwith "remplace_nieme: pas assez d'éléments"
   s'il n'y a pas assez d'éléments.

   1.25 point
*)

let rec remplace_nieme l n v =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

  match l with
    [] -> if n > 0 then failwith "remplace_nieme: pas assez d'éléments" else []
  | f :: ll -> if n = 0 then v :: (remplace_nieme ll (n-1) v) else f :: (remplace_nieme ll (n-1) v)

   (* Help_solitaire.remplace_nieme l n v *)
;;


(* Q18
   decouvre: carte list * carte list -> carte list * carte list
   La fonction prend en argument une pile (i.e. une paire (visibles, cachees))
   S'il n'y a pas de carte visible et au moins une carte cachée, la première carte
   cachée est mise dans le tas des visibles (et retirée du tas des cachées).
   La nouvelle paire est renvoyée.

   Sinon la pile est renvoyée telle qu'elle.

   0.5 point
*)

let decouvre pile =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   match (fst pile), (snd pile) with
   [], [] -> pile
   | [], _ -> (((List.hd (snd pile)) :: (fst pile)), (List.tl (snd pile)))
   | _, _ -> pile

   (* Help_solitaire.decouvre pile *)
;;

(* Q19 :
   On demande de ne faire que l'UNE des fonctions ci-dessous. Vous pouvez bien sûr chercher
   l'autre si vous voulez, mais vous pouvez laisser l'appel au code du corrigé dans l'une des deux.
   Faire les deux fonctions ne rapporte pas plus de points.

   2 points
*)


(* defausse_vers_pile: jeu -> int -> jeu.
   Cette fonction implémente le mouvement qui consiste à prendre la première pile de la défausse
   (au moyen de retire_defausse) et de la placer sur la pile numéro n (ATTENTION: n va de 0 à 6)
   Deux cas sont possibles :
   - si la pile numéro n est vide (ni carte visible, ni carte cachée) alors on ne peut faire ce
     mouvement que si la carte en haut de la défausse est un Roi.
     sinon on doit lever l'erreur: failwith "defausse_vers_pile: non-roi sur une case vide"

   - si la pile numéro n est non vide, alors il faut vérifier que la carte visible qui est en premier
     est une valeur plus grande que la carte de la défausse et de couleur compatible.
     Par exemple si la carte de la défausse est le 10 de Coeur, la premiere carte visible de la pile
     choisie doit être un Valet de Pique ou un Valet de Trefle (pour pouvoir poser le 10 de Coeur en
     dessous.
     Si la carte de la pile n'a pas la bonne valeur, lever l'erreur:
         failwith "defausse_vers_pile: déplacement invalide"

*)

let defausse_vers_pile jeu n =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   Help_solitaire.defausse_vers_pile jeu n
;;

(* pile_vers_pile: jeu -> int -> int -> jeu.
   Cette fonction réalise le mouvement du plus grand nombre de cartes possible depuis la pile n1
   vers la pile n2 (ATTENTION: n1 et n2 vont de 0 à 6, vous pouvez supposer qu'ils sont distincts,
   pas besoin de le vérifier).

   Pour cela, on récupère les deux piles correspondant à n1 et n2.

   Si la pile de destination est vide, alors lever une erreur :
       failwith ("pile_vers_pile: la pile " ^ (string_of_int n2) ^" est vide")
       (Noter que ce cas empêche de déplacer une suite commençant par un Roi, d'une pile vers
        une pile vide, ce qui est un mouvement inutile)

   Si la pile de destination possède une carte c2 visible, alors rechercher l'index dans les
   cartes visibles de la pile 1, de la carte de valeur juste inférieure à c2 et compatible (au moyen
   de la fonction cherche_index_pour_suivante).

       Si cette dernière renvoie 0, alors lever une erreur:
           failwith "pile_vers_pile: déplacement impossible"
       Si elle renvoie une valeur i >= 1, alors renvoyer le jeu où le i premières cartes
       visibles de la pile n1 sont déplacée en tête des cartes visibles de n2.

   Une fois les cartes de n1 retirées, on n'oubliera pas d'appeler decouvre sur cette pile,
   car si jamais on a retirer toutes les cartes visibles, il faut retourner la premier carte cachée.
*)

let pile_vers_pile jeu n1 n2 =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   match (nieme jeu.piles n1), (nieme jeu.piles n2) with
   ([], []), (_, _) -> failwith ("pile_vers_pile: la pile " ^ (string_of_int n1) ^" est vide")
   | (_, _), ([], []) ->   
                        (let rec findKing list acc=
                        match list with
                        [] -> failwith ("pile_vers_pile: la pile " ^ (string_of_int n2) ^" est vide")
                        | k :: kking -> if (k.valeur = Roi) then acc else findKing kking (acc+1)
                        in
                        let cartes_r, restes_r = n_premieres (fst (nieme jeu.piles n1)) ((findKing (fst (nieme jeu.piles n1)) 0)+1) in
                        let rec replace piles i1 i2 acc=
                        match piles with
                        [] -> []
                        | p :: ppiles ->  match acc with
                                          r1 when r1 = i1 -> (decouvre (restes_r, (snd (nieme jeu.piles i1)))) :: (replace ppiles i1 i2 (acc+1))
                                          | r2 when r2 = i2 -> ((cartes_r @ (fst (nieme jeu.piles i2))), (snd (nieme jeu.piles i2))) :: (replace ppiles i1 i2 (acc+1))
                                          | _ -> p :: (replace ppiles i1 i2 (acc+1))
                        in
                        { coeur=jeu.coeur;
                           pique=jeu.pique;
                           carreau=jeu.carreau;
                           trefle=jeu.trefle;
                           piles=(replace jeu.piles n1 n2 0);
                           defausse=jeu.defausse;
                           pioche=jeu.pioche})
   | _, _ ->
            if (cherche_index_pour_suivante (fst (nieme jeu.piles n1)) (List.hd (fst (nieme jeu.piles n2)))) = 0 then (failwith ("pile_vers_pile: déplacement impossible")) else
            let cartes_p, restes_p = n_premieres (fst (nieme jeu.piles n1)) (cherche_index_pour_suivante (fst (nieme jeu.piles n1)) (List.hd (fst (nieme jeu.piles n2)))) in
            let rec replace piles i1 i2 acc=
            match piles with
            [] -> []
            | p :: ppiles ->  match acc with
                              r1 when r1 = i1 -> (decouvre (restes_p, (snd (nieme jeu.piles i1)))) :: (replace ppiles i1 i2 (acc+1))
                              | r2 when r2 = i2 -> ((cartes_p @ (fst (nieme jeu.piles i2))), (snd (nieme jeu.piles i2))) :: (replace ppiles i1 i2 (acc+1))
                              | _ -> p :: (replace ppiles i1 i2 (acc+1))
            in
            { coeur=jeu.coeur;
                pique=jeu.pique;
                carreau=jeu.carreau;
                trefle=jeu.trefle;
                piles=(replace jeu.piles n1 n2 0);
                defausse=jeu.defausse;
                pioche=jeu.pioche}

   (* Help_solitaire.pile_vers_pile jeu n1 n2 *)
;;

(* Q20:
   Pour chacune des fonctions suivantes, donner son type dans le commentaire ci-dessous :

   tas_par_couleur : jeu -> couleur -> carte list
   range_tas_par_couleur : jeu -> couleur -> carte list -> jeu
   range_carte : jeu -> carte -> jeu

   0.75 point
*)

let tas_par_couleur jeu c =
    match c with
        Coeur -> jeu.coeur
        | Pique -> jeu.pique
        | Trefle -> jeu.trefle
        | Carreau -> jeu.carreau
;;

let range_tas_par_couleur jeu c tas =
    match c with
        Coeur -> { jeu with coeur = tas }
        | Pique -> { jeu with pique = tas }
        | Trefle -> { jeu with trefle = tas }
        | Carreau -> { jeu with carreau = tas }
;;

let range_carte jeu c =
    let tas = tas_par_couleur jeu c.couleur in
    let ntas = match tas with
                [] -> if c.valeur = Valeur 1 then [ c ] else failwith "range_carte: carte invalide"
                | c2:: _ -> if est_suivante_meme_couleur c2 c then c :: tas else failwith "range_carte: carte invalide"
    in
    range_tas_par_couleur jeu c.couleur ntas

;;

(* Q21
   victoire: jeu -> bool
   La fonction renvoie vrai si et seulement si jeu correspond à une partie terminée, c'est à dire :
   - la pioche ET la défausse sont vides
   - chaque paquet de cartes de couleur est de taille 13
   (et si les fonctions précédentes ne sont pas buggées, alors cela implique
    que toutes les piles sont vides)

  0.5 point
*)

let victoire jeu =
(* Commentez la ligne ci-dessous et mettez votre code.
   Si votre code ne fonctionne pas, commentez le et remettez cette ligne. *)

   (List.length jeu.defausse = 0) && (List.length jeu.pioche = 0) && (List.length jeu.coeur = 13) && (List.length jeu.pique = 13) && (List.length jeu.carreau = 13) && (List.length jeu.trefle = 13)
   && (List.length (fst (nieme jeu.piles 1)) = 0) && (List.length (snd (nieme jeu.piles 1)) = 0) && (List.length (fst (nieme jeu.piles 2)) = 0) && (List.length (snd (nieme jeu.piles 2)) = 0)
   && (List.length (fst (nieme jeu.piles 3)) = 0) && (List.length (snd (nieme jeu.piles 3)) = 0) && (List.length (fst (nieme jeu.piles 4)) = 0) && (List.length (snd (nieme jeu.piles 4)) = 0)
   && (List.length (fst (nieme jeu.piles 5)) = 0) && (List.length (snd (nieme jeu.piles 5)) = 0) && (List.length (fst (nieme jeu.piles 6)) = 0) && (List.length (snd (nieme jeu.piles 6)) = 0)
;;
