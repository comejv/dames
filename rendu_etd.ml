(* -----------------------------------------------------------------------------
   rendu_etd_NGUYEN-VINCENT.ml : Compte rendu du projet (partie 1)

   NGUYEN Chu Hoang Anh  \
   VINCENT Côme           - >  Groupe C
   
   -------------------------------------------------------------------------- *)

type dimension = int;; (*restreint aux entiers strictement positifs*)

type case = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)

(* Q1 *)
(* i < -dim représente les cases du triangle du protagoniste, son camp.
Dans un plateau où dim = 3 cela représente 6 cases.
i > dim représente les cases du camp opposé, le camp sud. 
j < -dim représente les cases du camp au nord ouest, à gauche du camp nord.
(𝑖, 𝑗, 𝑘) = (2𝑑𝑖𝑚, −𝑑𝑖𝑚, −𝑑𝑖𝑚) représente la case la plus au nord du plateau,
la pointe nord de l’étoile.
(𝑖, 𝑗, 𝑘) = (−𝑑𝑖𝑚 − 1, 1, 𝑑𝑖𝑚) représente la case en haut à gauche du camp du protagoniste.
𝑖 ≥ −𝑑𝑖𝑚 ∧ 𝑗 ≥ −𝑑𝑖𝑚 ∧ 𝑘 ≥ −𝑑𝑖𝑚 est un triangle dont les sommets ont pour coordonnées :
(6, -3, -3) (-3, -3, 6) (-3, 6, -3), c’est à dire que l’on a tout le plateau à l’exception
du camp du protagoniste et des camps nord ouest et nord est. *)

type vecteur = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)


type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (*Les couleurs des joueurs*)
               | Libre 
               | Code of string;; (*une chaine restreinte a 3 caracteres*)


type case_coloree = case * couleur;;

type configuration = case_coloree list * couleur list * dimension;; (*sans case libre*)
          
type coup = Du of case * case | Sm of case list;;

let indice_valide (x:int) (dim:dimension) : bool =
  x >= -2*dim && x<= 2*dim;;

let est_case ((i,j,k):case):bool=
 (i+j+k=0);;

(* Q2 *)
let est_dans_losange ((i, j, k) : case) (dim:dimension): bool =
  est_case (i,j,k) && j<=dim && j>=(-dim) && k<=dim && k>=(-dim);;

(* Q3 *)
let est_dans_etoile ((i, j, k) : case) (dim:dimension) : bool =
  (* Pour qu’une case soit dans l’étoile il faut qu’elle soit 
  dans un des deux triangles qui la compose : *)
  (* dans triangle pointe nord *)
  est_case (i,j,k) && (i >=(-dim) && i <=2*dim && j>=(-dim) && j<=2*dim && k>=(-dim) && k<=2*dim 
  (* ou dans triangle pointe sud *)
  || i >=(-2*dim) && i <=dim && j>=(-2*dim) && j<=dim && k>=(-2*dim) && k<=dim);;

(* Q4 *)
(* Les coordonnées de la case (i, j, k) après une rotation d’un
sixième de tour dans le sens anti-horaire deviennent (-k, -i, -j).
Pour gérer plus d’une rotation on rend récursive la fonction, avec
comme mesure m le nombre de rotations, et comme équations récursives :
(i, j, k) = (-k, -i, -j) si m=1 ;
(i, j, k) = rotation (m-1) (-k, -i, -j) sinon.
Comme la mesure diminue de 1 à chaque itération, la fonction a toujours une fin.*)
let rec rotation (m:int)((i, j, k) : case): case =
  if m = 1 then (-k, -i, -j) else rotation (m-1) (-k, -i, -j);;

(* Q5 *)
(* Pour implémenter la translation on renvoie simplement l’addition des
coordonnées de la case (i, j, k) et du vecteur (m, n, o) *)
let translation ((i, j, k) : case)((m, n, o): vecteur): case =
  (i+m, j+n, k+o);;

(* Q6 *)
(* Pour implémenter diff_cases on renvoie simplement la différence des
coordonnées de la case de départ (i, j, k) et de la case d’arrivée (m, n, o) *)
let diff_case ((i, j, k): case)((m, n, o):case):vecteur =
  (i-m, j-n, k-o);;

(* Q7 *)
(* Pour vérifier si deux cases c1 et c2 sont voisines on vérifie si le vecteur
(a, b, c) = diff_cases c1 c2 contient au moins une coordonnée nulle et qu’aucune
ne dépasse 1 en valeur absolue *)
let sont_cases_voisines (c1: case)(c2:case):bool=
  (* on vérifie que les deux cases sont adjacentes en regardant leur différence*)
  let (a, b, c) = (diff_case c1 c2) in
    (a=0||b=0||c=0)&&(abs a<2)&&(abs b<2)&&(abs c<2);;

(* Q8 *)
(* Pour vérifier que le pivot est possible on vérifie qu'il y a un nombre pair de
cases entre le départ (exclu) et l’arrivée (inclus) et qu'elles sont alignées ;
si oui on translate de la moitiée de l'écart entre elles pour obtenir le pivot *)
let calcul_pivot ((i, j, k): case)((m, n, o):case):case option=
  let (a,b,c) = diff_case (m, n, o) (i, j, k) in
    (* on vérifie qu'il y a un nombre pair entre les cases et qu'elles sont alignées*)
    if (i=m||j=n||k=o)&&(a mod 2 = 0 && b mod 2 = 0 && c mod 2 = 0)
      (* si oui on translate de la moitiée de l'écart entre elles *)
      then Some (translation (i,j,k) (a/2,b/2,c/2))
    else
      None;;

(* Q9 *)
(* Pour obtenir la distance on calcul la différence de cases avec diff_case.
Le résultat contiendra un 0 car les cases sont alignées, et potentiellement une
valeur négative. La distance c’est la valeur absolue d’une des coordonnées non nulle *)
let vec_et_dist ((i,j,k):case)((m,n,o):case):case*int=
  let (a,b,c) = diff_case (m,n,o) (i,j,k) in
    (* renvoie couple nul si cases pas alignées *)
    if a != 0 && b != 0 && c != 0 then ((0,0,0),0)
    else
      (* Pour éviter de chercher quelle coordonnée est nulle on
      additionne tout et divise par deux, on obtient la distance *)
      let d=((abs a + abs b + abs c)/2) in
        ((a/d,b/d,c/d), d);;

(* Q10 *)
let tourner_liste (l: 'a list):'a list=
  match l with
  |[] -> []
  |e::r -> r@[e];;

let rec der_liste (l:'a list):'a=
  match l with
  |e::[] -> e
  |e::r -> der_liste r;;

(* Q11 *)
(* remplir_segment 1 (i,j,k) = [(i,j,k)]
   remplir_segment n>1 (i,j,k) = (i,j,k) :: remplir_segment (n-1) (i,j+1,k-1)  *)
let rec remplir_segment (n:int)((i,j,k):case):case list=
  if n=1 then [(i,j,k)]
  else (i,j,k)::(remplir_segment (n-1) (i,j+1,k-1));;

(* Q12 *)
(* remplir_triangle_haut 1 (i,j,k) = [i,j,k]
   remplir_triangle_haut n>1 (i,j,k) = remplir_segment n (i,j,k) @ remplir_triangle_haut n-1 (i+1,j,k-1) *)
let rec remplir_triangle_haut (n:int)((i,j,k):case):case list=
  if n=1 then [i,j,k]
  else remplir_segment n (i,j,k) @ remplir_triangle_haut (n-1) (i+1,j,k-1);;

(* Q13 *)
(* remplir_triangle_bas 1 (i,j,k) = [i,j,k]
   remplir_triangle_bas n>1 (i,j,k) = remplir_segment n (i,j,k) @ remplir_triangle_bas n-1 (i-1,j+1,k) *)
let rec remplir_triangle_bas (n:int)((i,j,k):case):case list=
  if n=1 then [i,j,k]
  else remplir_segment n (i,j,k) @ remplir_triangle_bas (n-1) (i-1,j+1,k);;

(* Q14 *)
let colorie (c:couleur)(l:case list):case_coloree list=
  List.map (fun x -> x,c) l;;

(* Q15 *)
(* On utilise function pour faire du pattern matching pour prendre la case sans la couleur *)
let tourner_conf (r:int)((lcaco,joueurs,dim):configuration):configuration=
  List.map (function (case,couleur) -> (rotation r case),couleur) lcaco,tourner_liste joueurs,dim;;

(* Q16 *)
let creer_camp (joueur:couleur)((lcaco,lcodes,d):configuration):configuration=
  (colorie joueur (remplir_triangle_bas d (-d-1,1,d))@lcaco,joueur::lcodes,d);;

let remplir_init (lj:couleur list)(d:int):configuration=
  List.fold_left (fun c j -> creer_camp j (tourner_conf (6 / List.length lj) c)) ([], [], d) lj;;

let rec associe a l defaut=
  match l with
  | [] -> defaut
  | (a2, b) :: suite -> if a = a2 then b else associe a suite defaut;;

(*AFFICHAGE (fonctionne si les fonctions au dessus sont remplies)*)
(*transfo transforme des coordonnees cartesiennes (x,y) en coordonnees de case (i,j,k)*)
let transfo x y = (y, (x-y)/2,(-x-y)/2);;

let couleur2string (coul:couleur):string =
  match coul with
  | Libre -> " . "
  | Code s -> s  
  | Vert -> " V "
  | Jaune -> " J "
  | Rouge -> " R "
  | Noir -> " N "
  | Bleu -> " B "
  | Marron -> " M ";;

let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
    if m = (4 * dim) + 1 then " " (*fin de ligne*)
    else
      let c = transfo m n in
      (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
      if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then
        "   "^ affiche_ligne n (m + 1) config
      else (*ceci est une case ou bien en dehors du plateau*)
       (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;


let affiche (config:configuration):unit =
  let (_,_,dim)=config in
    let rec affiche_aux n =
      if n = - 2 * dim - 1 then ()
      else
      begin
      print_endline (affiche_ligne n (-4*dim-1) config);
      print_endline "\n";
      affiche_aux (n - 1)
      end
    in
    affiche_aux (2*dim+1);;

(* 
let conf_1=([((0,0,0),Jaune)],[Jaune],2);;
affiche conf_1;;
let conf_reggae=([((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)],[Vert;Jaune;Rouge],1);;
affiche conf_reggae;;
let conf_vide=([],[],2);;
affiche conf_vide;;
 *)

(* La configuration initiale avec deux joueurs et un plateau de dimension 2*)
(* Pourquoi une case colorée en dehors du plateau pour chaque ? *)
let conf_init : configuration =
  ([((3, -1, -2), Jaune); ((3, -2, -1), Jaune); ((4, -2, -2), Jaune); ((5, -3, -2), Jaune); (* liste cases colorées *)
  ((-3, 1, 2), Vert); ((-3, 2, 1), Vert);((-4, 2, 2), Vert); ((-5, 3, 2), Vert)],
   [Vert; Jaune], (* liste couleurs *)
   2);; (* dimension *)

affiche conf_init;;
(*A essayer apres avoir fait remplir_init
affiche (remplir_init [Code "Ali";Code "Bob";Code "Jim"] 3);;
*)