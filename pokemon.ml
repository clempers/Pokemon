open Graphics;;
open Unix;;
open Random;;
open Printf;;
open Scanf;;

let default a b =
  match b with
  | Some x -> x
  | None -> a

let map f b =
  match b with
  | Some x -> Some (f x)
  | None -> None

let may f b =
  match b with
  | Some x -> f x
  | None -> ()

let ave x y = (x + y)/2

let concat x y = String.concat "" [x ; y]

let ( */* ) x y = (float_of_int x) /. (float_of_int y);;

let tailMap (f : 'a -> 'b) (li : 'a list) : 'b list =
  let rec revM f l acc =
    match l with
    | [] -> acc
    | x::t -> revM f t ((f x)::acc) in
  List.rev (revM f li []);;


Random.self_init()

type ptype = Normal | Fight | Flying | Poison | Ground | Rock | Bug | Ghost |
  Steel | Fire | Water | Grass | Elec | Psy | Ice | Dragon | Dark | Fairy

type stat = Health | Attack | Defense | SAttack | SDefense | Speed

let typeList = [Normal ; Fight ; Flying ; Poison ; Ground ; Rock ; Bug ; Ghost ;
  Steel ; Fire ; Water ; Grass ; Elec ; Psy ; Ice ; Dragon ; Dark ; Fairy]

let string_to_ptype ptype = 
  match ptype with
  | "Normal" -> Normal
  | "Fight" -> Fight
  | "Flying" -> Flying
  | "Poison" -> Poison
  | "Ground" -> Ground
  | "Rock" -> Rock
  | "Bug" -> Bug
  | "Ghost" -> Ghost
  | "Steel" -> Steel
  | "Fire" -> Fire
  | "Water" -> Water
  | "Grass" -> Grass
  | "Elec" -> Elec
  | "Psy" -> Psy
  | "Ice" -> Ice
  | "Dragon" -> Dragon
  | "Dark" -> Dark
  | "Fairy" -> Fairy

let ptype_to_string ptype = 
  match ptype with
  | Normal -> "Normal"
  | Fight -> "Fight"
  | Flying -> "Flying"
  | Poison -> "Poison"
  | Ground -> "Ground"
  | Rock -> "Rock"
  | Bug -> "Bug"
  | Ghost -> "Ghost"
  | Steel -> "Steel"
  | Fire -> "Fire"
  | Water -> "Water"
  | Grass -> "Grass"
  | Elec -> "Elec"
  | Psy -> "Psy"
  | Ice -> "Ice"
  | Dragon -> "Dragon"
  | Dark -> "Dark"
  | Fairy -> "Fairy"

let ptype_to_int ptype = 
  match ptype with
  | Normal -> 0
  | Fight -> 1
  | Flying -> 2
  | Poison -> 3
  | Ground -> 4
  | Rock -> 5
  | Bug -> 6
  | Ghost -> 7
  | Steel -> 8
  | Fire -> 9
  | Water -> 10
  | Grass -> 11
  | Elec -> 12
  | Psy -> 13
  | Ice -> 14
  | Dragon -> 15
  | Dark -> 16
  | Fairy -> 17

let int_to_ptype ptype = 
  match ptype with
  | 0 -> Normal
  | 1 -> Fight 
  | 2 -> Flying 
  | 3 -> Poison 
  | 4 -> Ground 
  | 5 -> Rock 
  | 6 -> Bug 
  | 7 -> Ghost 
  | 8 -> Steel 
  | 9 -> Fire 
  | 10 -> Water 
  | 11 -> Grass 
  | 12 -> Elec 
  | 13 -> Psy 
  | 14 -> Ice 
  | 15 -> Dragon 
  | 16 -> Dark 
  | 17 -> Fairy 
  | _ -> Normal

let ptype_to_color_code ptype = 
  match ptype with
  | Normal -> (168,168,120)
  | Fight -> (120,48,40)
  | Flying -> (168,144,240)
  | Poison -> (160, 64, 160)
  | Ground -> (224, 192, 104)
  | Rock -> (184, 160, 56)
  | Bug -> (168, 184, 32)
  | Ghost -> (112, 88, 152)
  | Steel -> (184, 184, 208)
  | Fire -> (240, 128, 48)
  | Water -> (104, 144, 240)
  | Grass -> (120, 200, 80)
  | Elec -> (248, 208, 48)
  | Psy -> (248, 88, 136)
  | Ice -> (152, 216, 216)
  | Dragon -> (112, 56, 248)
  | Dark -> (112, 88, 72)
  | Fairy -> (238, 153, 172)

module PokeTypes = struct
  type t = ptype
  let compare p1 p2 =
    let v1 = ptype_to_int p1 in
    let v2 = ptype_to_int p2 in
    if v1 < v2 then -1
    else if v1 = v2 then 0
    else 1
end

module PM = Map.Make(PokeTypes);;

module PS = Set.Make(PokeTypes);;

type attack = {
  t : ptype ;
  isS : bool ;
  b : int ;
  status : stat option
}

type pokemon = {
  ch : int ;
  bh : int ;
  ba : int ;
  buffa : int;
  bd : int ;
  buffd : int;
  bsa : int ;
  buffsa : int ;
  bsd : int ;
  buffsd : int;
  bs : int ;
  buffs : int ;
  t1 : ptype ;
  t2 : ptype option ;
  ats : attack list ;
  id : int ;
  buffc : float ;
  enabled : bool
}

let calc_buff base buff =
  int_of_float (((2 + buff)*/* 2) *. (float_of_int base))

let calc_pstat (p : pokemon) stat =
  match stat with
  | Health -> p.ch
  | Attack -> calc_buff p.ba p.buffa
  | Defense -> calc_buff p.bd p.buffd
  | SAttack -> calc_buff p.bsa p.buffsa
  | SDefense -> calc_buff p.bsd p.buffsd
  | Speed -> calc_buff p.bs p.buffs


let int_to_stat i =
  match i with
  | 0 -> Health
  | 1 -> Attack
  | 2 -> Defense
  | 3 -> SAttack
  | 4 -> SDefense
  | 5 -> Speed

let string_to_stat i =
  match i with
  | "Health" -> Health
  | "Attack" -> Attack
  | "Defense" -> Defense
  | "SAttack" -> SAttack
  | "SDefense" -> SDefense
  | "Speed" -> Speed

let stat_to_string i =
  match i with
  | Health -> "Health"
  | Attack -> "Attack"
  | Defense -> "Defense"
  | SAttack -> "SAttack"
  | SDefense -> "SDefense"
  | Speed -> "Speed"

let stat_to_int i =
  match i with
  | Health -> 0
  | Attack -> 1
  | Defense -> 2
  | SAttack -> 3
  | SDefense -> 4
  | Speed ->5

let pokemon_to_color t1 t2 =
  match (ptype_to_color_code t1, map ptype_to_color_code t2) with
  | ( (r,g,b), None) -> Graphics.rgb r g b
  | ( (r1, g1, b1), Some (r2, g2, b2)) -> Graphics.rgb (ave r1 r2) (ave g1 g2) (ave b1 b2)

type typeE = {
  at : ptype ;
  ne : PS.t ;
  nve : PS.t ;
  se : PS.t
}

let normalE : typeE =  {
  at = Normal ;
  ne = PS.of_list [Ghost] ;
  nve = PS.of_list [Rock ; Steel] ;
  se = PS.empty
}


let fightE : typeE =  {
  at = Fight ;
  ne = PS.of_list [Ghost] ;
  nve = PS.of_list [Flying ; Poison ; Bug ; Psy ; Fairy] ;
  se = PS.of_list [Normal ; Rock ; Steel ; Ice ; Dark]
}

let flyingE : typeE =  {
  at = Flying ;
  ne = PS.empty ;
  nve = PS.of_list [Rock ; Steel ; Elec] ;
  se = PS.of_list [Fight ; Bug ; Grass]
}

let poisonE : typeE =  {
  at = Poison ;
  ne = PS.of_list [Steel] ;
  nve = PS.of_list [Poison ; Ground ; Rock ; Ghost] ;
  se = PS.of_list [Grass ; Fairy]
}

let groundE : typeE =  {
  at = Ground ;
  ne = PS.of_list [Flying] ;
  nve = PS.of_list [Bug ; Grass] ;
  se = PS.of_list [Poison ; Rock ; Steel ; Fire ; Elec]
}

let rockE : typeE =  {
  at = Rock ;
  ne = PS.empty ;
  nve = PS.of_list [Fight ; Ground ; Steel] ;
  se = PS.of_list [Flying ; Bug ; Fire ; Ice]
}

let bugE : typeE =  {
  at = Bug ;
  ne = PS.empty ;
  nve = PS.of_list [Fight ; Flying ; Poison ; Ghost ; Steel ; Fire ; Fairy] ;
  se = PS.of_list [Grass ; Psy ; Dark]
}

let ghostE : typeE =  {
  at = Ghost ;
  ne = PS.of_list [Normal] ;
  nve = PS.of_list [Dark] ;
  se = PS.of_list [Dark ; Psy]
}

let steelE : typeE =  {
  at = Steel ;
  ne = PS.empty ;
  nve = PS.of_list [Steel ; Fire ; Water ; Elec] ;
  se = PS.of_list [Rock ; Ice ; Fairy]
}

let fireE : typeE =  {
  at = Fire ;
  ne = PS.empty ;
  nve = PS.of_list [Rock ; Water ; Dragon] ;
  se = PS.of_list [Bug ; Steel ; Grass ; Ice]
}

let waterE : typeE =  {
  at = Water ;
  ne = PS.empty ;
  nve = PS.of_list [Water ; Grass ; Dragon] ;
  se = PS.of_list [Ground ; Rock ; Fire]
}

let grassE : typeE =  {
  at = Grass ;
  ne = PS.empty ;
  nve = PS.of_list [Flying ; Poison ; Bug ; Steel ; Fire ; Grass ; Dragon] ;
  se = PS.of_list [Ground ; Rock ; Water]
}

let elecE : typeE =  {
  at = Elec ;
  ne = PS.of_list [Ground] ;
  nve = PS.of_list [Grass ; Elec ; Dragon] ;
  se = PS.of_list [Flying ; Water]
}

let psyE : typeE =  {
  at = Psy ;
  ne = PS.of_list [Dark] ;
  nve = PS.of_list [Steel ; Psy] ;
  se = PS.of_list [Fight ; Poison]
}

let iceE : typeE =  {
  at = Ice ;
  ne = PS.empty ;
  nve = PS.of_list [Steel ; Fire ; Water ; Ice] ;
  se = PS.of_list [Flying ; Ground ; Grass ; Dragon]
}

let dragonE : typeE =  {
  at = Dragon ;
  ne = PS.of_list [Fairy] ;
  nve = PS.of_list [Steel] ;
  se = PS.of_list [Dragon]
}

let darkE : typeE =  {
  at = Dark ;
  ne = PS.empty ;
  nve = PS.of_list [Fight ; Dark ; Fairy] ;
  se = PS.of_list [Ghost ; Psy]
}

let fairyE : typeE =  {
  at = Fairy ;
  ne = PS.empty ;
  nve = PS.of_list [Poison ; Steel ; Fairy] ;
  se = PS.of_list [Fight ; Ice ; Dragon]
}

let typeChart = PM.empty |>
  PM.add Normal normalE |>
  PM.add Fight fightE |>
  PM.add Flying flyingE |>
  PM.add Poison poisonE |>
  PM.add Ground groundE |>
  PM.add Rock rockE |>
  PM.add Bug bugE |>
  PM.add Ghost flyingE |>
  PM.add Steel flyingE |>
  PM.add Fire flyingE |>
  PM.add Water flyingE |>
  PM.add Grass flyingE |>
  PM.add Elec flyingE |>
  PM.add Psy flyingE |>
  PM.add Ice flyingE |>
  PM.add Dragon flyingE |>
  PM.add Dark flyingE |>
  PM.add Fairy flyingE 

let random () = 0.85 +. Random.float 0.15

let random_type () = int_to_ptype (Random.int 18)

let rec random_new_value f found = 
  let t = f () in
  if List.mem t found then random_new_value f found
  else t

let random_values f n =
  let rec rv f n v =
    match n with
    | 0 -> v
    | x -> 
      rv f (x-1) ((random_new_value f v)::v) in
  rv f n []

let calculate_base base max remP remS =
  let rec rcb base max remP remS acc =
    if remS = 0 then acc
    else if (remS - 1) * max < remP then
      let req = remP - ((remS - 1) * max) in 
      let stat = req + (Random.int (1 + max - req)) in
      (base + stat) :: acc |>
      rcb base max (remP - stat) (remS - 1)
    else 
      let stat = Random.int (1 + max) in
      (base + stat) :: acc |>
      rcb base max (remP - stat) (remS -1)in
  rcb base max remP remS []

let gen_at ?at ?found () : attack =
  let typ = default (random_new_value random_type (default [] found)) at in
  {t = typ ; isS = Random.bool() ; b = 90 ; status = None}

let gen_buff ?stat () : attack =
  {t = Normal ; isS = true ; b = 0 ; status = Some (int_to_stat (default (1+(Random.int 5)) stat))}

let gen_ats n found : attack list =
  let rec gar n found acc =
    match n with
    | 0 -> acc
    | x -> 
      let attack = gen_at ~found:found () in
      gar (n-1) (attack.t :: found) (attack :: acc) in
  gar n found []

let gen_pok_ats t1 t2 buffs =
  match (t2, buffs) with
  | (None, false) -> (gen_at ~at:t1 ()) :: (gen_ats 3 [t1])
  | (None, true) -> (gen_at ~at:t1 ()) :: (gen_buff ()) :: (gen_ats 2 [t1])
  | (Some ty2, false) -> (gen_at ~at:t1 ()) :: (gen_at ~at:ty2 ()) :: (gen_ats 2 [t1 ; ty2])
  | (Some ty2, true) -> (gen_at ~at:t1 ()) :: (gen_at ~at:ty2 ()) :: (gen_buff ()) :: (gen_ats 1 [t1 ; ty2])

let calc_h base ev =
  2*((2 * base) + 31 + (if ev then 63 else 0) + 110)

let calc_stat base ev =
  (2 * base) + 31 + (if ev then 63 else 0) + 5

let buffRef = ref false
let disableRef = ref false
let loadRef = ref false
let gridW = ref 500
let displayW = ref 3
let saveNameRef = ref ""

let at_to_string at =
  String.concat "," [
    ptype_to_string at.t ;
    string_of_bool at.isS ;
    string_of_int at.b ;
    default "" (map stat_to_string at.status)]

let ats_to_string ats =
  tailMap at_to_string ats |>
  String.concat ","

let load_at t isS b status : attack = {
  t = string_to_ptype t ;
  isS = isS ;
  b = b ;
  status = if 0 = String.length status then None else Some (string_to_stat status)
}

let loadP saveFile : pokemon =
        bscanf saveFile "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%[^,],%[^,],%[^,],%B,%d,%[^,],%[^,],%B,%d,%[^,],%[^,],%B,%d,%[^,],%[^,],%B,%d,%[^,],%d,%f\n"
        (fun ch bh ba buffa bd buffd bsa buffsa bsd buffsd bs buffs t1 ot2 a1t a1s a1b a1st a2t a2s a2b a2st a3t a3s a3b a3st a4t a4s a4b a4st id buffc ->
                {
                        ch = ch ;
  bh = bh ;
  ba = ba ;
  buffa = buffa ;
  bd = bd ;
  buffd = buffd ;
  bsa = bsa ;
  buffsa = buffsa ;
  bsd = bsd ;
  buffsd = buffsd ;
  bs = bs ;
  buffs = buffs ;
  t1 = string_to_ptype t1 ;
  t2 = if 0 = String.length ot2 then None else Some(string_to_ptype ot2) ;
  ats = [
    load_at a1t a1s a1b a1st ;
    load_at a2t a2s a2b a2st ;
    load_at a3t a3s a3b a3st ;
    load_at a4t a4s a4b a4st ;
] ;
  id = id ;
  buffc = buffc ;
  enabled = true
}
                )

let saveP p saveFile =
        String.concat "," [
          string_of_int p.ch ; 
          string_of_int p.bh ; 
          string_of_int p.ba ; 
          string_of_int p.buffa ;
          string_of_int p.bd ; 
          string_of_int p.buffd ;
          string_of_int p.bsa ; 
          string_of_int p.buffsa ;
          string_of_int p.bsd ; 
          string_of_int p.buffsd ;
          string_of_int p.bs ; 
          string_of_int p.buffs ;
          ptype_to_string p.t1 ;
          default "" (map ptype_to_string p.t2) ;
          ats_to_string p.ats ;
          string_of_int p.id ;
          string_of_float p.buffc] |>
        fprintf saveFile "%s\n"

let genP ?saveFile = 
  let [h ; a ; d ; sa ; sd; s] = calculate_base 40 80 240 6 in
  let evs = random_values (fun () -> Random.int 6) 2 in
  let pt1 = random_type() in
  let pt2 = 
    let t = random_type() in
    if t = pt1 then None else Some t in
  let bc = Random.float (if !buffRef then 1.0 else 0.0) in
  let poke : pokemon = {
    ch = calc_h h (List.mem 0 evs) ;
    bh = calc_h h (List.mem 0 evs) ;
    ba = calc_stat a (List.mem 1 evs) ;
    buffa = 0 ;
    bd = calc_stat d (List.mem 2 evs) ;
    buffd = 0 ;
    bsa = calc_stat sa (List.mem 3 evs) ;
    buffsa = 0;
    bsd = calc_stat sd (List.mem 4 evs) ;
    buffsd = 0;
    bs = calc_stat s (List.mem 5 evs) ;
    buffs = 0;
    t1 = pt1 ;
    t2 = pt2 ;
    ats = gen_pok_ats pt1 pt2 (bc > 0.5);
    id = Random.int 1000000 ;
    buffc = if bc < 0.5 then 0.0 else bc-.0.5 ;
    enabled = true
  } in
  saveFile |> map (saveP poke);
  poke


let stabMult at p =
  match (p.t1, p.t2) with
  | (t1, None) -> if at = t1 then 1.5 else 1.0
  | (t1, Some t2) -> if at = t1 || at = t2 then 1.5 else 1.0

let enable p = {
  ch = p.ch;
  bh = p.bh ;
  ba = p.ba ;
  buffa = p.buffa ;
  bd = p.bd ;
  buffd = p.buffd ;
  bsa = p.bsa ;
  buffsa = p.buffsa ;
  bsd = p.bsd ;
  buffsd = p.buffsd ;
  bs = p.bs ;
  buffs = p.buffs ;
  t1 = p.t1 ;
  t2 = p.t2 ;
  ats = p.ats ;
  id = p.id ;
  buffc = p.buffc ;
  enabled = true
}

let disable p = {
  ch = p.ch;
  bh = p.bh ;
  ba = p.ba ;
  buffa = p.buffa ;
  bd = p.bd ;
  buffd = p.buffd ;
  bsa = p.bsa ;
  buffsa = p.buffsa ;
  bsd = p.bsd ;
  buffsd = p.buffsd ;
  bs = p.bs ;
  buffs = p.buffs ;
  t1 = p.t1 ;
  t2 = p.t2 ;
  ats = p.ats ;
  id = p.id ;
  buffc = p.buffc ;
  enabled = false
}

let setH h p = {
  ch = if h < 0 then 0 else if h > p.bh then p.bh else h;
  bh = p.bh ;
  ba = p.ba ;
  buffa = p.buffa ;
  bd = p.bd ;
  buffd = p.buffd ;
  bsa = p.bsa ;
  buffsa = p.buffsa ;
  bsd = p.bsd ;
  buffsd = p.buffsd ;
  bs = p.bs ;
  buffs = p.buffs ;
  t1 = p.t1 ;
  t2 = p.t2 ;
  ats = p.ats ;
  id = p.id ;
  buffc = p.buffc ;
  enabled = p.enabled
}

let getBuff p buff =
  match buff with
  | Health -> 6
  | Attack -> p.buffa
  | Defense -> p.buffd
  | SAttack -> p.buffsa
  | SDefense -> p.buffsd
  | Speed -> p.buffs

let subH dam p = 
  setH (p.ch - dam) p

let typeEffective at dt = 
  let te = PM.find at typeChart in
  if PS.mem dt te.ne then 0.0
  else if PS.mem dt te.nve then 0.5
  else if PS.mem dt te.se then 2.0
  else 1.0

let typeScalar a p =
  let f = typeEffective a in
  let s1 = f p.t1 in
  let s2 = map f p.t2 |> default 1.0 in
  s1 *. s2

let predictAttack a pa pd =
  let ast = calc_pstat pa in
  let level = 210.0/.250.0 in
  let stats = if a.isS then (ast SAttack) */* 80 else (ast Attack) */* 80 in
  let base = float_of_int a.b in
  let stab = stabMult a.t pa in
  let typeM = typeScalar a.t pd in
  let weakness = (if pd.ch = 0 then 1 else pd.ch) */* (pd.bh) in
  (((level*.stats*.base)+.2.0)*.stab*.typeM)/.weakness |>
  int_of_float

let testPokemon : pokemon = {
  ch = 0 ;
  bh = 0 ;
  ba = 0 ;
  buffa = 0;
  bd = 0 ;
  buffd = 0;
  bsa = 0 ;
  buffsa = 0 ;
  bsd = 0 ;
  buffsd = 0;
  bs = 0 ;
  buffs = 0 ;
  t1 = Normal ;
  t2 = None ;
  ats = [] ;
  id = 0 ;
  buffc = 0.0 ;
  enabled = true
}

let testFightAttack : attack = {
  t = Normal ;
  isS = false ;
  b = 90 ;
  status = None
}

let chooseAttack pa pd =
  tailMap (fun at -> (at, predictAttack at pa pd)) pa.ats |>
  List.fold_left (fun (ma, md) (a, d) -> if d > md then (a,d) else (ma,md)) (testFightAttack, 0)

let calculateAttack a pa pd =
  let ast = calc_pstat pa in
  let bst = calc_pstat pd in
  let level = 210.0/.250.0 in
  let stats = if a.isS then (ast SAttack)*/* (bst SDefense) else (ast Attack) */* (bst Defense) in
  let base = float_of_int a.b in
  let stab = stabMult a.t pa in
  let typeM = typeScalar a.t pd in
  let ran = random() in
  ((level*.stats*.base)+.2.0)*.stab*.typeM*.ran |>
  int_of_float

let attackPokemon a pa pd : pokemon =
  subH (calculateAttack a pa pd) pd

let attackTurn pa pd : pokemon =
  let (a, _) = chooseAttack pa pd in
  attackPokemon a pa pd

let rec draw_secondary_color t1 types i j =
  match types with
  | t2 :: t -> 
    Graphics.set_color (pokemon_to_color t1 (Some t2));
    Graphics.fill_rect (i * 50) (j * 50) 50 50;
    draw_secondary_color t1 t i (j+1)
  | [] -> ()

let rec draw_main_color types i =
  match types with
  | c :: t -> 
    draw_secondary_color c typeList i 0;
    draw_main_color t (i+1)
  | [] -> ()

let pokemon_type_to_string (p : pokemon) =
  map ptype_to_string p.t2 |>
  default "" |>
  concat " " |>
  concat (ptype_to_string p.t1) 

let attack_to_string (a : attack) =
  match a.status with
  | Some status -> stat_to_string status
  | None -> String.concat " " [ptype_to_string a.t ; if a.isS then "Spe" else "Phy" ; string_of_int a.b]

let stat_info p stat =
  let buff = getBuff p stat in
  let dspB = if buff = 0 then "" else concat "+" (string_of_int buff) in
  let display = String.concat "" [stat_to_string stat; ": " ; string_of_int (calc_pstat p stat) ; " "; dspB] in
  display

let drawPokemon p x y ?pd () =
  Graphics.set_color (pokemon_to_color p.t1 p.t2);
  Graphics.fill_rect x y 200 200;
  Graphics.set_color Graphics.black;

  Graphics.moveto x y ;
  concat "Health: " (string_of_int p.ch) |>
  Graphics.draw_string ;

  Graphics.moveto x (y+20) ;
  concat "Attack: " (string_of_int p.ba) |>
  Graphics.draw_string ;

  Graphics.moveto x (y+30) ;
  concat "Defense: " (string_of_int p.bd) |>
  Graphics.draw_string ;

  Graphics.moveto x (y+40) ;
  concat "SAttack: " (string_of_int p.bsa) |>
  Graphics.draw_string ;

  Graphics.moveto x (y+50) ;
  concat "SDefense: " (string_of_int p.bsd) |>
  Graphics.draw_string ;

  Graphics.moveto x (y+60) ;
  concat "Speed: " (string_of_int p.bs) |>
  Graphics.draw_string ;

  let [a1 ; a2 ; a3 ; a4] = p.ats in

  Graphics.moveto x (y+80) ;
  attack_to_string a4 |>
  concat "Attack 4: " |>
  Graphics.draw_string ;

  Graphics.moveto x (y+90) ;
  attack_to_string a3 |>
  concat "Attack 3: " |>
  Graphics.draw_string ;

  Graphics.moveto x (y+100) ;
  attack_to_string a2 |>
  concat "Attack 2: " |>
  Graphics.draw_string ;

  Graphics.moveto x (y+110) ;
  attack_to_string a1 |>
  concat "Attack 1: " |>
  Graphics.draw_string ;

  Graphics.moveto x (y+130) ;
  pokemon_type_to_string p |>
  concat "Type: " |>
  Graphics.draw_string ;

  pd |>
  may (fun p' ->
    let (a, _) = chooseAttack p p' in

    Graphics.moveto x (y+150) ;
    concat "Best Damage: " (string_of_int (calculateAttack a p p')) |>
    Graphics.draw_string ;

    Graphics.moveto x (y+160) ;
    attack_to_string a |>
    concat "Best Attack: " |>
    Graphics.draw_string 
  )

let drawState p1 p2 =
  Graphics.clear_graph();
  drawPokemon p1 0 0 ~pd:p2 ();
  drawPokemon p2 250 0 ~pd:p1 ()

let turn p1 p2 =
  if p1.bs > p2.bs then
    let p2' = attackTurn p1 p2 in
    if p2'.ch > 0 then
      (attackTurn p2' p1, p2')
    else
      (p1, p2')
  else
    let p1' = attackTurn p2 p1 in
    if p1'.ch > 0 then
      (p1', attackTurn p1' p2)
    else
      (p1', p2)

type pGrid = pokemon array array

let loadTbl saveFile =
  Graphics.set_color Graphics.blue;
  let table = Hashtbl.create (!gridW * !gridW) in
  let rec loadNext () =
    let p = loadP saveFile in
    Hashtbl.add table p.id p;
    p in
  let grid = Array.make !gridW (Array.make !gridW testPokemon) in
  let rec initArrayX i =
    if i = !gridW then ()
    else
      begin
        let row = Array.make !gridW testPokemon in
        let rec initArrayY j =
          if j = !gridW then ()
          else
            begin
              Array.set row j (loadNext());
              initArrayY (j+1)
            end in
        Array.set grid i row;
        initArrayY 0;
        Graphics.fill_rect (i* !displayW) 0 (!displayW) 20;
        Graphics.synchronize();
        initArrayX (i+1)
      end in
  initArrayX 0;
  (grid, table)

let initArray ?saveFile ()=
  Graphics.set_color Graphics.blue;
  let grid = Array.make !gridW (Array.make !gridW testPokemon) in
  let rec initArrayX i =
    if i = !gridW then ()
    else
      begin
        let row = Array.make !gridW testPokemon in
        let rec initArrayY j =
          if j = !gridW then ()
          else
            begin
              Array.set row j (genP ?saveFile);
              initArrayY (j+1)
            end in
        Array.set grid i row;
        initArrayY 0;
        Graphics.fill_rect (i* !displayW) 0 (!displayW) 20;
        Graphics.synchronize();
        initArrayX (i+1)
      end in
  initArrayX 0;
  grid

let update (x,y) p grid =
  let row = Array.get grid x in
  Array.set row y p

let find (x,y) grid =
  let row = Array.get grid x in
  Array.get row y

module PokeGrid = struct
  type t = int * int
  let compare (x1,y1) (x2,y2) =
    compare (x1* !gridW +y1) (x2+ !gridW +y2)
end

let drawSquare grid (x, y) =
  let p = find (x,y) grid in
  let x' = x* !displayW in
  let y' = y* !displayW in
  Graphics.set_color (if not !disableRef || p.enabled then pokemon_to_color p.t1 p.t2 else Graphics.black);
  Graphics.fill_rect x' y' !displayW !displayW
  (*Graphics.set_color Graphics.black;
  Graphics.moveto x' y';
  Graphics.draw_string (string_of_int p.ch);
  Graphics.moveto x' (y'+10);
  Graphics.draw_string (string_of_int p.bs)*)

let rec addToList s loc l =
  match l with
  | [] -> [(s, [loc])]
  | (sp, set)::t ->
    if s = sp then
      (sp, loc::set) :: t
    else if s > sp then
      (s, [loc])::(sp,set)::t
    else
      (sp, set)::(addToList s loc t)


let sortedGrid grid =
  let rec sort_x i lx =
    let rec sort_y j ly =
      if j = !gridW then
        ly
      else
        let p = find (i,j) grid in
        if p.enabled then
          addToList (calc_pstat p Speed) (i,j) ly |>
          sort_y (j+1)
        else
          sort_y (j+1) ly in
    if i = !gridW then
      lx
    else
      sort_y 0 lx |>
      sort_x (i+1) in
  sort_x 0 []

let getDown x y grid =
  let y' = if y = 0 then !gridW - 1 else y - 1 in
  ((x,y'), find (x,y') grid)

let getUp x y grid =
  let y' = if y = !gridW - 1 then 0 else y + 1 in
  ((x,y'), find (x,y') grid)

let getLeft x y grid =
  let x' = if x = 0 then !gridW - 1 else x - 1 in
  ((x',y), find (x',y) grid)

let getRight x y grid =
  let x' = if x = !gridW - 1 then 0 else x + 1 in
  ((x',y), find (x',y) grid)

let gridPredict p1 p2 =
  if p1.id = p2.id || p2.ch = 0 then
    None
  else
    Some (chooseAttack p1 p2)

let try_buff buff =
  if buff = 6 then 6
  else buff +1

let buff_pokemon stat (p : pokemon) = {
        ch = p.ch; 
  bh = p.bh ;
  ba = p.ba ;
  buffa = if stat = Attack then try_buff p.buffa else p.buffa; 
  bd = p.bd ;
  buffd = if stat = Defense then try_buff p.buffd else p.buffd; 
  bsa = p.bsa ;
  buffsa = if stat = SAttack then try_buff p.buffsa else p.buffsa; 
  bsd = p.bsd ;
  buffsd = if stat = SDefense then try_buff p.buffsa else p.buffsd; 
  bs = p.bs ;
  buffs = if stat = Speed then try_buff p.buffs else p.buffs; 
  t1 = p.t1 ;
  t2 = p.t2 ;
  ats = p.ats ;
  id = p.id ;
  buffc = p.buffc ;
  enabled = p.enabled
}

let rec pickAttack aops m =
  match (aops,m) with
  | ([],max) -> max
  | (None::t, max) -> pickAttack t max
  | (newM::t, None) -> pickAttack t newM
  | ((Some (newL, newD))::t, Some(oldL, oldD)) ->
    if newD > oldD then
      pickAttack t (Some (newL, newD))
    else
      pickAttack t (Some(oldL, oldD))

let gridAttack grid (x,y) changed =
  let p = find (x,y) grid in
  if p.ch = 0 then
    changed 
  else
    let [ _ ; _ ; buffAt ; _ ] = p.ats in
    let stat = default Health buffAt.status in
    let buff = getBuff p stat in
    if buff < 6 && Random.float 1.0 < p.buffc then
      let p' = buff_pokemon stat p in
      begin
        update (x,y) p' grid;
        changed
      end
    else
      let predict p' = gridPredict p p' in

      let (locd, pd) = getDown x y grid in
      let ad = map (fun (p',_) -> (locd, p')) (predict pd) in

      let (locu, pu) = getUp x y grid in
      let au = map (fun (p',_) -> (locu, p')) (predict pu) in

      let (locl, pl) = getLeft x y grid in
      let al = map (fun (p',_) -> (locl, p')) (predict pl) in

      let (locr, pr) = getRight x y grid in
      let ar = map (fun (p',_) -> (locr, p')) (predict pr) in

      if List.for_all (fun id -> p.id = id) [pd.id ; pu.id ; pl.id ; pr.id] then
        update (x,y) (disable p) grid
      else
        ();

      pickAttack [ad ; au ; al ; ar] None |>
      map (fun (loc, at) ->
        let p' = attackPokemon at p (find loc grid) in
        update loc p' grid;
        let changed' = if p'.ch = 0 then
          (loc, (setH p.bh p))::changed
        else
          changed in
        changed') |>
      default changed

let rec gridAttacks grid order changed =
  let rec performAttacks pSet u =
    match pSet with
    | [] -> u
    | loc :: t ->
       gridAttack grid loc u |>
       performAttacks t in
  match order with
  | [] -> changed
  | (_, pSet)::t -> 
    performAttacks pSet changed |>
    gridAttacks grid t 

let rec save_changed changed saveFile =
  match changed with 
  | [] -> fprintf saveFile "\n"
  | ((x,y), p):: t -> 
                  fprintf saveFile "%d,%d,%d;" x y p.id;
                  save_changed t saveFile

let save_changes changed saveFile =
  fprintf saveFile "%d " (List.length changed);
  save_changed changed saveFile

let enable_adj (x,y) grid =
        tailMap (fun (loc,p) -> update loc (enable p) grid) [getDown x y grid; getUp x y grid; getLeft x y grid; getRight x y grid]

let gridTurn grid ?saveFile () =
  let order = sortedGrid grid in
  let changed = gridAttacks grid order [] in
  map (save_changes changed) saveFile;
  tailMap (fun (loc, p) -> enable_adj loc grid; update loc p grid; loc) changed

let rec drawGrid grid updates =
  tailMap (drawSquare grid) updates

let gridDone grid =
  let p = find (0,0) grid in
  let rec checkX i =
    let rec checkY j =
      if j = !gridW then true
      else
        let p' = find (i,j) grid in
        if p'.id = p.id then
          checkY (j+1)
        else
          false in
    if i = !gridW then true
    else if checkY 0 then
      checkX (i+1)
    else
      false in
  checkX 0

let pretty_float f =
  let str = string_of_float f in
  if String.length str < 4 then str
  else String.sub str 0 4

let changeRange ix iy maxX maxY =
  let rec range x y acc =
    if x = maxX then
      acc
    else if y = maxY then
      range (x+1) 0 acc
    else
      range x (y+1) ((x,y)::acc) in
  range ix iy []

let drawInfoPane x y grid =
  let w = 190 in
  let h = 150 in
  let i = x/ !displayW in
  let j = y/ !displayW in
  if i >= !gridW || j >= !gridW then
    []
  else
    begin
      let p = find (i,j) grid in
      let cst stat = calc_pstat p stat in
      let info stat = stat_info p stat in
      Graphics.set_color (pokemon_to_color p.t1 p.t2);
      Graphics.fill_rect x y w h;
  
      Graphics.set_color Graphics.black;
  
      Graphics.moveto x y ;
      concat "Health: " (string_of_int p.ch) |>
      Graphics.draw_string ;
  
      Graphics.moveto x (y+20) ;
      info Attack |>
      Graphics.draw_string ;
  
      Graphics.moveto x (y+30) ;
      info SDefense |>
      Graphics.draw_string ;
    
      Graphics.moveto x (y+40) ;
      info SAttack |>
      Graphics.draw_string ;
    
      Graphics.moveto x (y+50) ;
      info SDefense |>
      Graphics.draw_string ;
    
      Graphics.moveto x (y+60) ;
      info Speed |>
      Graphics.draw_string ;
    
      Graphics.moveto x (y+70) ;
      pretty_float p.buffc |>
      concat "Buff Chance: " |>
      Graphics.draw_string ;
    
      let [a1 ; a2 ; a3 ; a4] = p.ats in
    
      Graphics.moveto x (y+80) ;
      attack_to_string a4 |>
      concat "Attack 4: " |>
      Graphics.draw_string ;
  
      Graphics.moveto x (y+90) ;
      attack_to_string a3 |>
      concat "Attack 3: " |>
      Graphics.draw_string ;
    
      Graphics.moveto x (y+100) ;
      attack_to_string a2 |>
      concat "Attack 2: " |>
      Graphics.draw_string ;
    
      Graphics.moveto x (y+110) ;
      attack_to_string a1 |>
      concat "Attack 1: " |>
      Graphics.draw_string ;
    
      Graphics.moveto x (y+130) ;
      pokemon_type_to_string p |>
      concat "Type: " |>
      Graphics.draw_string;
  
 
      let maxX = 1+(x+w)/ !displayW in
      let maxY = 1+(y+h)/ !displayW in
      changeRange i j (if maxX >= !gridW then !gridW - 1 else maxX) (if maxY >= !gridW then !gridW - 1 else maxY)
    end

let rec checkPaused grid changed =
        let status = wait_next_event [Key_pressed ; Button_down ; Button_up] in

  let newChanged = 
      if status.button then
              begin
              let newChanged =
                      drawInfoPane status.mouse_x status.mouse_y grid in
              drawGrid grid changed;
              Graphics.synchronize();
              newChanged
              end
      else
              begin
                      drawGrid grid changed;
              Graphics.synchronize();
                      []
              end in
  
  let unPaused = 
    if status.keypressed then
            if status.key = ' ' then
                    true
            else if status.key = 'q' then
                    exit 0
            else
                    false
    else
      false in
  if not unPaused then
    checkPaused grid newChanged
  else
    newChanged

let rec checkKeyStatus grid paused changed =
  let status = wait_next_event [Key_pressed] in
  let newChanged =
    if status.key = 'q' then 
      exit 0 
    else if status.key = ' ' && not paused then 
            begin
              Graphics.synchronize();
      checkPaused grid changed 
            end
    else
      changed in
  let next = wait_next_event [Poll] in
  if next.keypressed then
    checkKeyStatus grid paused newChanged
  else
    newChanged

let rec checkStatus grid paused status =
  let changed = 
  if status.button then
      drawInfoPane status.mouse_x status.mouse_y grid
  else
    [] in
  if status.keypressed then
    checkKeyStatus grid false changed
  else 
          changed

let allChanged () = changeRange 0 0 !gridW !gridW

let wait_for start time =
  if (Unix.gettimeofday()) -. start < time then
    wait_for start time
  else
    ()

let short_wait f =
  let t = Unix.gettimeofday() in
  let rec waiting ()=
    if (Unix.gettimeofday())-.t < f then
      waiting()
    else () in
  waiting()

let rec loadUpdate saveFile i=
        if i >0 then
                let change = bscanf saveFile "%d,%d,%d;" (fun x y id -> (x,y,id)) in
        change :: (loadUpdate saveFile (i-1))
        else
                begin
                        try
                                bscanf saveFile "%c" (fun x -> x);
                                []
                        with End_of_file -> []
                end

let loadUpdates saveFile =
  let n = bscanf saveFile "%d " (fun x -> x) in
  loadUpdate saveFile n

let rec loadGridBattle grid table saveFile =
  try
  let currentTime = Unix.gettimeofday() in
  let updates = loadUpdates saveFile in
  tailMap (fun (x,y,id) -> update (x,y) (Hashtbl.find table id) grid) updates;
  let drawn_over =
    wait_next_event [Poll] |>
    checkStatus grid false in
  drawGrid grid (drawn_over @ (tailMap (fun (x,y,_) -> (x,y)) updates));
  wait_for currentTime 0.01;
  Graphics.synchronize();
  loadGridBattle grid table saveFile
  with End_of_file -> 
    if gridDone grid then
            drawInfoPane 0 0 grid
    else
            begin
                    Graphics.movteto 0 0;
                    Graphics.set_color Graphics.black;
                    Graphics.draw_string "Done";
            end

let rec gridBattle grid ?saveFile updates =
  if !disableRef then
    drawGrid grid (allChaanged())
  else
    drawGrid grid updates;
  let drawn_over =
    wait_next_event [Poll] |>
    checkStatus grid false in
  Graphics.synchronize();
  (*Unix.sleep 1;*)
  if gridDone grid then
    begin
      drawInfoPane 0 0 grid;
  Graphics.synchronize();
      grid
    end
  else
    begin
      gridTurn grid ?saveFile:saveFile () |>
      (@) drawn_over |>
      gridBattle ?saveFile:saveFile grid
    end

let rec interactive () =
  let event = wait_next_event [Key_pressed] in
  if event.key == 'q' then exit 0
  else if event.key == 'r' then 
    begin
    let grid = initArray () in
    gridBattle grid (allChanged ());
    interactive()
    end
  else print_char event.key; print_newline (); interactive ()

let set_args () =
  Arg.parse [
    ("-b", Set buffRef, "Enables pokemon to use buff attacks") ;
    ("-disable", Set disableRef, "Enables graphic enabled UI") ;
    ("-w", Set_int gridW, "Sets the width of the pokemon grid") ;
    ("-d", Set_int displayW, "Sets the display width of each pokemon") ;
    ("-load", Set loadRef, "Instructs program to load from file") ;
    ("-s", Set_string saveNameRef, "Save output to file")]
    (printf "Did not recognize argument %s\n")
    ("Please pass in arguments to dictate behavior of simulation");;

let init_savefile saveFile =
  fprintf saveFile "%s\n" (String.concat "," [string_of_int !gridW ; string_of_int !displayW ; string_of_bool !buffRef])

let init_load saveFile =
  bscanf saveFile "%d,%d,%B\n" (fun w d b ->
    gridW := w;
    displayW := d;
    buffRef := b
  )

let () =
  set_args();
  let gw = !displayW * !gridW in
  let gh = gw + 40 in
  String.concat "" [string_of_int gw ; "x" ; string_of_int gh] |>
  Graphics.open_graph ;
  Graphics.auto_synchronize false;
  if !loadRef then
    let loadFile = Scanning.open_in !saveNameRef in
    init_load loadFile;
    let (grid,table) = loadTbl loadFile in
    drawGrid grid (allChanged());
    Graphics.synchronize();
    loadGridBattle grid table loadFile
  else 
          begin
    let saveFile =
      if !saveNameRef = "" then
        None
      else
        Some (open_out !saveNameRef) in
    map init_savefile saveFile;
    let grid = initArray ?saveFile:saveFile () in
    gridBattle grid ?saveFile:saveFile (allChanged()) ;
    map close_out saveFile |>
    ignore
  end;
  interactive()
