open Tk

module Utils = struct
  let range n =
    let rec _range i lst =
      if i = n
        then lst
        else i::(_range (i+1) lst)
    in _range 0 []
  let shuffle d =
    Random.self_init ();
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond
  let replace l pos a =
    List.mapi (fun i x -> if i = pos then a else x) l;;
  let random_pair () =
    Random.int 6 + 1, Random.int 6 + 1
  let rec find_idx elem lst =
    match lst with
    | [] -> raise (Failure "Not Found")
    | h :: t -> if elem = h then 0 else 1 + find_idx elem t
end

module Catan = struct
  (* TYPY *)
  type field_index = int (* Typ pomocniczy, żeby było mi się łatwiej ogarniać w kodzie *)
  type color =
    | Red
    | Green
    | Blue
  type resources =
    | RWood  of int
    | RClay  of int
    | RStone of int
    | RWheat of int
    | RSheep of int
  type field =
    (* Przechowuje typ pola, numer do niego przypisany oraz indeks na planszy *)
    | Wood  of int * field_index
    | Clay  of int * field_index
    | Stone of int * field_index
    | Wheat of int * field_index
    | Sheep of int * field_index
    | Desert
  type settlement =
    (*
     * Jeżeli dane pole jest wsią/miastem to przechowuje
     *  listę przyległych pól oraz kolor właściciela.
     * Dla pustych miejc na zabudowania pamiętamy tylko przyległe pola.
     * Dla zablokowanych pól nie trzeba nic pamiętać, nie można tam budować
     *)
    | Village of field list * color
    | Town    of field list * color
    | Empty   of field list
    | Blocked
  type road =
    | Road of color
    | NoRoad
  type player_resources = {
    (* Przechowuje liczbę każdego z surowców *)
    mutable wood  : int;
    mutable clay  : int;
    mutable stone : int;
    mutable wheat : int;
    mutable sheep : int;
  }
  type player = {
    (* Informacje o graczu *)
    mutable color       : color;
    mutable resources   : player_resources;
    mutable settlements : int list;
    mutable roads       : int list;
    mutable points      : int;
  }
  type t = {
    numberOfPlayers     : int;
    mutable players     : player list;
    mutable board       : field list;
    mutable settlements : settlement list;
    mutable roads       : road list;
    mutable tour        : color;
    mutable thief      : int;
    mutable dices       : int * int;
  }

  (* TABLICEC I STAŁE *)
  let nof_settlements = 54
  let nof_roads = 72
  let order = [Red; Green; Blue]
  let first_player = List.nth order 0
  let numbers = [5;2;6;3;8;10;9;12;11;4;8;10;9;4;5;6;3;11]
  let fields = Utils.shuffle ['D';'D';'D';'D';'G';'G';'G';'S';'S';'S';'S';'K';'K';'K';'O';'O';'O';'O';'P']
  let sblocks = [|
    "./blocks/sblocks/wood.png";
    "./blocks/sblocks/clay.png";
    "./blocks/sblocks/stone.png";
    "./blocks/sblocks/wheat.png";
    "./blocks/sblocks/sheep.png";
  |]
  let fields_buildplaces = [
    (*  0 *) [0;1;2; 8; 9;10];
    (*  1 *) [2;3;4;10;11;12];
    (*  2 *) [4;5;6;12;13;14];
    (*  3 *) [ 7; 8; 9;17;18;19];
    (*  4 *) [ 9;10;11;19;20;21];
    (*  5 *) [11;12;13;21;22;23];
    (*  6 *) [13;14;15;23;24;25];
    (*  7 *) [16;17;18;27;28;29];
    (*  8 *) [18;19;20;29;30;31];
    (*  9 *) [20;21;22;31;32;33];
    (* 10 *) [22;23;24;33;34;35];
    (* 11 *) [24;25;26;35;36;37];
    (* 12 *) [28;29;30;38;39;40];
    (* 13 *) [30;31;32;40;41;42];
    (* 14 *) [32;33;34;42;43;44];
    (* 15 *) [34;35;36;44;45;46];
    (* 16 *) [39;40;41;47;48;49];
    (* 17 *) [41;42;43;49;50;51];
    (* 18 *) [43;44;45;51;52;53];
  ]
  let blocking = [
    (*  0 *) [1;8];
    (*  1 *) [0;2];
    (*  2 *) [1;3;10];
    (*  3 *) [2;4];
    (*  4 *) [3;5;12];
    (*  5 *) [4;5];
    (*  6 *) [5;14];

    (*  7 *) [8;17];
    (*  8 *) [0;7;9];
    (*  9 *) [8;10;19];
    (* 10 *) [2;9;11];
    (* 11 *) [10;12;21];
    (* 12 *) [4;11;13];
    (* 13 *) [12;14;23];
    (* 14 *) [6;13;15];
    (* 15 *) [14;25];

    (* 16 *) [17;27];
    (* 17 *) [7;16;18];
    (* 18 *) [17;19;29];
    (* 19 *) [9;18;20];
    (* 20 *) [19;21;31];
    (* 21 *) [11;20;22];
    (* 22 *) [21;23;33];
    (* 23 *) [13;22;24];
    (* 24 *) [23;25;35];
    (* 25 *) [15;24;26];
    (* 26 *) [25;37];

    (* 27 *) [16;28];
    (* 28 *) [27;29;38];
    (* 29 *) [18;28;30];
    (* 30 *) [29;31;40];
    (* 31 *) [20;30;32];
    (* 32 *) [31;33;42];
    (* 33 *) [22;32;34];
    (* 34 *) [33;35;44];
    (* 35 *) [24;34;36];
    (* 36 *) [35;37;46];
    (* 37 *) [26;36];

    (* 38 *) [28;39];
    (* 39 *) [38;40;47];
    (* 40 *) [30;39;41];
    (* 41 *) [40;42;49];
    (* 42 *) [32;41;43];
    (* 43 *) [42;44;51];
    (* 44 *) [34;43;45];
    (* 45 *) [44;46;53];
    (* 46 *) [36;45];

    (* 47 *) [39;48];
    (* 48 *) [47;49];
    (* 49 *) [41;48;50];
    (* 50 *) [49;51];
    (* 51 *) [43;50;52];
    (* 52 *) [51;53];
    (* 53 *) [45;52];
  ]
  let roads = [
    0,1;   1,2;   2,3;   3,4;   4,5;   5,6;
    0,8;   2,10;  4,12;  6,14;
    7,8;   8,9;   9,10;  10,11; 11,12; 12,13; 13,14; 14,15;
    7,17;  9,19;  11,21; 13,23; 15,25;
    16,17; 17,18; 18,19; 19,20; 20,21; 21,22; 22,23; 23,24; 24,25; 25,26;
    16,27; 18,29; 20,31; 22,33; 24,35; 26,37;
    27,28; 28,29; 29,30; 30,31; 31,32; 32,33; 33,34; 34,35; 35,36; 36,37;
    28,38; 30,40; 32,42; 34,44; 36,46;
    38,39; 39,40; 40,41; 41,42; 42,43; 43,44; 44,45; 45,46;
    39,47; 41,49; 43,51; 45,53;
    47,48; 48,49; 49,50; 50,51; 51,52; 52,53;
  ]

  (* FUNKCJE *)
  let color2number c =
    Utils.find_idx c order
  let resource2str f =
    match f with
    | RWood  _ -> "wood"
    | RClay  _ -> "clay"
    | RStone _ -> "stone"
    | RWheat _ -> "wheat"
    | RSheep _ -> "sheep"
  let color2str c =
    match c with
    | Red   -> "red"
    | Green -> "green"
    | Blue  -> "blue"

  let get_desert_idx board =
    (*
     * Zwraca indeks pustyni na planszy
     *)
    let rec _aux n board =
      match List.nth board 0 with
        | Desert -> n
        | _ -> _aux (n+1) @@ List.tl board
    in _aux 0 board
  let check_resources fl n m =
    (*
     * Otrzymuje listę pól przyległych do danej osady, wynik kości i pozycję złodzieja.
     * Zwraca listę surowców, które w danej turze zbiera ta osada
     *)
    print_string @@ "Pozycja thiefa: " ^ Int.to_string m ^ "\n";
    List.filter_map (fun f ->
      match f with
      | Wood  (i, fi) -> print_string @@ "Indeks pola: " ^ Int.to_string fi ^ "\n"; if (i = n && fi != m) then Some (RWood  1) else None
      | Clay  (i, fi) -> print_string @@ "Indeks pola: " ^ Int.to_string fi ^ "\n"; if (i = n && fi != m) then Some (RClay  1) else None
      | Stone (i, fi) -> print_string @@ "Indeks pola: " ^ Int.to_string fi ^ "\n"; if (i = n && fi != m) then Some (RStone 1) else None
      | Wheat (i, fi) -> print_string @@ "Indeks pola: " ^ Int.to_string fi ^ "\n"; if (i = n && fi != m) then Some (RWheat 1) else None
      | Sheep (i, fi) -> print_string @@ "Indeks pola: " ^ Int.to_string fi ^ "\n"; if (i = n && fi != m) then Some (RSheep 1) else None
      | Desert  -> None
    ) fl
  let update_resources gameState player resource =
    (*
     * Dodaje do surowców podanego gracza jedną sztukę `resource`
     *)
    let p = List.nth gameState.players @@ color2number player in
    let r = p.resources in
    match resource with
      | RWood  i -> r.wood <- r.wood+1
      | RClay  i -> r.clay <- r.clay+1
      | RStone i -> r.stone <- r.stone+1
      | RWheat i -> r.wheat <- r.wheat+1
      | RSheep i -> r.sheep <- r.sheep+1
  let can_afford_road gameState =
    (*
     * Sprawdza, czy aktualnego gracza stać na budowę drogi
     * drewno, glina
     *)
    let player = List.nth gameState.players @@ color2number gameState.tour in
    let rs = player.resources in
    if List.for_all (fun b -> b) [rs.wood > 0; rs.clay > 0]
      then true
      else false
  let can_afford_village gameState =
    (*
     * Sprawdza, czy aktualnego gracza stać na budowę wsi
     * drewno, glina, sianko, owca
     *)
    let player = List.nth gameState.players @@ color2number gameState.tour in
    let rs = player.resources in
    if List.for_all (fun b -> b) [rs.wood > 0; rs.clay > 0; rs.sheep > 0; rs.wheat > 0]
      then true
      else false
  let can_afford_town gameState =
    (*
     * Sprawdza, czy aktualnego gracza stać na budowę miasta
     * 3x kamień, 2x sianko
     *)
    let player = List.nth gameState.players @@ color2number gameState.tour in
    let rs = player.resources in
    if List.for_all (fun b -> b) [rs.wheat >= 2; rs.stone >= 3]
      then true
      else false
  let is_valid_trade_offer gameState offer claim trader =
    (*
     * Sprawdza czy podana oferta wymiany jest prawidłowa,
     * to znaczy czy obydwie strony mają odpowiednią ilość surowców.
     * W wymianie obydwie strony muszą oferować przynajmniej jeden surowiec.
     * Jeżeli trader = -1, to drugą stroną jest bank
     *)
    if trader = (-1)
      then begin
        if 4*(Array.fold_left (+) 0 claim) = (Array.fold_left (+) 0 offer)
          then true
          else false
      end
      else begin
        let player = List.nth gameState.players @@ color2number gameState.tour in
        let rs = player.resources in
        let player2 = List.nth gameState.players trader in
        let rs2 = player2.resources in
        if List.mem false
            [rs.wood >= offer.(0); rs.clay >= offer.(1); rs.stone >= offer.(2); rs.wheat >= offer.(3); rs.sheep >= offer.(4);
            rs2.wood >= claim.(0); rs2.clay >= claim.(1); rs2.stone >= claim.(2); rs2.wheat >= claim.(3); rs2.sheep >= claim.(4)]
          then false
          else List.mem true [offer.(0) > 0; offer.(1) > 0; offer.(2) > 0; offer.(3) > 0; offer.(4) > 0]
              && List.mem true [claim.(0) > 0; claim.(1) > 0; claim.(2) > 0; claim.(3) > 0; claim.(4) > 0]
      end
  let is_valid_road_position gameState position =
    (*
     * Sprawdza czy aktualny gracz może legalnie wybudować miasto na podanej pozycji
     * (miasto można wybudować jedynie na swojej wsi).
     * Zwraca true jeżeli budowa jest możliwa, wpp. false
     *)
    let v1, v2 = List.nth roads position in
    let player = List.nth gameState.players @@ color2number gameState.tour in
    let buildings = player.settlements in
    let pl_roads = player.roads in
    let p = [List.mem v1 buildings; List.mem v2 buildings] in
    if List.mem true p
      then true
      else
        List.mem true @@
        List.map (fun r ->
          let w1, w2 = List.nth roads r in
          List.mem true [v1 = w1; v1 = w2; v2 = w1; v2 = w2]
        ) pl_roads
  let is_valid_village_position gameState position =
    (*
     * Sprawdza czy aktualny gracz może legalnie wybudować wieś na podanej pozycji
     * (czy jest tam doprowadzona jego droga).
     * Zwraca true jeżeli budowa jest możliwa, wpp. false
     *)
    let player = List.nth gameState.players @@ color2number gameState.tour in
    let plr = player.roads in
    let lst = List.map (fun i ->
      let n1, n2 = List.nth roads i in
      if position = n1 || position = n2
        then true
        else false
    ) plr
    in List.mem true lst

  (* FUNKCJE GŁOWNE *)
  let rec steal_from gameState c =
    (*
     * Losuje jeden surowiec, zabiera go graczowi `c` i daje aktualnegu graczowi.
     * Jeżeli gracz `c` nie ma surowców, nic się nie dzieje.
     *)
    let player = List.nth gameState.players @@ color2number gameState.tour in
    let rs = player.resources in
    let victim = List.nth gameState.players @@ color2number c in
    let vrs = victim.resources in
    let rand = Random.int 5 in
    if List.mem true [vrs.wood > 0; vrs.clay > 0; vrs.stone > 0; vrs.wheat > 0; vrs.sheep > 0]
      then
    match rand with
    | 0 -> if vrs.wood > 0
            then begin
              victim.resources <- {wood = vrs.wood-1; clay = vrs.clay; stone = vrs.stone; wheat = vrs.wheat; sheep = vrs.sheep};
              player.resources <- {wood = rs.wood+1; clay = rs.clay; stone = rs.stone; wheat = rs.wheat; sheep = rs.sheep}
            end
            else begin steal_from gameState c end
    | 1 -> if vrs.clay > 0
            then begin
              victim.resources <- {wood = vrs.wood; clay = vrs.clay-1; stone = vrs.stone; wheat = vrs.wheat; sheep = vrs.sheep};
              player.resources <- {wood = rs.wood; clay = rs.clay+1; stone = rs.stone; wheat = rs.wheat; sheep = rs.sheep}
            end
            else steal_from gameState c
    | 2 -> if vrs.stone > 0
            then begin
              victim.resources <- {wood = vrs.wood; clay = vrs.clay; stone = vrs.stone-1; wheat = vrs.wheat; sheep = vrs.sheep};
              player.resources <- {wood = rs.wood; clay = rs.clay; stone = rs.stone+1; wheat = rs.wheat; sheep = rs.sheep}
            end
            else steal_from gameState c
    | 3 -> if vrs.wheat > 0
            then begin
              victim.resources <- {wood = vrs.wood; clay = vrs.clay; stone = vrs.stone; wheat = vrs.wheat-1; sheep = vrs.sheep};
              player.resources <- {wood = rs.wood; clay = rs.clay; stone = rs.stone; wheat = rs.wheat+1; sheep = rs.sheep}
            end
            else steal_from gameState c
    | 4 -> if vrs.sheep > 0
            then begin
              victim.resources <- {wood = vrs.wood; clay = vrs.clay; stone = vrs.stone; wheat = vrs.wheat; sheep = vrs.sheep-1};
              player.resources <- {wood = rs.wood; clay = rs.clay; stone = rs.stone; wheat = rs.wheat; sheep = rs.sheep+1}
            end
            else steal_from gameState c
    | _ -> failwith "out of range [err 15]"
  let next_player gameState =
    (*
     * Ustawia turę na kolejnego gracza
     *)
    let current_player = color2number gameState.tour in
    if current_player+1 = List.length order
      then gameState.tour <- first_player
      else gameState.tour <- List.nth order (current_player+1)
  let move_thief gameState position =
    (* Przenosi złodzieja na nową pozycję *)
    print_string @@ "Moving thief to position " ^ Int.to_string position ^ "\n";
    gameState.thief <- position
  let build_village gameState position =
    (*
     * Buduje wioskę aktualnego gracza na zadanej pozycji.
     * Gracz traci odpowiednią ilość surowców.
     *)
    print_string @@ "Player " ^ color2str gameState.tour ^ " builds a village at position " ^ Int.to_string position ^ "\n";
    let place = List.nth gameState.settlements position in
    match place with
      | Empty x ->
          List.iter (fun i ->
            gameState.settlements <- Utils.replace gameState.settlements i Blocked
          ) @@ List.nth blocking position;
          let settlements = gameState.settlements in
          gameState.settlements <- Utils.replace settlements position (Village (x, gameState.tour));
          let player_info = List.nth gameState.players @@ color2number gameState.tour in
          let rs = player_info.resources in
          player_info.settlements <- List.append player_info.settlements [position];
          player_info.resources <- {wood = rs.wood-1; clay = rs.clay-1; wheat = rs.wheat-1; sheep = rs.sheep-1; stone = rs.stone};
          player_info.points <- player_info.points + 1
      | _ -> failwith "Ooops! You can build village only on empty field!\n"
  let build_town gameState position =
    (*
     * Buduje miasto aktualnego gracza na zadanej pozycji.
     * Gracz traci odpowiednią ilość surowców.
     *)
    print_string @@ "Player " ^ color2str gameState.tour ^ " builds a town at position " ^ Int.to_string position ^ "\n";
    let place = List.nth gameState.settlements position in
    match place with
      | Village (x,c) ->
          List.iter (fun i ->
            gameState.settlements <- Utils.replace gameState.settlements i Blocked
          ) @@ List.nth blocking position;
          let settlements = gameState.settlements in
          gameState.settlements <- Utils.replace settlements position (Town (x, gameState.tour));
          let player_info = List.nth gameState.players @@ color2number gameState.tour in
          let rs = player_info.resources in
          player_info.settlements <- List.append player_info.settlements [position];
          player_info.resources <- {wood = rs.wood; clay = rs.clay; wheat = rs.wheat-2; sheep = rs.sheep; stone = rs.stone-3};
          player_info.points <- player_info.points + 1
      | _ -> failwith "Ooops! You can build town only on villages!\n"
  let build_road gameState position =
    (*
     * Buduje drogę aktualnego gracza na zadanej pozycji.
     * Gracz traci odpowiednią ilość surowców.
     *)
    print_string @@ "Player " ^ color2str gameState.tour ^ " builds a road at position " ^ Int.to_string position ^ "\n";
    gameState.roads <- Utils.replace gameState.roads position (Road gameState.tour);
    let player_info = List.nth gameState.players @@ color2number gameState.tour in
    let rs = player_info.resources in
    player_info.roads <- List.append player_info.roads [position];
    player_info.resources <- {wood = rs.wood-1; clay = rs.clay-1; wheat = rs.wheat; sheep = rs.sheep; stone = rs.stone}
  let dice_roll gameState n1 n2 =
    (*
     * Funkcja zbiera surowce dla wszystkich graczy, bazując na wyniku kości.
     * Ta funkcja NIE rozpatruje przypadku, gdy wypadło 7.
     *)
    print_string @@ "Dice roll: " ^ Int.to_string (n1+n2) ^ "\n";
    gameState.dices <- n1, n2;
    List.iter (fun i ->
      let b = List.nth gameState.settlements i
      in match b with
      | Village (fl, c) ->
          List.iter (fun e ->
            print_string @@ "Player " ^ color2str c ^ " gains " ^ resource2str e ^ "\n";
            update_resources gameState c e
          ) @@ check_resources fl (n1+n2) gameState.thief
      | Town (fl, c) ->
          List.iter (fun e ->
            print_string @@ "Player " ^ color2str c ^ " gains 2 " ^ resource2str e ^ "\n";
            update_resources gameState c e;
            update_resources gameState c e
          ) @@ check_resources fl (n1+n2) gameState.thief
      | Empty _ -> ()
      | Blocked -> ()
    ) @@ Utils.range nof_settlements (* gameState.settlements *)
  let trade gameState offer claim trader =
    (*
     * Jeżeli dana oferta wymiany jest legalna,
     * funkcja dokonuje wymiany i zwraca true.
     * W przeciwnym wypadku, funkcja zwraca false i nie robi nic poza tym.
     * Ta funkcja NIE rozpatruje przypadku, gdy jedną ze stron wymiany jest bank.
     *)
    if is_valid_trade_offer gameState offer claim trader
      then begin
        print_string "deal\n";
        let player = List.nth gameState.players @@ color2number gameState.tour in
        let rs = player.resources in
        let player2 = List.nth gameState.players trader in
        let rs2 = player2.resources in
        rs.wood   <- rs.wood   - offer.(0) + claim.(0);
        rs.clay   <- rs.clay   - offer.(1) + claim.(1);
        rs.stone  <- rs.stone  - offer.(2) + claim.(2);
        rs.wheat  <- rs.wheat  - offer.(3) + claim.(3);
        rs.sheep  <- rs.sheep  - offer.(4) + claim.(4);
        rs2.wood  <- rs2.wood  + offer.(0) - claim.(0);
        rs2.clay  <- rs2.clay  + offer.(1) - claim.(1);
        rs2.stone <- rs2.stone + offer.(2) - claim.(2);
        rs2.wheat <- rs2.wheat + offer.(3) - claim.(3);
        rs2.sheep <- rs2.sheep + offer.(4) - claim.(4);
        true
      end else false
  let trade_bank gameState offer claim =
    (*
     * Służy jedynie do wymiany z bankiem.
     * Jeżeli dana oferta wymiany jest legalna,
     * funkcja dokonuje wymiany i zwraca true.
     * W przeciwnym wypadku, funkcja zwraca false i nie robi nic poza tym.
     *)
    if is_valid_trade_offer gameState offer claim (-1)
      then begin
        print_string "deal\n";
        let player = List.nth gameState.players @@ color2number gameState.tour in
        let rs = player.resources in
        rs.wood   <- rs.wood   - offer.(0) + claim.(0);
        rs.clay   <- rs.clay   - offer.(1) + claim.(1);
        rs.stone  <- rs.stone  - offer.(2) + claim.(2);
        rs.wheat  <- rs.wheat  - offer.(3) + claim.(3);
        rs.sheep  <- rs.sheep  - offer.(4) + claim.(4);
        true
      end else false

  (* FUNKCJE INICJALIZUJĄCE *)
  let settlementsInit board =
    (*
     * Ta funcja tworzy startową listę zabudowań.
     * Póki co jest ona złożona jedynie z pustych miejsc.
     * Każde pole do budowy wsi/miasta przechowuje
     * listę pól (z surowcami) do których przylega
     *)
    (*  1 *) List.append [Empty [List.nth board 0]] @@
    (*  2 *) List.append [Empty [List.nth board 0]] @@
    (*  3 *) List.append [Empty [List.nth board 0 ; List.nth board 1]] @@
    (*  4 *) List.append [Empty [List.nth board 1]] @@
    (*  5 *) List.append [Empty [List.nth board 1 ; List.nth board 2]] @@
    (*  6 *) List.append [Empty [List.nth board 2]] @@
    (*  7 *) List.append [Empty [List.nth board 2]] @@

    (*  8 *) List.append [Empty [List.nth board 3]] @@
    (*  9 *) List.append [Empty [List.nth board 0 ; List.nth board 3]] @@
    (* 10 *) List.append [Empty [List.nth board 0 ; List.nth board 3 ; List.nth board 4]] @@
    (* 11 *) List.append [Empty [List.nth board 0 ; List.nth board 1 ; List.nth board 4]] @@
    (* 12 *) List.append [Empty [List.nth board 1 ; List.nth board 4 ; List.nth board 5]] @@
    (* 13 *) List.append [Empty [List.nth board 1 ; List.nth board 2 ; List.nth board 5]] @@
    (* 14 *) List.append [Empty [List.nth board 2 ; List.nth board 5 ; List.nth board 6]] @@
    (* 15 *) List.append [Empty [List.nth board 2 ; List.nth board 6]] @@
    (* 16 *) List.append [Empty [List.nth board 6]] @@

    (* 17 *) List.append [Empty [List.nth board 7]] @@
    (* 18 *) List.append [Empty [List.nth board 3 ; List.nth board 7]] @@
    (* 19 *) List.append [Empty [List.nth board 3 ; List.nth board 7 ; List.nth board 8]] @@
    (* 20 *) List.append [Empty [List.nth board 3 ; List.nth board 4 ; List.nth board 8]] @@
    (* 21 *) List.append [Empty [List.nth board 4 ; List.nth board 8 ; List.nth board 9]] @@
    (* 22 *) List.append [Empty [List.nth board 4 ; List.nth board 5 ; List.nth board 9]] @@
    (* 23 *) List.append [Empty [List.nth board 5 ; List.nth board 9 ; List.nth board 10]] @@
    (* 24 *) List.append [Empty [List.nth board 5 ; List.nth board 6 ; List.nth board 10]] @@
    (* 25 *) List.append [Empty [List.nth board 6 ; List.nth board 10 ; List.nth board 11]] @@
    (* 26 *) List.append [Empty [List.nth board 6 ; List.nth board 11]] @@
    (* 27 *) List.append [Empty [List.nth board 11]] @@

    (* 28 *) List.append [Empty [List.nth board 7]] @@
    (* 29 *) List.append [Empty [List.nth board 7 ; List.nth board 12]] @@
    (* 30 *) List.append [Empty [List.nth board 7 ; List.nth board 8; List.nth board 12]] @@
    (* 31 *) List.append [Empty [List.nth board 8 ; List.nth board 12 ; List.nth board 13]] @@
    (* 32 *) List.append [Empty [List.nth board 8 ; List.nth board 9 ; List.nth board 13]] @@
    (* 33 *) List.append [Empty [List.nth board 9 ; List.nth board 13 ; List.nth board 14]] @@
    (* 34 *) List.append [Empty [List.nth board 9 ; List.nth board 10 ; List.nth board 14]] @@
    (* 35 *) List.append [Empty [List.nth board 10 ; List.nth board 14 ; List.nth board 15]] @@
    (* 36 *) List.append [Empty [List.nth board 10 ; List.nth board 11 ; List.nth board 15]] @@
    (* 37 *) List.append [Empty [List.nth board 11 ; List.nth board 15]] @@
    (* 38 *) List.append [Empty [List.nth board 11]] @@

    (* 39 *) List.append [Empty [List.nth board 12]] @@
    (* 40 *) List.append [Empty [List.nth board 12 ; List.nth board 16]] @@
    (* 41 *) List.append [Empty [List.nth board 12 ; List.nth board 13 ; List.nth board 16]] @@
    (* 42 *) List.append [Empty [List.nth board 13 ; List.nth board 16 ; List.nth board 17]] @@
    (* 43 *) List.append [Empty [List.nth board 13 ; List.nth board 14 ; List.nth board 17]] @@
    (* 44 *) List.append [Empty [List.nth board 14 ; List.nth board 17 ; List.nth board 18]] @@
    (* 45 *) List.append [Empty [List.nth board 14 ; List.nth board 15 ; List.nth board 18]] @@
    (* 46 *) List.append [Empty [List.nth board 15 ; List.nth board 18]] @@
    (* 47 *) List.append [Empty [List.nth board 15]] @@

    (* 48 *) List.append [Empty [List.nth board 16]] @@
    (* 49 *) List.append [Empty [List.nth board 16]] @@
    (* 50 *) List.append [Empty [List.nth board 16 ; List.nth board 17]] @@
    (* 51 *) List.append [Empty [List.nth board 17]] @@
    (* 52 *) List.append [Empty [List.nth board 17 ; List.nth board 18]] @@
    (* 53 *) List.append [Empty [List.nth board 18]] @@
    (* 54 *) List.append [Empty [List.nth board 18]] []
  let boardInit () =
    (*
     * Losuje i zwraa planszę
     * (z zachowaniem zasad przyjętych w grze Catan, nie całkiem losowo)
     *)
    let rec _generate_board n f out i =
      match n, f with
        | nhd::ntl, fhd::ftl ->
            begin match fhd with
              | 'P' -> _generate_board n ftl (Desert::out) (i-1)
              | 'D' -> _generate_board ntl ftl ((Wood (nhd,i))::out) (i-1)
              | 'G' -> _generate_board ntl ftl ((Clay (nhd,i))::out) (i-1)
              | 'S' -> _generate_board ntl ftl ((Wheat (nhd,i))::out) (i-1)
              | 'K' -> _generate_board ntl ftl ((Stone (nhd,i))::out) (i-1)
              | 'O' -> _generate_board ntl ftl ((Sheep (nhd,i))::out) (i-1)
              | _ -> failwith "Ooops, no such field!\n"
            end
        | [], fhd::ftl -> Desert::out
        | [], [] -> out
        | _ -> failwith "Ooops, shouldn't get here!\n"
      in _generate_board numbers fields [] 18
  let playerInit n =
    (*
     * Zwraca startową strukturę dla gracza o indeksie n
     *)
    {
      settlements = [];
      resources   = {wood = 4; clay = 4; stone = 0; wheat = 2; sheep = 2};
      points      = 0;
      roads       = [];
      color       = List.nth order n;
    }
  let gameInit numberOfPlayers =
    (*
     * Inicjalizuje strukturę przechowującą cały stan gry
     * dla zadanej liczby graczy
     *)
    let board = boardInit () in {
      numberOfPlayers    = numberOfPlayers;
      board   = board;
      settlements  = settlementsInit board;
      roads   = List.map (fun _ -> NoRoad) @@ Utils.range nof_roads;
      thief  = get_desert_idx board;
      dices   = 6, 6;
      tour    = first_player;
      players = List.map (fun n -> playerInit n) @@ Utils.range numberOfPlayers;
    }

end

module Textures = struct
  (* TYPY *)
  type player_info = {
    mutable background     : tagOrId;
    mutable avatar         : tagOrId;
    mutable victory_points : tagOrId * tagOrId;
    mutable roads          : tagOrId * tagOrId;
    mutable resources      : (tagOrId * tagOrId) list;
  }
  type t = {
    mutable background  : tagOrId;
    mutable thief      : tagOrId;
    mutable fields      : tagOrId list;
    mutable numbers     : tagOrId list;
    mutable settlements : tagOrId list;
    mutable roads       : tagOrId list;
    mutable dices       : tagOrId * tagOrId;
    mutable non_active  : tagOrId list;
    mutable info        : tagOrId list;
    mutable players     : player_info list;
  }

  (* TABLICE I STAŁE *)
  let resolution = (1920, 1080)
  let field_size = (173, 200)
  let xoffset = fst resolution / 2
  let yoffset = snd resolution / 2
  let field_coords = [
    (xoffset + 90*2 - 180*2), (yoffset - 155*2); (* field 1 *)
    (xoffset + 90*2 - 180*1), (yoffset - 155*2); (* field 2 *)
    (xoffset + 90*2 - 180*0), (yoffset - 155*2); (* field 3 *)

    (xoffset + 90*1 - 180*2), (yoffset - 155*1); (* field 4 *)
    (xoffset + 90*1 - 180*1), (yoffset - 155*1); (* field 5 *)
    (xoffset + 90*1 - 180*0), (yoffset - 155*1); (* field 6 *)
    (xoffset + 90*1 + 180*1), (yoffset - 155*1); (* field 7 *)

    (xoffset + 90*0 - 180*2), (yoffset - 155*0); (* field 8 *)
    (xoffset + 90*0 - 180*1), (yoffset - 155*0); (* field 9 *)
    (xoffset + 90*0 - 180*0), (yoffset - 155*0); (* field 10 *)
    (xoffset + 90*0 + 180*1), (yoffset - 155*0); (* field 11 *)
    (xoffset + 90*0 + 180*2), (yoffset - 155*0); (* field 12 *)

    (xoffset + 90*1 - 180*2), (yoffset + 155*1); (* field 13 *)
    (xoffset + 90*1 - 180*1), (yoffset + 155*1); (* field 14 *)
    (xoffset + 90*1 - 180*0), (yoffset + 155*1); (* field 15 *)
    (xoffset + 90*1 + 180*1), (yoffset + 155*1); (* field 16 *)

    (xoffset + 90*2 - 180*2), (yoffset + 155*2); (* field 17 *)
    (xoffset + 90*2 - 180*1), (yoffset + 155*2); (* field 18 *)
    (xoffset + 90*2 - 180*0), (yoffset + 155*2); (* field 19 *)
  ]
  let settlements_coords = [
    (xoffset + 90*2 - 180*2 - 90), (yoffset - 155*2 - 55);
    (xoffset + 90*2 - 180*2),      (yoffset - 155*2 - 100);
    (xoffset + 90*2 - 180*2 + 90), (yoffset - 155*2 - 55);
    (xoffset + 90*2 - 180*1),      (yoffset - 155*2 - 100);
    (xoffset + 90*2 - 180*1 + 90), (yoffset - 155*2 - 55);
    (xoffset + 90*2 - 180*0),      (yoffset - 155*2 - 100);
    (xoffset + 90*2 - 180*0 + 90), (yoffset - 155*2 - 55);

    (xoffset + 90*1 - 180*2 - 90), (yoffset - 155*1 - 55);
    (xoffset + 90*1 - 180*2),      (yoffset - 155*1 - 100);
    (xoffset + 90*1 - 180*2 + 90), (yoffset - 155*1 - 55);
    (xoffset + 90*1 - 180*1),      (yoffset - 155*1 - 100);
    (xoffset + 90*1 - 180*1 + 90), (yoffset - 155*1 - 55);
    (xoffset + 90*1 - 180*0),      (yoffset - 155*1 - 100);
    (xoffset + 90*1 - 180*0 + 90), (yoffset - 155*1 - 55);
    (xoffset + 90*1 + 180*1),      (yoffset - 155*1 - 100);
    (xoffset + 90*1 + 180*1 + 90), (yoffset - 155*1 - 55);

    (xoffset + 90*1 - 180*3),      (yoffset - 155*1 + 100);
    (xoffset + 90*1 - 180*2 - 90), (yoffset - 155*1 + 55);
    (xoffset + 90*1 - 180*2),      (yoffset - 155*1 + 100);
    (xoffset + 90*1 - 180*2 + 90), (yoffset - 155*1 + 55);
    (xoffset + 90*1 - 180*1),      (yoffset - 155*1 + 100);
    (xoffset + 90*1 - 180*1 + 90), (yoffset - 155*1 + 55);
    (xoffset + 90*1 - 180*0),      (yoffset - 155*1 + 100);
    (xoffset + 90*1 - 180*0 + 90), (yoffset - 155*1 + 55);
    (xoffset + 90*1 + 180*1),      (yoffset - 155*1 + 100);
    (xoffset + 90*1 + 180*1 + 90), (yoffset - 155*1 + 55);
    (xoffset + 90*1 + 180*2),      (yoffset - 155*1 + 100);

    (xoffset + 90*1 - 180*3),      (yoffset + 155*1 - 100);
    (xoffset + 90*1 - 180*2 - 90), (yoffset + 155*1 - 55);
    (xoffset + 90*1 - 180*2),      (yoffset + 155*1 - 100);
    (xoffset + 90*1 - 180*2 + 90), (yoffset + 155*1 - 55);
    (xoffset + 90*1 - 180*1),      (yoffset + 155*1 - 100);
    (xoffset + 90*1 - 180*1 + 90), (yoffset + 155*1 - 55);
    (xoffset + 90*1 - 180*0),      (yoffset + 155*1 - 100);
    (xoffset + 90*1 - 180*0 + 90), (yoffset + 155*1 - 55);
    (xoffset + 90*1 + 180*1),      (yoffset + 155*1 - 100);
    (xoffset + 90*1 + 180*1 + 90), (yoffset + 155*1 - 55);
    (xoffset + 90*1 + 180*2),      (yoffset + 155*1 - 100);

    (xoffset + 90*1 - 180*2 - 90), (yoffset + 155*1 + 55);
    (xoffset + 90*1 - 180*2),      (yoffset + 155*1 + 100);
    (xoffset + 90*1 - 180*2 + 90), (yoffset + 155*1 + 55);
    (xoffset + 90*1 - 180*1),      (yoffset + 155*1 + 100);
    (xoffset + 90*1 - 180*1 + 90), (yoffset + 155*1 + 55);
    (xoffset + 90*1 - 180*0),      (yoffset + 155*1 + 100);
    (xoffset + 90*1 - 180*0 + 90), (yoffset + 155*1 + 55);
    (xoffset + 90*1 + 180*1),      (yoffset + 155*1 + 100);
    (xoffset + 90*1 + 180*1 + 90), (yoffset + 155*1 + 55);

    (xoffset + 90*2 - 180*2 - 90), (yoffset + 155*2 + 55);
    (xoffset + 90*2 - 180*2),      (yoffset + 155*2 + 100);
    (xoffset + 90*2 - 180*2 + 90), (yoffset + 155*2 + 55);
    (xoffset + 90*2 - 180*1),      (yoffset + 155*2 + 100);
    (xoffset + 90*2 - 180*1 + 90), (yoffset + 155*2 + 55);
    (xoffset + 90*2 - 180*0),      (yoffset + 155*2 + 100);
    (xoffset + 90*2 - 180*0 + 90), (yoffset + 155*2 + 55);
  ]

  (* FUNKCJE *)
  let draw_image screen img_path x y =
    let img_canvas = Canvas.create_image screen ~x:x ~y:y ~anchor:`Center in
    let img = Imagephoto.create () in
    Imagephoto.configure img ~file:img_path;
    Canvas.configure_image screen img_canvas ~image:img;
    img_canvas
  let draw_background screen =
    draw_image screen "./imgs/wood.png" (fst resolution / 2) (snd resolution / 2)
  let draw_board screen board =
    let field2img f =
      match f with
      | Catan.Wood _  -> "./blocks/wood.png"
      | Catan.Clay _  -> "./blocks/clay.png"
      | Catan.Stone _ -> "./blocks/stone.png"
      | Catan.Sheep _ -> "./blocks/sheep.png"
      | Catan.Wheat _ -> "./blocks/wheat.png"
      | Catan.Desert  -> "./blocks/desert.png"

    in let rec draw n lst =
      if n >= 0
      then draw (n-1) @@ draw_image screen (field2img @@ List.nth board n) (fst @@ List.nth field_coords n) (snd @@ List.nth field_coords n) ::lst
      else lst
    in draw 18 []
  let draw_numbers screen board =
    let rec draw n lst =
      if n >= 0
      then begin match List.nth board n with
        | Catan.Wood (i, _)
        | Catan.Clay (i, _)
        | Catan.Stone (i, _)
        | Catan.Wheat (i, _)
        | Catan.Sheep (i, _) -> draw (n-1) @@ draw_image screen ("./numbers/" ^ Int.to_string i ^ ".png") (fst @@ List.nth field_coords n) (snd @@ List.nth field_coords n) ::lst
        | Catan.Desert  -> draw (n-1) lst
      end
      else lst
    in draw 18 []
  let draw_building screen buildings =
    List.map2 (fun (x, y) b ->
      match b with
      | Catan.Village (_, c) -> draw_image screen ("./buildings/player" ^ Int.to_string (Catan.color2number c) ^ "village.png") x y
      | Catan.Town    (_, c) -> draw_image screen ("./buildings/player" ^ Int.to_string (Catan.color2number c) ^ "town.png") x y
      | Catan.Empty   _ -> draw_image screen "./buildings/none.png" x y
      | Catan.Blocked   -> draw_image screen "./buildings/none.png" x y
    ) settlements_coords buildings
  let draw_roads screen roads =
    let objLst = List.map (fun n ->
      let n1, n2 = List.nth Catan.roads n in
      let x1, y1 = List.nth settlements_coords n1 in
      let x2, y2 = List.nth settlements_coords n2 in
      match List.nth roads n with
        | Catan.Road c ->
            let c = Int.to_string @@ Catan.color2number c in
            let road_img =
              if x1 = x2
                then "./buildings/player"^c^"roadv.png"
                else if y1 > y2 then "./buildings/player"^c^"roadr.png" else"./buildings/player"^c^"roadl.png"
              in draw_image screen road_img ((x1+x2)/2) ((y1+y2)/2)
        | Catan.NoRoad -> draw_image screen "./buildings/none.png" ((x1+x2)/2) ((y1+y2)/2)
      ) @@ Utils.range Catan.nof_roads
    in objLst
  let draw_thief screen n =
    draw_image screen "./blocks/zlodziej.png" (fst @@ List.nth field_coords n) (snd @@ List.nth field_coords n)
  let draw_dices screen (n1, n2) =
    let d1 = draw_image screen ("./dice/wd" ^ Int.to_string n1 ^ ".png") 100 100 in
    let d2 = draw_image screen ("./dice/wd" ^ Int.to_string n2 ^ ".png") 250 100 in
    d1, d2
  let draw_player screen tour player =
    let n = Catan.color2number player.Catan.color + 1 in
    let back   = if tour = player.color
                    then draw_image screen "./imgs/current_player.png" (1920-210) (135 + 260*n - 260)
                    else draw_image screen "./imgs/player_background.png" (1920-210) (135 + 260*n - 260)
    in
    let avatar = draw_image screen ("./players/player" ^ (Int.to_string @@ n-1) ^ ".png") (1920-135) (135 + 260*n - 260) in
    let rs = player.Catan.resources in
    let new_rs = [
      (let img = draw_image screen "./blocks/sblocks/wood.png" (1920-265-00) (135 + 260*n - 260 - 70) in
       let t = Canvas.create_text ~x:(1920-265-00) ~y:(135 + 260*n - 260 - 66) ~font:"Helvetica 28 bold" ~text:(Int.to_string rs.Catan.wood) screen in
       Canvas.configure_text screen t;
       img, t);
      (let img = draw_image screen "./blocks/sblocks/clay.png"  (1920-265-20) (135 + 260*n - 260 - 35) in
       let t = Canvas.create_text ~x:(1920-265-20) ~y:(135 + 260*n - 260 - 31) ~font:"Helvetica 28 bold" ~text:(Int.to_string rs.Catan.clay) screen in
       Canvas.configure_text screen t;
       img, t);
      (let img = draw_image screen "./blocks/sblocks/stone.png"  (1920-265-00) (135 + 260*n - 260) in
       let t = Canvas.create_text ~x:(1920-265-00) ~y:(135 + 260*n - 260 + 4) ~font:"Helvetica 28 bold" ~text:(Int.to_string rs.Catan.stone) screen in
       Canvas.configure_text screen t;
       img, t);
      (let img = draw_image screen "./blocks/sblocks/wheat.png"  (1920-265-20) (135 + 260*n - 260 + 35) in
       let t = Canvas.create_text ~x:(1920-265-20) ~y:(135 + 260*n - 260 + 39) ~font:"Helvetica 28 bold" ~text:(Int.to_string rs.Catan.wheat) screen in
       Canvas.configure_text screen t;
       img, t);
      (let img = draw_image screen "./blocks/sblocks/sheep.png"  (1920-265-00) (135 + 260*n - 260 + 70) in
       let t = Canvas.create_text ~x:(1920-265-00) ~y:(135 + 260*n - 260 + 74) ~font:"Helvetica 28 bold" ~text:(Int.to_string rs.Catan.sheep) screen in
       Canvas.configure_text screen t;
       img, t)
    ] in
    let victory_points =
      (let img = draw_image screen ("./players/player" ^ (Int.to_string @@ Catan.color2number player.Catan.color) ^ "c.png")  (1920-350) (135 + 260*n - 260 - 40) in
       let t = Canvas.create_text ~x:(1920-350) ~y:(135 + 260*n - 260 - 36) ~font:"Helvetica 28 bold" ~text:(Int.to_string @@ player.points) screen in
       Canvas.configure_text screen t;
       img, t)
    in let roads =
      (let img = draw_image screen ("./players/player" ^ (Int.to_string @@ Catan.color2number player.Catan.color) ^ "c.png")  (1920-350) (135 + 260*n - 260 + 40) in
        let t = Canvas.create_text ~x:(1920-350) ~y:(135 + 260*n - 260 + 44) ~font:"Helvetica 28 bold" ~text:(Int.to_string @@ List.length player.roads) screen in
        Canvas.configure_text screen t;
        img, t)
    in
    {
      avatar = avatar;
      resources = new_rs;
      background = back;
      victory_points = victory_points;
      roads = roads;
    }
  let draw_info screen =
    let rx, ry = 74, (1006-(128+10)*0) in
    let vx, vy = 74, (1006-(128+10)*1) in
    let tx, ty = 74, (1006-(128+10)*2) in
    let ri = draw_image screen "./imgs/road_info.png" (rx+200) ry in
    let vi = draw_image screen "./imgs/village_info.png" (vx+200) vy in
    let ti = draw_image screen "./imgs/town_info.png" (tx+200) ty in
    [ri; vi; ti]

  let render screen gameState =
    let background = draw_background screen in
    let info       = draw_info       screen in
    let fields     = draw_board      screen gameState.Catan.board  in
    let numbers    = draw_numbers    screen gameState.Catan.board  in
    let roads      = draw_roads      screen gameState.Catan.roads in
    let buildings  = draw_building   screen gameState.Catan.settlements  in
    let thief     = draw_thief     screen gameState.Catan.thief in
    let dices      = draw_dices      screen gameState.Catan.dices in
    let players    = List.map
                      (fun n -> draw_player screen gameState.tour @@ List.nth gameState.Catan.players n)
                      @@ Utils.range gameState.Catan.numberOfPlayers in
    let next       = draw_image screen "./control/next_trans.png"  (1846-(128+10)*0) (1006-(128+10)*0) in
    let bank       = draw_image screen "./control/bank_trans.png"  (1846-(128+10)*1) (1006-(128+10)*0) in
    let bob0       = draw_image screen "./control/bob_trans.png"   74 (1006-(128+10)*0) in
    let bob1       = draw_image screen "./control/bob_trans.png"   74 (1006-(128+10)*1) in
    let bob2       = draw_image screen "./control/bob_trans.png"   74 (1006-(128+10)*2) in
    let trade      = draw_image screen "./control/trade_trans.png" 74 (1006-(128+10)*3) in
    pack [screen];
    {
      background  = background;
      thief      = thief;
      settlements = buildings;
      numbers     = numbers;
      fields      = fields;
      roads       = roads;
      dices       = dices;
      players     = players;
      info     = info;
      non_active  = [next; bank; bob0; bob1; bob2; trade];
    }
  let clear_screen screen gameObjects =
    Canvas.delete screen [gameObjects.background];
    Canvas.delete screen [gameObjects.thief];
    Canvas.delete screen gameObjects.fields;
    Canvas.delete screen gameObjects.numbers;
    Canvas.delete screen gameObjects.settlements;
    Canvas.delete screen gameObjects.roads;
    Canvas.delete screen [fst gameObjects.dices; snd gameObjects.dices];
    Canvas.delete screen gameObjects.non_active;
    Canvas.delete screen gameObjects.info;
    List.iter (fun (p : player_info) ->
      Canvas.delete screen [p.background];
      Canvas.delete screen [p.avatar];
      Canvas.delete screen [fst p.victory_points; snd p.victory_points];
      Canvas.delete screen [fst p.roads; snd p.roads];
      List.iter (fun (r, t) ->
        Canvas.delete screen [r; t]
      ) p.resources
    ) gameObjects.players
  let refresh screen gameState renderedObjects =
    clear_screen screen renderedObjects;
    let newObj = render screen gameState in
    renderedObjects.background  <- newObj.background;
    renderedObjects.thief      <- newObj.thief;
    renderedObjects.fields      <- newObj.fields;
    renderedObjects.numbers     <- newObj.numbers;
    renderedObjects.settlements <- newObj.settlements;
    renderedObjects.roads       <- newObj.roads;
    renderedObjects.dices       <- newObj.dices;
    renderedObjects.non_active  <- newObj.non_active;
    renderedObjects.players     <- newObj.players
end

module Control = struct
  (* TYPY *)
  type t = {
    mutable enter_trade   : tagOrId option;
    mutable bank_trade    : tagOrId option;
    mutable build_village : tagOrId option;
    mutable build_town    : tagOrId option;
    mutable build_road    : tagOrId option;
    mutable roll_dices    : tagOrId option;
    mutable end_tour      : tagOrId option;
  }

  let clear_controls screen gameState gameObjects gameControls =
    begin match gameControls.enter_trade with
    | Some obj -> (Canvas.delete screen [obj]; gameControls.enter_trade <- None)
    | None -> () end;
    begin match gameControls.build_village with
    | Some obj -> (Canvas.delete screen [obj]; gameControls.build_village <- None)
    | None -> () end;
    begin match gameControls.build_town with
    | Some obj -> (Canvas.delete screen [obj]; gameControls.build_town <- None)
    | None -> () end;
    begin match gameControls.build_road with
    | Some obj -> (Canvas.delete screen [obj]; gameControls.build_road <- None)
    | None -> () end;
    begin match gameControls.roll_dices with
    | Some obj -> (Canvas.delete screen [obj]; gameControls.roll_dices <- None)
    | None -> () end;
    begin match gameControls.end_tour with
    | Some obj -> (Canvas.delete screen [obj]; gameControls.end_tour <- None)
    | None -> () end;
    Textures.refresh screen gameState gameObjects

  let rec end_tour screen gameState gameObjects gameControls =
    let obj = Textures.draw_image screen "./control/next.png" (1920-74) (1006-(128+10)*0) in
    gameControls.end_tour <- Some obj;
    Canvas.bind ~events:[`ButtonPress]
      ~action:(fun _ ->
        (* Canvas.delete screen [obj];
        gameControls.enter_trade <- None; *)
        Catan.next_player gameState;
        clear_controls screen gameState gameObjects gameControls;
        enable_dice_roll screen gameState gameObjects gameControls
      ) screen obj
  and enable_standatd_tour screen gameState gameObjects gameControls =
    (* Textures.refresh screen gameState gameObjects; *)
    clear_controls screen gameState gameObjects gameControls;
    (* Kupno karty niedorozwoju *)
    (* Wykorzystanie karty niedorozwoju *)
    end_tour             screen gameState gameObjects gameControls;
    enable_build_road    screen gameState gameObjects gameControls;
    enable_build_village screen gameState gameObjects gameControls;
    enable_build_town    screen gameState gameObjects gameControls;
    enable_trade         screen gameState gameObjects gameControls;
    enable_trade_bank    screen gameState gameObjects gameControls
  and enable_dice_roll screen gameState gameObjects gameControls =
    let obj = Textures.draw_image screen "./control/trans_butt.png" 74 74 in
    gameControls.roll_dices <- Some obj;
    Canvas.bind ~events:[`ButtonPress]
      ~action:(fun e ->
        (* Catan.next_player gameState; *)
        Canvas.delete screen [obj];
        gameControls.roll_dices <- None;
        let n1, n2 = Utils.random_pair () in
        if n1+n2 = 7
          then begin
            gameState.Catan.dices <- n1, n2;
            Textures.refresh screen gameState gameObjects;
            enable_move_thief screen gameState gameObjects gameControls
          end
          else begin
            Catan.dice_roll gameState n1 n2;
            (* Textures.refresh screen gameState gameObjects; *)
            enable_standatd_tour screen gameState gameObjects gameControls
          end
      ) screen obj

  and enable_move_thief screen gameState gameObjects gameControls =
    let objLst = List.map (fun (x, y) ->
      Textures.draw_image screen "./control/trans_butt.png" x y
    ) Textures.field_coords
    in List.iter2 (fun obj idx ->
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun e ->
          Catan.move_thief gameState idx;
          Canvas.delete screen objLst;
          choose_player screen gameState gameObjects gameControls idx
          (* enable_standatd_tour screen gameState gameObjects gameControls *)
        )
        screen
        obj
    ) objLst (Utils.range 19)
  and choose_player screen gameState gameObjects gameControls idx =
    let places = List.nth Catan.fields_buildplaces idx in
    let objLst = List.filter_map (fun i ->
        let b = List.nth gameState.Catan.settlements i
        in match b with
        | Village (fl, c)
        | Town (fl, c) ->
            begin
              let x,y = List.nth Textures.settlements_coords i
              in Some ((Textures.draw_image screen "./control/trans_butt.png" x y), c)
            end
        | Empty fl -> None
        | Blocked -> None
      ) places
    in if List.length objLst > 0
      then begin
        List.iter (fun (e, c) ->
          Canvas.bind ~events:[`ButtonPress]
            ~action:(fun _ ->
              Canvas.delete screen @@ List.map (fun e -> fst e) objLst;
              Catan.steal_from gameState c;
              enable_standatd_tour screen gameState gameObjects gameControls
            ) screen e
        ) objLst
      end
      else enable_standatd_tour screen gameState gameObjects gameControls

  and enable_trade_bank screen gameState gameObjects gameControls =
    let obj = Textures.draw_image screen "./control/bank.png" (1846-(128+10)*1) (1006-(128+10)*0) in
    gameControls.bank_trade <- Some obj;
    Canvas.bind ~events:[`ButtonPress]
      ~action:(fun _ ->
        (* Canvas.delete screen [obj];
        gameControls.enter_trade <- None; *)
        clear_controls screen gameState gameObjects gameControls;
        trade_offer_bank screen gameState gameObjects gameControls [|0;0;0;0;0|] [|0;0;0;0;0|]
      ) screen obj
  and trade_offer_bank screen gameState gameObjects gameControls claim offer =
    let accept =
      let img = if Catan.is_valid_trade_offer gameState offer claim (-1)
                  then "./control/accept.png"
                  else "./control/accept_trans.png"
      in Textures.draw_image screen img (160) (1080/4+80)

    in let decline = Textures.draw_image screen "./control/decline.png" (1846-(128+10)*1) (1006-(128+10)*0)

    (* Kontrolki do kupna *)
    in let take_controls = List.map (fun i ->
      Textures.draw_image screen Catan.sblocks.(i) (80+40*i) (1080/4-50)
    ) @@ Utils.range 5
    in let take_numbers = List.map (fun i ->
      let t = Canvas.create_text ~x:(80+40*i) ~y:(1080/4-46) ~font:"Helvetica 28 bold" ~text:(Int.to_string claim.(i)) screen in
      Canvas.configure_text screen t;
      t
    ) @@ Utils.range 5

    (* Kontrolnki do sprzedarzy *)
    in let give_controls = List.map (fun i ->
      Textures.draw_image screen Catan.sblocks.(i) (80+40*i) (1080/4)
    ) @@ Utils.range 5
    in let give_numbers = List.map (fun i ->
      let t = Canvas.create_text ~x:(80+40*i) ~y:(1080/4+4) ~font:"Helvetica 28 bold" ~text:(Int.to_string offer.(i)) screen in
      Canvas.configure_text screen t;
      t
    ) @@ Utils.range 5

    (* Eventy accept i decline *)
    in Canvas.bind ~events:[`ButtonPress]
      ~action:(fun _ ->
        Canvas.delete screen take_numbers;
        Canvas.delete screen give_numbers;
        Canvas.delete screen take_controls;
        Canvas.delete screen give_controls;
        Canvas.delete screen [decline; accept];
        enable_standatd_tour screen gameState gameObjects gameControls
      ) screen decline;
    Canvas.bind ~events:[`ButtonPress]
    ~action:(fun _ ->
      Canvas.delete screen take_numbers;
      Canvas.delete screen give_numbers;
      Canvas.delete screen take_controls;
      Canvas.delete screen give_controls;
      Canvas.delete screen [decline; accept];
      if Catan.trade_bank gameState offer claim
        then enable_standatd_tour screen gameState gameObjects gameControls
        else trade_offer_bank screen gameState gameObjects gameControls claim offer
    ) screen accept;

    List.iter (fun i ->
      (* Eventy kup *)
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          Canvas.delete screen take_numbers;
          Canvas.delete screen give_numbers;
          Canvas.delete screen take_controls;
          Canvas.delete screen give_controls;
          Canvas.delete screen [decline; accept];
          claim.(i) <- claim.(i)+1;
          trade_offer_bank screen gameState gameObjects gameControls claim offer
        ) screen @@ List.nth take_controls i;
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          Canvas.delete screen take_numbers;
          Canvas.delete screen give_numbers;
          Canvas.delete screen take_controls;
          Canvas.delete screen give_controls;
          Canvas.delete screen [decline; accept];
          claim.(i) <- claim.(i)+1;
          trade_offer_bank screen gameState gameObjects gameControls claim offer
        ) screen @@ List.nth take_numbers i;
      (* Eventy sprzedaj *)
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          Canvas.delete screen take_numbers;
          Canvas.delete screen give_numbers;
          Canvas.delete screen take_controls;
          Canvas.delete screen give_controls;
          Canvas.delete screen [decline; accept];
          offer.(i) <- offer.(i)+4;
          trade_offer_bank screen gameState gameObjects gameControls claim offer
        ) screen @@ List.nth give_numbers i;
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          Canvas.delete screen take_numbers;
          Canvas.delete screen give_numbers;
          Canvas.delete screen take_controls;
          Canvas.delete screen give_controls;
          Canvas.delete screen [decline; accept];
          offer.(i) <- offer.(i)+4;
          trade_offer_bank screen gameState gameObjects gameControls claim offer
        ) screen @@ List.nth give_controls i;
    ) @@ Utils.range 5

  and enable_trade screen gameState gameObjects gameControls =
    let obj = Textures.draw_image screen "./control/trade.png" 74 (1006-(128+10)*3) in
    gameControls.enter_trade <- Some obj;
    Canvas.bind ~events:[`ButtonPress]
      ~action:(fun _ ->
        (* Canvas.delete screen [obj];
        gameControls.enter_trade <- None; *)
        clear_controls screen gameState gameObjects gameControls;
        trade_offer screen gameState gameObjects gameControls [|0;0;0;0;0|] [|0;0;0;0;0|]
      ) screen obj
  and trade_offer screen gameState gameObjects gameControls claim offer =
    let oponents = List.filter (fun e -> e != Catan.color2number gameState.tour) @@ Utils.range gameState.Catan.numberOfPlayers
    in let accepts = List.map (fun i ->
      let img = if Catan.is_valid_trade_offer gameState offer claim @@ List.nth oponents i
                  then "./control/player" ^ (Int.to_string @@ List.nth oponents i) ^ "accept.png"
                  else "./control/player" ^ (Int.to_string @@ List.nth oponents i) ^ "accept_trans.png"
      in Textures.draw_image screen img (110 + 100*i) (1080/4+80)
    ) @@ Utils.range @@ List.length oponents
    in let decline = Textures.draw_image screen "./control/decline.png" 74 (1006-(128+10)*3)

    (* Kontrolki do kupna *)
    in let take_controls = List.map (fun i ->
      Textures.draw_image screen Catan.sblocks.(i) (80+40*i) (1080/4-50)
    ) @@ Utils.range 5
    in let take_numbers = List.map (fun i ->
      let t = Canvas.create_text ~x:(80+40*i) ~y:(1080/4-46) ~font:"Helvetica 28 bold" ~text:(Int.to_string claim.(i)) screen in
      Canvas.configure_text screen t;
      t
    ) @@ Utils.range 5

    (* Kontrolnki do sprzedarzy *)
    in let give_controls = List.map (fun i ->
      Textures.draw_image screen Catan.sblocks.(i) (80+40*i) (1080/4)
    ) @@ Utils.range 5
    in let give_numbers = List.map (fun i ->
      let t = Canvas.create_text ~x:(80+40*i) ~y:(1080/4+4) ~font:"Helvetica 28 bold" ~text:(Int.to_string offer.(i)) screen in
      Canvas.configure_text screen t;
      t
    ) @@ Utils.range 5

    (* Eventy accept i decline *)
    in Canvas.bind ~events:[`ButtonPress]
      ~action:(fun _ ->
        Canvas.delete screen take_numbers;
        Canvas.delete screen give_numbers;
        Canvas.delete screen take_controls;
        Canvas.delete screen give_controls;
        Canvas.delete screen (decline::accepts);
        enable_standatd_tour screen gameState gameObjects gameControls
      ) screen decline;
    List.iter (fun i ->
      Canvas.bind ~events:[`ButtonPress]
      ~action:(fun _ ->
        Canvas.delete screen take_numbers;
        Canvas.delete screen give_numbers;
        Canvas.delete screen take_controls;
        Canvas.delete screen give_controls;
        Canvas.delete screen (decline::accepts);
        if Catan.trade gameState offer claim @@ List.nth oponents i
          then enable_standatd_tour screen gameState gameObjects gameControls
          else trade_offer screen gameState gameObjects gameControls claim offer
      ) screen @@ List.nth accepts i
    ) @@ Utils.range (gameState.Catan.numberOfPlayers - 1);

    List.iter (fun i ->
      (* Eventy kup *)
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          Canvas.delete screen take_numbers;
          Canvas.delete screen give_numbers;
          Canvas.delete screen take_controls;
          Canvas.delete screen give_controls;
          Canvas.delete screen (decline::accepts);
          claim.(i) <- claim.(i)+1;
          trade_offer screen gameState gameObjects gameControls claim offer
        ) screen @@ List.nth take_controls i;
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          Canvas.delete screen take_numbers;
          Canvas.delete screen give_numbers;
          Canvas.delete screen take_controls;
          Canvas.delete screen give_controls;
          Canvas.delete screen (decline::accepts);
          claim.(i) <- claim.(i)+1;
          trade_offer screen gameState gameObjects gameControls claim offer
        ) screen @@ List.nth take_numbers i;
      (* Eventy sprzedaj *)
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          Canvas.delete screen take_numbers;
          Canvas.delete screen give_numbers;
          Canvas.delete screen take_controls;
          Canvas.delete screen give_controls;
          Canvas.delete screen (decline::accepts);
          offer.(i) <- offer.(i)+1;
          trade_offer screen gameState gameObjects gameControls claim offer
        ) screen @@ List.nth give_numbers i;
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          Canvas.delete screen take_numbers;
          Canvas.delete screen give_numbers;
          Canvas.delete screen take_controls;
          Canvas.delete screen give_controls;
          Canvas.delete screen (decline::accepts);
          offer.(i) <- offer.(i)+1;
          trade_offer screen gameState gameObjects gameControls claim offer
        ) screen @@ List.nth give_controls i;
    ) @@ Utils.range 5

  and enable_build_road screen gameState gameObjects gameControls =
    if Catan.can_afford_road gameState
      then begin
        let obj = Textures.draw_image screen "./control/bob.png" 74 (1006-(128+10)*0) in
        gameControls.build_road <- Some obj;
        Canvas.bind ~events:[`ButtonPress]
          ~action:(fun _ ->
            (* Canvas.delete screen [obj]; *)
            clear_controls screen gameState gameObjects gameControls;
            choose_position_road screen gameState gameObjects gameControls
          ) screen obj
      end
  and enable_build_village screen gameState gameObjects gameControls =
    if Catan.can_afford_village gameState
      then begin
        let obj = Textures.draw_image screen "./control/bob.png" 74 (1006-(128+10)*1) in
        gameControls.build_village <- Some obj;
        Canvas.bind ~events:[`ButtonPress]
          ~action:(fun _ ->
            (* Canvas.delete screen [obj]; *)
            clear_controls screen gameState gameObjects gameControls;
            choose_position_village screen gameState gameObjects gameControls
          ) screen obj
      end
  and enable_build_town screen gameState gameObjects gameControls =
    if Catan.can_afford_town gameState
      then begin
        let obj = Textures.draw_image screen "./control/bob.png" 74 (1006-(128+10)*2) in
        gameControls.build_town <- Some obj;
        Canvas.bind ~events:[`ButtonPress]
          ~action:(fun _ ->
            (* Canvas.delete screen [obj]; *)
            clear_controls screen gameState gameObjects gameControls;
            choose_position_town screen gameState gameObjects gameControls
          ) screen obj
      end

  and choose_position_road screen gameState gameObjects gameControls =
    let cancel = Textures.draw_image screen "./control/decline.png" 74 (1006-(128+10)*0) in
    let objLst = List.filter_map (fun n ->
      let n1, n2 = List.nth Catan.roads n in
      let x1, y1 = List.nth Textures.settlements_coords n1 in
      let x2, y2 = List.nth Textures.settlements_coords n2 in
      match List.nth gameState.Catan.roads n with
        | Road _ -> None
        | NoRoad ->
          if Catan.is_valid_road_position gameState n
            then Some (Textures.draw_image screen "./control/empty.png" ((x1+x2)/2) ((y1+y2)/2), n)
            else None
    ) @@ Utils.range Catan.nof_roads
    in List.iter (fun (obj, idx) ->
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          let e = List.nth gameState.Catan.roads idx in
          match e with
          | Catan.NoRoad ->
              Catan.build_road gameState idx;
              Canvas.delete screen @@ List.map (fun e -> fst e) objLst;
              (* Textures.refresh screen gameState gameObjects; *)
              enable_standatd_tour screen gameState gameObjects gameControls
          | _ -> failwith "Ooops! Shouldn't get here... [err6]\n"
        ) screen obj
    ) objLst;
    Canvas.bind ~events:[`ButtonPress]
      ~action:(fun _ ->
        Canvas.delete screen [cancel];
        Canvas.delete screen @@ List.map (fun e -> fst e) objLst;
        enable_standatd_tour screen gameState gameObjects gameControls;
      ) screen cancel
  and choose_position_village screen gameState gameObjects gameControls =
    let cancel = Textures.draw_image screen "./control/decline.png" 74 (1006-(128+10)*1) in
    let objLst = List.filter_map (fun n ->
      let coords = List.nth Textures.settlements_coords n in
      let e = List.nth gameState.Catan.settlements n in
      match e with
      | Catan.Empty _ ->
        if Catan.is_valid_village_position gameState n
          then Some ((Textures.draw_image screen "./control/empty.png" (fst coords) (snd coords)), n)
          else None
      | _ -> None
    ) @@ Utils.range Catan.nof_settlements
    in List.iter (fun (obj, idx) ->
      print_string "siemano\n";
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          let e = List.nth gameState.Catan.settlements idx in
          match e with
          | Catan.Empty xs ->
              Catan.build_village gameState idx;
              Canvas.delete screen @@ List.map (fun e -> fst e) objLst;
              (* Textures.refresh screen gameState gameObjects; *)
              enable_standatd_tour screen gameState gameObjects gameControls
          | _ -> failwith "Ooops! Shouldn't get here... [err7]\n"
        ) screen obj
    ) objLst;
    Canvas.bind ~events:[`ButtonPress]
      ~action:(fun _ ->
        Canvas.delete screen [cancel];
        Canvas.delete screen @@ List.map (fun e -> fst e) objLst;
        enable_standatd_tour screen gameState gameObjects gameControls;
      ) screen cancel
  and choose_position_town screen gameState gameObjects gameControls =
    let cancel = Textures.draw_image screen "./control/decline.png" 74 (1006-(128+10)*2) in
    let objLst = List.filter_map (fun n ->
      let coords = List.nth Textures.settlements_coords n in
      let e = List.nth gameState.Catan.settlements n in
      match e with
      | Catan.Village (fl, c) ->
          begin
            if c = gameState.Catan.tour
              then Some ((Textures.draw_image screen "./control/empty.png" (fst coords) (snd coords)), n)
              else None
          end
      | _ -> None
    ) @@ Utils.range Catan.nof_settlements
    in List.iter (fun (obj, idx) ->
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          let e = List.nth gameState.Catan.settlements idx in
          match e with
          | Catan.Village (xs, c) ->
              Catan.build_town gameState idx;
              Canvas.delete screen @@ List.map (fun e -> fst e) objLst;
              (* Textures.refresh screen gameState gameObjects; *)
              enable_standatd_tour screen gameState gameObjects gameControls
          | _ -> failwith "Ooops! Shouldn't get here... [err8]\n"
        ) screen obj
    ) objLst;
    Canvas.bind ~events:[`ButtonPress]
      ~action:(fun _ ->
        Canvas.delete screen [cancel];
        Canvas.delete screen @@ List.map (fun e -> fst e) objLst;
        enable_standatd_tour screen gameState gameObjects gameControls;
      ) screen cancel

  let rec starting_buildings screen gameState gameObjects gameControls villagesLeft =
    let objLst = List.filter_map (fun n ->
      let coords = List.nth Textures.settlements_coords n in
      let e = List.nth gameState.Catan.settlements n in
      match e with
      | Catan.Empty _ -> Some ((Textures.draw_image screen "./control/empty.png" (fst coords) (snd coords)), n)
      | _ -> None
    ) @@ Utils.range Catan.nof_settlements
    in List.iter (fun (obj, idx) ->
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          let e = List.nth gameState.Catan.settlements idx in
          match e with
          | Catan.Empty xs ->
              Catan.build_village gameState idx;
              Canvas.delete screen @@ List.map (fun e -> fst e) objLst;
              (* Catan.next_player gameState; *)
              Textures.refresh screen gameState gameObjects;
              starting_roads screen gameState gameObjects gameControls idx villagesLeft
          | _ -> failwith "Ooops! Shouldn't get here... [err5]\n"
        ) screen obj
    ) objLst
  and starting_roads screen gameState gameObjects gameControls position villagesLeft =
    let objLst = List.filter_map (fun n ->
      let n1, n2 = List.nth Catan.roads n in
      let x1, y1 = List.nth Textures.settlements_coords n1 in
      let x2, y2 = List.nth Textures.settlements_coords n2 in
      match List.nth gameState.Catan.roads n with
        | Road _ -> None
        | NoRoad ->
          if n1 = position || n2 = position
            then Some (Textures.draw_image screen "./control/empty.png" ((x1+x2)/2) ((y1+y2)/2), n)
            else None
    ) @@ Utils.range Catan.nof_roads
    in List.iter (fun (obj, idx) ->
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          let e = List.nth gameState.Catan.roads idx in
          match e with
          | Catan.NoRoad ->
              Catan.build_road gameState idx;
              Canvas.delete screen @@ List.map (fun e -> fst e) objLst;
              Catan.next_player gameState;
              Textures.refresh screen gameState gameObjects;
              if villagesLeft > 1
                then begin
                  (* Catan.next_player gameState;
                  Textures.refresh screen gameState gameObjects; *)
                  starting_buildings screen gameState gameObjects gameControls (villagesLeft - 1)
                end
                else begin
                  (* Catan.next_player gameState;
                  Textures.refresh screen gameState gameObjects; *)
                  enable_dice_roll screen gameState gameObjects gameControls
                end
          | _ -> failwith "Ooops! Shouldn't get here... [err8]\n"
        ) screen obj
    ) objLst

  let start_game screen numberOfPlayers =
    (* Główny obiekt trzymający stan gry *)
    let gameState   = Catan.gameInit numberOfPlayers in
    (* Główny obiekt trzymający wszystkie narysowane obiekty (tagOrId) *)
    let gameObjects = Textures.render screen gameState in
    (* Główny obiekt trzymający kontrolki *)
    let gameControls =
      {
        enter_trade   = None;
        bank_trade    = None;
        build_village = None;
        build_town    = None;
        build_road    = None;
        roll_dices    = None;
        end_tour      = None;
      } in
    (* Korzeń drzwa eventów *)
    starting_buildings screen gameState gameObjects gameControls (numberOfPlayers*2)
end

let catan =
  let top = openTk () in
  Wm.title_set top "CATAN";
  let mainCanvas = Canvas.create ~width:1920 ~height:1080 top in
  Control.start_game mainCanvas 3;

  mainLoop () ;;
