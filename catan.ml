open Tk

(*
 TODO:
  0. Sprzątnąć gówniany kod
  1. Budowa dróg [DONE]
  2. Budowa miast [DONE]
  3. Sensowniejszy handel (ikonka accept dla każdego gracza)
  4. Murzyn (murzyn kradnie i blokuje)
  5. Punkty zwycięstwa (wsie, miasta, autostrada, władza rycerska)
  6. Karty niedorozwoju (rycerz, budowa drogi, pobranie surowców, monopol, punkt zwycięstwa)
  7. Kosmetyka
  8. Porty
  9. AI
 *)

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
  let droll () =
    Random.int 6 + 1, Random.int 6 + 1

end

module Catan = struct
  (* TYPY *)
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
    | Wood  of int
    | Clay  of int
    | Stone of int
    | Wheat of int
    | Sheep of int
    | Desert
  type settlement =
    | Village of field list * color
    | Town    of field list * color
    | Empty   of field list
    | Blocked
  type road =
    | Road of color
    | NoRoad
  type player_resources = {
    mutable wood  : int;
    mutable clay  : int;
    mutable stone : int;
    mutable wheat : int;
    mutable sheep : int;
  }
  type player = {
    mutable settlements : int list;
    mutable resources   : player_resources;
    mutable points      : int;
    mutable roads       : int list;
    mutable color       : color;
  }
  type t = {
            nofp    : int;
    mutable tour    : color;
    mutable board   : field list;
    mutable towns   : settlement list;
    mutable roads   : road list;
    mutable murzyn  : int;
    mutable dices   : int * int;
    mutable players : player list;
  }

  (* TABLICEC I STAŁE *)
  let sblocks = [|
    "./blocks/sblocks/wood.png";
    "./blocks/sblocks/clay.png";
    "./blocks/sblocks/stone.png";
    "./blocks/sblocks/wheat.png";
    "./blocks/sblocks/sheep.png";
  |]
  let order = [Red; Green; Blue]
  let nof_settlements = 54
  let nof_roads = 72
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
  let numbers = [5;2;6;3;8;10;9;12;11;4;8;10;9;4;5;6;3;11]
  let fields = Utils.shuffle ['D';'D';'D';'D';'G';'G';'G';'S';'S';'S';'S';'K';'K';'K';'O';'O';'O';'O';'P']
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

  (* FUNKCJE POMOCNICZE *)
  let color2number c =
    match c with
    | Red   -> 0
    | Green -> 1
    | Blue  -> 2
  let resource2str f =
    match f with
    | RWood  _ -> "wood"
    | RClay  _ -> "clay"
    | RStone _ -> "stone"
    | RWheat _ -> "wheat"
    | RSheep _ -> "sheep"
  let field2str f =
    match f with
    | Wood  _ -> "wood"
    | Clay  _ -> "clay"
    | Stone _ -> "stone"
    | Wheat _ -> "wheat"
    | Sheep _ -> "sheep"
    | Desert  -> "gówno"
  let color2str c =
    match c with
    | Red   -> "red"
    | Green -> "green"
    | Blue  -> "blue"
  let get_desert_idx board =
    let rec _aux n board =
      match List.nth board 0 with
        | Desert -> n
        | _ -> _aux (n+1) @@ List.tl board
    in _aux 0 board
  let check_resources fl n =
    List.filter_map (fun f ->
      match f with
      | Wood  i -> if i = n then Some (RWood  1) else None
      | Clay  i -> if i = n then Some (RClay  1) else None
      | Stone i -> if i = n then Some (RStone 1) else None
      | Wheat i -> if i = n then Some (RWheat 1) else None
      | Sheep i -> if i = n then Some (RSheep 1) else None
      | Desert  -> None
    ) fl
  let update_resources gameState player resource =
    let p = List.nth gameState.players @@ color2number player in
    let r = p.resources in
    match resource with
      | RWood  i -> r.wood <- r.wood+1
      | RClay  i -> r.clay <- r.clay+1
      | RStone i -> r.stone <- r.stone+1
      | RWheat i -> r.wheat <- r.wheat+1
      | RSheep i -> r.sheep <- r.sheep+1
  let can_build_village gameState =
    let player = List.nth gameState.players @@ color2number gameState.tour in
    let rs = player.resources in
    if List.for_all (fun b -> b) [rs.wood > 0; rs.clay > 0; rs.sheep > 0; rs.wheat > 0]
      then true
      else false
  let can_build_road gameState =
    let player = List.nth gameState.players @@ color2number gameState.tour in
    let rs = player.resources in
    if List.for_all (fun b -> b) [rs.wood > 0; rs.clay > 0]
      then true
      else false
  let can_build_town gameState =
    let player = List.nth gameState.players @@ color2number gameState.tour in
    let rs = player.resources in
    if List.for_all (fun b -> b) [rs.wheat >= 2; rs.stone >= 3]
      then true
      else false
  let is_trade_offer_valid gameState offer claim trader =
    let player = List.nth gameState.players @@ color2number gameState.tour in
    let rs = player.resources in
    if List.for_all (fun b -> b) [rs.wood >= offer.(0); rs.clay >= offer.(1); rs.stone >= offer.(2); rs.wheat >= offer.(3); rs.sheep >= offer.(4)]
      then true
      else false
  let verify_road_position gameState position =
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

  (* FUNKCJE GŁOWNE *)
  let first_player gameState =
    gameState.tour <- Red
  let next_player gameState =
    if gameState.nofp != 1
      then if gameState.nofp = 2
      then begin
        let current_player = gameState.tour in
        match current_player with
          | Red   -> gameState.tour <- Green
          | Green -> gameState.tour <- Red
          | _ -> failwith ""
      end
      else begin
        let current_player = gameState.tour in
        match current_player with
          | Red   -> gameState.tour <- Green
          | Green -> gameState.tour <- Blue
          | Blue  -> gameState.tour <- Red
      end
  let move_murzyn gameState position =
    print_string @@ "Moving murzyn to position " ^ Int.to_string position ^ "\n";
    gameState.murzyn <- position
  let build_village gameState position =
    print_string @@ "Player " ^ color2str gameState.tour ^ " builts a village at position " ^ Int.to_string position ^ "\n";
    let place = List.nth gameState.towns position in
    match place with
      | Empty x ->
          List.iter (fun i ->
            gameState.towns <- Utils.replace gameState.towns i Blocked
          ) @@ List.nth blocking position;
          let settlements = gameState.towns in
          gameState.towns <- Utils.replace settlements position (Village (x, gameState.tour));
          let player_info = List.nth gameState.players @@ color2number gameState.tour in
          let rs = player_info.resources in
          player_info.settlements <- List.append player_info.settlements [position];
          player_info.resources <- {wood = rs.wood-1; clay = rs.clay-1; wheat = rs.wheat-1; sheep = rs.sheep-1; stone = rs.stone};
          player_info.points <- player_info.points + 1
      | _ -> failwith "Ooops! You can build village only on empty field!\n"
  let build_town gameState position =
    print_string @@ "Player " ^ color2str gameState.tour ^ " builts a town at position " ^ Int.to_string position ^ "\n";
    let place = List.nth gameState.towns position in
    match place with
      | Village (x,c) ->
          List.iter (fun i ->
            gameState.towns <- Utils.replace gameState.towns i Blocked
          ) @@ List.nth blocking position;
          let settlements = gameState.towns in
          gameState.towns <- Utils.replace settlements position (Town (x, gameState.tour));
          let player_info = List.nth gameState.players @@ color2number gameState.tour in
          let rs = player_info.resources in
          player_info.settlements <- List.append player_info.settlements [position];
          player_info.resources <- {wood = rs.wood; clay = rs.clay; wheat = rs.wheat-2; sheep = rs.sheep; stone = rs.stone-3};
          player_info.points <- player_info.points + 1
      | _ -> failwith "Ooops! You can build town only on villages!\n"
  let build_road gameState position =
    print_string @@ "Player " ^ color2str gameState.tour ^ " builts a road at position " ^ Int.to_string position ^ "\n";
    gameState.roads <- Utils.replace gameState.roads position (Road gameState.tour);
    let player_info = List.nth gameState.players @@ color2number gameState.tour in
    let rs = player_info.resources in
    player_info.roads <- List.append player_info.roads [position];
    player_info.resources <- {wood = rs.wood-1; clay = rs.clay-1; wheat = rs.wheat; sheep = rs.sheep; stone = rs.stone}
  let dice_roll gameState n1 n2 =
    print_string @@ "Dice roll: " ^ Int.to_string (n1+n2) ^ "\n";
    gameState.dices <- n1, n2;
    List.iter (fun b ->
      match b with
      | Village (fl, c) ->
          List.iter (fun e ->
            print_string @@ "Player " ^ color2str c ^ " gains " ^ resource2str e ^ "\n";
            update_resources gameState c e
          ) @@ check_resources fl (n1+n2)
      | Town (fl, c) ->
          List.iter (fun e ->
            print_string @@ "Player " ^ color2str c ^ " gains 2 " ^ resource2str e ^ "\n";
            update_resources gameState c e;
            update_resources gameState c e
          ) @@ check_resources fl (n1+n2);
      | Empty _ -> ()
      | Blocked -> ()
    ) gameState.towns
  let trade gameState offer claim trader =
    if is_trade_offer_valid gameState offer claim trader
      then begin
        print_string "deal\n";
        let player = List.nth gameState.players @@ color2number gameState.tour in
        let rs = player.resources in
        rs.wood  <- rs.wood - offer.(0) + claim.(0);
        rs.clay  <- rs.clay - offer.(1) + claim.(1);
        rs.stone <- rs.stone - offer.(2) + claim.(2);
        rs.wheat <- rs.wheat - offer.(3) + claim.(3);
        rs.sheep <- rs.sheep - offer.(4) + claim.(4);
        true
      end else false

  (* FUNKCJE INICJALIZUJĄCE *)
  let settlementsInit board =
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
    let rec _generate_board n f out =
      match n, f with
        | nhd::ntl, fhd::ftl ->
            begin match fhd with
              | 'P' -> _generate_board n ftl (Desert::out)
              | 'D' -> _generate_board ntl ftl ((Wood nhd)::out)
              | 'G' -> _generate_board ntl ftl ((Clay nhd)::out)
              | 'S' -> _generate_board ntl ftl ((Wheat nhd)::out)
              | 'K' -> _generate_board ntl ftl ((Stone nhd)::out)
              | 'O' -> _generate_board ntl ftl ((Sheep nhd)::out)
              | _ -> failwith "Ooops, no such field!\n"
            end
        | [], fhd::ftl -> Desert::out
        | [], [] -> out
        | _ -> failwith "Ooops, shouldn't get here!\n"
      in _generate_board numbers fields []
  let playerInit n =
    {
      settlements = [];
      (* resources   = {wood = 4; clay = 4; stone = 0; wheat = 2; sheep = 2}; *)
      resources   = {wood = 20; clay = 20; stone = 20; wheat = 20; sheep = 20};
      points      = 0;
      roads       = [];
      color       = match n with 0 -> Red | 1 -> Green | 2 -> Blue | _ -> failwith "Ooops! Wrong player number!\n";
    }
  let gameInit numberOfPlayers =
    let board = boardInit () in {
      nofp    = numberOfPlayers;
      board   = board;
      towns   = settlementsInit board;
      roads   = List.map (fun _ -> NoRoad) @@ Utils.range nof_roads;
      murzyn  = get_desert_idx board;
      dices   = 6, 6;
      tour    = List.nth order 0;
      players = List.map (fun n -> playerInit n) @@ Utils.range numberOfPlayers;
    }

end

module Textures = struct
  (* TYPY *)
  type player_info = {
    mutable resources      : (tagOrId * tagOrId) list;
    mutable avatar         : tagOrId;
    mutable victory_points : tagOrId * tagOrId;
    mutable roads          : tagOrId * tagOrId;
    mutable background     : tagOrId;
  }
  type t = {
    mutable background : tagOrId;
    mutable murzyn     : tagOrId;
    mutable villages   : tagOrId list;
    mutable numbers    : tagOrId list;
    mutable fields     : tagOrId list;
    mutable roads      : tagOrId list;
    mutable dices      : tagOrId * tagOrId;
    mutable players    : player_info list;
    (* mutable current_pl : tagOrId; *)
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
    (xoffset + 90*2 - 180*2 - 90), (yoffset - 155*2 - 55); (**)
    (xoffset + 90*2 - 180*2),      (yoffset - 155*2 - 100);
    (xoffset + 90*2 - 180*2 + 90), (yoffset - 155*2 - 55);
    (xoffset + 90*2 - 180*1),      (yoffset - 155*2 - 100);
    (xoffset + 90*2 - 180*1 + 90), (yoffset - 155*2 - 55);
    (xoffset + 90*2 - 180*0),      (yoffset - 155*2 - 100);
    (xoffset + 90*2 - 180*0 + 90), (yoffset - 155*2 - 55);

    (xoffset + 90*1 - 180*2 - 90), (yoffset - 155*1 - 55);
    (xoffset + 90*1 - 180*2),      (yoffset - 155*1 - 100); (**)
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
        | Catan.Wood i
        | Catan.Clay i
        | Catan.Stone i
        | Catan.Wheat i
        | Catan.Sheep i -> draw (n-1) @@ draw_image screen ("./numbers/" ^ Int.to_string i ^ ".png") (fst @@ List.nth field_coords n) (snd @@ List.nth field_coords n) ::lst
        | Catan.Desert  -> draw (n-1) lst
      end
      else lst
    in draw 18 []
  let draw_building screen buildings =
    List.map2 (fun (x, y) b ->
      (* draw_image screen "./control/trans_butt.png" x y *)
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
  let draw_murzyn screen n =
    draw_image screen "./blocks/zlodziej.png" (fst @@ List.nth field_coords n) (snd @@ List.nth field_coords n)
  let move_murzyn screen n objLst =
    Canvas.delete screen [objLst.murzyn];
    draw_murzyn screen n
  let draw_dices screen (n1, n2) =
    let d1 = draw_image screen ("./dice/wd" ^ Int.to_string n1 ^ ".png") 85 85 in
    let d2 = draw_image screen ("./dice/wd" ^ Int.to_string n2 ^ ".png") 235 85 in
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
  let draw_info screen color =
    draw_image screen ("./players/player" ^ Int.to_string (Catan.color2number color) ^ "c.png") 360 85

  let render screen gameState =
    let background = draw_background screen in
    let fields     = draw_board      screen gameState.Catan.board  in
    let numbers    = draw_numbers    screen gameState.Catan.board  in
    let roads      = draw_roads      screen gameState.Catan.roads in
    let buildings  = draw_building   screen gameState.Catan.towns  in
    let murzyn     = draw_murzyn     screen gameState.Catan.murzyn in
    let dices      = draw_dices      screen gameState.Catan.dices in
    let players    = List.map (fun n -> draw_player screen gameState.tour @@ List.nth gameState.Catan.players n) @@ Utils.range gameState.Catan.nofp in
    (* let curr_pl    = draw_info       screen gameState.Catan.tour in *)
    let _ = draw_image screen "./control/trans_bob.png" 74 1006 in
    let _ = draw_image screen "./control/trans_trade.png" 74 (1006-128-10) in
    pack [screen];
    {
      background = background;
      murzyn     = murzyn;
      villages   = buildings;
      numbers    = numbers;
      fields     = fields;
      roads      = roads;
      dices      = dices;
      players    = players;
      (* current_pl = curr_pl; *)
    }
  let clear_screen screen gameObjects =
    Canvas.delete screen [gameObjects.background];
    Canvas.delete screen gameObjects.villages;
    Canvas.delete screen gameObjects.roads;
    Canvas.delete screen gameObjects.numbers;
    Canvas.delete screen gameObjects.fields;
    Canvas.delete screen gameObjects.roads;
    List.iter (fun p ->
      Canvas.delete screen [fst p.victory_points; snd p.victory_points; fst p.roads; snd p.roads];
      Canvas.delete screen [p.avatar];
      Canvas.delete screen [p.background];
      List.iter (fun (r, t) ->
        Canvas.delete screen [r];
        Canvas.delete screen [t]
      ) p.resources
    ) gameObjects.players
  let refresh screen gameState renderedObjects =
    clear_screen screen renderedObjects;
    let newObj = render screen gameState in
    renderedObjects.villages <- newObj.villages
end

module Control = struct
  let rec end_tour screen gameState gameObjects =
    failwith "not implemented\n"
  and enable_standatd_tour screen gameState gameObjects =
    Textures.refresh screen gameState gameObjects;
    (* Kupno karty niedorozwoju *)
    (* Wykorzystanie karty niedorozwoju *)
    (* Przycisk końca tury *)
    enable_build_road    screen gameState gameObjects;
    enable_build_village screen gameState gameObjects;
    enable_build_town    screen gameState gameObjects;
    enable_trade         screen gameState gameObjects;
    enable_dice_roll     screen gameState gameObjects
  and enable_move_murzyn screen gameState gameObjects =
    let objLst = List.map (fun (x, y) ->
      Textures.draw_image screen "./control/trans_butt.png" x y
    ) Textures.field_coords
    in List.iter2 (fun obj idx ->
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun e ->
          Catan.move_murzyn gameState idx;
          Canvas.delete screen objLst;
          Textures.refresh screen gameState gameObjects;
          enable_standatd_tour screen gameState gameObjects
        )
        screen
        obj
    ) objLst (Utils.range 19)
  and enable_dice_roll screen gameState gameObjects =
    let obj = Textures.draw_image screen "./control/trans_butt.png" 74 74 in
    Canvas.bind ~events:[`ButtonPress]
      ~action:(fun e ->
        Catan.next_player gameState;
        Canvas.delete screen [obj];
        let n1, n2 = Utils.droll () in
        if n1+n2 = 7
          then begin
            gameState.Catan.dices <- n1, n2;
            Textures.refresh screen gameState gameObjects;
            enable_move_murzyn screen gameState gameObjects
          end
          else begin
            Catan.dice_roll gameState n1 n2;
            Textures.refresh screen gameState gameObjects;
            enable_standatd_tour screen gameState gameObjects
          end
      ) screen obj

  and enable_trade screen gameState gameObjects =
    let obj = Textures.draw_image screen "./control/trade.png" 74 (1006-128-10) in
    Canvas.bind ~events:[`ButtonPress]
      ~action:(fun _ ->
        Canvas.delete screen [obj];
        trade_offer screen gameState gameObjects [|0;0;0;0;0|] [|0;0;0;0;0|]
        (* Catan.trade gameState *)
        (* Textures.refresh screen gameState gameObjects *)
      ) screen obj
  and trade_offer screen gameState gameObjects claim offer =
    let oponents = List.filter (fun e -> e != Catan.color2number gameState.tour) @@ Utils.range gameState.Catan.nofp
    in let accepts = List.map (fun i ->
      Textures.draw_image screen ("./control/player" ^ (Int.to_string @@ List.nth oponents i) ^ "accept.png") (110 + 100*i) (1080/4+80)
    ) @@ Utils.range @@ List.length oponents
    in let decline = Textures.draw_image screen "./control/decline.png" 74 (1006-128-10)

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
        enable_standatd_tour screen gameState gameObjects
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
          then enable_standatd_tour screen gameState gameObjects
          else trade_offer screen gameState gameObjects claim offer
      ) screen @@ List.nth accepts i
    ) @@ Utils.range (gameState.Catan.nofp - 1);

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
          trade_offer screen gameState gameObjects claim offer
        ) screen @@ List.nth take_controls i;
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          Canvas.delete screen take_numbers;
          Canvas.delete screen give_numbers;
          Canvas.delete screen take_controls;
          Canvas.delete screen give_controls;
          Canvas.delete screen (decline::accepts);
          claim.(i) <- claim.(i)+1;
          trade_offer screen gameState gameObjects claim offer
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
          trade_offer screen gameState gameObjects claim offer
        ) screen @@ List.nth give_numbers i;
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          Canvas.delete screen take_numbers;
          Canvas.delete screen give_numbers;
          Canvas.delete screen take_controls;
          Canvas.delete screen give_controls;
          Canvas.delete screen (decline::accepts);
          offer.(i) <- offer.(i)+1;
          trade_offer screen gameState gameObjects claim offer
        ) screen @@ List.nth give_controls i;
    ) @@ Utils.range 5

  and enable_build_road screen gameState gameObjects =
    if Catan.can_build_road gameState
      then begin
        let obj = Textures.draw_image screen "./control/bob.png" (74+10+128) 1006 in
        Canvas.bind ~events:[`ButtonPress]
          ~action:(fun _ ->
            Canvas.delete screen [obj];
            choose_position_road screen gameState gameObjects
            (* Textures.refresh screen gameState gameObjects *)
          ) screen obj
      end
  and enable_build_village screen gameState gameObjects =
    if Catan.can_build_village gameState
      then begin
        let obj = Textures.draw_image screen "./control/bob.png" 74 1006 in
        Canvas.bind ~events:[`ButtonPress]
          ~action:(fun _ ->
            Canvas.delete screen [obj];
            choose_position_village screen gameState gameObjects
            (* Textures.refresh screen gameState gameObjects *)
          ) screen obj
      end
  and enable_build_town screen gameState gameObjects =
    if Catan.can_build_town gameState
      then begin
        let obj = Textures.draw_image screen "./control/bob.png" (74+10+128) (1006-10-128) in
        Canvas.bind ~events:[`ButtonPress]
          ~action:(fun _ ->
            Canvas.delete screen [obj];
            choose_position_town screen gameState gameObjects
            (* Textures.refresh screen gameState gameObjects *)
          ) screen obj
      end

  and choose_position_road screen gameState gameObjects =
    let objLst = List.filter_map (fun n ->
      let n1, n2 = List.nth Catan.roads n in
      let x1, y1 = List.nth Textures.settlements_coords n1 in
      let x2, y2 = List.nth Textures.settlements_coords n2 in
      match List.nth gameState.Catan.roads n with
        | Road _ -> None
        | NoRoad ->
          if Catan.verify_road_position gameState n
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
              Textures.refresh screen gameState gameObjects;
              enable_standatd_tour screen gameState gameObjects
          | _ -> failwith "Ooops! Shouldn't get here... [err6]\n"
        ) screen obj
    ) objLst
  and choose_position_village screen gameState gameObjects =
    let objLst = List.filter_map (fun n ->
      let coords = List.nth Textures.settlements_coords n in
      let e = List.nth gameState.Catan.towns n in
      match e with
      | Catan.Empty _ -> Some ((Textures.draw_image screen "./control/empty.png" (fst coords) (snd coords)), n)
      | _ -> None
    ) @@ Utils.range Catan.nof_settlements
    in List.iter (fun (obj, idx) ->
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          let e = List.nth gameState.Catan.towns idx in
          match e with
          | Catan.Empty xs ->
              Catan.build_village gameState idx;
              Canvas.delete screen @@ List.map (fun e -> fst e) objLst;
              Textures.refresh screen gameState gameObjects;
              enable_standatd_tour screen gameState gameObjects
          | _ -> failwith "Ooops! Shouldn't get here... [err7]\n"
        ) screen obj
    ) objLst
  and choose_position_town screen gameState gameObjects =
    let objLst = List.filter_map (fun n ->
      let coords = List.nth Textures.settlements_coords n in
      let e = List.nth gameState.Catan.towns n in
      match e with
      | Catan.Village _ -> Some ((Textures.draw_image screen "./control/empty.png" (fst coords) (snd coords)), n)
      | _ -> None
    ) @@ Utils.range Catan.nof_settlements
    in List.iter (fun (obj, idx) ->
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          let e = List.nth gameState.Catan.towns idx in
          match e with
          | Catan.Village (xs, c) ->
              Catan.build_town gameState idx;
              Canvas.delete screen @@ List.map (fun e -> fst e) objLst;
              Textures.refresh screen gameState gameObjects;
              enable_standatd_tour screen gameState gameObjects
          | _ -> failwith "Ooops! Shouldn't get here... [err8]\n"
        ) screen obj
    ) objLst

  let rec starting_buildings screen gameState gameObjects hm =
    let objLst = List.filter_map (fun n ->
      let coords = List.nth Textures.settlements_coords n in
      let e = List.nth gameState.Catan.towns n in
      match e with
      | Catan.Empty _ -> Some ((Textures.draw_image screen "./control/empty.png" (fst coords) (snd coords)), n)
      | _ -> None
    ) @@ Utils.range Catan.nof_settlements
    in List.iter (fun (obj, idx) ->
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          let e = List.nth gameState.Catan.towns idx in
          match e with
          | Catan.Empty xs ->
              Catan.build_village gameState idx;
              Canvas.delete screen @@ List.map (fun e -> fst e) objLst;
              if hm > 1
                then begin
                  Catan.next_player gameState;
                  Textures.refresh screen gameState gameObjects;
                  starting_buildings screen gameState gameObjects (hm - 1)
                end
                else begin
                  (* Catan.first_player gameState; *)
                  Textures.refresh screen gameState gameObjects;
                  enable_dice_roll screen gameState gameObjects
                end
          | _ -> failwith "Ooops! Shouldn't get here... [err5]\n"
        ) screen obj
    ) objLst

  let start_game screen numberOfPlayers =
    (* Główny obiekt trzymający stan gry *)
    let gameState   = Catan.gameInit numberOfPlayers in
    (* Główny obiekt trzymający wszystkie narysowane obiekty (tagOrId) *)
    let gameObjects = Textures.render screen gameState in
    (* Korzeń drzwa eventów *)
    (* choose_position_road screen gameState gameObjects *)
    starting_buildings screen gameState gameObjects (numberOfPlayers*2)
end

let catan =
  let top = openTk () in
  Wm.title_set top "CATAN";
  let mainCanvas = Canvas.create ~width:1920 ~height:1080 top in
  Control.start_game mainCanvas 3;

  mainLoop () ;;

(*
let objLst = List.map (fun (n1, n2) ->
      let x1, y1 = List.nth Textures.settlements_coords n1 in
      let x2, y2 = List.nth Textures.settlements_coords n2 in
      let road_img =
        if x1 = x2
          then "./buildings/player0roadv.png"
          else if y1 > y2 then "./buildings/player0roadr.png" else"./buildings/player0roadl.png"
      in Textures.draw_image screen road_img ((x1+x2)/2) ((y1+y2)/2)
    ) Catan.roads
    in ()
*)