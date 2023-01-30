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
  let droll () =
    Random.int 6 + 1, Random.int 6 + 1

end

module Catan = struct
  (* TYPY *)
  type color =
    | Green
    | Red
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
    mutable roads       : unit;
    mutable color       : color;
  }
  type t = {
    mutable tour    : color;
    mutable board   : field list;
    mutable towns   : settlement list;
    mutable roads   : unit;
    mutable murzyn  : int;
    mutable dices   : int * int;
    mutable players : player list;
  }

  (* TABLICEC I STAŁE *)
  let order = [Green; Red]
  let nof_settlements = 54
  let numbers = [5;2;6;3;8;10;9;12;11;4;8;10;9;4;5;6;3;11]
  let fields = Utils.shuffle ['D';'D';'D';'D';'G';'G';'G';'S';'S';'S';'S';'K';'K';'K';'O';'O';'O';'O';'P']

  (* FUNKCJE POMOCNICZE *)
  let color2number c =
    match c with
      | Green -> 0
      | Red   -> 1
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
    | Green -> "green"
    | Red   -> "red"
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

  (* FUNKCJE GŁOWNE *)
  let first_player gameState =
    gameState.tour <- Green
  let next_player gameState =
    let current_player = gameState.tour in
    match current_player with
      | Green -> gameState.tour <- Red
      | Red   -> gameState.tour <- Green
  let move_murzyn gameState position =
    print_string @@ "Moving murzyn to position " ^ Int.to_string position ^ "\n";
    gameState.murzyn <- position
  let build_village gameState position =
    print_string @@ "Player " ^ color2str gameState.tour ^ " builts a village at position " ^ Int.to_string position ^ "\n";
    let place = List.nth gameState.towns position in
    match place with
      | Empty x ->
          let settlements = gameState.towns in
          gameState.towns <- Utils.replace settlements position (Village (x, gameState.tour));
          let player_info = List.nth gameState.players @@ color2number gameState.tour in
          let rs = player_info.resources in
          player_info.settlements <- List.append player_info.settlements [position];
          player_info.resources <- {wood = rs.wood-1; clay = rs.clay-1; wheat = rs.wheat-1; sheep = rs.sheep-1; stone = rs.stone}
      | _ -> failwith "Ooops! You can build village only on empty field!\n"
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
      | Town (fl, c) -> ()
      | Empty _ -> ()
      | Blocked -> ()
    ) gameState.towns

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
      resources   = {wood = 2; clay = 2; stone = 0; wheat = 2; sheep = 2};
      points      = 0;
      roads       = ();
      color       = match n with 0 -> Green | 1 -> Red | _ -> failwith "Ooops! Wrong player number!\n";
    }
  let gameInit () =
    let board = boardInit () in {
      board   = board;
      towns   = settlementsInit board;
      roads   = ();
      murzyn  = get_desert_idx board;
      dices   = 0, 0;
      tour    = List.nth order 0;
      players = [playerInit 0; playerInit 1];
    }

end

module Textures = struct
  (* TYPY *)
  type player_info = {
    mutable resources   : (tagOrId * tagOrId) list;
    mutable avatar      : tagOrId;
    mutable settlements : tagOrId * tagOrId;
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
    mutable current_pl : tagOrId;
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
    (xoffset + 90*2 - 180*2 - 89), (yoffset - 155*2 - 55);
    (xoffset + 90*2 - 180*2), (yoffset - 155*2 - 100);
    (xoffset + 90*2 - 180*2 + 89), (yoffset - 155*2 - 55);
    (xoffset + 90*2 - 180*1), (yoffset - 155*2 - 100);
    (xoffset + 90*2 - 180*1 + 89), (yoffset - 155*2 - 55);
    (xoffset + 90*2 - 180*0), (yoffset - 155*2 - 100);
    (xoffset + 90*2 - 180*0 + 89), (yoffset - 155*2 - 55);

    (xoffset + 90*1 - 180*2 - 89), (yoffset - 155*1 - 55);
    (xoffset + 90*1 - 180*2), (yoffset - 155*1 - 100);
    (xoffset + 90*1 - 180*2 + 89), (yoffset - 155*1 - 55);
    (xoffset + 90*1 - 180*1), (yoffset - 155*1 - 100);
    (xoffset + 90*1 - 180*1 + 89), (yoffset - 155*1 - 55);
    (xoffset + 90*1 - 180*0), (yoffset - 155*1 - 100);
    (xoffset + 90*1 - 180*0 + 89), (yoffset - 155*1 - 55);
    (xoffset + 90*1 + 180*1), (yoffset - 155*1 - 100);
    (xoffset + 90*1 + 180*1 + 89), (yoffset - 155*1 - 55);

    (xoffset + 90*1 - 180*3), (yoffset - 155*1 + 100);
    (xoffset + 90*1 - 180*2 - 89), (yoffset - 155*1 + 55);
    (xoffset + 90*1 - 180*2), (yoffset - 155*1 + 100);
    (xoffset + 90*1 - 180*2 + 89), (yoffset - 155*1 + 55);
    (xoffset + 90*1 - 180*1), (yoffset - 155*1 + 100);
    (xoffset + 90*1 - 180*1 + 89), (yoffset - 155*1 + 55);
    (xoffset + 90*1 - 180*0), (yoffset - 155*1 + 100);
    (xoffset + 90*1 - 180*0 + 89), (yoffset - 155*1 + 55);
    (xoffset + 90*1 + 180*1), (yoffset - 155*1 + 100);
    (xoffset + 90*1 + 180*1 + 89), (yoffset - 155*1 + 55);
    (xoffset + 90*1 + 180*2), (yoffset - 155*1 + 100);

    (xoffset + 90*1 - 180*3), (yoffset + 155*1 - 100);
    (xoffset + 90*1 - 180*2 - 89), (yoffset + 155*1 - 55);
    (xoffset + 90*1 - 180*2), (yoffset + 155*1 - 100);
    (xoffset + 90*1 - 180*2 + 89), (yoffset + 155*1 - 55);
    (xoffset + 90*1 - 180*1), (yoffset + 155*1 - 100);
    (xoffset + 90*1 - 180*1 + 89), (yoffset + 155*1 - 55);
    (xoffset + 90*1 - 180*0), (yoffset + 155*1 - 100);
    (xoffset + 90*1 - 180*0 + 89), (yoffset + 155*1 - 55);
    (xoffset + 90*1 + 180*1), (yoffset + 155*1 - 100);
    (xoffset + 90*1 + 180*1 + 89), (yoffset + 155*1 - 55);
    (xoffset + 90*1 + 180*2), (yoffset + 155*1 - 100);

    (xoffset + 90*1 - 180*2 - 89), (yoffset + 155*1 + 55);
    (xoffset + 90*1 - 180*2), (yoffset + 155*1 + 100);
    (xoffset + 90*1 - 180*2 + 89), (yoffset + 155*1 + 55);
    (xoffset + 90*1 - 180*1), (yoffset + 155*1 + 100);
    (xoffset + 90*1 - 180*1 + 89), (yoffset + 155*1 + 55);
    (xoffset + 90*1 - 180*0), (yoffset + 155*1 + 100);
    (xoffset + 90*1 - 180*0 + 89), (yoffset + 155*1 + 55);
    (xoffset + 90*1 + 180*1), (yoffset + 155*1 + 100);
    (xoffset + 90*1 + 180*1 + 89), (yoffset + 155*1 + 55);

    (xoffset + 90*2 - 180*2 - 89), (yoffset + 155*2 + 55);
    (xoffset + 90*2 - 180*2), (yoffset + 155*2 + 100);
    (xoffset + 90*2 - 180*2 + 89), (yoffset + 155*2 + 55);
    (xoffset + 90*2 - 180*1), (yoffset + 155*2 + 100);
    (xoffset + 90*2 - 180*1 + 89), (yoffset + 155*2 + 55);
    (xoffset + 90*2 - 180*0), (yoffset + 155*2 + 100);
    (xoffset + 90*2 - 180*0 + 89), (yoffset + 155*2 + 55);
  ]

  (* FUNKCJE *)
  let draw_image screen img_path x y =
    let img_canvas = Canvas.create_image screen ~x:x ~y:y ~anchor:`Center in
    let img = Imagephoto.create () in
    Imagephoto.configure img ~file:img_path;
    Canvas.configure_image screen img_canvas ~image:img;
    img_canvas
  let draw_background screen =
    draw_image screen "./imgs/background.png" (fst resolution / 2) (snd resolution / 2)
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
      | Catan.Town    _ -> failwith "Ooops! We don't have towns yet!\n"
      | Catan.Empty   _ -> draw_image screen "./buildings/empty.png" x y
      | Catan.Blocked   -> draw_image screen "./buildings/empty.png" x y
    ) settlements_coords buildings
  let draw_murzyn screen n =
    draw_image screen "./blocks/murzyn.png" (fst @@ List.nth field_coords n) (snd @@ List.nth field_coords n)
  let move_murzyn screen n objLst =
    Canvas.delete screen [objLst.murzyn];
    draw_murzyn screen n
  let draw_dices screen (n1, n2) =
    let d1 = draw_image screen ("./dice/d" ^ Int.to_string n1 ^ ".png") 74 74 in
    let d2 = draw_image screen ("./dice/d" ^ Int.to_string n2 ^ ".png") 222 74 in
    d1, d2
  let draw_player screen player =
    let n = Catan.color2number player.Catan.color + 1 in
    let avatar = draw_image screen ("./players/player" ^ (Int.to_string @@ n-1) ^ ".png") (1920-110) (110*n + 150*(n-1)) in
    let rs = player.Catan.resources in
    let new_rs = [
      (let img = draw_image screen "./blocks/sblocks/wood.png"  (1920-210+20)  (200*n + 40 + (60*n-60)) in
       let t = Canvas.create_text ~x:(1920-210+20) ~y:(200*n + 44 + (60*n-60)) ~font:"Helvetica 28 bold" ~text:(Int.to_string rs.Catan.wood) screen in
       Canvas.configure_text screen t;
       img, t);
      (let img = draw_image screen "./blocks/sblocks/clay.png"  (1920-210+60)  (200*n + 40 + (60*n-60)) in
       let t = Canvas.create_text ~x:(1920-210+60) ~y:(200*n + 44 + (60*n-60)) ~font:"Helvetica 28 bold" ~text:(Int.to_string rs.Catan.clay) screen in
       Canvas.configure_text screen t;
       img, t);
      (let img = draw_image screen "./blocks/sblocks/stone.png"  (1920-210+100)  (200*n + 40 + (60*n-60)) in
       let t = Canvas.create_text ~x:(1920-210+100) ~y:(200*n + 44 + (60*n-60)) ~font:"Helvetica 28 bold" ~text:(Int.to_string rs.Catan.stone) screen in
       Canvas.configure_text screen t;
       img, t);
      (let img = draw_image screen "./blocks/sblocks/wheat.png"  (1920-210+140)  (200*n + 40 + (60*n-60)) in
       let t = Canvas.create_text ~x:(1920-210+140) ~y:(200*n + 44 + (60*n-60)) ~font:"Helvetica 28 bold" ~text:(Int.to_string rs.Catan.wheat) screen in
       Canvas.configure_text screen t;
       img, t);
      (let img = draw_image screen "./blocks/sblocks/sheep.png"  (1920-210+180)  (200*n + 40 + (60*n-60)) in
       let t = Canvas.create_text ~x:(1920-210+180) ~y:(200*n + 44 + (60*n-60)) ~font:"Helvetica 28 bold" ~text:(Int.to_string rs.Catan.sheep) screen in
       Canvas.configure_text screen t;
       img, t)
    ] in
    let settlements = player.Catan.settlements in
    let settlements_gui =
      (let img = draw_image screen ("./buildings/player" ^ (Int.to_string @@ Catan.color2number player.Catan.color) ^ "village.png")  (1920-250)  (60 + 250*n - 250) in
       let t = Canvas.create_text ~x:(1920-250) ~y:(65 + 250*n - 250) ~font:"Helvetica 28 bold" ~text:(Int.to_string @@ List.length settlements) screen in
       Canvas.configure_text screen t;
       img, t)
    in
    {
      avatar = avatar;
      resources = new_rs;
      settlements = settlements_gui;
    }
  let draw_info screen color =
    draw_image screen ("./players/player" ^ Int.to_string (Catan.color2number color) ^ "c.png") 340 74

  let render screen gameState =
    let background = draw_background screen in
    let fields     = draw_board      screen gameState.Catan.board  in
    let numbers    = draw_numbers    screen gameState.Catan.board  in
    let buildings  = draw_building   screen gameState.Catan.towns  in
    let murzyn     = draw_murzyn     screen gameState.Catan.murzyn in
    let dices      = draw_dices      screen gameState.Catan.dices in
    let player1    = draw_player     screen @@ List.nth gameState.Catan.players 0 in
    let player2    = draw_player     screen @@ List.nth gameState.Catan.players 1 in
    let curr_pl    = draw_info       screen gameState.Catan.tour in
    let _ = draw_image screen "./control/trans_bob.png" 74 1006 in
    pack [screen];
    {
      background = background;
      murzyn     = murzyn;
      villages   = buildings;
      numbers    = numbers;
      fields     = fields;
      roads      = [];
      dices      = dices;
      players    = [player1; player2];
      current_pl = curr_pl;
    }
  let clear_screen screen gameObjects =
    Canvas.delete screen [gameObjects.background];
    Canvas.delete screen gameObjects.villages;
    Canvas.delete screen gameObjects.numbers;
    Canvas.delete screen gameObjects.fields;
    Canvas.delete screen gameObjects.roads;
    List.iter (fun p ->
      Canvas.delete screen [p.avatar];
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
    enable_build_village screen gameState gameObjects;
    (* Budowa drogi *)
    (* Kupno karty niedorozwoju *)
    (* Wykorzystanie karty niedorozwoju *)
    (* Handel *)
    (* Przycisk końca tury *)
    enable_dice_roll screen gameState gameObjects
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
  and enable_build_village screen gameState gameObjects =
    if Catan.can_build_village gameState
      then begin
        let obj = Textures.draw_image screen "./control/bob.png" 74 1006 in
        Canvas.bind ~events:[`ButtonPress]
          ~action:(fun _ ->
            Canvas.delete screen [obj];
            choose_position screen gameState gameObjects
            (* Textures.refresh screen gameState gameObjects *)
          ) screen obj
      end
  and choose_position screen gameState gameObjects =
    let objLst = List.filter_map (fun n ->
      let coords = List.nth Textures.settlements_coords n in
      let e = List.nth gameState.Catan.towns n in
      match e with
      | Catan.Empty _ -> Some ((Textures.draw_image screen "./control/button.png" (fst coords) (snd coords)), n)
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

  let rec starting_buildings screen gameState gameObjects hm =
    let objLst = List.filter_map (fun n ->
      let coords = List.nth Textures.settlements_coords n in
      let e = List.nth gameState.Catan.towns n in
      match e with
      | Catan.Empty _ -> Some ((Textures.draw_image screen "./control/button.png" (fst coords) (snd coords)), n)
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
              if hm > 1
                then begin
                  Catan.next_player gameState;
                  starting_buildings screen gameState gameObjects (hm - 1)
                end
                else begin
                  Catan.first_player gameState;
                  enable_dice_roll screen gameState gameObjects
                end
          | _ -> failwith "Ooops! Shouldn't get here... [err7]\n"
        ) screen obj
    ) objLst

  let start_game screen numberOfPlayers =
    (* Główny obiekt trzymający stan gry *)
    let gameState   = Catan.gameInit () in
    (* Główny obiekt trzymający wszystkie narysowane obiekty (tagOrId) *)
    let gameObjects = Textures.render screen gameState in
    (* Korzeń drzwa eventów *)
    starting_buildings screen gameState gameObjects (numberOfPlayers*2)
end

let catan =
  let top = openTk () in
  Wm.title_set top "CATAN";
  let mainCanvas = Canvas.create ~width:1920 ~height:1080 top in
  Control.start_game mainCanvas 2;

  mainLoop () ;;
