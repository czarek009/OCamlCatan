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
  type color =
    | Green
    | Red
  type resources =
    | Wood
    | Clay
    | Stone
    | Wheat
    | Sheep
  type field =
    | Wood  of int
    | Clay  of int
    | Stone of int
    | Wheat of int
    | Sheep of int
    | Desert
  type settlement =
    | Village of field list
    | Town    of field list
    | None    of field list
  (* Game state *)
  type t = {
    mutable board  : field list;
    mutable towns  : settlement list;
    mutable roads  : unit;
    mutable murzyn : int;
    mutable dices  : int;
  }

  let nof_settlements = 54
  let numbers = [5;2;6;3;8;10;9;12;11;4;8;10;9;4;5;6;3;11]
  let fields = Utils.shuffle ['D';'D';'D';'D';'G';'G';'G';'S';'S';'S';'S';'K';'K';'K';'O';'O';'O';'O';'P']

  let init_settlements board =
    (*  1 *) List.append [None [List.nth board 0]] @@
    (*  2 *) List.append [None [List.nth board 0]] @@
    (*  3 *) List.append [None [List.nth board 0 ; List.nth board 1]] @@
    (*  4 *) List.append [None [List.nth board 1]] @@
    (*  5 *) List.append [None [List.nth board 1 ; List.nth board 2]] @@
    (*  6 *) List.append [None [List.nth board 2]] @@
    (*  7 *) List.append [None [List.nth board 2]] @@

    (*  8 *) List.append [None [List.nth board 3]] @@
    (*  9 *) List.append [None [List.nth board 0 ; List.nth board 3]] @@
    (* 10 *) List.append [None [List.nth board 0 ; List.nth board 3 ; List.nth board 4]] @@
    (* 11 *) List.append [None [List.nth board 0 ; List.nth board 1 ; List.nth board 4]] @@
    (* 12 *) List.append [None [List.nth board 1 ; List.nth board 4 ; List.nth board 5]] @@
    (* 13 *) List.append [None [List.nth board 1 ; List.nth board 2 ; List.nth board 5]] @@
    (* 14 *) List.append [None [List.nth board 2 ; List.nth board 5 ; List.nth board 6]] @@
    (* 15 *) List.append [None [List.nth board 2 ; List.nth board 6]] @@
    (* 16 *) List.append [None [List.nth board 6]] @@

    (* 17 *) List.append [None [List.nth board 7]] @@
    (* 18 *) List.append [None [List.nth board 3 ; List.nth board 7]] @@
    (* 19 *) List.append [None [List.nth board 3 ; List.nth board 7 ; List.nth board 8]] @@
    (* 20 *) List.append [None [List.nth board 3 ; List.nth board 4 ; List.nth board 8]] @@
    (* 21 *) List.append [None [List.nth board 4 ; List.nth board 8 ; List.nth board 9]] @@
    (* 22 *) List.append [None [List.nth board 4 ; List.nth board 5 ; List.nth board 9]] @@
    (* 23 *) List.append [None [List.nth board 5 ; List.nth board 9 ; List.nth board 10]] @@
    (* 24 *) List.append [None [List.nth board 5 ; List.nth board 6 ; List.nth board 10]] @@
    (* 25 *) List.append [None [List.nth board 6 ; List.nth board 10 ; List.nth board 11]] @@
    (* 26 *) List.append [None [List.nth board 6 ; List.nth board 11]] @@
    (* 27 *) List.append [None [List.nth board 11]] @@

    (* 28 *) List.append [None [List.nth board 7]] @@
    (* 29 *) List.append [None [List.nth board 7 ; List.nth board 12]] @@
    (* 30 *) List.append [None [List.nth board 7 ; List.nth board 8; List.nth board 12]] @@
    (* 31 *) List.append [None [List.nth board 8 ; List.nth board 12 ; List.nth board 13]] @@
    (* 32 *) List.append [None [List.nth board 8 ; List.nth board 9 ; List.nth board 13]] @@
    (* 33 *) List.append [None [List.nth board 9 ; List.nth board 13 ; List.nth board 14]] @@
    (* 34 *) List.append [None [List.nth board 9 ; List.nth board 10 ; List.nth board 14]] @@
    (* 35 *) List.append [None [List.nth board 10 ; List.nth board 14 ; List.nth board 15]] @@
    (* 36 *) List.append [None [List.nth board 10 ; List.nth board 11 ; List.nth board 15]] @@
    (* 37 *) List.append [None [List.nth board 11 ; List.nth board 15]] @@
    (* 38 *) List.append [None [List.nth board 11]] @@

    (* 39 *) List.append [None [List.nth board 12]] @@
    (* 40 *) List.append [None [List.nth board 12 ; List.nth board 16]] @@
    (* 41 *) List.append [None [List.nth board 12 ; List.nth board 13 ; List.nth board 16]] @@
    (* 42 *) List.append [None [List.nth board 13 ; List.nth board 16 ; List.nth board 17]] @@
    (* 43 *) List.append [None [List.nth board 13 ; List.nth board 14 ; List.nth board 17]] @@
    (* 44 *) List.append [None [List.nth board 14 ; List.nth board 17 ; List.nth board 18]] @@
    (* 45 *) List.append [None [List.nth board 14 ; List.nth board 15 ; List.nth board 18]] @@
    (* 46 *) List.append [None [List.nth board 15 ; List.nth board 18]] @@
    (* 47 *) List.append [None [List.nth board 15]] @@

    (* 48 *) List.append [None [List.nth board 16]] @@
    (* 49 *) List.append [None [List.nth board 16]] @@
    (* 50 *) List.append [None [List.nth board 16 ; List.nth board 17]] @@
    (* 51 *) List.append [None [List.nth board 17]] @@
    (* 52 *) List.append [None [List.nth board 17 ; List.nth board 18]] @@
    (* 53 *) List.append [None [List.nth board 18]] @@
    (* 54 *) List.append [None [List.nth board 18]] []
  let generate_board () =
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
  let get_desert_idx board =
    let rec _aux n board =
      match List.nth board 0 with
        | Desert -> n
        | _ -> _aux (n+1) @@ List.tl board
    in _aux 0 board
  let build_village (gameState : t) (idx : int) (builder : color) =
    let place = List.nth gameState.towns idx in
    match place with
      | None x ->
          let settlements = gameState.towns in
          gameState.towns <- Utils.replace settlements idx (Village x)
      | _ -> failwith "Ooops! You can build village only on empty field!\n"

  let gameInit () =
    let board = generate_board () in {
      board  = board;
      towns  = init_settlements board;
      roads  = ();
      murzyn = get_desert_idx board;
      dices  = 0;
    }

end

module Player = struct
  type color =
    | Green
    | Red
  type t = {
    settlements : int list;
    resources   : Catan.resources list;
    points      : int;
    roads       : unit;
    color       : color;
  }
end

module Textures = struct
  type t = {
    mutable background : tagOrId;
    mutable murzyn     : tagOrId;
    mutable villages   : tagOrId list;
    mutable numbers    : tagOrId list;
    mutable fields     : tagOrId list;
    mutable roads      : tagOrId list;
    mutable dices      : tagOrId * tagOrId;
  }
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
    (xoffset + 90*2 - 180*2), (yoffset - 155*2 - 100);
    (xoffset + 90*2 - 180*1), (yoffset - 155*2 - 100);
    (xoffset + 90*2 - 180*0), (yoffset - 155*2 - 100);
    (xoffset + 90*2 - 180*2 - 89), (yoffset - 155*2 - 55);
    (xoffset + 90*2 - 180*2 + 89), (yoffset - 155*2 - 55);
    (xoffset + 90*2 - 180*1 + 89), (yoffset - 155*2 - 55);
    (xoffset + 90*2 - 180*0 + 89), (yoffset - 155*2 - 55);

    (xoffset + 90*1 - 180*2), (yoffset - 155*1 - 100);
    (xoffset + 90*1 - 180*1), (yoffset - 155*1 - 100);
    (xoffset + 90*1 - 180*0), (yoffset - 155*1 - 100);
    (xoffset + 90*1 + 180*1), (yoffset - 155*1 - 100);
    (xoffset + 90*1 - 180*2 - 89), (yoffset - 155*1 - 55);
    (xoffset + 90*1 - 180*2 + 89), (yoffset - 155*1 - 55);
    (xoffset + 90*1 - 180*1 + 89), (yoffset - 155*1 - 55);
    (xoffset + 90*1 - 180*0 + 89), (yoffset - 155*1 - 55);
    (xoffset + 90*1 + 180*1 + 89), (yoffset - 155*1 - 55);

    (xoffset + 90*1 - 180*3), (yoffset - 155*1 + 100);
    (xoffset + 90*1 - 180*2), (yoffset - 155*1 + 100);
    (xoffset + 90*1 - 180*1), (yoffset - 155*1 + 100);
    (xoffset + 90*1 - 180*0), (yoffset - 155*1 + 100);
    (xoffset + 90*1 + 180*1), (yoffset - 155*1 + 100);
    (xoffset + 90*1 + 180*2), (yoffset - 155*1 + 100);
    (xoffset + 90*1 - 180*2 - 89), (yoffset - 155*1 + 55);
    (xoffset + 90*1 - 180*2 + 89), (yoffset - 155*1 + 55);
    (xoffset + 90*1 - 180*1 + 89), (yoffset - 155*1 + 55);
    (xoffset + 90*1 - 180*0 + 89), (yoffset - 155*1 + 55);
    (xoffset + 90*1 + 180*1 + 89), (yoffset - 155*1 + 55);

    (xoffset + 90*1 - 180*3), (yoffset + 155*1 - 100);
    (xoffset + 90*1 - 180*2), (yoffset + 155*1 - 100);
    (xoffset + 90*1 - 180*1), (yoffset + 155*1 - 100);
    (xoffset + 90*1 - 180*0), (yoffset + 155*1 - 100);
    (xoffset + 90*1 + 180*1), (yoffset + 155*1 - 100);
    (xoffset + 90*1 + 180*2), (yoffset + 155*1 - 100);
    (xoffset + 90*1 - 180*2 - 89), (yoffset + 155*1 - 55);
    (xoffset + 90*1 - 180*2 + 89), (yoffset + 155*1 - 55);
    (xoffset + 90*1 - 180*1 + 89), (yoffset + 155*1 - 55);
    (xoffset + 90*1 - 180*0 + 89), (yoffset + 155*1 - 55);
    (xoffset + 90*1 + 180*1 + 89), (yoffset + 155*1 - 55);

    (xoffset + 90*1 - 180*2), (yoffset + 155*1 + 100);
    (xoffset + 90*1 - 180*1), (yoffset + 155*1 + 100);
    (xoffset + 90*1 - 180*0), (yoffset + 155*1 + 100);
    (xoffset + 90*1 + 180*1), (yoffset + 155*1 + 100);
    (xoffset + 90*1 - 180*2 - 89), (yoffset + 155*1 + 55);
    (xoffset + 90*1 - 180*2 + 89), (yoffset + 155*1 + 55);
    (xoffset + 90*1 - 180*1 + 89), (yoffset + 155*1 + 55);
    (xoffset + 90*1 - 180*0 + 89), (yoffset + 155*1 + 55);
    (xoffset + 90*1 + 180*1 + 89), (yoffset + 155*1 + 55);

    (xoffset + 90*2 - 180*2), (yoffset + 155*2 + 100);
    (xoffset + 90*2 - 180*1), (yoffset + 155*2 + 100);
    (xoffset + 90*2 - 180*0), (yoffset + 155*2 + 100);
    (xoffset + 90*2 - 180*2 - 89), (yoffset + 155*2 + 55);
    (xoffset + 90*2 - 180*2 + 89), (yoffset + 155*2 + 55);
    (xoffset + 90*2 - 180*1 + 89), (yoffset + 155*2 + 55);
    (xoffset + 90*2 - 180*0 + 89), (yoffset + 155*2 + 55);
  ]

  let image_load screen img_path x y =
    let img_canvas = Canvas.create_image screen ~x:x ~y:y ~anchor:`Center in
    let img = Imagephoto.create () in
    Imagephoto.configure img ~file:img_path;
    Canvas.configure_image screen img_canvas ~image:img;
    img_canvas
  let draw_background screen =
    image_load screen "./imgs/background.png" xoffset yoffset
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
      then draw (n-1) @@ image_load screen (field2img @@ List.nth board n) (fst @@ List.nth field_coords n) (snd @@ List.nth field_coords n) ::lst
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
        | Catan.Sheep i -> draw (n-1) @@ image_load screen ("./numbers/" ^ Int.to_string i ^ ".png") (fst @@ List.nth field_coords n) (snd @@ List.nth field_coords n) ::lst
        | Catan.Desert  -> draw (n-1) lst
      end
      else lst
    in draw 18 []
  let draw_building screen buildings =
    List.map2 (fun (x, y) b ->
      (* image_load screen "./control/trans_butt.png" x y *)
      match b with
      | Catan.None _ -> image_load screen "./buildings/empty.png" x y
      | _            -> image_load screen "./buildings/green_village.png" x y
    ) settlements_coords buildings
  let draw_murzyn screen n =
    image_load screen "./blocks/murzyn.png" (fst @@ List.nth field_coords n) (snd @@ List.nth field_coords n)
  let move_murzyn screen n objLst =
    Canvas.delete screen [objLst.murzyn];
    draw_murzyn screen n
  let draw_dices screen =
    let d1 = image_load screen "./dice/d0.png" 74 74 in
    let d2 = image_load screen "./dice/d0.png" 222 74 in
    d1, d2
  let render screen gameState =
    let background = draw_background screen in
    let fields     = draw_board      screen gameState.Catan.board  in
    let numbers    = draw_numbers    screen gameState.Catan.board  in
    let buildings  = draw_building   screen gameState.Catan.towns  in
    let murzyn     = draw_murzyn     screen gameState.Catan.murzyn in
    let dices      = draw_dices      screen in
    pack [screen];
    {
      background = background;
      murzyn     = murzyn;
      villages   = buildings;
      numbers    = numbers;
      fields     = fields;
      roads      = [];
      dices      = dices;
    }
  let clear_screen screen objects =
    Canvas.delete screen [objects.background];
    Canvas.delete screen objects.villages;
    Canvas.delete screen objects.numbers;
    Canvas.delete screen objects.fields;
    Canvas.delete screen objects.roads
  let refresh screen gameState renderedObjects =
    clear_screen screen renderedObjects;
    let newObj = render screen gameState in
    renderedObjects.villages <- newObj.villages
end

module Control = struct
  let order = [Player.Green; Player.Red]

  let enable_move_murzyn screen gameState gameObjects =
    let fields = gameObjects.Textures.fields in
    let objLst = List.map (fun e ->
      let lst = Canvas.coords_get screen e in
      Textures.image_load screen "./control/trans_butt.png" (Int.of_float @@ List.nth lst 0) (Int.of_float @@ List.nth lst 1)
    ) fields
    in List.iter (fun n ->
      let obj = List.nth objLst n in
      Canvas.bind ~events:[`ButtonPress] ~extend:true ~fields:[`MouseX; `MouseY]
        ~action:(fun e ->
          (* print_string @@ "Murzyn na polu: " ^ Int.to_string n ^ "\n"; *)
          gameState.Catan.murzyn <- n;
          let lst = Canvas.coords_get screen obj in
          Canvas.coords_set screen gameObjects.Textures.murzyn ~xys:[(Int.of_float @@ List.nth lst 0), (Int.of_float @@ List.nth lst 1)];
          Canvas.delete screen objLst
        )
        screen
        obj
    )
    (Utils.range 19)
  let rec enable_build_town screen gameState gameObjects =
    let buildings = gameObjects.Textures.villages in
    let objLst = List.map (fun e ->
      let lst = Canvas.coords_get screen e in
      Textures.image_load screen "./control/button.png" (Int.of_float @@ List.nth lst 0) (Int.of_float @@ List.nth lst 1)
    ) buildings
    in List.iter (fun n ->
      let obj = List.nth objLst n in
      Canvas.bind ~events:[`ButtonPress] ~fields:[`MouseX; `MouseY]
        ~action:(fun e ->
          let lst = gameState.Catan.towns in
          match List.nth lst n with
            | None xs ->
                gameState.towns <- Utils.replace lst n (Catan.Town xs);
                Canvas.delete screen objLst;
                Textures.refresh screen gameState gameObjects
            | _ -> enable_build_town screen gameState gameObjects;
        )
        screen
        obj
    )
    (Utils.range 54)
  let rec enable_dice_roll screen gameState gameObjects =
    let d1, d2 = gameObjects.Textures.dices in
    Canvas.delete screen [d1; d2];
    let d1 = Textures.image_load screen "./dice/d0.png" 74 74 in
    let d2 = Textures.image_load screen "./dice/d0.png" 222 74 in
    Canvas.bind ~events:[`ButtonPress]
      ~action:(fun e ->
        print_string "Rolling... ";
        Canvas.delete screen [d1; d2];
        let n1, n2 = Utils.droll () in
        print_string @@ Int.to_string (n1+n2) ^ "\n";
        let d1 = Textures.image_load screen ("./dice/d" ^ Int.to_string n1 ^ ".png") 74 74 in
        let d2 = Textures.image_load screen ("./dice/d" ^ Int.to_string n2 ^ ".png") 222 74 in
        gameState.Catan.dices <- n1+n2;
        gameObjects.Textures.dices <- d1, d2
      ) screen d1

  let rec build_starting_villages screen gameState gameObjects hm =
    let objLst = List.filter_map (fun n ->
      let coords = List.nth Textures.settlements_coords n in
      let e = List.nth gameState.Catan.towns n in
      match e with
      | Catan.None _ -> Some ((Textures.image_load screen "./control/button.png" (fst coords) (snd coords)), n)
      | _ -> None
    ) @@ Utils.range Catan.nof_settlements
    in List.iter (fun (obj, idx) ->
      Canvas.bind ~events:[`ButtonPress]
        ~action:(fun _ ->
          print_string @@ "Building village at position " ^ Int.to_string idx ^ "\n";
          let e = List.nth gameState.Catan.towns idx in
          match e with
          | Catan.None xs ->
              gameState.Catan.towns <- Utils.replace gameState.Catan.towns idx (Catan.Village xs);
              Canvas.delete screen @@ List.map (fun e -> fst e) objLst;
              Textures.refresh screen gameState gameObjects;
              if hm > 1
                then build_starting_villages screen gameState gameObjects (hm - 1)
                else enable_dice_roll screen gameState gameObjects
          | _ -> failwith "Ooops! Shouldn't get here... [err7]\n"
        ) screen obj
    ) objLst

  let start_game screen numberOfPlayers =
    (* Główny obiekt trzymający stan gry *)
    let gameState   = Catan.gameInit () in
    (* Główny obiekt trzymający wszystkie narysowane obiekty (tagOrId) *)
    let gameObjects = Textures.render screen gameState in
    (* Korzeń drzwa eventów *)
    build_starting_villages screen gameState gameObjects (numberOfPlayers*2)
end

let catan =
  let top = openTk () in
  Wm.title_set top "CATAN";
  let mainCanvas = Canvas.create ~width:1920 ~height:1080 top in
  Control.start_game mainCanvas 1;

  mainLoop () ;;