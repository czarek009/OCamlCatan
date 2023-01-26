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

end

module Player = struct
end

module Catan = struct
  (* Field types *)
  type field =
    | Wood  of int
    | Clay  of int
    | Stone of int
    | Wheat of int
    | Sheep of int
    | Desert
  (* Game state *)
  type t = {
    mutable board  : field list;
    mutable towns  : unit;
    mutable roads  : unit;
    mutable murzyn : int;
  }

  let numbers = [5;2;6;3;8;10;9;12;11;4;8;10;9;4;5;6;3;11]
  let fields = Utils.shuffle ['D';'D';'D';'D';'G';'G';'G';'S';'S';'S';'S';'K';'K';'K';'O';'O';'O';'O';'P']

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
  let gameInit () =
    let board = generate_board () in {
      board  = board;
      towns  = ();
      roads  = ();
      murzyn = get_desert_idx board;
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
  let town_coords = [
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
    List.map (fun (x, y) ->
      image_load screen "./control/trans_butt.png" x y
    ) town_coords
  let draw_murzyn screen n =
    image_load screen "./blocks/murzyn.png" (fst @@ List.nth field_coords n) (snd @@ List.nth field_coords n)
  let move_murzyn screen n objLst =
    Canvas.delete screen [objLst.murzyn];
    draw_murzyn screen n

  let render screen gameState =
    let background = draw_background screen in
    let fields = draw_board screen gameState.Catan.board in
    let numbers = draw_numbers screen gameState.Catan.board in
    let villages = draw_building screen [] in
    let murzyn = draw_murzyn screen gameState.Catan.murzyn in
    pack [screen];
    {
      background = background;
      murzyn     = murzyn;
      villages   = villages;
      numbers    = numbers;
      fields     = fields;
      roads      = [];
    }
  let clear_screen screen objects =
    Canvas.delete screen [objects.background];
    Canvas.delete screen objects.villages;
    Canvas.delete screen objects.numbers;
    Canvas.delete screen objects.fields;
    Canvas.delete screen objects.roads

end

module Control = struct

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
  let enable_build_town screen gameState gameObjects =
    let towns = gameObjects.Textures.villages in
    failwith "not yet implemented"
end

let catan =
  let top = openTk () in
  Wm.title_set top "CATAN";

  let mainCanvas = Canvas.create ~width:1920 ~height:1080 top in
  let game = Catan.gameInit () in
  let renderedObjects = Textures.render mainCanvas game in
  (* Control.enable_move_murzyn mainCanvas game renderedObjects; *)

  mainLoop () ;;