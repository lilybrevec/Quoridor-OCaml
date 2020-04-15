open Color
open Image
open World
open Const

(* ターン数を保存するcounter *)
let counter = ref 1

(* 様々な画像を表示する関数 *)
(* draw : world_t -> Image.t *)
let draw world =
  let player_images_lst = [player1_image;
                           player2_image;
                           player3_image;
                           player4_image;] in
  (* player画像listをid順に並び替える関数 *)
  let rec make_player_images posn_lst list = 
    match posn_lst with
      [] -> list
    | (id, w) :: rest -> make_player_images rest (list @ [List.nth player_images_lst (id - 1)])
  in 
  (* worldのposn_lstは(int, int)管理なので、それを実際のfloatの座標位置に変更 *)
  let posn_lst_c = List.map (fun (id, posn) -> mass_to_c posn) world.posn_lst in
  
  (* 壁情報を表示する *)
  (* put_wall_image : a list -> Image_t -> Image_t *)
  let put_wall_image wl c = List.fold_right (fun ((x, y), dir) c -> if dir = Virtical then place_image v_wall_image (mass_to_c(x, y)) c else place_image h_wall_image (mass_to_c(x, y)) c) wl c in
  
  (* 選択中の壁表示を行う、modeの状態を表示する *)
  let choice_wall cp j c = match cp with ((posn, dir), mode) ->
                      if j = In_turn then 
                        if mode = Wall then
                          if dir = Virtical then 
                            place_image wall_choice_image (300., 795.) (place_image v_wall_image (mass_to_c posn) c) 
                          else place_image wall_choice_image (300., 795.) (place_image h_wall_image (mass_to_c posn) c) 
                        else place_image piece_choice_image (300., 795.) c 
                      else if mode = Wall then
                        if dir = Virtical then place_image v_wall_image (mass_to_c posn) c
                        else place_image h_wall_image (mass_to_c posn) c 
                      else c in
  
  (* judge_made : judgement -> Image_t -> Image_t *)
  (* judgeによる状態を下部に表示する *)
  let judge_made j c = 
    let j_image = match j with
        Stop -> wait_image
      | In_turn -> my_turn_image
      | Not_turn -> not_turn_image
      | Miss_piece -> miss_piece
      | Miss_wall -> miss_wall
      | Miss_up -> miss_up
      | Miss_down -> miss_up
      | Miss_left -> miss_left
      | Miss_right -> miss_left
      | _ -> c in
    let ichi = match j with
        Miss_up -> 700.
      | Miss_down -> 700.
      | Miss_left -> 700.
      | Miss_right -> 700.
      | _ -> 800. in
    place_image j_image (50., ichi) c
  in

  (* add_wall_num_image: int -> Image_t -> Image_t *)
  (* 壁の数を右下に表示する *)
  let add_wall_num_image j c =
    if j = 0 then  place_image ((text "you can't use wall" ~size:25. (make_color 72 61 139)))
	(500., 725.) c
    else place_image ((text ("wall: " ^ (string_of_int j)) ~size:25. (make_color 72 61 139)))
	(600., 725.) c in
  (* ターン数の表示 *)
  let add_turn_num_image c = place_image ((text ("turn: " ^ (string_of_int (!counter - 2))) ~size:25. (make_color 72 61 139))) (200., 725.) c in
  
  (* 右の表示系について *)
  let player_list = List.map (fun (x, y) -> if ((!counter + 2) mod 4) = int_of_float (y /. 50.) then (x -. 30., y) else (x, y)) [(750., 50.); (750., 100.); (750., 150.); (750., 200.);] in
  let right_lst  =  List.map(fun (x, y) -> (x +. 60., y)) player_list in
  let direction_lst = [up_image;
                       left_image;
                       down_image;
                       right_image;] in
  let make_player_direction id list = [List.nth list ((5 - world.my_id) mod 4)] @ [List.nth list ((6 - world.my_id) mod 4)] @ [List.nth list ((7 - world.my_id) mod 4)] @ [List.nth list ((4 - world.my_id) mod 4)] in
  
  (* draw関数本体 *)
  place_images (make_player_direction world.my_id direction_lst) right_lst 
    (place_images player_images_lst player_list
       (add_turn_num_image (add_wall_num_image world.wall_number (judge_made world.judgement 
                                                                    (choice_wall world.choice_posn world.judgement 
                                                                       (put_wall_image world.wall_posn_lst
                                                                          (place_images (make_player_images world.posn_lst [])
                                                                             posn_lst_c background_image)))))))
    
(* 衝突判定し、もし駒が進めるマスだったらtrueそうでなければfalseを返す関数 *)
let rec check_collision wall_pos_lst key next_x next_y (x, y)  = 
  let collision_wall1 =
    if (key = "up") then ((next_x, next_y + 1), Horizontal)
    else if(key = "down") then ((next_x, next_y), Horizontal)
    else if (key = "left") then ((next_x + 1, next_y), Virtical)
    else if (key = "right") then ((next_x, next_y), Virtical)
    else ((x, y), Virtical) in
  let collision_wall2 =
    if (key = "up") then ((next_x - 1, next_y + 1), Horizontal)
    else if (key = "down") then ((next_x - 1, next_y), Horizontal)
    else if (key = "left") then ((next_x + 1, next_y - 1), Virtical)
    else if (key = "right") then ((next_x, next_y - 1), Virtical)
    else ((x, y - 1), Virtical) in
  List.for_all (fun v -> collision_wall1 <> v && collision_wall2 <> v) wall_pos_lst 

(* 衝突時のリアクション *)
let collision_judgement key =
  if (key = "up") then Miss_up
  else if(key = "down") then Miss_down
  else if (key = "left") then Miss_left
  else  Miss_right
let check_collision_piece  next_x next_y  p_l   = 
  if(List.length(List.filter(fun(idl, (xl, yl)) -> (xl = next_x) && (yl = next_y)) p_l) = 0)then true
  else false
    

(* 閉路になっていないかチェックする関数 Not completed *)
(*let check_closed world ((w_x, w_y), d) =
  let new_wl = world.wall_pos_lst @ [((w_x, w_y), d)] in *)
    
(* キーを受け取って、もしchoice_posnがPieceであれば駒の位置を更新し、Wallであれば壁を置く *)
(* key_draw : world_t -> string -> world_t *)
let key_draw world key =
  match world with
    {my_id = i; posn = (x,y); choice_posn = (((w_x, w_y), d), m);
     posn_lst = p_l; wall_posn_lst = w_l; wall_number = w_n;judgement = j} ->
    if (j <> Not_turn) && (j <> Start) && (j <> Stop)  then (* ターンが来たらキー操作を受け付ける *) 
      let new_mode = if (m = Wall) then Piece else Wall in
      if (key = "e") then (* もしeキーを押されたらモードを変える *)
        if w_n = 0 then World world(* wallが0のときに壁モードを使えないようにする *)
       	else  World  {my_id = i; posn = (x,y); choice_posn = (((w_x, w_y),d), new_mode);
	              posn_lst = p_l; wall_posn_lst = w_l; wall_number = w_n;judgement = j}
            
      else if (key = "s") then   (* もしsキーを押されたらserverへ情報を送る *)
        if w_n = 0 || m = Piece || (w_y = 0 && d = Horizontal) ||(w_y = 8 && d = Virtical) || (w_x = 0 && d = Virtical) then World world
        else
          (* 既にある壁の一つと新しくおこうとしている壁が衝突したらtrue、おいても良いのならfalse *)
          let check_wallposn w  ((w_x, w_y),d) = 
            if d = Virtical then (((w_x, w_y + 1), d) = w)  || (((w_x, w_y), d) = w) || (((w_x, w_y - 1), d) = w)|| (((w_x - 1, w_y + 1), Horizontal) = w)
            else  (((w_x, w_y), d) = w)  || (((w_x + 1, w_y), d) = w) || (((w_x - 1, w_y), d) = w) || (((w_x + 1, w_y - 1), Virtical) = w) in
          (* wall_posn_lstの一つでも置こうとしている駒の位置と重なるならserverに送らずやり直し *)
          if (List.exists (fun w -> (check_wallposn w  ((w_x, w_y),d))) w_l)
          then  World {my_id = i; posn = (x,y); choice_posn = (((w_x, w_y),d), m);
	        posn_lst = p_l ; wall_posn_lst = w_l ; wall_number = w_n ; judgement = Miss_wall}
	  else Package ({my_id = i; posn = (x,y); choice_posn = (((w_x, w_y),d), m);
	          posn_lst = p_l ; wall_posn_lst = w_l @ [((w_x, w_y),d)]; wall_number = w_n - 1; judgement = Not_turn},
	         {my_id = i; posn = (x,y); choice_posn = (((w_x, w_y),d), m);
           posn_lst = p_l; wall_posn_lst = w_l @ [((w_x, w_y),d)]; wall_number = w_n - 1; judgement = Not_turn})
       
      else if(key = "x") then 
        World  {my_id = i; posn = (x,y); choice_posn = (((w_x, w_y),d), m);
	        posn_lst = p_l ; wall_posn_lst = w_l ; wall_number = w_n ; judgement = In_turn}
      else if (key = "left" || key = "right" || key = "up" || key = "down") then
        if (m = Piece) then (* もし駒モードだったら駒の位置を変更してserverに情報を伝える *)
          let next_x = 
            if (key = "left") then
              if(world.judgement = In_turn)then
                if (List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x - 1) && (yl = y)) p_l) = 1) && (check_collision world.wall_posn_lst key (x-1) y (x, y))then x - 2 
                else if  List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x - 1) && (yl = y)) p_l) = 0 then x - 1
                else x 
              else if (world.judgement = Miss_up) then 
                if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x - 1) && (yl = y-1)) p_l) = 0 then x-1 
                else x
               else if  (world.judgement = Miss_down) then 
                 if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x - 1) && (yl = y+1)) p_l) = 0 then x-1 
                 else x
               else x
                 
            else if (key = "right") then 
              if(world.judgement = In_turn)then
                if (List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x + 1) && (yl = y)) p_l) = 1 ) && (check_collision world.wall_posn_lst key (x+1) y (x, y)) then x + 2 
                else if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x + 1) && (yl = y)) p_l) = 0 then x + 1
                else x
              else if (world.judgement = Miss_up) then 
                if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x + 1) && (yl = y-1)) p_l) = 0 then x + 1 
                else x 
              else if(world.judgement = Miss_down) then 
                if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x + 1) && (yl = y+1)) p_l) = 0 then x + 1 
                else x  
              else x
                
            else if (key = "down") then
                if (world.judgement = Miss_left) then
                  if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x - 1) && (yl = y + 1)) p_l) = 0 then x - 1 
                  else x
                else if (world.judgement = Miss_right) then
                  if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x + 1) && (yl = y + 1)) p_l) = 0 then x + 1 
                  else x
                else x
                  
            else if (key = "up") then
              if (world.judgement = Miss_left) then
                if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x - 1) && (yl = y - 1)) p_l) = 0 then x - 1 
                else x
              else if (world.judgement = Miss_right) then
                if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x + 1) && (yl = y - 1)) p_l) = 0 then x + 1 
                else x
              else x
            else x
          in
          let next_y =
            if (key = "up") then
              if(world.judgement = In_turn) then
                if (List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (yl = y - 1) && (xl = x)) p_l) = 1 )&&(check_collision world.wall_posn_lst key x (y-1) (x, y))then y - 2 
                else if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (yl = y - 1) && (xl = x)) p_l) = 0 then y - 1
                else y
              else if (world.judgement = Miss_left) then 
                if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x - 1) && (yl = y - 1)) p_l) = 0 then y - 1 
                else x 
              else if(world.judgement = Miss_right) then 
                if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x + 1) && (yl = y-1)) p_l) = 0 then y - 1 
                else x 
              else y
                
            else if (key = "down") then 
              if(world.judgement = In_turn) then
                if (List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (yl = y + 1) && (xl = x)) p_l) = 1)&&(check_collision world.wall_posn_lst key x (y+1) (x, y)) then y + 2 
                else if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (yl = y + 1) && (xl = x)) p_l) = 0 then y + 1
                else y
              else if (world.judgement = Miss_left) then 
                if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x - 1) && (yl = y + 1)) p_l) = 0 then y + 1 
                else y 
              else if(world.judgement = Miss_right) then 
                if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x + 1) && (yl = y + 1)) p_l) = 0 then y + 1 
                else y
            else y 
              

        else if (key = "left") then
          if (world.judgement = Miss_up) then
            if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x - 1) && (yl = y - 1)) p_l) = 0 then y - 1 
            else y
          else if (world.judgement = Miss_down) then
            if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x - 1) && (yl = y + 1)) p_l) = 0 then y + 1 
            else y
          else y
            
        else if (key = "right") then
          if (world.judgement = Miss_up) then
            if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x + 1) && (yl = y - 1)) p_l) = 0 then y - 1 
            else y
          else if (world.judgement = Miss_down) then
            if List.length(List.filter(fun(idl, (xl, yl)) -> (idl <> i) && (xl = x + 1) && (yl = y + 1)) p_l) = 0 then y + 1 
            else y
          else y
        else y
          in
          
          let new_posn_lst = List.map (fun (id, (x, y)) -> if id = i then (id, (next_x, next_y)) else (id, (x, y))) p_l in
          if (next_x >= 0 && next_y >= 0 && next_x < 9 && next_y < 9 && 
              (check_collision world.wall_posn_lst key next_x next_y (x, y)) && (check_collision_piece next_x next_y  p_l)) then
            Package ({ my_id = i; posn = (next_x, next_y);
                       choice_posn = (((w_x, w_y),d), m);
                         posn_lst = new_posn_lst; wall_posn_lst = w_l;
		       wall_number = w_n; judgement = Not_turn},
	             { my_id = i; posn = (next_x, next_y); 
		choice_posn = (((w_x, w_y),d), m); posn_lst = new_posn_lst;
		wall_posn_lst = w_l; wall_number = w_n; judgement = Not_turn})
              

          else if  (next_x >= 0 && next_y >= 0 && next_x < 9 && next_y < 9 && (check_collision_piece next_x next_y  p_l)) then 
                         if ((abs (next_x - x)) = 1) || ((abs (next_y - y)) = 1) then
                           World world       
                         else 
                           World { my_id = i; posn = (x, y); 
			           choice_posn = (((w_x, w_y),d), m); posn_lst = p_l;
			           wall_posn_lst = w_l; wall_number = w_n; judgement = (collision_judgement key)}
          
          else World world
              
        else (* もし壁モードだったら、serverには何も送らずに自分のworldに壁を表示していく *)
          let next_x = 
            if (key = "left") then w_x - 1 
	    else if (key = "right") then w_x + 1 else w_x in
          let next_y = if (key = "up") then w_y - 1
	    else if (key = "down") then w_y + 1 else w_y in
          let next_d = 
            if (key = "up" || key = "down") then 
	      (if d = Virtical then Horizontal else d)
	    else (if d = Horizontal then Virtical else d) in 
          let next_posn = (((next_x, next_y), next_d), m) in
          (* 壁をボード際に置け無いようにする判定 *)
          if (((next_d = Virtical && next_x >= 0) || (next_d = Horizontal && next_x >= 0)) && ((next_d = Virtical && next_y >= 0) || (next_d = Horizontal && next_y >= 0)) && ((next_d = Virtical && next_x < 9) || (next_d = Horizontal && next_x < 8)) && ((next_d = Virtical && next_y < 8) || (next_d = Horizontal && next_y < 9))) then
	    World  {my_id = i; posn = (x, y); choice_posn = next_posn; posn_lst = p_l; wall_posn_lst = w_l; wall_number = w_n; judgement = j}
          else World world
      else World world
    else World world
        
(* 新しいworldがserverから送られてくるので世界を変更する *)
(* receive : world_t -> message_t -> (world_t, message_t) World.t *)
let receive world message =
  let new_judge = if message.judgement = Start then In_turn
    else  message.judgement
  in
  begin(
    (counter := (!counter + 1));
    World { my_id = message.my_id;
            posn = List.assoc message.my_id message.posn_lst;
            choice_posn = message.choice_posn;
            posn_lst = message.posn_lst;
            wall_posn_lst = message.wall_posn_lst;
            wall_number = message.wall_number;
            judgement = new_judge}
  )end
(* ゲーム終了判定 *)
(* game_over : world_t -> bool *)
let game_over world =  
    (world.judgement = Win) || (world.judgement = Lose)

(* ゲーム終了時の画面表示 *)
(* draw_game_over : world_t -> Image.t *)
let draw_game_over world =
  let make_player_last_image my_id = (* num個分のイメージのリスト *)
    let win_lst = [win_image1;
                   win_image2;
                   win_image3;
                   win_image4;] 
    in
    let lose_lst = [lose_image1;
                    lose_image2;
                    lose_image3;
                    lose_image4;] 
    in
    if (world.judgement = Lose) then List.nth lose_lst (my_id - 1)
    else List.nth win_lst (my_id - 1)
  in
  place_image (make_player_last_image world.my_id ) (0., 200.) (draw world)
    
(* game start *)
let _ =
  big_bang initial_world
           ~name:"Quoridor"
           ~to_draw:draw
           ~width:(int_of_float width)
           ~height:(int_of_float height)
           ~on_key_press:key_draw
           ~rate:1.0
           ~on_receive:receive
           ~stop_when:game_over
           ~to_draw_last:draw_game_over
           
