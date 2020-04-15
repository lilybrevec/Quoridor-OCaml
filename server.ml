open Universe
open Const

(* ４人全員playing(一番始めのpickが終わった状態)ならばtrue *)
let start = ref false

(* serverが持つ宇宙の状態: idとしてのiworld_t とclientそれぞれのworldのリスト *)
type state_t = (iworld_t * world_t) list

(* 初期のstate_tの値 *)
let init_state : state_t = []

(* onnew : ('a * 'b) list -> 'b -> ('a * 'b list) * 'b list * 'a list *)
(* クライアントが通信に参加したら state にその iworld を加え全員にその旨のメッセージを送る *)
let onnew state iworld =
  let new_state = (iworld, make_initial_message (List.length state + 1)) :: state in
  if List.length state < 3 then
    Bundle(new_state, new_state, [])
  else if List.length state = 3 then
    begin(
      start := true;
      Bundle(new_state, new_state, []) 
    )end
  else State state

(* change_world_pl : world_t -> world_t -> world_t *)
(* 変更前のstate中のそれぞれのworldのposnlstとwall_numberとjudgeとwall_numberを、変更が生じたworldの値に書き換える *)
let change_world_pl world sexp = 
  let new_posn =  if abs (world.my_id - sexp.my_id) = 2 then 
      (8 - fst sexp.posn, 8 - snd sexp.posn)
    else if (world.my_id - sexp.my_id = 3) || (world.my_id - sexp.my_id = -1) then 
      (snd sexp.posn, 8 - fst sexp.posn)
    else if (world.my_id - sexp.my_id = -3) || (world.my_id - sexp.my_id = 1) then
      (8 - snd sexp.posn, fst sexp.posn)
    else sexp.posn in 
  let new_lst = List.map (fun (i, (x, y)) -> if i = sexp.my_id then (i, new_posn) else (i, (x, y))) world.posn_lst in
  let new_judge = if world.my_id = sexp.my_id then sexp.judgement else world.judgement in 
  let new_wall_number = if world.my_id = sexp.my_id then sexp.wall_number else world.wall_number in
  { my_id = world.my_id;
    posn = world.posn;
    choice_posn = world.choice_posn;
    posn_lst = new_lst;
    wall_posn_lst = world.wall_posn_lst;
    wall_number = new_wall_number;
    judgement = new_judge }

(* change_turn : world_t -> string -> world_t *)  
(* 次の番のプレイヤーのjudgementを変える *)
let change_turn world newjudge = match world with
    { my_id = id; posn = p; choice_posn = c_p; posn_lst = p_l; wall_posn_lst = w_l;
      wall_number = w_n; judgement = j}
    ->  { my_id = id; posn = p; choice_posn = c_p; posn_lst = p_l; wall_posn_lst = w_l;
          wall_number = w_n; judgement = newjudge}
        
(* wall_rotation : state_t -> world_t -> state_t *)
(* wall_posn_lstをstateのすべてのworldでそのworldに合わせた回転をして、更新する *)
let wall_rotation state sexp =
  if snd sexp.choice_posn = Piece then state
  else 
    List.map (fun (iw, world) -> match world with
          { my_id = id; posn = p; choice_posn = c_p; posn_lst = p_l; wall_posn_lst = w_l;
            wall_number = w_n; judgement = j} ->
          let sexp_w =  List.hd (List.rev sexp.wall_posn_lst) in
          let new_w =  match sexp_w with ((x, y), d) ->
            if id = sexp.my_id then sexp_w
            else if abs (id - sexp.my_id) = 2 then 
              (if d = Horizontal then ((8 - (x + 1), 8 - (y - 1)), d) else ((8 - (x - 1), 8 - (y + 1)), d))
            else if (id - sexp.my_id = -3) || (id - sexp.my_id = 1) then
              (if d = Horizontal then ((8 - (y - 1), x), Virtical) else ((8 - (y + 1), x), Horizontal))
            else if (id - sexp.my_id = 3) || (id - sexp.my_id = -1) then
              (if d = Horizontal then ((y, 8 - (x + 1)), Virtical) else ((y, 8 - (x - 1)), Horizontal))
            else sexp_w in
          let new_cp = ((p, Virtical),Piece) in
          (iw, { my_id = id; posn = p; choice_posn = new_cp; posn_lst = p_l; wall_posn_lst =  w_l @ [new_w]; wall_number = w_n; judgement = j})
      ) state

(* judge : state_t -> world_t -> state_t *)
(* プレイヤーがゴールについたらworldのjudgement をwinかloseに変える *)
let judge state sexp =
  if (snd sexp.posn <= 0) then
    List.map (fun (iw, world) -> if (world.my_id = sexp.my_id) then (iw, change_turn world Win)
               else (iw, change_turn world Lose)) state
  else state

(* on_msg : state_t -> iworld -> world_t -> state_t *)
(* clientから変更のmessageが来たら、それをもとにstateを書き換え、clientそれぞれに送信する*)
(* sexp は iworld からきたメッセージ。つまり今回変更をしようとしている世界 *)
let on_msg state iworld sexp =
  if !start (*4人全員参加したら*) then
    let new_state  = List.map (fun (iw, world) -> (iw, change_world_pl world sexp)) state in
    let change_turned_state = List.map (fun (iw, world) -> if world.my_id = (sexp.my_id  mod 4 + 1) then (iw, change_turn world In_turn) else (iw, change_turn world Not_turn)) new_state in
    let judge_state = judge change_turned_state sexp in
    let wall_new_state = wall_rotation judge_state sexp in
    Bundle (wall_new_state, wall_new_state,[])
  else State state

(* change_on_disconnect : state_t -> Unix.file_descr ->
                          (state_t, world_t) Universe.t *)
(* 通信が途切れたclientに対応する *)
let change_on_disconnect state world_id =
  let new_state = List.filter (fun (id, clist) -> id <> world_id) state in 
  Bundle (new_state, [], [world_id])

(* 宇宙の始まり *)
(* server start *)
let _ = universe init_state
    ~on_new:onnew
    ~on_msg:on_msg
    ~on_disconnect:change_on_disconnect
    ~rate:0.1
