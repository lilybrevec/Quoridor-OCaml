
(* const.ml *)

open Image
open Color

(* ボード上関連画像 *)
let table_image = read_image "img/table.png" (* ボード *)
let h_wall_image = read_image "img/hWall.png" (* 横の壁 *)
let v_wall_image = read_image "img/vWall.png" (* 縦の壁 *)
let player_image = read_image "img/player.png" (* プレイヤー *)

(* ターン表示画像 *)
let wait_image = read_image "img/wait.png"
let not_turn_image = read_image "img/notturn.png"
let my_turn_image = read_image "img/turn.png"
let wall_choice_image = read_image"img/wallchoice.png"
let piece_choice_image = read_image"img/piecechoice.png" 

(* プレーヤごとの画像 *)
let player1_image = read_image "img/player1.png"
let win_image1 = read_image "img/player1_win.png"
let lose_image1 = read_image "img/player1_lose.png"

let player2_image = read_image "img/player2.png"
let win_image2 = read_image "img/player2_win.png"
let lose_image2 = read_image "img/player2_lose.png"

let player3_image = read_image "img/player3.png"
let win_image3 = read_image "img/player3_win.png"
let lose_image3 = read_image "img/player3_lose.png"

let player4_image = read_image "img/player4.png"
let win_image4 = read_image "img/player4_win.png"
let lose_image4 = read_image "img/player4_lose.png"

(* リアクション画像 *)
let miss_piece  = read_image "img/can't move.png"
let miss_wall  = read_image "img/can't put.png"
let miss_up  = read_image "img/missup.png"
let miss_left  = read_image "img/missleft.png"

(* 方向画像 *)
let up_image = read_image "img/up.png"
let down_image = read_image "img/down.png"
let right_image = read_image "img/right.png"
let left_image = read_image "img/left.png"

(* 画面の画面の横幅、縦幅 *)
let width = 900.
let height = 900.

(* １マスの長さ *)
let mass_length = 80.

(* テーブルの長さ *)
let table_size = 720.

(* 背景画像 *)
let background_image = (place_image table_image (0., 0.)
                          (rectangle width height lemonChiffon))

(* プレイヤーの人数 *)
let player_number = ref 0

(* 判定の型 *)
type judge_t = In_turn | Not_turn | Start | Stop | Win | Lose 
             | Miss_piece | Miss_wall | Miss_up | Miss_down | Miss_right | Miss_left

(* 壁の方向の型 *)
type direction_t = Horizontal | Virtical

(* 壁選択モードか駒選択モードか *)
type mode_t = Wall | Piece

(* worldの型 *)
type world_t = {
  my_id : int; (* 自分のID *)
  posn : int * int; (* 自分のマスの位置 *)
  choice_posn : (((int * int)* direction_t) * mode_t); (* 選択状態の位置とモード *)
  posn_lst : (int * (int * int))list; (* (プレーヤ番号, プレーヤのマスの位置)のリスト *)
  wall_posn_lst : ((int * int) * direction_t ) list;
  wall_number : int; (* 現在自分が持っている壁の数 *)
  judgement : judge_t; 
}

(* bigban時の初期world(すぐに更新されてしまう) *)
let initial_world =  {
    my_id = 0;
    posn = (0, 0);
    choice_posn = (((0, 0), Virtical), Piece);
    posn_lst = [];
    wall_posn_lst = [];
    wall_number = 5;
    judgement = Stop;
  }

(* clientが4人参入し終わるまでにclientに送る初期メッセージを作成 *)
let make_initial_message id =
  let p_lst = if id = 1 then [(1, (4, 8)); (2, (8, 4)); (3, (4, 0)); (4, (0, 4))]
    else if id = 2 then [(1, (0, 4)); (2, (4, 8)); (3, (8, 4)); (4, (4, 0))]
    else if id = 3 then [(1, (4, 0)); (2, (0, 4)); (3, (4, 8)); (4, (8, 4))]
    else  [(1, (8, 4)); (2, (4, 0)); (3, (0, 4)); (4, (4, 8))] in
  let judge = if id = 4 then Start else Stop in

  { my_id = id;
    posn = (4, 8);
    choice_posn = (((4, 8), Horizontal), Piece);
    posn_lst = p_lst;
    wall_posn_lst = [];
    wall_number = 5;
    judgement = judge;
  }

(* (8, 8)管理からfloat値での位置(800., 800.)を返す *)
let mass_to_c (x, y) =
  ((float_of_int x) *. mass_length, (float_of_int y) *. mass_length)
