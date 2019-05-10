
(** takes a list of tuples, each tuple being a pair of values, 
and returns the tail of the list. If the list is empty, returns an empty list*)
let select_tail l =
	match l with
	| [] -> []
	| (k,v)::tail -> tail
(** takes a list of tuples, returns the reversal of the list using @ to concatenate*)
let rec reverse_list l =
	match l with
	| [] -> []
	| (k,v)::tail -> (reverse_list tail)@[(k,v)]

 (** takes a tuple, which is a pair of values, and prints each part of the tuple, first and second element. 
 Prints an empty line after printing the values and separate them with a semicolon*)
let display_location (x,y) =
  print_endline ( string_of_int x ^ ";" ^ string_of_int y);;

(**takes a tuple and list of tuples. Then checks if the tuple exists in the list. 
Return true if it does false otherwise. Empty list is also false*)
let rec is_element (x,y) l =
	match l with
	| [] -> false
	| (k,v)::tail -> if x = k && v = y then true else is_element (x,y) tail (*to test*)

(**a function that takes an integer and a list of tuples, checks if the integer equals the first element in any of the tuples. 
Each tuple is a pair of integers, return true if you find a match or false for not or empty list*)
let rec lookupx x l = 
	match l with 
	| [] -> false
	| (k,v)::t -> if k = x then true else lookupx x t

(**a function that takes an integer and a list of tuples, checks if the integer equals the first element in any of the tuples. 
Each tuple is a pair of integers, return true if you find a match or false for not or empty list*)
let rec lookupy y l =
	match l with 
	| [] -> false
	| (k,v)::t -> if v = y then true else lookupy y t

let rec printgrid2 gridlength snake (mx,my) h i =
	if i = gridlength then 
    ()
	else if h = 0 then 
    print_string "#_ _ _ _ _ _ _ _ #"
	else begin 
    if i = 0 || i = (gridlength-1) then 
      print_string "|"  
    else if (lookupy h snake) && (h = my) && (i = mx) && (lookupx i snake) then 
      print_string "X " 
    else if is_element (i,h) snake then 
      print_string "S "
    else if (h = my) && (i = mx) then 
      print_string "M " 
    else 
      print_string "  ";
    printgrid2 gridlength snake (mx,my) h (i+1) 
  end

let rec print_lines2 gridlength snake (mx,my) h i gridheight =
	 if (h = gridheight) then print_endline "#_ _ _ _ _ _ _ _ #"
	 else (printgrid2 gridlength snake (mx,my) h i;print_endline ""; print_lines2 gridlength snake (mx,my) (h+1) i gridheight );;

let rec display_snake snake = 
	match snake with 
	| [] -> print_endline "-snake"
	| (k,v):: tail -> display_location(k, v);display_snake tail;;(*to test*)

(**calls display location with the coordinates fed in as a tuple. Then print_endline the word mouse *)
let show_mouse (x,y) = display_location(x, y);
 print_endline "-mouse";;

let display_board mouse snake wall =
	print_lines2 (wall+1) snake mouse 0 0 (wall+1);
  show_mouse mouse;
  display_snake snake;;

type direction = DIRECTION_UP | DIRECTION_DOWN | DIRECTION_RIGHT | DIRECTION_LEFT;;

(**picks the direction to go to*)
let rec get_direction () =
  let input_direction = read_line() in
    match input_direction with
      "s" -> DIRECTION_UP | "w" -> DIRECTION_DOWN | "a" -> DIRECTION_LEFT | "d" -> DIRECTION_RIGHT
    | _ -> get_direction();;

(**creates a tuple that will be the rat using 
random numbers so that it can be anywhere on the board. Add 1 to random number to avoid 0,0 values*)
let rec create_rat rat =
	 Random.self_init();
  let x = (Random.int 8)+1 and
      y = (Random.int 8)+1 in
  if is_element (x,y) rat then
    create_rat rat
  else
    (x,y);;

(**creates a tuple that will be the rat using random numbers 
so that it can be anywhere on the board. Add 1 to random number to avoid 0,0 values*)
let create_snake () =
	 Random.self_init();
  let x = (Random.int 8)+1 and
      y = (Random.int 8)+1 in
    [(x,y)];;

(**updates the tail of the snake for its new position post update, trim the end of the tail*)
let update_snake_tail = function
    [] -> [];
  | snake -> reverse_list (select_tail (reverse_list snake));;


(**checks if the rat and mouse are in the same location. If the snake is empty return false*)
let is_rat_consumed mouse snake =
  match snake with
    [] -> false
  | (head :: body) -> head = mouse;;

(**Update the state game, first call get_direction with the direction the user put in, 
perform actions if the mouse is consumed or not*)
let game_update  snake mouse =
	let direction = get_direction() in
	match is_rat_consumed mouse snake with
	| true -> 
		(match snake with 
		| [] -> (snake,mouse)
    | ((x, y) :: _) ->
		match direction with 
		| DIRECTION_UP    -> 
			let snake2 = (x,y+1) :: snake in
			let mouse2 = create_rat snake in
			(snake2,mouse2)
		|  DIRECTION_DOWN    -> 
			let snake2 = (x,y-1) :: snake in
			let mouse2 = create_rat snake in
			(snake2,mouse2)
		|  DIRECTION_LEFT    -> 
			let snake2 = (x-1,y) :: snake in
			let mouse2 = create_rat snake in
			(snake2,mouse2)
		|	DIRECTION_RIGHT   -> 
			let snake2 = (x+1,y) :: snake in
			let mouse2 = create_rat snake in
			(snake2,mouse2)
		| _ ->(snake,mouse)
			
			)

	| false ->
			(match snake with 
		| [] -> (snake,mouse)
    | ((x, y) :: _) ->
			match direction with 
		| DIRECTION_UP    -> let snake2 = (x,y+1) :: update_snake_tail(snake) in	(snake2,mouse)
		| DIRECTION_DOWN  -> let snake2 = (x,y-1) :: update_snake_tail(snake) in	(snake2,mouse)
		| DIRECTION_LEFT  -> let snake2 = (x-1,y) :: update_snake_tail(snake) in	(snake2,mouse)
		| DIRECTION_RIGHT  -> let snake2 = (x+1,y) :: update_snake_tail(snake) in	(snake2,mouse)
		| _ ->(snake,mouse)
				);;


(*if hit wall or ate itself*)
let rec hit_wall snake wall =
  match snake with
    [] -> false
  | (k,v)::tail -> if (k = 0 || k = wall || v = 0 || v = wall) then true else hit_wall tail wall

let rec consumed_itself snake =
	match snake with
	| [] -> false
	| (k,v)::tail -> if (is_element (k,v) tail) then true else consumed_itself tail


let rec call_game snake mouse wall =
	  if (hit_wall snake wall)||(consumed_itself snake) then
    ( print_endline "Well played, final score is";
      print_int (List.length snake);
      print_newline ();
    )
		else  
		( display_board mouse snake wall;
      let (snake2, mouse2) = game_update snake mouse in      
      call_game snake2 mouse2 wall ) ;;

let start () =
  let snake = create_snake() in
  let rat = create_rat snake in
	let wall = 9 in
  call_game snake rat wall ;;

start();;
