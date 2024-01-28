type state = {
  green_chars: char option list;  (* List of characters in the right place *)
  yellow_chars: char option list; (* List of characters in the wrong place, but contained by the word*)
  grey_chars: char list;          (* List of characters absent in the word *)
  right_words: string list;       (* List of words fitting given criteria *)
  wrong_words: string list;       (* List of words with only unknown letters *)
}

(* Define a state monad type, parameterized by the result type 'a *)
type 'a state_monad = state -> 'a * state

(* Return function lifts a value into the state monad *)
let return a state = (a, state)

(* Bind function chains two state monads together *)
let bind m f state =
  let (a, new_state) = m state in
  f a new_state

(* Get the current state as the result *)
let get_state : state state_monad = fun state -> (state, state)

(* Set the state to a new value *)
let set_state new_state : unit state_monad = fun _ -> ((), new_state)

let print_state state =
  List.iter (function
    | Some c -> Printf.printf "\027[32m%c\027[0m " c (* zielony tekst *)
    | None -> Printf.printf "_ ") state.green_chars;
  
  Printf.printf "\n";
  List.iter (function
    | Some c -> Printf.printf "\027[33m%c\027[0m " c (* żółty tekst *)
    | None -> Printf.printf "_ ") state.yellow_chars;
  
  Printf.printf "\n";
  List.iter (function c ->  Printf.printf "\027[90m%c\027[0m " c (* szary tekst *)
  ) state.grey_chars;
  Printf.printf "\n"


let print_right_words state =
  if (List.length state.right_words) > 1 then
    Printf.printf "There are %d words left: %s\n" (List.length state.right_words) (String.concat " " state.right_words)
  else 
    Printf.printf "There is one word left: %s\n" (String.concat " " state.right_words)

let print_wrong_words state = 
  if (List.length state.wrong_words) > 0 then
    Printf.printf "A word that would narrow choice of letters: %s\n" (List.hd state.wrong_words)
  else 
    Printf.printf "There are no such words\n"

let rec insert c i list =
  if i = 0 then Some c :: List.tl list
  else List.hd list :: insert c (i - 1) (List.tl list)

let rec contains_char str ch =
  match str with
  | "" -> false                   
  | s -> 
    if s.[0] = ch then true       
    else contains_char (String.sub s 1 (String.length s - 1)) ch
     
let update_right_words is_place_right c i current_state : state =
  if is_place_right then
    let updated_right_words =
      List.filter (fun word ->
        Char.equal (String.get word i) c
      ) current_state.right_words
    in
    { current_state with right_words = updated_right_words }
  else
    let updated_right_words =
      List.filter (fun word ->
        (not (Char.equal (String.get word i) c)) && (contains_char word c))
        current_state.right_words
    in
    { current_state with right_words = updated_right_words }

let update_wrong_words c current_state : state =
  let updated_wrong_words =
    List.filter (fun word ->
      not (contains_char word c)
    ) current_state.wrong_words
  in
  { current_state with wrong_words = updated_wrong_words }

let new_character is_place_right c i : unit state_monad =
  bind get_state (fun current_state ->
    let chars = if is_place_right then current_state.green_chars else current_state.yellow_chars in
    let new_chars = insert c i chars in
    let updated_chars =
      if is_place_right then
        { current_state with green_chars = new_chars }
      else
        { current_state with yellow_chars = new_chars }
    in
    let updated_right_words = update_right_words is_place_right c i updated_chars in
    let updated_words = update_wrong_words c updated_right_words in
    set_state updated_words)

let erase_words c words = 
  List.filter (fun word -> 
    not (String.contains word c)
  ) words

let erase_character (c : char) : unit state_monad =
  bind get_state (fun current_state ->
    set_state {
      current_state with
      right_words = erase_words c current_state.right_words;
      wrong_words = erase_words c current_state.wrong_words;
      grey_chars = 
      if List.exists ((=) c) current_state.grey_chars
        then current_state.grey_chars
        else c :: current_state.grey_chars
    })