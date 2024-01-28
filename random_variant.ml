open State

let check_letter guess word idx =
    if guess.[idx] = word.[idx] 
      then new_character true guess.[idx] idx
      else if contains_char word guess.[idx]
        then new_character false guess.[idx] idx
        else erase_character guess.[idx]
        
let rec check_letters guess word idx : unit state_monad =
  if String.length word = idx + 1 then
    check_letter guess word idx
  else
    bind (check_letter guess word idx)
         (fun () -> check_letters guess word (idx + 1))


let rec random_game_loop word state words =
  print_string "Guess a word: ";
  flush stdout;
  match read_line () with
  | guess when guess = "r" -> 
    print_right_words state;
    random_game_loop word state words
  | guess when guess = "n"-> 
    print_wrong_words state;
    random_game_loop word state words
  | guess when not (List.exists ((=) guess) words) -> 
      print_string "It's not an allowed word\n";
      random_game_loop word state words
  | guess when guess = word -> 
      print_string "Success!\n"
  | guess when guess <> word -> 
      let (_, new_state) = (check_letters guess word 0) state in
      print_state new_state;
      random_game_loop word new_state words
  | _ -> print_string "Something went wrong with input\n"
