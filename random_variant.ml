open State
open Bot

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
  | guess when guess = "b"-> 
    Printf.printf "Recommended word: %s\n" (best_word state.right_words (option_list_to_list state.green_chars));
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
  | _ -> failwith "Something went wrong with input\n"
