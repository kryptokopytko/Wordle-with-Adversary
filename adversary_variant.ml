open State

let rec adversary_game_loop state words =
  print_string "Guess a word: ";
  flush stdout;
  match read_line () with
  | guess when guess = "r" -> 
    print_right_words state;
    adversary_game_loop state words
  | guess when guess = "n"-> 
    print_wrong_words state;
    adversary_game_loop state words
  | guess when not (List.exists ((=) guess) words) -> 
      print_string "It's not an allowed word\n";
      adversary_game_loop state words
 (* | guess when true -> 
     adversary_guess guess state
      adversary_game_loop word state words*)
  | _ -> print_string "Something went wrong with input\n"
