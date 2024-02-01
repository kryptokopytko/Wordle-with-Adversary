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


let rec bot_game_loop word state i = 
  let guess = (best_word state.right_words (option_list_to_list state.green_chars)) in
  Printf.printf "Guess: %s\n" guess;
  if word = guess then 
    Printf.printf "Success! Number of guesses: %d\n" i
  else
    let (_, new_state) = (check_letters guess word 0) state in
      print_state new_state;
      bot_game_loop word new_state (i + 1)

let rec bot_silent_loop word state strategy = 
  let guess = (best_word_narrow_letters state strategy) in
  if word = guess then 
   1
  else
    let (_, new_state) = (check_letters guess word 0) state in
      (bot_silent_loop word new_state strategy) + 1

let stats_rand is_std_strategy state n =
  let rec loop acc remaining =
    if remaining = 0 then
      acc
    else
      let result = bot_silent_loop (List.nth state.right_words (Random.int (List.length state.right_words))) state is_std_strategy in
      loop (acc + result) (remaining - 1)
  in
  let average = loop 0 n in
  float_of_int average /. float_of_int n

