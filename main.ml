open State
open Random_variant
open Adversary_variant
open Bot
  
let read_words_from_file filename =
  try
    let channel = open_in filename in
    let rec read_words acc =
      try
        let line = input_line channel in
        let words = List.filter (fun s -> s <> "") (String.split_on_char ' ' line) in
        read_words (List.rev_append words acc)
      with End_of_file ->
        close_in channel;
        List.rev acc
    in
    read_words []
  with Sys_error msg ->
    Printf.printf "Error: %s\n" msg;
    []

let print_rainbow_string str idx =
  Printf.printf "\027[38;5;%dm%s\027[0m" (idx * 36 + 21) str

let () =
  let _ = Random.init (int_of_float (Unix.time ())) in 
  let words = read_words_from_file "words.txt" in
  let random_word = List.nth words (Random.int (List.length words)) in 

  let initial_state : state = {
    green_chars = [None; None; None; None; None];
    yellow_chars = [[]; []; []; []; []];
    grey_chars = [];
    right_words = words;
    wrong_words = words;
  } in
  print_rainbow_string "Welcome to Wordle game!\n" 0;
  print_rainbow_string "Type 'r' to play normally,\n" 1;
  print_rainbow_string "     'a' to play with adversary,\n" 2;
  print_rainbow_string "     'b' to see how bot suffers\n" 3;
  print_rainbow_string "     's' to see statistics for different bot strategies\n" 4;
  flush stdout;

  match read_line () with
  | choice when choice = "r" ->
    print_rainbow_string "\nType 'r' to get a list of words left,\n" 0;
    print_rainbow_string "     'n' for a word that consists only of unknown letters\n" 2;
    print_rainbow_string "     'b' for a word with the most common letters\n" 4;
    random_game_loop random_word initial_state words
  | choice when choice = "a" ->
    print_rainbow_string "\nType 'r' to get a list of words left,\n" 0;
    print_rainbow_string "     'n' for a word that consists only of unknown letters\n" 2 ;
    print_rainbow_string "     'b' for a word with the most common letters\n" 4;
    adversary_game_loop initial_state words
  | choice when choice = "b" ->
    bot_game_loop random_word initial_state words 1
  | choice when choice = "s" ->
    print_rainbow_string "meh\n" 1
  | _ -> failwith "Something went wrong with input\n" 


(* git add -- * ':!*.cmi' ':!*.cmo' ':!*.out' *)

