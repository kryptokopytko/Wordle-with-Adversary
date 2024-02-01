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
  } in (*
  let easy_state : state = {
    green_chars = [None; None; None];
    yellow_chars = [[]; []; []];
    grey_chars = [];
    right_words = ["abc"; "bac"; "cab"; "cba"; "bca"; "acb"];
    wrong_words = ["abc"; "bac"; "cab"; "cba"; "bca"; "acb"];
  } in *)

  print_string "Welcome to Wordle game!\nType 'r' to get a list of words left,\n     'n' for a word that consists only of unknown letters\n  or 'b' for a word with the most common letters\n";
  Printf.printf "A random word: %s\n" random_word;
  (*adversary_game_loop initial_state words;
  random_game_loop random_word initial_state words;*)
  (*bot_game_loop random_word initial_state words 1*)
  adversary_game_loop initial_state words
  (*
  let update_state_monad =
    bind (new_yellow_character 'i' 1) (fun () ->
      bind (new_green_character 'e' 4) (fun () ->
        bind (new_green_character 'a' 0) (fun () ->
        erase_character 'p'
      ))
    )
  in
  let (_, final_state) = update_state_monad  initial_state in
  *)


(* git add -- * ':!*.cmi' ':!*.cmo' ':!*.out' *)

