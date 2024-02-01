open State

module CharMap = Map.Make(Char)

let rec string_to_list s idx = 
  if idx = String.length s then
    []
  else
    s.[idx] :: string_to_list s (idx + 1)
let count_letters words green_letters =
  let letters_count_map =
    List.fold_left
      (fun acc word ->
        List.fold_left
          (fun acc_char c ->
            match acc_char with
            | Some count_map ->
              let count = 
                if List.mem c green_letters then 0
                else
                  try CharMap.find c count_map
                  with Not_found -> 0
              in
              Some (CharMap.add c (count + 1) count_map)
            | None -> Some (CharMap.singleton c 1)
          )
          acc (string_to_list word 0)
      )
      (Some CharMap.empty)
      words
  in
  match letters_count_map with
  | Some count_map -> count_map
  | None -> CharMap.empty

let rec string_to_list s idx = 
  if idx = String.length s then
    []
  else
    s.[idx] :: string_to_list s (idx + 1)

let sum_of_letter_values word letter_values =
  let letter_to_value c =
    try CharMap.find c letter_values
    with Not_found -> 0
  in
  List.fold_left (fun acc c -> acc + letter_to_value c) 0 (string_to_list word 0)

let has_repeating_letters word =
  let c_set = Hashtbl.create (String.length word) in
  let rec has_repeating_letters_helper idx =
    if idx < String.length word then
      let c = word.[idx] in
      if Hashtbl.mem c_set c then
        true
      else begin
        Hashtbl.add c_set c true;
        has_repeating_letters_helper (idx + 1)
      end
    else
      false
  in
  has_repeating_letters_helper 0

let filter_out_repeating_letters pairs_list =
  List.filter (fun (word, _) -> not (has_repeating_letters word)) pairs_list

let best_word words green_letters =   
  let letter_counts = count_letters words green_letters in
  let word_sums = List.map (fun word -> (word, sum_of_letter_values word letter_counts)) words in
  let sorted_word_sums = List.sort (fun (_, sum1) (_, sum2) -> compare sum2 sum1) word_sums in
  let filtered_words = filter_out_repeating_letters  sorted_word_sums in
    if List.length filtered_words = 0 
      then fst (List.hd sorted_word_sums)
      else fst (List.hd filtered_words)

let best_word_narrow_letters state = 
  if List.length state.wrong_words = 0 
    then best_word state.right_words (option_list_to_list state.green_chars)
    else List.hd state.wrong_words

let rec bot_game_loop word state words i = 
  let guess = (best_word state.right_words (option_list_to_list state.green_chars)) in
  Printf.printf "Guess: %s\n" guess;
  if word = guess then 
    Printf.printf "Success! Number of guesses: %d\n" i
  else
    let (_, new_state) = (check_letters guess word 0) state in
      print_state new_state;
      bot_game_loop word new_state words (i + 1)