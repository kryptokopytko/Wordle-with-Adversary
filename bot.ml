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
  let filtered_words = filter_out_repeating_letters sorted_word_sums in
  match filtered_words with
  | [] -> fst (List.hd sorted_word_sums)
  | hd :: _ -> fst hd

let best_word_narrow_letters strategy : string state_monad =
  bind get_state (fun state ->
    let words, green_letters =
      if (List.length state.wrong_words = 0) || (not strategy) then
        state.right_words, (option_list_to_list state.green_chars)
      else
        state.wrong_words, []
    in
    return (best_word words green_letters))
