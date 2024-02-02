open State
open Bot

let rec generate_answers colors length = 
  if length = 0 then
    [[]]
  else
    List.flatten (
      List.map (fun color ->
        List.map (fun suffix ->
          color :: suffix
        ) (generate_answers colors (length - 1))
      ) colors
    )

let rec zip list1 list2 idx =
  match list1, list2 with
  | [], _ | _, [] -> []
  | hd1 :: tl1, hd2 :: tl2 -> (hd1, hd2, idx) :: zip tl1 tl2 (idx + 1)

let is_char_on_option_list c char_option_list =
  List.fold_left (fun acc x ->
    match x with
    | Some y when y = c -> true
    | _ -> acc
  ) false char_option_list
  
let check_answer (answer : (string * char * int) list) : bool state_monad =
  bind get_state (fun state ->
    let check_grey =
      List.fold_left
        (fun acc (category, c, index) ->
          if category = "grey" then
            acc &&
              (not (is_char_on_option_list c state.green_chars)) &&
              not (List.mem c (List.flatten state.yellow_chars))
          else
            acc)
        true
        answer
    in
    let check_green =
      List.fold_left
        (fun acc (category, c, index) ->
          if category = "green" then
            acc && (not (List.mem c state.grey_chars)) &&
            (not (List.mem c (List.nth state.yellow_chars index)))
          else
            acc)
        true
        answer
    in
    let check_yellow =
      List.fold_left
        (fun acc (category, c, index) ->
          if category = "yellow" then
            acc && (not (List.mem c state.grey_chars)) &&
            (not ((List.nth state.green_chars index) = Some (c)))
          else
            acc)
        true
        answer
    in
    return (check_green && check_grey && check_yellow)
  )

let update_letter answer =
  match (match answer with (x, _, _) -> x) with
  | "grey" -> erase_character (match answer with (_, x, _) -> x)
  | "green" -> new_green_character (match answer with (_, x, _) -> x) (match answer with (_, _, x) -> x)
  | "yellow" -> new_yellow_character (match answer with (_, x, _) -> x) (match answer with (_, _, x) -> x)
  | _ -> failwith "error with generating colors"
        
let rec update_state answer =
  if List.length answer = 1 then
    update_letter (List.hd answer)
  else
    bind (update_letter (List.hd answer))
         (fun () -> update_state (List.tl answer))

         
let adversary_answer guess state =
  let answers = generate_answers ["green"; "yellow"; "grey"] 5 in
  let possible_answers =
    List.fold_left
      (fun acc answer ->
        let is_valid =
          run_state (check_answer (zip answer (List.of_seq (String.to_seq guess)) 0)) state
        in
        match is_valid with
        | true, _ -> zip answer (List.of_seq (String.to_seq guess)) 0 :: acc
        | false, _ -> acc
      )
      [] answers
  in
  let answers_evaluated =
    List.fold_left
      (fun acc answer ->
        let (_, new_state) = update_state answer state in
        (List.length new_state.right_words, answer) :: acc
      )
      [] possible_answers
  in
  let filtered_answers =
    List.filter
      (fun (possibilities, _) -> possibilities > 0)
      answers_evaluated
  in
  let compare_answers (a, _) (b, _) =
    compare b a
  in
  let sorted_answers = List.sort compare_answers filtered_answers in
  match sorted_answers with
  | [] -> failwith "No valid answers found"
  | (_, best_answer) :: _ ->
    let is_success =
      List.for_all
        (fun (c, _, _) ->
          c = "green"
        )
        best_answer
    in
    if List.length sorted_answers > 1 && is_success
    then (snd (List.hd (List.tl sorted_answers)), false)
    else (best_answer, is_success)


let rec read_line_safe () =
  try
    Some (read_line ())
  with
  | End_of_file -> None
  | ex ->
    Printf.eprintf "Error: %s\n" (Printexc.to_string ex);
    None


let rec adversary_game_loop state words : int state_monad =
  bind get_state (fun current_state ->
    match read_line_safe () with
    | Some guess when guess = "r" -> 
      print_right_words current_state; adversary_game_loop current_state words
    | Some guess when guess = "n" -> 
      print_wrong_words current_state; adversary_game_loop current_state words
    | Some guess when guess = "b" ->
      Printf.printf "Recommended word: %s\n" 
      (best_word current_state.right_words (option_list_to_list current_state.green_chars));
      adversary_game_loop current_state words
    | Some guess when List.mem guess words ->
      let (best_answer, is_success) = adversary_answer guess current_state in
      let (_, new_state) = update_state best_answer current_state in
      print_state new_state;
      if is_success then
        return 1
      else
        bind (update_state best_answer) (fun result ->
         bind (adversary_game_loop new_state words) (fun result ->
          return (result + 1)
        ))
    | Some _ -> print_string "It's not an allowed word\n"; adversary_game_loop current_state words
    | None -> print_string "Error while reading input\n"; adversary_game_loop current_state words
  )


let rec bot_adv_silent_loop strategy : int state_monad =
  bind get_state (fun state ->
    bind (best_word_narrow_letters false) (fun guess ->
    let (best_answer, is_success) = adversary_answer guess state in
      if is_success then
        return 1
      else
        bind (update_state best_answer) (fun result ->
         bind (bot_adv_silent_loop strategy) (fun result ->
          return (result + 1)
        ))))

let stats_adv is_std_strategy n state =
  let rec loop acc remaining =
    if remaining = 0 then
      acc
    else
      let (result, _) = bot_adv_silent_loop is_std_strategy state in
      loop (acc + result) (remaining - 1)
  in
  let average = loop 0 n in
  float_of_int average /. float_of_int n
