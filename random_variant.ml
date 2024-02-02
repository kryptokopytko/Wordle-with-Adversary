open State
open Bot

let rec random_game_loop word words : int state_monad =
  bind get_state (fun state ->
    print_string "Guess a word: ";
    flush stdout;
    match read_line () with
    | guess when guess = "r" ->
      print_right_words state;
      random_game_loop word words
    | guess when guess = "n" ->
      print_wrong_words state;
      random_game_loop word words
     | guess when guess = "b" ->
      bind (best_word_narrow_letters false) (fun recommended ->
        Printf.printf "Recommended word: %s\n" recommended;
        random_game_loop word words
      )
    | guess when not (List.exists ((=) guess) words) ->
      print_string "It's not an allowed word\n";
      random_game_loop word words
    | guess when guess = word ->
      print_string "Success!\n";
      return 1
    | guess when guess <> word ->
      let (_, new_state) = (check_letters guess word 0) state in
      print_state new_state;
      bind (check_letters guess word 0) (fun result ->
        bind (random_game_loop word words) (fun result ->
            return (result + 1)
        )
      )
    | _ -> failwith "Something went wrong with input\n"
  )

let rec bot_game_loop word : int state_monad =
  bind get_state (fun state ->
    bind (best_word_narrow_letters false) (fun guess ->
      Printf.printf "Guess: %s\n" guess;
      if word = guess then
        return 1
      else
        bind (check_letters guess word 0) (fun () ->
          bind print_state_monad (fun () ->
            bind (bot_game_loop word) (fun result ->
              return (result + 1)
            )
          )
        )
    )
  )


let rec bot_silent_loop word strategy : int state_monad =
  bind get_state (fun state ->
    bind (best_word_narrow_letters strategy) (fun guess ->
    if word = guess then
      return 1
    else
      bind (check_letters guess word 0) (fun () ->
          bind (bot_silent_loop word strategy) (fun result ->
              return (result + 1)
          ))
        )
  )

let stats_rand is_std_strategy n state =
  let rec loop acc remaining =
    if remaining = 0 then
      acc
    else
      let (result, _) =
        run_state (bot_silent_loop (List.nth state.right_words (Random.int (List.length state.right_words))) is_std_strategy) state
      in
      loop (acc + result) (remaining - 1)
  in
  let average = loop 0 n in
  float_of_int average /. float_of_int n
