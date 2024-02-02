# Wordle with Adversary

The Wordle with Adversary is a project that provides a platform for playing the Wordle game with various features. The Wordle game is a word puzzle where players attempt to guess a secret word by suggesting words and receiving feedback on the correctness of their guesses. The game involves a combination of deduction and word analysis.

## Features

1. **Normal Gameplay Mode:**
   - In this mode, players can play the classic version of Wordle, attempting to guess the secret word that was randomly picked at the begining of the game.

2. **Adversary Gameplay Mode:**
   - The Adversary Gameplay Mode introduces an opponent (adversary) with its own set of words and strategies. Players face the challenge of guessing the secret word while competing against the adversary's dynamic responses.

3. **Bot Implementation:**
   - The project includes the implementation of bots with different strategies to assist players or compare different strategies.
     - **Random Bot:** Makes random guesses during the game.
     - **Adversary Bot:** Implements strategies to compete against the adversary in the game.

4. **Colorful User Interface:**
   - The user interface is designed to be visually appealing, using colors to represent different aspects of the game.
     - **Green Characters:** Characters in the correct position.
     - **Yellow Characters:** Characters in the wrong position but present in the word.
     - **Grey Characters:** Characters absent in the word.

## Wordle Gameplay

- Wordle is a word puzzle game where the player attempts to guess a secret word within a limited number of attempts.
- Feedback is provided for each guess, indicating correct characters in the correct position (green), correct characters in the wrong position (yellow), and incorrect characters (grey).
- The player must use deduction and word analysis to refine their guesses and ultimately identify the secret word.

## Objective

- Successfully guess the secret word within the given number of attempts.
- Outsmart the adversary in the adversary gameplay mode.

**Have fun playing Wordle with a twist and explore the various strategies employed by the implemented bots!**
Compiling: ocamlc unix.cma state.ml bot.ml random_variant.ml adversary_variant.ml main.ml