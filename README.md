# Domino Strategy Simulator

## Overview

A Haskell implementation of a dominoes match simulator that allows two players to compete against each other over multiple games. The framework enables testing and evaluating different domino-playing strategies.

## Tech Stack

* Haskell

## Key Features

* Multi-game tournament simulation with flexible player strategies
* Game history storage and randomised shuffle options 

## How to Run

Ensure Haskell (GHC) is installed.

1. Load and run the module in GHCi: `ghci DomsMatch.hs`

3. Run a dominoes match by calling `domsMatch` with your desired parameters: `domsMatch games target player1 player2 seed`

   The parameters correspond to:
   - `games` = number of games to play
   - `target` = target score to reach
   - `player1`, `player2` = your DomsPlayer strategy functions ( `simplePlayer`, `smartPlayer`)
   - `seed` = random number seed for reproducibility
