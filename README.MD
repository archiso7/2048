# 2048 - Fully in Haskell

## Discription

<p>This project was a challenge from a friend to learn Haskell. I have never used a fully functional language before this, so it was a tough challenge. I have thoughts for expanding this game and making it multiplayer in the future, and adding more game modes. While this was never meant to actually be played, if you do play it, I hope you enjoy!</p>

### Goal

<p>The goal of the game is to combine tiles, by moving them in different directions, until you reach the 2048 tile.</p>

## How to Play

### Installation

1. Navigate to the [releases page](https://github.com/archiso7/2048/releases) and download the latest release or pre-release. (or build from [source](#building-From-Source))
2. Double click the file or run it in a terminal.
3. Have Fun!

### Controls

- W, A, S, D to move pieces up, left, down, and right.
  - Arrow keys coming soon!


## Contributing

### Bugs

If you find any bugs in the game, head over to the [issues](https://github.com/archiso7/2048/issues) page and report the bug, ONLY IF NOT ALREADY REPORTED.

### Building From Source

1. Instatl GHC, ghcup, and Cabal or Stack (google it).
2. Clone this repository or download the compressed source code from the [releases page](https://github.com/archiso7/2048/releases).
3. Open the directory with the source code.

#### Cabal

4. Run `cabal build` (the .cabal file is included in the source code)
5. Run `cabal run` to launch the game

#### Stack

4. Run `stack init`
5. Run `stack build` (this will take a while)
6. Run `stack run` to launch the game