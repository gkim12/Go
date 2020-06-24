# Go
The strategy-based board game Go recreated in the functional programming language Typed Racket.

## Running the program

Clone the repository and open *go.rkt* in the [Dr. Racket IDE](https://download.racket-lang.org/).

Click *Run* in the top-right corner of the IDE and wait for the program to compile 
- The first run will take approximately a minute to start up 
- NOTE: Dr. Racket may request a higher memory limit. Simply accept if this happens.
- The Racket console at the bottom will eventually display a ">" indicating it has finished compilation. 

To start the game, put the following into the Racket console: `(play 19 (BoardSpec 'burlywood 30 10 10))`

- This will create a Go game with a 19x19 board and burlywood background color. The final three numbers represent the cell size, margin size, and stone radius, respectively. Feel free to try out different parameters!

## Saving and Loading Games

### Saving

Pressing "s" on the keyboard during a game will prompt the user to save a game.

### Loading

Pressing "l" on the keyboard during a game will allow the user to load a game that was previously saved.
