
#LittleGames Package

Authors: Wei Wang, Yimo Zhang, Bowei Wei

##Idea of the Package

Now, many games are made in the plotform of C++, javascipt and so on. Few games are driven by R programming, probably because people always see R as a tool to do statistical analysis. We want to show that games can also be made through R program.

Note that we also refer to other people's work when we are making these games, [fun package](https://github.com/yihui/fun). Most of our functions are modified based on the original code.


## What We Do

(1)Adding comments to the codes so that they are easy to understand;

(2)Fixing the bugs and make some modifications to the codes;

(3)Adding some factors to the games so that they are more "playable". For example, add a restart button so that we don't have to call the function over and over again.

## Functions

### 1. Profile of Games In This Package

Function Name: little_game()

This function returns a table that contains the names of the games and their following profile; you can call little_game("game name") to find relevant information 
of the game.

### 2. Mine Sweeper Game

Function Name: minesweeper

You can just type in minesweeper() to start the game. left click is to identify the mines. Right click is for the flags. You will have the options for restart if the game ends. I won't tell you there's bonus scene that you can cheat in the program! Hahaha glhf!

### 3. R Snake Game

Function Name: Snake

Call Snake() to play the r_snake game; move the snake by using keyboard.

### 4. Gomoku Game

Functon Name: gomoku

Call gomoku(n) to play the gomoku game. You can decide the size of the chessboard by intering different values of n. You can also call gomoku() to play the standard gomoku game.

### 5. R Flag Games

Function Name: Denmark, Finland, Japan, Iceland, Norway, Sweden

Call the country name to return a corresponding flag. Note that the flag functions are limited, so you may want to check the package to see what country flags are included in this package.

## Contributions of Group Members

Wei Wang: R file
   
          Flags.R (R Flag Games)
          
          Snake.R (R Snake Game)
          
          list.R (Game Profile)
          
Yimo Zhang: R file

            gomoku.R (Gomoku Game)
            
            list.R (Game Profile)
            
Bowei Wei: R file

            mine_sweeper_pkg.R
            
            list.R (Game Profile)
            

      
          




