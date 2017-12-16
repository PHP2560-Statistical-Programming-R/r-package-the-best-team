#'@title The profile of this package
#'@description This function contains some information regarding the four games in this 
#'            package. If you have doubt about these games, this function will probably help you.
#'@param name The name of a function.
#'@return A list of information about the game.
#'@Author Wei Wang, Yimo Zhang, Bowei Wei
#'@examples 
#'little_game("Snake")
#'@export

little_game = function(name){
  t = tibble("Name" = c("","","",""), "Rule" = c("","","",""), "Additional Notes" = c("","","",""))
  t$Name = c("Mine Sweeper", "Gomoku", "Snake", "Flags")
  
  #gomoku
  t$Rule[2] = "Each player chooses a color and who gets black goes first; Players place their stones on 
               the board alternatively, one at a time, and the first one who has five stones of his color
               in row wins the game."
  t$`Additional Notes`[2] = "Although gomoku is not as complicated as Go, it will be hard to 
  win the game if you don't use some strategies. For more information, 
  please see website {http://gomokuworld.com/articles/16_useful_tips_to_become_a_better_gomoku_player}."
  
  #mine sweeper
  
  
  #Snake
  
  #Flags
  return(filter(t, Name == name))
  }