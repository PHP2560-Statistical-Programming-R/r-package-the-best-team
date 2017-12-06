gomoku = function(n = 19){
  
  if (.Platform$OS.type == "windows") 
    x11() else x11(type = "Xlib")
  
  click = 0
  result = ""
  
  #Choose whom to play wit
    
    click = first_choose()
    
    if(click == 1){result = gomoku_self(n)}
    
    
    if(click == 2){
      #Choose color
      click = second_choose()
    
      if(click == 3){result = gomoku_computer(n, choose = 1)}
      if(click == 4){result = gomoku_computer(n, choose = 2)}
      
    }
  gameover(result = result)
  repeat{
    l = locator(1)
    x = l$x
    y = l$y
    if(x>33.7 & x<65.5 & y>32 & y<48){gomoku(n)}
    if(x>42.3 & x<58 & y>12 & y<27){return("Game Closed")}
  }
}
  

first_choose = function(){
  stage0()
  repeat{
    l = locator(1)
    x = l$x
    y = l$y
    if(x>39.7 & x<59.7 & y>33 & y<47){return(1)}
    if(x>35 & x<65 & y>12 & y<27){return(2)}
  }
}


second_choose = function(){
  stage1()
  repeat{
    l = locator(1)
    x = l$x
    y = l$y
    if(x>39.7 & x<59.7 & y>32 & y<46){return(3)}
    if(x>39.7 & x<59.7 & y>12 & y<27){return(4)}
  }
}



