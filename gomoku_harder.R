#check availability
check_blank = function(num_point, index, point, player, computer, n){
  i = num_point
  x = point
  if(index == 1)
  {
    left = list(c(x[1]-1, x[2]-1))
    right = list(c(x[1]+i, x[2]+i))
  }
  if(index == 2)
  {
    left = list(c(x[1]-1, x[2]+1))
    right = list(c(x[1]+i, x[2]-i))
  }
  if(index == 3)
  {
    left = list(c(x[1], x[2]-1))
    right = list(c(x[1], x[2]+i))
  }
  if(index == 4)
  {
    left = list(c(x[1]-1, x[2]))
    right = list(c(x[1]+i, x[2]))
  }
  
  left = list(as.double(unlist(left)))
  right = list(as.double(unlist(right)))
  if_left = sum(is.element(left, player),!within_boundary(left, n),is.element(left, computer))
  if_right = sum(is.element(right, player),!within_boundary(right, n), is.element(right,computer))
  
  if(if_left + if_right == 0){return(list(left, right, 2))}
  if(if_left == 0){return(list(left, 1))}
  if(if_right == 0){(return(list(right, 1)))}
  return(0)
}


#Get the avalible spot; if not available , return 0
get_function = function(num, player, computer, n = NULL){
  get_num = lapply(player, judge, num, player, computer, n)
  temp = sapply(get_num, length)
  two_ava = which(temp == 3)
  one_ava = which(temp == 2)
  if(length(two_ava) > 0){
    get_one = sample(1:length(two_ava),1)
    get_list = get_num[[two_ava[get_one]]]
    random = sample(1:2, 1)
    final = get_list[[random]]
    return(final)
  }
  if(length(one_ava) > 0){
    get_one = sample(1:length(one_ava),1)
    get_list = get_num[[one_ava[get_one]]]
    final = get_list[[1]]
    return(final)
  }
  else {return (0)}
}   

#Check if the point is located inside the chessboard
within_boundary = function(x, n){
  x = unlist(x)
  judge_l = x<1
  judge_r = x>n
  if(sum(judge_l)>0 || sum(judge_r)>0){return(0)}
  else{return(1)}
}

##Generate five points based on their x-value and y-value
make_line = function(x,y){
  a = list()
  for(i in 1:length(x)){
    a[[i]] = c(x[i],y[i])
  }
  return(a)
}

#computer play
computer_play = function(player, computer, playlist, n){
  get_4 = get_function(4, player, computer, n)
  if(!is.list(get_4)){get_3 = get_function(3, player, computer, n)} else{return(get_4)}
  if(!is.list(get_3)){get_2 = get_function(2, player, computer, n)} else{return(get_3)}
  if(!is.list(get_2)){get_1 = get_function(1, player, computer, n)} else{return(get_2)}
  if(!is.list(get_1)){
    repeat{
      new = list(c(sample(1:n,1),sample(1:n,1)))
      xy <- paste(new, collapse = ":") #record the step
      if (!is.element(xy, playlist)) #break when the point had chessman on it
        break
    } }else{return(get_1)}
  return(new)
}

#player play
player_play = function(playlist, n){
  repeat {
    options(locatorBell = FALSE)
    l = locator(1)
    l$x <- min(n, max(1, round(l$x))) #modify the x-location to where nearest point
    l$y <- min(n, max(1, round(l$y))) #modify the y-location to where nearest point
    xy <- paste(l, collapse = ":") #record the step
    if (!is.element(xy, playlist)) #break when the point had chessman on it
      break
  }
  return(l)
}


judge_five = function(x, location){
  #line1 is a y=x kind of line, the point x is the middle of the five points
  x1 = c((x[1]-2):(x[1]+2))
  x1 = as.double(x1)
  y1 = c((x[2]-2):(x[2]+2))
  y1 = as.double(y1)
  line1 = make_line(x1, y1)
  
  #line2 is a y=-x kind of line, the point x is the middle of the five points
  x2 = x1
  y2 = rev(y1)
  line2 = make_line(x2, y2)
  
  #line3 is a horizontal line, the point x is the middle of the five points
  x3 = rep(x[1], 5)
  y3 = y1
  line3 = make_line(x3, y3)
  
  #line4 is a vertical line, the point x is the middle of the five points
  x4 = x1
  y4 = rep(x[2], 5)
  line4 = make_line(x4, y4)
  
  #save the lines in a list
  line = list(line1, line2, line3, line4)
  
  #check if there are five continuous points in the set
  for(i in 1:4){
    judge = sum(is.element(line[[i]], location))
    if (judge == 5)
      return(1)
    if(i == 4)
      return(0)
  }
}

#check if wins
if_win = function(set){
  win = sapply(set, judge_five, set)#check if there are five continuous points in black chessmen set
  return(is.element(1, win))
}

#Check if there are num continuous points
judge = function(x, num, player, computer, n = NULL){
  #line1 is a y=x kind of line, the point x is the middle of the five points
  x1 = c(x[1]:(x[1]+num-1))
  x1 = as.double(x1)
  y1 = c(x[2]:(x[2]+num-1))
  y1 = as.double(y1)
  line1 = make_line(x1, y1)
  
  #line2 is a y=-x kind of line, the point x is the middle of the five points
  x2 = x1
  y2 = c(x[2]:(x[2]-num+1))
  line2 = make_line(x2, y2)
  
  #line3 is a horizontal line, the point x is the middle of the five points
  x3 = rep(x[1], num)
  y3 = y1
  line3 = make_line(x3, y3)
  
  #line4 is a vertical line, the point x is the middle of the five points
  x4 = x1
  y4 = rep(x[2], num)
  line4 = make_line(x4, y4)
  
  #save the lines in a list
  line = list(line1, line2, line3, line4)
  
  #check if there are five continuous points in the set
  get_num = list()

  for(i in 1:4){
    if_line = sum(is.element(line[[i]], player))
    if(if_line == num)
    get_num[[i]] = check_blank(num, i, x, player, computer, n)
    else {get_num[[i]] = 0}
  }
  
  temp = sapply(get_num, length)
  two_ava = which(temp == 3)
  one_ava = which(temp == 2)
  
  if(length(two_ava) > 0){
    get_one = sample(1:length(two_ava),1)
    get_list = get_num[[two_ava[get_one]]]
    return(get_list)
  }
  if(length(one_ava) > 0){
    get_one = sample(1:length(one_ava),1)
    get_list = get_num[[one_ava[get_one]]]
    return(get_list)
  }
  return(0)
}