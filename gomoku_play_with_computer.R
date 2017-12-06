#check if wins
if_win = function(set){
win = sapply(set, judge, 5, set)#check if there are five continuous points in black chessmen set
return(is.element(1, win))
}

#player play
player_play = function(playlist, n){
  repeat {
    l <- locator(1) #record the location where the mouse clicks
    l$x <- min(n, max(1, round(l$x))) #modify the x-location to where nearest point
    l$y <- min(n, max(1, round(l$y))) #modify the y-location to where nearest point
    xy <- paste(l, collapse = ":") #record the step
    if (!is.element(xy, playlist)) #break when the point had chessman on it
      break
  }
  return(l)
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
  



#Get the avalible spot; if not available , return 0
get_function = function(num, set1, set2, n){
  get_num = lapply(set1, judge, num, set1, set2, n)
  temp = sapply(get_num, is.list)
  if(is.element(1, temp)){
    equal_to_one = which(temp == 1)
    get_one = sample(1:length(equal_to_one),1)
    get_list = get_num[[equal_to_one[get_one]]]
  } else {return (0)}
  return(get_list)
}


#Check if there are num continuous points
judge = function(x, num, set1, set2, n){
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
  random = sample(c(1:4))
  j = 1
  for(i in random){
    judge = sum(is.element(line[[i]], set1))
    if(num == 5){
      if (judge == num)
        return(1)
    } else{
      temp = check_blank(num, i, x, set2, set1, n)
      if(judge == num & is.list(temp))
        return(temp)
    }
    j = j+1
    if(j == 4)
      return(0)
  }
}

#Check if the near points are filled
check_blank = function(num_point, index, point, location, location2, n){
  i = num_point
  x = point
  if(index == 1)
  {
    left = list(c(x[1]-1, x[2]-1))
    right = list(c(x[1]+i, x[2]+i))
    if(!is.element(left, location) & !is.element(left, location2) & boundary(left, n))
      return(left)
    if(!is.element(right, location) & !is.element(right, location2) & boundary(right, n))
      return(right)
  }
  if(index == 2)
  {
    left = list(c(x[1]-1, x[2]+1))
    right = list(c(x[1]+i, x[2]-i))
    if(!is.element(left, location) & !is.element(left, location2) & boundary(left, n))
      return(left)
    if(!is.element(right, location) & !is.element(right, location2) & boundary(right, n))
      return(right)
  }
  if(index == 3)
  {
    left = list(c(x[1], x[2]-1))
    right = list(c(x[1], x[2]+i))
    if(!is.element(left, location) & !is.element(left, location2) & boundary(left, n))
      return(left)
    if(!is.element(right, location) & !is.element(right, location2) & boundary(right, n))
      return(right)
  }
  if(index == 4){
    left = list(c(x[1]-1, x[2]))
    right = list(c(x[1]+i, x[2]))
    if(!is.element(left, location) & !is.element(left, location2) & boundary(left, n))
      return(left)
    if(!is.element(right, location) & !is.element(right, location2) & boundary(right, n))
      return(right)
  }
  return(0)
}

#Check if the point is located inside the chessboard
boundary = function(x, n){
  x = unlist(x)
  judge_l = x<1
  judge_r = x>n
  if(sum(judge_l)>0 || sum(judge_r)>0){return(0)}
  else{return(1)}
}