judge = function(x, num, location, location2 = black){
  #line1 is a y=x kind of line, the point x is the middle of the five points
  x1 = c(x[1]:(x[1]+num-1))
  y1 = c(x[2]:(x[2]+num-1))
  line1 = line(x1, y1)
  
  #line2 is a y=-x kind of line, the point x is the middle of the five points
  x2 = x1
  y2 = rev(y1)
  line2 = line(x2, y2)
  
  #line3 is a horizontal line, the point x is the middle of the five points
  x3 = rep(x[1], num)
  y3 = y1
  line3 = line(x3, y3)
  
  #line4 is a vertical line, the point x is the middle of the five points
  x4 = x1
  y4 = rep(x[2], num)
  line4 = line(x4, y4)
  
  #save the lines in a list
  line = list(line1, line2, line3, line4)
  
  #check if there are five continuous points in the set
  for(i in 1:4){
    print(line[[1]])
    judge = sum(is.element(line[[i]], location))
    if (judge == num)
      return(1)
    if(i == 4)
      return(0)
  }
}




check_ava = function(num_point, index, point, location2){
  i = num_point
  x = point
  if(index == 1)
  {
    left = c(x[1]-1, x[2]-1)
    right = c(x[1]+i, x[2]+i)
    if(is.element(left, location2) & boundary(left, n))
      return(left)
    if(is.element(right, location2) & boundary(right, n))
      return(right)
  }
  if(index == 2)
  {
    left = c(x[1]-1, x[2]+1)
    right = c(x[1]+i, x[2]-i)
    if(is.element(left, location2) & boundary(left, n))
      return(left)
    if(is.element(right, location2) & boundary(right, n))
      return(right)
  }
  if(index == 3)
  {
    left = c(x[1], x[2]+1)
    right = c(x[1], x[2]-i)
    if(is.element(left, location2) & boundary(left, n))
      return(left)
    if(is.element(right, location2) & boundary(right, n))
      return(right)
  }
  if(index == 4){
    left = c(x[1]-1, x[2])
    right = c(x[1]+i, x[2])
    if(is.element(left, location2) & boundary(left, n))
      return(left)
    if(is.element(right, location2) & boundary(right, n))
      return(right)
  }
  return(0)
}


boundary = function(x, n){
  judge_l = x<1
  judge_r = x>n
  if(sum(judge_l)<2){return(0)}
  if(sum(judge_r)<2){return(0)}
  return(1)
}