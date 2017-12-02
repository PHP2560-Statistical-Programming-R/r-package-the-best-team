#Generate the next location
get_4 = get_function(4, black, white)
if(get_4 == 0){get_3 = get_function(3, black, white)} else{new = get_4}
if(get_3 == 0){get_2 = get_function(2, black, white)} else{new = get_3}
if(get_2 == 0){get_1 = get_function(1, black, white)} else{new = get_2}
if(get_1 == 0){
  repeat{
    new = c(sample(1:n,1),sample(1:n,1))
    xy <- paste(new, collapse = ":") #record the step
    if (!is.element(xy, playedlist)) #break when the point had chessman on it
      break
  } else{new = get_1}
}


#Get the avalible spot; if not available , return 0
get_function = function(num, set1, set2){
  get_num = lapply(set1, judge, num, set1)
  temp = sapply(get_num, is.list)
  if(is.element(1, temp)){
    get_list = get_4[[which(temp == 1)]]
    get_vector = check_blank(get_list[[1]], get_list[[2]], get_list[[3]], set2)
  } else {return (0)}
  return(get_vector)
}


#Check if there are num continuous points
judge = function(x, num, location){
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
  for(i in 1:4){
    judge = sum(is.element(line[[i]], location))
    if (judge == num)
      return(list(num, i, x))
    if(i == 4)
      return(0)
  }
}

#Check if the near points are filled
check_blank = function(num_point, index, point, location){
  i = num_point
  x = point
  if(index == 1)
  {
    left = list(c(x[1]-1, x[2]-1))
    right = list(c(x[1]+i, x[2]+i))
    if(!is.element(left, location) & boundary(left, n))
      return(left)
    if(!is.element(right, location) & boundary(right, n))
      return(right)
  }
  if(index == 2)
  {
    left = list(c(x[1]-1, x[2]+1))
    right = list(c(x[1]+i, x[2]-i))
    if(!is.element(left, location) & boundary(left, n))
      return(left)
    if(!is.element(right, location) & boundary(right, n))
      return(right)
  }
  if(index == 3)
  {
    left = list(c(x[1], x[2]+1))
    right = list(c(x[1], x[2]-i))
    if(!is.element(left, location) & boundary(left, n))
      return(left)
    if(!is.element(right, location) & boundary(right, n))
      return(right)
  }
  if(index == 4){
    left = list(c(x[1]-1, x[2]))
    right = list(c(x[1]+i, x[2]))
    if(!is.element(left, location) & boundary(left, n))
      return(left)
    if(!is.element(right, location) & boundary(right, n))
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