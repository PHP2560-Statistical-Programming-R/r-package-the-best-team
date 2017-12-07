stage0 = function(){
  colfunc <- colorRampPalette(c("indianred", "firebrick"))
  img<-readPNG("gomoku_2.png")
  n = 100
  par(mar = rep(0, 4))
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))
  rasterImage(img,0,0,n,n)
  text(50,80,label="GOMOKU",cex=5, col = "coral4")
  text(seq(37, 58, length.out = 6), rep(40, 6), label=unlist(strsplit("BATTLE",NULL)), cex=2,col=colfunc(6))
  text(seq(34, 66, length.out = 8), rep(20 ,8), label=unlist(strsplit("COMPUTER",NULL)), cex=2,col=colfunc(8))
  rect(39.7, 33, 59.7, 47)
  rect(35,12,65,27)
}


stage1 = function(){
  colfunc <- colorRampPalette(c("black", "white"))
  img<-readPNG("gomoku_2.png")
  n = 100
  par(mar = rep(0, 4))
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))
  rasterImage(img,0,0,n,n)
  text(50,80,label="Computer",cex=5)
  text(seq(41, 58, length.out = 5), rep(40, 5), label=unlist(strsplit("BLACK" ,NULL)),cex=2,col=colfunc(5), adj = c(0.5,0.5))
  text(seq(41, 58, length.out = 5), rep(20, 5), label=unlist(strsplit("WHITE" ,NULL)),cex=2,col=colfunc(5), adj = c(0.7, 0.7))
  rect(39.7, 32, 59.7, 46)
  rect(39.7,12,59.7,27)
}

gameover = function(result){
  colfunc <- colorRampPalette(c("black", "white"))
  img1 = readPNG("gomoku_2.png", FALSE)
  n = 100
  par(mar = rep(0, 4))
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))
  rasterImage(img1,0,0,n,n)
  rect(0,0,n,n,col = alpha("white", 0.3), border = NA)
  if(result == "Computer Wins!"){
    img = readPNG("sad_face.png")
    rasterImage(img,35,50,65,80)
  } else {
    img = readPNG("happy_face.png")
    rasterImage(img,35,50,65,80)
  }
  text(seq(33, 65, length.out = 10), rep(40,10), labels = unlist(strsplit("PLAY AGAIN", NULL)), 
       cex = 2, adj = c(0.5,0.5),col = colfunc(10))
  text(seq(43,57, length.out = 4), rep(20,4), labels=unlist(strsplit("QUIT", NULL)), cex=2,adj = c(0.7,0.7), col = colfunc(4))
  text(50,87,label=result,cex=3,col = "firebrick4")
  rect(33.7, 32, 65.5, 48)
  rect(42.3,12,58,27)
  

}

w <- matrix(rgb(m[,,1],m[,,2],m[,,3]), nrow=dim(m)[1])


