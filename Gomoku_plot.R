stage0 = function(){
plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
     ylab = "", bty = "o", lab = c(n, n, 1))
text(50,80,label="Gomoku",cex=5)
text(50,40,label="Single",cex=2,col="darkred")
text(50,20,label="Computer",cex=2,col="darkblue")
rect(39.7, 33, 59.7, 47)
rect(35,12,65,27)
l = locator(1)
print(l)
}

stage1 = function(){
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))
  text(50,80,label="Computer",cex=5)
  text(50,40,label="Black",cex=2,col=4)
  text(50,20,label="White",cex=2,col=2)
  rect(39.7, 32, 59.7, 46)
  rect(39.7,12,59.7,27)
  theme_minimal()
}

gameover = function(game, choose){
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))
  text(50,80,label="Somebody Wins!",cex=3)
  text(50,40,label="Play Again",cex=2,col=4)
  text(50,20,label="Quit",cex=2,col=2)
  rect(33.7, 32, 65.5, 48)
  rect(42.3,12,58,27)

}