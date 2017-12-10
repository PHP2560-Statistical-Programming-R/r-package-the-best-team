stage0 = function(){
  n = 100
  x=c(1:n)
  y=c(1:n)
  par(mar = rep(0, 4))
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))
  taiji = readPNG("taiji.png")
  windowsFonts(JP1 = windowsFont("Pristina"),
               JP2 = windowsFont("MS Gothic"),
               JP3 = windowsFont("Arial Unicode MS"))
  bg = readJPEG("bg.jpg")
  colfunc <- colorRampPalette(c("white","goldenrod3", "white","goldenrod3","white"))
  colfunc1 = colorRampPalette(c("black","gray90"))
  ggplot(data =NULL,color = "black") + 
    geom_segment(aes(x, y = rep(1, n), xend = x, yend = rep(n,n)))+
    geom_segment(aes(x = rep(1,n), y = y, xend = rep(n,n), yend = y))+
    annotation_custom(rasterGrob(bg, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      -Inf, Inf, -Inf, Inf)+
    annotate("text", x = seq(33,67, length.out = 6), y=rep(8*n/9,6), col = colfunc(6),
             size = 14, label = unlist(strsplit("GOMOKU", NULL)), family = "JP1")+
    annotate("text", x = seq(39,61, length.out = 6), y = rep(28,6), col = colfunc1(6), label = unlist(strsplit("BATTLE",NULL)), family = "JP1", size = 10)+
    annotate("text",x = seq(33,67, length.out = 8), y = rep(13,8), col = colfunc1(8), family = "JP1", size = 10, label = unlist(strsplit("COMPUTER", NULL)))+
    annotation_custom(rasterGrob(taiji),xmin = 35, xmax = 65, ymin = 35, ymax = 80)+
    theme_void()
}


stage1 = function(){
  taiji = readPNG("taiji.png")
  windowsFonts(JP1 = windowsFont("Pristina"),
               JP2 = windowsFont("MS Gothic"),
               JP3 = windowsFont("Arial Unicode MS"))
  bg = readJPEG("bg.jpg")
  colfunc <- colorRampPalette(c("white","goldenrod3", "white","goldenrod3","white"))
  colfunc1 = colorRampPalette(c("black","gray90"))
  n = 100
  x=c(1:n)
  y=c(1:n)
  ggplot(data =NULL,color = "black") + 
    geom_segment(aes(x, y = rep(1, n), xend = x, yend = rep(n,n)))+
    geom_segment(aes(x = rep(1,n), y = y, xend = rep(n,n), yend = y))+
    annotation_custom(rasterGrob(bg, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      -Inf, Inf, -Inf, Inf)+
    annotate("text", x = seq(26,74, length.out = 8), y=rep(8*n/9,8), col = colfunc(8),
             size = 14, label = unlist(strsplit("COMPUTER", NULL)), family = "JP1")+
    annotate("text", x = 50, y = 28, label = "BLACK", col = "black", family = "JP1", size = 10)+
    annotate("text", x = 50, y = 13, label = "WHITE", col = "white", size = 10, family = "JP1")+
    annotation_custom(rasterGrob(taiji),xmin = 35, xmax = 65, ymin = 35, ymax = 80)+
    theme_void()
}

gameover = function(result, img){
  windowsFonts(JP1 = windowsFont("Pristina"),
               JP2 = windowsFont("MS Gothic"),
               JP3 = windowsFont("Arial Unicode MS"))
  bg = readJPEG("bg.jpg")
  len = str_count(result)
  n = 100
  x=c(1:n)
  y=c(1:n)
  ggplot(data =NULL,color = "black") + 
    geom_segment(aes(x, y = rep(1, n), xend = x, yend = rep(n,n)))+
    geom_segment(aes(x = rep(1,n), y = y, xend = rep(n,n), yend = y))+
    annotation_custom(rasterGrob(bg, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      -Inf, Inf, -Inf, Inf)+
    annotate("text", x = 50, y = 8*n/9, label = result, size = 14, col = "white", family = "JP1")+
    annotate("text", x = 50, y = 28, label = "PLAY AGAIN", col = "black", family = "JP1", size = 10)+
    annotate("text", x = 50, y = 13, label = "QUIT", col = "white", size = 10, family = "JP1")+
    annotation_custom(rasterGrob(img),xmin = 35, xmax = 65, ymin = 35, ymax = 80)+
    theme_void()
}



