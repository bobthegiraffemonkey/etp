
source("./common.R")

xmax = 5
ymax = 5

setup_plot = function(x_max, y_max, filename, write_to_file)
{
  graphics.off()
  if (write_to_file)
  {
    png(filename, width=480, height=480)
  }
  else
  {
    x11(width=480, height=480)
  }
  
  par(mar=rep(0, 4), xpd = NA)
  plot(NULL,
       xlim = c(-x_max, x_max),
       ylim = c(-y_max, y_max),
       axes = F,
       frame.plot = F,
       xaxt='n',
       yaxt='n',
       ann = F,
       xaxs = "i",
       yaxs = "i")
}

setup_plot(xmax, ymax, "always_check_the_door.png", F)

text_xy = matrix(0, 2, 6)
words = c("full", "grid", "leaf", "out", "fixed", "white")

text_xy[,1] = c(3, 0)

pi3 = pi/3
rot60 = matrix(c(cos(pi3), -sin(pi3), sin(pi3), cos(pi3)), 2, 2)

for (ii in 2:6)
{
  text_xy[,ii] = rot60 %*% text_xy[,ii-1]
}
rots = seq(-90, len=6, by=-60)

word_cols = c(COL_G, COL_G, COL_B, COL_B, COL_R, COL_R)

p_inner = matrix(0, 2, 3)
p_inner[2,2] = 2.5
p_inner[,3] = rot60 %*% p_inner[,2]
polygon(p_inner[1,], p_inner[2,])

setup_plot(xmax, ymax, "always_check_the_door.png", F)
text(0, 3, "=", cex=3)
for (ii in 1:6)
{
  text(text_xy[1,ii], text_xy[2,ii], words[ii], srt=rots[ii], cex=4)
}

