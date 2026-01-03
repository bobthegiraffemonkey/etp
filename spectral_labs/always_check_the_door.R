
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

setup_plot(xmax, ymax, "always_check_the_door.png", T)

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

p_inner = matrix(0, 2, 3)
p_inner[1,2] = 2.5 * sin(pi/64)
p_inner[2,2] = 2.5 * cos(pi/64)
p_inner[,3] = rot60 %*% p_inner[,2]
p_inner_cols = c(COL_B, COL_B, COL_R, COL_R, COL_G, COL_G)

p_outer = matrix(0, 2, 4)
p_outer[,1] = p_inner[,2]
p_outer[,4] = p_inner[,3]
p_outer[,2] = p_outer[,1] * 1.6
p_outer[,3] = p_outer[,4] * 1.6

p_sym = t((p_inner[,2] * .5) %*% get_rot_matrix(-pi/6))
rot_sym = -pi/64 - pi/6

text(0, 3, "=", cex=3)
for (ii in 1:6)
{
  text(text_xy[1,ii], text_xy[2,ii], words[ii], srt=rots[ii], cex=4)
  
  polygon(p_inner[1,], p_inner[2,], lwd=6, border = COL_BRONZE, col=p_inner_cols[ii])
  polygon(p_outer[1,], p_outer[2,], lwd=6, border = COL_BRONZE)
  draw_col_symbol(p_inner_cols[ii],
                  p_sym[1], p_sym[2],
                  4,
                  F,
                  rot_sym)

  p_inner = rot60 %*% p_inner
  p_outer = rot60 %*% p_outer
  p_sym = rot60 %*% p_sym
  rot_sym = rot_sym - pi/3
}

dev.off()
