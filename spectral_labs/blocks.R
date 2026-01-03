
source("./common.R")


draw_square = function(x, y, col, lwd=1)
{
  if (col == COL_RGB)
  {
    border = COL_K
  }
  else
  {
    border = col
  }

  polygon(x + c(0, 1, 1, 0),
          y + c(0, 0, 1, 1),
          col=col,
          lwd=lwd)
}


BLOCK_LINE_WIDTH = 4
SYMBOL_LINE_WIDTH = 4

draw_block = function(x, y, rgb)
{
  if (rgb == E_RGB)
  {
    draw_square(x, y, COL_RGB, BLOCK_LINE_WIDTH)
  }
  else
  {
    draw_square(x, y, RGB[rgb], BLOCK_LINE_WIDTH)
  }
  
  if (rgb == E_R)
  {
    draw_col_symbol_r(x, y, SYMBOL_LINE_WIDTH)
  }
  else if (rgb == E_G)
  {
    draw_col_symbol_g(x, y, SYMBOL_LINE_WIDTH)
  }
  else if (rgb == E_B)
  {
    draw_col_symbol_b(x, y, SYMBOL_LINE_WIDTH)
  }
  else if (rgb == E_RGB)
  {
    draw_col_symbol_rgb(x, y, SYMBOL_LINE_WIDTH)
  }
}

draw_grid = function()
{
  for (ii in 1:5){
    lines(c(ii, ii), c(1, 5), lwd=BLOCK_LINE_WIDTH)
    lines(c(1, 5), c(ii, ii))
  }
  
  for (ii in 1:4)
  {
    lines(c(.1, .9) + ii, c(.9, .9), lwd=2)
  }
}

xmax = 10
ymax = 10

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
       xlim = c(0, x_max),
       ylim = c(0, y_max),
       axes = F,
       frame.plot = F,
       xaxt='n',
       yaxt='n',
       ann = F,
       xaxs = "i",
       yaxs = "i")
}

draw_base_puzzle = function(write_to_file)
{
  setup_plot(9, 9, "block_base.png", write_to_file)
  draw_grid()
  draw_block(2, 3, E_R)
  draw_block(1, 2, E_G)
  draw_block(3, 1, E_B)
  draw_block(4, 1, E_RGB)
    
  draw_block(6, 1, E_R)
  draw_block(6, 2, E_R)
  draw_block(7, 2, E_R)
  draw_block(7, 3, E_R)
  
  draw_block(5, 7, E_G)
  draw_block(6, 7, E_G)
  draw_block(6, 6, E_G)
  draw_block(6, 5, E_G)
  
  draw_block(1, 6, E_B)
  draw_block(2, 6, E_B)
  draw_block(3, 6, E_B)
  draw_block(2, 7, E_B)
  
  if (write_to_file) {dev.off()}
}

draw_soln_line = function(x1, x2, y1, y2, col)
{
  lines(c(x1, x2) + .5,
        c(y1, y2) + .5,
        col=col,
        lwd=10)
}

draw_answer = function(answer)
{
  text(1:4 + .5, rep(.5, 4), answer)
}

draw_soln_r = function(write_to_file)
{
  setup_plot(6, 6, "block_soln_r.png", write_to_file)
  draw_grid()
  draw_block(2, 3, E_R)
  draw_block(1, 2, E_G)
  draw_block(3, 1, E_B)
  draw_block(4, 1, E_RGB)
  
  draw_soln_line(1, 2, 2, 2, COL_R)
  draw_soln_line(2, 2, 2, 1, COL_R)
  draw_soln_line(2, 3, 1, 1, COL_R)
  
  draw_soln_line(2, 3, 3, 3, COL_G)
  draw_soln_line(3, 3, 3, 1, COL_G)
  
  draw_soln_line(1, 1, 2, 4, COL_B)
  draw_soln_line(1, 2, 3, 3, COL_B)
  
  answer = c(1, 3, 1, 1)
  draw_answer(answer)
  
  if (write_to_file) {dev.off()}
}

draw_soln_g = function(write_to_file)
{
  setup_plot(6, 6, "block_soln_g.png", write_to_file)
  draw_grid()
  draw_block(2, 3, E_R)
  draw_block(1, 2, E_G)
  draw_block(3, 1, E_B)
  draw_block(4, 1, E_RGB)
  
  draw_soln_line(1, 2, 1, 1, COL_R)
  draw_soln_line(2, 2, 1, 2, COL_R)
  draw_soln_line(2, 3, 2, 2, COL_R)
  
  draw_soln_line(1, 1, 3, 4, COL_G)
  draw_soln_line(1, 3, 4, 4, COL_G)
  
  draw_soln_line(3, 4, 3, 3, COL_B)
  draw_soln_line(4, 4, 2, 4, COL_B)

  answer = c(3, 1, 1, 1)
  draw_answer(answer)
  
  if (write_to_file) {dev.off()}
}

draw_soln_b = function(write_to_file)
{
  setup_plot(6, 6, "block_soln_b.png", write_to_file)
  draw_grid()
  draw_block(2, 3, E_R)
  draw_block(1, 2, E_G)
  draw_block(3, 1, E_B)
  draw_block(4, 1, E_RGB)
  
  answer = c(0, 0, 1, 1)
  draw_answer(answer)
  
  if (write_to_file) {dev.off()}
}

draw_base_puzzle(T)
draw_soln_r(T)
draw_soln_g(T)
draw_soln_b(T)
