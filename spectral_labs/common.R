tau = pi * 2

# R doesn't support enums it seems, so do a poor imitation that's good enough.
E_R = 1
E_G = 2
E_B = 3
E_RGB = 4

COL_R = "#F00"
COL_G = "#0F0"
COL_B = "#00F"
COL_RG = "#FF0"
COL_RB = "#F0F"
COL_GB = "#0FF"
COL_RGB = "#FFF"
COL_K = "#000"

COL_FAV = "#e8b3d6"
COL_BRONZE = "#CD7F32"

arc = function(t1, t2, p=0, q=0, r=1)
{
  t = seq(t1 * tau, t2 * tau, 0.01 * sign(t2 - t1))
  cbind(r*cos(t) + p, r*sin(t) + q)
}


rainbow7 = c("#F00",
             "#F80",
             "#FF0",
             "#0F0",
             "#0FF",
             "#00F",
             "#80F")

RGB = c(COL_R, COL_G, COL_B)

get_rot_matrix = function(theta)
{
  matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2)
}

# Takes nx2 matrix of points and moves it to the right location and angle.
adjust_col_symbol = function(p, x, y, block = T, theta = 0)
{
  p = p %*% get_rot_matrix(theta)

  p[,1] = p[,1] + x
  p[,2] = p[,2] + y

  if (block)
  {
    p = p + 0.5
  }

  p
}

draw_col_symbol_r = function(x, y, lwd, block = T, theta = 0)
{
  # Triangle.
  h = .3 * sqrt(3)
  # a + a*sin30 = h
  # a = h / (1 + sin30)
  a = h / (1 + sin(pi/6))
  as30 = a * sin(pi/6)
  p = matrix(c(-.3, -as30,
               .3, -as30,
               0, a),
             ncol=2,
             byrow=T)

  p = adjust_col_symbol(p, x, y, block, theta)

  polygon(p, lwd=lwd)
}

draw_col_symbol_g = function(x, y, lwd, block = T, theta = 0)
{
  # 3 vertical lines.
  p = matrix(c(-.1, -.2,
               -.4, .4),
             ncol=2)
  p = adjust_col_symbol(p, x, y, block, theta)
  lines(p[,1], p[,2], lwd=lwd)

  p = matrix(c(0, 0,
               -.4, .4),
             ncol=2)
  p = adjust_col_symbol(p, x, y, block, theta)
  lines(p[,1], p[,2], lwd=lwd)

  p = matrix(c(.1, .2,
               -.4, .4),
             ncol=2)
  p = adjust_col_symbol(p, x, y, block, theta)
  lines(p[,1], p[,2], lwd=lwd)
}

draw_col_symbol_b = function(x, y, lwd, block = T, theta = 0)
{
  # 2 wavy lines.
  num_points = 27
  t = seq(0, tau, length.out=num_points)

  p = matrix(c(seq(-.3, .3, length.out=num_points),
               sin(t) / 10 - .1),
             ncol=2)
  p2 = p
  p2[,2] = p2[,2] + .2

  p = adjust_col_symbol(p, x, y, block, theta)
  p2 = adjust_col_symbol(p2, x, y, block, theta)

  lines(p[,1], p[,2], lwd=lwd)
  lines(p2[,1], p2[,2], lwd=lwd)
}

draw_col_symbol_rgb = function(x, y, lwd, block = T, theta = 0)
{
  # 2 diag lines.
  lines(c(.2, .6) + x,
        c(.4, .8) + y,
        lwd=lwd)
  lines(c(.4, .8) + x,
        c(.2, .6) + y,
        lwd=lwd)
}

draw_col_symbol = function(col, x, y, lwd, block = T, theta = 0)
{
  if (col == COL_R)
  {
    draw_col_symbol_r(x, y, lwd, block, theta)
  }
  if (col == COL_G)
  {
    draw_col_symbol_g(x, y, lwd, block, theta)
  }
  if (col == COL_B)
  {
    draw_col_symbol_b(x, y, lwd, block, theta)
  }
  if (col == COL_RGB)
  {
    draw_col_symbol_rgb(x, y, lwd, block, theta)
  }
}
