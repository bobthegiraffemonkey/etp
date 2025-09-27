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
