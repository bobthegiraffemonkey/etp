
setwd("C:/Users/damie/OneDrive/Escape this Podcast/Spectral Labs")
source("./common.R")

# Entire plot area size (starts at 0).
x_max = 5
y_max = 5 * sqrt(2)


# Need to colour in the background with a big rectangle.
background = rbind(c(0,0), c(x_max,0), c(x_max,y_max), c(0,y_max))

# Circle coordinates, starting with the top one and going clockwise.
circ_x1 = 2.5
circ_x2 = 3
circ_x3 = 2
circ_y1 = y_max - 2
circ_y2 = circ_y3 = circ_y1 - 0.5 * sqrt(3)

# Arcs (outside)
ao1 = arc(0, 1/2, circ_x1, circ_y1)
ao2 = arc(2/3, 7/6, circ_x2, circ_y2)
ao3 = arc(1/3, 5/6, circ_x3, circ_y3)

# Arcs (middle)
am12 = arc(1/3, 1/6, circ_x2, circ_y2)
am13 = arc(1/3, 1/6, circ_x3, circ_y3)
am21 = arc(0, -1/6, circ_x1, circ_y1)
am23 = arc(0, -1/6, circ_x3, circ_y3)
am31 = arc(2/3, 1/2, circ_x1, circ_y1)
am32 = arc(2/3, 1/2, circ_x2, circ_y2)

# Arcs (inner)
ai12 = arc(0, 1/6, circ_x3, circ_y3)
ai23 = arc(4/6, 5/6, circ_x1, circ_y1)
ai31 = arc(1/3, 1/2, circ_x2, circ_y2)

# Segments (probably the wrong term but meh).
seg_r = rbind(ao1, am13, am12)
seg_g = rbind(ao2, am21, am23)
seg_b = rbind(ao3, am32, am31)
seg_rg = rbind(am12, am21, ai12)
seg_gb = rbind(am23, am32, ai23)
seg_br = rbind(am31, am13, ai31)
seg_rgb = rbind(ai12, ai31, ai23)

# Fancy rainbow S, made with two sets of semicircles.
# Start with the top one (st) then calculate the bottom one from 
# that (sb).
st_x = 0.4
st_y = 2.18
# Max and min radii.
s_rmax = .3
s_rmin = .1
# radii.
s_rs = seq(s_rmax, s_rmin, len=8)

# Now construct all the semicircles in a list.
segs_st = list()
segs_sb = list()
for (ii in 1:8)
{
  # Top semicircle.
  seg_t_ii = arc(1/4 ,3/4, st_x, st_y, s_rs[ii])
  segs_st[[ii]] = seg_t_ii
  
  # Bottom.
  # Flip.
  seg_b_ii = -seg_t_ii
  # Adjust horizontally.
  seg_b_ii[,1] = seg_b_ii[,1] + (2 * st_x)
  # Adjust vertically.
  seg_b_ii[,2] = seg_b_ii[,2] + (2 * st_y) - s_rmax - s_rmin
  segs_sb[[ii]] = seg_b_ii
}

# Either draw here or draw to file.
# x11(width=480, height=480*y_max/x_max)
png("poster.png", width=480, height=480*y_max/x_max)

# Not sure if all of these are needed, but they work.
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

# Background.
polygon(background, col="#000")

# Circles.
polygon(seg_r, col=COL_R, border=NA)
polygon(seg_g, col=COL_G, border=NA)
polygon(seg_b, col=COL_B, border=NA)
polygon(seg_rg, col=COL_RG, border=NA)
polygon(seg_gb, col=COL_GB, border=NA)
polygon(seg_br, col=COL_RB, border=NA)
polygon(seg_rgb, col=COL_RGB, border=NA)

# Fancy rainbow S, first letter  of SPECTRAL.
for (ii in 1:7)
{
  polygon(segs_st[[ii]], col=rainbow7[ii], border=NA)
  polygon(segs_sb[[ii]], col=rainbow7[8-ii], border=NA)
}
# Erase the inner part so it looks like a rainbow.
polygon(segs_st[[8]], col=COL_K, border=NA)
polygon(segs_sb[[8]], col=COL_K, border=NA)

# Rest of SPECTRAL
text(seq(1, 4.6, len=7), 2,
     labels=c("P", "E", "C", "T", "R", "A", "L"), 
     cex=6, 
     col=rainbow7)


# LABS.
labs_s_sizes=seq(9,7,len=4)
labs_xs = seq(1,4,len=4)
labs = c("L", "A", "B", "S")

# LAB in RGB, same size as largest S.
for (ii in 1:3){
  text(labs_xs[ii], .8,
       labels=labs[ii],
       cex=labs_s_sizes[ii],
       col=rgb[ii])
}

# S, in RGB then white in decreasing size order.
s_cols = c(rgb, COL_rgb)
for (ii in 1:4) {
  text(labs_xs[4],.8, 
       labels=c("S"), 
       cex=labs_s_sizes[ii],
       col=s_cols[ii])
}

# Draw a line between SPECTRAL and LABS, in the colour that is the
# solution to the first light colour puzzle.
lines(c(0.2,4.8), c(1.42,1.42), col=COL_FAV, lwd=4)


dev.off()
    
