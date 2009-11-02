### Copyright (C) 2002 Martin Maechler
###
### This is free software; you can redistribute it and/or modify it under
### the terms of the GNU General Public License as published by the Free
### Software Foundation; either version 2 of the License, or (at your
### option) any later version.
###
### The software is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
### or FITNESS FOR A PARTICULAR PURPOSE.
### See the GNU General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with R; if not, you can obtain it via the World Wide Web at
### 'http://www.gnu.org/copyleft/gpl.html', or by writing to the Free
### Software Foundation, 59 Temple Place -- Suite 330, Boston, MA
### 02111-3307, USA.

iterImage <- function(xyi, n.col = 16,
                      col.type = c("heat", "rainbow", "terrain", "topo", "cm",
                      "gray", "zebra", "b&w"), col = NULL,
                      breaks = c(quantile(n.it[n.it < i.mx],
                                 probs=seq(0,1, len=n.col),names=FALSE), i.mx),
                      asp = 1, frame.plot = FALSE,
                      xlab = "Re(c)", ylab = "Im(z)",
                      tit = "", main = "Set & #{iter.}")
{
  ## Purpose: image() plot of mandelbrot like result
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 22 Feb 2002, 19:02
    if(any(dim(n.it <- xyi$n.it) !=
       c(nx <- length(x <- xyi$x), ny <- length(y <- xyi$y))))
        stop("invalid `xyi' argument")
    if(is.null(col)) {
        col.type <- match.arg(col.type)
        n1 <- n.col - 1
        col <- c(switch(col.type,
                        heat   = rev(heat.colors(n1)),
                        rainbow= {k <- n1%/%6 ; rainbow(n1)[c(k:n1,1:(k-1))]},
                        terrain= rev(terrain.colors(n1)),
                        topo   = rev(topo.colors(n1)),
                        cm     = rev(  cm.colors(n1)),
                        gray   = gray((1:(n1))/n.col),
                        "b&w"=rep(c("white","black"), len = n1),
                        zebra=rep(c("gray85","gray30"),len = n1),
                        ), "black")
    } else col.type <- "user specified"
    i.mx <- max(n.it)
    image(x, y, n.it, breaks = breaks, col = col,
          asp = asp, frame.plot = frame.plot,
          xlab = xlab, ylab = ylab, main = paste(tit, main))
    mtext(paste(nx," x ",ny,"  points  --- color type: \"", col.type,"\",  ",
                ## should do the following only when i.mx > 1
                col[length(col)]," means `> ",i.mx - 1," iter.'", sep=""))
} # iterImage()

###########################################################################
# HISTORY:
# 2005-02-18
# o Extracted from Martin Maechler's Mandelbrot.R found at
#   ftp://stat.ethz.ch/U/maechler/R/ on 2005-02-18.
###########################################################################
