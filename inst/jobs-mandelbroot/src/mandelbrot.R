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

mandelbrot <- function(xlim = c(-2, 0.5), ylim = c(-1,1), nx, ny = nx * diff(ylim)/diff(xlim), iter = 200, r = 2, trace = FALSE) {
    ## Purpose: Mandelbrot iteration count for nx x ny
    ##          equispaced c in  xlim + i*ylim;  #{iter; |z| > r}
    ## ----------------------------------------------------------------------
    ## Arguments: xlim, ylim: specify range of `c'
    ##            nx, ny    : number of points in x- and y-direction
    ##            iter      : number of iterations
    ##            r         : radius; if Mod(z) > r, we assume divergence
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 22 Feb 2002, 17:30
    if((nx <- as.integer(nx)) <= 0) stop("`nx' must be in 1,2,..")
    if((ny <- as.integer(ny)) <= 0) stop("`ny' must be in 1,2,..")
    if((iter <- as.integer(iter)) <= 0) stop("`iter' must be in 1,2,..")
    x <- seq(xlim[1], xlim[2], length = nx)
    if(ylim[1] > ylim[2]) stop("`ylim' is not increasing")
    Ny <- ny
    if(prod(ylim) >= 0)
        y <- seq(ylim[1], ylim[2], length = ny)
    else { ## make sure to have y = 0, and make use of symmetry around y = 0:
        symm.neg <- abs(ylim[1]) > ylim[2]
        Ny <- ceiling(ny * abs(ylim[1])/diff(ylim))
        if(trace) cat("symmetry around y = 0:  Only need Ny=",Ny,
                      " instead of ny=",ny," y-values\n")
        y <- {
            if(symm.neg) seq(ylim[1], 0, length = Ny)
            else         seq(0, ylim[2], length = Ny)
        }
    }
    acc <- cc <- outer(x, y, function(x,y) complex(real=x, imag = y))
    az <- z <- cc ## first z = 0 ==> z <- z^2 + c = c
    sml <- rep(TRUE, nx * Ny)
    isml <- 1:(nx*Ny)                   # need this really?
    res <- matrix(iter+ 1:1, nx, Ny)    #integer
    ## acc and az are the `active' values of c (= cc) and z, i.e.,
    ## acc == c(cc[sml]),  az == c(z[sml])
    for(i in 1:iter) {
        asmall <- Mod(az <- az*az + acc) <= r
        ## NOT(asmall)  are those that are newly > r
        res[isml[!asmall]] <- i
        az    <- az[asmall]
        acc  <- acc[asmall]
        isml <-isml[asmall]
        sml[sml] <- asmall
        if(trace) cat(i,": no{sml}= ", length(az),"\n")
    }
    if(prod(ylim) < 0) { ## fill `symmetry' result
        if(symm.neg) {
            i <- y >= -ylim[2] ## (-y) <= ylim[2]
            y <- c(y, -rev(y[i])[-1])
            j <- c(1:Ny, rev(which(i))[-1])
        } else {
            i <- y <= -ylim[1]
            y <- c(-rev(y[i][-1]), y)
            j <- c(rev(which(i))[-1], 1:Ny)
        }
        res <- res[, j]
    }
    list(it = iter, x = x, y = y, n.iter = res)
} # mandelbrot()

###########################################################################
# HISTORY:
# 2005-02-18
# o Extracted from Martin Maechler's Mandelbrot.R found at
#   ftp://stat.ethz.ch/U/maechler/R/ on 2005-02-18.
###########################################################################
