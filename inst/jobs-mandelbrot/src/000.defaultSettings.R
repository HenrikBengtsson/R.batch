######################################################################
# Here we setup the default parameters
######################################################################

# Default Mandelbrot region
center <- c(0.282, -0.01)
size   <- 0.02

# Default number of iterations
iter <- 100L

# Default "resolution" (nx-by-nx matrix)
nx <- 150L

# Default width and height of saved images
devOptions("png", width=480, height=480)
