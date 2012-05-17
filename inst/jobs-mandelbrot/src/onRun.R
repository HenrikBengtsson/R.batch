####################################################################
# The onRun() function is the main function for the job. It is 
# called after the onStart() function. onFinally() is always called
# after onRun() or if an interrupt or an error occurs.
# The Job object is passed as the first argument, which we refer
# here to as 'job'.
####################################################################
onRun <- function(job, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Pre-processing
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  nx <- as.integer(nx);
  title <- "Mandelbrot";

  # Create a label for the given region of the Mandelbrot set
  mandelbrotName <- sprintf("mandelbrot_%+.8g,%+.8g_%.8g", 
                               center[1], center[2], size);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Region
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  xlim <- center[1] + size/2*c(-1,1);
  ylim <- center[2] + size/2*c(-1,1);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Verbose
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  cat("\n");
  cat("Mandelbrot set:\n");
  cat("---------------\n");
  cat(sprintf("Center: (%+.8g,%+.8g)\n", center[1], center[2]));
  cat(sprintf("Size: %+.8g\n", size));
  cat(sprintf("<=> Region: (%+.8g,%+.8g) x (%+.8g,%+.8g)\n", 
                              xlim[1], xlim[2], ylim[1], ylim[2]));
  cat("Width and height: ", nx, "x", nx, "\n", sep="");
  cat("Number of iterations: ", iter, "\n", sep="");
  cat("\n");

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Find the Mandelbrot set for given region
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  fit <- mandelbrot(xlim=xlim, ylim=ylim, nx=nx, iter=iter)

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Save set to file
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  dataname <- filename <- paste(mandelbrotName, ".RData", sep="");
  pathname <- file.path(getOutputPath(job), filename);
  mandelbrotSet <- list(
    center=center,
    size=size,
    xlim=xlim,
    ylim=ylim,
    iter=iter,
    nx=nx,
    fit=fit
  )

  save(mandelbrotSet, file=pathname);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Generate images
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  images <- list(
   topo=64, 
   terrain=99, 
   heat=64, 
   rainbow=77, 
   cm=30, 
   gray=30, 
   zebra=30, 
   "b&w"=30
  );

  # For each color type and number of color levels...
  for (kk in seq(images)) {
    # Hot-plugins
    sourceHotCode(job);

    colorType <- names(images)[kk];
    nbrOfColors <- as.integer(images[[kk]]);
    imageName <- sprintf("%s_%s-%d_%dx%d", mandelbrotName, 
                                           colorType, nbrOfColors, nx, nx);

    printf("Creating image: %s\n", imageName);
    toPNG(imageName, path=getOutputPath(job), {
      iterImage(fit, n.col=nbrOfColors, col.type=colorType, tit=title);
    });

    cat("\n");
  } # for (kk ...)
} # onRun()
