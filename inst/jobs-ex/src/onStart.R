####################################################################
# The onStart() is called before onRun().
####################################################################
onStart <- function(...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Find a working PNG device
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  pngDevice <<- System$findGraphicsDevice(list(png,png2));
  if (is.null(pngDevice))
    cat("Working not PNG device not found. Will not save images.");
}
