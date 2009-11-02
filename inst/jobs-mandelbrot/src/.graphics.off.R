####################################################################
# This file is just to fool R CMD check and is not needed otherwise.
####################################################################
graphics.off <- function() {
  # If R CMD check is running, we must not close the 2nd device
  # which was opened by R CMD check. 

  # Detect if R CMD check is running (see for instance *.Rout)
  isRCmdCheck <- ("CheckExEnv" %in% search());
  if (isRCmdCheck) {
    # Close all but the 2nd device opened by R CMD check.
    while(TRUE) {
      which <- dev.cur();
      if (which == 1 || (which == 2 && names(which) == "postscript"))
        break;
      dev.off(which);
    }
    dev.cur();
  } else {
    grDevices::graphics.off();
  }
}
