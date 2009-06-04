####################################################################
# There is a risk, although small, that the job is interrupted
# or that an error occurs while running. Regardless of if the job
# was successful, and interrupts or an error occured, the
# onFinally() function will always be called.
####################################################################
onFinally <- function(...) {
  # Clean up, e.g. close open devices, connections etc.
  graphics.off();
}
