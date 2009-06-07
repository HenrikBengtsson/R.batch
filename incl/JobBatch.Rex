# To prevent R from asking us to press ENTER before each image
options(device.ask.default=FALSE)

# Create a job batch
batch <- JobBatch("jobs-ex")

# Copy example for generating multiple Mandelbrot sets.
# The Mandelbrot code was written by Martin Maechler. See
# files in system.file("jobs-ex/src", package="R.batch")
# for details.
exBatch <- JobBatch(system.file("jobs-ex", package="R.batch"))
copyFrom(batch, exBatch, conflicts="overwrite")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Now, imaging that this code was running on several 
# different host all with access to the job directory.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
print(batch)

# Process jobs until no more exist or no more 
# can be retrieved.
run(batch, verbose=-150)

print(batch)

print(list.files(getOutputPath(batch)))


# Assert that jobs in batch ended up where expected.
cat("Validation JobBatch results...\n");
expected <- list(erroneous="job05", failed="job04", interrupted=NULL,
                    finished=c("job01", "job02", "job03"), running=NULL);
for (name in names(expected)) {
  jobs <- getSummary(batch)[[name]]$jobs;
  if (!identical(jobs, expected[[name]])) {
    msg <- paste("Final directory '", name, 
                 "' does not contain expected jobs: ", 
                 paste(jobs, collapse=", "), " != ", 
                 paste(expected[[name]], collapse=","));
    cat(msg, "\n");
    stop(msg);
  }
}
cat("Validation JobBatch results...done\n");
