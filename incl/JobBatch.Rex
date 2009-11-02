# To prevent R from asking us to press ENTER before each image
options(device.ask.default=FALSE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Setup a demo job directory
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
rootPath <- JobBatch$setupDemo("mandelbroot", overwrite=TRUE)
batch <- JobBatch(rootPath)

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


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# ONLY FOR REDUNDANCY TESTING OF THE PACKAGE
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
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
