# Run scripts to generate all figures in NAFO Solar report

run_all <- function() {
    source("src/maps.r")
    source("src/temporal.r")

    # For running with a timer function such as `system.time()`
    print("Total time taken:")
}

start <- Sys.time()
run_all()
end <- Sys.time()
print("Runtime:")
print(end - start)
