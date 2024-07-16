# Run scripts to generate all figures in NAFO Solar report

run_all <- function() {
    source("src/nlcd.r")
    source("src/eia.r")
}

start <- Sys.time()
run_all()
end <- Sys.time()

cat("Runtime:\n"); print(end - start)
