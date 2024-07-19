# Run scripts to generate all figures in NAFO Solar report

sq_m_per_acre <- 4046.8564224
sq_m_per_sq_mi <- 2589989.1738453

run_all <- function() {
    source("src/nlcd.r")
    source("src/eia.r")
}

start <- Sys.time()
run_all()
end <- Sys.time()

cat("Runtime:\n"); print(end - start)
