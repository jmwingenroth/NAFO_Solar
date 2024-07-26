##### Run scripts to generate all figures in NAFO Solar report #####

sq_m_per_acre <- 4046.8564224
sq_m_per_sq_mi <- 2589989.1738453
watt_dc_per_ac <- 1.3
acre_per_MW <- 7.5

run_all <- function() {
    source("src/nlcd.r")
    source("src/eia_nrel.r")
    source("src/rps_states.r")
    source("src/seia.r") # depends on nlcd.r
}

start <- Sys.time()
run_all()
end <- Sys.time()

cat("Runtime:\n"); print(end - start)
