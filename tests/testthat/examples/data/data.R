orderly2::orderly_artefact(description = "Some data", "data.rds")
d <- data.frame(a = 1:10, x = runif(10), y = 1:10 + runif(10))
saveRDS(d, "data.rds")
