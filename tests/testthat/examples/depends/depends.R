orderly2::orderly_dependency("data", "latest", files = "data.rds")
numbers <- readRDS("data.rds")
saveRDS(sum(numbers), "sum.rds")
