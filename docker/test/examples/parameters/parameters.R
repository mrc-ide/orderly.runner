pars <- orderly::orderly_parameters(a = NULL, b = 2)
data <- list(a = pars$a, b = pars$b)
write.csv(data, "parameters.csv")
