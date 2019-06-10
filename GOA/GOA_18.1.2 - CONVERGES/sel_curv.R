slp <- ss_run$estimated_params$srv_sel_slp[,5]
inf <-  ss_run$estimated_params$srv_sel_inf[,5]

slp <- c(0, 0)

curve((1/(1+exp(-exp(slp[1]) * (x - inf[1])))) * (1 - (1 / (1 + exp(-exp(slp[2]) * (x - inf[2]))))), from = 0, to = 10, ylab = "Selectivity", xlab = "Age")
