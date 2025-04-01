for(i in fs::dir_ls("source", regexp = "(r|R)$")) { source(i) }

source("0_conditions.R") 


dir.create("data", showWarnings = F)
dir.create("simout", showWarnings = F)
dir.create("results", showWarnings = F)



o <- foreach(i = 1:nrow(condition),
             .combine = 'c',
             .packages = c("glue","MplusAutomation", "dplyr", "stringr")
             ) %do% {
               
               # i = 1
               print(i)
               
               sample_size <- condition$sample_size[i]
               event_proportions <- condition$event_proportions[i]
               total_prop_events <- condition$total_prop_events[i]
               
               
               eff_size <- condition$eff_size[i] # b-path
               tp <- condition$timepoints[i]
               nrep <- condition$nrep[i]
               
               thres <- get_threshold(
                 ep = event_proportions,
                 prop_events = total_prop_events,
                 tp = tp)
               
               beta1 = rep(0.5, tp) # a-path
               ome1  = -0.5       # c-path
               gam1  = rep(eff_size, tp) # b-path
               X1    = rnorm(sample_size)
               
               data <- gen_DTSA(thresholds = thres, beta = beta1, 
                                gamma = gam1, omega = ome1, X1)
               
               data$class = 1
               
               if(F) {
                 testdata <- data %>% 
                   mutate_all(~ if_else(.x == -99, NA, .x))
                 U <- testdata %>% select(1:5)
                 (freqs <- apply(U, 2, function(x) table(x)))
                 (ncen <- apply(freqs, 2, function(x) sum(x)))
                 (hazard <- apply(freqs, 2, function(x) x / sum(x)))
               }
               
               dataname = glue("data/data_{sample_size}_{event_proportions}_{total_prop_events}_{eff_size}_{tp}_{nrep}.dat")
               
               fwrite(data, dataname, col.names = F)

               NA
             }

# stopCluster(cl)











