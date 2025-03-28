for(i in fs::dir_ls("source", regexp = "(r|R)$")) { source(i) }

# Conditions?
# sample_size = c(500, 1000, 2000)
sample_size = c(500)

# event_proportions <- c("unif","symetry") #,"asymetry")
event_proportions <- c("unif") #,"asymetry")

# total_prop_events = c(0.2, 0.4, 0.8)
total_prop_events = c(0.4)

# logistic interaction 
# eff_size = c(0, 0.2, 0.4)
eff_size = c(0.4)

# Equal or varying across timepoints?
# eqaul = c()

timepoints = c(5)

condition0 <- crossing(sample_size, 
                      event_proportions, 
                      total_prop_events,
                      eff_size,
                      timepoints)
nrow(condition0)
nrep = 1

condition <- crossing(condition0, nrep = 1:nrep)

# =======