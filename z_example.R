for(i in fs::dir_ls("source", regexp = "(r|R)$")) { source(i) }

library(rblimp)
library(tidyverse)

thres <- get_threshold(
  ep = "unif",
  prop_events = 0.4,
  tp = 5)

beta1 = rep(0.5, 5)
ome1  = -0.5
gam1  = c(0.4, 0.4, 0.4, 0.4, 0.4)
X1    = rnorm(1000)

data <- gen_DTSA(thresholds = thres, beta = beta1, 
                 gamma = gam1, omega = ome1, X1)
names(data) <- c("v1","v2","v3","v4","v5","x","z")

bfit_ <- rblimp::rblimp(
  model = (
    glue::glue("


v1 ~ 1 x@a1_1;
v2 ~ 1 x@a1_2;
v3 ~ 1 x@a1_3;
v4 ~ 1 x@a1_4;
v5 ~ 1 x@a1_5;

z ~ v1.latent@eh1;
z ~ v2.latent@eh2;
z ~ v3.latent@eh3;
z ~ v4.latent@eh4;
z ~ v5.latent@eh5;

z ~ x ;
")
  ),

parameters = c("int_1 = a1_1 * eh1;", "int_2 = a1_2 * eh2;", "int_3 = a1_3 * eh3;", "int_4 = a1_4 * eh4;", "int_5 = a1_5 * eh5;"),
data = data,
ordinal = c("v1","v2","v3","v4","v5"),
iter = 5000,
burn = 5000,
chain = "2 processors 1;",
seed = 1234)


bfit_