for(i in fs::dir_ls("source", regexp = "(r|R)$")) { source(i) }

# https://www.appliedmissingdata.com/blimp

library(rblimp)
library(tidyverse)

thres <- get_threshold(
  ep = "unif",
  prop_events = 0.4,
  tp = 5)

beta1 = rep(0.5, 5)
ome1  = -0.5
gam1  = c(0.4, 0.4, 0.4, 0.4, 0.4)
X1    = rnorm(10000)


args(gen_DTSA)
data <- gen_DTSA(thresholds = thres, beta = beta1, 
                 gamma = gam1, omega = ome1, X1,latent = F)
names(data) <- c("v1","v2","v3","v4","v5","x","z")

data <- data %>% 
  mutate_all(~ if_else(.x == -99, NA, .x))

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
iter = 10000,
burn = 10000,
chain = "2",# processors 1;",
seed = 1234)

bfit_

bfit_@syntax


# -------------------------------------------------------------------------

int.omega = rep(.2, 5)

data <- gen_DTSA(thresholds = thres, beta = beta1, 
                 gamma = gam1, omega = ome1, X1,
                 int.ome = int.omega, 
                 latent = F)
names(data) <- c("v1","v2","v3","v4","v5","x","z")

data <- data %>% 
  mutate_all(~ if_else(.x == -99, NA, .x))


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

z ~ x  v1.latent*x v2.latent*x v3.latent*x v4.latent*x v5.latent*x;
")
  ),

# parameters = c("int_1 = a1_1 * eh1;", "int_2 = a1_2 * eh2;", "int_3 = a1_3 * eh3;", "int_4 = a1_4 * eh4;", "int_5 = a1_5 * eh5;"),
data = data,
ordinal = c("v1","v2","v3","v4","v5"),
iter = 20000,
burn = 20000,
chain = "2",# processors 1;",
seed = 1232124)

bfit_
bfit_@syntax

bfit_0 <- rblimp::rblimp(
  model = (
    glue::glue("


v1 ~ 1 x@a1_1;
v2 ~ 1 x@a1_2;
v3 ~ 1 x@a1_3;
v4 ~ 1 x@a1_4;
v5 ~ 1 x@a1_5;

z ~ v1@eh1;
z ~ v2@eh2;
z ~ v3@eh3;
z ~ v4@eh4;
z ~ v5@eh5;

z ~ x  v1*x v2*x v3*x v4*x v5*x;
")
  ),

# parameters = c("int_1 = a1_1 * eh1;", "int_2 = a1_2 * eh2;", "int_3 = a1_3 * eh3;", "int_4 = a1_4 * eh4;", "int_5 = a1_5 * eh5;"),
data = data,
ordinal = c("v1","v2","v3","v4","v5"),
iter = 20000,
burn = 20000,
chain = "2",# processors 1;",
seed = 1232124)

bfit_0



# -------------------------------------------------------------------------

int.omega = rep(.2, 5)

data <- gen_DTSA(thresholds = thres, beta = beta1, 
                 gamma = gam1, omega = ome1, X1,
                 int.ome = int.omega, 
                 latent = T)
names(data) <- c("v1","v2","v3","v4","v5","x","z")

bfit_99 <- rblimp::rblimp(
  model = (
    glue::glue("


v1 ~ 1 x@a1_1;

z ~ v1.latent@eh1;

z ~ x  v1.latent*x ;
")
  ),

# parameters = c("int_1 = a1_1 * eh1;", "int_2 = a1_2 * eh2;", "int_3 = a1_3 * eh3;", "int_4 = a1_4 * eh4;", "int_5 = a1_5 * eh5;"),
data = data,
ordinal = c("v1"),
iter = 10000,
burn = 10000,
chain = "2",# processors 1;",
seed = 1232124)


bfit_98 <- rblimp::rblimp(
  model = (
    glue::glue("


v1 ~ 1 x@a1_1;

z ~ v1@eh1;

z ~ x  v1*x ;
")
  ),

# parameters = c("int_1 = a1_1 * eh1;", "int_2 = a1_2 * eh2;", "int_3 = a1_3 * eh3;", "int_4 = a1_4 * eh4;", "int_5 = a1_5 * eh5;"),
data = data,
ordinal = c("v1"),
iter = 10000,
burn = 10000,
chain = "2",# processors 1;",
seed = 1232124)
