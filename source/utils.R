library(foreach)
library(doSNOW)
library(parallel)
library(rblimp)
library(glue)
library(MplusAutomation)
library(tidyverse)
library(lavaan)
# library(data.table)

fread <- data.table::fread
fwrite <- data.table::fwrite
select <- dplyr::select
# install.packages('RMediation')
library(RMediation)

dist_prod <- function(a, a.se, b, b.se) {

  # a = -0.103
  # b = -0.078
  # a.se = 0.046
  # b.se = 0.029
  ci <- RMediation::medci(a, b, a.se, b.se, type = "prodclin", alpha = 0.05)
  # ub<- RMediation::qprodnormal(p = 0.975, a, b, a.se, b.se, type = "all")
  # lb <- RMediation::qprodnormal(p = 0.025, a, b, a.se, b.se, type = "all")
  
  # medieff <- a * b
  # medieff.se <- sqrt(a*b.se + b*a.se)
 
  
  ci
  # c(medieff + ub*medieff.se, medieff + lb*medieff.se)
  
  
  
}



get_threshold <- function(ep, prop_events, tp) {
  # prop_events = total_prop_events[1]
  
  if(ep == "unif") {
    
    prop0  = prop_events / tp
    
    z_score <- rep(qnorm(prop0), tp)
    
  } else if(ep == "symetry") {
    
    if(tp == 5) {
      
      symprop <- c(0.15,  0.2, 0.3, 0.2, 0.15)
      # c(0, -0.5, -0.7, -0.9)
      
    } else if(tp == 7) {
      symprop <- c(0.05,  0.1, 0.2, 0.3, 0.2, 0.1, 0.05)
      
    }
    
    prop0 = symprop * prop_events
    z_score <- qnorm(prop0)
    
  } else if(ep == "asymetry") {
    
    if(tp == 5) {
      asymprop <- c(0.4,  0.3, 0.15, 0.1, 0.05); # sum(asymprop)
    } else if(tp == 7) {
      asymprop <- c(0.3,  0.2, 0.15, 0.1, 0.1, 0.1, 0.05); # sum(asymprop)
    }
    
    # prop0 = asymprop * prop_events
    prop0 = symprop * prop_events
    z_score <- qnorm(prop0)
  }
  
  round(z_score,3)
  
  # logits = log(prop0 / (1 - prop0))
  # -round(logits,3)
}

gen_DTSA <- function(thresholds, beta, gamma = rep(0.4, 5), omega = -0.2, X1) {
  
  N = length(X1)
  tp <- length(thresholds)
  
  # Generate u_star
  u_star <- matrix(0, ncol = tp, nrow = N)
  U <- matrix(0, ncol = tp, nrow = N)
  for(i in 1:tp) { # i = 1
    u_star[,i] <- thresholds[i] + beta[i] * X1 + rnorm(N, 0, 1)
    
    U[,i] = u_star[,i]
    U[u_star[,i] <= 0, i] <- 0 # tau fixed to 0; \nu is estimated instead.
    U[u_star[,i] > 0, i] <- 1
  }
  
  # Gen distal outcome
  ## Use observed binary mediator
  # Y <- U %*% gamma + omega*X1 + rnorm(N, 0, 1)
  
  ## Use Latent propensity mediator
  Y <- u_star %*% gamma + omega*X1 + rnorm(N, 0, 1)
  
  if(F) {
    summary(lm(Y ~  X1.1+X2+X3+X4+X5 + X1, data = data.frame(Y, u_star, X1)))
    
    summary(lm(Y ~  X1.1+X2+X3+X4+X5 + X1, data = data.frame(Y, U, X1)))
  }
  
  # censoring
  for(i in 1:(tp-1)) { # i = 1
    rightcen <- U[, i] == 1 | is.na(U[, i])
    U[rightcen, i+1] <- NA
  }
  
  data.frame(U, X1, Y) %>% 
    mutate_all(~ if_else(is.na(.x), -99, .x))
}