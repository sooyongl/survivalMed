for(i in fs::dir_ls("source", regexp = "(r|R)$")) { source(i) }

files <- fs::dir_ls("data", regexp = "dat$")


cores <- parallel::detectCores() - 2
cl <- parallel::makeCluster(cores)
doSNOW::registerDoSNOW(cl)

pb <- txtProgressBar(max=length(files), style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

# Bayesian -------------------------------------------------------
o <- foreach(i = 1:length(files),
             .combine = 'c',
             .packages = c("glue","MplusAutomation", "dplyr", "stringr"),
             .options.snow = opts) %dopar% {

               # Blimp #####################
               savename <- str_replace(files[i], "data","simout")
               savename <- str_replace(savename, "data","bayes")
               savename <- str_replace(savename, "dat","RDS")

               data <- fread(files[i])
               conds <- str_split(files[i], "_")[[1]]
               tp <- conds[6]
               names(data) <-  c(paste0("v",1:tp), "x","z","class")

               data <- data %>%
                 mutate_all(
                   ~ if_else(.x == -99, NA, .x)
                 )

               events <- lapply(1:tp, function(x) {
                 paste0("v",x, " ~ 1 x@a1_", x, ";")
               })
               events <- paste(unlist(events), collapse = "\n")

               beff <- paste(
                 paste0("z ~ v",1:tp, ".latent@eh", 1:tp, ";"), collapse = "\n")


               medeff  <- lapply(1:tp, function(x) {
                 paste0("int_",x, " = a1_", x, " * eh",x,";")
               })
               medeff <- unlist(medeff)


               bfit_ <- rblimp::rblimp(
                 model = (
                   glue::glue("

  {events}

  {beff}

  z ~ x ;
")
                 ),

parameters = c(medeff),
data = data,
ordinal = paste0("v",1:tp),
iter = 5000,
burn = 5000,
chain = "2 processors 1;",
options = 'prior2',
seed = 1234)

               saveRDS(bfit_, savename)

               NA
               }
stopCluster(cl)

