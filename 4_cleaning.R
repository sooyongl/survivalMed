for(i in fs::dir_ls("source", regexp = "(r|R)$")) { source(i) }

source("0_conditions.R") 

blimpout <- fs::dir_ls("simout", regexp = "RDS$"); length(blimpout)

bayes <- sort(blimpout[str_detect(blimpout, "bayes")])

res0 <- vector('list', length(bayes))
for(i in 1:length(bayes)) {
  # i = 1
  print(i)
  
  a1 <- suppressWarnings(readRDS(bayes[i]))
  a1 <- data.frame(a1@estimates)
  a1$par_name <- rownames(a1)

  a1.1 <- a1 %>%
    mutate(
      sig = case_when(X2.5. < 0 & X97.5. > 0 ~ 0,
                      TRUE ~ 1),

      par_name = toupper(par_name),
      par_name = str_replace(par_name, " ~ ", ".ON"),
      par_name = str_replace(par_name, "PARAMETER: INT", "New.Additional.ParametersINDI"),
      par_name = str_replace(par_name, "Z RESIDUAL VARIANCE", "Residual.VariancesZ"),
      par_name = str_replace(par_name, "Z ~ INTERCEPT", 'InterceptsZ'),
      par_name = str_replace(par_name, ".ONINTERCEPT", ''),

      par_name =
        case_when(str_detect(par_name, "^V\\d{1}$") ~ paste0("Thresholds",par_name,"$1"),
                  TRUE ~ par_name),

      par_name =
        case_when(str_detect(par_name, "^Z$") ~ paste0("Intercepts",par_name),
                  TRUE ~ par_name)
    ) %>%
    filter(str_detect(par_name, "(ParametersINDI|Residual.Variances|\\$1|Z$|V\\d{1}$|V\\d{1}.LATENT|X$)")) %>%
    filter(!str_detect(par_name, "(STANDA)")) %>%
    # mutate(par_name = factor(par_name, levels = a0.1$par_name)) %>%
    rename("est" = "Median", "se"="StdDev") %>%
    arrange(par_name)
  
  a1.1$par_name_blimp <- rownames(a1.1)
  
  res0[[i]] <- 
    
    a1.1 %>% mutate(model = "2") %>% 
    
    mutate(
      model = if_else(model == "1", "mle","bayes"),
      filename = str_remove(bayes[i], "simout/bayes_")
    )
}

res1 <- do.call('rbind', res0)

res2 <- res1 %>% 
  separate(filename, c("N", "EProp","TProp", "eff_Size", "tp","rep"), "_")

saveRDS(res2, "results/res3_blimp.RDS")

