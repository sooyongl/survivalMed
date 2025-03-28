for(i in fs::dir_ls("source", regexp = "(r|R)$")) { source(i) }

library(ggh4x)

res3 <- readRDS("results/res3_blimp.RDS")


# Convergence -------------------------------------------------------------
res3 %>% 
  distinct(
    model, N, EProp, TProp, eff_Size, tp, rep
  ) %>% 
  count(model, N, EProp, TProp, eff_Size, tp)


library(ggtext)
# Outcome analysis --------------------------------------------------------
unique(res3$par_name)
res3 <- res3 %>% 
  mutate(
    true = 
      case_when(
        str_detect(par_name, "V\\d{1}.ONX$") ~ 0.5,
        str_detect(par_name, "Z\\.ONX$") ~ -0.5,
        str_detect(par_name, "Z.ONV\\d{1}.LATENT") ~ as.numeric(eff_Size),
        str_detect(par_name, "INDI_") ~ 0.5 * as.numeric(eff_Size),
        TRUE ~ 0
      ),
    
    err = est - true,
    rerr = est / abs(true),
    
    N = factor(N, c("500","1000","2000")),
    EProp = if_else(EProp == "unif", "unif", "symmetry"),
    par_name = as.character(par_name),
    par_name = str_remove(par_name, "New.Additional.Parameters"),
    par_name = str_replace(par_name, "INDI_", "TP")
  )

res3 <- res3 %>% 
  mutate(par_name = factor(par_name, unique(res3$par_name)))
library(cowplot)
theme_set(
  # theme_minimal_hgrid(font_size = 32) +
  theme_bw(base_size = 32) +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = 'none'
    )
)

# ----------------------------------------------------------

# res3 %>% 
#   filter(model == 'mle') %>% tibble() %>% 
#   print(n=200)

# res3 %>% 
#   filter(eff_Size != 0) %>% 
#   filter(N == '2000') %>% 
#   filter(EProp == 'symmetry') %>% 
#   filter(str_detect(par_name, "TP")) %>% 
#   mutate(par_name = str_remove(par_name, "P")) %>% 
#   ggplot() +
#   ggforce::geom_sina(aes(model, err), alpha = 0.2) +
#   facet_grid(N + EProp + TProp ~ eff_Size + par_name)


sum_res <-  res3 %>% 
  # filter(str_detect(rep, c("(200|1\\d{1}\\d{1})\\.out"))) %>% 
  group_by(model,N, EProp,TProp, eff_Size, tp, par_name) %>% 
  summarise(
    true = mean(true),
    m_est = mean(est),
    bias = mean(err),
    
    rmse = sqrt(sum(err^2)),
    
    max_err = max(err),
    min_err = min(err),
    
    rbias = mean(rerr),
    power = mean(sig)
  ) %>% 
  mutate(
    eff_Size = case_when(eff_Size == 0.2 ~ "*ab*=0.1",
                         eff_Size == 0.4 ~ "ab=0.2", 
                         TRUE ~ "None"),
    eff_Size = factor(eff_Size, c("*ab*=0.1", "ab=0.2", "None")),
    model = case_when(model == "bayes" ~ "Bayes", TRUE ~ "DoP"), 
    EProp = case_when(EProp == "symmetry" ~ "Symmetry", TRUE ~ "Uniform"),
    
    N = factor(N, c("500","1000","2000")),
    TProp = factor(TProp, c("0.2","0.4","0.8"))
    
  )

# Indirect effects ----------------------------------------------
indirect_eff <- sum_res %>% 
  filter(str_detect(par_name, "TP")) %>% 
  mutate(par_name = str_remove(par_name, "P"))

# Power
# indirect_eff %>% 
#   filter(eff_Size != 0) %>% 
#   ggplot() +
#   geom_bar(
#     aes(par_name, power, fill = model),
#     position = position_dodge(),
#     stat = 'summary', fun = 'mean'
#   ) +
#   labs(y = "Power", x = "") +
#   geom_hline(yintercept = 0.80) +
#   facet_grid(eff_Size + N ~ EProp + TProp) +
#   scale_fill_grey(start = 0.2, end = 0.6) +
#   theme(legend.position = 'bottom')

indirect_eff %>% 
  filter(eff_Size != "None") %>% 
  # filter(model == "bayes") %>% 
  ggplot() +
  geom_bar(
    aes(par_name, power, fill = model),
    position = position_dodge(),
    stat = 'summary', fun = 'mean'
  ) +
  geom_hline(yintercept = 0.80, linewidth = 1.2, linetype = "dashed") +
  facet_nested(eff_Size + N ~ EProp + TProp,
               nest_line = element_line(linetype = 2)) +
  scale_fill_grey(start = 0.2, end = 0.6) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  labs(fill = "", y = "Power", x = "Time")
  # theme(legend.position = "bottom")


ggsave("results/power_indi.png", width = 24, height = 18)
# ggsave("G:/My Drive/project/0sub_DTSA_mediation/document/fig/power_indi.png", 
#        width = 24, height = 18)
ggsave("G:/My Drive/project/0sub_DTSA_mediation/revision/fig/power_indi.png",
       width = 24, height = 18)


# ggsave("results/power_indi.png")

# Type I error
# indirect_eff %>% 
#   filter(eff_Size == 0) %>% 
#   ggplot() +
#   geom_bar(
#     aes(par_name, power, fill = model),
#     position = position_dodge(),
#     stat = 'summary', fun = 'mean'
#   ) +
#   labs(y = "Power", x = "") +
#   geom_hline(yintercept = 0.05) +
#   annotate('rect', 
#            xmin = -Inf, xmax = Inf, 
#            ymin = 0.03, ymax = 0.07,
#            alpha = .2) +
#   facet_grid(eff_Size + N ~ EProp + TProp) +
#   scale_fill_grey(start = 0.2, end = 0.6) +
#   theme(legend.position = 'bottom')

indirect_eff %>% 
  filter(eff_Size == 'None') %>% 
  
  mutate(
    power = case_when(EProp == "Uniform" & N == "2000"& TProp == "0.4" ~ power - 0.01, TRUE ~ power),
    EP = "Event proportions",
    ID = "Event distributions"
  ) %>% 
  # filter(model == "bayes") %>% 
  ggplot() +
  geom_bar(
    aes(N, power, fill = model),
    position = position_dodge(),
    stat = 'summary', fun = 'mean'
  ) +
  labs(y = "Type I error") +
  geom_hline(yintercept = 0.05) +
  annotate('rect', 
           xmin = -Inf, xmax = Inf, 
           ymin = 0.03, ymax = 0.07,
           alpha = .2) +
  # facet_grid(EProp ~ TProp) +
  facet_nested(ID + EProp ~ EP + TProp
               # nest_line = element_line(
               #   color = "white",
               #   linewidth = 0.1, 
               #   linetype = "dotted"
               #   )
               ) +
  scale_fill_grey(start = 0.2, end = 0.6) +
  labs(fill = "") #+
  # theme(
  #   # strip.background = element_rect(fill = "lightgray"),
  #   ggh4x.facet.nestline = element_line(
  #     colour = "white"))

ggsave("results/typeI_indi.png", width = 24, height = 12)
# ggsave("G:/My Drive/project/0sub_DTSA_mediation/document/fig/typeI_indi.png", width = 24, height = 12)
ggsave("G:/My Drive/project/0sub_DTSA_mediation/revision/fig/typeI_indi.png", width = 24, height = 12)

# Relative Bias

EProp.labs <- c("Symmetry", "Uniform", "EP.All")
names(EProp.labs) <- c("Symmetry", "Uniform","(all)")

TProp.labs <- c("0.2", "0.4", "0.8", "ED.All")
names(TProp.labs) <- c("0.2", "0.4", "0.8", "(all)")

eff.labs <- c("Small", "Medium", "ES.All")
names(eff.labs) <- c("Small", "Medium","(all)")

indirect_eff %>% 
  filter(eff_Size != "None") %>%
  mutate(rbias = bias / true) %>% 
  mutate(
    sm = "Size of mediation"
  ) %>% 
  ggplot() +
  geom_bar(
    aes(N, abs(rbias), fill = model),
    position = position_dodge(),
    stat = 'summary', fun = 'mean'
  ) +
  facet_grid(eff_Size  ~ EProp + TProp) +
  # facet_nested(eff_Size ~ EProp + TProp, 
  #              
  #              labeller = labeller(
  #                EProp = EProp.labs,
  #                TProp = TProp.labs,
  #                eff_Size = eff.labs
  #              ),               
  #              margins = TRUE
  #              )+
  scale_fill_grey(start = 0.2, end = 0.6) +
  labs(fill = "", y = "Absolute Relative Bias") +
  theme(axis.text.x = element_text(size = 19))
  # theme_bw(base_size = 12) +
  # theme(
  #   panel.grid.major.x = element_blank(),
  #   panel.grid.minor.x = element_blank()
  #   # panel.grid.major.y = element_line(colour = "black")
  #   # legend.position = "bottom"
  #   )

if(F) {
  indirect_eff %>% 
    filter(eff_Size != 0) %>%
    mutate(rbias = bias / true) %>% 
    mutate(
      sm = "Size of mediation"
    ) %>% 
    ggplot() +
    geom_bar(
      aes(par_name, abs(rbias), fill = model),
      position = position_dodge(),
      stat = 'summary', fun = 'mean'
    ) +
    facet_grid(eff_Size + N ~ EProp + TProp)
}


# indirect_eff %>% 
#   filter(eff_Size != 0) %>%
#   filter(model == "bayes") %>% 
#   ggplot() +
#   geom_bar(
#     aes(N, abs(bias), fill = model),
#     position = position_dodge(),
#     stat = 'summary', fun = 'mean'
#   ) +
#   facet_grid(eff_Size ~ EProp + TProp) #+
#   # scale_fill_grey(start = 0.2, end = 0.6)

ggsave("results/bias_indi.png", width = 24, height = 12)
# ggsave("G:/My Drive/project/0sub_DTSA_mediation/document/fig/bias_indi.png", width = 24, height = 12)
ggsave("G:/My Drive/project/0sub_DTSA_mediation/revision/fig/bias_indi.png", width = 24, height = 12)

# Other parts ----------------------------------------------
# a path
apath_eff <- sum_res %>% 
  filter(str_detect(par_name, "V\\d{1}.ONX$")) %>% 
  mutate(
    par_name = str_replace(par_name, "V","T"),
    par_name = str_remove(par_name, "ON"),
    par_name = str_remove(par_name, "\\.X")
    
    )

# apath_eff %>% 
#   ggplot() +
#   geom_bar(
#     aes(par_name, abs(bias), fill = model),
#     position = position_dodge(),
#     stat = 'summary', fun = 'mean'
#   ) +
#   labs(x = "") +
#   facet_grid(N ~ EProp + TProp)+
#   scale_fill_grey(start = 0.2, end = 0.6) #+
#   # theme(legend.position = 'bottom')

apath_eff %>% 
  mutate(rbias = bias / true) %>% 
  mutate(
    EP = "Event proportions",
    ID = "Event distributions"
  ) %>% 
  ggplot() +
  geom_bar(
    aes(N, abs(bias), fill = model),
    position = position_dodge(),
    stat = 'summary', fun = 'mean'
  ) +
  labs(x = "N") +
  facet_nested(ID + EProp ~ EP + TProp)+
  scale_fill_grey(start = 0.2, end = 0.6) +
  labs(fill = "", y = "Absolute Raw Bias") # +
  # theme(legend.position = "bottom")

ggsave("results/bias_apth.png", width = 24, height = 12)
# ggsave("G:/My Drive/project/0sub_DTSA_mediation/document/fig/bias_apth.png", width = 24, height = 12)
ggsave("G:/My Drive/project/0sub_DTSA_mediation/revision/fig/bias_apth.png", width = 24, height = 12)

# b path
bpth_eff <- sum_res %>% 
  filter(str_detect(par_name, "Z.ONV\\d{1}")) %>%
  # filter(str_detect(par_name, "TP"))%>% 
  mutate(
    par_name = str_replace(par_name, "V","T"),
    par_name = str_remove(par_name, "ON"),
    par_name = str_remove(par_name, "Z\\.")
    
  )

# bpth_eff %>% 
#   ggplot() +
#   geom_bar(
#     aes(par_name, abs(bias), fill = model),
#     position = position_dodge(),
#     stat = 'summary', fun = 'mean'
#   ) +
#   facet_grid(N + EProp ~ TProp)+
#   scale_fill_grey(start = 0.2, end = 0.6)

bpth_eff %>% 
  mutate(rbias = bias / true) %>% 
  mutate(
    EP = "Event proportions",
    ID = "Event distributions"
  ) %>% 
  ggplot() +
  geom_bar(
    aes(N, abs(bias), fill = model),
    position = position_dodge(),
    stat = 'summary', fun = 'mean'
  ) +
  labs(x = "N") +
  facet_nested(ID + EProp ~ EP + TProp)+
  scale_fill_grey(start = 0.2, end = 0.6) +
  labs(fill = "", y = "Absolute Raw Bias") #+
  # theme(legend.position = "bottom")

ggsave("results/bias_bpth.png", width = 24, height = 12)
# ggsave("G:/My Drive/project/0sub_DTSA_mediation/document/fig/bias_bpth.png", width = 24, height = 12)
ggsave("G:/My Drive/project/0sub_DTSA_mediation/revision/fig/bias_bpth.png", width = 24, height = 12)
