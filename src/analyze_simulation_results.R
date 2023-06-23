library(tidyverse)
library(lme4)
library(nnet)

#setwd("C:/Users/conno/git_repos/bracing_simulations")
setwd("E:/git_repos/asymmetrical_bracing/")


data_folder <- "data/50-150_simulations"

contacts_df <- tibble()
excitation_df <- tibble()
failures_df <- tibble()

contacts_headers <- c(
  "sim", "front", "mid", "back", "lat_left", "lat_right", "coronal", "condition"
)
excitation_headers <- c(
  "sim", "IL_L", "IL_R", "GGM_L", "GGM_R", "GGP_L", "GGP_R", "VERT_L", "VERT_R",
  "HG_L", "HG_R", "STY_L", "STY_R", "MH_L", "MH_R", "SL", "TRANS_L", "TRANS_R",
  "GGA_L", "GGA_R", "condition"
)
failures_headers <- c(
  "IL_L", "IL_R", "GGM_L", "GGM_R", "GGP_L", "GGP_R", "VERT_L", "VERT_R",
  "HG_L", "HG_R", "STY_L", "STY_R", "MH_L", "MH_R", "SL", "TRANS_L", "TRANS_R",
  "GGA_L", "GGA_R", "condition"
)

for (f in list.files(data_folder)) {
  full_path <- file.path(data_folder, f)
  data <- read_csv(full_path, col_names=FALSE,col_types = cols())
  data$condition <- '50'
  if (str_detect(f, "contacts")) {
    contacts_df <- rbind(contacts_df, data)
  } else if (str_detect(f, "failed")) {
    failures_df <- rbind(failures_df, data)
  } else if (str_detect(f, "excitations")) {
    excitation_df <- rbind(excitation_df, data)
  }
}

colnames(contacts_df) <- contacts_headers
colnames(excitation_df) <- excitation_headers
colnames(failures_df) <- failures_headers

success_df <- inner_join(contacts_df, excitation_df, by = c("sim", "condition"))

groupings_df <- read_csv("data/asymmetrical_activations.csv")
colnames(groupings_df)[1] <- 'sim'

success_df <- inner_join(success_df, groupings_df, by=c('sim', "IL_L", "IL_R", "GGM_L", "GGM_R", "GGP_L", "GGP_R", "VERT_L", "VERT_R",
                                                        "HG_L", "HG_R", "STY_L", "STY_R", "MH_L", "MH_R", "SL", "TRANS_L", "TRANS_R",
                                                        "GGA_L", "GGA_R"))

success_df$condition <- paste("Agonists:", success_df$L_AG, "Antagonists", success_df$L_ANT)
success_df <- success_df %>% filter(SL_ACT == 1)

mean_contacts <- success_df %>%
  select(condition, front, mid, back, lat_left, lat_right, coronal) %>%
  group_by(condition) %>%
  summarise(across(c(front, mid, back, lat_left, lat_right, coronal), mean, na.rm=TRUE))

mean_activations <- success_df %>%
  select(condition, IL_L, IL_R, GGM_L, GGM_R, GGP_L, GGP_R, VERT_L, VERT_R,
         HG_L, HG_R, STY_L, STY_R, MH_L, MH_R, SL, TRANS_L, TRANS_R,
         GGA_L, GGA_R) %>%
  group_by(condition) %>%
  summarise(across(c(IL_L, IL_R, GGM_L, GGM_R, GGP_L, GGP_R, VERT_L, VERT_R,
                     HG_L, HG_R, STY_L, STY_R, MH_L, MH_R, SL, TRANS_L, TRANS_R,
                     GGA_L, GGA_R), mean, na.rm=TRUE))


# 
# print("Successful sims")
# print(success_df %>% group_by(condition) %>% count())
# 
# print("Bracing outcomes")
# print(bracing %>% group_by(condition) %>% count())
# 
# print("Unilateral bracing outcomes")
# print(uni_bracing %>% group_by(condition) %>% count())
# 
# print("Mean bracing activations")
# print(mean_activations)
# 
# print("Mean unilateral bracing activations")
# print(mean_uni_activations)
# 
# print("Mean contacts")
# print(mean_contacts)
# 
# print("Mean unilateral contacts")
# print(mean_uni_contacts)
# 
# # Compare activation by bracer/non-bracer category
# cat_data <- success_df %>%
#   select(L_AG, L_ANT, SL_ACT, IL_L, IL_R, GGM_L, GGM_R, GGP_L, GGP_R, VERT_L, VERT_R,
#          HG_L, HG_R, STY_L, STY_R, MH_L, MH_R, SL, TRANS_L, TRANS_R,
#          GGA_L, GGA_R)
# 
# cat_pivot <- cat_data %>%
#   pivot_longer(c(-L_AG, -L_ANT, -SL_ACT), names_to="muscle", values_to="activation") %>%
#   mutate(type=ifelse(muscle %in% c("GGP_L", "GGP_R", "GGM_L", "GGM_R", "MH_L",
#                                    "MH_R", "VERT_L", "VERT_R", "SL"), 'agonist', 'antagonist'))
# 
# print("Mean activation across conditions by agonist/antagonist")
# cat_pivot %>% group_by(L_AG, L_ANT, SL_ACT, type) %>% summarize(mean=mean(activation))
# 
# # Stats
# activation_df_stats <- success_df %>%
#   mutate(
#     contact = front > 0 | mid > 0 | back > 0 | lat_left > 0 | lat_right > 0 | coronal > 0, 'contact', 'no contact',
#     bracing = ifelse(lat_left > 0 & lat_right > 0, 'bi_bracing', ifelse(lat_right | lat_left, 'uni_bracing', 'no bracing'))
#   ) %>%
#   select(contact, bracing, lat_left, L_AG, L_ANT, SL_ACT) 
# 
# #activation_df_stats$condition <- factor(activation_df_stats$condition, levels=c("5mm", "10mm"))
# test <- lm(lat_left ~ L_AG + L_ANT + SL_ACT, data = activation_df_stats)
# z <- summary(test)$coefficients/summary(test)$standard.errors
# p <- (1 - pnorm(abs(z), 0, 1)) * 2
# p
# summary(test)
# 
# # Visualization
# 
# # Histogram of activation level counts
# 
# activation_df <- success_df %>%
#   mutate(
#     contact = ifelse(front > 0 | mid > 0 | back > 0 | lat_left > 0 | lat_right > 0 | coronal > 0, 'contact', 'no contact'),
#     bracing = ifelse(lat_left > 0 & lat_right > 0, 'bi_bracing', ifelse(lat_right | lat_left, 'uni_bracing', 'no bracing')),
#     group = str_c(contact, bracing, sep = ' & ')
#   ) %>%
#   select(condition, contact, bracing, group, IL_L, IL_R, GGM_L, GGM_R, GGP_L, GGP_R, VERT_L, VERT_R,
#          HG_L, HG_R, STY_L, STY_R, MH_L, MH_R, SL, TRANS_L, TRANS_R,
#          GGA_L, GGA_R)
# 
# activation_df %>%
#   mutate(bracing = factor(bracing, levels = c('bi_bracing', 'no bracing', 'uni_bracing'))) %>%
#   gather(key, value, -condition, -contact, -bracing, -group) %>%
#   ggplot(aes(x=value, fill=bracing)) +
#   geom_bar(aes(y=..prop..), position=position_dodge(preserve = "single")) +
#   facet_wrap(~key, nrow=2) +
#   xlab("Activation level") +
#   ylab("Proportion of bracing outcomes")
# ggsave("images/activation_histogram.png")
# 
# # Barplot of mean levels
# 
# pivot_data %>%
#   mutate(condition = factor(condition, levels = c("5mm", "10mm"))) %>%
#   ggplot(aes(x=factor(muscle, 
#                       levels = c("HG", "TRANS", "STY", "GGA", "IL", "VERT", "GGM", "MH", "SL", "GGP")), 
#              y=mean, fill=condition)) +
#   geom_bar(position=position_dodge(preserve = "single"), stat='identity') +
#   xlab("Muscle") +
#   ylab("Mean activation level for bilateral bracing outcomes") + 
#   scale_fill_discrete(name = "Jaw Opening") +
#   facet_grid(~ type, scale='free') + 
#   theme(text = element_text(size=20))
# ggsave("images/activation_barplot.png")

