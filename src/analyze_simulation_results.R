library(tidyverse)
library(lme4)

# Change this to your local directory
setwd("E:/git_repos/asymmetrical_bracing/")

# You may need to change these
data_folder <- "data/50-150_jaw_simulations"
groupings_file <- "data/asymmetrical_activations_50_150_jaw.csv"

contacts_df <- tibble()
excitation_df <- tibble()
failures_df <- tibble()

contacts_headers <- c(
  "sim", "front", "mid", "back", "lat_left", "lat_right", "coronal"
)

# Get the output of the Artisynth simulations and read contacts,
# excitations, and failures into separate tibbles
for (f in list.files(data_folder)) {
  full_path <- file.path(data_folder, f)
  data <- read_csv(full_path, col_names=FALSE,col_types = cols())
  if (str_detect(f, "contacts")) {
    contacts_df <- rbind(contacts_df, data)
  } else if (str_detect(f, "failed")) {
    failures_df <- rbind(failures_df, data)
  } else if (str_detect(f, "excitations")) {
    excitation_df <- rbind(excitation_df, data)
  }
}

colnames(contacts_df) <- contacts_headers

# Read csv that relates simulation numbers to specific levels of
# agonist/antagonist activation
groupings_df <- read_csv(groupings_file)
colnames(groupings_df)[1] <- 'sim'

# Join contact results to groupings CSV based on simulation number.
success_df <- inner_join(contacts_df, groupings_df, by = c("sim"))
success_df$condition <- paste("Agonists:", success_df$L_AG, "\nAntagonists:", success_df$L_ANT)

# Mean contacts in different conditions
mean_contacts <- success_df %>%
  select(condition, front, mid, back, lat_left, lat_right, coronal) %>%
  group_by(condition) %>%
  summarise(across(c(front, mid, back, lat_left, lat_right, coronal), mean, na.rm=TRUE))

# Mean activations for each condition
mean_activations <- success_df %>%
  select(condition, IL_L, IL_R, GGM_L, GGM_R, GGP_L, GGP_R, VERT_L, VERT_R,
         HG_L, HG_R, STY_L, STY_R, MH_L, MH_R, SL, TRANS_L, TRANS_R,
         GGA_L, GGA_R) %>%
  group_by(condition) %>%
  summarise(across(c(IL_L, IL_R, GGM_L, GGM_R, GGP_L, GGP_R, VERT_L, VERT_R,
                     HG_L, HG_R, STY_L, STY_R, MH_L, MH_R, SL, TRANS_L, TRANS_R,
                     GGA_L, GGA_R), mean, na.rm=TRUE))

print("Successful sims")
print(success_df %>% group_by(condition) %>% count())

print("Mean bracing activations")
print(mean_activations)

print("Mean contacts")
print(mean_contacts)

# Make a data frame for plotting purposes
plot_df <- success_df %>%
  mutate(left = ifelse(lat_left > 0, 1, 0),
         right = ifelse(lat_right > 0, 1, 0)) %>%
  select(left, right, condition) %>%
  pivot_longer(cols=c("left", "right"), 
               names_to='side', values_to='contacts')

# Barplot of L/R contact by condition
ggplot(plot_df) +
  geom_bar(aes(x=condition, y=contacts, fill=side), 
           stat='summary', fun='mean', position='dodge') +
  ylab("Proportion of simulations with bracing") +
  xlab("Condition") +
  scale_fill_discrete(name = "Bracing side") +
  theme_classic(base_size=22) +
  theme(legend.text=element_text(size=23),
        legend.title=element_text(size=23),
        axis.text.x = element_text(size=20),
        legend.position = c(0.2, 0.9))

# Compare activation by bracer/non-bracer category
cat_data <- success_df %>%
  select(L_AG, L_ANT, SL_ACT, IL_L, IL_R, GGM_L, GGM_R, GGP_L, GGP_R, VERT_L, VERT_R,
         HG_L, HG_R, STY_L, STY_R, MH_L, MH_R, SL, TRANS_L, TRANS_R,
         GGA_L, GGA_R)

cat_pivot <- cat_data %>%
  pivot_longer(c(-L_AG, -L_ANT, -SL_ACT), names_to="muscle", values_to="activation") %>%
  mutate(type=ifelse(muscle %in% c("GGP_L", "GGP_R", "GGM_L", "GGM_R", "MH_L",
                                   "MH_R", "VERT_L", "VERT_R", "SL"), 'agonist', 'antagonist'))

print("Mean activation across conditions by agonist/antagonist")
cat_pivot %>% group_by(L_AG, L_ANT, SL_ACT, type) %>% summarize(mean=mean(activation))

# Stats
activation_df_stats <- success_df %>%
  mutate(
    contact = ifelse(front > 0 | mid > 0 | back > 0 | lat_left > 0 | lat_right > 0 | coronal > 0, 
                     'contact', 'no contact'),
    r_bracing = ifelse(lat_right > 0, TRUE, FALSE),
    l_bracing = ifelse(lat_left > 0, TRUE, FALSE),
    L_ANT = scale(L_ANT),
    L_AG = scale(L_AG)
  ) 

# Coarse analysis. This is what's used in the paper.
# Model predicting right-side bracing
r_m_coarse <- glm(r_bracing ~ L_AG * L_ANT,
           data = activation_df_stats, family = 'binomial')
summary(r_m_coarse)

# Model predicting left-side bracing
l_m_coarse <- glm(l_bracing ~ L_AG * L_ANT,
                  data = activation_df_stats, family = 'binomial')
summary(l_m_coarse)


# Fine analysis
l_m <- glm(l_bracing ~ GGP_L + GGM_L + GGA_L + STY_L + MH_L + HG_L + TRANS_L + VERT_L + IL_L,
           data = activation_df_stats, family = 'binomial')
summary(l_m)

# Fine analysis by individual muscles. These results aren't used in the paper.
r_m <- glm(r_bracing ~ GGP_L + GGM_L + GGA_L + STY_L + MH_L + HG_L + TRANS_L + VERT_L + IL_L,
         data = activation_df_stats, family = 'binomial')
summary(r_m)

l_m <- glm(l_bracing ~ GGP_L + GGM_L + GGA_L + STY_L + MH_L + HG_L + TRANS_L + VERT_L + IL_L,
           data = activation_df_stats, family = 'binomial')
summary(l_m)
