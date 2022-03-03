#************************ cif_hazard*******************************************#
# STEP 5
# This Script take care of data preparation for generating hazard ratio plots
# and CIF
#******************************************************************************#
source("1_packages.R")
source("2_function_helpers.R")
#********************************* Parameters *********************************# 
# set maximal follow-up time
max.follow <- 60
# for preprocessing, set extubaton time of patients with extubation time > max.follow to max.time
max.time <- 60.1
# define number of days time-dependent covariate was provided
maxdays.nutri <- 11
# set max extubation time for cif
surv_max <- 60
#******************************************************************************#
#*
#*#*************************** loading in data ********************************#

m_2_A   <- readRDS("output/models/m_2_A.Rds")
m_2_B   <- readRDS("output/models/m_2_B.Rds")
daily   <- readRDS("data/daily.Rds")
patient <- readRDS("data/patient.Rds")
ll      <- readRDS("data/ll.Rds")

#******************* Data Preperation for Hazard Ratio Plots ******************#

f6 <- make_six_frames(m_2_A, patient, daily, ll,
                      var1 = "proteinCat2", var2 = "proteinCat3", type = "1", effect = "proteinCat",
                      surv_max = max.follow)
f6_extubation <- make_six_frames(m_2_B, patient, daily, ll,
                                var1 = "proteinCat2", var2 = "proteinCat3", type = "1", effect = "proteinCat",
                                surv_max = max.follow)


protocol1 <- rep("< 0.8 g/kg", maxdays.nutri)
protocol2 <- c(rep("< 0.8 g/kg", 4), rep("0.8 - 1.2 g/kg", 7))
protocol3 <- rep("0.8 - 1.2 g/kg", maxdays.nutri)
protocol4 <- c(rep("0.8 - 1.2 g/kg", 4), rep("> 1.2 g/kg", 7))
protocol5 <- rep("> 1.2 g/kg", maxdays.nutri)


list_p_effect <- purrr::map(seq_along(f6), ~single_plot(f6[[.x]], NULL)) # list of plots Competing Risk
list_p_extubation <- purrr::map(seq_along(f6_extubation),
                                ~single_plot(f6_extubation[[.x]], NULL)) # list of plots Extubation

comparisons_df <-
  data.frame(
    comparison = LETTERS[1:6],
    p1 = c("protocol1", "protocol2", "protocol1", "protocol3", "protocol4", "protocol3"),
    p2 = c("protocol2", "protocol3", "protocol3", "protocol4", "protocol5", "protocol5"),
    p1.name = c("exclusively low", "late standard", "exclusively low", "early standard", "late high", "early standard"),
    p2.name = c("late standard", "early standard", "early standard", "late high", "early high", "early high"),
    p1.count = c(1, 2, 1, 3, 4, 3),
    p2.count = c(2, 3, 3, 4, 5, 5)
  ) %>%
  mutate(comparison_text = paste0(comparison, ": ", p1.name, " vs. ", p2.name)) # dataframe with interested comparisons

names(f6) <- names(f6_extubation) <- paste0(comparisons_df$p1.name, " vs. ", comparisons_df$p2.name)
#******************************************************************************#
#*
#************************ Data Preperation for CIF ****************************#
only_low_ped <- prepare_predict_frame(
  patient,
  daily,
  ll_fun   = ll,
  scheme   = list(C2 = 0, C3 = 0),
  surv_max = surv_max,
  type     = 1,
  var1     = "proteinCat2",
  var2     = "proteinCat3")

# data for profile: early standard
only_med_ped <- prepare_predict_frame(
  patient, daily, ll_fun=ll,
  scheme = list(C2 = 1, C3 = 0),
  surv_max = surv_max, type=1, var1="proteinCat2", var2 = "proteinCat3")

# data for profile: late standard
low_to_med_ped <- prepare_predict_frame(
  patient, daily, ll_fun=ll,
  scheme = list(C2 = c(rep(0, 4), rep(1, 7)), C3 = 0),
  surv_max = surv_max, type=1, var1="proteinCat2", var2 = "proteinCat3")

# data for profile: late high
med_to_high_ped <- prepare_predict_frame(
  patient, daily, ll_fun=ll,
  scheme = list(C2 = c(rep(1, 4), rep(0, 7)), C3 = c(rep(0, 4), rep(1, 7))),
  surv_max = surv_max, type=1, var1="proteinCat2", var2 = "proteinCat3")

# data for profile: early high
only_high_ped <- prepare_predict_frame(
  patient, daily, ll_fun=ll,
  scheme = list(C2 = 0, C3 = 1),
  surv_max = surv_max, type=1, var1="proteinCat2", var2 = "proteinCat3")


# create list of comparisons
comp1 <- list(only_low_ped, low_to_med_ped)
names(comp1) <- c("low", "low_to_med")
comp2 <- list(low_to_med_ped, only_med_ped)
names(comp2)<- c("low_to_med", "med")
comp3 <-  list(only_low_ped, only_med_ped)
names(comp3)<- c("low", "med")
comp4 <-  list(only_med_ped, med_to_high_ped)
names(comp4)<- c("med", "med_to_high")
comp5 <-  list(med_to_high_ped, only_high_ped)
names(comp5)<- c("med_to_high", "high")
comp6 <-  list(only_med_ped, only_high_ped)
names(comp6)<- c("med", "high")

comparisons_list <- list(comp1, comp2, comp3, comp4, comp5, comp6)
names(comparisons_list) <- purrr::map_chr(comparisons_list, ~paste0(names(.x), collapse = " vs. "))

res_df_cif <- purrr::imap_dfr(
  .x = comparisons_list,
  .f = ~{
    get_cif_df(.x, m_2_A, m_2_B, comparison_name = .y)
  })

res_df_cif <- res_df_cif %>%
  mutate(comparison = factor(comparison,
                             levels = c("low vs. low_to_med", "low_to_med vs. med", "low vs. med",
                                        "med vs. med_to_high", "med_to_high vs. high", "med vs. high"),
                             labels = comparisons_df$comparison_text))
res_df_cif <- res_df_cif %>%
  mutate(
    protocol = factor(
      protocol,
      levels = c("low", "low_to_med", "med", "med_to_high", "high"),
      labels = c("exclusively low", "late standard", "early standard", "late high", "early high")))


#***************************** Plot: Hazard Ratios ****************************#
# generating hazard ratios plots for comparisons A - F
# plots are used for PowerPoint-Präsentation & Bericht
#
#******************************************************************************#

# Bericht: Comparisons A - F: Extubation 
p_1 <- list_p_extubation[[1]] + labs(title = "Vergleich A", subtitle = names(f6)[1]) + themes_plot2 +
  list_p_extubation[[2]] + labs(title = "Vergleich B", subtitle = names(f6)[2]) + themes_plot2 +
  list_p_extubation[[3]] + labs(title = "Vergleich C", subtitle = names(f6)[3]) + themes_plot2 +
  list_p_extubation[[4]] + labs(title = "Vergleich D", subtitle = names(f6)[4]) + themes_plot2 +
  list_p_extubation[[5]] + labs(title = "Vergleich E", subtitle = names(f6)[5]) + themes_plot2 +
  list_p_extubation[[6]] + labs(title = "Vergleich F", subtitle = names(f6)[6]) + themes_plot2 +
  plot_layout(nrow = 2, byrow = TRUE)

ggsave(p_1, file = "output/visuals/cifs_hazard/protein_effect_extubation_bericht.png",
       height = 9, width = 14)

# Bericht: Comparisons A - F: Tod vor Extubation (single plots)
hr_competingrisk <- list()
letter <- c("A", "B", "C", "D", "E", "F")

for (i in seq_along(letter)) {
  hr_competingrisk[[i]] <- list_p_effect[[i]] + 
    labs(subtitle = names(f6)[i]) + themes_plot
  ggsave(hr_competingrisk[[i]], file = paste0("output/visuals/cifs_hazard/hazardratio_competingrisk_", i, ".png"), 
         height = 9, width = 9)
}

# PowerPoint: Comparisons A - F: Tod vor Extubation 
p_2 <- list_p_effect[[1]] + labs(title = "Vergleich A", subtitle = names(f6)[1]) + themes_plot3 +
  list_p_effect[[2]] + labs(title = "Vergleich B", subtitle = names(f6)[2]) + themes_plot3 +
  list_p_effect[[3]] + labs(title = "Vergleich C", subtitle = names(f6)[3]) + themes_plot3 +
  list_p_effect[[4]] + labs(title = "Vergleich D", subtitle = names(f6)[4]) + themes_plot3 +
  list_p_effect[[5]] + labs(title = "Vergleich E", subtitle = names(f6)[5]) + themes_plot3 +
  list_p_effect[[6]] + labs(title = "Vergleich F", subtitle = names(f6)[6]) + themes_plot3 +
  plot_layout(nrow = 2, byrow = TRUE)

ggsave(p_2, file = "output/visuals/cifs_hazard/protein_effect_competingrisk.png",
       height = 9, width = 14)

# PowerPoint: Comparisons A - F: Extubation 
p_3 <- list_p_extubation[[1]] + labs(title = "Vergleich A", subtitle = names(f6)[1]) + themes_plot3 +
  list_p_extubation[[2]] + labs(title = "Vergleich B", subtitle = names(f6)[2]) + themes_plot3 +
  list_p_extubation[[3]] + labs(title = "Vergleich C", subtitle = names(f6)[3]) + themes_plot3 +
  list_p_extubation[[4]] + labs(title = "Vergleich D", subtitle = names(f6)[4]) + themes_plot3 +
  list_p_extubation[[5]] + labs(title = "Vergleich E", subtitle = names(f6)[5]) + themes_plot3 +
  list_p_extubation[[6]] + labs(title = "Vergleich F", subtitle = names(f6)[6]) + themes_plot3 +
  plot_layout(nrow = 2, byrow = TRUE)

ggsave(p_3, file = "output/visuals/cifs_hazard/protein_effect_extubation.png",
       height = 9, width = 14)

# PowerPoint: stepwise hazard ratio: early standard vs. early high
list_plot <- list()
hazardratios_fit <- data.frame() # Hazard Ratio Werte
for (i in seq(10)) {
  df <- f6$`early standard vs. early high`[seq(from = 1, to = i), ]
  hazardratios_fit[i, 1] <- df$intmid[i]
  hazardratios_fit[i, 2] <- df$fit[i]
  list_plot[[i]] <- single_plot_powerpoint(df, NULL)
  ggsave(list_plot[[i]],
         file = paste0("output/visuals/cifs_hazard/", "stepwise_HR_C_", i, ".png"),
         height = 9, width = 9)
}

#******************** Plot: Cumulative Incidence Function *********************#
# generating cif for comparisons A - F 
# plots used for Bericht
#
#******************************************************************************#

# Bericht: Comparisons A - F: Extubation
p_cs_extubation <- ggplot(
  data = res_df_cif %>% filter(cause == "discharge"),
  aes(x = int_mid, y = cif, col = protocol)) +
  geom_line() +
  geom_ribbon(aes(ymin = cif_lower, ymax = cif_upper, fill = protocol), alpha = .3) +
  facet_wrap(~comparison) +
  ylab("CIF") + xlab("Days after ICU admission") +
  ggtitle("Extubation") +
  labs(color = "Ernährungsprofil", fill = "Ernährungsprofil") +
  ylim(c(0, 1.0)) +
  themes_plot2
ggsave(p_cs_extubation, file = "output/visuals/cifs_hazard/cif-extubation.png",
       height = 9, width = 14)

# Bericht: Comparisons A - F: Tod vor Extubation (single plots)
cif_competingrisk <- list()
comparison_name <- c("A: exclusively low vs. late standard",
                     "B: late standard vs. early standard",
                     "C: exclusively low vs. early standard",
                     "D: early standard vs. late high",
                     "E: late high vs. early high",
                     "F: early standard vs. early high")
for (i in seq_along(comparison_name)) {
  cif_competingrisk[[i]] <- ggplot(
    data = res_df_cif %>% filter(cause == "death", comparison == comparison_name[i]),
    aes(x = int_mid, y = cif, col = protocol)) +
    geom_line() +
    geom_ribbon(aes(ymin = cif_lower, ymax = cif_upper, fill = protocol), alpha = .3) +
    ylab("CIF") + xlab("Tage nach ICU Aufnahme") +
    ggtitle("Cause specific (Tod vor Extubation)") +
    labs(color = "Ernährungsprofil", fill = "Ernährungsprofil") +
    scale_y_continuous(breaks = c(0.000, 0.025, 0.050, 0.075, 0.100), limits = c(0, 0.11)) +
    themes_plot
  
  ggsave(cif_competingrisk[[i]], file = paste0("output/visuals/cifs_hazard/cif-death-extubation_", i, ".png"),
         height = 9, width = 14)
}
