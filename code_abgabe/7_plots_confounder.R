#************************7_Plots_Confounder**************************************#
#* Step 7: 
#* 
#* This Step visualizes the confounders "age" and the "Apache 2" score with 
#* boxplots for an easier overview
#* 
#* ****************************************************************************#
#* source packages and funtion helpers
source("1_packages.R")
source("2_function_helpers.R")
#******** Models and Data ********#
m_2_A   <- readRDS("output/models/m_2_A.Rds") 
m_2_B   <- readRDS("output/models/m_2_B.Rds")
patient <- readRDS("data/patient.Rds")


# Boxplot for the confounder age

age <- ggplot(patient, aes(x = "", y = Age)) +
  geom_boxplot() +
  labs(y = "Alter", x = "") +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120), limits = c(0, 125)) +
  theme_bw()


# Boxplot for the confounder Apache II Score

apache <- ggplot(patient, aes(x = "", y = ApacheIIScore)) +
  geom_boxplot() +
  labs(y = "ApacheII Score", x = "") +
  ylim(c(0, 80)) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 71)) +
  theme_bw()

# Merging both plots
age_apache <- plot_grid(age, apache)
ggsave("output/visuals/confounder/age_apache.png", age_apache, width = 10, height = 8)


# Fixed coefficients
p_coef_tab_2_A_B_cat <- coefPlotGAM_cat(
  models = list(m_2_A, m_2_B),
  modelnames = c("Risk of death before extubation", "Risk of extubation"))

ggsave(p_coef_tab_2_A_B_cat, file="output/visuals/confounder/fixed_coefs_main_categorial.png")


# Fixed coefficients
p_coef_tab_2_A_B_met <- coefPlotGAM_met(
  models = list(m_2_A, m_2_B),
  modelnames = c("Risk of death before extubation", "Risk of extubation"))

ggsave(p_coef_tab_2_A_B_met, file="output/visuals/confounder/fixed_coefs_main_metric.png")
