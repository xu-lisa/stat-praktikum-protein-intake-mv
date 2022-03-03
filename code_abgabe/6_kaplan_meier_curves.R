#************************Plots Kaplan-Meier-Curves*****************************#
#* STEP 6
#*
#* Plots of the respective Kaplan-Meier-Curves for diffrent admission categories
#******************************************************************************#
# Packages
# loading in the neccesary packages
source("1_packages.R")
# loading in the data 
patient <- readRDS("data/patient.Rds")

#********** Generation of the needed survfit objects based on the data ********#
# Determination of the Kaplan-Meier-Curve by Survfit (general)
Surv_event_time <- survfit(Surv(event_time, mv_status == 1) ~ 1, data = patient)

# Determination of the Kaplan-Meier-Curve by Survfit (according to diagnoses)
Surv_event_time_cat <- survfit(Surv(event_time, mv_status == 1) ~ DiagID2, data = patient)
#summary(Surv_DaysMechVent)

# Determination of the Kaplan-Meier-Curve by Survfit (Respiratory)

patient_resp <- patient %>%
  filter(DiagID2 == "Respiratory")

Surv_event_time_resp <- survfit(Surv(event_time, mv_status == 1) ~ DiagID2, data = patient_resp)

# Determination of the Kaplan-Meier-Curve by Survfit (Respiratory, Renal, Metabolic) -> CI

target= c("Respiratory", "Renal" , "Metabolic")
patient_selected <- patient %>%
  filter(DiagID2 %in% target)

Surv_event_time_CI <- survfit(Surv(event_time, mv_status == 1) ~ DiagID2, data = patient_selected)

#******************************************************************************#
#************************** Creation of the plots *****************************#
# Kaplan-Meier-Curve for Respiratory

kaplanMeierDiagResp <- ggsurvplot(
  Surv_event_time_resp,  # survfit object
  data = patient_resp,  # data used to fit survival curves
  risk.table = TRUE,  # show risk table
  pval = FALSE,  # show p-value of log-rank test --> FALSE: nothing compared
  conf.int = FALSE,  # show confidence intervals
  xlim = c(0, 61),
  legend = "right",
  legend.title = "Diagnose bei Einweisung",
  legend.labs = "Respiratorisch",
  palette = "#b2df8a",
  break.time.by = 10,
  ggtheme = theme_minimal(),  # customize plot and risk table with a theme
  risk.table.y.text.col = F,  # colour risk table text annotations
  risk.table.y.text = T,  # show bars instead of names in text annotations
  risk.table.title = "Anzahl unter Risiko",
  ylab = "Überlebenswahrscheinlichkeit",
  xlab = "Beatmungsdauer",
  title = "Zeit bis zur Entwöhnung vom Beatmungsgerät"
)

# Adding day 61 (censoring time)

kaplanMeierDiagResp$plot <- kaplanMeierDiagResp$plot +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 61))

# Kaplan-Meier-Curves for all Diagnoses at admission

kaplanMeierDiag <- ggsurvplot(
  Surv_event_time_cat,  # survfit object
  data = patient,  # data used to fit survival curves
  risk.table = TRUE,  # show risk table
  pval = FALSE,  # show p-value of log-rank test --> FALSE: nothing compared
  conf.int = FALSE,# show confidence intervals
  xlim = c(0, 61),
  legend = "right",
  legend.title = "Diagnose bei Einweisung",
  legend.labs = c("Andere","Vaskulär","Respiratorisch", "Gastrointestinal", "Neurologisch", "Sepsis","Orthopädisch/Trauma", "Metabolisch","Renal"),
  palette = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6"),
  break.time.by = 10,
  ggtheme = theme_minimal(),
  risk.table.y.text.col = F,
  risk.table.y.text = T, # show bars instead of names in text annotations
  risk.table.title = "Anzahl unter Risiko",
  ylab = "Überlebenswahrscheinlichkeit",
  xlab = "Beatmungsdauer",
  title = "Zeit bis zur Entwöhnung vom Beatmungsgerät"
)

# Adding day 61 (censoring time)

kaplanMeierDiag$plot <- kaplanMeierDiag$plot +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 61))


# Table of results of Kaplan-Meier-Curves divided into different diagnoses
pdf("output/visuals/kaplan_meier_plots/kaplanMeierDiagTable.pdf", width = 15, height = 7)
print(kaplanMeierDiag$table, newpage = FALSE)
dev.off()

grid <- plot_grid(kaplanMeierDiagResp$plot, kaplanMeierDiag$plot, ncol = 1, align = "v")

ggsave("output/visuals/kaplan_meier_plots/KaplanMeierDiagBoth.jpg", grid, width = 15, height = 20)

#  Kaplan-Meier-Curve for Respiratory, Renal, Metabolic

kaplanMeierDiagCI <- ggsurvplot(
  Surv_event_time_CI,  # survfit object
  data = patient_selected,  # data used to fit survival curves
  risk.table = TRUE,  # show risk table
  pval = FALSE,  # show p-value of log-rank test --> FALSE: nothing compared
  conf.int = TRUE,  # show confidence intervals
  surv.median.line = "hv",  # show median
  xlim = c(0, 61), # censoring time: day 61
  legend = "right",
  legend.title = "Diagnose bei Einweisung",
  legend.labs = c("Respiratorisch", "Metabolisch","Renal"),
  palette = c("#b2df8a", "#ff7f00", "#cab2d6"),
  break.time.by = 10,
  ggtheme = theme_minimal(),
  risk.table.y.text.col = F,
  risk.table.y.text = T, # show bars instead of names in text annotations
  risk.table.title = "Anzahl unter Risiko",
  ylab = "Überlebenswahrscheinlichkeit",
  xlab = "Beatmungsdauer",
  title = "Zeit bis zur Entwöhnung vom Beatmungsgerät"
)

# Adding day 61 (censoring time)

kaplanMeierDiagCI$plot <- kaplanMeierDiagCI$plot +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 61))

# Table of results of Kaplan-Meier-Curves divided into Respiratory, Renal, Metabolic
pdf("output/visuals/kaplan_meier_plots/kaplanMeierDiagCITable.pdf", width = 12, height = 2.5)
print(kaplanMeierDiagCI$table, newpage = FALSE)
dev.off()

ggsave("output/visuals/kaplan_meier_plots/KaplanMeierDiagCI.jpg", kaplanMeierDiagCI$plot, width = 12, height = 5)


