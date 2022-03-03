#***************************** 4_model_main *************************************#
#* STEP 4
#* Description:
#* This script generates the model used for the analysis
#* In order for this script to work the user must have the data in the "/data"
#* folder under the current working directory 
#* 
#******************************************************************************#
#*
#***************************** Parameters *************************************#
# load in the neccesary packages 
source("1_packages.R")
# set the wished output theme
theme_set(theme_bw())
# get the helper functions
source("2_function_helpers.R")
#******************************************************************************#
#*
#*************************** loading in data **********************************#

patient <- readRDS("data/patient.Rds")
daily <- readRDS("data/daily.Rds")

summary(patient)
summary(daily)
patient_old <- patient

patient <- patient %>%
  filter(DaysMechVent != 0)

ll_fun <- function(t, tz) { t >= tz + 4 & t <= tz * 3 + 10 }
ll <- get_laglead(
  0:60,
  tz = 1:11,
  ll_fun = ll_fun)
ll_dynamic <- gg_laglead(ll) + scale_x_discrete(guide = guide_axis(angle = 90))
ggsave(ll_dynamic, filename = paste0(main_subfolder,"/visuals/figures/ll.png"))
saveRDS(ll_fun, "data/ll.Rds")
saveRDS(patient,"data/patient_old.Rds")
#******************************************************************************#
#*
#**************************** Creation of new Variable ************************#
# Creation of the variables mv_status and event_time related to our study characteristic: extubation 

# mv_status: 
# 2 died before extubation
# 1 extubation
# 0 censored

patient <- patient %>% 
  mutate(mv_status = case_when(DaysMechVent >= 60 | DaysMechVent > Disc0To60 ~ 0,
                               DaysMechVent <= Disc0To60 | DaysMechVent < Surv0To60 ~ 1,
                               DaysMechVent >= Surv0To60 ~ 2))

# event_time:
patient <- patient %>%
  mutate(event_time = case_when(mv_status == 1 ~ DaysMechVent,
                                mv_status == 2 ~ Surv0To60,
                                mv_status == 0 ~ 61))


# delta_1:
# 1 Death before extubation
# 0 else

# delta_2:
# 1 Extubation
# 0 else

patient <- patient %>%
  mutate(delta_1 = 1L * (mv_status == 2),
         delta_2 = 1L * (mv_status == 1))

# save patient with newly created variables 
saveRDS(patient, file = "data/patient.Rds")
#******************************************************************************#
#*
#***************************** Main model *************************************#
# ####################### Model I ###############
formula = as.formula('ped_status ~ s(int_mid, bs = "ps") +
  Year +
  ApacheIIScore + ApacheIIScore:int_mid +
  s(Age, by = int_mid, bs = "ps") +
  s(BMI, by = int_mid, bs = "ps") + DiagID2 + AdmCatID + Gender +
  inMV2_4 + Propofol2_4 + OralIntake2_4 + PN2_4 +
  s(CombinedicuID, bs = "re", by = icuByDummy) +
 te(t, tz, by = I(LL * proteinCat3),
  bs = "ps", m = list(c(2, 1), c(2, 1)), id = "cal") +
  te(t, tz, by = I(LL * proteinCat2),
   bs = "ps", m = list(c(2, 1), c(2, 1)), id = "cal")
  ')

############## Model 2 ###############
### Cause spec hazard 60 days: Admin censoring after 60 days, Protein
### This model sets ALL survivors (more prec. dischargees) to the maximal event
### time of 60.
### Model A: Death before extubation
ped_2_A <- as_ped(
  data    = patient,
  formula = Surv(event_time, delta_1) ~ Year + DiagID2 + AdmCatID + Gender +
    ApacheIIScore + BMI + Propofol2_4 + inMV2_4 + OralIntake2_4 + PN2_4 + Age +
    CombinedicuID + icuByDummy,
  cut     = 0:60, id = "CombinedID")
ped_2_A <- ped_2_A %>%
  add_cumulative_eff_vec(daily, "proteinCat2", "proteinCat3", LL = ll)
ped_2_A$int_mid <- 0.5 * (ped_2_A$tstart + ped_2_A$tend)
ped_2_A <- ped_2_A[ped_2_A$tend >= 5, ]
saveRDS(ped_2_A, "data/ped-data-death-hosp-sub5.Rds")

# Ucan can use this code as a basis for the parallelization if needed
# parallezation for the bam function
# numCores <- detectCores()-6
# cluster_1 <- makeCluster(numCores)
# for this you also need to set the cluster argument of the BAM function to 
# cluster = cluster_1

model_2_A <- bam(
  formula = formula,
  data    = ped_2_A,
  family  = "poisson",
  offset  = offset)

# stopCluster()
saveRDS(model_2_A,  paste0(main_subfolder,"/models/m_2_A.Rds"))

### Model B: Extubation
ped_2_B <- as_ped(
  data    = patient,
  formula = Surv(event_time, delta_2) ~ Year + DiagID2 + AdmCatID +
    Gender + ApacheIIScore + BMI + Propofol2_4 + inMV2_4 + OralIntake2_4 +
    PN2_4 + Age + CombinedicuID + icuByDummy,
  cut     = 0:60, id = "CombinedID")
ped_2_B <- ped_2_B %>%
  add_cumulative_eff_vec(daily, "proteinCat2", "proteinCat3", LL = ll)
ped_2_B$int_mid <- 0.5 * (ped_2_B$tstart + ped_2_B$tend)
ped_2_B <- ped_2_B[ped_2_B$tend >= 5, ]
saveRDS(ped_2_B, "data/ped-data-discharge-hosp-sub5.Rds")

# parallelization
# numCores <- detectCores()-6
# 
# cluster_1 <- makeCluster(numCores)

model_2_B <- bam(
  formula = formula,
  data    = ped_2_B,
  family  = "poisson",
  offset  = offset)

#stopCluster()
saveRDS(model_2_B, paste0(main_subfolder,"/models/m_2_B.Rds"))
