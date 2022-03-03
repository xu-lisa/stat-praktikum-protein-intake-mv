#************************ data_description*************************************#
#*
#* This Script performs some exploratory data analysis for futher questions
#*
#*
#*#***************************** Parameters *************************************#
# load in the neccesary packages 
source("1_packages.R")
#******************************************************************************#
#*
#**************************** loading in data **********************************#
patient <- readRDS("data/patient.Rds")
daily <- readRDS("data/daily.Rds")
mergedData <- readRDS("data/mergedAndCleanedData.Rds")

#************************* Confounder description******************************#
# Description of patient-related covariates

summary(patient$Age) # Age
summary(patient$BMI) # BMI
table(patient$Gender) # Gender
table(patient$DiagID2) # Diagnosis at admission
table(patient$AdmCatID) # Reason for admission
summary(patient$ApacheIIScore) # Apache II Score

# Description of patient-independent covariates

table(patient$Year) # Year of admission

#************************* Protein description ********************************#
# Total Protein intake
mergedData %>% 
  summarise(TotalProteinCat2 = sum(proteinCat2), 
            TotalProteinCat3 = sum(proteinCat3), 
            TotalProteinCat1 = sum(proteinCat2 == 0 & proteinCat3 == 0))

# Protein intake: Comparison of people who survived/died
ProtCat <- as.data.frame(cbind(proteinCat2 = mergedData$proteinCat2, proteinCat3 = mergedData$proteinCat3))
ProtCat$proteinCat2[ProtCat$proteinCat2 == 1] <- "proteinCat2"
ProtCat$proteinCat3[ProtCat$proteinCat3 == 1] <- "proteinCat3"
ProtCat <- ProtCat %>% add_column(proteinCat = NA)
ProtCat[ProtCat == 0] <- NA

for (i in seq(nrow(ProtCat))) {
  if (is.character(ProtCat$proteinCat2[i]) & !is.na(ProtCat$proteinCat2[i])) {
    ProtCat$proteinCat[i] <- ProtCat$proteinCat2[i]
  } else if (is.character(ProtCat$proteinCat3[i]) & !is.na(ProtCat$proteinCat3[i])) {
    ProtCat$proteinCat[i] <- ProtCat$proteinCat3[i]
  } else {
    ProtCat$proteinCat[i] <- "proteinCat1"
  }
}

mergedData <- mergedData %>% add_column(proteinCat = ProtCat$proteinCat)

ggplot(mergedData, mapping = aes(x = PatientDied, col = proteinCat)) +
  geom_bar()

# protein intake of specific patients and patient characteristics

set.seed(124)
ID <- sample(mergedData$CombinedID, 1)

ID_data <- mergedData[mergedData$CombinedID == ID, ]
ID_personal <- ID_data %>%
  summarise(CombinedID = unique(CombinedID), 
            Gender = unique(Gender),
            Age = unique(Age), 
            Weight = unique(Weight),
            BMI = unique(BMI),
            ApacheIIScore = unique(ApacheIIScore), 
            PatientDied = unique(PatientDied), 
            PatientDischarged = unique(PatientDischarged),
            Protein = unique(Protein),
            DaysMechVent = unique(DaysMechVent),
            DiagID2 = unique(DiagID2), 
            AdmCatID = unique(AdmCatID), 
            Propofol2_4 = unique(Propofol2_4),
            inMV2_4 = unique(inMV2_4)) 


ID_daily <- ID_data %>% 
  select(Study_Day, 
         incomplete_day, 
         EN_Protein, PN_Protein, 
         proteinCat2, proteinCat3, 
         caloriesPercentage, 
         proteinAdjustedPercentage)

ID_personal
ID_daily

EN_plot <- ggplot(ID_daily, mapping = aes(x = Study_Day, y = EN_Protein)) + 
  geom_point() +
  ylim(0, 300) +
  scale_x_discrete(limits = factor(seq(11))) +
  geom_vline(xintercept = ID_daily$Study_Day[ID_daily$incomplete_day][1])

PN_plot <- ggplot(ID_daily, mapping = aes(x = Study_Day, y = PN_Protein)) + 
  geom_point() +
  ylim(0, 300) +
  scale_x_discrete(limits = factor(seq(11))) +
  geom_vline(xintercept = ID_daily$Study_Day[ID_daily$incomplete_day][1])

grid.arrange(EN_plot, PN_plot)
