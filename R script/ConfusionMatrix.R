rm(list = ls())

library(pbapply)
library(tidyverse)

library(caret)
library(e1071)



df <-read.csv2("75_NC_across.csv" , stringsAsFactors = FALSE)




#df1 <-  select(df, ID, Name, URL, Description, NC.1.0, Tipo)

df$CardiologyM <- (grepl("Cardiology", df$SA1, ignore.case = TRUE)|grepl("Cardiology", df$SA2, ignore.case = TRUE)|grepl("Cardiology", df$SA3, ignore.case = TRUE))
df$DentistryM <- (grepl("Dentistry", df$SA1, ignore.case = TRUE)|grepl("Dentistry", df$SA2, ignore.case = TRUE)|grepl("Dentistry", df$SA3, ignore.case = TRUE))
df$DermatologyM <- (grepl("Dermatology", df$SA1, ignore.case = TRUE)|grepl("Dermatology", df$SA2, ignore.case = TRUE)|grepl("Dermatology", df$SA3, ignore.case = TRUE))
df$DiabetesCareM <- (grepl("Diabetes", df$SA1, ignore.case = TRUE)|grepl("Diabetes", df$SA2, ignore.case = TRUE)|grepl("Diabetes", df$SA3, ignore.case = TRUE))
df$EmergencyMedicineM <- (grepl("Emergency", df$SA1, ignore.case = TRUE)|grepl("Emergency", df$SA2, ignore.case = TRUE)|grepl("Emergency", df$SA3, ignore.case = TRUE))
df$EndocrinologyM <- (grepl("Endocrinology", df$SA1, ignore.case = TRUE)|grepl("Endocrinology", df$SA2, ignore.case = TRUE)|grepl("Endocrinology", df$SA3, ignore.case = TRUE))
df$GastroenterologyM <- (grepl("Gastroenterology", df$SA1, ignore.case = TRUE)|grepl("Gastroenterology", df$SA2, ignore.case = TRUE)|grepl("Gastroenterology", df$SA3, ignore.case = TRUE))
df$GynecologyandObstetricsM <- (grepl("Ginecology|Gynecology", df$SA1, ignore.case = TRUE)|grepl("Ginecology|Gynecology", df$SA2, ignore.case = TRUE)|grepl("Ginecology|Gynecology", df$SA3, ignore.case = TRUE))
df$MentalHealthAndNeurologyM <- (grepl("Mental", df$SA1, ignore.case = TRUE)|grepl("Mental", df$SA2, ignore.case = TRUE)|grepl("Mental", df$SA3, ignore.case = TRUE))
df$NutritionM <- (grepl("Nutrition", df$SA1, ignore.case = TRUE)|grepl("Nutrition", df$SA2, ignore.case = TRUE)|grepl("Nutrition", df$SA3, ignore.case = TRUE))
df$OncologyM <- (grepl("Oncology", df$SA1, ignore.case = TRUE)|grepl("Oncology", df$SA2, ignore.case = TRUE)|grepl("Oncology", df$SA3, ignore.case = TRUE))
df$PediatricsM <- (grepl("Pediatrics", df$SA1, ignore.case = TRUE)|grepl("Pediatrics", df$SA2, ignore.case = TRUE)|grepl("Pediatrics", df$SA3, ignore.case = TRUE))
df$SensorySystemsHealthcareM <- (grepl("Sensory", df$SA1, ignore.case = TRUE)|grepl("Sensory", df$SA2, ignore.case = TRUE)|grepl("Sensory", df$SA3, ignore.case = TRUE))
df$SleepAndRespiratoryCareM <- (grepl("Sleep", df$SA1, ignore.case = TRUE)|grepl("Sleep", df$SA2, ignore.case = TRUE)|grepl("Sleep", df$SA3, ignore.case = TRUE))
df$SurgeryM <- (grepl("Surgery", df$SA1, ignore.case = TRUE)|grepl("Surgery", df$SA2, ignore.case = TRUE)|grepl("Surgery", df$SA3, ignore.case = TRUE))



df$Cardiology <- (grepl("Cardiology", df$V1, ignore.case = T)|grepl("Cardiology", df$V2, ignore.case = T)|grepl("Cardiology", df$V3, ignore.case = T))
df$Dentistry <- (grepl("Dentistry", df$V1, ignore.case = T)|grepl("Dentistry", df$V2, ignore.case = T)|grepl("Dentistry", df$V3, ignore.case = T))
df$Dermatology <- (grepl("Dermatology", df$V1, ignore.case = T)|grepl("Dermatology", df$V2, ignore.case = T)|grepl("Dermatology", df$V3, ignore.case = T))
df$DiabetesCare <- (grepl("Diabetes", df$V1, ignore.case = T)|grepl("Diabetes", df$V2, ignore.case = T)|grepl("Diabetes", df$V3, ignore.case = T))
df$EmergencyMedicine <- (grepl("EmergencyMedicine", df$V1, ignore.case = T)|grepl("EmergencyMedicine", df$V2, ignore.case = T)|grepl("EmergencyMedicine", df$V3, ignore.case = T))
df$Endocrinology <- (grepl("Endocrinology", df$V1, ignore.case = T)|grepl("Endocrinology", df$V2, ignore.case = T)|grepl("Endocrinology", df$V3, ignore.case = T))
df$Gastroenterology <- (grepl("Gastroenterology", df$V1, ignore.case = T)|grepl("Gastroenterology", df$V2, ignore.case = T)|grepl("Gastroenterology", df$V3, ignore.case = T))
df$GynecologyandObstetrics <- (grepl("Ginecology", df$V1, ignore.case = T)|grepl("Ginecology", df$V2, ignore.case = T)|grepl("Ginecology", df$V3, ignore.case = T))
df$MentalHealthAndNeurology <- (grepl("Mental", df$V1, ignore.case = T)|grepl("Mental", df$V2, ignore.case = T)|grepl("Mental", df$V3, ignore.case = T))
df$Nutrition <- (grepl("Nutrition", df$V1, ignore.case = T)|grepl("Nutrition", df$V2, ignore.case = T)|grepl("Nutrition", df$V3, ignore.case = T))
df$Oncology <- (grepl("Oncology", df$V1, ignore.case = T)|grepl("Oncology", df$V2, ignore.case = T)|grepl("Oncology", df$V3, ignore.case = T))
df$Pediatrics <- (grepl("Pediatrics", df$V1, ignore.case = T)|grepl("Pediatrics", df$V2, ignore.case = T)|grepl("Pediatrics", df$V3, ignore.case = T))
df$SensorySystemsHealthcare <- (grepl("Sensory ", df$V1, ignore.case = T)|grepl("Sensory", df$V2, ignore.case = T)|grepl("Sensory", df$V3, ignore.case = T))
df$SleepAndRespiratoryCare <- (grepl("Sleep", df$V1, ignore.case = T)|grepl("Sleep", df$V2, ignore.case = T)|grepl("Sleep", df$V3, ignore.case = T))
df$Surgery <- (grepl("Surgery", df$V1, ignore.case = T)|grepl("Surgery", df$V2, ignore.case = T)|grepl("Surgery", df$V3, ignore.case = T))



df1 <- df[21:50] 

df1 <- ifelse(df1 == TRUE, "1", "0")%>%
 as.data.frame() 



levels(df1$NutritionM) <- c(levels(df1$NutritionM), "1")
levels(df1$OncologyM) <- c(levels(df1$OncologyM), "1")
levels(df1$SleepAndRespiratoryCareM) <- c(levels(df1$SleepAndRespiratoryCareM), "1")
levels(df1$SurgeryM) <- c(levels(df1$SurgeryM), "1")


df$NC.1.0 <- as.factor(df$NC.1.0)
df$NC_predicted <- as.factor(df$NC_predicted)
df$across <- as.factor(df$across)
df$across_predicted <- as.factor(df$across_predicted)


cmNC <- confusionMatrix(df$NC_predicted, df$NC.1.0, positive = "0", mode="everything")
cmAcross <- confusionMatrix(df$across_predicted, df$across, positive = "1", mode="everything") 
cmCardiology <- confusionMatrix(df1$Cardiology, df1$CardiologyM,  positive = "1", mode="everything")
cmCardiology
#cmDentistry <-confusionMatrix(df1$Dentistry, df1$DentistryM,  positive = "0", mode="everything")
#cmDermatology <-confusionMatrix(df1$Dermatology, df1$DermatologyM,  positive = "0", mode="everything")
#cmDiabetesCare <-confusionMatrix(df1$DiabetesCare, df1$DiabetesCareM,  positive = "0", mode="everything")
cmEmergencyMedicine <-confusionMatrix(df1$EmergencyMedicine, df1$EmergencyMedicineM,  positive = "1", mode="everything")
#cmEndocrinology <-confusionMatrix(df1$Endocrinology, df1$EndocrinologyM,  positive = "0", mode="everything")
#cmGastroenterology <-confusionMatrix(df1$Gastroenterology, df1$GastroenterologyM,  positive = "0", mode="everything") #solo un livello
cmMentalHealthAndNeurology <-confusionMatrix(df1$MentalHealthAndNeurology, df1$MentalHealthAndNeurologyM,  positive = "1", mode="everything")
cmNutrition <-confusionMatrix(df1$Nutrition, df1$NutritionM,  positive = "1", mode="everything")
cmOncology <-confusionMatrix(df1$Oncology, df1$OncologyM,  positive = "1", mode="everything")
cmPediatrics <-confusionMatrix(df1$Pediatrics, df1$PediatricsM,  positive = "1", mode="everything")
cmSensorySystemsHealthcare <-confusionMatrix(df1$SensorySystemsHealthcare, df1$SensorySystemsHealthcareM,  positive = "1", mode="everything")
cmSleepAndRespiratoryCare <-confusionMatrix(df1$SleepAndRespiratoryCare, df1$SleepAndRespiratoryCareM,  positive = "1", mode="everything")
cmSurgery <-confusionMatrix(df1$Surgery, df1$SurgeryM,  positive = "1", mode="everything")
cmGynecology <-confusionMatrix(df1$GynecologyandObstetrics, df1$GynecologyandObstetricsM,  positive = "1", mode="everything")


