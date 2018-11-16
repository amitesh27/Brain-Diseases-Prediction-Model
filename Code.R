Daily_Observations1 <- c(Daily_Observations, -11)
View(Daily_Observations1)
rm(Daily_Observations1)
rm()
Daily_Observations1 <- Daily_Observations[-grep('BIT_comment', colnames(Daily_Observations))]
demographic1 <- demographic[colMeans(is.na(demographic)) <= 0.6]
ICU_Monitoring1 <- ICU_Monitoring[colMeans(is.na(ICU_Monitoring)) <= 0.5]
rm(demographic1)
rm(ICU_Monitoring1)
Daily_Observations1 <- Daily_Observations[colMeans(is.na(Daily_Observations)) <= 0.6]
demographic1 <- demographic[colMeans(is.na(demographic)) <= 0.6]
ICU_Monitoring1 <- ICU_Monitoring[colMeans(is.na(ICU_Monitoring)) <= 0.6]
Lab_Results1 <- Lab_Results[colMeans(is.na(Lab_Results)) <= 0.6]
Neurological_Status1 <- Neurological_Status[colMeans(is.na(Neurological_Status)) <= 0.6]
Other_Clinical_Events1 <- Other_Clinical_Events[colMeans(is.na(Other_Clinical_Events)) <= 0.6]
Physiological1 <- Physiological[colMeans(is.na(Physiological)) <= 0.6]
Surgery1 <- Surgery[colMeans(is.na(Surgery)) <= 0.6]
