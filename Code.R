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

#Low Pass Filter
number_of_cycles = 2
max_y = 20

Fluid_input <- Daily_Observations2[,2]
a = number_of_cycles * 2*pi/length(Fluid_input)
install.packages("ggplot2")
library(ggplot2)
install.packages("lubridate")
library(lubridate)
y = max_y*sin(Fluid_input*a)
noise1 = max_y * 1/10 * sin(Fluid_input*a*10)

plot(Fluid_input, y, type="l", col="red", ylim=range(-1.5*max_y,1.5*max_y,5))
points(Fluid_input, y + noise1, col="green", pch=20)
points(Fluid_input, noise1, col="yellow", pch=20)


lowpass.spline <- smooth.spline(Fluid_input,y, spar = 0.6) ## Control spar for amount of smoothing
lowpass.loess <- loess(y ~ x, data = data.frame(x = x, y = y), span = 0.3) ## control span to define the amount of smoothing

lines(predict(lowpass.spline, x), col = "red", lwd = 2)
lines(predict(lowpass.loess, x), col = "blue", lwd = 2)
#High Pass Filter
highpass <- y - predict(lowpass.loess, Fluid_input)
lines(Fluid_input, highpass, lwd =  2)
#Normalisation
Daily_Observations2 = subset(Daily_Observations1, select = -c(Daily_Sedation,Daily_Analgesia,Daily_Paralysis,Daily_Vasopressors,Daily_Antibiotics,Daily_Nutrition,Time_Stamp) )
Daily_Observations2$Daily_Fluid_Input[is.na(Daily_Observations2$Daily_Fluid_Input)] <- mean(Daily_Observations2$Daily_Fluid_Input)     
mean(Daily_Observations2$Daily_Fluid_Input,na.rm = TRUE)
Daily_Observations2$Daily_Fluid_Input[is.na(Daily_Observations2$Daily_Fluid_Input)] <- mean(Daily_Observations2$Daily_Fluid_Input,na.rm = TRUE)     
Daily_Observations2$Daily_Fluid_Output[is.na(Daily_Observations2$Daily_Fluid_Output)] <- mean(Daily_Observations2$Daily_Fluid_Output,na.rm = TRUE)
Daily_Observations2 <- round(Daily_Observations2$Daily_Fluid_Input,Daily_Observations2$Daily_Fluid_Output)
Daily_Observations2$Daily_Fluid_Input <- round(Daily_Observations2$Daily_Fluid_Input,0)
Daily_Observations2$Daily_Fluid_Output <- round(Daily_Observations2$Daily_Fluid_Output,0)
#=====================================================================================
#==================INFORMATION GAIN==================================================
install.packages("FSelector")
library(FSelector)
install.packages("RWekajars")
library(RWekajars)
library(FSelector)
Daily_Observations2A <- information.gain(Patient_Id~., Daily_Observations2)
View(Daily_Observations2A)


str(Daily_Observations2A)
str(Daily_Observations2)

View(Physiological1)


#=====================================================================================

library(ggplot2)

 ggplot(Daily_Observations2, aes(x = Daily_Fluid_Input, y = Daily_Fluid_Output)) + geom_point(color = "red") 

View(Physiological1)
ggplot(Physiological1, aes (x = Time_Stamp, y = ICPm)) + geom_point()

View(Daily_Observations2)



  
  #=====================================================================================

library(ggplot2)

ggplot(Daily_Observations2, aes(x = Daily_Fluid_Input, y = Daily_Fluid_Output)) + geom_point(color = "red") 

View(Physiological1)
ggplot(Physiological1, aes (x = Time_Stamp, y = ICPm)) + geom_point()

View(Daily_Observations2)

#===================================================================================

Physiological2 = subset(Physiological1, select = -c(HRT,BPs,BPd,CPP,Time_Stamp) )
Physiological2$ICPm[is.na(Physiological2$ICPm)] <- mean(Physiological2$ICPm,na.rm=TRUE)
Physiological2$ICPm <- round(Physiological2$ICPm,0)
Physiological2$SaO2[is.na(Physiological2$SaO2)] <- mean(Physiological2$SaO2,na.rm=TRUE)
Physiological2$SaO2 <- round(Physiological2$SaO2,0)
Physiological3 <- subset(Physiological2[-(24445:2862767),] )
rm(Physiological3)

#====================================================================================

demographic2 = subset(demographic1, select = c(Patient_Id,Sex,BrainIT_Center,Age,PNSH_SaO2))
demographic2$PNSH_SaO2[is.na(demographic2$PNSH_SaO2)] <- mean(demographic2$PNSH_SaO2,na.rm=TRUE)
demographic2$PNSH_SaO2 <- round(demographic2$PNSH_SaO2,0)
demographic2$Age[is.na(demographic2$Age)] <- mean(demographic2$Age,na.rm=TRUE)
demographic2$Age <- round(demographic2$Age,0)
View(demographic2)

#==================================================================================

Lab_Results2 =  subset(Lab_Results1)
Lab_Results2$FiO2[is.na(Lab_Results2$FiO2)] <- mean(Lab_Results2$FiO2,na.rm=TRUE)
Lab_Results2$FiO2 <- round(Lab_Results2$FiO2,0)
View(Lab_Results2)


#==================================================================================

ICU_Monitoring2 = subset(ICU_Monitoring1, select = c(Patient_Id, Time_Stamp, ICP_Monitor_Type, CPP_Source))

ICU_Monitoring2$ICP_Monitor_Type[is.na(ICU_Monitoring2$ICP_Monitor_Type)] <- mean(ICU_Monitoring2$ICP_Monitor_Type,na.rm=TRUE)
View(ICU_Monitoring2)

#===================================================================================
Surgery2 = subset(Surgery1)
Surgery2$ICP_Placement <- as.numeric(Surgery2$ICP_Placement)
str(Surgery2)
Surgery2 <- ifelse(ICP_Placement=="Yes", 1, 0)
View(Surgery2)

Surgery3 = subset(Surgery2, select = -c(Evacuation_of_Mass_Lesion, Elevation_of_Depressed_Skull, 
                                        Removal_of_Foreign_Body_from_Skull, Anterior_Fossa_repair_for_CSF_Leak,
                                        Placement_of_Ventricular_Drain, Active_External_Decompression ))

View(Surgery3)
#=================================================================================

Neurological_Status2 = subset(Neurological_Status1, select=-c(Left_Pupil_Reaction,Left_Pupil_Size,Right_Pupil_Reaction,Right_Pupil_Size))
Neurological_Status2$GCS_Eye <- as.numeric(Neurological_Status2$GCS_Eye)
Neurological_Status2$GCS_Eye[is.na(Neurological_Status2$GCS_Eye)] <- mean(Neurological_Status2$GCS_Eye,na.rm=TRUE)
str(Neurological_Status2)
Neurological_Status2$GCS_Eye <- round(Neurological_Status2$GCS_Eye,0)

Neurological_Status2$GCS_Motor <- as.numeric(Neurological_Status2$GCS_Motor)
str(Neurological_Status2)
Neurological_Status2$GCS_Motor[is.na(Neurological_Status2$GCS_Motor)] <- mean(Neurological_Status2$GCS_Motor,na.rm=TRUE)
Neurological_Status2$GCS_Motor <- round(Neurological_Status2$GCS_Motor,0)

Neurological_Status2$GCS_Verbal <- as.numeric(Neurological_Status2$GCS_Verbal)
str(Neurological_Status2)
Neurological_Status2$GCS_Verbal[is.na(Neurological_Status2$GCS_Verbal)] <- mean(Neurological_Status2$GCS_Verbal,na.rm=TRUE)
Neurological_Status2$GCS_Verbal <- round(Neurological_Status2$GCS_Verbal)
View(Neurological_Status2)

#==================================================================================

Daily_Observations2 <- merge(Daily_Observations2,Physiological2,by=c("Patient_Id","ICPm"),all.x=TRUE,sort=FALSE)
total <- merge(Daily_Observations2,Physiological3,by="Patient_Id")
totalC <- merge(total, Neurological_Status2, by=" Patient_Id")
totalH <- merge(totalG, Lab_Results2, by = "Patient_Id")
View(Daily_Observations2)
View(Physiological2)
View(Physiological3)
View(totalH)

totalB <- subset(total, select = -c(Patient_Id))
totalF <- subset(totalE, select = -c(Patient_Id))
totalH <- subset(totalH, select = -c(Time_Stamp.x,Time_Stamp.y))
View(totalH)

#=============================== Plotting With Respect to ICPm=====================

ggplot(total, aes (x = Daily_Fluid_Input, y = ICPm)) + geom_point()
ggplot(total, aes (x = Daily_Fluid_Output, y = ICPm)) + geom_point()
ggplot(total, aes (x = Respirationrate, y = ICPm)) + geom_point()
ggplot(total, aes (x = TC, y = ICPm)) + geom_point()
ggplot(total, aes (x = SaO2, y = ICPm)) + geom_point()
ggplot(totalE, aes (x = GCS_Eye, y = ICPm)) + geom_point()
ggplot(totalE, aes (x = GCS_Motor, y = ICPm)) + geom_point()
ggplot(totalE, aes (x = GCS_Verbal, y = ICPm)) + geom_point()
ggplot(totalH, aes (x = FiO2, y = ICPm)) + geom_point()



# ========================================Info Gain================================

library(RWekajars)
library(FSelector)

totalA <- information.gain(ICPm~., total)
totalA <- information.gain(ICPm~., totalE)
totalA <-information.gain(ICPm~., totalF)
totalI <- information.gain(ICPm~., totalH)

View(totalI) 

# =================================================================================

totalD<- subset(total[-(13034:345996),] )
totalE <- merge(totalD, Neurological_Status2, by ="Patient_Id")
totalG <- subset(totalE[-(35161:52132),])

View(totalE)
#===================================================================================

View(demographic2)
View(Other_Clinical_Events1)
View(Physiological3)


#===================================================================================



library(ggplot)
lib(ggplot())
install.packages("ggplot2")
library(ggplot2)
 



View(totalI)
View(totalH)

install.packages("e1071")
library(e1071)
str(totalH)


skewness(totalH$Daily_Fluid_Input, na.rm = TRUE)
skewness(totalH$Daily_Fluid_Output, na.rm = TRUE)
skewness(totalH$BPm, na.rm = TRUE)
skewness(totalH$ICPm, na.rm = TRUE)
skewness(totalH$TC, na.rm = TRUE)
skewness(totalH$SaO2, na.rm = TRUE)
skewness(totalH$GCS_Eye, na.rm = TRUE)
skewness(totalH$GCS_Motor, na.rm = TRUE)
skewness(totalH$GCS_Verbal, na.rm = TRUE)
skewness(totalH$FiO2, na.rm = TRUE)

#=======================Plotting skewness============================

library(ggplot2)

value_S <- c(-0.7593394, -0.7593394, 2.404807, 0.8962195,-6.596874, 1.154697, 1.154697)
  
hist ( value_S )

#=======================To find the mean of all the column============



colMeans(totalH, na.rm = TRUE)
#==========================histogram option===========================
h1 <- hist(totalH$Daily_Fluid_Input)
h2<- hist(totalH$Daily_Fluid_Output)
h3<- hist(totalH$BPm)
h4 <- hist(totalH$ICPm)
h4<- hist(totalH$TC)
h5 <-hist(totalH$SaO2)
h6 <-hist(totalH$GCS_Eye)
h7 <- hist(totalH$GCS_Motor)
h8 <-hist(totalH$GCS_Verbal)
h9 <- hist(totalH$FiO2)

#=========================Normal Distribution=========================

sd(value_S)
mean(totalH$Daily_Fluid_Input)
sd(totalH$Daily_Fluid_Input)
library(ggplot2)

Daily_Fluid_Input_NormalDistribution <- data.frame(totalH$Daily_Fluid_Input)
plot <- ggplot(xvalues, aes(x = Daily_Fluid_Input_NormalDistribution))
plot + stat_function(fun = dnorm) + xlim(c(-4000,4000))


BPm_NormalDistribution <- data.frame(totalH$BPm)
plot <- ggplot(xvalues, aes(x = BPm_NormalDistribution))
plot + stat_function(fun = dnorm) + xlim(c(-50,50))

ICPm_NormalDistribution <- data.frame(totalH$ICPm)
plot <- ggplot(xvalues, aes(x = ICPm_NormalDistribution))
plot + stat_function(fun = dnorm) + xlim(c(-5,80))


TC_NormalDistribution <- data.frame(totalH$ICPm)
plot <- ggplot(xvalues, aes(x = TC_NormalDistribution))
plot + stat_function(fun = dnorm) + xlim(c(-6,10))

SaO2_NormalDistribution <- data.frame(totalH$ICPm)
plot <- ggplot(xvalues, aes(x = SaO2_NormalDistribution))
plot + stat_function(fun = dnorm) + xlim(c(-6,10))

GCS_Eye_NormalDistribution <- data.frame(totalH$ICPm)
plot <- ggplot(xvalues, aes(x = GCS_Eye_NormalDistribution))
plot + stat_function(fun = dnorm) + xlim(c(-4,10))


GCS_Motor_NormalDistribution <- data.frame(totalH$ICPm)
plot <- ggplot(xvalues, aes(x = GCS_Motor_NormalDistribution))
plot + stat_function(fun = dnorm) + xlim(c(-3,3))

GCS_Verbal_NormalDistribution <- data.frame(totalH$ICPm)
plot <- ggplot(xvalues, aes(x = GCS_Verbal_NormalDistribution))
plot + stat_function(fun = dnorm) + xlim(c(-2,10))


#================== Multiple Linear Regression============================================

fit <- lm(totalH$ICPm ~ totalH$Daily_Fluid_Input + totalH$Daily_Fluid_Output + totalH$BPm 
          + totalH$TC + totalH$SaO2 + totalH$GCS_Eye + totalH$GCS_Motor + totalH$GCS_Verbal
          + totalH$FiO2, data=totalH)
summary(fit) # show results


fit1 <- lm(totalH$ICPm ~ totalH$Daily_Fluid_Input  + totalH$BPm + totalH$TC + totalH$SaO2 
                        + totalH$GCS_Eye + totalH$GCS_Motor, data=totalH)
summary(fit1)

# make predictions
predictions <- predict(fit1, data = totalH)
View(predictions)
summary(predictions)
# summarize accuracy
mse <- mean((totalH$Daily_Fluid_Input - predictions)^2)
print(mse)



fit2 <- lm(totalH$ICPm ~  totalH$BPm + totalH$TC + totalH$SaO2 , data=totalH)
summary(fit2)

fit3 <- lm(totalH$ICPm ~   totalH$TC + totalH$SaO2 , data=totalH)
summary(fit3)

fit4 <- lm(totalH$ICPm ~ totalH$SaO2 + totalH$GCS_Eye + totalH$GCS_Motor, data=totalH)
summary(fit4)

# =========   diagnostic plots ========================

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 

plot(fit)
#=========================================================
#create_model <- function(trainData,target) {
 # set.seed(120)
 # myglm <- glm(target ~ . , data=trainData, family = "binomial")
 # return(myglm) }

create_model <- function(trainData,target) {
 
  
  myglm <- glm(target ~ . , data=trainData, family = "binomial")
 return(myglm) 
  }
View(myglm)
score <- predict(myglm, newdata = testData, type = "response")

score_train <- predict(myglm, newdata = complete, type = "response")

#===================================================================================

library(caret)
library(lattice)
library(ggplot2)

totalH1 <- totalH[colMeans(is.na(totalH)) <= 0]
totalH1 <- subset( totalH1, select = -FiO2 )
totalH2 <- subset(totalH1[-(5001:527400),] )
anyNA(totalH2)

brainicpm_df <- totalH2
str(brainicpm_df)
head(brainicpm_df)

set.seed(2747)

intrain <- createDataPartition(y = brainicpm_df$ICPm, p= 0.7, list = FALSE)
training <- brainicpm_df[intrain,]
testing <- brainicpm_df[-intrain,]

dim(training)

dim(testing)

anyNA(brainicpm_df)
View(totalH2)


summary(brainicpm_df)


training[["ICPm"]] = factor(training[["ICPm"]])

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(2748)

svm_Linear <- train(ICPm ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear
 
 test_pred <- predict(svm_Linear, newdata = testing)
 test_pred
confusionMatrix(test_pred, testing$ICPm)


#========================== KNN Algo=================================================




trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(334)
knn_fit <- train(ICPm ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit
View(totalH2)



test_pred1 <- predict(knn_fit, newdata = testing)
test_pred1





View (Daily_Observations)
View(demographic)
View(ICU_Monitoring)
View(Lab_Results)
View(Neurological_Status)
View(Other_Clinical_Events)
View(Physiological)
View(Surgery)
View (total)

# Rename a column in R
colnames(total)[colnames(total)=="Blood Pressure"] <- "Blood_Pressure"
colnames(total)[colnames(total)=="Intracranial pressure"] <- "Intracranial_pressure"

View(total)
library(ggplot2)

ggplot(total, aes (x = Blood_Pressure , y = Intracranial_pressure)) + geom_point()




