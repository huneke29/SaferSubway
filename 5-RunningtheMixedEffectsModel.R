#Loading Libraries
library(lme4)
library(dplyr)
library(caret)

### Training and Test
TrainingNYC<-read.csv("C:/Users/PC/Desktop/Insight/MTASafety/Training6_20.csv", header = TRUE)
TestingNYC<-read.csv("C:/Users/PC/Desktop/Insight/MTASafety/Testing6_20.csv", header = TRUE)
View(TrainingNYC)
View(TestingNYC)

#Linear Mixed Effects Model
m1.lme5 = lmer(RateofCrime ~ timegroupstr_x + weekday_x + timegroupstr_x*weekday_x + (1|Station2),
               data = TrainingNYC)
TestingPredLME<-predict(m1.lme5, TestingNYC, allow.new.levels = TRUE)
TrainingPredLME<-predict(m1.lme5, TrainingNYC, allow.new.levels = TRUE)
TrainingNYC$Predictions<-TrainingPredLME
mean((TestingNYC$RateofCrime - TestingPredLME)^2)
mean((TrainingNYC$RateofCrime - TrainingPredLME)^2)

#Setting a 5% Cutoff
CutoffValue5<-quantile(TestingPredLME, c(.95))
Top5PredLME<-ifelse(TestingPredLME>CutoffValue5, 1, 0)
TestingNYC$Top5PredLME<-Top5PredLME
TestingNYC$Top5PredLME<-as.factor(TestingNYC$Top5PredLME)
CrimePercents =TestingNYC$RateofCrime 
quantile(CrimePercents, c(.90, .95, .99)) 
CutoffValue<-quantile(CrimePercents, c(.95))
Top5Perc<-ifelse(TestingNYC$RateofCrime>CutoffValue, 1, 0)
Top5Perc<-as.factor(Top5Perc)
TestingNYC$Top5Perc<-Top5Perc

#Setting a 1% Cutoff
Cutoff1<-quantile(CrimePercents, c(.99))
TestingNYC$Top1Perc<-ifelse(TestingNYC$RateofCrime>Cutoff1, 1, 0)
TestingNYC$Top1Perc<-as.factor(TestingNYC$Top1Perc)
mean(TestingNYC$Top5Perc==TestingNYC$Top5PredLME)

confusionMatrix(TestingNYC$Top5Perc, TestingNYC$Top5PredLME, positive = "1")


#Running a Cumulative Model
m1.lmecum = lmer( Complaint_x_x ~ timegroupstr_x + weekday_x + weekday_x*timegroupstr_x+(1|Station2),
                  data = TrainingNYC)

TestingPredLMECum<-predict(m1.lmecum, TestingNYC, allow.new.levels = TRUE)
TrainingPredLME<-predict(m1.lme4, TrainingNYC7, allow.new.levels = TRUE)


#Getting a Cumulative Top 5 Percent
Top5PredLMECum<-ifelse(TestingPredLMECum>CutoffValue6, 1, 0)
TestingNYC$Top5PredLMECum<-Top5PredLMECum
TestingNYC$Top5PredLMECum<-as.factor(TestingNYC$Top5PredLMECum)
mean(TestingNYC$Top5PredLME==TestingNYC$Top5Perc)

confusionMatrix(TestingNYC$Top5PredLMECum, TestingNYC$Top5Perc, positive = "1")


#Getting a Cumulative Top 1 Percent
CutoffValue6<-quantile(TestingPredLMECum, c(.99))
Top1PredLMECum<-ifelse(TestingPredLMECum>CutoffValue6, 1, 0)
TestingNYC$Top1PredLMECum<-Top1PredLMECum
TestingNYC$Top1PredLMECum<-as.factor(TestingNYC$Top1PredLMECum)
mean(TestingNYC$Top1PredLME==TestingNYC$Top1Perc)

confusionMatrix(TestingNYC$Top1PredLMECum, TestingNYC$Top1Perc, positive = "1")

#Saving the Dataset
write.csv(TrainingNYCNoLots, file = "C:/Users/PC/Desktop/Insight/MTASafety/Predictions.csv")