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
TrainingNYC$Predictions<-TrainingPredLME #Setting Predictions
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

confusionMatrix(TestingNYC$Top5Perc, TestingNYC$Top5PredLME, positive = "1")

#Setting a 1% Cutoff
Cutoff1<-quantile(CrimePercents, c(.99))
TestingNYC$Top1Perc<-ifelse(TestingNYC$RateofCrime>Cutoff1, 1, 0)
TestingNYC$Top1Perc<-as.factor(TestingNYC$Top1Perc)

#Getting a Predicted Top 1 Percent
CutoffValue2<-quantile(TestingPredLME, c(.99))
Top1PredLME1<-ifelse(TestingPredLME>CutoffValue2, 1, 0)
TestingNYC$Top1PredLME1<-Top1PredLME1
TestingNYC$Top1PredLME1<-as.factor(TestingNYC$Top1PredLME1)
mean(TestingNYC$Top1PredLME1==TestingNYC$Top1Perc)

confusionMatrix(TestingNYC$Top1PredLME1, TestingNYC$Top1Perc, positive = "1")


#Running a Cumulative Model
m1.lmecum = lmer(Complaint_x_x ~ timegroupstr_x + weekday_x + weekday_x*timegroupstr_x+(1|Station2),
                  data = TrainingNYC)

TestingPredLMECum<-predict(m1.lmecum, TestingNYC, allow.new.levels = TRUE)
TrainingPredLMECum<-predict(m1.lmecum, TrainingNYC, allow.new.levels = TRUE)

#Getting a Cumulative Top 5 Percent
CutoffValue7<-quantile(TestingPredLMECum, c(.95))
Top5PredLMECum<-ifelse(TestingPredLMECum>CutoffValue7, 1, 0)
TestingNYC$Top5PredLMECum<-Top5PredLMECum
TestingNYC$Top5PredLMECum<-as.factor(TestingNYC$Top5PredLMECum)
mean(TestingNYC$Top5PredLMECum==TestingNYC$Top5Perc)

confusionMatrix(TestingNYC$Top5PredLMECum, TestingNYC$Top5Perc, positive = "1")


#Getting a Cumulative Top 1 Percent
CutoffValue6<-quantile(TestingPredLMECum, c(.99))
Top1PredLMECum<-ifelse(TestingPredLMECum>CutoffValue6, 1, 0)
TestingNYC$Top1PredLMECum<-Top1PredLMECum
TestingNYC$Top1PredLMECum<-as.factor(TestingNYC$Top1PredLMECum)
mean(TestingNYC$Top1PredLMECum==TestingNYC$Top1Perc)

confusionMatrix(TestingNYC$Top1PredLMECum, TestingNYC$Top1Perc, positive = "1")

#Checking the Additional Victims Potentially Protected
TestingNYC$NowMannedStationsTop5<-ifelse(TestingNYC$Top5PredLME==1 & TestingNYC$Top5PredLMECum==0, 1, 0)
TestingNowMannedTop5<-subset(TestingNYC, TestingNYC$NowMannedStationsTop5==1)
sum(TestingNowMannedTop5$Complaint_x_x)

TestingNYC$NowMannedStations<-ifelse(TestingNYC$Top1PredLME1==1 & TestingNYC$Top1PredLMECum==0, 1, 0)
TestingNowManned<-subset(TestingNYC, TestingNYC$NowMannedStations==1)
sum(TestingNowManned$Complaint_x_x)


#Saving the Dataset
write.csv(TrainingNYC, file = "C:/Users/PC/Desktop/Insight/MTASafety/Predictions2.csv")
View(TestingNYC)
