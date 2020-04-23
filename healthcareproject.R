setwd(choose.dir() #getting folder
getwd()
hosp<-read.csv("HospitalCosts.csv") #datasets
hosp 
hist(hosp$AGE,main = "Frequency of patients",col = "#515151",xlab = "Age") #graph
attach(hosp) 
AGE<-as.factor(AGE)# as factor
summary(AGE)
aggregate(TOTCHG~AGE,FUN=sum,data = hosp) 
max(aggregate(TOTCHG~AGE,FUN=sum,data=hosp)) # max
hist(APRDRG,col = "#0000FF",main = "Frequency of Treatments",xlab = "Trea tment Categories")#graph 
APRDRG_fact<-as.factor(hosp$APRDRG) 
summary(APRDRG_fact) 
which.max(summary(APRDRG_fact)) 
dataf<-aggregate(TOTCHG~APRDRG,FUN = sum,data=hosp) 
dataf
dataf[which.max(dataf$TOTCHG),] 
hosp<-na.omit(hosp)#first we remove "NA"values 
hosp$RACE<-as.factor(hosp$RACE) 
model_aov<-aov(TOTCHG~RACE,data = hosp) 
model_aov#ANOVA RESULTS 
summary(model_aov)
summary(hosp$RACE)#getting max hospital cost per race 
hosp$FEMALE<-as.factor(hosp$FEMALE) 
model_lm4<-lm(TOTCHG~AGE+FEMALE,data = hosp)#calling Regression funtion
summary(model_lm4) 
summary(hosp$FEMALE)#comapring genders
hosp$RACE<-as.factor(hosp$RACE)
model_lm5<-lm(LOS~AGE+FEMALE+RACE,data = hosp) 
summary(model_lm5)
model_lm6<-lm(TOTCHG~AGE+FEMALE+RACE+LOS+APRDRG,data = hosp)
summary(model_lm6) 
