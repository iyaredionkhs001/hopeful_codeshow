hosp<-read.csv(file.choose(), header=T)
head(hosp)
attach(hosp)

hist(hosp$AGE)
AGE<- as.factor(AGE)

summary(AGE)
#finding the category with the highest expenditure
aggregate(TOTCHG ~ AGE, FUN = sum, data=hosp)

max(aggregate(TOTCHG ~ AGE, FUN = sum, data=hosp))

#Diagnosis with the maximum hospitalization costs and exenditure
APRDRG1<-as.factor(APRDRG)
summary(APRDRG1)
which.max(summary(APRDRG1))
df<-aggregate(TOTCHG ~ APRDRG, FUN = sum, data=hosp)

df[which.max(df$TOTCHG),]

df

#Measure the levelof biasness in terms of race
head(hosp,20)
tail(hosp,20)

hosp<-na.omit(hosp)

attach(hosp)

RACE1<-as.factor(RACE)

model<-aov(TOTCHG ~ RACE1, data = hosp)

model

summary(model)

#for proper allocation of resources, independent variables male and female is considered

FEMALE1<- as.factor(FEMALE)

head(hosp)
head(FEMALE1)
table(FEMALE1)

model1<- lm(TOTCHG ~ AGE + FEMALE1, data = hosp )

model1

summary(model1)

#Prediction of length of stay
RACE1<- as.factor(RACE)
model2<-  lm(LOS ~ AGE + FEMALE1 + RACE1, data = hosp )

model2
summary(model2)

#Complete analysis of factors affecting hospital costs


model3<- lm(TOTCHG ~ ., data=hosp)

model3

summary(model3)





















