LifeStyle <- read.csv("~/Desktop/MSBA courses/MKT591/Assignment2/lifestyle.csv", header = TRUE)
dim(LifeStyle)
head(LifeStyle)
LifeStyle$gender <- as.factor(LifeStyle$gender)

ls_1 <- lm(liberal ~ age + buyamer + LifeStyle$gender, data=LifeStyle)
summary(ls_1)
ls_2 <- lm(liberal ~ age + buyamer + LifeStyle$gender:buyamer, data=LifeStyle)
summary(ls_2)
ls_3 <- lm(liberal ~ age + buyamer, data=LifeStyle)
summary(ls_3)
BIC(ls_1); BIC(ls_3)
# ridge resression
library("MASS")
ls_4 <- lm.ridge(liberal~age+buyamer+LifeStyle$gender, data=LifeStyle, lambda = 100)

#logistic regression
TrainData <- read.csv("~/Desktop/MSBA courses/MKT591/Assignment2/bank.train.csv", header = TRUE)
TestData <- read.csv("~/Desktop/MSBA courses/MKT591/Assignment2/bank.valid.csv", header = TRUE)
n=dim(TrainData)[1]
xBeta=0.9
plogis(xBeta)
logit_1 <- glm(y~age+marital+housing+duration, data = TrainData, 
               family = binomial('logit'))
summary(logit_1)
# Predict ln(odd) and possibility
NewData_1=data.frame(40,'single', 'yes', 300)
colnames(NewData_1)=c("age", "marital", "housing", "duration")
predict(logit_1,NewData_1,type = "response")
NewData_2=data.frame(60,'single','no',400)
colnames(NewData_2)=c("age", "marital", "housing","duration")
predict(logit_1,NewData_2,type = "link")
NewData_3=data.frame(30,'married','yes',250)
colnames(NewData_3)=c("age", "marital", "housing","duration")
predict(logit_1,NewData_3,type = "link")

# validate the model by using external data
pred.valid=predict(logit_1, TestData[-5], type = 'response')
pred.valid <- ifelse(pred.valid>0.5,1,0)
ctv=table(TestData[,5], pred.valid)
diag(prop.table(ctv, 1))
sum(diag(prop.table(ctv)))

