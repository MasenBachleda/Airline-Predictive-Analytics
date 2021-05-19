library(plyr)
library(caret)
library(rpart) 
library(party) 
library(partykit)
library(pROC)
library(psych)
library(nnet)


#Number 2
midwest.df <- read.csv("MidWest_balanced.csv")
midwest.df <- midwest.df[,-1]
train.index <- sample(c(1:dim(midwest.df)[1]), dim(midwest.df)[1]*0.7)  
train.df <- midwest.df[train.index, ]
test.df <- midwest.df[-train.index, ]
summary(midwest.df$Topflight)
summary(midwest.df$Balance)
summary(midwest.df$Bonus_miles)


#Number 3
logit.reg <- glm(Phone_sale ~ ., data = train.df, family = "binomial")
logit.reg
coef(logit.reg)
summary(logit.reg)
exp(coef(logit.reg))
exp(cbind(OR = coef(logit.reg), confint(logit.reg)))
test_prob <- predict(logit.reg, newdata = test.df, type = "response")
test_roc <- roc(test.df$Phone_sale ~ test_prob, plot = TRUE, print.auc = TRUE)
logit.reg.pred <- predict(logit.reg, test.df, type = "response")
cm=table(test.df$Phone_sale, logit.reg.pred>0.5)
accuracy <- sum(diag(cm))/sum(sum(cm))
precision <- cm[2, 2]/(cm[2, 2] + cm[2, 1])
sensitivity <- cm[2, 2]/(cm[2, 2] + cm[1, 2])
f1 <- 2*(precision*sensitivity)/(precision+sensitivity)
accuracy
precision
sensitivity
f1
auc(test_roc)


#Number 4
model.step <- step(logit.reg, direction="both")
summary(model.step)
exp(coef(model.step))
exp(cbind(OR = coef(model.step), confint(model.step)))
test_prob2 <- predict(model.step, newdata = test.df, type = "response")
test_roc2 <- roc(test.df$Phone_sale ~ test_prob2, plot = TRUE, print.auc = TRUE)
logit.reg.pred2 <- predict(model.step, test.df, type = "response")
cm2=table(test.df$Phone_sale, logit.reg.pred2>0.5)
accuracy2 <- sum(diag(cm2))/sum(sum(cm2))
precision2 <- cm2[2, 2]/(cm2[2, 2] + cm2[2, 1])
sensitivity2 <- cm2[2, 2]/(cm2[2, 2] + cm2[1, 2])
f12 <- 2*(precision2*sensitivity2)/(precision2+sensitivity2)
accuracy2
precision2
sensitivity2
f12
auc(test_roc2)


#Number 5
train2.df <- train.df
train2.df$Phone_sale <- as.factor(train2.df$Phone_sale)
nn <- nnet(Phone_sale ~ ., data=train2.df,  size=20, maxit=100, rang=0.1, decay=5e-4)
pred <- predict(nn, test.df,  type="class")
cm3 <- table(pred=pred, test.df$Phone_sale)
accuracy3 <- sum(diag(cm3))/sum(sum(cm3))
precision3 <- cm3[2, 2]/(cm3[2, 2] + cm3[2, 1])
sensitivity3 <- cm3[2, 2]/(cm3[2, 2] + cm3[1, 2])
f13 <- 2*(precision3*sensitivity3)/(precision3+sensitivity3)
accuracy3
precision3
sensitivity3
f13
auc(test_roc3)
par(pty="s")
test_prob3 = (predict(nn, test.df))
test_roc3 = roc(test.df$Phone_sale ~ test_prob3, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate", lwd=4)

nn2 <- nnet(Phone_sale ~ ., data=train2.df,  size=100, maxit=100, rang=0.1, decay=5e-4, MaxNWts= 10000)
pred <- predict(nn2, test.df,  type="class")
cm4 <- table(pred=pred, test.df$Phone_sale)
accuracy4 <- sum(diag(cm4))/sum(sum(cm4))
precision4 <- cm4[2, 2]/(cm4[2, 2] + cm4[2, 1])
sensitivity4 <- cm4[2, 2]/(cm4[2, 2] + cm4[1, 2])
f14 <- 2*(precision4*sensitivity4)/(precision4+sensitivity4)
accuracy4
precision4
sensitivity4
f14
auc(test_roc4)
par(pty="s")
test_prob4 = (predict(nn2, test.df))
test_roc4 = roc(test.df$Phone_sale ~ test_prob4, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate", lwd=4)


#Number 8, pick one of logit.reg, model.step, nn, or nn2 for the first argument of predict
#If one of the nn, change type to class, if regression, change type to response
newdata.df <- read.csv("Prediction_Scoring.csv")
prediction <- predict(nn2, newdata.df, decision.values=TRUE, type="class") 
prediction









