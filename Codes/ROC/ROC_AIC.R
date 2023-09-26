library(ROCR)
library(pROC)

#### AIC
probfull <- predict(fit.step_AIC, newdata = test, type = "response")
predfull <- prediction(probfull, test$target)
AUC_full <- performance(predfull, "auc")@y.values[[1]]
plot(performance(predfull, "tpr", "fpr"), colorize = T, lwd = 3, main = "ROC Curves of AIC")

fitsex <- glm(target ~ sex, data = train_set, family = binomial())
probsex <- predict(fitsex, newdata = test, type = "response")
predsex <- prediction(probsex, test$target)
AUC_sex <- performance(predsex, "auc")@y.values[[1]]
plot(performance(predsex, "tpr", "fpr"), colorize = T, lwd = 3, main = "ROC Curves")

fittrestbps <- glm(target ~ trestbps, data = train_set, family = binomial())
probtrestbps <- predict(fittrestbps, newdata = test, type = "response")
predtrestbps <- prediction(probtrestbps, test$target)
AUC_trestbps <- performance(predtrestbps, "auc")@y.values[[1]]
plot(performance(predtrestbps, "tpr", "fpr"), add = TRUE, colorize = T, lwd = 3, main = "ROC Curves")


fitcp3 <- glm(target ~ cp3, data = train_set, family = binomial())
probcp3 <- predict(fitcp3, newdata = test, type = "response")
predcp3 <- prediction(probcp3, test$target)
AUC_cp3 <- performance(predcp3, "auc")@y.values[[1]]
plot(performance(predcp3, "tpr", "fpr"), add = TRUE, colorize = T, lwd = 3, main = "ROC Curves")


fitslope2 <- glm(target ~ slope2, data = train_set, family = binomial())
probslope2 <- predict(fitslope2, newdata = test, type = "response")
predslope2 <- prediction(probslope2, test$target)
AUC_slope2 <- performance(predslope2, "auc")@y.values[[1]]
plot(performance(predslope2, "tpr", "fpr"), add = TRUE, colorize = T, lwd = 3, main = "ROC Curves")

fitchol <- glm(target ~ chol, data = train_set, family = binomial())
probchol <- predict(fitchol, newdata = test, type = "response")
predchol <- prediction(probchol, test$target)
AUC_chol <- performance(predchol, "auc")@y.values[[1]]
plot(performance(predchol, "tpr", "fpr"), add = TRUE, colorize = T, lwd = 3, main = "ROC Curves")

fitslope1 <- glm(target ~ slope1, data = train_set, family = binomial())
probslope1 <- predict(fitslope1, newdata = test, type = "response")
predslope1 <- prediction(probslope1, test$target)
AUC_slope1 <- performance(predslope1, "auc")@y.values[[1]]
plot(performance(predslope1, "tpr", "fpr"), add = TRUE, colorize = T, lwd = 3, main = "ROC Curves")

fitca3 <- glm(target ~ ca3, data = train_set, family = binomial())
probca3 <- predict(fitca3, newdata = test, type = "response")
predca3 <- prediction(probca3, test$target)
AUC_ca3 <- performance(predca3, "auc")@y.values[[1]]
plot(performance(predca3, "tpr", "fpr"), add = TRUE, colorize = T, lwd = 3, main = "ROC Curves")

fitthalach <- glm(target ~ thalach, data = train_set, family = binomial())
probthalach <- predict(fitthalach, newdata = test, type = "response")
predthalach <- prediction(probthalach, test$target)
AUC_thalach <- performance(predthalach, "auc")@y.values[[1]]
plot(performance(predthalach, "tpr", "fpr"), add = TRUE, colorize = T, lwd = 3, main = "ROC Curves")

fitthal2 <- glm(target ~ thal2, data = train_set, family = binomial())
probthal2 <- predict(fitthal2, newdata = test, type = "response")
predthal2 <- prediction(probthal2, test$target)
AUC_thal2 <- performance(predthal2, "auc")@y.values[[1]]
plot(performance(predthal2, "tpr", "fpr"), add = TRUE, colorize = T, lwd = 3, main = "ROC Curves")

fitca1 <- glm(target ~ ca1, data = train_set, family = binomial())
probca1 <- predict(fitca1, newdata = test, type = "response")
predca1 <- prediction(probca1, test$target)
AUC_ca1 <- performance(predca1, "auc")@y.values[[1]]
plot(performance(predca1, "tpr", "fpr"), add = TRUE, colorize = T, lwd = 3, main = "ROC Curves")

fitca2 <- glm(target ~ ca2, data = train_set, family = binomial())
probca2 <- predict(fitca2, newdata = test, type = "response")
predca2 <- prediction(probca2, test$target)
AUC_ca2 <- performance(predca2, "auc")@y.values[[1]]
plot(performance(predca2, "tpr", "fpr"), add = TRUE, colorize = T, lwd = 3, main = "ROC Curves")

AUC_full

AUC_ca1
AUC_ca2
AUC_ca3
AUC_chol
AUC_cp3
AUC_sex
AUC_slope1
AUC_slope2
AUC_thal2
AUC_thalach
AUC_trestbps


AUC_ALL <- data.frame(x1="thal2", x2=AUC_thal2)
row<- c("cp3", AUC_cp3)
AUC_ALL <- rbind(AUC_ALL, row)
row<- c("thalach", AUC_thalach)
AUC_ALL <- rbind(AUC_ALL, row)
row<- c("sex", AUC_sex)
AUC_ALL <- rbind(AUC_ALL, row)
row<- c("ca2", AUC_ca2)
AUC_ALL <- rbind(AUC_ALL, row)
row<- c("trestbps", AUC_trestbps)
AUC_ALL <- rbind(AUC_ALL, row)
row<- c("slope1", AUC_slope1)
AUC_ALL <- rbind(AUC_ALL, row)
row<- c("chol", AUC_chol)
AUC_ALL <- rbind(AUC_ALL, row)
row<- c("slope2", AUC_slope2)
AUC_ALL <- rbind(AUC_ALL, row)
row<- c("ca3", AUC_ca3)
AUC_ALL <- rbind(AUC_ALL, row)
row<- c("ca1", AUC_ca1)
AUC_ALL <- rbind(AUC_ALL, row)

AUC_ALL
