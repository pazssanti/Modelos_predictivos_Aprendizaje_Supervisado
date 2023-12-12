rm(list=ls())
setwd("C:/Users/DELL/Downloads")
data <- read.csv("C:/Users/DELL/Downloads/breast_cancer_dataset.csv")
df <- as.data.frame(data)
str(df)
summary(df)
df$X <- NULL
df$id <- NULL
df$diagnosis <- as.factor(df$diagnosis)
###################################################################################
#MATRIZ CORRELATION Y DATOS NULOS
library(DataExplorer)
plot_missing(df)
plot_correlation(df, type = 'continuous','Review.Date')
##################################################################################
library(ggplot2)
library(cowplot)
vars <- c("radius_mean", "texture_mean", "perimeter_mean", "area_mean")
hist <- function(var) {
  ggplot(df, aes_string(x = var)) +
    geom_histogram(fill = "#FFFF00", color = "#0000FF", position = "identity") +
    ggtitle(paste(var, "distribution")) +
    theme_classic()
}
plots <- lapply(vars, hist)
plot_grid(plotlist = plots)

vars <- c("smoothness_mean", "compactness_mean", "concavity_mean", "concave.points_mean")
hist <- function(var) {
  ggplot(df, aes_string(x = var)) +
    geom_histogram(fill = "#FFFF00", color = "#0000FF", position = "identity") +
    ggtitle(paste(var, "distribution")) +
    theme_classic()
}
plots <- lapply(vars, hist)
plot_grid(plotlist = plots)

vars <- c("symmetry_mean", "fractal_dimension_mean")
hist <- function(var) {
  ggplot(df, aes_string(x = var)) +
    geom_histogram(fill = "#FFFF00", color = "#0000FF", position = "identity") +
    ggtitle(paste(var, "distribution")) +
    theme_classic()
}
plots <- lapply(vars, hist)
plot_grid(plotlist = plots)

vars <- c("radius_se", "texture_se", "perimeter_se", "area_se")
hist <- function(var) {
  ggplot(df, aes_string(x = var)) +
    geom_histogram(fill = "#FFFF00", color = "#0000FF", position = "identity") +
    ggtitle(paste(var, "distribution")) +
    theme_classic()
}
plots <- lapply(vars, hist)
plot_grid(plotlist = plots)

vars <- c("smoothness_se ", "compactness_se", "concavity_se", "concave.points_se")
hist <- function(var) {
  ggplot(df, aes_string(x = var)) +
    geom_histogram(fill = "#FFFF00", color = "#0000FF", position = "identity") +
    ggtitle(paste(var, "distribution")) +
    theme_classic()
}
plots <- lapply(vars, hist)
plot_grid(plotlist = plots)

vars <- c("symmetry_se", "fractal_dimension_se")
hist <- function(var) {
  ggplot(df, aes_string(x = var)) +
    geom_histogram(fill = "#FFFF00", color = "#0000FF", position = "identity") +
    ggtitle(paste(var, "distribution")) +
    theme_classic()
}
plots <- lapply(vars, hist)
plot_grid(plotlist = plots)

vars <- c("radius_worst", "texture_worst", "perimeter_worst", "area_worst")
hist <- function(var) {
  ggplot(df, aes_string(x = var)) +
    geom_histogram(fill = "#FFFF00", color = "#0000FF", position = "identity") +
    ggtitle(paste(var, "distribution")) +
    theme_classic()
}
plots <- lapply(vars, hist)
plot_grid(plotlist = plots)

vars <- c("smoothness_worst", "compactness_worst", "concavity_worst", "concave.points_worst")
hist <- function(var) {
  ggplot(df, aes_string(x = var)) +
    geom_histogram(fill = "#FFFF00", color = "#0000FF", position = "identity") +
    ggtitle(paste(var, "distribution")) +
    theme_classic()
}
plots <- lapply(vars, hist)
plot_grid(plotlist = plots)

vars <- c("symmetry_worst", "fractal_dimension_worst")
hist <- function(var) {
  ggplot(df, aes_string(x = var)) +
    geom_histogram(fill = "#FFFF00", color = "#0000FF", position = "identity") +
    ggtitle(paste(var, "distribution")) +
    theme_classic()
}
plots <- lapply(vars, hist)
plot_grid(plotlist = plots)

#boxplot
library(ggplot2)
library(cowplot)

df_boxplot <- data.frame(
  Medida = c(df$radius_mean, df$radius_se, df$radius_worst),
  Variables = factor(rep(c('Radio Medio', 'Radio SE', 'Worst Radio'), each = nrow(df))))
ggplot(df_boxplot, aes(x = Variables, y = Medida)) +
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal() +
  ylab("IQR")


df_boxplot <- data.frame(
  Medida = c(df$texture_mean, df$texture_se, df$texture_worst),
  Variables = factor(rep(c('Texture Medio', 'Texture SE', 'Worst Texture'), each = nrow(df))))
ggplot(df_boxplot, aes(x = Variables, y = Medida)) +
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal() +
  ylab("IQR") 


df_boxplot <- data.frame(
  Medida = c(df$perimeter_mean, df$perimeter_se, df$perimeter_worst),
  Variables = factor(rep(c('Perimeter Mean', 'Perimeter SE', 'Worst Perimeter'), each = nrow(df))))
ggplot(df_boxplot, aes(x = Variables, y = Medida)) +
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal() +
  ylab("IQR")


df_boxplot <- data.frame(
  Medida = c(df$area_mean, df$area_se, df$area_worst),
  Variables = factor(rep(c('Area Mean', 'Area SE', 'Worst Area'), each = nrow(df))))
ggplot(df_boxplot, aes(x = Variables, y = Medida)) +
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal() +
  ylab("IQR")


df_boxplot <- data.frame(
  Medida = c(df$smoothness_mean, df$smoothness_se, df$smoothness_worst),
  Variables = factor(rep(c('Smoothness Mean', 'Smoothness SE', 'Worst Smoothness'), each = nrow(df))))
ggplot(df_boxplot, aes(x = Variables, y = Medida)) +
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal() +
  ylab("IQR")


df_boxplot <- data.frame(
  Medida = c(df$compactness_mean, df$compactness_se, df$compactness_worst),
  Variables = factor(rep(c('Compactness Mean', 'Compactness SE', 'Worst Compactness'), each = nrow(df))))
ggplot(df_boxplot, aes(x = Variables, y = Medida)) +
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal() +
  ylab("IQR")


df_boxplot <- data.frame(
  Medida = c(df$concavity_mean, df$concavity_se, df$concavity_worst),
  Variables = factor(rep(c('Concavity Mean', 'Concavity SE', 'Worst Concavity'), each = nrow(df))))
ggplot(df_boxplot, aes(x = Variables, y = Medida)) +
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal() +
  ylab("IQR")


df_boxplot <- data.frame(
  Medida = c(df$concave.points_mean, df$concave.points_se, df$concave.points_worst),
  Variables = factor(rep(c('Concave.points Mean', 'Concave.points SE', 'Worst Concave.points'), each = nrow(df))))
ggplot(df_boxplot, aes(x = Variables, y = Medida)) +
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal() +
  ylab("IQR")


df_boxplot <- data.frame(
  Medida = c(df$symmetry_mean, df$symmetry_se, df$symmetry_worst),
  Variables = factor(rep(c('Symmetry Mean', 'Symmetry SE', 'Worst Symmetry'), each = nrow(df))))
ggplot(df_boxplot, aes(x = Variables, y = Medida)) +
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal() +
  ylab("IQR")


df_boxplot <- data.frame(
  Medida = c(df$fractal_dimension_mean, df$fractal_dimension_se, df$fractal_dimension_worst),
  Variables = factor(rep(c('Fractal_dimension Mean', 'Fractal_dimension SE', 'Worst Fractal_dimension'), each = nrow(df))))
ggplot(df_boxplot, aes(x = Variables, y = Medida)) +
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal() +
  ylab("IQR")
########################################################################################
#ESCALAMIENTO DE LAS VARIABLES
df_esc <- as.data.frame(scale(df[,-1]))
diagnosis <- as.factor(df$diagnosis)
df <- as.data.frame(cbind(diagnosis,df_esc))
########################################################################################
#K NEAREST NEIGHBORS
library(FNN)
library(caret)
library(ROCR)

#TRAIN & TEST
set.seed(123) 
trainIndex <- sample(c(TRUE, FALSE), nrow(df), replace = TRUE, prob = c(0.8, 0.2))
train.X <- df[trainIndex, -1]
test.X <- df[!trainIndex, -1]
train.Y <- diagnosis[trainIndex]
test.Y <- diagnosis[!trainIndex]

#MAX ACCURACY
accuracy <- c()
for (i in 1:30){
  knn.pred <- knn(train.X, test.X, cl = train.Y, k = i)
  accuracy[i] <- mean(knn.pred == test.Y)
}
which(accuracy == max(accuracy))

#ERROR VS 1/K
errors <- 1 - accuracy
plot_data <- data.frame(K = 1:length(errors), Error = errors)
ggplot(plot_data, aes(x = 1/K, y = Error)) +
  geom_line(colour = "red") +
  labs(x = "1/K", y = "Error", title = "Classification Error by Number of Clusters (K)")

#MODEL
knn.pred <- knn(train.X, test.X, cl= train.Y, k = 9, prob=TRUE)
table(knn.pred, test.Y)
dat.pred <- as.factor(knn.pred)
dat.true <- as.factor(test.Y)
conf.knn <- confusionMatrix(dat.pred, dat.true, mode="everything", positive="B")
conf.knn
pred.knn <- prediction(as.numeric(knn.pred), as.numeric(test.Y))
perf.knn <- performance(pred.knn, "tpr", "fpr")

#ROC & AUC
plot(perf.knn,
     main = "Curva ROC",
     xlab="False positive rate", 
     ylab="True positive rate", col="red", lwd= 1.5)
abline(a=0,b=1,col="black",lty=2)
grid()
auc.knn <- as.numeric(performance(pred.knn,"auc")@y.values)
legend("bottomright",legend=paste(" AUC_knn =",round(auc.knn,4)))

##########################################################################
#REGRESIÓN LOGISTICA
library(pROC)
library(caret)
library(ROCR)

#MULTICOLINEALIDAD
df_ind <- df[,-1]
corr_matrix <- cor(df_ind[, sapply(df_ind, is.numeric)])
vars_corr <- findCorrelation(corr_matrix, cutoff = 0.7)
not_corr <- colnames(df_ind)[-vars_corr]
df_log <- df[, c("diagnosis", not_corr)]

#TRAIN & TEST
trainIndex <- createDataPartition(df_log$diagnosis, p = .8, list = FALSE, times = 1)
trainSet_log <- df_log[trainIndex,]
validationSet_log <- df_log[-trainIndex,]

#MODEL
train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
model.log <- train(diagnosis ~ ., data = trainSet_log, method = "glm", family = "binomial", trControl = train_control, metric = "ROC")
pred.log <- predict(model.log, newdata = validationSet_log)
conf.log <- confusionMatrix(pred.log, validationSet_log$diagnosis)
print(conf.log)
pred.log <- prediction(as.numeric(pred.log), as.numeric(validationSet_log$diagnosis))

#ROC
perf.log <- performance(pred.log, "tpr", "fpr")
plot(perf.log,
     main = "ROC curve",
     xlab="False positive rate",
     ylab="True positive rate", col="red", lwd= 1.5)
abline(a=0,b=1,col="black",lty=2)
grid()
auc.log <- as.numeric(performance(pred.log,"auc")@y.values)
legend("bottomright",legend=paste(" AUC_logistic =",round(auc.log,4)))

#var explicativas
summary(model.log$finalModel)
#ODDS
odds_ratios <- exp(coef(model.log$finalModel))
print(odds_ratios)

#VARIANZA
anova_log<- anova(model.log$finalModel, test="Chisq")
print(anova_log)

#NULL MODEL
null_deviance <- model.log$finalModel$null.deviance
residual_deviance <- model.log$finalModel$deviance
print(paste("Null Deviance: ", null_deviance))
print(paste("Residual Deviance: ", residual_deviance))
print(paste("Difference: ", null_deviance - residual_deviance))

#GRAFICO PROBABILIDAD DE OCURRENCIA POR VARIABLE QUE MEJOR EXPLICA
df_log$diagnosis_numeric <- ifelse(df_log$diagnosis == "M", 1, 0)
ggplot(df_log, aes(x = !!sym(best_var), y = diagnosis_numeric)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, colour = "red") +
  labs(x = best_var, y = "Probabilidad de ocurrencia de Y") +
  theme_minimal()

# Determine a cutoff probability?
#library(dplyr)
#probabilities <- seq(from = 0, to = 1, by = 0.01)
#best_cutoff <- 0
#best_f1 <- 0
#for(cutoff in probabilities) 
#predicted_classes <- ifelse(predictions > cutoff, 0, 1)
#predicted_classes <- factor(predicted_classes, levels = unique(validationSet$diagnosis))
#confusion <- confusionMatrix(predicted_classes, validationSet$diagnosis)
#f1 <- 2 * (confusion$table[2,2] / (2 * confusion$table[2,2] + confusion$table[2,1] + confusion$table[1,2]))
#if(f1 > best_f1) {
#best_f1 <- f1
#best_cutoff <- cutoff
#}
#}
#print(paste("The best cutoff is", best_cutoff, "with an F1 score of", best_f1))


#ÁRBOL DE DECISIÓN
library(ROCR)
library(tree)
library(caret)

#TRAIN % TEST
trainIndex <- createDataPartition(df$diagnosis, p = .8, list = FALSE, times = 1)
trainSet <- df[trainIndex,]
validationSet <- df[-trainIndex,]

#MODEL
set.seed(123)
tree_model <- tree(diagnosis~ ., data = trainSet)
par(mar = c(1,1,1,1))
plot(x = tree_model, type = "proportional")
text(x = tree_model, splits = TRUE, pretty = 0, cex = 0.8, col = "firebrick")

#CROSS VALIDATION
cv_tree <- cv.tree(tree_model, FUN = prune.misclass, K = 30)
size_optimo <- rev(cv_tree$size)[which.min(rev(cv_tree$dev))]
size_optimo
resultados_cv <- data.frame(n_nodos = cv_tree$size, clas_error = cv_tree$dev, alpha = cv_tree$k)
tree_final <- prune.misclass(tree = tree_model, best = size_optimo)
pred.tree <- predict(tree_model, newdata = validationSet, type = "class")
conf.tree <- confusionMatrix(predicciones, validationSet$diagnosis)
plot(x = tree_final, type = "proportional")
text(x = tree_final, splits = TRUE, pretty = 0, cex = 0.8, col = "firebrick")

#ROC & AUC
prob.tree <- predict(tree_model, newdata = validationSet, type = "where")
pred.tree <- prediction(prob.tree, validationSet$diagnosis)
perf.tree <- performance(pred.tree,"tpr","fpr")
plot(perf.tree, main = "ROC curve", xlab="False positive rate", ylab="True positive rate", col="red", lwd= 1.5)
abline(a=0,b=1,col="black",lty=2)
grid()
auc.tree <- performance(pred.tree, measure = "auc")
auc.tree <- auc.tree@y.values[[1]]
legend("bottomright",legend=paste("AUC_tree =",round(auc.tree,4)))


#RANDOM FOREST
library(randomForest)
library(caret)

#ELECCIÓN NODOS Y ARBOLES
best_num_trees <- NULL
best_min_node_size <- NULL
best_cv_error <- Inf

# Prueba diferentes valores de num_trees y min_node_size
for (num_trees in seq(100, 500, by = 100)) {
  for (min_node_size in seq(1, 5, by = 1)) {
    
    # Entrena el modelo de Random Forest
    model <- randomForest(diagnosis ~ ., data = trainSet, ntree = num_trees, nodesize = min_node_size)
    cv_results <- rfcv(trainx = trainSet[, -1], trainy = trainSet[, 1], cv.fold = 10)
    
    # Si el error de validación cruzada es menor que el mejor encontrado hasta ahora, actualiza los mejores parámetros
    if (mean(cv_results$error) < best_cv_error) {
      best_num_trees <- num_trees
      best_min_node_size <- min_node_size
      best_cv_error <- mean(cv_results$error)
    }
    
  }
}

# Imprime los mejores parámetros
print(paste("Best number of trees: ", best_num_trees))
print(paste("Best minimum node size: ", best_min_node_size))
print(paste("Best accuracy: ", best_accuracy))


min_node_size <- 3
best_n_tree <- 200

trainIndex <- createDataPartition(df$diagnosis, p = .8, list = FALSE, times = 1)
trainData <- df[trainIndex,]
testData <- df[-trainIndex,]
model.rf <- randomForest(diagnosis ~ ., data = trainData, ntree = best_n_tree, nodesize = min_node_size)
cv_rf <- rfcv(trainx = trainData[-ncol(trainData)], trainy = trainData$diagnosis, cv.fold = 10)
print(cv_rf)
pred.rf <- predict(model.rf, newdata = testData)
conf.rf <- confusionMatrix(pred.rf, testData$diagnosis, positive = 'B')
conf.rf
var.imp <-varImp(model.rf)
varImpPlot(model.rf)
library(ROCR)
prob.rf<- predict(model.rf, newdata=df[-trainIndex,], type="prob")[,2]
pred.rf <- prediction(prob.rf, df[-trainIndex, "diagnosis"])
perf.rf <- performance(pred.rf, "tpr", "fpr")
plot(perf.rf,
     main = "Curva ROC",
     xlab="Tasa de falsos positivos",
     ylab="Tasa de verdaderos positivos", col="red", lwd= 1.5)
abline(a=0,b=1,col="black",lty=2)
grid()

auc.rf <- performance(pred.rf, measure = "auc")
auc.rf <- auc.rf@y.values[[1]]
legend("bottomright",legend=paste("AUC_rF =",round(auc.rf,4)))


#SUPPORT VECTOR MACHINE
library(e1071)
library(ROCR)
library(caret)

#TRAIN & TEST
set.seed(123)
trainIndex <- sample(1:nrow(df), nrow(df)*0.8)
trainData <- df[trainIndex,]
testData <- df[-trainIndex,]

#MODEL AND CROSS-VALIDATION
ctrl.svm <- trainControl(method="cv", number=10)
set.seed(123)
svm.linear <- train(diagnosis~., data=trainData, method="svmLinear", trControl=ctrl.svm, preProcess = c("center", "scale"))
svm.radial <- train(diagnosis~., data=trainData, method="svmRadial", trControl=ctrl.svm, preProcess = c("center", "scale"))
svm.poly <- train(diagnosis~., data=trainData, method="svmPoly", trControl=ctrl.svm, preProcess = c("center", "scale"))

print(svm.linear)
print(svm.radial)
print(svm.poly)

pred.linear <- predict(svm.linear, newdata=testData)
pred.radial <- predict(svm.radial, newdata=testData)
pred.poly <- predict(svm.poly, newdata=testData)

#ROC & AUC
pred.linear <- prediction(as.numeric(pred.linear), testData$diagnosis)
pred.radial <- prediction(as.numeric(pred.radial), testData$diagnosis)
pred.poly <- prediction(as.numeric(pred.poly), testData$diagnosis)

perf.linear <- performance(pred.linear, "tpr", "fpr")
perf.radial <- performance(pred.radial, "tpr", "fpr")
perf.poly <- performance(pred.poly, "tpr", "fpr")

auc.linear <- performance(pred.linear, measure = "auc")@y.values[[1]]
auc.radial <- performance(pred.radial, measure = "auc")@y.values[[1]]
auc.poly <- performance(pred.poly, measure = "auc")@y.values[[1]]

plot(perf.linear, main = "Curva ROC", xlab="Tasa de falsos positivos", ylab="Tasa de verdaderos positivos", col="red", lwd= 1.5)
lines(perf.radial@x.values[[1]], perf.radial@y.values[[1]], col = "blue", lwd = 2)
lines(perf.poly@x.values[[1]], perf.poly@y.values[[1]], col = "green", lwd = 2)

# Añadir leyenda
legend("bottomright", legend = c(paste("AUC para SVM Lineal = ", round(auc.linear, 4)),
                                 paste("AUC para SVM Radial = ", round(auc.radial, 4)),
                                 paste("AUC para SVM Polinomial = ", round(auc.poly, 4))),
       col = c("red", "blue", "green"), lwd = 2)


#ROC SVM LINEAL (MEJOR AUC)
plot(perf.linear, main = "Curva ROC para SVM Lineal", xlab="Tasa de falsos positivos", ylab="Tasa de verdaderos positivos", col="red", lwd= 1.5)
abline(a=0,b=1,col="black",lty=2)
grid()

# Añadir leyenda
legend("bottomright", legend = paste("AUC para SVM Lineal = ", round(auc.linear, 4)), col = "red", lwd = 2)


#GRAFICO CURVA ROC DE TODOS LOS MODELOS
library(ROCR)

plot(perf.knn, main = "Curvas ROC", xlab ="Tasa de falsos positivos", ylab ="Tasa de verdaderos positivos", col="red", lwd= 1.5)
lines(perf.log@x.values[[1]], perf.log@y.values[[1]], col = "blue", lwd = 2)
lines(perf.tree@x.values[[1]], perf.tree@y.values[[1]], col = "green", lwd = 2)
lines(perf.rf@x.values[[1]], perf.rf@y.values[[1]], col = "purple", lwd = 2)
lines(perf.linear@x.values[[1]], perf.linear@y.values[[1]], col = "orange", lwd = 2)
abline(a=0,b=1,col="black",lty=2)
grid()

# Añadir leyenda
legend("bottomright", legend = c(paste("AUC para KNN = ", round(auc.knn, 4)),
                                 paste("AUC para Regresión Logística = ", round(auc.log, 4)),
                                 paste("AUC para Árbol de Decisión = ", round(auc.tree, 4)),
                                 paste("AUC para Bosque Aleatorio = ", round(auc.rf, 4)),
                                 paste("AUC para SVM Linear = ", round(auc.linear, 4))),
       col = c("red", "blue", "green", "purple", "orange"), lwd = 2)

title(main = "Curvas ROC", xlab = "Tasa de falsos positivos", ylab = "Tasa de verdaderos positivos")

