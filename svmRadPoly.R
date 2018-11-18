library(dplyr)
library(tibble)
library(ggplot2)
library(caret)
library(glmnet)
library(randomForest)

total_olfac <- read.csv("Olfaction_Merged.csv", stringsAsFactors = FALSE)

# Create Factors for labels
total_olfac$INTENSITY.STRENGTH <- factor(total_olfac$INTENSITY.STRENGTH)
total_olfac$VALENCE.PLEASANTNESS <- factor(total_olfac$VALENCE.PLEASANTNESS)
dim(total_olfac)
#Take out nzvs
nzv <- nearZeroVar(total_olfac, saveMetrics= TRUE)
nzv <- nzv[nzv$nzv,]

exclude_cols <- names(total_olfac) %in% rownames(nzv)
to2 <- total_olfac[!exclude_cols]
total_olfac <- to2
dim(total_olfac)

# Add intensity labels and move to first column
olfac_intensity$INTENSITY.STRENGTH <- total_olfac$INTENSITY.STRENGTH
olfac_intensity <- olfac_intensity %>% select(INTENSITY.STRENGTH, everything())

# Set seed
set.seed(1000009)

# Create train and test set
train_size <- floor(0.75 * nrow(olfac_intensity))
train_pos <- sample(seq_len(nrow(olfac_intensity)), size = train_size)

train_set <- olfac_intensity[train_pos,]
test_set <- olfac_intensity[-train_pos,]

# Train control
ctrl <- trainControl(method = "repeatedcv", repeats = 15,classProbs = T, savePredictions = T)

#SVMRAD
svm_rad <- train(INTENSITY.STRENGTH ~ ., train_set, method = "svmRadial", tuneLength = 10, trControl = ctrl)
svm_rad

# Predict SVM Radial Kernel
svm_rad_pred <- predict(svm_rad, newdata = test_set)
confusionMatrix(svm_rad_pred, reference = test_set$INTENSITY.STRENGTH)
multiclass.roc(predictor = svm_rad$pred$Low, response = svm_rad$pred$obs)
multiclass.roc(predictor = svm_rad$pred$Medium, response = svm_rad$pred$obs)
multiclass.roc(predictor = svm_rad$pred$High, response = svm_rad$pred$obs)

svm_poly <- train(INTENSITY.STRENGTH ~ ., train_set, method = "svmPoly", tuneLength = 10, trControl = ctrl)
svm_poly

# Predict SVM Polynomial Kernal
svm_poly_pred <- predict(svm_poly, newdata = test_set)
confusionMatrix(svm_test, reference = test_set$INTENSITY.STRENGTH)
multiclass.roc(predictor = svm_poly$pred$Low, response = svm_poly$pred$obs)
multiclass.roc(predictor = svm_poly$pred$Medium, response = svm_poly$pred$obs)
multiclass.roc(predictor = svm_poly$pred$High, response = svm_poly$pred$obs)
