library(dplyr)
library(tibble)
library(ggplot2)
library(caret)
library(glmnet)
library(randomForest)

# Molecuar data and labels
molec_desc <- read.delim('molecular_descriptors.txt', stringsAsFactors = FALSE)
labels <- read.delim("training_set.txt", stringsAsFactors = FALSE)


# Remove rows with NA values
molec_desc <- na.omit(molec_desc)

# Remove columns with no variance
molec_desc <- molec_desc[ ,(apply(molec_desc,2,var)!=0)]


# Make factors
labels$VALENCE.PLEASANTNESS <- factor(labels$VALENCE.PLEASANTNESS)
levels(labels$VALENCE.PLEASANTNESS) <- list("Unpleasant" = c(0:33), "Neutral" = c(34:66), "Pleasant" = c(67:100))

labels$INTENSITY.STRENGTH <- factor(labels$INTENSITY.STRENGTH)
levels(labels$INTENSITY.STRENGTH) <- list("Low" = c(0:33), "Medium" = c(34:66), "High" = c(67:100))

# Subset
olfaction_data <- subset(labels, Intensity == "high ", select = c("Compound.Identifier", "Odor", "VALENCE.PLEASANTNESS", "INTENSITY.STRENGTH"))

# Rename Compound Identifier column
colnames(olfaction_data)[1] <- "CID"

# Merge data and labels
total_olfac <- merge(x = olfaction_data, y = molec_desc, by= "CID")

# Write to csv
write.csv(total_olfac, "Olfaction_Merged.csv", row.names = FALSE)





library(dplyr)
# library(tibble)
library(ggplot2)
library(caret)
library(glmnet)
library(randomForest)
library(pROC)

# Load in df with obs and labels
total_olfac <- read.csv("Olfaction_Merged.csv", stringsAsFactors = FALSE)

str(total_olfac)
colnames(total_olfac)[1:10]

# Create Factors for labels
total_olfac$INTENSITY.STRENGTH <- factor(total_olfac$INTENSITY.STRENGTH)
total_olfac$VALENCE.PLEASANTNESS <- factor(total_olfac$VALENCE.PLEASANTNESS)

# # Labels
# labels <- subset(total_olfac, select = c(1,2,3,4))
# 
# # Molecular descriptors only
# total_olfac <- subset(total_olfac, select = -c(1,2,3,4))

# Intensity df
olfac_intensity <- subset(total_olfac, select = -c(1,2,3,4))

# Standardize features
olfac_intesity <- data.frame(scale(olfac_intesity))

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



# To Try on my comp
train_set <- train_set[1:200,]
test_set <- test_set[1:200,]


# Random Forest
RF_classification <- randomForest(INTENSITY.STRENGTH ~ ., data = train_set, importance = TRUE, oob.times = 15, confusion = TRUE)
RF_classification

# Plot error based on number of trees
plot(RF_classification$err.rate[,1], type = "l", ylab = "Error rate", xlab = "Number of trees", main = "Random Forest: Error Rate vs Number of Trees")
dev.copy(png,'RandomForest.png')
dev.off()

# View importance
importance(RF_classification)

# Predict
RF_pred <- predict(RF_classification, newdata=test_set)

# Make confusion matrix
confusionMatrix(RF_pred, reference = test_set$INTENSITY.STRENGTH)

olfac_intensity <- olfac_intensity[1:100,]



# Train control
train_ctrl <- trainControl(method = "repeatedcv", repeats = 2, classProbs = T, savePredictions = T)
# Boosted tree
boost <- train(INTENSITY.STRENGTH ~ ., train_set, method = 'gbm', trControl = train_ctrl, tuneLength = 10, verbose = F)

# Predict
boost_pred <- predict(boost, newdata =test_set)

# Make confusion matrix
confusionMatrix(boost_pred, reference = test_set$INTENSITY.STRENGTH)



# Hierarchical Clustering
hier_dist <- dist(olfac_intensity[c(-1)], method = "euclidean")
hier_clust <- hclust(hier_dist, method = "average")
hier_clust
plot(hier_clust)
dev.copy(png,'HierClust.png')
dev.off()


# Lasso
ctrl <- trainControl(method="cv", number=5, savePredictions = TRUE, classProbs=TRUE)
lasso <- train(INTENSITY.STRENGTH ~ ., train_set, method = "glmnet", trControl = ctrl)
lasso

# Predict Lasso
lasso_pred <- predict(lasso, newdata = test_set)
confusionMatrix(lasso_pred, reference = test_set$INTENSITY.STRENGTH)

# Plot
# Kit thinks this needs to be deleted or changed
# plot(x = roc(predictor = lasso$pred$High, response = lasso$pred$obs)$specificities, y = roc(predictor = lasso$pred$High, response = lasso$pred$obs)$sensitivities, col= "blue", xlim = c(1, 0), type ="l", ylab = "Sensitivity", xlab = "Specificity")
# dev.copy(png,'Lasso.png')
# dev.off()

#SVMRAD
svm_rad <- train(INTENSITY.STRENGTH ~ ., train_set, method = "svmRadial", tuneLength = 10, trControl = ctrl)
svm_rad

# Predict SVM Radial Kernel
svm_rad_pred <- predict(svm_rad, newdata = test_set)
confusionMatrix(svm_pred, reference = test_set$INTENSITY.STRENGTH)

# plot(x = roc(predictor = svm_rad$pred$High, response = svm_rad$pred$obs)$specificities, y = roc(predictor = svm_rad$pred$High, response = svm_rad$pred$obs)$sensitivities, col= "blue", xlim = c(1, 0), type ="l", ylab = "Sensitivity", xlab = "Specificity")
# dev.copy(png,'SVMRad.png')
# dev.off()

#SVMLIN
svm_linear <- train(INTENSITY.STRENGTH ~ ., train_set, method = "svmLinear", tuneLength = 10, trControl = ctrl)
svm_linear

# Predict SVM Linear Kernel
svm_linear_pred <- predict(svm_linear, newdata = test_set)
confusionMatrix(svm_linear_pred, reference = test_set$INTENSITY.STRENGTH)

# plot(x = roc(predictor = svm_rad$pred$High, response = svm_rad$pred$obs)$specificities, y = roc(predictor = svm_rad$pred$High, response = svm_rad$pred$obs)$sensitivities, col= "blue", xlim = c(1, 0), type ="l", ylab = "Sensitivity", xlab = "Specificity")
# dev.copy(png,'SVMLin')
# dev.off()

#SVMPOLY
svm_poly <- train(INTENSITY.STRENGTH ~ ., train_set, method = "svmPoly", tuneLength = 10, trControl = ctrl)
svm_poly

# Predict SVM Polynomial Kernal
svm_poly_pred <- predict(svm_poly, newdata = test_set)
confusionMatrix(svm_test, reference = test_set$INTENSITY.STRENGTH)

# plot(x = roc(predictor = svm_rad$pred$High, response = svm_rad$pred$obs)$specificities, y = roc(predictor = svm_rad$pred$High, response = svm_rad$pred$obs)$sensitivities, col= "blue", xlim = c(1, 0), type ="l", ylab = "Sensitivity", xlab = "Specificity")
# dev.copy(png,'SVMPoly')
# dev.off()

