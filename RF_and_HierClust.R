library(dplyr)
library(ggplot2)
library(caret)
library(glmnet)
library(randomForest)

# Load in df with obs and labels
total_olfac <- read.csv("Olfaction_Merged.csv", stringsAsFactors = FALSE)

# Create Factors for labels
total_olfac$INTENSITY.STRENGTH <- factor(total_olfac$INTENSITY.STRENGTH)
total_olfac$VALENCE.PLEASANTNESS <- factor(total_olfac$VALENCE.PLEASANTNESS)


# Intensity df
labels_odor <- total_olfac$Odor
olfac_intensity <- subset(total_olfac, select = -c(1,2,3,4))
olfac_valence <- subset(total_olfac, select= -c(1,2,3,4))

# Standardize features
# olfac_intensity <- data.frame(scale(olfac_intensity))

# Add intensity labels and move to first column
olfac_intensity$INTENSITY.STRENGTH <- total_olfac$INTENSITY.STRENGTH
olfac_intensity <- olfac_intensity %>% select(INTENSITY.STRENGTH, everything())
olfac_valence$VALENCE.PLEASANTNESS <- total_olfac$VALENCE.PLEASANTNESS
olfac_valence <- olfac_valence %>% select(VALENCE.PLEASANTNESS, everything())

# remove NAs from Valence
olfac_valence <- na.omit(olfac_valence) ## about 15,000 observations

# Clean environment
rm(total_olfac)

# Set seed
set.seed(1000009)


### PREDICTING INTENSITY/STRENGTH ###
# Create train and test set
train_size <- floor(0.75 * nrow(olfac_intensity))
train_pos <- sample(seq_len(nrow(olfac_intensity)), size = train_size)

train_set <- olfac_intensity[train_pos,]
test_set <- olfac_intensity[-train_pos,]



# Hierarchical Clustering
hier_dist <- dist(olfac_intensity[c(-1)], method = "euclidean")
hier_clust <- hclust(hier_dist, method = "average", labels=labels_odor)
hier_clust
plot(hier_clust)
dev.copy(png,'HierClust.png')
dev.off()



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


### VALENCE/PLEASANTNESS ###
train_size <- floor(0.75 * nrow(olfac_valence))
train_pos <- sample(seq_len(nrow(olfac_valence)), size = train_size)

train_set <- olfac_valence[train_pos,]
test_set <- olfac_valence[-train_pos,]

# Setting the VALENCE.PLEASANTNESS as a factor
train_set$VALENCE.PLEASANTNESS <- as.factor(train_set$VALENCE.PLEASANTNESS)

# Hierarchical Clustering
hier_dist <- dist(olfac_valence[c(-1)], method = "euclidean")
hier_clust <- hclust(hier_dist, method = "average")
hier_clust
plot(hier_clust)
dev.copy(png,'HierClust.png')
dev.off()

# Random Forest
RF_classification <- randomForest(VALENCE.PLEASANTNESS ~ ., data = train_set[1:50,], importance = TRUE, oob.times = 15, confusion = TRUE)
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
confusionMatrix(table(RF_pred, reference = test_set$VALENCE.PLEASANTNESS))
