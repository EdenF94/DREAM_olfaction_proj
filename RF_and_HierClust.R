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
olfac_intensity <- subset(total_olfac, select = -c(1,2,3,4))

# Standardize features
# olfac_intensity <- data.frame(scale(olfac_intensity))

# Add intensity labels and move to first column
olfac_intensity$INTENSITY.STRENGTH <- total_olfac$INTENSITY.STRENGTH
olfac_intensity <- olfac_intensity %>% select(INTENSITY.STRENGTH, everything())

# Clean environment
rm(total_olfac)

# Set seed
set.seed(1000009)

# Create train and test set
train_size <- floor(0.75 * nrow(olfac_intensity))
train_pos <- sample(seq_len(nrow(olfac_intensity)), size = train_size)

train_set <- olfac_intensity[train_pos,]
test_set <- olfac_intensity[-train_pos,]



# Hierarchical Clustering
hier_dist <- dist(olfac_intensity[c(-1)], method = "euclidean")
hier_clust <- hclust(hier_dist, method = "average")
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

