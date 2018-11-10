library(dplyr)
library(tibble)
library(ggplot2)
library(ggfortify)
library(OpenImageR)
library(caret)
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
levels(labels$VALENCE.PLEASANTNESS) <- list("Unpleasent" = c(0:33), "Neutral" = c(34:66), "Pleasant" = c(67:100))

labels$INTENSITY.STRENGTH <- factor(labels$INTENSITY.STRENGTH)
levels(labels$INTENSITY.STRENGTH) <- list("Low" = c(0:33), "Medium" = c(34:66), "High" = c(67:100))

# Subset
olfaction_data <- subset(labels, Intensity == "high ", select = c("Compound.Identifier", "Odor", "VALENCE.PLEASANTNESS", "INTENSITY.STRENGTH"))

# Rename Compound Identifier column
colnames(olfaction_data)[1] <- "CID"


# Merge data and labels
total_olfac <- merge(x = olfaction_data, y = molec_desc, by= "CID")

#variance selection: near zero variance
#nzv <- nearZeroVar(molec_desc_pca, saveMetrics= TRUE)

#look at each nzv descriptor and select more closely
#filtered_molec_desc <- molec_desc_pca[unlist(nzv)]

labels <- total_olfac$Odor
olfac_no_label <- subset(total_olfac, select = -c(1,2,3,4))
set.seed(1000009)
olfac_no_label <- na.omit(olfac_no_label)


rf_olfac <- subset(total_olfac, select = -c(1,3,4))
rf_olfac <- na.omit(rf_olfac)
rf_olfac$Odor <- factor(rf_olfac$Odor)
str(rf_olfac)

train_size <- floor(0.75 * nrow(rf_olfac))
set.seed(1000009)
train_pos <- sample(seq_len(nrow(rf_olfac)), size = train_size)

trainrf <- rf_olfac[train_pos, ]
valrf <- rf_olfac[-train_pos, ]

#RF
set.seed(1000009)
exclude <- c('Odor')
RF_classification <- randomForest(Odor ~ ., data = trainrf,  importance = TRUE, oob.times = 15, confusion = TRUE, na.action=na.roughfix)

RF_classification
plot(RF_classification$err.rate[,1], type = "l", ylab = "Error rate", xlab = "Number of trees")
dev.copy(png,'RandomForest.png')
dev.off()
importance(RF_classification)

hier_dist <- dist(olfac_no_label, method = "euclidean")
hier_clust <- hclust(hier_dist, method = "average")
hier <- plot(hier_clust, cex = 0.5, labels= labels, main= "Cluter Dendogram: Average")
dev.copy(png,'hiercluster.png')
dev.off()