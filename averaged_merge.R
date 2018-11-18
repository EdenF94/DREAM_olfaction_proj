library(dplyr)
library(tibble)
library(ggplot2)
library(ggfortify)
library(OpenImageR)

# Molecuar data and labels
molec_desc <- read.delim('molecular_descriptors.txt', stringsAsFactors = FALSE)
labels <- read.delim("training_set.txt", stringsAsFactors = FALSE)
head(as.tibble(molec_desc))
head(as.tibble(labels))

# Remove rows with NA values
molec_desc <- na.omit(molec_desc)

# Remove columns with no variance
molec_desc <- molec_desc[ ,(apply(molec_desc,2,var)!=0)]

# Subset
olfaction_data <- subset(labels, Intensity == "high ", select = c("Compound.Identifier", "Odor", "VALENCE.PLEASANTNESS", "INTENSITY.STRENGTH"))
head(as.tibble(olfaction_data))

# Averaging the Intensity and Valence values
average_olf <- data.frame(aggregate(cbind(VALENCE.PLEASANTNESS, INTENSITY.STRENGTH) ~ Compound.Identifier, data=olfaction_data, FUN = "mean"))
average_olf$VALENCE.PLEASANTNESS <- as.integer(average_olf$VALENCE.PLEASANTNESS)
average_olf$INTENSITY.STRENGTH <- as.integer(average_olf$INTENSITY.STRENGTH)

# Get odors col w/o the duplicates
unique_odor <- unique(olfaction_data[, 1:2])
str(unique_odor)



# Make factors
average_olf$VALENCE.PLEASANTNESS <- factor(average_olf$VALENCE.PLEASANTNESS)
levels(average_olf$VALENCE.PLEASANTNESS) <- list("Unpleasant" = c(0:33), "Neutral" = c(34:66), "Pleasant" = c(67:100))

average_olf$INTENSITY.STRENGTH <- factor(average_olf$INTENSITY.STRENGTH)
levels(average_olf$INTENSITY.STRENGTH) <- list("Low" = c(0:33), "Medium" = c(34:66), "High" = c(67:100))

# Merge
average_olf <- merge(x= average_olf, y=unique_odor, by = 'Compound.Identifier', all.x=TRUE)
str(average_olf)
# Rename Compound Identifier column
colnames(average_olf)[1] <- "CID"

# Merge data and labels
total_olfac <- merge(x = average_olf, y = molec_desc, by= "CID", all.x = TRUE)

# Examine data
head(as.tibble(total_olfac))
dim(total_olfac)

# Write out the file
write.csv(total_olfac, file="Average_Merged.csv")
