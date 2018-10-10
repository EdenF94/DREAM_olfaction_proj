library(dplyr)
library(tibble)
library(ggplot2)
library(ggfortify)
library(OpenImageR)
library(fastICA)
library(NMF)
olfaction_train_data <- read.csv('training_set.txt', header = T, sep = '\t')
olfaction_train_data

molec_disc <- read.csv('molecular_descriptors.txt', header=T, sep='\t')
molec_disc <- na.omit(molec_disc)


#molec_disc_pca <- data.frame(molec_disc[,2:4870])
molec_disc_pca <- data.frame(molec_disc)


#which(apply(molec_disc_pca, 2, var)==0) ## lookin for columns with a variance of 0

molec_disc_pca <- molec_disc_pca[ , apply(molec_disc_pca,2,var)!=0] # using only columns with variance > 0
dim(molec_desc_pca)
molec_disc_pca <- molec_disc_pca[ , apply(molec_disc_pca,2,var)!=0]
dim(molec_desc_pca)
identifiers <- molec_disc_pca[ , 1] # setting the CID column separately

molec_disc_pca <- molec_disc_pca[ , 2:ncol(molec_disc_pca) ] # setting all other columns
dim(molec_disc_pca)

as.tibble(molec_disc_pca)
pca <- prcomp(molec_disc_pca, scale. = T) # scaling pca
autoplot(pca)

screeplot(pca, 100, main='Screeplot of Olfaction data', type='l')


## cumulative scree plot
std_dev <- pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
sum(prop_varex[1:50])
plot(cumsum(prop_varex), type='b', main="Cumulative Sum Screeplot", xlab='Principle Component', ylab='Total Variance Proportion')
abline(h=0.897,col='red',v=50)
as.tibble(pca$x[,1:50])
initial_model <- pca$rotation[, 1:50] # grabbing the first 50 PC rotations (scores)
dim(initial_model)

initial_model_scores <- as.matrix(molec_disc_pca) %*% initial_model ### Data after PCA is done...matrix multiplication
as.tibble(initial_model_scores)
molec_data_dim_red <- data.frame(cbind(identifiers, initial_model_scores)) # combining the CID column w/ rest

colnames(molec_data_dim_red)[1] <- "CID" # renaming column


library(factoextra)
fviz_eig(pca)
fviz_pca_var(pca, col.var = 'contrib',  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel=TRUE)
fviz_pca_biplot(pca, repel = TRUE,col.var = "#2E9FDF",col.ind = "#696969")
