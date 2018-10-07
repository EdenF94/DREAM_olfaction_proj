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


molec_disc_pca <- data.frame(molec_disc[,2:4870])
autoplot(prcomp(molec_disc_pca))



which(apply(molec_disc_pca, 2, var)==0) ## lookin for columns with a variance of 0
molec_disc_pca <- molec_disc_pca[ , apply(molec_disc_pca,2,var)!=0] # using only columns with variance > 0
as.tibble(molec_disc_pca)
pca <- prcomp(molec_disc_pca, scale. = T) # scaling pca
autoplot(pca)
pca$sdev
screeplot(pca, 100, main='Screeplot of Olfaction data', type='l')


## cumulative scree plot
std_dev <- pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(cumsum(prop_varex), type='b', main="Cumulative Sum Screeplot", xlab='Principle Component', ylab='Total Variance Proportion')



