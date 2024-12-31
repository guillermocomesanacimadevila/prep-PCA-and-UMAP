# ===== Install packages ===== #

install.packages("MASS")
install.packages("factoextra")
install.packages("ggplot2")
 
# ===== Load libraries ===== #

library(MASS)
library(factoextra)
library(ggplot2)

setwd("/Users/guillermocomesanacimadevila/Desktop/MACHINE LEARNING PROJECTS/prep PCA and UMAP")

# ===== Pre-processing ===== #

# Import biopsy data from "MASS" package
# Data summary
data(biopsy) 
print(dim(biopsy))

# Data structure
print(str(biopsy)) # all variables = numeric
print(summary(biopsy))

# Cleaning data (dealing with missing entries)
biopsy_missing <- na.omit(biopsy)
print(dim(biopsy) - dim(biopsy_missing)) # 16 entries = removed

# Exclude categorical data (first and last column) - only continuous
biopsy_sample <- biopsy_missing[, -c(1,11)]

# ===== Run PCA ===== #

biopsy_pca <- prcomp(biopsy_sample,
                     scale = TRUE)

# Summary of Analysis
print(summary(biopsy_pca))

# Elements of PCA object
print(names(biopsy_pca))

# SD of components
print(biopsy_pca$sdev)

# Eigenvectors
print(biopsy_pca$rotation) # loads per variable, per component

# Measures of central tendency
print(biopsy_pca$center)
print(biopsy_pca$scale)

# PCA Scores
print(biopsy_pca$x)

# ===== PCA visualisation ===== #
# Scree Plot of variance
fviz_eig(biopsy_pca,
         addlabels = TRUE,
         ylim = c(0, 70))

# Biplot with default settings
fviz_pca_biplot(biopsy_pca)

# Biplot with labeled variables
fviz_pca_biplot(biopsy_pca,
                label = "var")

# Biplot with coloured groups
fviz_pca_biplot(biopsy_pca,
                label = "var",
                habillage = biopsy_missing$class)

# Biplot with customised coloured groups and variables
fviz_pca_biplot(biopsy_pca,
                label = "var",
                habillage = biopsy_missing$class,
                col.var = "black") +
                scale_color_manual(values=c("orange", "purple"))