# Script modificado de "ENIGMA	1000	Genomes	phase	3	version	5	cookbook"
install.packages("calibrate")	
library(calibrate)	

# Importar tabela com os 10 PCs da amostra referência e amostra alvo
pca_test	=	read.table("HM3_b37pca.eigenvec",	header=F);
pca_test <- pca_test[c("FID", "IID", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")]

# Fiz um extra e depois usei meu merged pra colocar os dados nele e fazer o script do enigma
mds.cluster -> mds.cluster_extra
mds.cluster_extra -> mds.cluster
# Na amostra referência a váriavel FID é a população, então devemos alterar o FID da amostra alvo para "casos" e "controles"
test -> test_ex

test_ex$FID <- ifelse(test$W0_dceat == 2 | test$W1_dceat == 2 | test$W2_dceat == 2, "AN",
                      ifelse(test$W0_dceat == 3 | test$W1_dceat == 3 | test$W2_dceat == 3, "BN",
                             ifelse(test$W0_dceat == 4 | test$W1_dceat == 4 | test$W2_dceat == 4, "NOS",
                                    ifelse(test$W0_dceat == 5 | test$W1_dceat == 5 | test$W2_dceat == 5, "BED", 0))))

test_ex$subjectid <- NULL
test_ex$W0_dceat <- NULL
test_ex$W1_dceat <- NULL
test_ex$W2_dceat <- NULL
library(plyr)
library(dplyr)
merged_data_pca <- merge(pca_test, test_ex, by = "IID", all.x = TRUE)
merged_data_pca$FID.x[!is.na(merged_data_pca$FID.y)] <- merged_data_pca$FID.y[!is.na(merged_data_pca$FID.y)]
merged_data_pca$FID.y <- NULL
names(merged_data_pca)[names(merged_data_pca) == "FID.x"] <- "FID"
merged_data_pca$FID <- ifelse(merged_data_pca$FID == 0, "CONT", merged_data_pca$FID)
merged_data_pca$Sex <- NULL

merged_data_pca -> mds.cluster

# Define colors based on FID in mds.cluster
colors <- rep("burlywood", length(mds.cluster$FID))
colors[which(mds.cluster$FID == "CEU")] <- "green"         # EUR
colors[which(mds.cluster$FID == "CHB")] <- "bisque4"      # EAS
colors[which(mds.cluster$FID == "YRI")] <- "darkolivegreen" # AFR
colors[which(mds.cluster$FID == "TSI")] <- "green"        # EUR
colors[which(mds.cluster$FID == "JPT")] <- "bisque4"      # EAS
colors[which(mds.cluster$FID == "CHD")] <- "bisque4"      # EAS
colors[which(mds.cluster$FID == "MEX")] <- "black"        # ADM
colors[which(mds.cluster$FID == "GIH")] <- "black"        # ADM
colors[which(mds.cluster$FID == "ASW")] <- "darkolivegreen" # AFR
colors[which(mds.cluster$FID == "LWK")] <- "darkolivegreen" # AFR
colors[which(mds.cluster$FID == "MKK")] <- "darkolivegreen" # AFR
colors[which(mds.cluster$FID == "AN")] <- "aquamarine"
colors[which(mds.cluster$FID == "BED")] <- "orange"
colors[which(mds.cluster$FID == "BN")] <- "magenta"
colors[which(mds.cluster$FID == "NOS")] <- "brown"
colors[which(mds.cluster$FID == "CONT")] <- "grey"

# Create a logical vector to separate controls and other groups
is_control <- mds.cluster$FID %in% c("CONT")

# Separate controls and other groups
controls <- mds.cluster[is_control, ]
others <- mds.cluster[!is_control, ]

# Create color vectors for controls and others
colors_controls <- colors[is_control]
colors_others <- colors[!is_control]

# Open PDF device
pdf(file = "mdsplot_ed.pdf", width = 7, height = 7)

# Plot controls first
plot(controls$PC2, controls$PC1, col = colors_controls, ylab = "PC 1", xlab = "PC 2", pch = 20, cex = 0.7)

# Add other groups
points(others$PC2, others$PC1, col = colors_others, pch = 20)

# Add title
title(main = "Principal Component Analysis - ED", sub = "Ancestrality of Sample")

# Legend including specific populations and ancestral categories
legend("bottomright", 
       legend = c("AN", "BED", "BN", "NOS", "EUR", "AFR", "EAS", "ADM", "CONT"),
       fill = c("aquamarine", "orange", "magenta", "brown", "green", "darkolivegreen", "bisque4", "black", "grey"))

# Close PDF device
dev.off()


library(calibrate)	
library(ggplot2)

################################# Graficos PCA para TOC

pca_test_ocd	=	read.table("HM3_b37pca_ocd.eigenvec",	header=F);
colnames(pca_test_ocd) <- c("FID", "IID", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")
library(plyr)
library(dplyr)
merged_data_pca_ocd <- merge(pca_test_ocd, tabela_BHRC_OCD, by = "IID", all.x = TRUE)
merged_data_pca_ocd$FID.x[!is.na(merged_data_pca_ocd$FID.y)] <- merged_data_pca_ocd$FID.y[!is.na(merged_data_pca_ocd$FID.y)]
merged_data_pca_ocd$FID.y <- NULL
names(merged_data_pca_ocd)[names(merged_data_pca_ocd) == "FID.x"] <- "FID"
merged_data_pca_ocd$FID <- ifelse(merged_data_pca_ocd$FID == 0, "CONT", merged_data_pca_ocd$FID)
merged_data_pca_ocd$Sex <- NULL

merged_data_pca_ocd -> mds.cluster_ocd

colors <- rep("burlywood", length(mds.cluster_ocd$FID))
colors[which(mds.cluster_ocd$FID == "CEU")] <- "green"         # EUR
colors[which(mds.cluster_ocd$FID == "CHB")] <- "bisque4"      # EAS
colors[which(mds.cluster_ocd$FID == "YRI")] <- "darkolivegreen" # AFR
colors[which(mds.cluster_ocd$FID == "TSI")] <- "green"        # EUR
colors[which(mds.cluster_ocd$FID == "JPT")] <- "bisque4"      # EAS
colors[which(mds.cluster_ocd$FID == "CHD")] <- "bisque4"      # EAS
colors[which(mds.cluster_ocd$FID == "MEX")] <- "black"        # ADM
colors[which(mds.cluster_ocd$FID == "GIH")] <- "black"        # ADM
colors[which(mds.cluster_ocd$FID == "ASW")] <- "darkolivegreen" # AFR
colors[which(mds.cluster_ocd$FID == "LWK")] <- "darkolivegreen" # AFR
colors[which(mds.cluster_ocd$FID == "MKK")] <- "darkolivegreen" # AFR
colors[which(mds.cluster_ocd$FID == "OCD")] <- "deepskyblue4"
colors[which(mds.cluster_ocd$FID == "CONT")] <- "grey"

# Create a logical vector to separate controls and other groups
is_control <- mds.cluster_ocd$FID %in% c("CONT")

# Separate controls and other groups
controls <- mds.cluster_ocd[is_control, ]
others <- mds.cluster_ocd[!is_control, ]

# Create color vectors for controls and others
colors_controls <- colors[is_control]
colors_others <- colors[!is_control]

# Open PDF device
pdf(file = "mdsplot_ocd.pdf", width = 7, height = 7)

# Plot controls first
plot(controls$PC2, controls$PC1, col = colors_controls, ylab = "PC 1", xlab = "PC 2", pch = 20, cex = 0.7)

# Add other groups
points(others$PC2, others$PC1, col = colors_others, pch = 20)

# Add title
title(main = "Principal Component Analysis", sub = "Ancestrality of Sample")

# Legend including specific populations and ancestral categories
legend("bottomright", 
       legend = c("OCD", "EUR", "AFR", "EAS", "ADM", "CONT"),
       fill = c("deepskyblue4", "green", "darkolivegreen", "bisque4", "black", "grey"))

# Close PDF device
dev.off()
