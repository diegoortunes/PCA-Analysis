
install.packages("calibrate")	
library(calibrate)	
pca_test	=	read.table("HM3_b37pca.eigenvec",	header=F);
pca_test <- pca_test[c("FID", "IID", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")]

# Fiz um extra e depois usei meu merged pra colocar os dados nele e fazer o script do enigma
mds.cluster -> mds.cluster_extra
mds.cluster_extra -> mds.cluster
# Na amostra referencia FID é a população, mmeu FID tem que ser caso e controle 
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

library(calibrate)	
library(ggplot2)

colors=rep("burlywood",length(mds.cluster$C1));
colors[which(mds.cluster$FID	==	"CEU")]	<- "lightblue";
colors[which(mds.cluster$FID	==	"CHB")]	<- "bisque4";
colors[which(mds.cluster$FID	==	"YRI")]	<- "darkslategray";
colors[which(mds.cluster$FID	==	"TSI")]	<- "green";
colors[which(mds.cluster$FID	==	"JPT")]	<- "purple";
colors[which(mds.cluster$FID	==	"CHD")]	<- "yellow";
colors[which(mds.cluster$FID	==	"MEX")]	<- "grey50";
colors[which(mds.cluster$FID	==	"GIH")]	<- "black";
colors[which(mds.cluster$FID	==	"ASW")]	<- "darkolivegreen";
colors[which(mds.cluster$FID	==	"LWK")]	<- "deepskyblue4";
colors[which(mds.cluster$FID	==	"MKK")]	<- "darkblue";
colors[which(mds.cluster$FID	==	"AN")]	<- "aquamarine";
colors[which(mds.cluster$FID	==	"BED")]	<- "orange";
colors[which(mds.cluster$FID	==	"BN")]	<- "magenta";
colors[which(mds.cluster$FID	==	"NOS")]	<- "brown";
colors[which(mds.cluster$FID	==	"CONT")]	<- "grey";

pdf(file="mdsplot.pdf",width=7,height=7)	
plot(rev(mds.cluster$PC2),	rev(mds.cluster$PC1),	col=rev(colors),	ylab="Dimension_1",	xlab="Dimension_2",pch=20)	
legend("topright",	c("AN", "BED", "BN", "NOS", "CEU",	"CHB",	"YRI",	"TSI",	"JPT",	"CHD",	"MEX",	"GIH","ASW","LWK",	"MKK", "CONT"),	fill=c("aquamarine", "orange", "magenta", "brown","lightblue",	"bisque4",	"darkslategray",	"green",	"purple",	"yellow","grey50",	"black",	"darkolivegreen",	"deepskyblue4",	"darkblue","transparent"))

FIDlabels	<- c("AN", "BED", "BN", "NOS", "CONT", "CEU",	"CHB",	"YRI",	"TSI",	"JPT",	"CHD",	"MEX",	"GIH",	"ASW","LWK",	"MKK");
textxy(mds.cluster[which(!(mds.cluster$FID	%in%	FIDlabels)), "PC2"],	
       mds.cluster[which(!(mds.cluster$FID	%in%	FIDlabels)), "PC1"], mds.cluster[which(!(mds.cluster$FID	%in% FIDlabels)), "IID"]) 
dev.off();

# Define colors for each FID
colors <- rep("burlywood", length(mds.cluster$C1))
colors[which(mds.cluster$FID == "CEU")] <- "lightblue"
colors[which(mds.cluster$FID == "CHB")] <- "bisque4"
colors[which(mds.cluster$FID == "YRI")] <- "darkslategray"
colors[which(mds.cluster$FID == "TSI")] <- "green"
colors[which(mds.cluster$FID == "JPT")] <- "purple"
colors[which(mds.cluster$FID == "CHD")] <- "yellow"
colors[which(mds.cluster$FID == "MEX")] <- "grey50"
colors[which(mds.cluster$FID == "GIH")] <- "black"
colors[which(mds.cluster$FID == "ASW")] <- "darkolivegreen"
colors[which(mds.cluster$FID == "LWK")] <- "deepskyblue4"
colors[which(mds.cluster$FID == "MKK")] <- "darkblue"
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

# Open PDF device
pdf(file = "mdsplot.pdf", width = 7, height = 7)

# Plot controls first
plot(controls$PC2, controls$PC1, col = colors[is_control], ylab = "PC 1", xlab = "PC 2", pch = 20, cex = 0.7)

# Add other groups
points(others$PC2, others$PC1, col = colors[!is_control], pch = 20)

# Add legend
title(main = "Principal Component Analysis", sub = "Ancestrality of Sample")
legend("bottomright", c("AN", "BED", "BN", "NOS", "CEU", "CHB", "YRI", "TSI", "JPT", "CHD", "MEX", "GIH", "ASW", "LWK", "MKK", "CONT"),
       fill = c("aquamarine", "orange", "magenta", "brown", "lightblue", "bisque4", "darkslategray", "green", "purple", "yellow",
                "grey50", "black", "darkolivegreen", "deepskyblue4", "darkblue", "grey"))

# Add text labels only for non-control groups
FIDlabels <- c("AN", "BED", "BN", "NOS", "CEU", "CHB", "YRI", "TSI", "JPT", "CHD", "MEX", "GIH", "ASW", "LWK", "MKK")

# Close PDF device
dev.off()
################################# TENTANDO SO TOC

pca_test_ocd	=	read.table("HM3_b37pca_ocd.eigenvec",	header=F);
colnames(pca_test_ocd) <- c("FID", "IID", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")



############### Tentando colocar OCD nesse grafico ####################
tabela_BHRC_OCD <- scores_sice_ocd[scores_sice_ocd$OCD == 1, c("IID", "OCD")]
tabela_BHRC_OCD$FID <- rep("OCD", "43")
library (dplyr)
tabela_BHRC_OCD <- merge(tabela_BHRC_OCD, BHRC_eigenvec_ocd, by = "IID")
tabela_BHRC_OCD$OCD <- NULL
mds.cluster_ed_ocd <- mds.cluster
mds.cluster_ed_ocd <- rbind(tabela_BHRC_OCD, mds.cluster)


# Define colors based on FID in mds.cluster_ed_ocd
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
title(main = "Principal Component Analysis", sub = "Ancestrality of Sample")

# Legend including specific populations and ancestral categories
legend("bottomright", 
       legend = c("AN", "BED", "BN", "NOS", "EUR", "AFR", "EAS", "ADM", "CONT"),
       fill = c("aquamarine", "orange", "magenta", "brown", "green", "darkolivegreen", "bisque4", "black", "grey"))

# Close PDF device
dev.off()


################################# TENTANDO SO TOC

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