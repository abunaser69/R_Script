library("phyloseq")
library("ggplot2")
library("vegan")
library("DESeq2")
library("tidyverse")
library("microbiome")
library("dplyr")
library("plyr")
library("vegan")
library("ape")
library("plotly")

#Loadin and Cleaning data
#loading biom file
mydata <- import_biom(BIOMfilename = "otu_table.v1.biom")

#loading meta file
mapfile <- import_qiime_sample_data("mapfile.txt")

#merging taxnomic table and meta data
mydata_map <- merge_phyloseq(mydata, mapfile)

#changing name of taxinomic rank
colnames(tax_table(mydata_map)) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

#ignoring unclassified phylum
mydata_map_sub1 = subset_taxa(mydata_map, !is.na(Phylum) & !Phylum %in% c("p__", "uncharacterized", " "))
#ignoring unclassified data
mydata_map_sub2 = subset_taxa(mydata_map_sub1, !is.na(Genus) & !Genus %in% c("g__", "uncharacterized", " "))

#Selection of two groups  for paired comparison
Group <- c("0_IC1", "1_IC1")

#Which sample i want to collect
Select_0IC1_1_IC1 <- sample_data(mydata_map_sub2)[["TreatmentGroup"]] %in% Group

##Get those sample
Sample_0IC1_1_IC1 <- prune_samples(samples = Select_0IC1_1_IC1, mydata_map_sub2)

#Check the data are normally distribuated
                                    
data <- Sample_0IC1_1_IC1 %>%
  tax_glom(taxrank = "Phylum") %>%                     
  psmelt() %>%                                       
  arrange(Phylum) 

data2 <- data  %>%
        select(Phylum, TreatmentGroup, Abundance) %>%
         group_by(Phylum, TreatmentGroup)
    
p1 <- ggplot(data2, aes(x = TreatmentGroup, y = Abundance)) +
       geom_boxplot() +
       labs(title = "Absolute abundances", y = "Abundance (read count)") +
       coord_flip()
 
plot(p1)

p <- ggplot(data2, aes(fill = TreatmentGroup, x = Abundance)) +
       geom_density(alpha = 0.5)
print(p)
# Not normally distributed
# Let us add the log10(1+x) version:
data2$Log10_Abundance  <- log10(1 + data2$Abundance)
p2 <- ggplot(data2, aes(x = TreatmentGroup, y = Log10_Abundance)) +
       geom_boxplot() +
       labs(title = "Log10 abundances", y = "Abundance (log10(1+x) read count)")  +     
       coord_flip()
plot(p2)
library(patchwork)
p1 + p2

#Check the data are normally distribuated
p <- ggplot(data2, aes(fill = TreatmentGroup, x = Log10_Abundance)) +
       geom_density(alpha = 0.5)
print(p)

#It is not normally distributed. therefore, we can not use t-test
# However, I have performed ttest for demostration purpose only

#We want to see the diffreneces in core taxa in these two groups
#core taxa that are observed in more than 30% of the samples with more than 3 reads.

core.taxa.standard <- core_members( Sample_0IC1_1_IC1, detection = 3, prevalence = 30/100)
taxonomy <- as.data.frame(tax_table(Sample_0IC1_1_IC1))
core_taxa_id <- subset(taxonomy, rownames(taxonomy) %in% core.taxa.standard)
mytaxa <- core_taxa_id$Phylum
print(mytaxa)

# Calculate p-values with the two different methods for each taxonomic unit
pvalue.ttest <- c()
pvalue.wilcoxon <- c()
#droplevels(mytaxa)
for (taxa in core.taxa.standard){
    df <- data.frame(Abundance = abundances(Sample_0IC1_1_IC1)[taxa,],
                   Log10_Abundance = log10(1 + abundances(Sample_0IC1_1_IC1)[taxa,]), 
                   Group = meta(Sample_0IC1_1_IC1)$TreatmentGroup)
 
pvalue.ttest[[taxa]] <- t.test(Log10_Abundance ~ Group, data = df)$p.value
pvalue.wilcoxon[[taxa]] <- wilcox.test(Abundance ~ Group, data = df)$p.value  
}

pvalue.wilcoxon
pvalue.ttest

# Note that multiple testing occurs. We must correct the p-values.
# let us apply the standard Benjamini-Hochberg False Discovery Rate (FDR) correction
pvalue.ttest.adjusted <- p.adjust(pvalue.ttest)
pvalue.wilcoxon.adjusted <- p.adjust(pvalue.wilcoxon, method = "BH")

# Arrange the results in a data.frame
pvalues <- data.frame(taxon = core.taxa.standard,
                  pvalue.ttest = pvalue.ttest,
                  pvalue.wilcoxon = pvalue.wilcoxon,
                  pvalue.ttest.adjusted = pvalue.ttest.adjusted,
                  pvalue.wilcoxon.adjusted = pvalue.wilcoxon.adjusted)
                 
 #Compare the distribution of raw and adjusteed p-values using wilcoxon.
p1 <- ggplot(pvalues, aes(x = pvalue.wilcoxon)) +
        geom_histogram(na.rm = TRUE, binwidth = 0.001) +
        labs(title = "Raw p-values") +
    ylim(c(0, 6))

p2 <- ggplot(pvalues, aes(x = pvalue.wilcoxon.adjusted)) +
        geom_histogram(na.rm = TRUE, binwidth = 0.001) +
        labs(title = "Adjusted p-values") +
    ylim(c(0, 6)) 

library(patchwork)
print(p1 + p2)

library(qvalue)
# Benjamini-Hochberg limit
 p = pvalues$pvalue.wilcoxon
 p.adj = p.adjust(p, method='BH')

#Storey method
qobj = qvalue(p)
summary(qobj)
plot(qobj)
hist(qobj)

