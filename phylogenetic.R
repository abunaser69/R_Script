library(microbiome) # data analysis and visualisation
library(phyloseq) # also the basis of data object. Data analysis and visualisation
library(microbiomeutilities) # some utility tools
library(RColorBrewer) # nice color options
library(ggpubr) # publication quality figures, based on ggplot2
library(DT) # interactive tables in html and markdown
library(data.table) # alternative to data.frame
library(dplyr) # data handling
library(ape) # Load tree file

#Loading library
invisible()
library("phyloseq")
library("ggplot2")
library("vegan")
library("DESeq2")
library("tidyverse")
library("microbiome")
library("dplyr")
library("plyr")

#loading biom file
mydata <- import_biom(BIOMfilename = "otu_table.v1.biom")

#loading meta file
mapfile <- import_qiime_sample_data("3metaforwardonlyshsthasampleswnecrotfiltered_corrected.txt.no_gz")

#merging taxnomic table and meta data
mydata_map <- merge_phyloseq(mydata, mapfile)

#changing name of taxinomic rank
colnames(tax_table(mydata_map)) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

#ignoring unclassified data
mydata_map_sub = subset_taxa(mydata_map, !is.na(Genus) & !Genus %in% c("g__", "uncharacterized", " "))

##Create table ready for making stacked bar graph for Genus <1%##
# get abundance in %
genus <- transform_sample_counts(mydata_map_sub, function(x)( x/sum(x)))
# agglomerate taxa
glom <- tax_glom(genus, taxrank = 'Genus')
# create dataframe from phyloseq object
dat <- psmelt(glom)

# convert Genus to a character vector from a factor because R
dat$Genus <- as.character(dat$Genus)
    
# group dataframe by Genus, calculate median rel. abundance
medians <- ddply(dat, ~Genus, function(x) c(median=median(x$Abundance)))

# find Genus whose rel. abund. is less than 0.1%
Other <- medians[medians$median <= 0.001,]$Genus
    

# change their name to "Other Genus"
dat[dat$Genus %in% Other,]$Genus <- 'Other'
#dat
#remove all Genus labeled Other Genus
dat <-dat[!dat$Genus == 'Other',]
    
#dat <-dat$Genus[!dat$Genus=='Other Genus',]
#remove unncessary columns
dat <- subset(dat, select=c(TreatmentGroup, Abundance, Genus))
#Arrange by Abundance
dat <- arrange(dat, Abundance)
#Create a table that is the leftover Genus
Abundance <- ddply(dat, ~TreatmentGroup, function(x) c(Abundance=1-sum(x$Abundance)))
#Add a column labeling the leftover Genus
Abundance$Genus<- 'Other'
#combine with original table
OneGenusatAb <- rbind(dat, Abundance)

#Plotting:
ggplot(OneGenusatAb)+
geom_col(aes(TreatmentGroup, Abundance, fill = Genus), colour="black", show.legend = TRUE) +
ylab("Relative Abundance") +
xlab("Treatment Group")

##Create table ready for making stacked bar graph for Phylum <x%##
# get abundance in %
phylum <- transform_sample_counts(mydata_map_sub, function(x)(x/sum(x)))
# agglomerate taxa
glom <- tax_glom(phylum, taxrank = 'Phylum')
# create dataframe from phyloseq object
dat <- psmelt(glom)

# convert Phylum to a character vector from a factor because R
dat$Phylum <- as.character(dat$Phylum)
    
# group dataframe by Phylum, calculate median rel. abundance
medians <- ddply(dat, ~Phylum, function(x) c(median=median(x$Abundance)))


# find Phylum whose rel. abund. is less than 1%
Other <- medians[medians$median <= 0.01,]$Phylum
    

# change their name to "Other"
dat[dat$Phylum %in% Other,]$Phylum <- 'Other'

#remove all Phylum labeled Other
dat <-dat[!dat$Phylum == 'Other',]

#remove unncessary columns
dat <- subset(dat, select=c(TreatmentGroup, Abundance, Phylum))

#Arrange by Abundance
dat <- arrange(dat, Abundance)

#Create a table that is the leftover Phylum
Abundance <- ddply(dat, ~TreatmentGroup, function(x) c(Abundance=1-sum(x$Abundance)))

    
#Add a column labeling the leftover Phylum
Abundance$Phylum<- 'Other'
#Abundance
#combine with original table
OneGenusatAb <- rbind(dat, Abundance)

ggplot(OneGenusatAb)+
geom_col(aes(TreatmentGroup, Abundance, fill = Phylum), colour="black", show.legend = TRUE) +
ylab("Relative Abundance") +
xlab("Treatment Group")

##Create table ready for making stacked bar graph for Genus <1%##
# get abundance in %
genus <- transform_sample_counts(mydata_map_sub, function(x) ( x/sum(x)))
# agglomerate taxa
glom <- tax_glom(genus, taxrank = 'Genus')
# create dataframe from phyloseq object
dat <- psmelt(glom)

# convert Genus to a character vector from a factor because R
dat$Genus <- as.character(dat$Genus)
    
# group dataframe by Genus, calculate median rel. abundance
medians <- ddply(dat, ~Genus, function(x) c(median=median(x$Abundance)))

# find Genus whose rel. abund. is less than 0.001%
Other <- medians[medians$median <= 0.0001,]$Genus
    

# change their name to "Other Genus"
dat[dat$Genus %in% Other,]$Genus <- 'Other'

#Plotting:
#ggplot(dat)+
#geom_col(aes(TreatmentGroup, Abundance, fill = Genus), colour="black", show.legend = TRUE) +
#ylab("Relative Abundance") +
#xlab("Treatment Group")
c_count = length(unique(dat$Genus))

getPalette = colorRampPalette(RColorBrewer::brewer.pal(c_count, "Paired"))

p2 <- ggplot(data=dat, aes(x=TreatmentGroup, y=Abundance, fill=Genus))

p2 <- p2 + geom_bar(aes(), stat="identity", position="stack", colour="black") + 
  scale_fill_manual(values=getPalette(c_count)) + theme(legend.position="bottom") + 
  guides(fill=guide_legend(nrow=5))
show(p2)

#pdf("plots_rel_abundances_Genus_level_collapsed.pdf", onefile = TRUE, width = 12, height = 6 ) # size in cm
#print(p1); print(p2)
#dev.off()

 ggplot(dat, aes(x=Genus, y=Abundance)) + geom_boxplot() + coord_flip()
