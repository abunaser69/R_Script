#library
library('dplyr')
library('pivottabler')

#reading data files
validation <- read.csv("999930041_LC1_HP_1.ixo_20200803155131.csv")
production <- read.csv("CBVAL_999930060_LC5_OC_3.ixo_20200802170329.csv")
#head(validation)
validation_result <-validation %>% select(Sample_ID, Result,ResultInfo)  
validation_sub <- validation %>% select(Sample_ID, Result, Channel_1_Cq, Channel_1_EndFluoRaw, Channel_2_Cq, Channel_2_EndFluoRaw)

production_sub <- production %>% select(Sample_ID, Result, Channel_1_Cq, Channel_1_EndFluoRaw, Channel_2_Cq, Channel_2_EndFluoRaw)

#concordance
colSums(df[,seq(1,ncol(df),2)]==df[,seq(2,ncol(df),2)])/nrow(df)
required_df <- df2[df2$genecolumn %in% df1$gene_list_column_name,]
required_df <- production_sub[production_sub$Result %in% validation_sub$Result,]
m <- merge(production_sub, validation_sub, by = c("Result"), all = FALSE)

library(arsenal)
comparedf(production_sub, validation_sub, by="Sample_ID")
summary(comparedf(production_sub, validation_sub))
summary(comparedf(as.data.frame(production_sub$Result), as.data.frame(validation_sub$Result)))

com <- comparedf(as.data.frame(production_sub$Result), as.data.frame(validation_sub$Result))

#how many differences were found,
n.diffs(com)

#Differences can also be summarized by variable.

diffs(com, by.var = TRUE)

diffs(cmp, vars = c("ps", "ast"), by.var = TRUE)

diffs(cmp, vars = c("ps", "ast"))
#######################################################################################
#Another way of matching
df2 <- df %>% 
  mutate(D = lead(B, 1)) # value of next row is new column
  
df3 <- df2 %>% 
  filter(A == "X" & D == 1) 
  
df3 %>% 
  count(A)
  library(dplyr)
inner_join(df1, df2, by = c("ID" = "Id")) # Because Id columns names are different
inner_join(df1, df2) # If the Id columns were the same
myFreqs <- df.example %>%  
  group_by(Group, Size) %>% 
  summarise(Freq = n()) 
  
groupDataAdjusted  <- left_join(groupDataUnadjusted, nationalPuf, by = c("year", "income_group","Joint","children") %>% 
                            mutate(tax_difference_pct = tax_difference_pct + diff)
							
 result <- merge(as.data.frame(production_sub$Result), as.data.frame(validation_sub$Result), by="Result") 
##############################################################################
#piot table
library(pivottabler)
pt <- PivotTable$new()
pt$addData(production_sub)
pt$addData(validation_sub)
pt$addColumnDataGroups("Result")
pt$addColumnDataGroups("PowerType")    #    << **** CODE CHANGE **** <<
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName="Result", summariseExpression="n()")
pt$renderPivot()
########################################################################################################################
tab <- read.csv("Anna_EXP_20201027_02.csv")

colnames(tab)<-c("PSample_ID",	"PResultInfo",	"PChannel_1_Cq",
                 "PChannel_1_EndFluoRaw",	"PChannel_1_EndFluoBC",
                 "PChannel_2_Cq",	"PChannel_2_EndFluoRaw",
                 "PChannel_2_EndFluoBC",	"VSample_ID",	"VResultInfo",
                 "VChannel_1_Cq",	"VChannel_1_EndFluoRaw",
                 "VChannel_1_EndFluoBC",	"VChannel_2_Cq",
                 "VChannel_2_EndFluoRaw",	"VChannel_2_EndFluoBC",
                 "Concordant?",	"Change" )

library(ggplot2)

ggplot(tab, aes(x=PChannel_2_Cq,y=VChannel_2_Cq))+
  geom_point()+
  geom_smooth()+
  labs(title = "IEC Cq", subtitle = "Outliers included")+
  xlab("Production")+
  ylab("Validation")

ggplot(tab, aes(x=PChannel_2_Cq,y=VChannel_2_Cq))+
  geom_point()+
  geom_smooth()+
  xlim(16,30)+
  ylim(15,30)+
  labs(title = "IEC Cq", subtitle = "Outliers excluded")+
  xlab("Production")+
  ylab("Validation")

ggplot(tab, aes(x=PChannel_1_Cq,y=VChannel_1_Cq))+
  geom_point()+
  geom_smooth()+
  labs(title = "SARS Cq", subtitle = "Outliers included")+
  xlab("Production")+
  ylab("Validation")

ggplot(tab, aes(x=PChannel_1_Cq,y=VChannel_1_Cq))+
  geom_point()+
  geom_smooth()+
  xlim(15,40)+
  ylim(12,35)+
  labs(title = "SARS Cq", subtitle = "Outliers excluded")+
  xlab("Production")+
  ylab("Validation")

ggplot(tab, aes(x=PChannel_2_EndFluoBC,y=VChannel_2_EndFluoBC))+
  geom_point()+
  geom_smooth()+
  labs(title = "IEC end fluorescence", subtitle = "Baseline corrected")+
  xlab("Production")+
  ylab("Validation")

tab <- subset( tab, `Concordant?`==TRUE)
tab <- subset( tab, PResultInfo=="2019-nCoV detected.")

ggplot(tab, aes(x=PChannel_1_EndFluoBC,y=VChannel_1_EndFluoBC))+
  geom_point()+
  geom_smooth()+
  labs(title = "SARS end fluorescence", subtitle = "Baseline corrected, concordant positives only")+
  xlab("Production")+
  ylab("Validation")
