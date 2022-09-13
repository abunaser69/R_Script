#Load whole data
raw_data <- read.csv("MHSDS_Monthly_File_Oct_2018_Final.csv")

# Finding column name
#colnames(main_data)[109]

# deleting some columsn

#Data <- Data[,-(2:3)]             # vector
raw_data2 <- raw_data[,c(-(1:2), -(4:7)),drop=FALSE]  # still a data.frame


#selecting colmn 1
library("dplyr")
col1 <- raw_data2 %>% select(1)

col1 <- raw_data2 %>% select(1)
mhs <- raw_data2 %>% select(45:90)
write.csv(mhs, file = "mhs.csv")

amh <- raw_data2 %>% select(2:23)
write.csv(amh, file = "amh.csv")

mh <- raw_data2 %>% select(32:44)
write.csv(mh, file = "mh.csv")

ccr <- raw_data2 %>% select(91:102)
write.csv(ccr, file = "ccr.csv")

cyp <- raw_data2 %>% select(24:26)
write.csv(cyp, file = "cyp.csv")

lda <- raw_data2 %>% select(27:31)
write.csv(lda, file = "lda.csv")

#cleaning each catagory
clean <- function(data){
   as.numeric( gsub('[:alpha:]', '', gsub('*', '', gsub('-', '', data))))
 }

clean_amh <- sapply(amh, clean)
clean_mhs <- sapply(mhs, clean)
clean_mh <- sapply(mh, clean)
clean_ccr <- sapply(ccr, clean)
clean_cyp <- sapply(cyp, clean)
clean_lda <- sapply(lda, clean)

write.csv(clean_amh, file = "clean_amh.csv")
write.csv(clean_mhs, file = "clean_mhs.csv")
write.csv(clean_mh, file = "clean_mh.csv")
write.csv(clean_ccr, file = "clean_ccr.csv")
write.csv(clean_cyp, file = "clean_cyp.csv")
write.csv(clean_lda, file = "clean_lda.csv")

clean_data <- cbind(col1, raw_data3)

#saving clean data
write.csv(clean_data, file = "clean_data.csv")

# Checking missing values

table(is.na(clean_data))

#Locating missing value

colSums(is.na(clean_data))

# dimenstion

dim(clean_data)


#omit all row containing missing value
na.omit(clean_data)

#checking missing value
library(naniar)
vis_miss(clean_data)
gg_miss_upset(clean_data)
n_var_miss(clean_data)
gg_miss_upset(clean_data, nsets = n_var_miss(clean_data))

library(ggplot2)

ggplot(clean_data,
       aes(x = Ozone,
           y = Solar.R)) +
geom_point()


# Number of missing per row
rowSums(is.na(clean_data))
# Number of missing per column/variable
colSums(is.na(clean_data)) 
write.csv(clean_data, file = "clean_data2.csv")

image(is.na(clean_data), main = "Missing Values", xlab = "Observation", ylab = "Variable",
    xaxt = "n", yaxt = "n", bty = "n")
axis(1, seq(0, 1, length.out = nrow(clean_data)), 1:nrow(clean_data), col = "white", font.axis=1, font.lab=1, cex.axis=0.5, cex.lab=0.5)
axis(2, seq(0, 101), names(clean_data), col = "white", las = 2, font.axis=1, font.lab=1, cex.axis=0.5, cex.lab=0.5)

# printing column name
colnames(clean_data)[1:102]
capture.output(colnames(clean_data)[1:102],file="column_name.doc")

# calculate % missing
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(clean_data,2,pMiss)
apply(clean_data,1,pMiss)

# missing data pattern
library(mice)
md.pattern(data)

# Missing value visualization
library(VIM)
aggr_plot <- aggr(clean_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(clean_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#pattern of missing data

library(mice)
md.pattern(clean_data)

# adding 0 to missing values
mutate_all(funs(replace(., is.na(.), 0))) #faster

#density plot
densityplot(as.numeric(unlist(clean_cyp)))

#extact duplicated value
clean_cyp[duplicated(clean_cyp)]

#setting missing to zero
clean_mhs[is.na(clean_mhs)] <-0
clean_mh[is.na(clean_mh)] <-0
clean_amh[is.na(clean_amh)] <-0
clean_ccr[is.na(clean_ccr)] <-0
clean_cyp[is.na(clean_cyp)] <-0
clean_lda[is.na(clean_lda)] <-0


#selected colum from each category

mhs_selected <- clean_mhs %>% select(45, 48:50, 52, 56, 66:68)
write.csv(mhs_selected, file = "mhs_selected.csv")

mh_selected <- clean_mh %>% select(32:35, 39:41, 43, 37)
write.csv(mh_selected, file = "mh_selected.csv")

amh_selected <- clean_amh %>% select(2, 4, 9:12, 20:23,16,17)
write.csv(amh_selected, file = "amh_selected.csv")

library(ggplot2)

#save image file

svg('filename.svg')
# make plot
dev.off()
