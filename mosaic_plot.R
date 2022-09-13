#Mosaic plot was drawn to examine the relationship between the categorical variables.

#Following R command were used to extract the variables “Driver” and “Event” from “Data.csv” file.

mydata <- read.csv(file="data.csv",head=TRUE,sep=",")

#Then contingency table was created using the following R command: 

 drivevnt <- table(mydata$driver, mydata$event)

#Finally mosaic plot was drawn using the following command:

mosaicplot(drivevnt, main="Mosaic plot", xlab="Driver", ylab="Event", col=c("darkblue","red"))
