library('dplyr')
library('ggplot2')
library('tidyr')


#reading data files
production <- read.csv("003105765_LC14_ZK_1.ixo_20201201141933.csv")

production_sub <- production %>%
select(all_of(select_colnames))

#selecting and renaming columns
select_colnames <- c("Sample.ID", "Well", "Result", "ResultInfo")

production_sub$Row <- substr(production_sub$Well,0,1)
production_sub$Col <- substr(production_sub$Well,2,4) 

Plate1<-"Plate-1"
Plate2<-"Plate-2"
Plate3<-"Plate-3"
Plate4<-"Plate-4"
  
production_sub$Plate <- as.character(apply(production_sub, 1, 
                          function(x){
						       if(x[6] %in% seq(1,23,2) & x[5] %in% LETTERS[seq(1,18, 2)])Plate1 
						  else if(x[6] %in% seq(2,24,2) & x[5] %in% LETTERS[seq(1,18, 2)])Plate2 
						  else if(x[6] %in% seq(1,23,2) & x[5] %in% LETTERS[seq(2,18, 2)])Plate3 
						  else if(x[6] %in% seq(2,24,2) & x[5] %in% LETTERS[seq(2,18, 2)])Plate4}))
						  

write.csv(production_sub, "Results.csv")
