df %>%
 slice(which(row_number() %% 5 == 1))

df %>%
 filter(row_number() %% 5 == 1)
even_indexes<-seq(2,42,2)
odd_indexes<-seq(1,41,2)
plate <- data.frame( Row = rep(LETTERS[1:16], 24)
                   , Col = unlist(lapply(1:24, rep, 16)))

plate$Row <-  factor(plate$Row, levels=rev(levels(plate$Row)))

set.seed(1)
plate[001:096, "val"] <- rpois(96,10)
plate[097:384, "val"] <- rpois(96, 6)

png("plate.png", h=400,w=600)
  ggplot2::ggplot(plate, ggplot2::aes(Col, Row, fill=val)) + ggplot2::geom_raster()
dev.off()

# plate needs to be sorted as follows: A1, A2, ..., P23, P24
plate$position <- paste0(plate$Row, plate$Col)
plate$position <- sub("^(.)(.)$", "\\10\\2", plate$position, perl=T)
plate <- plate[order(plate$position),]

library(HTqPCR)
mat <- matrix(plate$val, nrow = nrow(plate))
raw <- new("qPCRset", exprs = mat, featureCategory = as.data.frame(array("OK", nrow(plate))))
sampleNames(raw) <- "sampleName"
featureNames(raw) <- paste0("feature", 1:nrow(plate))

png("plate.png", h=400,w=600)
HTqPCR::plotCtCard(raw, col.range = c(0, 16), well.size = 2.6)
dev.off()
