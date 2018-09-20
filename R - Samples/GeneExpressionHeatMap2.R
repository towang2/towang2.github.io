library(gplots)

datatable <- read.table("PCR_Array_Median.txt")
data <- scale(as.matrix(datatable))


heatmap.2(data,scale='none',trace="none", dendrogram =c("col"), key = TRUE, 
          keysize = 1,  key.ylab = NA, key.xlab = NA, 
          density.info = 'none',margins = c(7,10),lhei=c(1,10),col=redgreen(1000), symbreaks=TRUE)

