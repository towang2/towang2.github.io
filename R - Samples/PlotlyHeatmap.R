library(plotly)
GeneExpression = read.table("GeneExpression.txt")
#Imports table from GeneExpression.txt into variable GeneExpression
GeneExpression
#Prints out GeneExpression Dataset
head(GeneExpression)
data=as.matrix(GeneExpression)
#Expresses GeneExpression as matrix
data

data=apply(data, 2, function(x){x/mean(x)})
plot_ly(z=data, x=colnames(data),y=rownames(data), type = "heatmap",colors = colorRamp(c("Blue", "yellow")))
