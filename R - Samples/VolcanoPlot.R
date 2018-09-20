library(plotly)

Control <- read.table("DataControl.txt")
BM <- read.table("DataBM.txt")
DataControl <- as.matrix(Control)
DataBM <- as.matrix(BM)
Difference <- DataBM - DataControl
#Calculates difference of treatment vs control groups gene expression
FoldChangeABCB1 <-mean(DataBM[1,])/mean(DataControl[1,])
FoldChangeABTA1 <-mean(DataBM[2,])/mean(DataControl[2,])
FoldChangeALCAM <-mean(DataBM[3,])/mean(DataControl[3,])
FoldChangeANPEP <-mean(DataBM[4,])/mean(DataControl[4,])
FoldChangeANXA5 <-mean(DataBM[5,])/mean(DataControl[5,])
FoldChangeBDNF <-mean(DataBM[6,])/mean(DataControl[6,])
FoldChangeBGLAP <-mean(DataBM[7,])/mean(DataControl[7,])
FoldChange <- matrix(c(FoldChangeABCB1, FoldChangeABTA1, FoldChangeALCAM, FoldChangeANPEP, 
                       FoldChangeANXA5, FoldChangeBDNF, FoldChangeBGLAP), byrow=FALSE, nrow=7)
FoldChange <- log2(FoldChange)
#Plots fold change of genes, performs log based 2 on fold change, plots as column
PvalueABCB1 <- t.test(Difference[1,])$p.value
PvalueACTA2 <- t.test(Difference[2,])$p.value
PvalueALCAM <- t.test(Difference[3,])$p.value
PvalueANPEP <- t.test(Difference[4,])$p.value
PvalueANXA5 <- t.test(Difference[5,])$p.value
PvalueBDNF <- t.test(Difference[6,])$p.value
PvalueBGLAP <- t.test(Difference[7,])$p.value
#Performs t value for Control and BM groups among the different patient groups for each gene
Pvalue <- matrix(c(PvalueABCB1, PvalueACTA2, PvalueALCAM, PvalueANPEP, PvalueANXA5, PvalueBDNF, PvalueBGLAP), byrow=FALSE, nrow=7)
Pvalue <- -log10(Pvalue)
#Performs -log10 of Pvalue, plots it in a column
VolcanoData <- cbind(FoldChange, Pvalue)
#Adds new column to DataBM matrix with P value
VolcanoData <- as.data.frame(VolcanoData)
#Converts VolcanoData to Data frame
attach(VolcanoData)
colnames(VolcanoData) = c("FoldChange", "Pvalue")
plot(FoldChange, Pvalue, main="Volcano Plot", 
     xlab="log2(FoldCHange) ", ylab="-log10(P-Value) ", pch=19)
#Plots log2FoldChange on x axis and -log10P-Value on y axis as volcano plot
