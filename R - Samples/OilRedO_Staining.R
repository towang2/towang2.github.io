library(plotly)		

Blank <- 0.283		
Day_0 <- c(0.348, 0.414, 0.323)
Day_21 <- c(0.577, 0.597, 0.662)
#Setting Oil_Red as the values of Day_0 + Day_21, please plug in

Day_0_Subtracted <- Day_0 - Blank
Day_21_Subtracted <- Day_21 - Blank
#Subtracting the blank well measurement 0.283 from the Oil Red readings

Day_0_Average <- mean(Day_0_Subtracted)
Day_21_Average <- mean(Day_21_Subtracted)
#Calculates the averages for Day 0 and Day 21 Oil Red Readings
Day_0_SD <- sd(Day_0_Subtracted)
Day_21_SD <- sd(Day_21_Subtracted)
#Calculates the standard deviation for Day 0 and Day 21

Fold_Change_Average <- Day_21_Average/Day_0_Average
Fold_Change_Day0_AverageSD <- Day_0_SD/Day_0_Average
Fold_Change_Day21_AverageSD <- Day_21_SD/Day_21_Average
#Obtains the relative fold change for the average values and standard deviation

Day <- c("0", "21")
FoldChange <-c(1, Fold_Change_Average)
FoldChangeSD <- c(Fold_Change_Day0_AverageSD, Fold_Change_Day21_AverageSD)
OilRed <- data.frame(Day, FoldChange, FoldChangeSD)
#Merges vectors Day, Fold Change, FoldChangeSD to dataframe OilRed
OilRed 
#Prints out OilRedO Data

plot_ly(OilRed, x = ~Day, y = ~FoldChange, type="bar",  error_y = ~list(type = "data", array = FoldChangeSD, color = '#000000')) %>%
layout(title = "Oil Red O Staining",
       yaxis = list(side = 'left', title = 'Relative Fold Change', showgrid = FALSE, zeroline = FALSE))
#Prints plot with Day for x axis, Relative Fold Change for y axis for Oil Red O Staining 
