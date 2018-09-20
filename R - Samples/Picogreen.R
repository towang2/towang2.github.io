library(plotly)
library(RColorBrewer)

Day1 <- c(1.901, 1.602, 1.226)
Day3 <- c(2.544, 1.759, 1.51)
Day7 <- c(5.748, 7.104, 4.366)
Standard <- c(12.35, 6.36, 3.073, 1.599, 0.8163, 0.4356, 0.252)
DNA <- c(500.00, 250.00, 125.00, 62.5, 31.25, 15.625, 0.00)
#Assigning vectors to each row of your Picogreen data
DNAStandard <- data.frame(Standard, DNA)
#Converts Picogreen standard solution data to table DNAStandard
DNAStandard

plot_ly(DNAStandard, x=DNA, y=Standard, type="scatter")
#Plots intensity on y axis and DNA concentration on x axis as a scatter plot
regression <- lm(DNA ~ Standard, data = DNAStandard)
#Performs linear regression on the scatter plot 

summary(regression)
#Obtains a summary of the linear regression
coef(regression)["(Intercept)"] 
#Obtains the x intercept of the linear regression line
coef(regression)["Standard"] 
#Obtains the slope of the linear regression line

Measurement <- matrix(c(Day1, Day3, Day7),byrow=TRUE, nrow=3)
#Arranges Day1, Day3, Day7 measurements into a matrix arranged by row with 3 rows
Measurement <- Measurement*coef(regression)["Standard"] +coef(regression)["(Intercept)"] 
#Standardizes the measurements by plugging into linear regression equation

StandardDeviation <- c(sd(Measurement[1,]), sd(Measurement[2,]), sd(Measurement[3,]))
#Arranges Standard Deviations of Day 1, Day 3, Day 7 values into vector StandardDeviation
Measurement <- rowMeans(Measurement)
#Obtains the averages of each row in matrix measurement for Day1, Day3, and Day7
Days <- c("1", "3", "7")
#Creates a vector specifying the variable Day

Picogreen <- data.frame(Measurement, StandardDeviation, Days)
#Creates a dataframe with 3 columns: Measurement, Standard Deviation, and Days
plot_ly(Picogreen, x = ~Days, y = ~Measurement, type="bar",  marker=list(color=c('lightblue', 'blue', 'darkblue')), error_y = ~list(type = "data", array = StandardDeviation, color = '#000000')) %>%
  layout(title = "Picogreen Proliferation Assay",
         yaxis = list(side = 'left', title = 'DNA Content (ng)', showgrid = FALSE, zeroline = FALSE))
#Plots out Picogreen with days on x axis, measurement values on y axis as a bar graph with standard deviation as error bars
