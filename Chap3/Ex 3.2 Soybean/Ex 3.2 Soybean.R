"""
Applied Predictive Analytics

Excercises 3.2: To predict diseases in Soybeans
A CLASSIFICATION Problem
- 35 Variables (Mostly Categorical)
- 19 Outcome classes 

This for practise purpose only
"""

# Loading the database from "mlbench"
library(mlbench) # For standard meachine learning databases
library(ggplot2) # For graphics
library(scales) # For percent scales
data(Soybean) 

# Creating a plot function
# Column Names to be passed for this function
# Scale is converted from count to % 
myplot <- function(x_string){
  ggplot(Soybean, aes_string(x =x_string)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), binwidth = 25) + 
  scale_y_continuous(labels = percent_format())
}

# Getting colnames
# If any non-categorical variables are there, they need to be removed
colNam <- colnames(Soybean)

# Applying myplot function to all colnames
plots = lapply(unique(colNam), myplot)

# gridExtra used for grid formation, usually par(mfrow) can be used
library(gridExtra)

# Plots all variables in one plot
do.call(grid.arrange, plots)
# For smaller grids below code is used
multiplot <- do.call(marrangeGrob, c(plots, list(nrow = 3, ncol = 2)))
# Saving the Output in ml.pdf
ggsave("ml.pdf", multiplot)