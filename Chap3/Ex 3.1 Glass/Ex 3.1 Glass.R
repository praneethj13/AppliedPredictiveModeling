# Chap 3 (Excercise 1)

# Loading ML Benchmark Problems
# Loading Glass Data
library(mlbench)
data(Glass) # 214 Obs, 10 Variables
str(Glass)

skewness(Glass$RI)
hist(Glass$RI)

# Column or Variable Names
colNam <- colnames(Glass)

# Matrix Plot of Histogram(distribution) & Skewness
library(e1071)
par(mfrow=c(3, 3)) # 3 X 3 Matrix Plot
for(i in 1:9) {
  hist(Glass[, i], 
       main = paste(colNam[i], "Skew", 
                    round(skewness(Glass[, i]), 2)))
}       
      
par(mfrow=c(1, 1)) # Resetting the Plot

# Summary of all variables in Glass database
summary(Glass)

# Checking freq % of Ba, Fe
histogram(~Ba + Fe, data = Glass, Type = "percent" )

# Ba can be dropped, as it is a Near Zero Variance (Unique Values is 15%)
# Fe can be dropped, as it is a Near Zero Variance (Unique Values is 10%)

# Scatter Plot Matrix using ggplot2
library(ggplot2)
plotmatrix(Glass[, 1:9], mapping = aes(), colour = "black")

# Correlation Matrix
Correlation <- cor(Glass[, 1:9])
round(Correlation, 2)

# Loading Corr Plot
library(corrplot)
corrplot(Correlation, order = "hclust") 
# High correlation b/w Rl & Ca

# Removing the Type (factor) variable and creating new database
GlassData <- Glass[, -(8:10)]

# Calculating skewvalues
skewvalues <- apply(GlassData, 2, skewness)
skewvalues

# Applying Box Cox Transformations, Centering & Scaling to improve numerical stability
library(caret)
trans <- preProcess(GlassData,
                    method = c("BoxCox", "center", "scale"))
transformed <- predict(trans, GlassData)
skewvaluesTrans <- apply(transformed, 2, skewness)
skewvaluesTrans

# Applying PCA
GlassData_Rl_Ca <- GlassData[, -(2:6)]
pcaobject <- prcomp(GlassData_Rl_Ca,
                    center = TRUE, scale. = TRUE)

percentVariance <- pcaobject$sd^2/sum(pcaobject$sd^2)*100
percentVariance[1:2] # 90% Variance in explained by first PC

# Applying PCA for All 7
pcaobjectNew <- prcomp(GlassData,
                    center = TRUE, scale. = TRUE)

percentVarianceNew <- pcaobjectNew$sd^2/sum(pcaobjectNew$sd^2)*100
percentVarianceNew[1:5]
sum(percentVarianceNew[1:5]) # First 5 PC explain 98% Variance
pcaobjectNew$rotation[, 1:5] # Loadings


