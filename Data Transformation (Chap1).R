"""
LOADING DATA
"""
# Loading AppliedPredictiveModeling
library(AppliedPredictiveModeling)

# Loading Segmentation data
# Segmentation data has 2019 Observations & 119 Variables
data(segmentationOriginal)

# Variable "Case" indicates which cells are used as train & test (Originally)
# Train data has 1009 Observations 
SegData <-subset(segmentationOriginal, Case == "Train")

# Creating Seperate Vectors
cellID <- SegData$Cell
class <- SegData$Class
case <- SegData$Case

# Removing above variables from "SegData"
SegData <- SegData[, -(1:3)]

# Identifying Status Columns (Binary format of some predictors) 
# There are 58 Status variables (for 58 Original variables)
statusColNum <- grep("Status", names(SegData))
statusColNum
SegData <- SegData[, -statusColNum]

"""
TRANSFORMATIONS
"""
# Loading Library
library(e1071)

# Skewness for 1 predictor
skewness(SegData$AngleCh1)

# Applying "skewness" function to all
skewValues <- apply(SegData, 2, skewness)
skewValues

# Applying sort function
skewValues_sort <- sort.int(skewValues, decreasing = TRUE)
head (skewValues_sort)
tail (skewValues_sort)

# Loadign "caret" for applying BoxCox Transformations
library(caret)
Ch1AreaTrans <- BoxCoxTrans(SegData$AreaCh1)
Ch1AreaTrans

# The Original Data
head(SegData$AreaCh1)

# After BoxCox Transformation
predict(Ch1AreaTrans, head(SegData$AreaCh1))

# Centering & Scaling
pcaObject <- prcomp(SegData, center = TRUE, scale. = TRUE)
# Calculate the cumulative percentage of variance which each component
# accounts for.
percentVariance <- pcaObject$sd^2/sum(pcaObject$sd^2)*100
percentVariance[1:3]
# The transformed values are stored in pcaObject as a sub-object called x:
head(pcaObject$x[, 1:5])
# Variable Loadings are stored in pcaobject as a sub-object "rotation"
head(pcaObject$rotation[, 1:3])

# Box-Cox transform, center, and scale the data, 
# then execute PCA for signal extraction, the syntax would be:
trans <- preProcess(SegData, method = c("BoxCox", "center", "scale", "pca"))
trans

# Apply the transformations:
transformed <- predict(trans, SegData)
head(transformed[, 1:5])

"""
FILTERING  
"""
# Variables with Non Zero Variance will be filtered
nearZeroVar(SegData)

# Filtering variables with Collinearity
# Correlation
correlations <- cor(SegData)
# dimensions of correlations - 58 x 58 Matrix
dim(correlations)

# Loading Corrplot for visually seeing the correlations....
library(corrplot)
corrplot(correlations, order = "hclust")

# Finding variables with very high correlations...

# This function recommends variables with high correlations, 
# which can be removed
highCorr <- findCorrelation(correlations, cutoff = .75)
length(highCorr)

# 33 variables are highly correlated
head(highCorr)
# filtering data without highly correlated variables
fitteredSegData <- SegData[, -highCorr]