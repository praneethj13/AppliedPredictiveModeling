"""
Applied Predictive Analytics

Excercises 3.3: To predict chemical properties 
from characteristic in chemical compound

QSAR Modeling
- 208 Cases (Compounds)
- 134 descriptors 

"""
library(caret) # Loading caret library
data(BloodBrain) # Loading Data

Data <- bbbDescr
library(e1071)
library(ggplot2)

"""
HISTOGRAMS OF DESCRIPTORS
"""
# Creating a plot function
PlotSkew <- function(x_string){
  plot_title <- paste("Skew ", round(skewness(Data[, x_string]),2))
  m <- ggplot(Data, aes_string(x = x_string))
  m + geom_histogram(aes(y = ..density..)) +
    geom_density() + theme_minimal()+ labs(title = plot_title)
}

colNam <- colnames(Data) # Getting all names of descriptors (columns)
plots = lapply(unique(colNam), PlotSkew) # Applying PlotSkew

# Creating Matrix Plots
multiplot <- do.call(marrangeGrob, c(plots, list(nrow = 3, ncol = 2)))
# Saving Matrix Plots in PDF
ggsave("Chap3/Ex 3.3 BloodBrain/Hist graphs.pdf", multiplot)


"""
CHECKING THE NEAR ZERO
"""
NearZero <- nearZeroVar(Data) # 6 Variables
DataNoZeroVar <- Data[, -NearZero] # Data without NearZeroVariables

"""
CORRELATION MATRIX
"""
# Loading psych for correlation matrix
library(corrplot)
correlations <- cor(DataNoZeroVar)

library(Cairo) #High Resolution Plotting
Cairo(2000, 1800, file="Chap3/Ex 3.3 BloodBrain/corrplot2.png", type="png", bg="white")
corrplot(correlations, order ="hclust", 
         method = "square", addrect = 10) # Correlation Plot
dev.off()

# Insignificant correlations are removed
cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(DataNoZeroVar, 0.95)
Cairo(2000, 1800, file="Chap3/Ex 3.3 BloodBrain/corrplot3.png", type="png", bg="white")
corrplot(correlations, p.mat = res1[[1]], 
         order ="hclust", insig = "blank", addrect=10)
dev.off()

"""
APPLYING DATA TRANSFORMATION
"""
# Applying BoxCox, Centering, Scaling
Transformation <- preProcess(DataNoZeroVar, 
                    method = c("BoxCox", "center", "scale"))
# Transformed Data
DataTrans <- predict(Transformation, DataNoZeroVar)

CorrTrans <- cor(DataTrans) # Correlation on Transformed data

"""
PLOTTING VARIABLES OF TRANSFORMED DATA
"""
colNam2 <- colnames(DataTrans) # Getting all names of descriptors (columns)
plots2 = lapply(unique(colNam2), PlotSkew) # Applying PlotSkew

# Creating Matrix Plots
multiplot2 <- do.call(marrangeGrob, c(plots2, list(nrow = 3, ncol = 2)))
# Saving Matrix Plots in PDF
ggsave("Chap3/Ex 3.3 BloodBrain/Hist graphs_Trans.pdf", multiplot2)

"""
CORRELATION MATRIX ON TRANSFORMED DATA
"""  

# Correlation Plot 1
Cairo(2000, 1800, file="Chap3/Ex 3.3 BloodBrain/corrTrans1.png", type="png", bg="white")
corrplot(CorrTrans, order ="hclust", addrect = 10) 
dev.off()

# Correlation Plot 2
Cairo(2000, 1800, file="Chap3/Ex 3.3 BloodBrain/corrTrans2.png", type="png", bg="white")
corrplot(CorrTrans, order ="hclust", method ="square", 
         addrect = 10) 
dev.off()

# Correlation Plot 3
resTrans1 <- cor.mtest(DataTrans, 0.95)
Cairo(2000, 1800, file="Chap3/Ex 3.3 BloodBrain/corrTrans3.png", type="png", bg="white")
corrplot(CorrTrans, p.mat = resTrans1[[1]], 
         order ="hclust", insig = "blank", addrect=10)
dev.off()

# Counting High Correlation Varibles
highCorr <- findCorrelation(CorrTrans, cutoff = .75)
length(highCorr)