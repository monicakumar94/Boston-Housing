## Dimension Reduction


library(tidyverse)
library(reshape)
library(gplots)
library(leaps)
library(forecast)
library(gains)



  # DIMENSIONALITY ANALYSIS

## Read in Boston Housing Data
boston.housing.df <- read.csv("BostonHousing.csv", header = TRUE) 
head(boston.housing.df)
summary(boston.housing.df) 

# Compute Statistics - mean, standard dev, min, max, median, length, and missing values of CRIM
mean(boston.housing.df$CRIM) 
sd(boston.housing.df$CRIM)
min(boston.housing.df$CRIM)
max(boston.housing.df$CRIM)
median(boston.housing.df$CRIM) 
length(boston.housing.df$CRIM) 

  ### find the number of missing values of variable CRIM
sum(is.na(boston.housing.df$CRIM)) 

  ### Compute statistics for all variables
summ1 <- data.frame(mean=sapply(boston.housing.df, mean), 
                         sd=sapply(boston.housing.df, sd), 
                         min=sapply(boston.housing.df, min), 
                         max=sapply(boston.housing.df, max), 
                         median=sapply(boston.housing.df, median), 
                         length=sapply(boston.housing.df, length),
                         miss.val=sapply(boston.housing.df, function(x) 
                         sum((is.na(x)))))
options(scipen = 999)
print(summ1, digits=1)

  ### Correlation Matrix
round(cor(boston.housing.df),2)

  ### heatmap with values
library(gplots)
heatmap.2(cor(boston.housing.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(boston.housing.df),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

  ### Frequency Table (by CHAS)
table(boston.housing.df$CHAS)

  ### Frequency Table by multiple categorical variables
    ### convert to numerical variable to categorical
boston.housing.df$RM.bin <- .bincode(boston.housing.df$RM, c(1:9))

    ### compute the average of MEDV by (binned) RM and CHAS
aggregate(boston.housing.df$MEDV, by=list(RM=boston.housing.df$RM.bin, 
                                          CHAS=boston.housing.df$CHAS), FUN=mean) 



  ### Generate Pivot Table using reshpate - melt() and cast()
mlt <- melt(boston.housing.df, id=c("RM.bin", "CHAS"), measure=c("MEDV"))
head(mlt, 5)

    ### use cast() to reshape data and generate pivot table
cast(mlt, RM.bin ~ CHAS, subset=variable=="MEDV", 
     margins=c("grand_row", "grand_col"), mean)


  ### Distribution using Barplot
library(ggmap)
tbl <- table(boston.housing.df$CAT..MEDV, boston.housing.df$ZN)
prop.tbl <- prop.table(tbl, margin=2)
barplot(prop.tbl, col =c("darkblue", "red"), 
        xlab="ZN", ylab="", yaxt="n", main="Distribution of CAT.MEDV by ZN")
axis(2, at=(seq(0,1, 0.2)), paste(seq(0,100,20), "%"))















