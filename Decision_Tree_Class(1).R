library(rpart)
library(ggplot2)
library(rlist)
library(gridExtra)
library(grid)
library(corrplot)
#library(DAAG)
#Beautify tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(pROC)


#read data file
data= read.csv("Churn.csv", na.strings=c("","NA"))
View(data)

#checking structure & summary of the data
str(data)
summary(data)


#treating unique identifier columns
data <- data[,-1]

#credit history as factor
data$SeniorCitizen <- as.factor(data$SeniorCitizen)

# #detecting and treating missing values
colSums(is.na(data))



#segregating Numeric & factor data
data.numeric <- data[sapply(data, is.numeric)]
data.factor <- data[sapply(data, is.factor)]

#Analysing & treating NA's in numeric data
summary(data.numeric)


#Analysing histogram of each numeric values
numplot <- function(column, df)
{
     ggplot(df, aes_string(x=column))+
          geom_histogram(aes(y=..density..),fill = "grey", color = "black")+
          geom_density(fill='blue', alpha=0.2)+
          xlab(column)
}


np <- lapply(colnames(data.numeric), numplot, df=data.numeric)
do.call("grid.arrange", np)


data.skewed <- apply(data.numeric, c(2), skewness)
data.skewed

data.kurtosis <- apply(data.numeric, c(2), kurtosis)
data.kurtosis

#boxplot analysis
apply(data.numeric,c(2), boxplot)


out_std_fix = function(x, y, f){
     m=mean(x[,y])
     s=sd(x[,y])
     lc=m-f*s #lower cut-off
     uc=m+f*s #upper cut-off
     out_value <- which(x[,y] > uc | x[,y] < lc)
     #aq_sd_fix <- aq[-out_value,]
     x[out_value,y] <- m
     x[,y]<-floor(x[,y])
     return(x)
}

data.numeric <- out_std_fix(data.numeric,3, 3) # calling fix function to fix outlier.

data.numeric$TotalAmount <- (data.numeric$TotalAmount)^(1/3)


# #bar plot for categorical varibale etc.
#
factplot <- function(column, df)
{
     ggplot(df, aes_string(x=column))+
          geom_bar(fill = "blue", color = "black", alpha= 0.2)+
          xlab(column)
}
#calling all bar plot
fp <- lapply(colnames(data.factor), factplot, df=data.factor)
do.call("grid.arrange", fp)


#Significance with target variable.
sigficant_var <- list()
df <- data.factor
for(i in 1:(length(df)-1))
{
     k= i+1
     y<-  chisq.test(table(df[,i], df[,k]))
     vec<-c(Var=paste(colnames(df[length(df)]),"-",colnames(df[i])), y$statistic, pval=y$p.value, significant = (if(y$p.value<0.05 & y$p.value!='NaN') "yes" else "no"))
     sigficant_var<-list.append(sigficant_var, vec)
}

print(sigficant_var)
str(sigficant_var)




