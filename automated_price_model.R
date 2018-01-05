### install.packages("Boruta")
### install.packages("ranger")
library(dplyr)
library(ranger)
library(Boruta)

traindata <- read.csv("/Users/abhimanyujataria/Downloads/Kaggle Data Science/House Priceing/train.csv", header=TRUE, stringsAsFactors=FALSE)
traindata[traindata == ""] <- NA

datafn <- function(table, col){
  require("dplyr")
  print(col)
  col <- as.name(col)
  table%>%
    group_by_(col)%>%
    summarise(count = n())
}
#test
pool <- traindata%>%
  group_by(traindata$MasVnrType)%>%
  summarise(count = n())


### Changing NA to No Alley in Alley column
traindata$Alley [is.na(traindata$Alley)] <- "No Alley"

### Missing values
print(sum(is.na(traindata)))
##test
for(i in names(traindata)){
  if(sum(is.na(traindata[[i]])) > 500)
  {
    traindata[[i]] <- NULL
  }
  else
  {
    if(mode(traindata[[i]]) == "numeric")
    {
      traindata[[i]] <- ifelse(is.na(traindata[[i]]),
                               ave(traindata[[i]], FUN = function(x) mean(x, na.rm = TRUE)),
                               traindata[[i]])
    }
    else
    {
      pool <- datafn(traindata, i)
      #print(pool)
      traindata[[i]] <- ifelse(is.na(traindata[[i]]),
                               ave(traindata[[i]], FUN = function(x) x = pool[pool$count == (max(pool$count, na.rm = TRUE)),][[i]]),
                               traindata[[i]])
      pool <- datafn(traindata, i)
    }
  }
}

print(sum(is.na(traindata)))



## changing factors

for(i in names(traindata))
{
  pool <- datafn(traindata, i)
  traindata[[i]] <- factor(traindata[[i]],
                           levels = as.vector(pool[[i]]),
                           labels = c(1:nrow(pool)))
}

####### Boruta
set.seed(123)
boruta.train <- Boruta(SalePrice~. , data = traindata, doTrace = 2)
print(boruta.train)


##### Plot Boruta
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

#### List of important features
# getSelectedAttributes(final.boruta, withTentative = F)



# #### Random Forest
# ##install.packages("randomForest")
# library(randomForest)
# set.seed(121)
# regressor <- randomForest(x = traindata[, 1:76],
#                           y = traindata$SalePrice,
#                           ntree = 5)
