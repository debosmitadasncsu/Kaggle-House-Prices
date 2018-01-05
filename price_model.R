### install.packages("Boruta")
### install.packages("ranger")
library(dplyr)
library(ranger)
library(Boruta)

traindata <- read.csv("/Users/abhimanyujataria/Downloads/Kaggle Data Science/House Priceing/train.csv", header=TRUE, stringsAsFactors=FALSE)
traindata[traindata == ""] <- NA
pool <- traindata%>%
  group_by(traindata$BsmtFinType1)%>%
  summarise(count = n())

### Changing NA to No Alley in Alley column
traindata$Alley [is.na(traindata$Alley)] <- "No Alley"

### Missing values mean
traindata$LotFrontage <- ifelse(is.na(traindata$LotFrontage),
                                ave(traindata$LotFrontage, FUN = function(x) mean(x, na.rm = TRUE)),
                                traindata$LotFrontage)

traindata$MasVnrArea <- ifelse(is.na(traindata$MasVnrArea),
                               ave(traindata$MasVnrArea, FUN = function(x) mean(x, na.rm = TRUE)),
                               traindata$MasVnrArea)

traindata$GarageYrBlt <- ifelse(is.na(traindata$GarageYrBlt),
                               ave(traindata$GarageYrBlt, FUN = function(x) round(mean(x, na.rm = TRUE), digits = 0)),
                               traindata$GarageYrBlt)

##### Short-cut for mode
traindata$GarageCond [is.na(traindata$GarageCond)] <- "TA"
traindata$GarageQual [is.na(traindata$GarageQual)] <- "TA"
traindata$GarageFinish [is.na(traindata$GarageFinish)] <- "Unf"
traindata$GarageType [is.na(traindata$GarageType)] <- "Attchd"
traindata$BsmtCond [is.na(traindata$BsmtCond)] <- "TA"
traindata$MasVnrType [is.na(traindata$MasVnrType)] <- "None"
traindata$BsmtQual [is.na(traindata$BsmtQual)] <- "TA"
traindata$BsmtExposure [is.na(traindata$BsmtExposure)] <- "No"
traindata$BsmtFinType1 [is.na(traindata$BsmtFinType1)] <- "Unf"
traindata$BsmtFinType2 [is.na(traindata$BsmtFinType2)] <- "Unf"
traindata$Electrical [is.na(traindata$Electrical)] <- "SBrkr"


####### Removing columns
traindata$Fence <- NULL
traindata$PoolQC <- NULL
traindata$FireplaceQu <- NULL
traindata$MiscFeature <- NULL
traindata$Fence <- NULL

sum(is.na(traindata))

traindata <- as.data.frame(unclass(traindata))
traindata$YrSold <- as.factor(traindata$YrSold)
traindata$MoSold <- as.factor(traindata$MoSold)
traindata$MiscVal <- as.factor(traindata$MiscVal)
traindata$PoolArea <- as.factor(traindata$PoolArea)
traindata$ScreenPorch <- as.factor(traindata$ScreenPorch)
traindata$X3SsnPorch <- as.factor(traindata$X3SsnPorch)
traindata$EnclosedPorch <- as.factor(traindata$EnclosedPorch)
traindata$WoodDeckSF <- as.factor(traindata$WoodDeckSF)
traindata$OpenPorchSF <- as.factor(traindata$OpenPorchSF)
traindata$GarageArea <- as.factor(traindata$GarageArea)
traindata$GarageCars <- as.factor(traindata$GarageCars)
traindata$GarageYrBlt <- as.factor(traindata$GarageYrBlt)
traindata$Fireplaces <- as.factor(traindata$Fireplaces)
traindata$TotRmsAbvGrd <- as.factor(traindata$TotRmsAbvGrd)
traindata$KitchenAbvGr <- as.factor(traindata$KitchenAbvGr)
traindata$BedroomAbvGr <- as.factor(traindata$BedroomAbvGr)
traindata$HalfBath <- as.factor(traindata$HalfBath)
traindata$FullBath <- as.factor(traindata$FullBath)
traindata$BsmtHalfBath <- as.factor(traindata$BsmtHalfBath)
traindata$BsmtFullBath <- as.factor(traindata$BsmtFullBath)
traindata$GrLivArea <- as.factor(traindata$GrLivArea)
traindata$LowQualFinSF <- as.factor(traindata$LowQualFinSF)
traindata$X2ndFlrSF <- as.factor(traindata$X2ndFlrSF)
traindata$X1stFlrSF <- as.factor(traindata$X1stFlrSF)
traindata$TotalBsmtSF <- as.factor(traindata$TotalBsmtSF)
traindata$BsmtUnfSF <- as.factor(traindata$BsmtUnfSF)
traindata$BsmtFinSF2 <- as.factor(traindata$BsmtFinSF2)
traindata$BsmtFinSF1 <- as.factor(traindata$BsmtFinSF1)
traindata$MasVnrArea <- as.factor(traindata$MasVnrArea)
traindata$YearRemodAdd <- as.factor(traindata$YearRemodAdd)
traindata$YearBuilt <- as.factor(traindata$YearBuilt)
traindata$OverallCond <- as.factor(traindata$OverallCond)
traindata$OverallQual <- as.factor(traindata$OverallQual)
traindata$Id <- as.factor(traindata$Id)


####### Boruta
set.seed(123)
boruta.train <- Boruta(SalePrice~. , data = traindata, doTrace = 2)
print(boruta.train)


