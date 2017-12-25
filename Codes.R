getwd()
setwd("D:/Kaggle/Titanic/Dataset")
getwd()
train <- read.csv("train.csv")
test <- read.csv("test.csv")
library(rpart)
library(randomForest)
library(party)

#combined_dfne Test and train datasets for more detailed Analysis
test$Survived <- NA
combined_df <- rbind(train, test)
combined_df$Name <- as.character(combined_df$Name)

#Study passengers on: Title
combined_df$Title <- sapply(combined_df$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combined_df$Title <- sub(' ', '', combined_df$Title)
combined_df$Title[combined_df$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combined_df$Title[combined_df$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combined_df$Title[combined_df$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combined_df$Title[combined_df$Title %in% c('Ms')] <- 'Miss'
combined_df$Title <- factor(combined_df$Title)
unique(combined_df$Title)

#Study passengers on:Family Size
combined_df$FamilySize <- combined_df$SibSp + combined_df$Parch + 1
combined_df$Surname <- sapply(combined_df$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combined_df$FamilyID <- paste(as.character(combined_df$FamilySize), combined_df$Surname, sep="")
combined_df$FamilyID[combined_df$FamilySize <= 2] <- 'Small'

# Delete erroneous family IDs
famIDs <- data.frame(table(combined_df$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combined_df$FamilyID[combined_df$FamilyID %in% famIDs$Var1] <- 'Small'
combined_df$FamilyID <- factor(combined_df$FamilyID)

# Fill in Age NAs
summary(combined_df$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data=combined_df[!is.na(combined_df$Age),], method="anova")
combined_df$Age[is.na(combined_df$Age)] <- predict(Agefit, combined_df[is.na(combined_df$Age),])

# Check what else might be missing
summary(combined_df)

# Fill in Embarked blanks
summary(combined_df$Embarked)
which(combined_df$Embarked == '')
combined_df$Embarked[c(62,830)] = "S"
combined_df$Embarked <- factor(combined_df$Embarked)

# Fill in Fare NAs
summary(combined_df$Fare)
which(is.na(combined_df$Fare))
combined_df$Fare[1044] <- median(combined_df$Fare, na.rm=TRUE)

# New factor for Random Forests, only allowed <32 levels, so reduce number
combined_df$FamilyID2 <- combined_df$FamilyID

# Convert back to string
combined_df$FamilyID2 <- as.character(combined_df$FamilyID2)
combined_df$FamilyID2[combined_df$FamilySize <= 3] <- 'Small'
combined_df$FamilyID2[combined_df$FamilySize >=6] <- 'Large'
# And convert back to factor
combined_df$FamilyID2 <- factor(combined_df$FamilyID2)
summary(combined_df$FamilyID2)

# Split back into test and train sets
train <- combined_df[1:891,]
test <- combined_df[892:1309,]

# Build Random Forest Ensemble
set.seed(0)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2, data=train, importance=TRUE, ntree=880)
# Look at variable importance
varImpPlot(fit)
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
#write.csv(submit, file = "firstforest.csv", row.names = FALSE)

# Build condition inference tree Random Forest
set.seed(0)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=880, mtry=2)) 
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "Result240v2.csv", row.names = FALSE)