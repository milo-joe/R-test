
titanic.train <- read.csv("train.csv")
titanic.test <- read.csv("test.csv")

#preparing the data to be combined later
titanic.train$isTrain <- TRUE
titanic.test$isTrain <- FALSE

#adding "Survived" to the test set since it does not have one
titanic.test$Survived <- NA

#combining the datasets to deal with missing values faster
titanic.full <- rbind(titanic.train, titanic.test)

#checking for missing values
table(titanic.full$Embarked)

#change the missing data to the most used value
titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'

#checking for missing values in Age
table(is.na(titanic.full$Age))

#finding the median to used as default for the missing data
# na.rm = TRUE is used to take care of the null values in the date set
age.median <- median(titanic.full$Age, na.rm = TRUE)

#change the missing data to the most used value
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median


#checking for missing values in Fare
table(is.na(titanic.full$Fare))

#finding the median to used as default for the missing data
# na.rm = TRUE is used to take care of the null values in the date set
fare.median <- median(titanic.full$Fare, na.rm = TRUE)

#change the missing data to the most used value
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median

#categorical casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)


#divide titanic.full back to the original datasets
titanic.train <- titanic.full[titanic.full$isTrain == TRUE,]
titanic.test <- titanic.full[!titanic.full$isTrain == TRUE,]

#cast Survived to a category
titanic.train$Survived <- as.factor(titanic.train$Survived)

#creating the equation with the needed values
survived.equation <- "Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)

#installing and using the randomForest library
install.packages("randomForest")
library(randomForest)

#creating the model: mtry = sqrt(7) and  nodesizes = 1% 
titanic.model <- randomForest(formula = survived.formula, data = titanic.train, mtry = 3, nodesize = 0.01 * nrow(titanic.test))

#predicting Survived for the test data set
Survived <- predict(titanic.model, newdata = titanic.test)

#getting the passengerID 
PassengerId <- titanic.test$PassengerId

#combining PassengerId and Survived 
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

#making a csv file from the data frame
write.csv(output.df, file = "kaggleSub.csv", row.names = FALSE)