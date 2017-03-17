setwd("/home/shubham/Downloads")
train <- read.csv('train.csv', stringsAsFactors = F)
test <- read.csv('test.csv', stringsAsFactors = F)
##prop.table(data$Survived)
test$Survived <- rep(0,418)


#prop.table(table(train$Sex,train$Survived),1)
## all females survived
test$Survived[test$Sex == 'female'] <- 1

## adding a col child
train$Child <- 0
train$Child[train$Age < 18] <- 1

##aggregate(Survived ~ Child + Sex, data = train, FUN = sum)
##aggregate(Survived ~ Child + Sex, data = train, FUN = length)
##aggregate(Survived ~ Child + Sex, data = train, FUN = function(x){sum(x)/length(x)})
### since there is not much change therefore it is not considerable

## check on fare
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare > 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare > 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2, data = train, FUN = sum)
aggregate(Survived ~ Fare2 + Sex + Pclass, data = train, FUN = function(x) {sum(x)/length(x)})
## female of pclass 3 paying more than 20 are likely to die
test$Survived[test$Pclass == 3 & test$Sex == 'female' & test$Fare > 20] <- 0

####################### decision trees ###############################
library(rpart)
## use method ="anova" for conti.
fit <- rpart(Survived ~ Pclass + Sex + Age +SibSp + Embarked + Fare + Parch, data = train, method = "class")
plot(fit)
text(fit)
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)
Prediction <- predict(fit,test, type = "class")

################ Feature engineering #################
test$Survived <- NA
test$Child <- NA
test$Fare2 <- NA
combi <- rbind(train,test)
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Mlle','Mme')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
## for family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][1]})
combi$FamilyId <- paste(combi$Surname,as.character(combi$FamilySize), sep = "")
combi$FamilyId[combi$FamilySize <= 2] <- 'small'
familyId <- data.frame(table(combi$FamilyId))
familyId <- familyId[familyId$Freq <= 2,]
combi$FamilyId[combi$FamilyId %in% familyId$Var1] <- 'small'
combi$FamilyId <- as.factor(combi$FamilyId)
train <- combi[1:891,]
test <- combi[892:1309,]
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyId,
             data=train, 
             method="class")
fancyRpartPlot(fit)
Prediction <- predict(fit,test, type = "class")


################ Random Forest ####################3
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
##removing two value nil values of embarked
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = 'S'
combi$Embarked <- factor(combi$Embarked)
## which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
combi$Child[is.na(combi$Child)] = 0
## reducing the factor levels of familyid
combi$FamilyID2 <- combi$FamilyId
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
combi$Sex <- factor(combi$Sex)
combi$Pclass <- factor(combi$Pclass)
train <- combi[1:891,]
test <- combi[892:1309,]
install.packages('randomForest')
library(randomForest)
set.seed(1234)

fit <- randomForest(as.factor(Survived) ~ Sex + Pclass + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)
varImpPlot(fit)
Prediction <- predict(fit, test)
### conditional inferential trees
install.packages('party')
library(party)
set.seed(415)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyId,
               data = train, 
               controls = cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")


################# final  output ################################
submit <- data.frame(PassengerId = test$PassengerId, Survived= Prediction)
write.csv(submit,file = 'final.csv', row.names = F)
