#read in the data
full <- read.csv("./data/full_titanic.csv", header = T)
full <- full[order(full$PassengerId), ]

# Split the data back into a train set and a test set
train <- full[1:891, ]
test <- full[892: nrow(full), ]


##use caretList to make a list and comparison of models
library(caretEnsemble)
str(train)
train$Survived <- as.factor(train$Survived)
form <- Survived ~ Age + Pclass + Sex + SibSp + Parch + Fare + 
        Embarked + Title + FsizeD + TicketNew + Deck + Mother + Child
#form <- Survived ~ Age + Pclass + Sex + SibSp + Fare + 
#       Embarked + Title + FsizeD + TicketNew
control <- trainControl(method = "repeatedcv", 
                        number = 10, 
                        repeats = 3, 
                        savePredictions = "final", 
                        classProbs = TRUE
)

#lda(lda), gbm(gbm), svm(svmRadial), glm(glm), vgl(vglmAdjCat), rf(rf)
algorithmList <- c("svmRadial")
set.seed(754)
models <- caretList(form = form, 
                    data=train, 
                    trControl=control, 
                    methodList=algorithmList)
results <- resamples(models)
first.five <- summary(results)
second.three <-summary(results)
dotplot(results)

#Combine models above into ensemble method--Only way to get any kind of pick up
# Example of Stacking algorithms--General Linear Model
stackControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3, 
                             savePredictions=TRUE, 
                             classProbs=TRUE
)
set.seed(754)
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)

# Example of Stacking Algorithm--Random Forest
set.seed(754)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)


###########################
print(rf_model)
importance <- varImp(rf_model, scale=FALSE)
summary(rf_model)
Prediction <- predict(fit.xgbt, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
submit$Survived <- as.integer(levels(submit$Survived))[submit$Survived]
write.csv(submit, file = "2016-10-31-12-15-Solution.csv", row.names = FALSE)
