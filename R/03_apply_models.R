full <- read.csv("./data/full_titanic.csv", header = T)
full <- full[order(full$PassengerId), ]
# Split the data back into a train set and a test set
train <- full[1:891, ]
test <- full[892: nrow(full), ]

#Try a variety of models and use the one with the highest accuracy
set.seed(754)
control <- trainControl(method = "repeatedcv", 
                        number = 10, 
                        repeats = 3,
                        #savePredictions = "final", 
                        #classProbs = TRUE,
                        #summaryFunction = twoClassSummary
                        )
metric <- "Accuracy"
set.seed(754)

#formula
train$Survived <- as.factor(train$Survived)
form <- Survived ~ Age + Pclass + Sex + SibSp + Parch + Fare + 
        Embarked + Title + FsizeD + TicketNew + Deck + Mother + Child

#models
fit.xgbt <- train(form = form,
                  data = train,
                  method = "xgbTree",
                  metric = metric,
                  trControl = control
                  )
fit.lasso <- train(form = form,
                   data = train,
                   method = "lasso",
                   metric = metric,
                   trControl = control
                   )
fit.lda <- train(form = form, 
                 data=train, method="lda", metric=metric, trControl=control)

fit.lda
plot(varImp(fit.lda,scale=F))
library(pROC)
library(pROC)
predictions <- predict(fit.lda, test, type='prob')
auc <- roc(ifelse(test[, 2]=="lived",1,0), predictions[[2]])
head(predictions)

metric <- "Accuracy"
set.seed(754)
fit.cart <- train(form = form,   
                  data=train, method="rpart", metric=metric, trControl=control)
set.seed(754)
fit.knn <- train(form = form,
                 data=train, method="knn", metric=metric, trControl=control)
set.seed(754)
fit.svm <- train(form = form,
                 data=train, method="svmRadial", metric=metric, trControl=control)
set.seed(754)
fit.rf <- train(form = form,
                data=train, method="rf", metric=metric, trControl=control)
set.seed(754)
fit.gbm <- train(form = form,
                 data=train, method="gbm", metric=metric, trControl=control)
plot(varImp(fit.gbm,scale=F))
set.seed(754)
fit.glm <- train(form = form,
                data=train, method="glm", metric=metric, trControl=control)
plot(varImp((fit.glm)))
set.seed(754)
fit.lrm <- train(form = form,
                 data=train, 
                 method="vglmAdjCat", 
                 metric=metric, 
                 trControl=control
                 #nleaves = 50
                 #ntrees = 200
                 )

fit.vgl <- train(form = form,
                 data=train, 
                 method="vglmAdjCat", 
                 metric=metric, 
                 trControl=control
                 )
                 
fit.lb <- train(form = form,
                 data=train, 
                 method="LogitBoost", 
                 metric=metric, 
                 trControl=control
)
 



results <- resamples(list(lda = fit.lda, 
                          #cart = fit.cart, 
                          #knn = fit.knn,
                          svm = fit.svm,
                          rf = fit.rf,
                          gbm = fit.gbm,
                          glm = fit.glm,
                          vgl = fit.vgl
                          #lb = fit.lb

)
)
sults <- summary(results)
dotplot(results)
modelCor(results)
splom(results)

#write out the data
