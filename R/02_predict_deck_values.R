full <- read.csv("./data/full_titanic_w_missing_deck_values.csv", 
                 header = T, stringsAsFactors = T)
#Convert from factor to string
full$Deck <- as.character(levels(full$Deck))[full$Deck]
#omit NAs
full$Deck <- as.factor(full$Deck)
#full <- full[, c(6, 3, 5, 7, 8, 9, 11, 12, 16, 20, 18, 19, 17)]
training <- full[grep("[A-Z]", full$Deck), ]
testing <-  full[-grep("[A-Z]", full$Deck), ]


##########Partial Least Squares Discriminant Analysis ("pls")
seed <- 7
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3,
                     classProbs = T
                     #summaryFunction = twoClassSummary
)
set.seed(seed = seed)
form <- Deck ~ Age + Pclass + Sex + SibSp + Parch + Fare + 
        Embarked + Title + FsizeD + TicketNew + Child + Mother

fit.rf <- train(form = form, 
                data = training,
                method = "rf",
                tuneLength = 3,
                metric = "Accuracy",
                trControl = ctrl
                )


fit.rf
plot(fit.rf)
rfClasses <- predict(fit.rf, newdata = testing)
testing$Deck <- rfClasses
full <- rbind(testing, training)
write.csv(full, file = "./data/full_titanic.csv", row.names = F)
