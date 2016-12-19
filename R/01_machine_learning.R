# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library("rattle")
library("rpart.plot")
library("RColorBrewer")
library("dplyr")
library("caret")
library("magrittr")
library("Boruta")
library("ranger")

#load train set
url <- "https://kaggle2.blob.core.windows.net/competitions-data/kaggle/3136/train.csv?sv=2015-12-11&sr=b&sig=WWLBHeNGin4ouODJRe0H2%2Byve8RAsqd6g9%2F3y%2BRGKCg%3D&se=2016-12-22T01%3A58%3A35Z&sp=r"
destfile <- "./data/kaggle_titanic_train.csv"
if (!file.exists(destfile)) download.file(url = url, destfile = destfile, 
                                          method = "curl", quiet = T
                                          )
train <- read.csv(file = destfile, header = T, stringsAsFactors = F)

#load test set
url <- "https://kaggle2.blob.core.windows.net/competitions-data/kaggle/3136/test.csv?sv=2015-12-11&sr=b&sig=foveupWv4Ukl8HhiH%2FEXdv9lldK4BZayA0ooTSEcO2o%3D&se=2016-12-22T02%3A00%3A19Z&sp=r"
destfile <- "./data/kaggle_titanic_test.csv"
if (!file.exists(destfile)) download.file(url = url, destfile = destfile, 
                                          method = "curl", quiet = T
                                          )
test <- read.csv(file = destfile, header = T, stringsAsFactors = F)

#combine data sets
full <- bind_rows(train, test)

# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

cat(paste('We have <b>', nlevels(factor(full$Surname)), '</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time.'))

#Families Sink or Swim Together?
# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# Create a Deck variable. Get passenger deck A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62, 830)] <- 'C'

# Show row 1044
tbl_df(full)
full[1044, "Fare"]

# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin',
                                            'Family','Surname','Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)

# Replace Age variable from the mice model.
full$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(full$Age))

# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'
# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & 
        full$Age > 18 & full$Title != 'Miss'] <- 'Mother'
# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

#create new variables for Ticket numbers
full$Ticket_new <- gsub("\\D", "", full$Ticket)
full$Ticket_new[which(nchar(full$Ticket_new) == 0)] <- 0
full$Ticket_new[which(nchar(full$Ticket_new) == 1)] <- 1
full$Ticket_new[which(nchar(full$Ticket_new) == 2)] <- 2
full$Ticket_new[which(nchar(full$Ticket_new) == 3)] <- 3
full$Ticket_new[which(nchar(full$Ticket_new) == 4)] <- 4
full$Ticket_new[which(nchar(full$Ticket_new) == 5)] <- 5
full$Ticket_new[which(nchar(full$Ticket_new) == 6)] <- 6
full$Ticket_new[which(nchar(full$Ticket_new) == 7)] <- 7
full$Ticket_new[which(nchar(full$Ticket_new) == 8)] <- 8
full$Ticket_new <- as.factor(full$Ticket_new)

#
md.pattern(full)

#########
full$Ticket_new <- paste("L", full$Ticket_new, sep = "")
full$Ticket_new <- as.factor(full$Ticket_new)

names(full)[which(names(full) == "Ticket_new")] <- "TicketNew"
train$Survived <- as.factor(train$Survived)
levels(train$Survived) <- c("died", "lived")

write.csv(full, file = "./data/full_titanic_w_missing_deck_values.csv", row.names = F)

