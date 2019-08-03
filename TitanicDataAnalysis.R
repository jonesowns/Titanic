

#Load Raw Data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])



#combine data sets
data.combined <- rbind(train, test.survived)

# A little bit about data types

str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

# Take a look at gross survival rates
table(data.combined$Survived)

#Distribution across classes
table(data.combined$Pclass)

# Load up ggplot2 package to use for visializations
library(ggplot2)

# Hypothesis - Rich folks survival rate is higher

  train$Pclass <- as.factor(train$Pclass) 
  ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
    geom_bar(width = 0.5) +
    xlab("Pclass") +
    ylab("Total Count") +
   labs(fill = "Survived")

  # Hypothesis - Female survival rate is higher
  
  train$Pclass <- as.factor(train$Pclass) 
  ggplot(train, aes(x = Sex, fill = factor(Survived))) +
    geom_bar(width = 0.5) +
    xlab("Sex") +
    ylab("Total Count") +
    labs(fill = "Survived")
  
# Example tge first few names in the training data set
  head(as.character(train$Name))
  
  
  
#how many unique names across train and test
  
  length(unique(as.character(data.combined$Name)))
  
#two duplicates, take a closer look
#first, get duplicate names and store as vector
  dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
  
  
#view dups
  data.combined[which(data.combined$Name %in% dup.names),]

#the above command is gettig those rows from data.combimed that have data.combined$Name in the dup.names
  
  
#Whats up with the Mr., Mrs., Miss etc
  
library(stringr)
#get an array of rows that contain "Miss."
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]

#check some stats on the above
table(misses$Survived)
misses[1:5,]


#what about 'Mrs'
mrs <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
table(mrs$Survived)
mrs[1:5,]
table(mrs$Sex)

#what about 'Mr'
mr <- data.combined[which(str_detect(data.combined$Name, "Mr. ")),]

table(mr$Sex)
#the above has some females too, is it a fuzzy search? I think so, the str_detec seems to be a fuzzy... maybe using the == operator instead

mr[1:5,]
mr[which(str_detect(mr$Sex, "female")),]



#check for males

males <- data.combined[which(data.combined$Sex =="male"),]
males[1:5,]
table(males$Sex)


#even though the males array does not have the female factor, when the table function is evoked, 
#it knows about the female fator, interresting... so it is still referencing the parent table



#Next
# Expand the relationship between Pclass and Survived by adding the a "Title" variable.
# This can possibly help us create a three dimensional array 

#create a utility function to extract title
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss. ", name)) > 0) {
    return("Miss.")
  }else if (length(grep("Mr. ", name)) > 0) {
    return("Mr.")
  }
  else if (length(grep("Master. ", name)) > 0) {
    return("Master.")
  }
  else if (length(grep("Mrs. ", name)) > 0) {
    return("Mrs.")
  }else return("Other")
}


Titles <- NULL

for(i in 1:nrow(data.combined)) {
  Titles <- c(Titles, extractTitle(data.combined[i, "Name"]))
}


str(Titles)
Titles <- as.factor(Titles)
table(Titles)


others <- data.combined[which(data.combined$Title =="Other"),]
others[,]
table(others$Title)


Master <- data.combined[which(str_detect(data.combined$Name ,"Master")),]

data.combined$Title = as.factor(Titles)
   

# Plot survival rate based on Pclass and Title
# only for the train records, since the test records do not have a survived value yet

str(data.combined)

#$ checking relationship between title and survival rate, grouped by  Pclass 
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived )) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")



#ratio of males to females 

table(data.combined$Sex)


#Visialize the 3 way relationship between Sex, Pclass and survival and compare with the Title relationship 
#from ealier



ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar(width = 0.5) + 
  facet_wrap(~Pclass) +
  ggtitle("Sirvival per sex per Pclass") +
  xlab("Sex") +
  ylab("Total Count")  +
  labs(fill = "Survived")


#females who died
mr <- data.combined[which(str_detect(data.combined$Name, "Mr. ")),]
dead_female <- data.combined[which(data.combined$Sex == "female" && data.combined$Survived == '0'),]
dead_female[,]