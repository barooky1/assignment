library(tidyverse)
library(randomForest)
library(scales)
library(formattable)
library(states)
library(anytime)
library(Boruta)
# library(Hmisc)
#Setting up the data
setwd("~/R/assignment")
getwd()
rawdata <- read.csv("2015topresent.csv")
#Copart only, remove peddle####
 rawdata2 <- rawdata %>%
   filter(PickUpState %in% state.abb| PickUpState =="DC",  HighestSeller!="Peddle",   Type=="Automobile") %>%  
  select(-Type)
# rawdata2 <- rawdata %>%
#   filter(  Seller=="Copart Auto Auction")
# 
# summary(rawdata2)

####Setting up the data####

# remove anything with no highestprice. However, try  only remove from test, not train set. 
rawdata2 <- rawdata2 %>% drop_na("HighestPrice")
# alternative method
# rawdata2 <- rawdata2[!is.na(rawdata2$HighestPrice),]
# rawdata2 <- rawdata2 %>% 
#   select(-Status,-PaidStatus)
rawdata2$ExteriorCondition <- NULL
rawdata2$InteriorCondition <- NULL
rawdata2$X <- NULL

# seperate weight
# rawdata2$don.month <- sapply(strsplit(as.character(rawdata2$DonationDate), "-"),"[",2)
rawdata2$Weight <- parse_number(rawdata2$Weight)
# rawdata2$Weight <- sapply(strsplit(as.character(rawdata2$Weight), "~"),"[", 1)
# rawdata2$Weight <- as.numeric(rawdata2$Weight)
#impute missing weights
weights <- rawdata2 %>% 
  drop_na(Weight) %>% 
  group_by(Make, Model) %>% 
  summarise(mean(Weight))
# rawdata2 <- left_join(rawdata2,Train_ratio_set, "YardNumber" )
rawdata2 <- left_join(rawdata2, weights, by= c("Model"="Model", "Make"="Make"))
rawdata2$weight <- ifelse(is.na(rawdata2$Weight), rawdata2$`mean(Weight)`, rawdata2$Weight )

# get rid of NA's in YearOfcar make and model

rawdata2 <- rawdata2 %>% 
  filter(!is.na(rawdata2$Model))
rawdata2 <- rawdata2 %>% 
  filter(!is.na(rawdata2$YearOfCar))
rawdata2 <- rawdata2 %>% 
  filter(rawdata2$Make!="")
rawdata2 <- rawdata2 %>% 
  filter(rawdata2$Model!="")
# should be filtering out sold for over 1 only for train, not test, of course!
# rawdata2 <- rawdata2 %>% 
#   filter(Type=="Automobile", PriceSoldFor>1) %>% 
#   select(-Type)
# clean data in Runs
rawdata2$Runs[rawdata2$Runs=="no"] <- "No"
rawdata2$Runs[rawdata2$Runs=="yes"] <- "Yes"
rawdata2$Runs[rawdata2$Runs==""] <- "Yes"

rawdata2$Runs <- as.character(rawdata2$Runs)
rawdata2$Runs <-  as.factor(rawdata2$Runs)
rawdata2$Make <- as.factor(rawdata2$Make)

rawdata2$Make[rawdata2$Make=="chevy"] <- "Chevrolet"
rawdata2$Make[rawdata2$Make=="chevy "] <- "Chevrolet"
rawdata2$Make[rawdata2$Make=="Chevy"] <- "Chevrolet"
rawdata2$Make[rawdata2$Make=="Chevy "] <- "Chevrolet"
rawdata2$Make[rawdata2$Make=="Chevrolet "] <- "Chevrolet"
# impute nissing mileage
rawdata2$Mileage <- 
ifelse(is.na(rawdata2$Mileage), (2020-rawdata2$YearOfCar)*10000,rawdata2$Mileage)
# omit na's one of following 2 ways
rawdata2 <- rawdata2 %>% 
  select(-Weight)
rawdata2 <- na.omit(rawdata2)
# rawdata3 <- rawdata2[rowSums(is.na(rawdata2))>0,]

# add auction benefit
rawdata2$AuctionBenefit <-rawdata2$Income-rawdata2$HighestPrice
# create what ideally should have been done. These  were, in hindsight,the best decisions we could have made
rawdata2$shooda <- ifelse(rawdata2$AuctionBenefit>0,1,0)
rawdata2$shooda <- as.factor(rawdata2$shooda)
rawdata2$shoodaresult <- ifelse(rawdata2$shooda==1,rawdata2$Income, rawdata2$HighestPrice)



# reconfigured donation date to remove time , and put date into readable date
# rawdata2$DonationDate <- anytime(rawdata2$DonationDate)
rawdata2$DonationDate <- sapply(strsplit(as.character(rawdata2$DonationDate), " "), "[", 1)
# as.Date(df$Date, "%m/%d/%Y %H:%M:%S")
rawdata2$DonationDate <- as.Date(rawdata2$DonationDate,"%m/%d/%Y")

#get don.month,  seasonality commented ####
rawdata2$don.month <- sapply(strsplit(as.character(rawdata2$DonationDate), "-"),"[",2)
rawdata2$don.month <- as.numeric(rawdata2$don.month)
rawdata2$don.year <- sapply(strsplit(as.character(rawdata2$DonationDate), "-"),"[",1)
rawdata2$don.year <- as.numeric(rawdata2$don.year)

#### # get yardmeans, commented####

# rawdata2$don.year <- sapply(strsplit(as.character(rawdata2$DD), "-"),"[",1)

#  cant do trainmean unless remove zeros of price sold for
Train_ratio_set <- rawdata2 %>%
  filter(PriceSoldFor>0, Seller=="Copart Auto Auction", don.year>2018) %>%
  group_by(Make,Model,YearOfCar, YardNumber, don.year) %>%
  summarise(sum_paid = sum(PriceSoldFor),
            count = n(),
            YardNumber_mean = mean(PriceSoldFor)) %>%
  group_by(Make,Model,YearOfCar, don.year) %>%
  mutate(National_mean = sum(sum_paid) / sum(count),
         tr.ratio = YardNumber_mean/National_mean) %>%
  group_by(YardNumber) %>%
  summarise(trainmean= mean(tr.ratio),
            tr.count=n())
rawdata2 <- left_join(rawdata2,Train_ratio_set, "YardNumber" )





# Now choose  YearOfCar , Make####
above2002 <- rawdata2 %>%
  # filter(YearOfCar>1900)
  filter(YearOfCar<2020, YearOfCar>1920, don.year==c("2019","2020"))
above2002 <- above2002 %>% 
  filter(Make %in% c( "Chevrolet"))
# above2002 <- above2002 %>% filter(don.year %in% c("2020"))#####
# above2002 <- above2002 %>%
# filter(Model %in% c( "Altima"))

#### Partition data - train (80%) & test (20%)####
#randomized:
set.seed(111)
randomizer <- sample(2, nrow(above2002), replace = T, prob = c(0.8, 0.2))
train <- above2002[randomizer==1,]
test <- above2002[randomizer==2,]
train <- train %>% 
  filter(Seller=="Copart Auto Auction")
# comparing firt 80% to last:
# train <- above2002[1:(nrow(above2002)*.8),]
# test <- above2002[(nrow(above2002)*.8):nrow(above2002),]
# test$don.month <-  9
# run 2019 off of 2018
# train <- above2002[above2002$don.month<10,]
# test <- above2002[above2002$don.month>=10,]
# train <-above2002 %>%  filter(Seller=="Copart Auto Auction", PriceSoldFor>0)
# test <- above2002 %>% filter(PriceSoldFor==0)
#both ways require removal of excess test levels:
unique_train <- unique(train$Model)
test <-  test[test$Model %in% unique_train,]


#### first lgm model  no yards:####


model2002 <- glm(shooda ~Model  +Runs+Mileage+ YearOfCar+HighestPrice+Charges+don.month+trainmean+weight+don.year , data = train, family = 'binomial')


#### train  Prediction glm####
tr.prediction <- predict(model2002, train, type = 'response')
train$tr.classifier <-  if_else(tr.prediction>.501,1,0)
train$tr.predictionresult <- ifelse(train$tr.classifier==1,train$Income, train$HighestPrice)
table(Predicted =train$tr.classifier, Actual = train$shooda)
table(train$shooda)
### train   RF ####
rfmodel <- randomForest(shooda ~ Model+Runs + Mileage+ YearOfCar+HighestPrice+Charges+don.month+trainmean+weight , data = train, proximity=TRUE)
train$tr.rf.prediction<- predict(rfmodel, train, type = "response")
train$rfresults <- ifelse(train$tr.rf.prediction=="1", train$Income, train$HighestPrice)
table(rfprediction=train$tr.rf.prediction,
      actual=train$shooda)
table(train$shooda)


train$both <- ifelse(train$tr.classifier&train$tr.rf.prediction=="0",train
                     $HighestPrice, train$Income
)
train$both.classifier <- ifelse(train$tr.classifier&train$tr.rf.prediction=="0","0","1")
table(bothprediction=train$both.classifier,
      actual=train$shooda)                     

# train lm
# linear <- lm(PriceSoldFor~Model  +Runs+Mileage+ YearOfCar+HighestPrice+Charges+don.month+trainmean+weight , data = train )
# train$lmpredict <- predict(linear, train )
# train$lresult <- ifelse(train$lmpredict-train$HighestPrice>0,train$Income, train$HighestPrice)
# train$lshooda <- ifelse(train$lmpredict>=train$Income,1, 0 )
# table(lm.prediction=train$lshooda,
      # actual=train$shooda)
TrainFrame <- data.frame( Make=above2002[3,"Make"],
                          shoodaresults=comma(sum(train$shoodaresult)),
                          Actual_Income= sum(train$Income),
                          lm.predictionresults=sum(train$tr.predictionresult),
                          lm.improvement= sum(train$tr.predictionresult)/sum(train$Income),
                          rf.results=sum(train$rfresults),
                          rf.improvement=sum(train$rfresults)/sum(train$Income),
                          
                          potential_improvement=percent(sum(train$shoodaresult)/sum(train$Income)-1),
                          both=accounting(sum(train$both)) ,
                          both_improvement= sum(train$both)/sum(train$Income),
                          both_cash_increase=accounting(sum(train$both)-sum(train$Income))) 
TrainFrame

# Allmakes <- TrainFrame

#Test Prediction####
table(test$shooda)

te.prediction <- predict(model2002, test, type = 'response')
test$te.classifier <-  if_else(te.prediction>.5,1,0)
test$te.classifier <-  as.factor(test$te.classifier)
test$te.predictionresult <- ifelse(test$te.classifier==1,test$Income, test$HighestPrice) 

test$tr.rf.prediction<- predict(rfmodel, test, type = "response")
test$rfresults <- ifelse(test$tr.rf.prediction=="1", test$Income, test$HighestPrice)
table(rfprediction=test$tr.rf.prediction,
      actual=test$shooda)

test$both <- ifelse( test$te.classifier=="0"& test$tr.rf.prediction=="0", test$HighestPrice,  test$Income
)
test$both.classifier <- ifelse( test$te.classifier=="0"& test$tr.rf.prediction=="0","0","1")
table(bothprediction= test$both.classifier,
      actual= test$shooda)  


test[1,"Make"]
testFrame <- data.frame(Make= test[1,"Make"],
                        shoodaresults=accounting(sum(test$shoodaresult)),
                        Actual_Income= accounting(sum(test$Income)),
                        Potential_accuracy=nrow(test),
                        Actual_accuracy=nrow(test[test$shooda=="1",]),
                        lm.predictionresults=sum(test$te.predictionresult),
                        lm_accuracyFIXME= nrow(test[test$te.classifier=="1",]),
                        lm.improvement= sum(test$te.predictionresult)/sum(test$Income),
                        rf.results=sum(test$rfresults),
                        rf.improvement=sum(test$rfresults)/sum(test$Income),
                        rf_accuracy=sum(test$tr.rf.prediction=="1"&test$shooda=="1")+sum((test$tr.rf.prediction=="0"&test$shooda=="0")),
                        both_accuracy=sum(test$both.classifier=="1"&test$shooda=="1")+sum(test$both.classifier=="0"&test$shooda=="0"),
                        potential_improvement=percent(sum(test$shoodaresult)/sum(test$Income)-1),
                        both   =accounting(sum(test$both)),
                        
                        bothimprovement=percent( sum(test$both)/sum(test$Income)-1),
                        both_cash_increase=accounting(sum(test$both)-sum(test$Income))
) 
testFrame
