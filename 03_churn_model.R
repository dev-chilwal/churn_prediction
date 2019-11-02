#load all the libraries
library(data.table)
library(dplyr)
library(lubridate)
library(randomForest)
library(DMwR)
library(caret)
library(e1071)
library(ggplot2)
library(RColorBrewer)


#load rg_base data
rg_base <- fread('rg_base_new.csv')

#load the constituents data
constituents <- fread ("dimConstituent_UTS.csv")

columns_to_keep_con <- c ("MasterConstituentId", "ConstituentSubRegion", "ConstituentRegion",
                          "Gender" , "LGA", "AgeBand")
#keep only desired columns
constituents_fil <- constituents %>% dplyr::select(columns_to_keep_con) %>% distinct()

#join rg_base data to constituent data
db_model <- left_join(rg_base, constituents_fil, by = c("donor" = "MasterConstituentId"))


# set status from 2017 data
payment_2017 <- payment %>% filter(year(ymd(DonationDateSKey)) == 2017)
#set churn status
donor_2017 <- payment_2017 %>%
  distinct(PrimaryConstituentID) %>%
  dplyr::select(PrimaryConstituentID)
donor_2017$churn_status <- 1

#attach churn status to rg data
payment_status <- db_model %>% left_join(donor_2017, by = c("donor"="PrimaryConstituentID"))
#assign 0 to everyone else
payment_status[is.na(payment_status$churn_status),]$churn_status <- 0

#Simple rF model
set.seed(12)

#load income data
avg_inc <- fread("avg_inc_dis.csv")

#attach income data to rg data
db_model_final <- left_join(payment_status, avg_inc, by = c("donor" = "MasterConstituentId"))

#remove unnecessary columns
db_model_final <- db_model_final %>% dplyr::select(- c( "status"))

#convert to factors
db_model_final <- db_model_final %>% mutate_if(is.character, as.factor)
db_model_final$churn_status <-as.factor(as.character( db_model_final$churn_status))

#drop NAs
db_model_final[is.na(db_model_final)] <- 0

#calculate train indices
train_indices <- sample(seq_len(nrow(db_model_final)), size = floor(0.7*nrow(db_model_final)))

train <- db_model_final[train_indices,]
test <- db_model_final[-train_indices,]

#smote train data (execute only if required)
#train_smote <- SMOTE(churn_status ~ ., data = as.data.frame(train), 
#                     perc.over = 100, perc.under = 100, k =10)

#create RF model
rf_model <- randomForest(churn_status ~ . -donor -LGA -last_3 -dist_cards -Gender,
                         data = train, 
                         importance = TRUE, ntree = 100, keep.forest = T)

#check the importance of features
importance(rf_model)

#plot variable importance plot
varImpPlot(rand, type = 2)

#rand #OOB error estimate = 0.12%

#get predicted values
pred <- rand$predicted

#create confusion matrix
rf_cm <- table(predicted = pred,true = train$churn_status)[2:1,2:1]

confusionMatrix(rf_cm)

#PDPs
library(vip)

vip(rand, bar = FALSE, horizontal = FALSE, size = 1.5) 

partialPlot(rand, pred.data = train, x.var = "avg_weekly_income") 


# PDP wrt AgeBand
test <- pdp::partial(rand, pred.data = train, pred.var = "AgeBand") 
test$order <- c(2,3,4,5,6,1,7)

ggplot(test %>% filter (! AgeBand == "Unknown"), aes(x = reorder(AgeBand, order), y= yhat, 
                                                     fill = AgeBand)) + 
  geom_bar (stat="identity",show.legend = F) +
  xlab ("Age") + ggtitle("Partial dependency on Age") +
  theme(plot.title = element_text(hjust = 0.25), legend.position = "none") +
  theme(plot.caption = element_text(hjust = 0, face = "italic")) +
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0)) +
  theme_minimal() + 
  scale_fill_brewer(palette = "Accent") 



