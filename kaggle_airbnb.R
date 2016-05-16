#install.packages('Amelia')
#install.packages('zoo')
#install.packages('xgboost')
#install.packages(('caret'))
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-tibshirani/8/R")))
#library(data.table)
#library(Amelia)
library(h2o)
#library(zoo)
library(stringr)
#library(xgboost)
#library(caret)
#library(car)
#library(plyr)
library(reshape2)
#library(rpart)

setwd("/Users/yumingfang/Documents/airbnb competation/")
list.files()

# load data
h2o.init(nthreads=-1, max_mem_size="4G")
h2o.removeAll()
train <- read.csv("train_users_2.csv", na.strings = c(""))
labels = train['country_destination']
test <- read.csv("test_users.csv", na.strings = c(""))
session <- read.csv("sessions.csv", na.strings = c(""))
summary(session)

#train <- h2o.importFile(normalizePath("train_users_2.csv"), col.types = list(by.col.name = c("date_account_created", "timestamp_first_active"),types=c("string")))
#test <- h2o.importFile(normalizePath("test_users.csv"), col.types = list(by.col.name = c("date_account_created", "timestamp_first_active"),types=c("string")))

response <- train$country_destination
train <- train[, -which(names(train) %in% c("date_first_booking", "country_destination"))]
n <- nrow(train)
test <- test[, -which(names(test) %in% c("date_first_booking"))]
alldata <- rbind(train, test)
m <- nrow(alldata)
str(alldata)

#get year and month of date_account_created
dac <- str_split_fixed(alldata$date_account_created, "-", 3)
alldata$dac_yr <- dac[, 1]
alldata$dac_mom <- dac[, 2]
alldata$dac_day <- dac[, 3]
alldata <- alldata[, -2]

alldata$dac_yr <- as.factor(alldata$dac_yr)
alldata$dac_mom <- as.factor(alldata$dac_mom)
alldata$dac_day <- as.factor(alldata$dac_day)

#get year and month of timestamp_first_active
tfa <- as.character(alldata$timestamp_first_active)
alldata$tfa_yr <- sapply(tfa, USE.NAMES = F,function(x){substr(x, 1, 4)})
alldata$tfa_mon <- sapply(tfa, USE.NAMES = F,function(x){substr(x, 5, 6)})
alldata$tfa_day <- sapply(tfa, USE.NAMES = F,function(x){substr(x, 7, 8)})
alldata <- alldata[, -2]

alldata$tfa_yr <- as.factor(alldata$tfa_yr)
alldata$tfa_mon <- as.factor(alldata$tfa_mon)
alldata$tfa_day <- as.factor(alldata$tfa_day)

#fill NA of age
sapply(alldata, function(x) sum(is.na(x)))
table(alldata$age)
alldata$age[is.na(alldata$age) | alldata$age <= 5 | alldata$age >= 100] <- 0
alldata$age[is.na(alldata$age)] <- -1
bin = seq(0,2014,5)
bins = c(-1.0,0,20,25,30,40,50,60,75,100,150,1000000000)
aaa = .bincode(alldata$age,bins, include.lowest = T)
alldata$age_bin = aaa

#fill NA for first_affiliate_tracted
#alldata$first_affiliate_tracked <- factor(alldata$first_affiliate_tracked, levels = c(levels(alldata$first_affiliate_tracked), "n/a"))
#tt <- table(alldata$first_affiliate_tracked)
alldata$first_affiliate_tracked[is.na(alldata$first_affiliate_tracked)] <- names(tt[which.max(tt)])
table(alldata$first_affiliate_tracked)

# split train and test
train <- alldata[1:n, ]
test <- alldata[(n+1):m, ]
train$country <- response
#remove na of session
session <- session[!is.na(session$user_id), ]
session <- session[!is.na(session$action), ]
sapply(session, function(x) sum(is.na(x)))

a <- aggregate(action ~ user_id, data = session, function(x){names(which.max(table(x)))})
act_ty <- aggregate(action_type ~ user_id, data = session, function(x){names(which.max(table(x)))})
act_dt <- aggregate(action_detail ~ user_id, data = session, function(x){names(which.max(table(x)))})
time <- aggregate(secs_elapsed ~ user_id, data = session, function(x){sum(x, na.rm = T)})
session_comb <- a[a$user_id %in% act_dt$user_id, ]
session_comb$action_type <- as.factor(act_ty$action_type)
session_comb$action_detail <- as.factor(act_dt$action_detail)
session_comb <- merge(session_comb,time, by = "user_id", all.x = T)
#session_comb$lang_multi <- lang_mul$action
#session_comb$spoken_lang <- lang_spo$action
session_comb$secs_elapsed[is.na(session_comb$secs_elapsed)] <- 0

#join session to train and test
alldata_comb <-  merge(alldata, session_comb, by.x = "id", by.y = "user_id")
summary(alldata_comb)
sapply(alldata_comb, function(x) sum(is.na(x)))
drops <- c("dac_yr", "tfa_yr","age")
alldata_comb <- alldata_comb[, !(names(alldata_comb) %in% drops)]
#index <- row.names(alldata_comb[is.na(alldata_comb$age), ])
#alldata_comb$age[is.na(alldata_comb$age)] <- age$reconstr_age[row.names(age) %in% index]
#alldata_comb$age <- as.integer(alldata_comb$age)
#row.names(alldata_comb) <- alldata_comb$id
train_comb <- merge(train, session_comb, by.x = "id", by.y = "user_id")
#drops <- c("dac_yr", "tfa_yr")
train_comb <- train_comb[, !(names(train_comb) %in% drops)]
#row.names(train_comb) <- train_comb$id
#train_comb$age[is.na(train_comb$age)] <- alldata_comb$age[row.names(alldata_comb) %in% row.names(train_comb[is.na(train_comb$age), ])]

#train_1 <- train[-which(train$id %in% train_comb$id), ]
#train_1 <- train_1[, !(names(train_1) %in% drops)]
test_comb <- merge(test, session_comb, by.x = "id", by.y = "user_id")
test_1 <- test[-which(test$id %in% test_comb$id), ]
#row.names(test_comb) <- test_comb$id
#test_comb$age[is.na(test_comb$age)] <- alldata_comb$age[row.names(alldata_comb) %in% row.names(test_comb[is.na(test_comb$age), ])]
test_comb <- test_comb[, !(names(test_comb) %in% drops)]
#merge session
#sapply(test_comb, function(x) sum(is.na(x)))
#train <- alldata[1:n, ]

#test <- alldata[(n+1):m, ]

#write.csv(alldata_comb, file = "alldata_comb.csv", row.names = F)
write.csv(train_comb, file = "train_comb.csv", row.names = F)
write.csv(train_comb, file = "train_comb_age.csv", row.names = F)
#write.csv(train_1, file = "train_1.csv", row.names = F)
write.csv(test_comb, file = "test_comb.csv", row.names = F)
write.csv(test_comb, file = "test_comb_age.csv", row.names = F)
write.csv(test_1, file = "test_1_age.csv", row.names = F)
write.csv(train, file = "train_df.csv", row.names = F)
write.csv(train, file = "train_df_age.csv", row.names = F)

#alldataHex <- h2o.importFile(path = normalizePath("alldata_comb.csv"))
train_combHex <- h2o.importFile(path = normalizePath("train_comb_age.csv"))
train_combHex.drop('age')
test_combHex <- h2o.importFile(path = normalizePath("test_comb_age.csv"))
trainHex <- h2o.importFile(path = normalizePath("train_df_age.csv"))
test_1Hex <- h2o.importFile(path = normalizePath("test_1_age.csv"))

splits <- h2o.splitFrame(train_combHex, c(0.6,0.2), seed = 111)
train_comb <- h2o.assign(splits[[1]], "trainHex")
valid_comb <- h2o.assign(splits[[2]], "validHex")
#test_comb <- h2o.assign(splits[[3]], "testHex")
features <- names(train_combHex)[-which(names(train_combHex) %in% c("id", "country","age"))]

splits_1 <- h2o.splitFrame(trainHex, c(0.6,0.2), seed = 110)
train_1 <- h2o.assign(splits_1[[1]], "train1Hex")
valid_1 <- h2o.assign(splits_1[[2]], "valid1Hex")
#test_1 <- h2o.assign(splits_1[[3]], "test1Hex")
features_1 <- names(trainHex)[-which(names(trainHex) %in% c("id", "country","age"))]


rfHex <- h2o.randomForest(training_frame = train_comb,
                           validation_frame = valid_comb,
                          x=features, y="country", 
                          ntrees = 100, max_depth = 10, 
                          score_each_iteration = T)

gbm <- h2o.gbm(training_frame = train_combHex,
               validation_frame = valid_comb,
                x=features, y="country",
                ntrees = 100, 
                max_depth = 10, 
                learn_rate = 0.01,
                score_each_iteration = T)

gbm <- h2o.gbm(training_frame = train_combHex,
               x=features, y="country",
               ntrees = 100, 
               max_depth = 10, 
               learn_rate = 0.01,
               score_each_iteration = T)
summary(rfHex)
summary(gbm)
h2o.hit_ratio_table(rfHex, valid = T)[1,2]
h2o.hit_ratio_table(gbm, valid = T)[1,2]

rfHex_1 <- h2o.randomForest(training_frame = train_1, 
                            validation_frame = valid_1, 
                            x=features_1, y="country", 
                            ntrees = 100,
                            max_depth = 10)

gbm_1 <- h2o.gbm(training_frame = train_1,
               validation_frame = valid_1,
               x=features_1, y="country",
               ntrees = 50, 
               max_depth = 10, 
               learn_rate = 0.1,
               score_each_iteration = T)

gbm_1 <- h2o.gbm(training_frame = trainHex,
                 x=features_1, y="country",
                 ntrees = 50, 
                 max_depth = 10, 
                 learn_rate = 0.1,
                 score_each_iteration = T)

h2o.hit_ratio_table(rfHex_1, valid = T)[1,2]
h2o.hit_ratio_table(gbm_1, valid = T)[1,2]

#prediction and submission
predictions_comb <- as.data.frame(h2o.predict(gbm, test_combHex))
predictions_1 <- as.data.frame(h2o.predict(gbm_1, test_1Hex))

q <- apply(predictions_comb[, -1], 1, function(x){names(sort(x, decreasing = T))[1:5]})
w <- t(q)
e <- apply(predictions_1[, -1], 1, function(x){names(sort(x, decreasing = T))[1:5]})
r <- t(e)
prediction_df1 <- data.frame(id = test_comb$id, country = w)
prediction_df2 <- data.frame(id = test_1$id, country = r)
submission <- rbind(prediction_df1, prediction_df2)
xo <- melt(submission, id.vars = c("id"))
xx <- xo[order(xo$id), ]
final <- xx[, -2]
colnames(final)[2] <- "country"
rownames(final) <- NULL
#submission <- data.frame(id= test$id, country = prediction$predict)
write.csv(final, "submission_airbnb_after.csv", row.names = F)

