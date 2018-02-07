# clean a data file and return a data frame
clean_file <- function(data_file_name) {
  dat_raw <- readLines(data_file_name)
  
  # apply headers
  df <- t(sapply(dat_raw, FUN = function(x) trimws(substring(x, dat_header[,2], dat_header[,3]))))
  dimnames(df) <- NULL
  df <- as.data.frame(df)
  colnames(df) <- dat_header[,1]
  
  # save original length of df
  original_len <- length(df$PseudoID)
  
  # make numeric fields numeric
  df$Pay <- as.numeric(as.character(df$Pay))
  
  # replace unknowns with NA
  df$Station <- replace(df$Station, df$Station == "#########", NA)
  df$Age <- replace(df$Age, df$Age == "UNSP", NA)
  df$Education <- replace(df$Education, df$Education == "" | df$Education == "*", NA)
  df$PayPlan <- replace(df$PayPlan, df$PayPlan == "" | df$PayPlan == "*", NA)
  df$Category <- replace(df$Category, df$Category == "" | df$Category == "*", NA)
  df$SupervisoryStatus <- replace(df$SupervisoryStatus, df$SupervisoryStatus == "" | df$SupervisoryStatus == "*", NA)
  df$Schedule <- replace(df$Schedule, df$Schedule == "" | df$Schedule == "*", NA)
  
  # make ordinal fields ordered factors
  df$Age <- factor(df$Age, ordered = TRUE, levels = levels(df$Age))
  df$Education <- factor(df$Education, ordered = TRUE, levels = levels(df$Education))
  
  # if Age is unspecified, use median age for agency
  df$Age <- with(df, ave(df$Age, df$Agency, FUN = function(x) replace(x, is.na(x), levels(df$Age)[median(as.integer(x), na.rm = TRUE)])))
  
  # fill NA pays with median pay for the Age of the employee at that agency
  df$Pay <- with(df, ave(df$Pay, df$Age, df$Agency, FUN = function(x) replace(x, is.na(x), median(x, na.rm = TRUE))))
  
  # drop any rows with NA pay after imputation
  na_pay <- is.na(df$Pay)
  df <- df[!na_pay,]
  
  # handle duplicate IDs
  # not touching employees who worked at multiple agencies in a quarter
  # remove the lower salary if an employee worked at the same agency twice in a quarter
  # 1. select rows with duplicate IDs
  df_dup_ids <- df[duplicated(df$PseudoID) | duplicated(df$PseudoID, fromLast = TRUE),]
  # 2. order selection by ID, then Agency, then descending Pay
  df_dup_ids <- df_dup_ids[order(df_dup_ids$PseudoID, df_dup_ids$Agency, -df_dup_ids$Pay),]
  # 3. select rows where the ID and Agency are duplicated (same employee at same agency)
  to_remove <- df_dup_ids[(duplicated(df_dup_ids[c("PseudoID", "Agency")]) | duplicated(df_dup_ids[c("PseudoID", "Agency")], fromLast = TRUE)),]
  # 4. get row numbers for rows with the lowest pay for each grouping in the above selection
  to_remove <- as.numeric(rownames(to_remove[duplicated(to_remove$PseudoID, to_remove$Agency),]))
  # 5. reselect from df where rows are not in to_remove
  df <- df[!(as.numeric(rownames(df)) %in% to_remove),]
  
  # add agency name
  m <- match(df$Agency, agency_trans_table$agency_ID)
  df$AgencyName <-  agency_trans_table$agency_name[m]
  
  # calculate percent of data saved
  final_len <- length(df$PseudoID)
  print(paste("[", data_file_name, "]", final_len, "of", original_len,"records maintained:",
              format(round(final_len/original_len*100, 2)), "%"))
  
  return(df)
}

data_file1 <- "../data/Status_Non_Dod_2001_03.txt"
data_file2<-"../data/Status_Non_Dod_2005_03.txt"
data_file3<-"../data/Status_Non_Dod_2009_03.txt"
data_file4<-"../data/Status_Non_Dod_2013_03.txt"



header_file <- "../data/headers.csv"
agency_file <- "../data/SCTFILE.TXT"

dat_header <- read.csv(header_file, header = TRUE)
agency_trans <- readLines(agency_file)
agency_ID <- sapply(agency_trans, FUN = function(x) substring(x, 3,6))
agency_name <- trimws(sapply(agency_trans, FUN = function(x) substring(x, 36,75)))
agency_trans_table <- data.frame(agency_ID = agency_ID, agency_name = agency_name)

df1 <- clean_file(data_file1)
df2<- clean_file(data_file2)
df3<-clean_file(data_file3)
df4<-clean_file(data_file4)

save(df1, file = "Employment_2001_03.rda")
save(df2, file = "Employment_2005_03.rda")
save(df3, file = "Employment_2009_03.rda")
save(df4, file = "Employment_2013_03.rda")


state_trans <- readLines('../data/state-trans.txt')
state_ID <- sapply(state_trans, FUN = function(x) substring(x, 1,2))
state_name <- trimws(sapply(state_trans, FUN = function(x) substring(x, 35,90)))
state_trans_table <- data.frame(state_ID = state_ID, state_name = state_name)



# prepare data for classification
make_cleaner <- function(df) {

  # remove columns
  to_save <- c("Agency","Station","Age","Education","LOS","Pay",
               "SupervisoryStatus","AgencyName")
  df <-df[,to_save]
  
  df$LOS <- factor(df$LOS, ordered = TRUE, levels = levels(df$LOS))
  df$Age <- factor(df$Age, ordered = TRUE, levels = levels(df$Age))
  df$Education <- factor(df$Education, ordered = TRUE,
                         levels = levels(ordered(unique(df$Education))))
  #df$Education <- as.numeric(as.character(df$Education))
 
  #df$Age <- as.numeric(substr(df$Age, start = 1, stop = 2))
  #df$LOS <- as.numeric(sub("-.*|\\+|<", "", df$LOS))
  
 # df$SupervisoryStatus <- as.numeric(as.character(df$SupervisoryStatus))
  
  
  # impute Station, Education, LOS, SupervisoryStatus
  df$Station <- sapply(df$Station, FUN = function(x)
    as.integer(substring(x, 1,2)))
  
  #add feature state
  m <- match(df$Station, state_trans_table$state_ID)
  df$States <-  state_trans_table$state_name[m]

  # fill NA Education with median Education for employees at same Agency with
  # same Age
  df$Education <- with(
    df, ave(df$Education, df$Agency, df$Age, FUN = function(x)
      replace(x, is.na(x),
              levels(df$Education)[median(as.integer(x),na.rm = TRUE)]))
  )
  # fill remaining NA Education with median education for Agency
  df$Education <- with(
    df, ave(df$Education, df$Agency, FUN = function(x)
      replace(x, is.na(x),
              levels(df$Education)[median(as.integer(x),na.rm = TRUE)]))
  )
  # fill NA LOS with median LOS for employees of the same Age
  df$LOS <- with(
    df, ave(df$LOS, df$Age, FUN = function(x)
      replace(x, is.na(x),
              levels(df$LOS)[median(as.integer(x),na.rm = TRUE)]))
  )
  # fill NA SupervisoryStatus with median status for employees with the
  # same pay
  
  #new feature state
  df$Station <- sapply(df$Station, FUN = function(x)
    as.integer(substring(x, 1,2)))
  m <- match(df$Station, state_trans_table$state_ID)
  df$States <-  state_trans_table$state_name[m]

 # df<- subset(df, !is.na(df$State))

  # make pay ordinal (cut)
  df$Pay <- cut(df$Pay,
                  breaks = c(0, 50000, 75000, 100000 ,Inf),
                  labels = c("<50k", "50-75k", "75k-100k", ">100k"))
  df$Pay <- factor(df$Pay, ordered = TRUE, levels = levels(df$Pay))
  
  # drop NA Pay
  df <- df[!is.na(df$Pay),]
  
  # make age an integer (take middle of range)
  df$Age <- sapply(df$Age, FUN = function(x)
    if(x == "75+") 75
    else (as.integer(substring(x, 1,2)) + as.integer(substring(x, 4,5)))/2)
  
  # make Education an integer to improve training speed
  df$Education <- as.numeric(as.character(df$Education))
  
  # make LOS a number (middle of range) to improve training speed
  df$LOS <- sapply(df$LOS, FUN = function(x)
    if(x == "< 1") 1
    else if(x == "35+") 35
    else floor((as.integer(strsplit(as.character(x), '-')[[1]][1]) +
                  as.integer(strsplit(as.character(x), '-')[[1]][2]))/2))
  
  return(df)
} # end make_cleaner


df1<-make_cleaner(df1)
df2<-make_cleaner(df2)
df3<-make_cleaner(df3)
df4<-make_cleaner(df4)

save(df1, file = "cleaner_2001_03.rda")
save(df2, file = "cleaner_2005_03.rda")
save(df3, file = "cleaner_2009_03.rda")
save(df4, file = "cleaner_2013_03.rda")

# follow project 2 instruction
#select employee number more than 10000
#df1
agencies <- names(which(table(df1$AgencyName)>10000))

df1 <- df1[df1$AgencyName %in% agencies,]
df1$AgencyName <- factor(df1$AgencyName)
df1$Agency <- factor(df1$Agency)

#df2
agencies <- names(which(table(df2$AgencyName)>10000))

df2 <- df2[df2$AgencyName %in% agencies,]
df2$AgencyName <- factor(df2$AgencyName)
df2$Agency <- factor(df2$Agency)

#df3
agencies <- names(which(table(df3$AgencyName)>10000))

df3 <- df3[df3$AgencyName %in% agencies,]
df3$AgencyName <- factor(df3$AgencyName)
df3$Agency <- factor(df3$Agency)

#df4
agencies <- names(which(table(df4$AgencyName)>10000))

df4 <- df4[df4$AgencyName %in% agencies,]
df4$AgencyName <- factor(df4$AgencyName)
df4$Agency <- factor(df4$Agency)

mode(df1$SupervisoryStatus)




save(df1, file = "10000employee_2001_03.rda")
save(df2, file = "10000employee_2005_03.rda")
save(df3, file = "10000employee_2009_03.rda")
save(df4, file = "10000employee_2013_03.rda")


# FSelector
install.packages("FSelector")
library(FSelector)
weights <- chi.squared(Pay~.,data = df2)
weights
str(weights)
dotchart(weights$attr_importance[o], labels = rownames(weights)[o],
         xlab = "Importance")

weights <- chi.squared(Pay~.,data = df4)
weights
str(weights)
dotchart(weights$attr_importance[o], labels = rownames(weights)[o],
         xlab = "Importance")




#sampling
sample_ID_2001_03<- sample(1:nrow(df1), size = 10000)
dat1 <- df1[sample_ID_2001_03, ]

sample_ID_2005_03<- sample(1:nrow(df2), size = 10000)
dat2 <- df2[sample_ID_2005_03, ]

sample_ID_2009_03<- sample(1:nrow(df3), size = 10000)
dat3 <- df3[sample_ID_2009_03, ]

sample_ID_2013_03<- sample(1:nrow(df4), size = 10000)
dat4 <- df4[sample_ID_2013_03, ]

save(dat1, file = "10000point_2001_03.rda")
save(dat2, file = "10000point_2005_03.rda")
save(dat3, file = "10000point_2009_03.rda")
save(dat4, file = "10000point_2013_03.rda")




install.packages("caret")
install.packages("e1071")
library(caret)
library(rpart)
library(rpart.plot)
# decision tree model(caret)
#2001
summary(dat1)

dat1$SupervisoryStatus <- as.numeric(as.character(dat1$SupervisoryStatus))
fit <- train(Pay ~ SupervisoryStatus + Education + Agency, data = dat1 , method = "rpart",
             na.action = na.pass,
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=10)

fit
varImp(fit, compete = FALSE)

rpart.plot(fit$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0,cex = 0.7)
varImp(fit)
testing <- dat1[-sample_ID_2001_03, ]
testing <- testing[sample(1:nrow(testing), size = 1000), ]
pred <- predict(fit, newdata = testing, na.action = na.pass)
confusionMatrix(data = pred, testing$Pay)
table = confusionMatrix(data = pred, testing$Pay)[[4]]
View(t(table))


#2005
#Education~agency~LOS
fit <- train(Pay ~ LOS + Education + Agency, data = dat2 , method = "rpart",
             na.action = na.pass,
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=10)

fit
varImp(fit)

rpart.plot(fit$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0,cex = 0.7)
varImp(fit)
testing <- dat2[-sample_ID_2005_03, ]
testing <- testing[sample(1:nrow(testing), size = 1000), ]
pred <- predict(fit, newdata = testing, na.action = na.pass)
confusionMatrix(data = pred, testing$Pay)
table = confusionMatrix(data = pred, testing$Pay)[[4]]
View(t(table))

#2005education~los~state
fit <- train(Pay ~ LOS + Education + States, data = dat2 , method = "rpart",
             na.action = na.pass,
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=10)
fit
rpart.plot(fit$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0,cex=0.8)
varImp(fit)
testing <- dat2[-sample_ID_2005_03, ]
testing <- testing[sample(1:nrow(testing), size = 1000), ]
pred <- predict(fit, newdata = testing, na.action = na.pass)
confusionMatrix(data = pred, testing$Pay)
table = confusionMatrix(data = pred, testing$Pay)[[4]]
View(t(table))

#2013education~los~state
fit <- train(Pay ~ LOS + Education + States, data = dat4 , method = "rpart",
             na.action = na.pass,
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=10)
fit
rpart.plot(fit$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0,cex=0.8)
varImp(fit)
testing <- dat2[-sample_ID_2005_03, ]
testing <- testing[sample(1:nrow(testing), size = 1000), ]
pred <- predict(fit, newdata = testing, na.action = na.pass)
confusionMatrix(data = pred, testing$Pay)
table = confusionMatrix(data = pred, testing$Pay)[[4]]
View(t(table))



#2009 education~los~age
fit <- train(Pay ~ LOS + Education + Age, data = dat3 , method = "rpart",
             na.action = na.pass,
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=10)
fit
rpart.plot(fit$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0)
varImp(fit)
testing <- dat2[-sample_ID_2009_03, ]
testing <- testing[sample(1:nrow(testing), size = 1000), ]
pred <- predict(fit, newdata = testing, na.action = na.pass)
confusionMatrix(data = pred, testing$Pay)
table = confusionMatrix(data = pred, testing$Pay)[[4]]
View(t(table))


#2013 model comparision

subset <- subset(dat4, select=c("Education","LOS","Pay"))
subset <- na.omit(subset)
summary(subset)

train<- createFolds(subset$Pay, k=10)



#C4.5
install.packages("RWeka")
library(RWeka)
C45Fit <- train(Pay ~ ., method = "J48", data = subset,
                tuneLength = 5,
                na.action = na.pass,
                trControl = trainControl(
                  method = "cv", indexOut = train))
C45Fit
C45Fit$finalModel

#PART(rule-based)

rulesFit <- train(Pay ~ ., method = "PART", data = subset,
                  tuneLength = 5,
                  trControl = trainControl(
                    method = "cv", indexOut = train))
rulesFit
varImp(rulesFit)

rulesFit$finalModel

save(subset, file = "2013_firstsub.rda")

#KNN--need continmuous variables
#data preparation
summary(df4)
agencies <- names(which(table(df4$AgencyName)>10000))

df4 <- df4[df4$AgencyName %in% agencies,]
df4$AgencyName <- factor(df4$AgencyName)
df4$Agency <- factor(df4$Agency)

sample_ID_2013_03<- sample(1:nrow(df4), size = 10000)
dat4 <- df4[sample_ID_2013_03, ]
subset2 <- subset(dat4, select=c("Education","LOS","Pay"))

save(subset2, file = "2013_subset2.rda")


subset2$LOS <- sapply(subset2$LOS, FUN = function(x)
  if(x == "< 1") 1
  else if(x == "35+") 35
  else floor((as.integer(strsplit(as.character(x), '-')[[1]][1]) +
                as.integer(strsplit(as.character(x), '-')[[1]][2]))/2))


subset2$Education <- as.numeric(as.character(subset2$Education))
subset2 <- na.omit(subset2)

#knn
library(caret)
train<- createFolds(subset2$Pay, k=10)
subset_scaled <- cbind(as.data.frame(scale(subset2[,-3])), Pay = subset2[,3])


knnFit <- train(Pay ~ .,  data = subset_scaled , method = "knn",
                tuneLength = 10,
                trControl = trainControl(
                  method = "cv", indexOut = train))


knnFit <- train(Pay ~ ., method = "knn", data = subset_scaled,
                tuneLength = 5,  tuneGrid=data.frame(k=1:10),
                trControl = trainControl(
                  method = "cv", indexOut = train))
knnFit
varImp(knnFit)

knnFit$finalModel



# CTree--need categorial variables
summary(df4)
subset2 <- subset(dat4, select=c("Education","Pay","LOS"))
subset2$Education <- as.numeric(as.character(subset2$Education))


subset2$Education <- cut(subset2$Education,
              breaks = c(1, 11,22),
              labels = c("<11","11-22"))

subset2$Pay <- cut(subset2$Pay,
              breaks = c(0, 50000, 75000, 100000 ,Inf),
              labels = c("<50k", "50-75k", "75k-100k", ">100k"))

subset2$LOS <- sapply(subset2$LOS, FUN = function(x)
  if(x == "< 1") 1
  else if(x == "35+") 35
  else floor((as.integer(strsplit(as.character(x), '-')[[1]][1]) +
                as.integer(strsplit(as.character(x), '-')[[1]][2]))/2))

subset2$LOS <- cut(subset2$LOS,
                   breaks = c(1,10, 20, 30 ,Inf),
                   labels = c("<10", "10-20", "20-30", ">30"))


subset2<- na.omit(subset2)
summary(subset2)
ctreeFit <- train(Pay~ ., method = "ctree", data = subset2,
                  tuneLength = 5,
                  na.action = na.pass,
                  trControl = trainControl(
                    method = "cv", indexOut = train))

ctreeFit
varImp(ctreeFit)
plot(ctreeFit$finalModel)

#Neural Network
nnetFit <- train(Pay ~ ., method = "nnet", data = subset,
                 tuneLength = 5,
                 na.action = na.pass,
                 trControl = trainControl(
                   method = "cv", indexOut = train),
                 trace = FALSE)


nnetFit
nnetFit$finalModel


#compare models
resamps <- resamples(list(
  rpart=fit,
  ctree=ctreeFit,
  rules=rulesFit
  ))
resamps
summary(resamps)


difs <- diff(resamps)
difs
summary(difs)





