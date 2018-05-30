
#213451 USERS 16 FEATURES
train=read.csv("train_users_2.csv",stringsAsFactors = F)

# df allow diffent variable type
# ARARY N*N*P
# list allow different length
summary(train$age)
# clean float variables
train$age[train$age<14 |train$age>90]<- -1
train$age[which(is.na(train$age))]<- -1
summary(train$age)
# clean categorical variables
table(train$gender)
table(train$signup_method)
table(train$signup_app)
# how many webpages he view on airbnb before signup
table(train$signup_flow)
table(train$country_destination)

head(train$date_account_created)
options("scipen"=10)
head(train$timestamp_first_active)
head(train$date_first_booking)
head(train$country_destination)

# linear regression assumption: linear realationship x1x2xn and y 
# b1b2bn how to calculate: dui b1b2bn qiu partial deriveative

# p(y=1|x) is a linear function of x
# problem: is very sensitive to outlier

