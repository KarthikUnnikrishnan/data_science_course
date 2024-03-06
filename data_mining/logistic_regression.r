#Read data file
data_in <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
str(data_in)
data_in$admit <- as.factor(data_in$admit)
data_in$rank <- as.factor(data_in$rank)
str(data_in)

# Two-way table of factor variables
xtabs(~admit + rank,data = data_in)

#Partition Data - train (80%) & test (20%)
set.seed(1234)
ind <- sample(2,nrow(data_in),replace=T,prob = c(0.8,0.2))
train <- data_in[ind==1,]
test <- data_in[ind==2,]

#Logistic regression model
my_model <- glm(admit ~ gre+gpa+rank, data = train, family = 'binomial')
summary(my_model)


# Prediction
p1 <- predict(my_model, train, type = 'response')
head(p1)
head(train)


#Miss classification Error - train data
pred1 <- ifelse(p1>0.5,1,0)
tab1 <- table(predicted = pred1, actual = train$admit)
tab1
1 - sum(diag(tab1))/sum(tab1)


#Miss Classification Error - test data
p2 <- predict(my_model, test, type = 'response')
pred2 <- ifelse(p2>0.5,1,0)
tab2 <- table(predicted = pred2, actual = test$admit)
tab2
1 - sum(diag(tab2))/sum(tab2)


# Goodness-of-fit test
with(my_model,pchisq(null.deviance - deviance, df.null-df.residual,lower.tail = F))

