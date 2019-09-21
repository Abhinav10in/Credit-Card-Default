library(DataExplorer)
library(psych)  #describe() function
library(ggplot2)
library(tree)
library(e1071)
library(pROC)
library(caret)
library(plyr)
library(InformationValue)
##################################################################################
# Loading and checking the data set; Taiwan Credit Card Data
##################################################################################
my.path <- 'C:\\Users\\Abhinav\\Desktop\\Fiverr\\Ali\\Capstone\\'; 
my.file <- paste(my.path,'credit_card_default.RData',sep='');
# Read the RData object using readRDS();
credit_card_default <- readRDS(my.file)


###########################
#Section 2
############################

# Intial dataframe structure Checkup;
str(credit_card_default)
#There are 30000 observation and 25 Variabales with Default being the Target Variable
#All the variables have type integer 

#Table data dictionary of default variables 
#Name	Description
#ID	ID of each client
#LIMIT_BAL	Amount of given credit in NT dollars (includes individual and family/supplementary credit)
#SEX	Gender (1=male, 2=female)
#EDUCATION	(1=graduate school, 2=university, 3=high school, 4=others)
#MARRIAGE	Marital status (1=married, 2=single, 3=others)
#AGE	Age in years
#PAY_0	Repayment status in September, 2005 (-2=no consumption, -1=pay duly, 0=the use of revolving credit, 1=payment delay for one month, 2=payment delay for two months, . 8=payment delay for eight months, 9=payment delay for nine months and above)
#PAY_2	Repayment status in August, 2005 (scale same as above)
#PAY_3	Repayment status in July, 2005 (scale same as above)
#PAY_4	Repayment status in June, 2005 (scale same as above)
#PAY_5	Repayment status in May, 2005 (scale same as above)
#PAY_6	Repayment status in April, 2005 (scale same as above)
#BILL_AMT1	Amount of bill statement in September, 2005 (NT dollar)
#BILL_AMT2	Amount of bill statement in August, 2005 (NT dollar)
#BILL_AMT3	Amount of bill statement in July, 2005 (NT dollar)
#BILL_AMT4	Amount of bill statement in June, 2005 (NT dollar)
#BILL_AMT6	Amount of bill statement in April, 2005 (NT dollar)
#PAY_AMT1	Amount of previous payment in September, 2005 (NT dollar)
#PAY_AMT2	Amount of previous payment in August, 2005 (NT dollar)
#PAY_AMT3	Amount of previous payment in July, 2005 (NT dollar)
#PAY_AMT4	Amount of previous payment in June, 2005 (NT dollar)
#PAY_AMT5	Amount of previous payment in May, 2005 (NT dollar)
#PAY_AMT6	Amount of previous payment in April, 2005 (NT dollar)
#Default payment (1=yes, 0=no)



########################################################################
#Data Exploration and Data Quality Check
########################################################################
#A high level Data exploration
str(credit_card_default)
#There are 30000 observation and 24 Predictors and Default acting as Response or Independent Variable
#We also see that some of Independent variables are assigned integer values but in actual they are categorical data hence Should be factor

#Doing data quality Check
summary(credit_card_default)
describe(credit_card_default)
#Here we see that there are no missing values in our dataset 
plot_missing(credit_card_default)
#Hence we don't require data imputation

#Data Transformation of variables
#Changing the name of the column 
names(credit_card_default)[names(credit_card_default) == "PAY_0"] <- "PAY_1"
#We see certian variables like Sex,Education, Marriage etc are categorial in nature hence we need to change it from integer to factor
credit_card_default$SEX = as.factor(credit_card_default$SEX)
credit_card_default$EDUCATION = as.factor(credit_card_default$EDUCATION)
credit_card_default$MARRIAGE = as.factor(credit_card_default$MARRIAGE)
credit_card_default$DEFAULT = as.factor(credit_card_default$DEFAULT)
credit_card_default$PAY_1 = as.factor(credit_card_default$PAY_1)
credit_card_default$PAY_2 = as.factor(credit_card_default$PAY_2)
credit_card_default$PAY_3 = as.factor(credit_card_default$PAY_3)
credit_card_default$PAY_4 = as.factor(credit_card_default$PAY_4)
credit_card_default$PAY_5 = as.factor(credit_card_default$PAY_5)
credit_card_default$PAY_6 = as.factor(credit_card_default$PAY_6)
#Now the values of sex is encoded as 1 and 2 so make it more legible we are converting the data to 'male' and 'female'
levels(credit_card_default$SEX)[levels(credit_card_default$SEX) == "1"]="Male"
levels(credit_card_default$SEX)[levels(credit_card_default$SEX) == "2"]="Female"
str(credit_card_default$SEX)
#Now the Education Variable has 7 factors but according to data description it should only have 4 so merging the education rows with value as 5 and 6 with 4 
#which is Others 
table(credit_card_default$EDUCATION)
#As seen below there are 7 categories but according to the data we should get only 4 categories with 1 = Graduate School
#2 = university 3= high school 4 = others 
#putting combining the values 0,5,6 into 1 which represents unknown
credit_card_default$EDUCATION[credit_card_default$EDUCATION == 0 | credit_card_default$EDUCATION== 5 | credit_card_default$EDUCATION == 6] =5
credit_card_default$EDUCATION = factor(credit_card_default$EDUCATION)
table(credit_card_default$MARRIAGE)
#Again we can club the category with value 0 in category with value 3
credit_card_default$MARRIAGE[credit_card_default$MARRIAGE == 0] =3
credit_card_default$MARRIAGE = factor(credit_card_default$MARRIAGE)
table(credit_card_default$MARRIAGE)
levels(credit_card_default$MARRIAGE)[levels(credit_card_default$MARRIAGE) == "1"] <- "Married"
levels(credit_card_default$MARRIAGE)[levels(credit_card_default$MARRIAGE) == "2"] <- "Single"
levels(credit_card_default$MARRIAGE)[levels(credit_card_default$MARRIAGE) == "3"] <- "Other"

##################################
#Exploratory Data Analysis
#################################

###Data Exploration for Dependent Variable
ggplot(credit_card_default, aes(x=DEFAULT)) + geom_bar(aes(group=DEFAULT, colour=DEFAULT, fill=DEFAULT), alpha=1)+geom_text(stat='count', aes(label=..count..), vjust=-1)
#As seen here there are lot of people who have not defaulted and only 22% of total test population had defaulted 

# Frequency Distribution of numerical features
par(mfrow=c(2,7))
for(i in c(2,6,13,14,15,16,17,18,19,20,21,22,23,24)){
  hist(credit_card_default[,i], xlab = '', col='steelblue',  main=names(credit_card_default[i]))
}

#Exploring the LIMIT_BAL and DEFAULT relation
ggplot(credit_card_default, aes(x=LIMIT_BAL)) + geom_histogram(aes(group=DEFAULT, colour=DEFAULT, fill=DEFAULT), alpha=0.3)+ggtitle('Histogram of LIMIT_BALS')
#We see there is default present in all across the limit bal
ggplot(credit_card_default, aes(x=LIMIT_BAL)) + geom_density(aes(group=DEFAULT, colour=DEFAULT, fill=DEFAULT), alpha=0.3)


# Studying Sex and Default
ggplot(credit_card_default, aes(x=SEX)) + geom_bar(aes(group=DEFAULT, colour=DEFAULT, fill=DEFAULT), alpha=1)+ggtitle('T')
table(credit_card_default$SEX,credit_card_default$DEFAULT)
#There are 11888 males and the default percentage in the males are 24.167%
#There are 18112 females and the default percentage in the females are 20.77%
#Hence Males are more likely to default then females 


# Studying Education and Default
ggplot(credit_card_default, aes(x=EDUCATION)) + geom_bar(aes(group=DEFAULT, colour=DEFAULT, fill=DEFAULT), alpha=1)+ggtitle('T')
table(credit_card_default$EDUCATION,credit_card_default$DEFAULT)
#Percentage of Graduate school Default is 19.23 %
#Percentage of University Default is 23.73 %
#Percentage of High school Default is 25.15 %
#Percentage of Others Default is 5.69%
#Percentage of Unknown Default is 7.53%

# Studying Marriage and Default
ggplot(credit_card_default, aes(x=MARRIAGE)) + geom_bar(aes(group=DEFAULT, colour=DEFAULT, fill=DEFAULT), alpha=1)+ggtitle('T')
table(credit_card_default$MARRIAGE,credit_card_default$DEFAULT)
#Percentage of Married Default is 23.47 %
#Percentage of Single Default is 20.92 %
#Percentage of Others Default is 23.60 %

# Studying Age and Default
ggplot(credit_card_default, aes(x=AGE)) + geom_bar(aes(group=DEFAULT, colour=DEFAULT, fill=DEFAULT), alpha=1)+ggtitle('T')
table(credit_card_default$AGE,credit_card_default$DEFAULT)

# Generating bar plots for PAY_1..6 variables 
pay.cols.names  <- paste0("PAY_", c(1:6))  # "PAY_1" "PAY_2" "PAY_3"..
pay.histograms <- lapply(pay.cols.names, function (pay.col.name){ 
           ggplot(data = credit_card_default[, pay.cols.names], 
                  aes(x = credit_card_default[, pay.col.name])) +
             geom_bar(stat = "count") + theme_minimal() +
             xlab(paste0("Repayment status ", pay.col.name)) +
             ylab("Observations count")
         })
library(Rmisc)
multiplot(plotlist = pay.histograms, cols = 3)
#we can see in certain repayment month certain values are very rare 

#Making a Correlation matrix to understand the important factors of the affecting the default prediction
install.packages("corrgram")
library(corrgram)
numerical_subset = credit_card_default[,c(2,6,13,14,15,16,17,18,19,20,21,22,23,24)]
corrgram(numerical_subset, order = TRUE, lower.panel = panel.shade,upper.panel = panel.pie, text.panel = panel.txt)
corrgram(numerical_subset, order = TRUE, lower.panel = panel.ellipse,upper.panel = panel.pts, text.panel = panel.txt,diag.panel = panel.minmax)
#We see there is high correlation between the billing amounts in different months and small amount of correlation between the payment amounts


#Checking the correlation between the categorical variables
# Creating combinations of pairs for categorical variables
cat.var.names <- colnames(credit_card_default[sapply(credit_card_default, is.factor)])  
# Combinations of pairs
cat.combs     <- as.data.frame(t(combn(cat.var.names, 2)))  

# Performing Chi-Square tests for all pairs
cat.chisq.test.res <- apply(t(cat.combs), 2, function(x){
    c(Var1   = x[1], 
      Var2   = x[2], 
      pvalue = chisq.test(credit_card_default[, x[1]], 
                          credit_card_default[, x[2]])$p.value)
  }
  )
library(dplyr)
cat.chisq.test.res<- cat.chisq.test.res %>% t() %>% data.frame(stringsAsFactors=FALSE) 
cat.chisq.test.res$pvalue <- as.numeric(cat.chisq.test.res$pvalue)

cat.chisq.test.res        <- cat.chisq.test.res %>% 
  filter(pvalue <= .05) %>%  # Filter p-values < 0.05
  arrange(pvalue)            # Most dependent at top
library(knitr)
knitr::kable(cat.chisq.test.res, caption = "Table with kable", 
             format = "markdown", padding = 0,
             col.names = c("", "", "p-value"))

# We are performing Chi-square test to acertain relationship between the categorical variables 
#we are testing null hypothesis whether the one variable is independent of another at 0.05 significance level. 
#We will filter all combinations with p-value less than 0.05 significance level because under this level we reject the null hypothesis that variables are independent.
#We see the variables are dependent on each other and Pay_X has significant effect on the Default

###################################
#Feature Engineering
##################################
#Discretizing Age
#Bin of Age
credit_card_default$Age_bin[credit_card_default$AGE >=18 & credit_card_default$AGE <=25] = "Age_18_25"
credit_card_default$Age_bin[credit_card_default$AGE >25 & credit_card_default$AGE <=40] = "Age_26_40"
credit_card_default$Age_bin[credit_card_default$AGE >40] = "Age_41_100"
credit_card_default$Age_bin = as.factor(credit_card_default$Age_bin)

credit_card_default$Avg_Bill_Amt = rowMeans(credit_card_default[13:18])
credit_card_default$Avg_Pmt_Amt = rowMeans(credit_card_default[19:24])
#Here the pmt_ratio_1 is the bill amount in month 2 and payment in month 1 if the value is 0 that means there is no bill to be payed in the 2nd month
#As the values increase from 0 to 1 and beyond that means that bill are either being partially paid or not being paid 
credit_card_default$pmt_ratio_1 = ifelse(credit_card_default$BILL_AMT2 == 0 , 0,(credit_card_default$BILL_AMT2 - credit_card_default$PAY_AMT1)/credit_card_default$BILL_AMT2 )
credit_card_default$pmt_ratio_2 = ifelse(credit_card_default$BILL_AMT3 == 0 , 0,(credit_card_default$BILL_AMT3 - credit_card_default$PAY_AMT2)/credit_card_default$BILL_AMT3 )
credit_card_default$pmt_ratio_3 = ifelse(credit_card_default$BILL_AMT4 == 0 , 0,(credit_card_default$BILL_AMT4 - credit_card_default$PAY_AMT3)/credit_card_default$BILL_AMT4 )
credit_card_default$pmt_ratio_4 = ifelse(credit_card_default$BILL_AMT5 == 0 , 0,(credit_card_default$BILL_AMT5 - credit_card_default$PAY_AMT4)/credit_card_default$BILL_AMT5 )
credit_card_default$pmt_ratio_5 = ifelse(credit_card_default$BILL_AMT6 == 0 , 0,(credit_card_default$BILL_AMT6 - credit_card_default$PAY_AMT5)/credit_card_default$BILL_AMT6 )
credit_card_default$avg_pmt_ratio = rowMeans(credit_card_default[34:38])
#utilization factor is How much you current owe divided by credit limit
credit_card_default$util_1 = (credit_card_default$BILL_AMT2 - credit_card_default$PAY_AMT1)/credit_card_default$LIMIT_BAL
credit_card_default$util_2 = (credit_card_default$BILL_AMT3 - credit_card_default$PAY_AMT2)/credit_card_default$LIMIT_BAL
credit_card_default$util_3 = (credit_card_default$BILL_AMT4 - credit_card_default$PAY_AMT3)/credit_card_default$LIMIT_BAL
credit_card_default$util_4 = (credit_card_default$BILL_AMT5 - credit_card_default$PAY_AMT4)/credit_card_default$LIMIT_BAL
credit_card_default$util_5 = (credit_card_default$BILL_AMT6 - credit_card_default$PAY_AMT5)/credit_card_default$LIMIT_BAL
credit_card_default$avg_util = rowMeans(credit_card_default[40:44])
credit_card_default$bal_growth_6mo = ((credit_card_default$BILL_AMT6 - credit_card_default$PAY_AMT5) - (credit_card_default$BILL_AMT2 - credit_card_default$PAY_AMT1))
credit_card_default$util_growth_6mo = credit_card_default$util_5 - credit_card_default$util_1 
credit_card_default$max_bill_amt = pmax(credit_card_default$BILL_AMT1,credit_card_default$BILL_AMT2,credit_card_default$BILL_AMT3,credit_card_default$BILL_AMT4,
                                        credit_card_default$BILL_AMT5,credit_card_default$BILL_AMT6)
credit_card_default$max_pmt_amt = pmax(credit_card_default$PAY_AMT1,credit_card_default$PAY_AMT2,credit_card_default$PAY_AMT3,credit_card_default$PAY_AMT4,
                                       credit_card_default$PAY_AMT5,credit_card_default$PAY_AMT6)
credit_card_default[,"max_dlq"] = apply(credit_card_default[,7:12],1,max) 
credit_card_default$max_dlq[credit_card_default$max_dlq < 0 ] = 0
credit_card_default$max_dlq = as.factor(credit_card_default$max_dlq)

##############################
#MOdel Based EDA
#############################
library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree

##############################Spliting the data in train, Validate and test data set 
train = credit_card_default[credit_card_default$train==1,]
validate = credit_card_default[credit_card_default$validate==1,]
test = credit_card_default[credit_card_default$test==1,]
train = train[,-c(1,26:30)]
validate = validate[,-c(1,26:30)]
test = test[,-c(1,26:30)]
#####################Table providing the number of data present in train,test and validate

########Decision Tree##############

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3003)
dtree_fit <- train(DEFAULT ~ LIMIT_BAL+SEX+EDUCATION+MARRIAGE+PAY_1+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+BILL_AMT1+BILL_AMT2+BILL_AMT3
                   +BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6+Age_bin+Avg_Bill_Amt+Avg_Pmt_Amt
                   +pmt_ratio_1+pmt_ratio_2+pmt_ratio_3+pmt_ratio_4+pmt_ratio_5+avg_pmt_ratio+util_1+util_2+util_3+util_4+util_5+avg_util+
                     bal_growth_6mo+util_growth_6mo+max_bill_amt+max_pmt_amt+max_dlq,
                   data = credit_card_default, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
dtree_fit
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

#From the Decision Tree we see that PAY_1 with repayment status 2 is most important classifier of dataset, Other important variables according to the decision
#tree EDA is Pay_2,Pay_3, max_dlq, Avg_pmt, Pay_4,pay_3,bal_grow_6month and so on

########################################################
#Modelling  And performance
#######################################################

######### Random Forest##############################3
library(randomForest)
model.rf = randomForest(DEFAULT ~ LIMIT_BAL+SEX+EDUCATION+MARRIAGE+PAY_1+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+BILL_AMT1+BILL_AMT2+BILL_AMT3
                        +BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6+Age_bin+Avg_Bill_Amt+Avg_Pmt_Amt
                        +pmt_ratio_1+pmt_ratio_2+pmt_ratio_3+pmt_ratio_4+pmt_ratio_5+avg_pmt_ratio+util_1+util_2+util_3+util_4+util_5+avg_util+
                          bal_growth_6mo+util_growth_6mo+max_bill_amt+max_pmt_amt+max_dlq,data = train)
model.rf
varImp(model.rf)
varImpPlot(model.rf,type=2)

########################Gradient Boosting################################
set.seed(3003)
library(gbm)
model.gb = gbm(DEFAULT ~ LIMIT_BAL+SEX+EDUCATION+MARRIAGE+PAY_1+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+BILL_AMT1+BILL_AMT2+BILL_AMT3
               +BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6+Age_bin+Avg_Bill_Amt+Avg_Pmt_Amt
               +pmt_ratio_1+pmt_ratio_2+pmt_ratio_3+pmt_ratio_4+pmt_ratio_5+avg_pmt_ratio+util_1+util_2+util_3+util_4+util_5+avg_util+
                bal_growth_6mo+util_growth_6mo+max_bill_amt+max_pmt_amt+max_dlq,data = train,distribution = "multinomial",
               n.trees = 10000,shrinkage = 0.01, interaction.depth = 4)
summary(model.gb)

############################Naive Bayes###############################
set.seed(3003)
model.nb = naiveBayes(DEFAULT ~ LIMIT_BAL+SEX+EDUCATION+MARRIAGE+PAY_1+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+BILL_AMT1+BILL_AMT2+BILL_AMT3
                      +BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6+Age_bin+Avg_Bill_Amt+Avg_Pmt_Amt
                      +pmt_ratio_1+pmt_ratio_2+pmt_ratio_3+pmt_ratio_4+pmt_ratio_5+avg_pmt_ratio+util_1+util_2+util_3+util_4+util_5+avg_util+
                        bal_growth_6mo+util_growth_6mo+max_bill_amt+max_pmt_amt+max_dlq,
                      data = train)
model.nb
#The model creates the conditional probability for each feature separately. We also have the a-priori probabilities which indicates the distribution of our data. 

#Logistic regression 
model.lr = glm(DEFAULT ~ PAY_1+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+max_dlq+Avg_Pmt_Amt+max_bill_amt+max_pmt_amt+BILL_AMT1+BILL_AMT2+BILL_AMT3
               +BILL_AMT4+BILL_AMT5+BILL_AMT6+LIMIT_BAL+util_1+util_2+util_3+util_4+util_5+avg_util+
                 bal_growth_6mo+util_growth_6mo, family = binomial,data = train)
#Taking those varibales which had maximum importance as shown in the Random Forest and Gradient Boosting 
summary(model.lr)
#using another method to get the Better logistic regression
model.lr1 = glm(DEFAULT ~ LIMIT_BAL+SEX+EDUCATION+MARRIAGE+PAY_1+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+BILL_AMT1+BILL_AMT2+BILL_AMT3
                +BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6+Age_bin+Avg_Bill_Amt+Avg_Pmt_Amt
                +pmt_ratio_1+pmt_ratio_2+pmt_ratio_3+pmt_ratio_4+pmt_ratio_5+avg_pmt_ratio+util_1+util_2+util_3+util_4+util_5+avg_util+
                  bal_growth_6mo+util_growth_6mo+max_bill_amt+max_pmt_amt+max_dlq,family = binomial,data = train)
step = stepAIC(model.lr1, direction = "backward",trace = FALSE)
coefficients(step) # Model coefficients
summary(step)
AIC(step)

##############################
#Performance and Comparision of the result 
##############################
###########################For Logistic function#####################
predictandevaluate = function(model,data,Model_name,data_name){
  pred.dt <- predict(model, newdata = data, type = "response")
  optCutOff = optimalCutoff(data$DEFAULT, pred.dt)[1]
  pred.dt = ifelse(pred.dt>optCutOff,"1","0")
  cf = table(`Actual Class` = data$DEFAULT, `Predicted Class` = pred.dt)
  error.rate.rpart <- sum(data$DEFAULT != pred.dt)/nrow(data)
  print("Confusion Matrix :")
  print(cf)
  Accuracy = (cf[1,1]+cf[2,2])/(nrow(data))
  Misclassification_rate = 1-Accuracy
  Sensitivity = cf[2,2]/(cf[2,1]+cf[2,2])
  Specificity = cf[1,1]/(cf[1,1]+cf[1,2])
  Precision = cf[2,2]/(cf[2,2]+cf[1,2])
  result = data.frame(Model_name,data_name,Accuracy,Sensitivity,Specificity,Precision)
  return(result)
}
predictandevaluate(model.lr,train,"Logistic","train")
predictandevaluate(model.lr,validate,"Logistic","Validate")
predictandevaluate(model.lr,test,"Logistic","Test")

###########################For Other Models###########################
predictandevaluate1 = function(model,data,Model_name,data_name){
  pred.dt <- predict(model, newdata = data, type = "class")
  cf = table(`Actual Class` = data$DEFAULT, `Predicted Class` = pred.dt)
  error.rate.rpart <- sum(data$DEFAULT != pred.dt)/nrow(data)
  print("Confusion Matrix :")
  print(cf)
  Accuracy = (cf[1,1]+cf[2,2])/(nrow(data))
  Misclassification_rate = 1-Accuracy
  Sensitivity = cf[2,2]/(cf[2,1]+cf[2,2])
  Specificity = cf[1,1]/(cf[1,1]+cf[1,2])
  Precision = cf[2,2]/(cf[2,2]+cf[1,2])
  result = data.frame(Model_name,data_name,Accuracy,Sensitivity,Specificity,Precision)
  return(result)
}
###########Random Forest###################
predictandevaluate1(model.rf,train,"Random Forest","train")
predictandevaluate1(model.rf,validate,"Random Forest","Validate")
predictandevaluate1(model.rf,test,"Random Forest","Test")

##############Gradient_Boost#################
predictandevaluate1(model.rf,train,"Gradient_Boost","train")
predictandevaluate1(model.rf,validate,"Gradient_Boost","Validate")
predictandevaluate1(model.rf,test,"Gradient_Boost","Test")

###############Naive Bayes#####################
predictandevaluate1(model.nb,train,"Naive Bayes","train")
predictandevaluate1(model.nb,validate,"Naive Bayes","Validate")
predictandevaluate1(model.nb,test,"Naive Bayes","Test")



