
############ CAPSTONE PROJECT ###########
## Title : Customer behavior and Recommender system
#########################################

## Load required libraries
library(dplyr)
library(readxl)
library(ggplot2)
library(reshape)
library(scales)
library(arules)
library(arulesViz)
library(lmtest)
library(pscl)
library(Deducer)
library(smooth)
library(forecast)
library(graphics)
library(tseries)
# Read the data
store <- read_excel("E:/.../Global_store.xlsx")

#Data Exploration
dim(store)
str(store)
summary(store)
head(store)

#Missing values are found in Postal code variable. This variable can be ignored as 
#we have values only for USA and is not of much importance.
#delete Postal code column
store$`Postal Code` <- NULL

######## Exploratory Data Analysis ########

##country wise sales and profit
coun_sal_pro <- store %>% group_by(Country) %>%
  summarise(Total_Orders = n(), Total_sales = sum(Sales), Profit = sum(Profit)) %>%
  arrange(desc(Total_sales)) %>%
  mutate(sales_per = round((Total_sales/sum(Total_sales)*100),1))

#Top 10 coutries with highest sales result to 62.5% of the total sales
#plot the sales and profit of top 10 countries

g <- ggplot(coun_sal_pro[1:10,], aes(x = reorder(Country,Total_sales), y = Total_sales))
g + geom_bar(aes(fill = Profit), stat = "identity", position = position_dodge()) + coord_flip() +
  labs(y = "Total Sales", x = "Country", title = "Top 10 Countries Sales & Profit") +
  scale_fill_gradient(labels = comma) + theme(plot.title = element_text(hjust = 0.5))
#######Excluded major data exploration##############

#####Association Rule#####

##filter data for orders from USA only
store_usa <- store[store$Country == 'United States',]

##create a table with orders as index and sub-categories as columns with counts
##basically which looks like a transaction data.
store_trans <- as(split(store_usa[,"Sub.Category"], store_usa[,"Order.ID"]), "transactions")

##frequently ordered items
freq_items <- eclat(store_trans, parameter = list(support = 0.05, maxlen = 20))
inspect(freq_items)
itemFrequencyPlot(store_trans, topN=20, type = "absolute", main="Item Frequency")

##create rules
rules <- apriori(store_trans, parameter = list(support = 0.001, confidence = 0.6))
rules <- sort(rules, by = 'confidence')
inspect(rules[1:5])

##remove redundant rules which might be subset of larger rules.
# subset.matrix <- is.subset(rules, rules)
# subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
# redundant <- colSums(subset.matrix, na.rm=T) >= 1
# rules.pruned <- rules[!redundant]
# rules <- rules.pruned

##visualization
plot(rules, main = "Scatter Plot")
#plot(rules, measure=c("support","lift"), shading="confidence")

#####Churn Customers##########
#considering orders after 2015 as test data
#determining churn as customers who have not ordered in 2014 which is our train data
store_usa_train <- store_usa[store_usa$Order.Date < '2015-01-01',]
churn_cust <- store_usa_train %>% group_by(Customer.ID) %>%
  summarise(first_order = min(Order.Date), last_order = max(Order.Date))
churn_cust$target <- ifelse(churn_cust$first_order < '2013-06-30' &
                              churn_cust$last_order < '2014-01-01', 1, 0)
store_train_model <- merge(x = store_usa_train, y = churn_cust[,c("Customer.ID","target")], by = "Customer.ID", all.x = TRUE)

######Logistic Model######
store_train_model$Country <- NULL
store_train_model$Market <- NULL
model <- glm(target ~ Segment+Order.Priority+Discount+Shipping.Cost, data = store_train_model, family = "binomial")
summary(model)

lrtest(model)
exp(coef(model)) ##odds ratio
exp(coef(model))/(1+exp(coef(model))) # Probability

pR2(model)

##Classification table
pred <- predict(model, newdata = store_train_model, type="response")
y_pred_num <- ifelse(pred > 0.4,1,0)
y_pred <- factor(y_pred_num, levels=c(0,1))
y_act <- store_train_model$target
confusionMatrix(y_pred,y_act,positive="1")
rocplot(model)

##Model on Test data
store_test <- store_usa[store_usa$Order.Date >= '2015-01-01',]
pred <- predict(model, newdata = store_test, type="response")
store_test$target <- ifelse(pred>0.5,1,0)
y_pred <- factor(y_pred_num, levels=c(0,1))
y_act <- store_test$target
table(store_test$target)
rocplot(model)

#######Time series Forecast#######
#create a dataframe with monthly sales
store_usa$yearmonth <- format(store_usa$Order.Date, format = "%Y-%m")
store_timeseries <- store_usa %>% group_by(yearmonth) %>%
  summarise(sales = round(sum(Sales),0))

ggplot(store_timeseries, aes(yearmonth, sales)) + geom_line() + scale_x_date('month')  + ylab("Sales") +
  xlab("")










