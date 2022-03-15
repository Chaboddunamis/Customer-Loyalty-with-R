install.packages('plyr')
install.packages('rpart.plot')
install.packages('caret')
install.packages('gridExtra')
install.packages('tidyverse')
install.packages('rsample')
install.packages('e1071')
install.packages('GGally')
install.packages('data.table')
install.packages('DT')
install.packages('readr')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('tidyr')
install.packages('corrplot')
install.packages('rms')
install.packages('MASS')
install.packages('e1071')
install.packages('ROCR')
install.packages('gplots')
install.packages('pROC')
install.packages('rpart')
install.packages('randomForest')
install.packages('ggpubr')
install.packages('readxl')
library(plyr)  
library(rpart.plot) 
library(caret)
library(gridExtra) 
library(tidyverse) 
library(rsample)
library(e1071) 
library(GGally)
library(data.table)
library(DT)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(rms)
library(MASS)
library(e1071)
library(ROCR)
library(gplots)
library(pROC)
library(rpart)
library(randomForest)
library(ggpubr)
library(readxl)
library(dplyr)
churn <- read_excel("DDM22_63_customer.xlsx")
score <- read_excel("DDM22_63_score.xlsx")
summary(churn)
mobiledata <- left_join(churn, score, by = "cid")
sapply(mobiledata, function(x) sum(is.na(x))) # Check missing values
mobiledata %>% group_by(gender) %>% summarise(mean = mean(monthly_charges, na.rm = TRUE)) # Average charges by gender
mobiledata[is.na(mobiledata$monthly_charges),]

# Demographic plots
gender_plot <- ggplot(mobiledata, aes(x = gender)) + geom_bar(aes(fill = contract_terminated)) + geom_text(aes(y = ..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)
citizenship <- ggplot(mobiledata, aes(x = citizenship)) + geom_bar(aes(fill = contract_terminated)) + geom_text(aes(y = ..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)
age_plot <- boxplot(mobiledata$age,
                    main = "Age Distribution",
                    xlab = "Age of customers",
                    col = "orange",
                    border = "brown",
                    horizontal = TRUE,
                    notch = TRUE
)


# Service Plots
contract_plot <- ggplot(mobiledata, aes(x = contract_type)) +
  geom_bar(aes(fill = contract_terminated)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

data_allowance <- ggplot(mobiledata, aes(x = data_allowance)) +
  geom_bar(aes(fill = contract_terminated)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

payment_method_plot <- ggplot(mobiledata, aes(x = payment_method)) +
  geom_bar(aes(fill = contract_terminated)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

payment_arrears_plot <- ggplot(mobiledata, aes(x = payment_arrears)) +
  geom_bar(aes(fill = contract_terminated)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
number_transferred_plot <- ggplot(mobiledata, aes(x = number_transferred)) +
  geom_bar(aes(fill = contract_terminated)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
tenure_plot <- boxplot(mobiledata$tenure,
        main = "Tenure",
        xlab = "Tenure of customers",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
# Satisfaction score analysis by variables
satisfaction_score_by_charge <- ggplot(mobiledata, aes(y= monthly_charges, x = score)) + 
  geom_point(aes(color = "blue"), alpha = 0.8) + 
  geom_smooth(formula = y~x, method="lm") + 
  ggtitle("Satisfaction Score By Monthly Charges") +
  xlab("Customer satisfaction score") + 
  ylab("Monthly Charges") +
  theme_bw(base_size =  12) + theme(legend.position = "none") 

satisfaction_score_by_tenure <- ggplot(mobiledata, aes(y= tenure, x = score)) + 
  geom_point(aes(color = "blue"), alpha = 0.8) + 
  geom_smooth(formula = y~x, method="lm") + 
  ggtitle("Satisfaction By Tenure") +
  xlab("Customer satisfaction score") + 
  ylab("Tenure") +
  theme_bw(base_size =  12) + theme(legend.position = "none")

satisfaction_gender_plot <- ggplot( mobiledata, aes(y= gender, fill=contract_terminated, x= score)) +
  geom_boxplot()  +
  ggtitle("Satisfaction By gender") +
  ylab("") + 
  xlab("Customer satisfaction score") + 
  theme(
    axis.text.y = element_text(),
    axis.ticks = element_blank())

satisfaction_citizenship <- ggplot( mobiledata, aes(y= citizenship, fill=contract_terminated, x= score)) +
  geom_boxplot()  +
  ggtitle("Satisfaction By citizenship") +
  ylab("") + 
  xlab("Customer satisfaction score") + 
  theme(
    axis.text.y = element_text()
    
satisfaction_data_plot <- ggplot(mobiledata, aes(y= data_allowance, fill=contract_terminated, x= score)) +
      geom_boxplot()  +
      ggtitle("Satisfaction by Data Allowance") +
      ylab("data") + 
      xlab("score") + 
      theme(
        axis.text.y = element_text(),
        axis.ticks = element_blank())
satisfaction_contract_type <- ggplot(mobiledata, aes(y= contract_type, fill=contract_terminated, x= score)) +
  geom_boxplot()  +
  ggtitle("Satisfaction by Contract Type") +
  ylab("Contract Type") + 
  xlab("score") + 
  theme(
    axis.text.y = element_text(),
    axis.ticks = element_blank())

satisfaction_payment_method <- ggplot(mobiledata, aes(y= payment_method, fill=contract_terminated, x= score)) +
  geom_boxplot()  +
  ggtitle("Satisfaction by Payment Method") +
  ylab("Payment Method") + 
  xlab("score") + 
  theme(
    axis.text.y = element_text(),
    axis.ticks = element_blank())

satisfaction_number_plot <- ggplot(mobiledata, aes(y= number_transferred, fill=contract_terminated, x= score)) +
  geom_boxplot()  +
  ggtitle("Satisfaction by Number Transferred") +
  ylab("Number Transferred") + 
  xlab("score") + 
  theme(
    axis.text.y = element_text(),
    axis.ticks = element_blank())
satisfaction_by_age


# Correlations
mobiledata %>%
  dplyr::select (contract_terminated, MonthlyCharges, tenure) %>%
  cor() %>%
  corrplot.mixed(upper = "circle", tl.col = "black", number.cex = 0.7)

# Modelling
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(mobiledata), size = floor(.75*nrow(mobiledata)), replace = F)
train <- mobiledata[sample, ]
test  <- mobiledata[-sample, ]

tr_fit <- rpart(contract_terminated ~., data = train, method="class")
rpart.plot(tr_fit) # 

rpart.plot(tr_fit)
> tr_prob1 <- predict(tr_fit, test)
> tr_pred1 <- ifelse(tr_prob1[,2] > 0.5,"Yes","No")
> table(Predicted = tr_pred1, Actual = test$contract_terminated) # 
tr_prob2 <- predict(tr_fit, train)
tr_pred2 <- ifelse(tr_prob2[,2] > 0.5,"Yes","No")
tr_tab1 <- table(Predicted = tr_pred2, Actual = train$contract_terminated)
tr_tab2 <- table(Predicted = tr_pred1, Actual = test$contract_terminated)


