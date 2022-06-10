library(ggplot2)
library("dplyr")

## Data Exploration

ggplot(data = Customertravel) +
  geom_bar(mapping = aes(x= Age, fill=Target), position = "dodge")

summarize(Customertravel, Churn_ave = mean())

##Segmenting each Income variable to know which one has the biggest churn. Does Income Class has something to do with it?
Seg_IncomeLevel<- Customertravel %>% group_by(AnnualIncomeClass) %>% summarize(Target)

ggplot(Seg_IncomeLevel) + geom_bar(aes(x="High Income", y= ))

ggplot(data = Seg_IncomeLevel) +
  geom_bar(mapping = aes(x= AnnualIncomeClass, fill=Target), position = "dodge")


ggplot(data = Customertravel) +
  geom_bar(mapping = aes(x= AnnualIncomeClass, fill=Target), position = "dodge")

filter(Customertravel, AnnualIncomeClass == "High Income")

select(Customertravel, AnnualIncomeClass, Target)

ggplot(Customertravel,aes(x=factor(AnnualIncomeClass),y = Target)) + geom_boxplot()

ggplot(Customertravel, aes(x = AnnualIncomeClass, y = Age, color = FrequentFlyer))+
  geom_point(aes(alpha = BookedHotelOrNot)) + geom_path()

ct_HighIncome <- filter(Customertravel, Target== "High Income") 
head(ct_HighIncome)


##1 - Are customers Frequent Flyers?
ggplot(data = Customertravel) +
  geom_bar(mapping = aes(x= Age, fill=FrequentFlyer), position = "dodge")

## According to the data the mojority of customers are not Frequent Flyers, with the most ranging at 30yrs old. 

##2- What are the most relevant income among customers?
ggplot(data = Customertravel) +
  geom_bar(mapping = aes(x= Age, fill=AnnualIncomeClass), position = "dodge")

## Looks that the majority of clients are low and middle income class. 

##3- Does the annual income class influence churn?
## 0= Customer doesn't Churn  1= Customer Churn
ggplot(data = Customertravel) +
  geom_bar(mapping = aes(x= Target, fill=AnnualIncomeClass), position = "dodge")
## Middle income seem to be the highest income not churning, followed by low income.

##4- Income and Services Opted
ggplot(data = Customertravel) +
  geom_bar(mapping = aes(x= ServicesOpted, fill=AnnualIncomeClass), position = "dodge")

ggplot(data = Customertravel) +
  geom_bar(mapping = aes(x= Age, fill=BookedHotelOrNot), position = "dodge")

ggplot(data = Customertravel) +
  geom_bar(mapping = aes(x= Age, fill=AccountSyncedToSocialMedia), position = "dodge")

ggplot(data = Customertravel) +
  geom_bar(mapping = aes(x= Age, fill=Target), position = "dodge")
## I am not able to see 


##Is there a correlation between Age and Target
d <- ggplot(Customertravel, aes(x=Age, y=Target))
d + geom_point() + geom_smooth(method=lm, se=FALSE)
## There is no correlation between Age of the customers and Churn.

d <- ggplot(Customertravel, aes(x = AnnualIncomeClass, y = Age ))
d + geom_point() + geom_smooth(method=lm, se=FALSE)

## Normally Distributed?
ggplot(Customertravel, aes(sample = Age)) + geom_qq()

head(Customertravel)

lin_reg <- lm(AnnualIncomeClass ~ Target, Customertravel)
print(lin_reg)

summary(lin_reg)

## Since P-value is greater than .05 there is no significance, therefore there is "0" correlation. 

## Data Testing Assumptions : Does Age influence Churn?

##Independent Chi-Square
install.packages("gmodels")
library("gmodels")

CrossTable(Customertravel$Age, Customertravel$Target, fisher=TRUE
, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")


## Since P-value is less than .05, this means that this analysis is significant, age does influence Target/ Churn.
## Some ages have a Std Residual above/below 2, Does this make ut more prone to churn?

CrossTable(Customertravel$AnnualIncomeClass, Customertravel$Target, fisher=TRUE
           , chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

##Goodness of Fit Chi-Square

library(dplyr)

Customertravel%>% group_by(AccountSyncedToSocialMedia) %>% summarize(count=n())
observed=c(360,594)
expected=c(.15, .85)
chisq.test(x=observed, p = expected)
## Since the P-value is less than .05 it seems that the sample is more or less confused compared to most of the population.

## Grouping Age and Target, to see if its influence.
##ggplot(Customertravel, aes(Target, Age, colour = Target)) + 
##  geom_point()

Customertravel %>% 
  count(Age)
breaks <- c(27,28,29,30,31,33,34,35, 36,37,38,39)
AgeGroups <- c("[27-29)", "[30-31)", "[33-34)", "[35-36)", "[37-38)")
ggplot (Customertravel = as_tibble((group_tags)))
summary(AgeGroups)

Customertravel

group_tags <- cut (Customertravel$Age,
                  breaks = breaks,
                  include.lowest = TRUE,
                  right = FALSE,
                  labels = AgeGroups)

ggplot(Customertravel = as_tibble(Age), mapping = aes(x=AgeGroups))
       geom_point()

summary(Customertravel)

d <- ggplot(Customertravel, aes(x = AgeGroups, y = Target ))
d + geom_point() + geom_smooth(method=lm, se=FALSE) 


ggplot(data=Customertravel) +
  geom_bar(mapping = aes(x = Age, fill= Target)) + 
  ggtitle("Sales Categories by Salary Level") +
  xlab("Sales Category") +
  ylab("Frequency")  

ggplot(data=Customertravel) +
  geom_bar(mapping = aes(x = Target, fill=Age), position = "fill") + 
  ggtitle("Target by Age") +
  xlab("Age") +
  ylab("Frequency")  


Customertravel$Target <- ifelse(Customertravel$Target == "1","0", "Yes", "No")
Customertravel$Target <- ifelse(Customertravel$Target == "1","0", "No")

