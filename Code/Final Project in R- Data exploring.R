library(ggplot2)
library("dplyr")

## Data Exploration

ggplot(dat = Customertravel) +
geom_bar(mapping = aes(x= Age, fill=Target), position = "dodge") +
  ggtitle()

  


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
d<-ggplot(Customertravel, aes(x=Age, y=Target))
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

##Customertravel %>% 
##  count(Age)
##breaks <- c(27,28,29,30,31,33,34,35, 36,37,38,39)
##AgeGroups <- c("[27-29)", "[30-31)", "[33-34)", "[35-36)", "[37-38)")
##ggplot (Customertravel = as_tibble((group_tags)))
##summary(AgeGroups)

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

## Finding Age and Target ( Churn)
ggplot(dat = Customertravel) +
  geom_bar(mapping = aes(x= Age, fill=ServiceOpted), position = "dodge") +
  ggtitle("Which Age group Churn the most?
          0=Customer not Churn & 1= Customer Churn")

## HighIncome filtered to compare with other variables
CT_HighIncome<- filter(Customertravel, AnnualIncomeClass == "High Income")
head(CT_HighIncome)


ggplot(CT_HighIncome) + geom_bar(aes(x = Age, fill= Target)) +
  ylab("Count") + ggtitle("High Income by Age and Target")

ggplot(CT_HighIncome) + geom_bar(aes(x = FrequentFlyer, fill= Target)) +
  ylab("Count") + ggtitle("High Income by FrequentFlyer and Target")
##All high income clients are frequent flyers! There is a possibility that is only used for business which is why they only used one service.

ggplot(CT_HighIncome) + geom_bar(aes(x = ServicesOpted, fill= Target)) +
  ylab("Count") + ggtitle("High Income by ServicesOpted and Target")
##I can see here that clients with a higher income picked only one service and that half of it still Churned.We don't know the time frame that this data was taken. What I can assume is that probably the clients that used more 
## service received a better deal then decided to opted out because didn't really see an used.

ggplot(CT_HighIncome) + geom_bar(aes(x = AccountSyncedToSocialMedia, fill= Target)) +
  ylab("Count") + ggtitle("High Income by Accounts Synched to Social Media and Target")

##Middle Income filtered to compare with other variables.
CT_MiddleIncome<- filter(Customertravel, AnnualIncomeClass == "Middle Income")
head(CT_MiddleIncome)

ggplot(CT_MiddleIncome) + geom_bar(aes(x = Age, fill= Target)) +
  ylab("Count") + ggtitle("Middle Income by Age and Target")
##Middle Income Clients didn't Churn 

ggplot(CT_MiddleIncome) + geom_bar(aes(x = FrequentFlyer, fill= Target)) +
  ylab("Count") + ggtitle("Middle Income by FrequentFlyer and Target")

ggplot(CT_MiddleIncome) + geom_bar(aes(x = Age, fill= FrequentFlyer)) +
  ylab("Count") + ggtitle("Middle Income by FrequentFlyer and Target")

ggplot(CT_MiddleIncome) + geom_bar(aes(x = ServicesOpted, fill= Target)) +
  ylab("Count") + ggtitle("Middle Income by ServicesOpted and Target")

ggplot(CT_MiddleIncome) + geom_bar(aes(x = AccountSyncedToSocialMedia, fill= Target)) +
  ylab("Count") + ggtitle("Middle Income by Accounts Synched to Social Media and Target")
##Accounts not being synched to social media did not impact clients staying with the company. 

ggplot(CT_MiddleIncome) + geom_bar(aes(x = Age, fill= AccountSyncedToSocialMedia )) +
  ylab("Count") + ggtitle("Middle Income by AccountSyncedToSocialMedia and Target")

## Low Income filtered to cpmapre with other variables
CT_LowIncome<- filter(Customertravel, AnnualIncomeClass == "Low Income")
head(CT_LowIncome)

ggplot(CT_LowIncome) + geom_bar(aes(x = Age, fill= Target)) +
  ylab("Count") + ggtitle("Low Income by Age and Target")


ggplot(Customertravel, aes(sample = Age)) + geom_qq()

ggplot(Customertravel, aes(sample = ServicesOpted)) + geom_qq()

##Chi- Square to test Relationship between Income and Target
CrossTable(Customertravel$AnnualIncomeClass, Customertravel$Target, fisher=TRUE
           , chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
