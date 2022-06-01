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

## According to the data customers are not Frequent Flyers. With the most ranging at 30yrs old. 

##2- What are the most relevant income among customers?
ggplot(data = Customertravel) +
  geom_bar(mapping = aes(x= Age, fill=AnnualIncomeClass), position = "dodge")

## Looks that the majority of clients are low and middle income class. 

##3- Does the annual income class influence churn?
##0= Customer doesn't Churn  1= Customer Churn
ggplot(data = Customertravel) +
  geom_bar(mapping = aes(x= Target, fill=AnnualIncomeClass), position = "dodge")
## Middle income seem to be the highest income not churning, followed by low income.

## Normally Distributed
ggplot(Customertravel, aes(sample = Age)) + geom_qq()
