install.packages('tidyverse')
install.packages('caret')
library(tidyverse)
library(caret)

df1<- read_excel("Amazon Data.xlsx")
df1$Center<- ifelse(df1$Center=="Yes",1,0)
summary(df1)
colnames(df1)

hist(df1$`Median Household Income`)
hist(df1$Population)
hist(df1$`Households 2016-2020`)
hist(df1$`High School Graduate`)
hist(df1$`Bachelors Degree Graduate`)
hist(df1$`People of Color`)

df1$income<- as.numeric(df1$`Median Household Income`)
df1$households<- as.numeric(df1$`Households 2016-2020`)
df1$pop<-log10(df1$Population)
df1$households<-log10(df1$households)
df1$hsg<-log10(df1$`High School Graduate`)
df1$bdg<-log10(df1$`Bachelors Degree Graduate`)
df1$poc<-log10(df1$`People of Color`)

hist(df1$income)
hist(df1$pop)
hist(df1$households)
hist(df1$hsg)
hist(df1$bdg)
hist(df1$poc)
hist(df1$realestate_taxes)
hist(df1$Electricity_Rate)

df1a<- df1[,c(-1:-6,-8:-9)]
colnames(df1a)

library(corrplot)
library(RColorBrewer)
df1_corr <-cor(df1a)
corrplot(df1_corr, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

mylogit1<- glm(Center ~ ., data = df1a, family = "binomial")
summary(mylogit1)

mylogit <- glm(df1a$Center ~ df1a$hsg+df1a$bdg+df1a$income ,family = "binomial")
summary(mylogit)               

res<-resid(mylogit)
summary(res)
sd(res)
hist(res)
plot(hsg,res)
abline(0,0, col="red")
qqnorm(res,col="red")
qqline(res)
plot(fitted(mylogit), res)
abline(0,0)