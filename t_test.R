#download and open data
# https://github.com/statsoffnow/healthcare

library(readr)
health <- read_csv("healthcare-dataset-stroke-data.csv")

#load packages
library(ggplot2)
library(ggpubr)
library(dplyr)



#get rid of unnecessary variables 
health_1 <- 
health %>% 
  select(gender, bmi)

health_1

#summarize data
summary(health_1)

#change gender to factor
health_1$gender <- as.factor(health_1$gender)
health_1$gender

#change BMI to numeric variable
health_1$bmi <- as.numeric(health_1$bmi)
health_1$bmi


#count female and male participants
health_1 %>% 
  count(gender)

#remove 1 individual identified as Other
health_2 <-
  health_1 %>% 
  select(everything()) %>% 
  filter(gender != 'Other')   # !=  is not equal


#build histograms for bmi
ggplot(data = health_2,
       aes(x=bmi))+
  geom_histogram()+          #to change number of bins add (bins = xxx)
  facet_wrap(.~gender)


#normality test
health_2 %>% 
  group_by(gender) %>% 
  summarise('SW p-value' = shapiro.test(bmi)$p.value)

format(5.49e-29, scientific=FALSE)  #change scientific format to regular

#check variance equality
library(car)

leveneTest(data = health_2,
           bmi~gender)        #p < 0.05 means that variances are not equal


# Compare two groups with unequal variances
t.test(data = health_2,
       bmi~gender)


# You can also set equal variances
t.test(data = health_2,
       bmi~gender,
       var.equal=TRUE)      #add var.equal=TRUE



#boxplot for groups
ggboxplot(data = health_2,
          y = 'bmi',
          x = 'gender',
          color = 'gender',
          palette = 'jco')+     #change colors using palette
  stat_compare_means(method = 't.test', 
                     label.y = 100, 
                     label.x = 0.5)


#You can also omit to include the test name and use only significance level
ggboxplot(data = health_2,
          y = 'bmi',
          x = 'gender',
          color = 'gender',
          palette = 'jco')+
  stat_compare_means(method = 't.test', 
                     label.y = 90, 
                     label.x = 0.5,
                     aes(label = paste0("p = ", ..p.signif..)))

