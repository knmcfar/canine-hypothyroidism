library(tidyverse)
library(rigr)
library(knitr)

hypo <- read.csv("hypothyroid_study.csv")

#descriptive information--------------------------------------------------------

#get number of dogs in study
dim(hypo)

#confirm all dogs are male
table(hypo$female)

#descriptive statistics: table 1
hypo %>% 
  filter(case == 1) %>%
  descrip()

hypo %>% 
  filter(case == 0) %>% 
  descrip()

hypo %>% 
  filter(case == 1) %>% 
  group_by(breed) %>% 
  summarise(cases = n(), prop = n()/table(hypo$case)[2])

hypo %>% 
  filter(case==0) %>% 
  group_by(breed) %>% 
  summarise(cases = n(), prop = n()/table(hypo$case)[1])

#relevel breed variable to change the reference group
hypo$breed <- as.factor(hypo$breed)
hypo$breed <- relevel(hypo$breed, ref = "mixed")

#multiple logistic regression with case as disease, neuter as exposure
#adjusted for age and breed
reg1 <- regress("odds", case ~ neuter + age + breed, data = hypo)

#regression estimates: table 2
tab1 <- round(coef(reg1)[ , c(4, 3, 5, 6)],2)
kable(tab1)
