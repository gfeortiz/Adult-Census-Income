######################################################################
################### CHOOSE YOUR OWN PROJECT ##########################
######################################################################

###################################
#### LOADING THE REQUIRED PACKAGES
###################################

if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(fastDummies)) install.packages("fastDummies")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(broom)) install.packages("broom")

library(tidyverse)
library(caret)
library(fastDummies)
library(gridExtra)
library(broom)




###################################
#### LOADING THE DATA 
###################################

# Note: this process could take a couple of minutes

adult <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"),
         header=FALSE, col.names = c("age", "workclass", "fnlwgt","education", "education.num", 
                                     "marital.status", "occupation", "relationship", "race", 
                                     "sex", "capital.gain", "capital.loss", "hours.per.week", 
                                     "native.country", "income"))

adult.validation <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test"),
                  header=FALSE, col.names = c("age", "workclass", "fnlwgt","education", "education.num", 
                                              "marital.status", "occupation", "relationship", "race", 
                                              "sex", "capital.gain", "capital.loss", "hours.per.week", 
                                              "native.country", "income"))

adult.validation <- adult.validation[-1,] %>% mutate(age=as.integer(age))



###################################
#### EXPLORING THE DATA 
###################################

# Summary of dataset
summary(adult)

# Barplot for each variable in dataset
for (i in 1:15) {
  table(adult[i]) %>% barplot(las=2, main=colnames(adult)[i])
}

## Relationships among variables and income

# Age
adult %>% 
  mutate(x = round(age)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(prop = mean(income == " >50K")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  ggtitle("Proportion of people with income above 50K",
          "By age") +
  labs(x="Age", y="Prop. of >50K")

# Workclass
adult %>% 
  mutate(x = workclass) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(prop = mean(income == " >50K")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  ggtitle("Proportion of people with income above 50K",
          "By workclass") +
  labs(x="Workclass", y="Prop. of >50K") + 
  theme(axis.text.x = element_text(angle = 45))

# Education
adult %>% 
  mutate(x = education) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(prop = mean(income == " >50K")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  ggtitle("Proportion of people with income above 50K",
          "By education") +
  labs(x="Education", y="Prop. of >50K") + 
  theme(axis.text.x = element_text(angle = 45))

# Years of education
adult %>% 
  mutate(x = round(education.num)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(prop = mean(income == " >50K")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  ggtitle("Proportion of people with income above 50K",
                         "By years of education") +
  labs(x="Years of education", y="Prop. of >50K")

# Marital status
adult %>% 
  mutate(x = marital.status) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(prop = mean(income == " >50K")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  ggtitle("Proportion of people with income above 50K",
          "By marital status") +
  labs(x="Marital status", y="Prop. of >50K") + 
  theme(axis.text.x = element_text(angle = 45))

# Occupation
adult %>% 
  mutate(x = occupation) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(prop = mean(income == " >50K")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  ggtitle("Proportion of people with income above 50K",
          "By occupation") +
  labs(x="Occupation", y="Prop. of >50K") + 
  theme(axis.text.x = element_text(angle = 45))

# Relationship
adult %>% 
  mutate(x = relationship) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(prop = mean(income == " >50K")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  ggtitle("Proportion of people with income above 50K",
          "By relationship") +
  labs(x="Relationship", y="Prop. of >50K") + 
  theme(axis.text.x = element_text(angle = 45))

# Race
adult %>% 
  mutate(x = race) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(prop = mean(income == " >50K")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  ggtitle("Proportion of people with income above 50K",
          "By race") +
  labs(x="Race", y="Prop. of >50K") + 
  theme(axis.text.x = element_text(angle = 45))

# Sex
adult %>% 
  mutate(x = sex) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(prop = mean(income == " >50K")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  ggtitle("Proportion of people with income above 50K",
          "By sex") +
  labs(x="Sex", y="Prop. of >50K") + 
  theme(axis.text.x = element_text(angle = 45))

# Capital gains > 0
adult %>% 
  mutate(x = capital.gain>0) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(prop = mean(income == " >50K")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  ggtitle("Proportion of people with income above 50K",
          "By capital gain") +
  labs(x="Capital gain", y="Prop. of >50K") + 
  theme(axis.text.x = element_text(angle = 45))

# Capital loss > 0
adult %>% 
  mutate(x = capital.loss>0) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(prop = mean(income == " >50K")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  ggtitle("Proportion of people with income above 50K",
          "By capital loss") +
  labs(x="Capital loss", y="Prop. of >50K") + 
  theme(axis.text.x = element_text(angle = 45))

# Hours per week
adult %>% 
  mutate(x = round(hours.per.week)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(prop = mean(income == " >50K")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  ggtitle("Proportion of people with income above 50K",
          "By hours per week") +
  labs(x="Hours per week", y="Prop. of >50K")

# Native Country
adult %>% 
  mutate(x = native.country) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(prop = mean(income == " >50K")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  ggtitle("Proportion of people with income above 50K",
          "By native country") +
  labs(x="Native country", y="Prop. of >50K") + 
  theme(axis.text.x = element_text(angle = 45))




###################################
#### PREPARING DATA FOR PREDICTION
###################################

# Triming string data

adult <- adult %>% mutate(
  workclass = str_trim(workclass),
  education = str_trim(education),
  marital.status = str_trim(marital.status),
  occupation = str_trim(occupation),
  relationship = str_trim(relationship),
  race = str_trim(race),
  sex = str_trim(sex),
  native.country = str_trim(native.country),
  income = str_trim(income)
)

# Replacing all hyphens with underscore

adult <- adult %>% mutate(
  workclass = str_replace_all(workclass, "-", "_"),
  education = str_replace_all(education, "-", "_"),
  marital.status = str_replace_all(marital.status, "-", "_"),
  occupation = str_replace_all(occupation, "-", "_"),
  relationship = str_replace_all(relationship, "-", "_"),
  race = str_replace_all(race, "-", "_"),
  sex = str_replace_all(sex, "-", "_"),
  native.country = str_replace_all(native.country, "-", "_"),
  income = str_replace_all(income, "-", "_")
)


# Training set and test set

set.seed(1)

test_index <- createDataPartition(y = adult$income, times = 1, p = 0.2, 
                                  list = FALSE)

train <- adult[-test_index,]
test <- adult[test_index,]

rm(test_index)


###################################
#### METHODS & ANALYSIS
###################################

# Informed logistic regression

train1 <- train %>% mutate(
  agesq = age^2,
  education.numsq = education.num^2,
  married=ifelse(marital.status=="Married_civ_spouse"|
                   marital.status=="Married_AF_spouse", 1, 0),
  whiteasian=ifelse(race=="White"|
                      race=="Asian_Pac_Islander", 1, 0),
  male=ifelse(sex=="Male", 1, 0),
  hours.per.weeksq=hours.per.week^2,
  hours.per.weekcub=hours.per.week^3
)

test1 <- test %>% mutate(
  agesq = age^2,
  education.numsq = education.num^2,
  married=ifelse(marital.status=="Married_civ_spouse"|
                   marital.status=="Married_AF_spouse", 1, 0),
  whiteasian=ifelse(race=="White"|
                      race=="Asian_Pac_Islander", 1, 0),
  male=ifelse(sex=="Male", 1, 0),
  hours.per.weeksq=hours.per.week^2,
  hours.per.weekcub=hours.per.week^3
)


fit_glm1 <- train1 %>% mutate(y=as.numeric(income==">50K")) %>%
  glm(y ~ age + agesq + education.num + education.numsq +
        married + whiteasian + capital.gain + capital.loss + 
        male + hours.per.week + hours.per.weeksq + 
        hours.per.weekcub,
      data=., family = "binomial")

summary(fit_glm1)

p_hat_glm1 <- predict(fit_glm1, test1, type="response")

y_hat_glm1 <- factor(ifelse(p_hat_glm1 > 0.5, ">50K", "<=50K"))

confusionMatrix(y_hat_glm1, factor(test1$income))



# Logistic regression using all information on the dataset

train2 <- dummy_cols(train, select_columns = c("workclass", "education",
                                               "marital.status", "occupation",
                                               "relationship", "race", "sex", "native.country",
                                               "income"),
                     remove_first_dummy = TRUE) %>%
  select(-workclass, -fnlwgt, -education, -marital.status,
         -occupation, -relationship, -race, -sex,-native.country,
         -income)

test2 <- dummy_cols(test, select_columns = c("workclass", "education",
                                               "marital.status", "occupation",
                                               "relationship", "race", "sex", "native.country",
                                               "income"),
                     remove_first_dummy = TRUE) %>%
  select(-workclass, -fnlwgt, -education, -marital.status,
         -occupation, -relationship, -race, -sex, -native.country,
         -income)

train2 <- train2 %>% select(-`native.country_Holand_Netherlands`)

fit_glm2 <- train2 %>% glm(`income_>50K` ~ ., data=., family = "binomial")

summary(fit_glm2)

p_hat_glm2 <- predict(fit_glm2, test2, type="response")

y_hat_glm2 <- factor(ifelse(p_hat_glm2 > 0.5, 1, 0))

confusionMatrix(y_hat_glm2, factor(test2$`income_>50K`))



# Combination of both (using variables squared and cubed)

train3 <- train2 %>% mutate(
  agesq = age^2,
  education.numsq = education.num^2,
  hours.per.weeksq=hours.per.week^2,
  hours.per.weekcub=hours.per.week^3
)


test3 <- test2 %>% mutate(
  agesq = age^2,
  education.numsq = education.num^2,
  hours.per.weeksq=hours.per.week^2,
  hours.per.weekcub=hours.per.week^3
)

fit_glm3 <- train3 %>% glm(`income_>50K` ~ ., data=., family = "binomial")

summary(fit_glm3)

p_hat_glm3 <- predict(fit_glm3, test3, type="response")

y_hat_glm3 <- factor(ifelse(p_hat_glm3 > 0.5, 1, 0))

confusionMatrix(y_hat_glm3, factor(test3$`income_>50K`))



###################################
#### VALIDATION & RESULTS
###################################

# Run regression

adult <- dummy_cols(adult, select_columns = c("workclass", "education",
                                               "marital.status", "occupation",
                                               "relationship", "race", "sex", "native.country",
                                               "income"),
                     remove_first_dummy = TRUE) %>%
  select(-workclass, -fnlwgt, -education, -marital.status,
         -occupation, -relationship, -race, -sex,-native.country,
         -income) %>% 
  mutate(agesq = age^2,
         education.numsq = education.num^2,
         hours.per.weeksq=hours.per.week^2,
         hours.per.weekcub=hours.per.week^3) %>%
  select(-native.country_Holand_Netherlands)



fit_final <- adult %>% glm(`income_>50K` ~ ., data=., family = "binomial")

summary(fit_final)


#### Preparing validation data

# Triming string data

adult.validation <- adult.validation %>% mutate(
  workclass = str_trim(workclass),
  education = str_trim(education),
  marital.status = str_trim(marital.status),
  occupation = str_trim(occupation),
  relationship = str_trim(relationship),
  race = str_trim(race),
  sex = str_trim(sex),
  native.country = str_trim(native.country),
  income = str_trim(income)
)

# Replacing all dots and hyphens with underscore

adult.validation <- adult.validation %>% mutate(
  workclass = str_replace_all(workclass, "-", "_"),
  education = str_replace_all(education, "-", "_"),
  marital.status = str_replace_all(marital.status, "-", "_"),
  occupation = str_replace_all(occupation, "-", "_"),
  relationship = str_replace_all(relationship, "-", "_"),
  race = str_replace_all(race, "-", "_"),
  sex = str_replace_all(sex, "-", "_"),
  native.country = str_replace_all(native.country, "-", "_"),
  income = str_replace_all(income, "-", "_")
)


# Adding relevant variables

adult.validation <- dummy_cols(adult.validation, select_columns = c("workclass", "education",
                                              "marital.status", "occupation",
                                              "relationship", "race", "sex", "native.country",
                                              "income"),
                    remove_first_dummy = TRUE) %>%
  select(-workclass, -fnlwgt, -education, -marital.status,
         -occupation, -relationship, -race, -sex,-native.country,
         -income) %>% 
  mutate(agesq = age^2,
         education.numsq = education.num^2,
         hours.per.weeksq=hours.per.week^2,
         hours.per.weekcub=hours.per.week^3)

p_hat_final <- predict(fit_final, adult.validation, type="response")

y_hat_final <- factor(ifelse(p_hat_final > 0.5, 1, 0))

confusionMatrix(y_hat_final, factor(adult.validation$`income_>50K`))



