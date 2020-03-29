library(tidyverse)
theme_set(theme_bw())
library(naniar)
library(readxl)
library(caret)
library(reshape2)
library(GGally)
library(mice)

dataset <- read_excel(path = "data/dataset.xlsx")

# fix column names

names(dataset) <- make.names(names(dataset), unique = TRUE)

#################################
### exploratory data analysis ###
#################################

# first look at the dataset

glimpse(dataset)

# remove columns that won't help on diagnosis

dataset_clean <- dataset %>%
  select(-Patient.ID, 
         -Patient.addmited.to.regular.ward..1.yes..0.no.,
         -Patient.addmited.to.semi.intensive.unit..1.yes..0.no.,
         -Patient.addmited.to.intensive.care.unit..1.yes..0.no.)

# convert level Urine...Leukocytes <1000 to 1000

dataset_clean$Urine...Leukocytes[dataset_clean$`Urine...Leukocytes` == "<1000"] <- 1000
dataset_clean$`Urine...Leukocytes` <- as.numeric(dataset_clean$`Urine...Leukocytes`)

# fix Urine...pH

dataset_clean$`Urine...pH`[dataset_clean$`Urine...pH` == "Não Realizado"] <- NA
dataset_clean$`Urine...pH` <- as.numeric(dataset_clean$`Urine...pH`)

# Urine...Hemoglobin

dataset_clean$`Urine...Hemoglobin`[dataset_clean$`Urine...Hemoglobin` == "not_done"] <- NA

# Urine...Aspect

dataset_clean$`Urine...Aspect` <- factor(dataset_clean$`Urine...Aspect`, 
                                         levels = c("clear", "lightly_cloudy", "cloudy", "altered_coloring"))

# Strepto A

dataset_clean$`Strepto.A`[dataset_clean$`Strepto.A` == "not_done"] <- NA

# transform character to factor

dataset_clean_num <- dataset_clean %>%
  select_if(is.numeric)

dataset_clean_cat <- dataset_clean %>%
  select_if(negate(is.numeric)) %>%
  mutate_all(as.factor)

dataset_clean <- base::cbind(dataset_clean_num, dataset_clean_cat)

# fix factor levels

# sort(sapply(dataset_clean[,sapply(dataset_clean, is.factor)], nlevels))

# let's take a look on missing data

missing_values <- dataset_clean %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing == TRUE) %>%
  select(-is.missing) %>%
  ungroup() %>%
  mutate(key = reorder(key, -num.missing)) %>%
  arrange(desc(num.missing)) %>%
  print(n = Inf)
  
missing_values %>%
  ggplot() +
  geom_bar(aes(x = key, y = 100*num.missing/dim(dataset_clean)[1]), stat = "identity") +
  labs(x = "Variable", y="Percent of missing values") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

vis_miss(dataset_clean) +
  theme(axis.text.x = element_text(size = 6))

# keep only columns with at least n observations

n <- 1000

dataset_clean <- dataset_clean[, which(dim(dataset_clean)[1] - apply(apply(dataset_clean, 2, is.na), 2, sum) >= n)]

# remove quantitative variables with variance equal to zero 

dataset_model_num <- dataset_clean %>%
  select_if(is.numeric)

if (sum(apply(dataset_model_num, 2, var, na.rm = TRUE) == 0) != 0) {
  dataset_model_num <- dataset_model_num[, which(apply(dataset_model_num, 2, var, na.rm = TRUE) == 0)]
}

# remove categorical variables with only one level 

dataset_model_cat <- dataset_clean %>%
  select_if(negate(is.numeric))

dataset_model_cat <- dataset_model_cat[, sapply(dataset_model_cat, nlevels) > 1]
  
# final dataset

dataset_model <- base::cbind(dataset_model_num, dataset_model_cat)

vis_miss(dataset_model) + # it needs naniar package
  theme(axis.text.x = element_text(size = 6))

# some other plots

ggpairs(dataset_model[, c(1:10)])

ggpairs(dataset_model[, c(11:ncol(dataset_model))])

# it seems there is no relation between any pair of variables



################
### modeling ###
################

# train/test split

covid <- dataset_model

set.seed(1)

index       <- createDataPartition(covid$SARS.Cov.2.exam.result, 
                                   p = 0.75, 
                                   list = FALSE)
covid_train <- covid[ index, ]
covid_test  <- covid[-index, ]

dim(covid_train)
table(covid_train$SARS.Cov.2.exam.result)

dim(covid_test)
table(covid_test$SARS.Cov.2.exam.result)

# parameters for cart

fitControl <- trainControl(method = "cv",
                           number = 5,
                           savePred = TRUE, 
                           classProb = TRUE)

tune.grid <- expand.grid(mincriterion = seq(from = 0.01, 
                                            to = .99, 
                                            by = 0.01))

set.seed(1)

x <- covid_train %>%
  select(-SARS.Cov.2.exam.result)

y <- covid_train %>%
  select(SARS.Cov.2.exam.result) %>%
  unlist()

covid_ctree <- train(x, y,
                     method = "ctree", 
                     tuneGrid = tune.grid,
                     trControl = fitControl)

ggplot(covid_ctree)

prediction <- predict(covid_ctree, covid_test)

confusionMatrix(prediction, covid_test$SARS.Cov.2.exam.result)

# 90% accuracy seems good, but No Information Rate is also 90%
# so, using this model or a random process gives the same answer
# high sensitivity, but very low specificity :(



#######################
### data imputation ###
#######################

covid_imp <- mice(covid, meth = "rf", ntree = 5) # be patient

covid <- complete(covid_imp)

################
### modeling ###
################

# train/test split

set.seed(1)

index       <- createDataPartition(covid$SARS.Cov.2.exam.result, 
                                   p = 0.75, 
                                   list = FALSE)
covid_train <- covid[ index, ]
covid_test  <- covid[-index, ]

dim(covid_train)
table(covid_train$SARS.Cov.2.exam.result)

dim(covid_test)
table(covid_test$SARS.Cov.2.exam.result)

# parameters for random forest

fitControl <- trainControl(method = "cv",
                           number = 5,
                           savePred = TRUE, 
                           classProb = TRUE)

tune.grid <- expand.grid(mtry = 1:35)

set.seed(1)

x <- covid_train %>%
  select(-SARS.Cov.2.exam.result)

y <- covid_train %>%
  select(SARS.Cov.2.exam.result) %>%
  unlist()

covid_rf <- train(x, y,
                  method = "rf", 
                  tuneGrid = tune.grid,
                  trControl = fitControl)

ggplot(covid_rf)

# high sensitivity, but very low specificity :(

