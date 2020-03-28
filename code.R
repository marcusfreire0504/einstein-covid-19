library(tidyverse)
theme_set(theme_bw())
library(naniar)
library(readxl)
library(caret)

dataset <- ?read_excel(path = "data/dataset.xlsx", )

#################################
### exploratory data analysis ###
#################################

# first look at the dataset

glimpse(dataset)

# remove columns that won't help on diagnosis

dataset_clean <- dataset %>%
  select(-`Patient ID`, 
         -`Patient addmited to regular ward (1=yes, 0=no)`,
         -`Patient addmited to semi-intensive unit (1=yes, 0=no)`,
         -`Patient addmited to intensive care unit (1=yes, 0=no)`)

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

n <- 500

columns <- names(dataset_clean)[which(missing_values$num.missing >= dim(dataset_clean)[1]-n)]

dataset_model <- dataset_clean %>%
  select(all_of(columns))

# remove variables with variance equal to zero 

dataset_model_num <- dataset_model %>%
  select_if(is.numeric)

dataset_model_num[, -which(apply(dataset_model_num, 2, var, na.rm = TRUE) == 0)]

# remove variables with only one level 

dataset_model_cat <- dataset_model %>%
  select_if(negate(is.numeric)) %>%
  mutate_all(as.factor)

dataset_model_cat <- dataset_model_cat[, sapply(dataset_model_cat, nlevels) > 1]
  
# final dataset

dataset_model <- cbind(dataset_model_num, dataset_model_cat)



################
### modeling ###
################

# train/test split

covid <- dataset_model

set.seed(1)

index       <- createDataPartition(covid$`SARS-Cov-2 exam result`, 
                                   p = 0.75, 
                                   list = FALSE)
covid_train <- covid[ index, ]
covid_test  <- covid[-index, ]

dim(covid_train)
table(covid_train$`SARS-Cov-2 exam result`)

dim(covid_test)
table(covid_test$`SARS-Cov-2 exam result`)

# parameters for cart

fitControl <- trainControl(method = "cv",
                           number = 5,
                           savePred = TRUE, 
                           classProb = TRUE)

#tune.grid <- expand.grid(maxdepth = )

set.seed(1)

x <- covid_train %>%
  select(-`SARS-Cov-2 exam result`)

y <- covid_train %>%
  select(`SARS-Cov-2 exam result`) %>%
  unlist()

covid_rpart <- train(x, y,
                     method = "ctree", 
                     #tuneGrid = tune.grid,
                     trControl = fitControl,
                     controls=ctree_control(maxsurrogate=2))

covid_rpart


