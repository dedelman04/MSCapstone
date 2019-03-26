library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)
library(grid)
library(caret)
library(Matrix)
library(rpart)

#path <- "/Users/edelmans/Documents/MSCapstone/"
path <- "C:/users/dedelman/desktop/capstone/"
train_df <- read.csv(file=paste0(path,"train_values.csv"),
                     header=TRUE,
                     stringsAsFactors = TRUE)

train_label <- read.csv(file=paste0(path,"train_labels.csv"),
                        header=TRUE,
                        stringsAsFactors = TRUE)

train_data <- merge(x=train_df, y=train_label, by="row_id")

#Store the high, low, mean and median values for later use
low_mort <- as.integer(min(train_data$heart_disease_mortality_per_100k))
high_mort <- as.integer(max(train_data$heart_disease_mortality_per_100k))
hd_mean = mean(train_data$heart_disease_mortality_per_100k)
hd_med = median(train_data$heart_disease_mortality_per_100k)

#Rename a typo in the data set
train_data <- train_data %>% 
  mutate(health__pct_physical_inactivity = health__pct_physical_inacticity) %>%
  select(-health__pct_physical_inacticity)

#NAs per feature
NAs <- data.frame(
  features = colnames(train_data %>% select(-heart_disease_mortality_per_100k, -yr, -row_id)),
  NA_pct = 
    as.numeric(format(sapply(train_data %>%
                               select(-heart_disease_mortality_per_100k,
                                      -yr, -row_id),
                             function(x){mean(ifelse(is.na(x), 1, 0))},
                             simplify=TRUE)*100, digits=3) )
) 

#Remove high NA features
high_NA <- NAs %>% 
  filter(NA_pct > 10) %>% 
  .$features %>% 
  as.character()

train_data <- train_data %>% select(-high_NA)

#Which features have minimum number of NAs (> 0)
min_missing <- min(NAs %>% filter(NA_pct > 0) %>% select(NA_pct))  
NA2 <- sapply(train_data %>% 
                select(as.vector(NAs$features[NAs$NA_pct == min_missing])),
              function(x){which(is.na(x))}, simplify=TRUE)

train_data <- train_data[-t(NA2)[1,c(1:2)], ]

#Rename original levels to show on plots
levels(train_data$area__rucc) <- c("Metro - 1 Million +",
                                   "Metro - 250,000 to 1 Mil",
                                   "Metro - less than 250,000",
                                   "Nonmetro - Rural or < 2.5k, adjacent",
                                   "Nonmetro - Rural or < 2.5k, non-adjacent",
                                   "Nonmetro - 2,500 to 19,999, adjacent",
                                   "Nonmetro - 2,500 to 19,999, non-adjacent",
                                   "Nonmetro - 20,000 or more, adjacent",
                                   "Nonmetro - 20,000 or more, non-adjacent")

#Add "population" column using mapping
#Note - 2 different patterns map to 20-250k
train_data <- train_data %>% 
  mutate(population = ifelse(like(train_data$area__rucc, "19,999"), "2.5-20k",
                             ifelse(like(train_data$area__rucc, "2.5k"), "under 2.5k",
                                    ifelse(like(train_data$area__rucc, "20,000"), "20-250k",
                                           ifelse(like(train_data$area__rucc, "fewer than 250,000"), "20-250k",
                                                  ifelse(like(train_data$area__rucc, "250,000"), "250k-1M",
                                                         "1M+"))))))

#Force population into a factor
train_data$population <- factor(train_data$population, 
                                levels=c("under 2.5k",
                                         "2.5-20k",
                                         "20-250k",
                                         "250k-1M", "1M+"))

#Group into metro/non-metro
train_data <- train_data %>% 
  mutate(metro = ifelse(like(area__rucc, "Nonmetro"), 
                        "Nonmetro", "Metro"))

#Since Metro is below mean, Nonmetro above mean, replace with 1, -1
#train_data$metro <- train_data %>% mutate(metro = ifelse(like(metro, "Nonmetro"), -1, 1))

#Find median, replace NA values with median of air polltion across all population
ap_med <- median(train_data$health__air_pollution_particulate_matter, na.rm=TRUE)
ap_NA <- which(is.na(train_data$health__air_pollution_particulate_matter))

train_data$health__air_pollution_particulate_matter[ap_NA] <- ap_med

#Histogram for air pollution regrouped
cut_labels <- c("<= 10", "11", "12", "13", "14+")
cut_levels <- c(0, 10.9, 11.9, 12.9, 13.9, 100)

train_data$air_pollution <- cut(train_data$health__air_pollution_particulate_matter, cut_levels)
levels(train_data$air_pollution) <- cut_labels

#Which features have a NA % between 1 and 10
na_mid <- NAs[NAs$NA_pct > 1 & NAs$NA_pct < 10, ]

#Replace the NAs with the median for each feature
mid_cols <- as.vector(na_mid[, 1])

train_data <- train_data %>%
  mutate_at(mid_cols, ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))

train_data <- train_data %>% 
  mutate(p_diab_obese = health__pct_adult_obesity * health__pct_diabetes) %>%
  mutate(p_obese_inact = health__pct_adult_obesity * health__pct_physical_inactivity) %>%
  mutate(p_diab_inact = health__pct_diabetes * health__pct_physical_inactivity) %>%
  mutate(p_all_three = health__pct_adult_obesity * health__pct_diabetes * health__pct_physical_inactivity )

#Remove unused columns from the data set
model_cols <- c("econ__economic_typology",
                "population",
#                "metro",
                "air_pollution",
                "econ__pct_civilian_labor",
                "demo__pct_non_hispanic_african_american",
                "demo__death_rate_per_1k",
                "demo__pct_adults_bachelors_or_higher",
                "demo__pct_adults_less_than_a_high_school_diploma",
                "p_diab_obese",
                "p_obese_inact",
                "p_diab_inact",
                "p_all_three")

train_data <- train_data %>% select(model_cols, heart_disease_mortality_per_100k)

#Store the names of continuous and categorical columns
#Continuous
num_cols <- train_data %>% 
  select(-heart_disease_mortality_per_100k) %>%
  .[sapply(., is.numeric)] %>% 
  colnames()

#Categorical columns
cat_cols <- colnames(train_data)[sapply(train_data, is.factor)]

#Normalize the continuous variables
 #Using scale
data_scale <- scale(train_data[num_cols])

#as.data.frame(data_scale) %>%
#  gather(feature, value) %>%
#  ggplot(aes(x=value)) + geom_histogram(bins=50) +
#  facet_wrap(~ feature, scales="free_y")+ggtitle("Scale")

 #Using cube root
data_scale <- sapply(train_data[num_cols], function(x) {x^(1/3)}, simplify=TRUE)

#as.data.frame(data_scale) %>%
#  gather(feature, value) %>%
#  ggplot(aes(x=value)) + geom_histogram(bins=50) +
#  facet_wrap(~ feature, scales="free_y")+ggtitle("Cube root")

 #Using square root
data_scale <- sapply(train_data[num_cols], function(x) {sqrt(x)}, simplify=TRUE)

#as.data.frame(data_scale) %>%
#  gather(feature, value) %>%
#  ggplot(aes(x=value)) + geom_histogram(bins=50) +
#  facet_wrap(~ feature, scales="free_y")+ggtitle("Square root")

 #Using log 10
data_scale <- sapply(train_data[num_cols], function(x) {log10(x)}, simplify=TRUE)

#as.data.frame(data_scale) %>%
#  gather(feature, value) %>%
#  ggplot(aes(x=value)) + geom_histogram(bins=50) +
#  facet_wrap(~ feature, scales="free_y")+ggtitle("Log 10")

 #Using ln
data_scale <- sapply(train_data[num_cols], function(x) {log(x)}, simplify=TRUE)

#as.data.frame(data_scale) %>%
#  gather(feature, value) %>%
#  ggplot(aes(x=value)) + geom_histogram(bins=50) +
#  facet_wrap(~ feature, scales="free_y")+ggtitle("Ln")

#Create new DF with the scaled data
model_data <- cbind(train_data %>% select(heart_disease_mortality_per_100k, cat_cols),
                                   sapply(train_data[num_cols], scale, simplify=TRUE))

#model_data <- train_data

#####Modelling#####

#Linear regression model
lm_form <- as.formula(paste("heart_disease_mortality_per_100k ~ ", paste(model_cols, collapse="+")))

set.seed(999)
test_index <- createDataPartition(model_data$heart_disease_mortality_per_100k, times = 1, p=0.5, list=FALSE)
train_set <- model_data[-test_index, ] 
test_set <- model_data[test_index, ]

fit <- lm(formula = lm_form, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat-test_set$heart_disease_mortality_per_100k)^2, na.rm=TRUE))

#Cross-validation LM
x <- model_data %>% select(model_cols)
y <- model_data$heart_disease_mortality_per_100k
control <- trainControl(method="cv", number=100, p=.9)

lm_fit <- train(x, y, method="lm", metric="RMSE", trControl=control)
lm_fit$results

y_hat_cv <- predict(lm_fit$finalModel, test_set)
sqrt(mean((y_hat_cv-test_set$heart_disease_mortality_per_100k)^2, na.rm=TRUE))

#Regression Trees
rt_fit <- rpart(heart_disease_mortality_per_100k ~ ., data = model_data,
                control = rpart.control(cp=0, minsplit=2))
y_hat_rt <- predict(rt_fit)
sqrt(mean((y_hat_rt-model_data$heart_disease_mortality_per_100k)^2, na.rm=TRUE))

bt <- as.numeric(train_rt$bestTune[which.min(train_rt$bestTune)])

rt_pruned <- prune(rt_fit, cp=bt)
y_hat_pruned <- predict(rt_pruned)
sqrt(mean((y_hat_pruned-model_data$heart_disease_mortality_per_100k)^2, na.rm=TRUE))

train_rt <- train(heart_disease_mortality_per_100k ~ .,
                  method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.05, len=25)),
                  data = model_data)
ggplot(train_rt)

plot(train_rt$finalModel, margin=0.1)
text(train_rt$finalModel, cex=0.5)

#Random Forests
library(randomForest)

train_rf <- randomForest(heart_disease_mortality_per_100k ~ ., data=model_data)

train_rf_full <- randomForest(x=train_set[model_cols],
                              y=train_set$heart_disease_mortality_per_100k,
                              xtest = test_set[model_cols]
                              )

fit_rf_full <- train(method="rf",
                     x=train_set[model_cols],
                     y=train_set$heart_disease_mortality_per_100k,
                     tuneGrid = data.frame(mtry = seq(1,11)))

library(Rborist)
fit_rf <- train(heart_disease_mortality_per_100k ~ .,
                method="Rborist",
                tuneGrid=data.frame(predFixed=2, minNode = seq(3,50) ),
                data=train_set)

y_hat_rf <- predict(fit_rf, test_set)
sqrt(mean((y_hat_rf-test_set$heart_disease_mortality_per_100k)^2, na.rm=TRUE))

results <- rbind(data.frame(method = "lm", 
                            RMSE = sqrt(mean((y_hat-test_set$heart_disease_mortality_per_100k)^2, na.rm=TRUE))),
                 data.frame(method="lm xv",
                            RMSE=sqrt(mean((y_hat_cv-test_set$heart_disease_mortality_per_100k)^2, na.rm=TRUE))),
                 data.frame(method="Reg Tree - pruned",
                            RMSE=sqrt(mean((y_hat_pruned-model_data$heart_disease_mortality_per_100k)^2, na.rm=TRUE))),
                 data.frame(method="randomForest",
                            RMSE=sqrt(mean((train_rf$predicted - model_data$heart_disease_mortality_per_100k)^2))),
                 data.frame(method="randomForest, train/test",
                            RMSE=sqrt(mean((train_rf_full$test$predicted-test_set$heart_disease_mortality_per_100k)^2))),
                 data.frame(method="trained Rborist",
                            RMSE=sqrt(mean((y_hat_rf-test_set$heart_disease_mortality_per_100k)^2, na.rm=TRUE))))

write.csv(results, "model_results.csv")
