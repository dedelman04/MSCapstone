cols <- colnames(train_data)[like(colnames(train_data), "demo__pct")]
cols <- cols[!like(cols, "health_")]
ncol <- ifelse(length(cols) <= 6, 2, 3)

train_data %>% select(heart_disease_mortality_per_100k, cols) %>%
  gather(data_type, val, -heart_disease_mortality_per_100k) %>%
  mutate(data_type = substr(data_type, 3, 100)) %>%
  mutate(val = val*100) %>%
  ggplot(aes(x=val, y= heart_disease_mortality_per_100k))+geom_point(na.rm = TRUE)+
  facet_wrap(~ data_type, ncol=ncol, scales="fixed")+
  geom_smooth(method="lm", na.rm = TRUE)+xlab("percent")
#  scale_x_continuous(labels=scales::percent)

corr <- data.frame()
for (i in 1:length(cols)) {
  corr[cols[i],1] <- round(cor(data.frame(train_data$heart_disease_mortality_per_100k,
                              train_data[,cols[i]]),
                   use="complete.obs")[1,2],3)
}

corr <- cbind(cols, corr)
colnames(corr) <- c("type", "cor")
corr <- corr %>% arrange(desc(abs(cor)))

corr %>% grid.table()

train_data <- train_data %>% 
  mutate(econ_factor = 
    #as.factor(
      ifelse(econ__economic_typology %in% c("Manufacturing-dependent", 
                                            "Mining-dependent", 
                                            "Nonspecialized"), 1,
             ifelse(econ__economic_typology %in% c("Farm-dependent",
                                                   "Recreation"), -1, 0)))#)

#levels(train_data$econ_factor) <- c("Below Mean", "No Diff", "Above Mean")

train_data %>% ggplot(aes(x=demo__pct_adults_less_than_a_high_school_diploma, y=heart_disease_mortality_per_100k))+
  geom_point(aes(color = econ__economic_typology))
  geom_hline(linetype="dashed", yintercept = hd_mean, color = "blue")+
  geom_hline(linetype="dashed", yintercept = hd_med, color = "red")

  
##Modelling

model_cols <- c("econ__economic_typology",
                "population",
                "metro",
                "air_pollution",
                "econ__pct_civilian_labor",
                "demo__pct_non_hispanic_african_american",
                "demo__pct_adults_bachelors_or_higher",
                "demo__pct_adults_less_than_a_high_school_diploma",
                "p_diab_obese",
                "p_diab_smoke",
                "p_all_three",
                "health__homicides_per_100k",
                "health__motor_vehicle_crash_deaths_per_100k")

cont_cols <- model_cols[-c(1:4)]

#How many rows have at least 1 missing numeric value?

mval <- function(x) {
  this_row <- train_data %>% filter(row_id==x) %>% 
    select(row_id, cont_cols) %>% as.vector()
  ifelse(length(which(is.na(this_row)))>0, this_row[1], 0)
}

missval <- sapply(train_data$row_id, mval, simplify = TRUE)

mean(missval!=0)
length(train_data$row_id)

#Which predictors have most NA values
pct_NA <- function(x) {
#  colnames(x)
#  NAcnt <- 
    length(which(is.na(x)))
#  c(col, NAcnt)
}

data.frame(
  NAs = sapply(train_data[cont_cols], function(x){length(which(is.na(x)))}, simplify=TRUE)) %>% 
  grid.table()

data.frame(
    NA_pct = 
      #round(
      format(
      sapply(train_data[cont_cols], function(x){mean(ifelse(is.na(x), 1, 0))}, simplify=TRUE),
      digits = 3)#* 100, 1)
      ) %>% 
  grid.table()


length(which(is.na(train_data[cont_cols])))


#plot distros of numeric data
train_data %>% select(cont_cols) %>%
  gather(feature, value) %>%
  ggplot(aes(y=value)) + geom_boxplot() + 
  facet_wrap(~ feature, scales="free_y")

feat_means <- train_data %>% select(cont_cols) %>%
  gather(feature, value) %>% group_by(feature) %>%
  mutate(mn = mean(value, na.rm=TRUE), md = median(value, na.rm=TRUE))

train_data %>% select(cont_cols) %>%
  gather(feature, value) %>%
  ggplot(aes(x=value)) + geom_histogram() +
  geom_vline(aes(xintercept=mn), data = feat_means, linetype = "dashed", color = "yellow")+
  geom_vline(aes(xintercept=md), data = feat_means, linetype = "dashed", color = "red")+
  facet_wrap(~ feature, scales="free") +
  xlab("yellow = mean; red = median")

#Replace NA with column median
train_data <- train_data %>%
  mutate_at(cont_cols, ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))

#Replace negative values with column median
train_data <- train_data %>%
  mutate_at(cont_cols, ~ifelse(.x < 0, median(.x, na.rm = TRUE), .x))

#Categorical NAs
mval <- function(x) {
  this_row <- train_data %>% filter(row_id==x) %>% 
    select(row_id, model_cols[1:4]) %>% as.vector()
  ifelse(length(which(is.na(this_row)))>0, this_row[1], 0)
}

missval_c <- sapply(train_data$row_id, mval, simplify = TRUE)

length(which(missval_c!=0))

train_data <- train_data[which(missval_c==0),]

#Data normalization
feat_means <- train_data %>% select(cont_cols) %>%
  gather(feature, value) %>% group_by(feature) %>%
  summarise(mn = mean(value, na.rm=TRUE), 
            md = median(value, na.rm=TRUE),
            sd = sd(value, na.rm=TRUE),
            minval = min(value), maxval = max(value))

xform <- train_data %>% select(cont_cols) %>%
  gather(feature, value) %>% merge(y=feat_means, by="feature") %>%
  mutate(cuberoot = value^(1/3), sqroot = sqrt(value), 
         l10 = log10(value), natlog = log(value),
         minmax = (value-minval)/(maxval-minval),
         z = (value-mn)/sd)

xform %>% ggplot(aes(x=cuberoot)) +geom_histogram()+
  facet_wrap(~ feature, scale="free")

xform %>% ggplot(aes(x=sqroot)) +geom_histogram()+
  facet_wrap(~ feature, scale="free")

xform %>% ggplot(aes(x=z)) +geom_histogram()+
  facet_wrap(~ feature, scale="free")

#Linear regression model
lm_form <- as.formula(paste("heart_disease_mortality_per_100k ~ ", paste(model_cols, collapse="+")))

set.seed(999)
test_index <- createDataPartition(train_data$heart_disease_mortality_per_100k, times = 1, p=0.5, list=FALSE)
train_set <- train_data[-test_index, c("heart_disease_mortality_per_100k", model_cols)] 
test_set <- train_data[test_index, c("heart_disease_mortality_per_100k", model_cols)]

fit <- lm(formula = lm_form, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat-test_set$heart_disease_mortality_per_100k)^2, na.rm=TRUE))

#Cross-validation
x <- train_data %>% select(model_cols)
y <- train_data$heart_disease_mortality_per_100k

lm_fit <- train(x, y, method="lm", metric="RMSE")
lm_fit$results

glm_fit <- train(x, y, method="glm")
glm_fit$results
