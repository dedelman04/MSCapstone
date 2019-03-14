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
missval <- c()
for (j in train_data$row_id){
   missing_ind <- c()
   missing_ind <- which(is.na(train_data[row_id==j, cont_cols]))
   if(length(missing_ind > 0)) {missval[j] <- "Yes"}  
}

mval <- function(x) {
  this_row <- train_data %>% filter(row_id==x) %>% 
    select(row_id, cont_cols) %>% as.vector()
  which(is.na(this_row))
}

retval <- c()
missval <- vapply(train_data$row_id, mval, FUN.VALUE=retval)

which(length(missval)>0)

#Replace NA with column mean
train_data <- train_data %>%
  mutate_at(cont_cols, ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>%
  mutate_at(c("health__air_pollution_particulate_matter"), ~ifelse(is.na(.x), median(.x, na.rm=TRUE), .x))

lm_form <- as.formula(paste("heart_disease_mortality_per_100k ~ ", paste(model_cols, collapse="+")))

test_index <- createDataPartition(train_data$heart_disease_mortality_per_100k, times = 1, p=0.5, list=FALSE)
train_set <- train_data[-test_index,]
test_set <- train_data[test_index,]

fit <- lm(formula = lm_form, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat-test_set$heart_disease_mortality_per_100k)^2, na.rm=TRUE))

median(train_data$health__air_pollution_particulate_matter, na.rm = TRUE)

