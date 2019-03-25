#Build categorical mean/median tables
bind_rows(train_data %>% select(heart_disease_mortality_per_100k,
                      air_pollution) %>%
  group_by(air_pollution) %>%
  summarize(mn = mean(heart_disease_mortality_per_100k),
            md = median(heart_disease_mortality_per_100k)) %>%
  mutate(MeanAboveBelow = ifelse(round(mn,1) < hd_mean, 
                                 "Below",
                                 ifelse(round(mn,1) == hd_mean,
                                        "Equal", "Above")),
         MedianAboveBelow = ifelse(round(md,1) < hd_med,
                                   "Below",
                                   ifelse(round(md,1) == hd_med,
                                          "Equal", "Above"))) %>%
  select(air_pollution, mn, MeanAboveBelow, md, MedianAboveBelow)
  , data.frame(air_pollution = "Population", 
               mn= hd_mean, 
               MeanAboveBelow = "",
               md = hd_med,
               MedianAboveBelow = "")
)

cols <- colnames(train_data)[like(colnames(train_data), "health__")]
cols <- cols[!like(cols, "health__air")]
ncol <- ifelse(length(cols) <= 6, 2, 3)

train_data %>% select(heart_disease_mortality_per_100k, cols) %>%
  gather(data_type, val, -heart_disease_mortality_per_100k) %>%
  mutate(data_type = substr(data_type, 9, 100)) %>%
#  mutate(val = val*100) %>%
  ggplot(aes(x=val, y= heart_disease_mortality_per_100k))+geom_point(na.rm = TRUE)+
  facet_wrap(~ data_type, ncol=ncol, scales="free_x")+
  geom_smooth(method="lm")+xlab("percent")
#  scale_x_continuous(labels=scales::percent)

#Continuous columns
num_cols <- train_data %>% 
  select(-c(row_id, heart_disease_mortality_per_100k)) %>%
  .[sapply(., is.numeric)] %>% 
  colnames()
  

#Categorical columns
cat_cols <- colnames(train_data)[sapply(train_data, is.factor)]

get_corr <- function(x) {
  cor(data.frame(train_data$heart_disease_mortality_per_100k, x),
      use="complete.obs")[1,2] }

#Get correlation for each numeric column
data.frame(feature=num_cols,
           corr = sapply(train_data[num_cols], get_corr)) %>% 
  arrange(desc(abs(corr)))

data.frame(
  feature=names(train_data[cols]),
  corr = sapply(train_data[cols], function(x) {
    cor(data.frame(train_data$heart_disease_mortality_per_100k, x),
        use="complete.obs")[1,2] })
) %>% arrange(desc(abs(corr)))

#Plot all numerics faceted by categorical variable and colored by cat value
#To look for possible clusters
multi_plot <- function(x) {
train_data %>% select(heart_disease_mortality_per_100k, colnames(x), cat_cols) %>%
  gather(category, value, -c(heart_disease_mortality_per_100k, colnames(x))) %>%
  ggplot(aes(x=get(x), y=heart_disease_mortality_per_100k))+
  facet_wrap(~ category) + geom_point(aes(color=value)) + xlab(x)
}

tmp <- lapply(names(train_data[num_cols]), 
       function(x) {
         train_data %>% select(heart_disease_mortality_per_100k, x, cat_cols) %>%
           gather(category, value, -c(heart_disease_mortality_per_100k, x)) %>%
           ggplot(aes(x=get(x), y=heart_disease_mortality_per_100k))+
           facet_wrap(~ category) + geom_point(aes(color=value)) +xlab(x)
         
       })

#Look for correlation between diabetes, obesity, physical inactivity
cols <- c("health__pct_physical_inactivity",
          "health__pct_diabetes",
          "health__pct_adult_obesity")

train_data <- train_data %>% 
  mutate(p_diab_obese = health__pct_adult_obesity * health__pct_diabetes) %>%
  mutate(p_obese_inact = health__pct_adult_obesity * health__pct_physical_inactivity) %>%
  mutate(p_diab_inact = health__pct_diabetes * health__pct_physical_inactivity) %>%
  mutate(p_all_three = health__pct_adult_obesity * health__pct_diabetes * health__pct_physical_inactivity )

cols <- c("p_diab_obese", 
          "p_obese_inact", 
          "p_diab_inact", 
          "p_all_three")

ncol <- ifelse(length(cols) <= 6, 2, 3)

train_data %>% select(heart_disease_mortality_per_100k, cols) %>%
  gather(data_type, val, -heart_disease_mortality_per_100k) %>%
  mutate(data_type = substr(data_type, 3, 100)) %>%
  mutate(val = val * 100) %>%
  ggplot(aes(x=val, y= heart_disease_mortality_per_100k))+geom_point(na.rm = TRUE)+
  facet_wrap(~ data_type, ncol=ncol, scales="fixed")+xlab("percent")+
  geom_smooth(method="lm", na.rm = TRUE)

data.frame(
  feature=names(train_data[cols]),
  corr = sapply(train_data[cols], function(x) {
    cor(data.frame(train_data$heart_disease_mortality_per_100k, x),
        use="complete.obs")[1,2] })
) %>% arrange(desc(abs(corr)))


#Cross each numeric feature with each categorical feature and get correlation
cross_cols <- paste(rep(num_cols, each=length(cat_cols)), cat_cols, sep="+")

data.frame(feature=cross_cols,
           corr = sapply(train_data[num_cols], get_corr)) %>% 
  arrange(desc(abs(corr)))

cor(data.frame(train_data$heart_disease_mortality_per_100k, 
               data.frame(train_data$econ__pct_civilian_labor,
                          ifelse(train_data$metro == "Metro", TRUE, FALSE ))))
                          #train_data$metro)))

#Old method
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
##

train_data <- train_data %>% 
  mutate(econ_factor = 
    #as.factor(
      ifelse(econ__economic_typology %in% c("Manufacturing-dependent", 
                                            "Mining-dependent", 
                                            "Nonspecialized"), 1,
             ifelse(econ__economic_typology %in% c("Farm-dependent",
                                                   "Recreation"), -1, 0)))#)

#levels(train_data$econ_factor) <- c("Below Mean", "No Diff", "Above Mean")

train_data %>% mutate(pop = ifelse(population %in% c("under 2.5k", "2.5-20k", "20-250k"),
                                   "Small", "Large")) %>%
  ggplot(aes(x=health__pct_physical_inactivity, y=heart_disease_mortality_per_100k))+
  geom_point(aes(color = metro))+
  geom_hline(linetype="dashed", yintercept = hd_mean, color = "blue")+
  geom_hline(linetype="dashed", yintercept = hd_med, color = "red")

  
##Modelling

model_cols <- c("econ__economic_typology",
                "population",
                "metro",
                "air_pollution",
                "econ__pct_civilian_labor",
                "demo__pct_non_hispanic_african_american",
                "demo__death_rate_per_1k",
                "demo__pct_adults_bachelors_or_higher",
                "demo__pct_adults_less_than_a_high_school_diploma",
                "health__pct_physical_inactivity",
                "p_diab_obese",
                "p_obese_inact",
                "p_diab_inact",
                "p_all_three")

train_data <- train_data %>% select(model_cols, heart_disease_mortality_per_100k)

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
data.frame(
  NAs = sapply(train_data[cont_cols], function(x){length(which(is.na(x)))}, simplify=TRUE)) %>% 
  grid.table()

#Model continuous features
data.frame(
    NA_pct = 
      format(
      sapply(train_data[cont_cols], function(x){mean(ifelse(is.na(x), 1, 0))}, simplify=TRUE),
      digits = 3)
      ) %>% 
  grid.table()

#All features
NAs <- data.frame(
  features = colnames(train_data %>% select(-heart_disease_mortality_per_100k)),
  NA_pct = 
      as.numeric(
       format( sapply(train_data %>% select(-heart_disease_mortality_per_100k),
             function(x){mean(ifelse(is.na(x), 1, 0))}, simplify=TRUE)*100, digits=3) 
      )
) 
NAs %>% arrange(desc(NA_pct))

high_NA <- NAs %>% filter(NA_pct > 10) %>% .$features %>% as.character()

train_data <- train_data %>% select(-high_NA)

na2 <- as.vector(NAs$features[NAs$NA_pct == 0.0625])

na_mid <- NAs[NAs$NA_pct > 1 & NAs$NA_pct < 10, ]

#Calculate the mean and median of the features in question
feat_means <- train_data %>% select(as.vector(na_mid[,1])) %>%
  gather(feature, value) %>% group_by(feature) %>%
  mutate(mn = mean(value, na.rm=TRUE), md = median(value, na.rm=TRUE))

#Plot historgram, faceted by feature, overlaying the mean and median values of each
train_data %>% select(as.vector(na_mid[,1])) %>%
  gather(feature, value) %>%
  ggplot(aes(x=value))+geom_histogram(bins=40)+
  geom_vline(aes(xintercept=mn), data = feat_means, linetype = "dashed", color = "yellow")+
  geom_vline(aes(xintercept=md), data = feat_means, linetype = "dashed", color = "red")+
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  facet_wrap(~ feature, scales="free") +
  xlab("yellow = mean; red = median") + ylim(0,1000)

sapply(train_data %>% select(as.vector(na_mid[, 1])),
       function(x) {
         feat_med <- median(x, na.rm=TRUE)
         feat_NA <- which(is.na(x))
         x[feat_NA] <- feat_med
       }
)
mid_cols <- as.vector(na_mid[, 1])
train_data <- train_data %>%
  mutate_at(mid_cols, ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))

min_missing <- min(NAs %>% filter(NA_pct > 0) %>% select(NA_pct))  
NA2 <- sapply(train_data %>% 
         select(as.vector(NAs$features[NAs$NA_pct == min_missing])),
       function(x){which(is.na(x))}, simplify=TRUE)

train_data <- train_data[-t(NA2)[1,c(1:2)], ]

train_data[t(NA2)[1,c(1:2)], ]


#Histogram for air pollution original
train_data %>% filter(!is.na(health__air_pollution_particulate_matter)) %>%
  ggplot(aes(x=health__air_pollution_particulate_matter))+
  geom_histogram(binwidth=1) + xlab("air pollution (µg/m^3)")+
  theme(axis.text.x = element_text(size=20))+
  ggtitle("(A)")+theme(plot.title = element_text(hjust=0.5))

#Histogram for air pollution regrouped
cut_labels <- c("<= 10", "11", "12", "13", "14+")
cut_levels <- c(0, 10.9, 11.9, 12.9, 13.9, 100)

train_data$air_pollution <- cut(train_data$health__air_pollution_particulate_matter, cut_levels)
levels(train_data$air_pollution) <- cut_labels

train_data %>% 
  ggplot(aes(x=air_pollution))+
  geom_bar() + xlab("air pollution (µg/m^3)")+
  theme(axis.text.x = element_text(size=20))+
  ggtitle("(B)")+theme(plot.title = element_text(hjust=0.5))

ap_med <- median(train_data$health__air_pollution_particulate_matter, na.rm=TRUE)
ap_NA <- which(is.na(train_data$health__air_pollution_particulate_matter))

train_data$health__air_pollution_particulate_matter[ap_NA] <- ap_med

#plot distros of numeric data
num_cols <- train_data %>% 
  select(-heart_disease_mortality_per_100k) %>%
  .[sapply(., is.numeric)] %>% 
  colnames()

train_data %>% select(num_cols) %>%
  gather(feature, value) %>%
  ggplot(aes(y=value)) + geom_boxplot() + 
  facet_wrap(~ feature, scales="free_y")

feat_means <- train_data %>% select(num_cols) %>%
  gather(feature, value) %>% group_by(feature) %>%
  mutate(mn = mean(value, na.rm=TRUE), md = median(value, na.rm=TRUE))

train_data %>% select(num_cols) %>%
  gather(feature, value) %>%
  ggplot(aes(x=value)) + geom_histogram(bins=50) +
  facet_wrap(~ feature, scales="free")



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




#Histograms by air pollution
train_data %>% ggplot(aes(x=heart_disease_mortality_per_100k))+
  xlab("heart_mort_100k")+
  geom_histogram(binwidth = (high_mort-low_mort)/40)+
  facet_wrap(~ air_pollution, drop=TRUE, ncol = 3)+
  geom_vline(linetype="dashed", xintercept = hd_mean, color = "blue")+
  geom_vline(linetype="dashed", xintercept = hd_med, color = "red")+
  annotate("text", x=hd_mean+40, y=80, label="<- mean", size=2.5, color = "blue")+
  annotate("text", x=hd_med-40, y=80, label="median ->", size=2.5, color = "red")

#Box plot by air_pollution
train_data %>% ggplot(aes(x=air_pollution, y=heart_disease_mortality_per_100k))+
  xlab("")+
  geom_boxplot()+
  geom_hline(linetype="dashed", yintercept = hd_mean, color = "blue")+
  geom_hline(linetype="dashed", yintercept = hd_med, color = "red")+
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  ggtitle("Air Pollution (mcg/m^3)")+theme(plot.title = element_text(hjust=0.5))+
  annotate("text", x=.5, y=hd_mean+30, label="mean", size=3, color = "blue", angle=90)+
  annotate("text", x=5.5, y=hd_med-30, label="median", size=3, color = "red", angle=90)