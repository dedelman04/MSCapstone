library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)
library(knitr)
library(kableExtra)
library(caret)

###BEGIN DATA LOAD WORK
#######################

train_df <- read.csv("C:/users/dedelman/desktop/capstone/train_values.csv",
                  header=TRUE,
                  stringsAsFactors = TRUE)

train_label <- read.csv("C:/users/dedelman/desktop/capstone/train_labels.csv",
                        header=TRUE,
                        stringsAsFactors = TRUE)

train_data <- merge(x=train_df, y=train_label, by="row_id")

low_mort <- as.integer(min(train_data$heart_disease_mortality_per_100k))
high_mort <- as.integer(max(train_data$heart_disease_mortality_per_100k))

#Group/factor population
#train_data <- train_data %>% mutate(population = case_when(like(train_data$area__rucc, "19,999") ~ "2.5-20K",
#                                                           like(train_data$area__rucc, "less than 2,500") ~ "under 2.5K",
#                                                           like(train_data$area__rucc, "20,000") ~ "20-250K",
#                                                           like(train_data$area__rucc, "fewer than 250,000") ~ "20-250K",
#                                                           like(train_data$area__rucc, "250,000") ~ "250K-1M",
#                                                           TRUE ~ "1M+"))

train_data <- train_data %>% mutate(metro = ifelse(like(area__rucc, "Nonmetro"), "Nonmetro", "Metro"))

train_data <- train_data %>% 
  mutate(population = ifelse(like(train_data$area__rucc, "19,999"),"2.5-20k",
                             ifelse(like(train_data$area__rucc, "less than 2,500"), "under 2.5k",
                                    ifelse(like(train_data$area__rucc, "20,000"), "20-250k",
                                           ifelse(like(train_data$area__rucc, "fewer than 250,000"), "20-250k", ##same group
                                                  ifelse(like(train_data$area__rucc, "250,000"), "250k-1M",
                                                         "1M+"))))))

train_data$population <- factor(train_data$population, 
                                levels=c("under 2.5k", "2.5-20k", "20-250k", "250k-1M", "1M+"))

#Group metro
train_data <- train_data %>% mutate(metro = factor(case_when(like(area__rucc, "Nonmetro") ~ "Non-Metro",
                                                             TRUE ~ "Metro")))

#Prob of combinations of conditions
train_data <- train_data %>% mutate(p_diab_obese = health__pct_adult_obesity * health__pct_diabetes) %>%
                  mutate(p_diab_smoke = health__pct_diabetes * health__pct_adult_smoking) %>%
                  mutate(p_obese_smoke = health__pct_adult_smoking * health__pct_adult_obesity) %>%
                  mutate(p_all_three = health__pct_adult_obesity * health__pct_adult_obesity * health__pct_adult_smoking )

#Turn air pollution into a factor
# <= 9 thru >= 14; 
cut_labels <- c("<= 10", "11", "12", "13", "14+")
cut_levels <- c(0, 10.9, 11.9, 12.9, 13.9, 100)

train_data$air_pollution <- cut(train_data$health__air_pollution_particulate_matter, cut_levels)
levels(train_data$air_pollution) <- cut_labels

#Turn population per med prof into factor
cut_labels <- c("< 1500", "1500-2499", "2500+")
cut_levels <- c(0, 1499, 2499, 100000)

train_data$primary_care_physician <- cut(train_data$health__pop_per_primary_care_physician, cut_levels)
levels(train_data$primary_care_physician) <- cut_labels

cut_labels <- c("< 1500", "1500-2499", "2500-3499", "3500-4499", "4500+")
cut_levels <- c(0, 1499, 2499, 3499, 4499, 100000)

train_data$dentist <- cut(train_data$health__pop_per_dentist, cut_levels)
levels(train_data$dentist) <- cut_labels

train_data$log_homicide_per_100k <- ifelse(train_data$health__homicides_per_100k <= 0, NA, log10(train_data$health__homicides_per_100k))
train_data$log_motor_vehicle_death_per_100k <- log10(train_data$health__motor_vehicle_crash_deaths_per_100k)


#store population mean and median
hd_mean = mean(train_data$heart_disease_mortality_per_100k)
hd_med = median(train_data$heart_disease_mortality_per_100k)
######################
###END Data Load Work

#Historgram & boxplot
train_data %>% ggplot(aes(x=heart_disease_mortality_per_100k))+geom_histogram(bins=40)+
  xlab("heart_mort_100k")+
  geom_vline(linetype = "dashed", color = "purple", xintercept = hd_med)+
  geom_vline(linetype = "dashed", color = "blue", xintercept = hd_mean)+
  annotate("text", x=hd_mean+20, y=250, label="<- mean", size=3, color = "blue")+
  annotate("text", x=hd_med-20, y=250, label="median ->", size=3, color = "purple")  

train_data %>% ggplot(aes(y=heart_disease_mortality_per_100k))+geom_boxplot()+
  geom_hline(linetype = "dashed", color = "blue", yintercept = hd_mean)+
  theme(axis.text.x=element_blank())

  

#write.csv(train_data, file="train_factor.csv", sep=",", row.names=FALSE)

plot <- ggplot(train_data, 
               aes(x=health__pct_adult_obesity, y=heart_disease_mortality_per_100k))
plot <- plot + geom_point() + facet_wrap(~ area__rucc, ncol=3)
plot

plot <- ggplot(train_data, 
               aes(x=health__pct_diabetes, y=heart_disease_mortality_per_100k))
plot <- plot + geom_point() + facet_wrap(~ area__rucc, ncol=3)
plot

plot <- ggplot(train_data, 
               aes(x=health__pct_adult_smoking, y=heart_disease_mortality_per_100k))
plot <- plot + geom_point() + facet_wrap(~ area__rucc, ncol=3)
plot

plot <- ggplot(train_data, 
               aes(x=health__pct_adult_obesity, y=heart_disease_mortality_per_100k))
plot <- plot + geom_point() + facet_wrap(~ econ__economic_typology, ncol=3)
plot

plot <- ggplot(train_data, 
               aes(x=health__pct_diabetes, y=heart_disease_mortality_per_100k))
plot <- plot + geom_point() + facet_wrap(~ econ__economic_typology, ncol=3)
plot

plot <- ggplot(train_data, 
               aes(x=health__pct_adult_smoking, y=heart_disease_mortality_per_100k))
plot <- plot + geom_point() + facet_wrap(~ econ__economic_typology, ncol=3)
plot

#plot <- ggplot(train_data, 
#               aes(x=log(health__pct_low_birthweight*10), y=heart_disease_mortality_per_100k))
#plot <- plot + geom_point() + facet_wrap(~ econ__economic_typology, ncol=3)
#plot

#plot <- ggplot(train_data, 
#               aes(x=log(health__pct_low_birthweight), y=heart_disease_mortality_per_100k))
#plot <- plot + geom_point() + facet_wrap(~ area__rucc, ncol=3)
#plot

#Correlation among econ stats
cname <- colnames(train_data)
cname <- cname[like(cname,"econ__pct")]
#cname <- cname[!like(cname, "_1k")]

par(mfrow=c(2,2))

p = list()

for (i in 1:length(cname)) {
#    print(cname[i])
#    print(max(train_data[cname[i]], na.rm=TRUE))
  
    dplot <- ggplot(train_data, 
                  aes_string(y="heart_disease_mortality_per_100k",
                             x=cname[i]))+
    ylab("hrt_mort")+
    xlab(substr(cname[i], 7, 100))+
    xlim(0,max(train_data[cname[i]], na.rm=TRUE)+.05)

    if(cname[i]=="econ__pct_civilian_labor") {
      dplot <- dplot + theme(plot.background = element_rect(colour = "black", fill=NA, size=2))
    }
  cor <- round(cor(data.frame(train_data$heart_disease_mortality_per_100k,
                              train_data[,cname[i]]),
                   use="complete.obs")[1,2],3)
  cor <- paste("corr =", as.character(cor))
  
  p[[i]] <- dplot+geom_point(na.rm=TRUE)+
    geom_smooth(method="lm", se=TRUE, na.rm=TRUE)+
    annotate("text", x=max(train_data[cname[i]],na.rm=TRUE), y=500, label= cor, size=3)
}

do.call(grid.arrange, p)

#Correlation among demo stats
cname <- colnames(train_data)
cname <- cname[like(cname,"demo_")]
cname <- cname[!like(cname, "_1k")]

par(mfrow=c(3,4))

p = list()

for (i in 1:length(cname)) {
  #Determine correlation with default method
  cor <- round(cor(data.frame(train_data$heart_disease_mortality_per_100k,
                              train_data[,cname[i]]),
                   use="complete.obs")[1,2],3)
  cor <- paste("corr =", as.character(cor))
  
  #Plot demo vs mortality
  dplot <- ggplot(train_data, 
                  aes_string(y="heart_disease_mortality_per_100k",
                             x=cname[i]))+
    ylab("hrt_mort")+
    xlab(substr(cname[i], 11, 100)) #+
  #xlim(0,1)
  
  if(cname[i] %in% c("demo__pct_non_hispanic_african_american",
                     "demo__pct_adults_less_than_a_high_school_diploma",
                     "demo__pct_adults_bachelors_or_higher")) {
    dplot <- dplot + theme(plot.background = element_rect(colour = "black", fill=NA, size=2))
  }
  
  p[[i]] <- dplot+geom_point(na.rm=TRUE)+
    geom_smooth(method="lm", se=TRUE, na.rm=TRUE)+
    annotate("text", x=.9, y=500, label= cor, size=2.5)+xlim(0, 1)
}

do.call(grid.arrange, p)



#Histogram for mortality rate
plot <- ggplot(train_data,
               aes(x=heart_disease_mortality_per_100k))+xlab("heart_mort_100k")+
  geom_histogram(binwidth = (high_mort-low_mort)/40)
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_mean, color = "blue")
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_med, color = "purple")
plot <- plot + annotate("text", x=hd_mean+20, y=250, label="<- mean", size=3, color = "blue")
plot <- plot + annotate("text", x=hd_med-20, y=250, label="median ->", size=3, color = "purple")

plot

#Box plot for mortality rate
plot <- ggplot(train_data,
               aes(x=row_id, y=heart_disease_mortality_per_100k))+
  geom_boxplot() + geom_hline(linetype="dashed", yintercept = hd_mean, color = "blue")
plot <- plot + theme(axis.text.x = element_text(color="white"))
plot <- plot + ggtitle("Heart Disease Mortality")
plot <- plot+xlab("")
plot <- plot + annotate("text", x=0, y=hd_mean+10, label="mean", size=3, color = "blue")
plot

#Histograms for economic type
plot <- ggplot(train_data,
               aes(x=heart_disease_mortality_per_100k))+xlab("heart_mort_100k")+
              geom_histogram(binwidth = (high_mort-low_mort)/50)
plot <- plot+facet_wrap(~ econ__economic_typology, ncol = 3)
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_mean, color = "blue")
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_med, color = "red")
plot <- plot + annotate("text", x=hd_mean+40, y=100, label="<- mean", size=2.5, color = "blue")
plot <- plot + annotate("text", x=hd_med-40, y=100, label="median ->", size=2.5, color = "red")
plot

#Box plot by economic type
plot <- ggplot(train_data,
               aes(x=econ__economic_typology, y=heart_disease_mortality_per_100k))+
  geom_boxplot() + geom_hline(linetype="dashed", yintercept = hd_mean, color = "blue")+xlab("")
plot <- plot+geom_hline(linetype="dashed", yintercept = hd_med, color = "red")
plot <- plot + theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1)) + ggtitle("Economic Typology")
plot <- plot + annotate("text", x=.5, y=hd_mean+30, label="mean", size=3, color = "blue", angle=90)
plot <- plot + annotate("text", x=6.5, y=hd_med-30, label="median", size=3, color = "red", angle=90)

plot

train_data %>% ggplot(aes(x=heart_disease_mortality_per_100k))+
  xlab("heart_mort_100k")+
  geom_histogram(binwidth = (high_mort-low_mort)/50)+
  facet_wrap(~ econ__economic_typology, ncol = 3)+
  geom_vline(linetype="dashed", xintercept = hd_mean, color = "blue")+
  geom_vline(linetype="dashed", xintercept = hd_med, color = "red")+
  annotate("text", x=hd_mean+40, y=100, label="<- mean", size=2.5, color = "blue")+
  annotate("text", x=hd_med-40, y=100, label="median ->", size=2.5, color = "red")

#Box plot by economic type
train_data %>% ggplot(aes(x=econ__economic_typology, y=heart_disease_mortality_per_100k))+
  geom_boxplot() + 
  geom_hline(linetype="dashed", yintercept = hd_mean, color = "blue")+xlab("")+
  geom_hline(linetype="dashed", yintercept = hd_med, color = "red")+
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1)) + 
  ggtitle("Economic Typology")+theme(plot.title = element_text(hjust=0.5))+
  annotate("text", x=.5, y=hd_mean+30, label="mean", size=3, color = "blue", angle=90)+
  annotate("text", x=6.5, y=hd_med-30, label="median", size=3, color = "red", angle=90)

#Summary for typology
econ <- rxSummary(data=train_data, heart_disease_mortality_per_100k ~ econ__economic_typology)
econ.df <- econ$categorical[[1]]
write.csv(econ.df, file="econ_stats.csv", row.names=FALSE)

med.vec <- c()
typo <- levels(train_data$econ__economic_typology)
for (i in 1:length(typo)) {
  hm_df <- train_data %>% subset(econ__economic_typology == typo[i])
  med.vec[i] <- median(hm_df$heart_disease_mortality_per_100k)
}

med.vec

#Correlation for pct civilian
dplot <- ggplot(train_data, 
       aes_string(y="heart_disease_mortality_per_100k",
                  x="econ__pct_civilian_labor"))+
  ylab("hrt_mort")+
  xlab("pct_civilian_labor")+
  xlim(0.15,1)

cor <- round(cor(data.frame(train_data$heart_disease_mortality_per_100k,
                            train_data$econ__pct_civilian_labor),
                 use="complete.obs")[1,2], 3)

cor <- paste("corr =", as.character(cor))

dplot+geom_point(na.rm=TRUE)+
  geom_smooth(method="lm", se=TRUE, na.rm=TRUE)+
  annotate("text", x=.9, y=500, label= cor, size=2.5)

#Rename levels for area_rucc
levels(train_data$area__rucc) <- c("Metro - 1 Million +",
                                  "Metro - 250,000 to 1 Mil",
                                  "Metro - less than 250,000",
                                  "Nonmetro - Rural or < 2.5k, adjacent",
                                  "Nonmetro - Rural or < 2.5k, non-adjacent",
                                  "Nonmetro - 2,500 to 19,999, adjacent",
                                  "Nonmetro - 2,500 to 19,999, non-adjacent",
                                  "Nonmetro - 20,000 or more, adjacent",
                                  "Nonmetro - 20,000 or more, non-adjacent"
                                  )


#Histograms for area/population type
plot <- ggplot(train_data,
               aes(x=heart_disease_mortality_per_100k))+xlab("heart_mort_100k")+
              geom_histogram(binwidth = (high_mort-low_mort)/40)
plot <- plot+facet_wrap(~ area__rucc, ncol = 3)
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_mean, color = "blue")
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_med, color = "red")
plot <- plot + annotate("text", x=hd_mean+40, y=55, label="<- mean", size=2.5, color = "blue")
plot <- plot + annotate("text", x=hd_med-40, y=55, label="median ->", size=2.5, color = "red")
plot

lvl <- levels(train_data$area__rucc)
pop_lvl <- c("1M+", "250k-1M", "20-250K", "under 2,500", "under 2,500", "2.5-20K", "2.5-20k", "20-250K", "20-250K")

rucc_tab <- data.frame(x=lvl, y=pop_lvl)

library(gridExtra)
library(grid)
grid.table(rucc_tab)

#Box plot by area/population type
plot <- ggplot(train_data,
               aes(x=area__rucc, y=heart_disease_mortality_per_100k))+xlab("population")+
  geom_boxplot()+ geom_hline(linetype="dashed", yintercept = hd_mean, color = "blue")
plot <- plot + theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1)) + ggtitle("Economic Typology")
plot

#Histograms by population groups
plot <- ggplot(train_data,
               aes(x=heart_disease_mortality_per_100k))+xlab("heart_mort_100k")+
            geom_histogram(binwidth = (high_mort-low_mort)/40)
plot <- plot+facet_wrap(~ population, ncol = 3)
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_mean, color = "blue")
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_med, color = "red")
plot <- plot + annotate("text", x=hd_mean+40, y=75, label="<- mean", size=2.5, color = "blue")
plot <- plot + annotate("text", x=hd_med-40, y=75, label="median ->", size=2.5, color = "red")
plot

#Box plot by population group
plot <- ggplot(train_data,
               aes(x=population, y=heart_disease_mortality_per_100k))+ xlab("")+
  geom_boxplot()+ geom_hline(linetype="dashed", yintercept = hd_mean, color = "blue")
plot <- plot+geom_hline(linetype="dashed", yintercept = hd_med, color = "red")
plot <- plot + theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1)) + ggtitle("Population Groups")
plot <- plot + annotate("text", x=.5, y=hd_mean+30, label="mean", size=3, color = "blue", angle=90)
plot <- plot + annotate("text", x=5.5, y=hd_med-30, label="median", size=3, color = "red", angle=90)
plot

#Summary for population
econ <- rxSummary(data=train_data, heart_disease_mortality_per_100k ~ population)
econ.df <- econ$categorical[[1]]
write.csv(econ.df, file="pop_stats.csv", row.names=FALSE)

med.vec <- c()
typo <- levels(train_data$population)
for (i in 1:length(typo)) {
  hm_df <- train_data %>% subset(population == typo[i])
  med.vec[i] <- median(hm_df$heart_disease_mortality_per_100k)
}

med.vec


#Histograms by metro type
plot <- ggplot(train_data,
               aes(x=heart_disease_mortality_per_100k))+xlab("heart_mort_100k")+
  geom_histogram(binwidth = (high_mort-low_mort)/40)
plot <- plot+facet_wrap(~ metro, ncol = 3)
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_mean, color = "blue")
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_med, color = "red")
plot <- plot + annotate("text", x=hd_mean+40, y=150, label="<- mean", size=2.5, color = "blue")
plot <- plot + annotate("text", x=hd_med-40, y=150, label="median ->", size=2.5, color = "red")
plot

#Box plot by metro_type
plot <- ggplot(train_data,
               aes(x=metro, y=heart_disease_mortality_per_100k))+xlab("")+
  geom_boxplot()+ geom_hline(linetype="dashed", yintercept = hd_mean, color = "blue")
plot <- plot+geom_hline(linetype="dashed", yintercept = hd_med, color = "red")
plot <- plot + theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1)) + ggtitle("Metropolitan Type")
plot <- plot + annotate("text", x=.5, y=hd_mean+30, label="mean", size=3, color = "blue", angle=90)
plot <- plot + annotate("text", x=2.5, y=hd_med-30, label="median", size=3, color = "red", angle=90)
plot

#Summary for metro type
econ <- rxSummary(data=train_data, heart_disease_mortality_per_100k ~ metro)
econ.df <- econ$categorical[[1]]
write.csv(econ.df, file="metro_stats.csv", row.names=FALSE)

med.vec <- c()
typo <- levels(train_data$metro)
for (i in 1:length(typo)) {
  hm_df <- train_data %>% subset(metro == typo[i])
  med.vec[i] <- median(hm_df$heart_disease_mortality_per_100k)
}

med.vec


#Correlation among health stats
cname <- colnames(train_data)
cname <- cname[like(cname,"health__pct")]
#cname <- cname[!like(cname, "_1k")]

par(mfrow=c(4,3))

p = list()

for (i in 1:length(cname)) {
  dplot <- ggplot(train_data,
                  aes_string(y="heart_disease_mortality_per_100k",
                             x=cname[i]))+
    ylab("hrt_mort")+
    xlab(substr(cname[i], 9, 100))+
    xlim(0,.6)
  
  cor <- round(cor(data.frame(train_data$heart_disease_mortality_per_100k,
                        train_data[,cname[i]]),
             use="complete.obs")[1,2],3)
  cor <- paste("corr =", as.character(cor))

  if(cname[i] %in% c("health__pct_adult_obesity",
                     "health__pct_adult_smoking",
                     "health__pct_diabetes",
                     "health__pct_physical_inacticity")) {
    dplot <- dplot + theme(plot.background = element_rect(colour = "black", fill=NA, size=2))
  }
  
#  if(cname[i] %in% c("health__pct_low_birthweight",
#                     "health__pct_excessive_drinking")) {
#    dplot <- dplot + theme(plot.background = element_rect(colour = "gray", fill=1, alpha = 0.3, size=2))
#  }
  
  p[[i]] <- dplot+geom_point(na.rm=TRUE)+
    geom_smooth(method="lm", se=TRUE, na.rm=TRUE)+
    annotate("text", x=.5, y=500, label= cor, size=3.5)
}

do.call(grid.arrange, p)

cor(train_data$heart_disease_mortality_per_100k, train_data$econ__pct_civilian_labor)

#Correlation of combo probabilities
cname <- colnames(train_data)
cname <- cname[like(cname,"p_")]
cname <- cname[!like(cname, "health_")]

par(mfrow=c(4,3))

p = list()

for (i in 1:length(cname)) {
  dplot <- ggplot(train_data, 
                  aes_string(y="heart_disease_mortality_per_100k",
                             x=cname[i]))+
    ylab("hrt_mort")+
   # xlab(substr(cname[i], 11, 100))+
    xlab(cname[i])+
    xlim(0,.25)
  
  cor <- round(cor(data.frame(train_data$heart_disease_mortality_per_100k,
                              train_data[,cname[i]]),
                   use="complete.obs")[1,2],3)
  cor <- paste("corr =", as.character(cor))
  
  p[[i]] <- dplot+geom_point(na.rm=TRUE)+
    geom_smooth(method="lm", se=TRUE, na.rm=TRUE)+
    annotate("text", x=.2, y=500, label= cor, size=3)
}

do.call(grid.arrange, p)

#normalize combo probabilities and check correlation
obsm_mean <- mean(train_data$p_obese_smoke, na.rm=TRUE)
dism_mean <- mean(train_data$p_diab_smoke, na.rm=TRUE)
diob_mean <- mean(train_data$p_diab_obese, na.rm=TRUE)
all_3_mean <- mean(train_data$p_all_three, na.rm=TRUE)

obsm_sd <- sd(train_data$p_obese_smoke, na.rm=TRUE)
dism_sd <- sd(train_data$p_diab_smoke, na.rm=TRUE)
diob_sd <- sd(train_data$p_diab_obese, na.rm=TRUE)
all_3_sd <- sd(train_data$p_all_three, na.rm=TRUE)

train_data$p_obese_smoke_norm <- (train_data$p_obese_smoke - obsm_mean) / obsm_sd
train_data$p_diab_smoke_norm <- (train_data$p_diab_smoke - dism_mean) / dism_sd
train_data$p_diab_obese_norm <- (train_data$p_diab_obese - diob_mean) / diob_sd
train_data$p_all_three_norm <- (train_data$p_all_three - all_3_mean) / all_3_sd

cname <- colnames(train_data)
cname <- cname[like(cname,"p_")]
cname <- cname[like(cname, "_norm")]

par(mfrow=c(4,3))

p = list()

for (i in 1:length(cname)) {
  dplot <- ggplot(train_data, 
                  aes_string(y="heart_disease_mortality_per_100k",
                             x=cname[i]))+
    ylab("hrt_mort")+
    # xlab(substr(cname[i], 11, 100))+
    xlab(cname[i])
#    xlim(0,.25)
  
  cor <- round(cor(data.frame(train_data$heart_disease_mortality_per_100k,
                              train_data[,cname[i]]),
                   use="complete.obs")[1,2],3)
  cor <- paste("corr =", as.character(cor))
  
  p[[i]] <- dplot+geom_point(na.rm=TRUE)+
    geom_smooth(method="lm", se=TRUE, na.rm=TRUE)+
    annotate("text", x=.2, y=500, label= cor, size=2.5)
}

do.call(grid.arrange, p)

#combine education types
train_data <- train_data %>% 
  mutate(pct_HS = demo__pct_adults_less_than_a_high_school_diploma + demo__pct_adults_with_high_school_diploma) %>%
  mutate(pct_College = demo__pct_adults_with_some_college + demo__pct_adults_bachelors_or_higher)

cname <- c("pct_HS", "pct_College")

par(mfrow=c(4,3))

p = list()

for (i in 1:length(cname)) {
  dplot <- ggplot(train_data, 
                  aes_string(y="heart_disease_mortality_per_100k",
                             x=cname[i]))+
    ylab("hrt_mort")+
    # xlab(substr(cname[i], 11, 100))+
    xlab(cname[i])+
    xlim(0,1)
  
  cor <- round(cor(data.frame(train_data$heart_disease_mortality_per_100k,
                              train_data[,cname[i]]),
                   use="complete.obs")[1,2],3)
  cor <- paste("corr =", as.character(cor))
  
  p[[i]] <- dplot+geom_point(na.rm=TRUE)+
    geom_smooth(method="lm", se=TRUE, na.rm=TRUE)+
    annotate("text", x=.8, y=500, label= cor, size=2.5)
}

do.call(grid.arrange, p)

#Histograms for demo rates
plot <- ggplot(train_data,
               aes(x=heart_disease_mortality_per_100k))+xlab("heart_mort_100k")+
  geom_histogram(binwidth = (high_mort-low_mort)/50)
plot <- plot+facet_wrap(~ demo__death_rate_per_1k, ncol = 5)
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_mean, color = "blue")
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_med, color = "red")
plot <- plot + annotate("text", x=hd_mean+40, y=40, label="<- mean", size=2.5, color = "blue")
plot <- plot + annotate("text", x=hd_med-40, y=40, label="median ->", size=2.5, color = "red")
plot


#Histograms by air pollution
plot <- ggplot(subset(train_data, !is.na(air_pollution)),
               aes(x=heart_disease_mortality_per_100k))+xlab("heart_mort_100k")+
  geom_histogram(binwidth = (high_mort-low_mort)/40)
plot <- plot+facet_wrap(~ air_pollution, drop=TRUE, ncol = 3)
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_mean, color = "blue")
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_med, color = "red")
plot <- plot + annotate("text", x=hd_mean+40, y=80, label="<- mean", size=2.5, color = "blue")
plot <- plot + annotate("text", x=hd_med-40, y=80, label="median ->", size=2.5, color = "red")
plot

#Box plot by air_pollution
plot <- ggplot(subset(train_data, !is.na(air_pollution)),
               aes(x=air_pollution, y=heart_disease_mortality_per_100k))+xlab("")+
  geom_boxplot()+ geom_hline(linetype="dashed", yintercept = hd_mean, color = "blue")
plot <- plot+geom_hline(linetype="dashed", yintercept = hd_med, color = "red")
plot <- plot + theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1)) + ggtitle("Air Pollution (mcg/m^3)")
plot <- plot + annotate("text", x=.5, y=hd_mean+30, label="mean", size=3, color = "blue", angle=90)
plot <- plot + annotate("text", x=5.5, y=hd_med-30, label="median", size=3, color = "red", angle=90)
plot

#Summary for air_pollution
econ <- rxSummary(data=train_data, heart_disease_mortality_per_100k ~ air_pollution)
econ.df <- econ$categorical[[1]]
write.csv(econ.df, file="air_stats.csv", row.names=FALSE)

med.vec <- c()
typo <- levels(train_data$air_pollution)
for (i in 1:length(typo)) {
  hm_df <- train_data %>% subset(air_pollution == typo[i])
  med.vec[i] <- median(hm_df$heart_disease_mortality_per_100k)
}

med.vec

#Histogram for air pollution
plot <- ggplot(subset(train_data, !is.na(health__air_pollution_particulate_matter)),
               aes(x=health__air_pollution_particulate_matter))+
  geom_histogram(binwidth=1) + xlab("air pollution (mcg/m3)")
plot <- plot + theme(axis.text.x = element_text(size=20))
plot

plot <- ggplot(subset(train_data, !is.na(air_pollution)),
               aes(x=air_pollution))+
  geom_bar() + xlab("air pollution (mcg/m3)")
plot <- plot + theme(axis.text.x = element_text(size=20))
plot

#Histogram for medical prof
plot <- ggplot(subset(train_data, !is.na(health__pop_per_primary_care_physician)),
               aes(x=health__pop_per_primary_care_physician))+
  geom_histogram(binwidth=1000) + xlab("")
plot <- plot + theme(axis.text.x = element_text(size=20))
plot

plot <- ggplot(subset(train_data, !is.na(health__pop_per_dentist)),
               aes(x=health__pop_per_dentist))+
  geom_histogram(binwidth=1000) + xlab("")
plot <- plot + theme(axis.text.x = element_text(size=20))
plot

plot <- ggplot(subset(train_data, !is.na(primary_care_physician)),
               aes(x=primary_care_physician))+
  geom_bar()# + xlab("primary car physicians")
#plot <- plot + theme(axis.text.x = element_text(size=20))
plot

plot <- ggplot(subset(train_data, !is.na(dentist)),
               aes(x=dentist))+
  geom_bar()# + xlab("")
#plot <- plot + theme(axis.text.x = element_text(size=20))
plot

#Histograms by physician
plot <- ggplot(subset(train_data, !is.na(primary_care_physician)),
               aes(x=heart_disease_mortality_per_100k))+xlab("heart_mort_100k")+
  geom_histogram(binwidth = (high_mort-low_mort)/40)
plot <- plot+facet_wrap(~ primary_care_physician, drop=TRUE, ncol = 3)
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_mean, color = "blue")
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_med, color = "red")
plot <- plot + annotate("text", x=hd_mean+40, y=120, label="<- mean", size=2.5, color = "blue")
plot <- plot + annotate("text", x=hd_med-40, y=120, label="median ->", size=2.5, color = "red")
plot

#Box plot by physician
plot <- ggplot(subset(train_data, !is.na(primary_care_physician)),
               aes(x=primary_care_physician, y=heart_disease_mortality_per_100k))+xlab("")+
  geom_boxplot()+ geom_hline(linetype="dashed", yintercept = hd_mean, color = "blue")
plot <- plot+geom_hline(linetype="dashed", yintercept = hd_med, color = "red")
plot <- plot + theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1)) + ggtitle("Air Pollution (mcg/m^3)")
plot <- plot + annotate("text", x=.5, y=hd_mean+30, label="mean", size=3, color = "blue", angle=90)
plot <- plot + annotate("text", x=3.5, y=hd_med-30, label="median", size=3, color = "red", angle=90)
plot

#Summary for physician
econ <- rxSummary(data=train_data, heart_disease_mortality_per_100k ~ primary_care_physician)
econ.df <- econ$categorical[[1]]
write.csv(econ.df, file="physician_stats.csv", row.names=FALSE)

med.vec <- c()
typo <- levels(train_data$primary_care_physician)
for (i in 1:length(typo)) {
  hm_df <- train_data %>% subset(primary_care_physician == typo[i])
  med.vec[i] <- median(hm_df$heart_disease_mortality_per_100k)
}

med.vec

#Histograms by dentist
plot <- ggplot(subset(train_data, !is.na(dentist)),
               aes(x=heart_disease_mortality_per_100k))+xlab("heart_mort_100k")+
  geom_histogram(binwidth = (high_mort-low_mort)/40)
plot <- plot+facet_wrap(~ dentist, drop=TRUE, ncol = 3)
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_mean, color = "blue")
plot <- plot+geom_vline(linetype="dashed", xintercept = hd_med, color = "red")
plot <- plot + annotate("text", x=hd_mean+40, y=120, label="<- mean", size=2.5, color = "blue")
plot <- plot + annotate("text", x=hd_med-40, y=120, label="median ->", size=2.5, color = "red")
plot

#Box plot by dentist
plot <- ggplot(subset(train_data, !is.na(dentist)),
               aes(x=dentist, y=heart_disease_mortality_per_100k))+xlab("")+
  geom_boxplot()+ geom_hline(linetype="dashed", yintercept = hd_mean, color = "blue")
plot <- plot+geom_hline(linetype="dashed", yintercept = hd_med, color = "red")
plot <- plot + theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1)) + ggtitle("Air Pollution (mcg/m^3)")
plot <- plot + annotate("text", x=.5, y=hd_mean+30, label="mean", size=3, color = "blue", angle=90)
plot <- plot + annotate("text", x=5.5, y=hd_med-30, label="median", size=3, color = "red", angle=90)
plot

#Summary for dentist
econ <- rxSummary(data=train_data, heart_disease_mortality_per_100k ~ dentist)
econ.df <- econ$categorical[[1]]
write.csv(econ.df, file="dentist_stats.csv", row.names=FALSE)

med.vec <- c()
typo <- levels(train_data$dentist)
for (i in 1:length(typo)) {
  hm_df <- train_data %>% subset(dentist == typo[i])
  med.vec[i] <- median(hm_df$heart_disease_mortality_per_100k)
}

med.vec

#Correlation among log death rates
cname <- c("log_homicide_per_100k", "log_motor_vehicle_death_per_100k")

par(mfrow=c(3,4))

p = list()

for (i in 1:length(cname)) {
  dplot <- ggplot(train_data, 
                  aes_string(y="heart_disease_mortality_per_100k",
                             x=cname[i]))+
    ylab("hrt_mort")
    #    xlab(substr(cname[i]), 9, 100)) #+
  #xlim(-1.5,2.5)
  
  cor <- round(cor(data.frame(train_data$heart_disease_mortality_per_100k,
                              train_data[,cname[i]]),
                   use="complete.obs")[1,2],3)
  cor <- paste("corr =", as.character(cor))
  
#  mn <- log(mean(train_data[,cname[i]], na.rm=TRUE))
  
  p[[i]] <- dplot+geom_point(na.rm=TRUE)+
    geom_smooth(method="lm", se=TRUE, na.rm=TRUE)+
    annotate("text", x=0.5, y=500, label= cor, size=2.5)
#    scale_x_log10()
}

do.call(grid.arrange, p)

#Correlation for regular death rates
cname <- c("health__homicides_per_100k", "health__motor_vehicle_crash_deaths_per_100k")

par(mfrow=c(3,4))

p = list()

for (i in 1:length(cname)) {
  dplot <- ggplot(train_data, 
                  aes_string(y="heart_disease_mortality_per_100k",
                             x=cname[i]))+
    ylab("hrt_mort")+
      xlab(substr(cname[i], 9, 100)) #+
  #xlim(-1.5,2.5)
  
  cor <- round(cor(data.frame(train_data$heart_disease_mortality_per_100k,
                              train_data[,cname[i]]),
                   use="complete.obs")[1,2],3)
  cor <- paste("corr =", as.character(cor))
  
  #  mn <- log(mean(train_data[,cname[i]], na.rm=TRUE))
  
  p[[i]] <- dplot+geom_point(na.rm=TRUE)+
    geom_smooth(method="lm", se=TRUE, na.rm=TRUE)+
    annotate("text", x=0.5, y=500, label= cor, size=2.5)
  #    scale_x_log10()
}

do.call(grid.arrange, p)
