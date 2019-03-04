cols <- colnames(train_data)[like(colnames(train_data), "p_")]
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

corr <- data.frame(round(data.frame(train_data %>% select(heart_disease_mortality_per_100k, cols) %>%
  cor(use="complete.obs")),3))
corr <- cbind(cols, corr)

colnames(corr) <- c("type", "cor")
corr <- corr %>% arrange(desc(abs(cor)))

corr %>% grid.table()

cor <- data.frame(cor=c_corr)
rownames(cor) <- cols
cor

cor(data.frame(train_data$heart_disease_mortality_per_100k, 
               train_data$p_diab_obese), use="complete.obs")