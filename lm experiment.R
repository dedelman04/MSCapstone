p = seq(0.1, 0.9, 0.1)

try_lm <- function(p){
  idx <- createDataPartition(model_data$heart_disease_mortality_per_100k,
                             times = 1,
                             p = p,
                             list=FALSE)
  
  tr <- model_data[-idx, ]
  ts <- model_data[idx, ]
  
  fit <- lm(formula = heart_disease_mortality_per_100k ~ ., data = tr)
  y_hat <- predict(fit, ts)
  #list(p, fit$, sqrt(mean((y_hat - ts$heart_disease_mortality_per_100k)^2)))
  sqrt(mean((y_hat - ts$heart_disease_mortality_per_100k)^2))
}

rmse <- replicate(1000, sapply(p, try_lm, simplify=TRUE))

which.min(cbind(data.frame(p), data.frame(rsme=rowMeans(rmse)))[,2])
  ggplot(aes(x=p, y=rmse))+geom_point()+geom_line()
