#1
install.packages("lmtest")
library("lmtest")
#Указать путь к файлу с данными
df <- read.table("Desktop/data.txt", header=TRUE)
df <- lapply(df, function(x) as.numeric(gsub(",", ".", gsub("\\.", "", as.character(x)))))
df <- as.data.frame(df)

#Создаем переменные - значения логарифмов от изначальных переменных
for (var_name in c('Y', paste('X',1:4,sep = ""))){
  var_log_name <- paste(var_name,'log', sep='_')
  df[var_log_name] <- log(df[var_name])
}

#1)Функция спроса
model1 <- lm(Y_log ~ X2_log, data = df)
#2)Функция потребления
model2 <- lm(Y_log ~ X1_log, data = df)
#3)Функция спроса-потребления
model3 <- lm(Y_log ~ X1_log + X2_log, data = df)
#4)Функция спроса с учетом цены на товары-заменители
model4 <- lm(Y_log ~ X2_log + X3_log + X4_log, data = df)

analyse <- function(model){
  print(summary(model))
  print(dwtest(model, alternative = "greater"))
  cat("Average relative error of approximation:", sum(abs(predict(model) - df['Y_log'])/(predict(model)*length(df$t)))*100, "%\n\n")
  for (var_name in names(model$coefficients)){
    print(exp(model$coefficients[var_name]))
  }
}

for (model in list(model1, model2, model3, model4)){
  analyse(model)
}

#2
fit <- lm(Y~poly(X1,4), data=df)
coef(summary(fit))
income.lims <- range(df$X1)
income.grid <- seq(from=income.lims[1], to=income.lims[2])
preds <- predict(fit, newdata = list(X1 = income.grid),se = TRUE)
se.bands <- cbind(preds$fit + preds$se.fit * 2, preds$fit - preds$se.fit*2 ) 
plot(df$X1, df$Y, xlim = income.lims, cex=.5, col = 'darkgrey')
lines(income.grid, preds$fit, lwd = 2, col= 'blue')
matlines(income.grid, se.bands, lwd = 1, col = 'red', lty=3)
#ANOVA test (сравниваем разные модели)
fit.1 <- lm(Y ~ poly(X1, 1), data = df)
fit.2 <- lm(Y ~ poly(X1, 2), data = df)
fit.3 <- lm(Y ~ poly(X1, 3), data = df)
fit.4 <- lm(Y ~ poly(X1, 4), data = df)
fit.5 <- lm(Y ~ poly(X1, 5), data = df)
fit.6 <- lm(Y ~ poly(X1, 6), data = df)
anova(fit.1,fit.2,fit.3,fit.4,fit.5,fit.6)
#3
d1 <- density(c(3,5,7,9,9,10,11,13,17,19), kernel="epanechnikov")

plot(d1)
dens_5 <- approx(d1$x, d1$y, xout=5)$y
abline(v = dens_5, col = "red")

plot(d1)
dens_10 <-  approx(d1$x, d1$y, xout=10)$y
abline(v = dens_10, col = "blue")

d2 <- density(c(3,5,7,9,9,10,11,13,17,19), kernel="gaussian")

plot(d2)
dens_5 <- approx(d2$x, d2$y, xout=5)$y
abline(v = dens_5, col = "red")

plot(d2)
dens_10 <- approx(d2$x, d2$y, xout=10)$y
abline(v = dens_10, col = "blue")
#4
install.packages('ISLR')
library('ISLR')
data(Smarket)
tendency <- as.numeric(Smarket[,9]) - 1
smarket <- Smarket[1:8]
cor(smarket, method = "pearson")
model_bin<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               family = binomial(link = 'logit'),
               data=Smarket)
summary(model_bin)

fitted<-predict(model_bin,type = 'response')
fitted
fitted.res<-ifelse(fitted>0.5,1,0)
as.numeric(summary(fitted.res == tendency)[["TRUE"]])/length(tendency)
#5
install.packages("psych")
library("psych")
hdi.initial <- read.table("Desktop/hdi.txt", header=TRUE)
str(hdi.initial)
describe(hdi.initial)
hdi.num <- lapply(hdi.initial, function(x) as.numeric(gsub(",", ".", gsub("\\.", "", as.character(x)))))
hdi <- as.data.frame(hdi.num[3:5])
cor(hdi)
hdi.pca <- prcomp(hdi, scale=TRUE)
hdi.pca 
pca1 <- hdi.pca$x[,1]
v1 <- hdi.pca$rotation[,1]
v1
summary(hdi.pca)
cor(hdi.num$hdi, pca1)
plot(hdi.pca)
biplot(hdi.pca, xlim=c(-0.1, 0.35))

