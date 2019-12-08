#1
install.packages("lmtest")
library("lmtest")
df <- read.table("Desktop/data.txt", header=TRUE)
df <- lapply(df, function(x) as.numeric(gsub(",", ".", gsub("\\.", "", as.character(x)))))
df <- as.data.frame(df)
str(df)
df['Y_log']<- log(df['Y'])
df['X1_log']<- log(df['X1'])
df['X2_log']<- log(df['X2'])
df['X3_log']<- log(df['X3'])
df['X4_log']<- log(df['X4'])
model1 <- lm(Y_log ~ X2_log, data = df)
summary(model1)
dwtest(model1)
sum(abs(predict(model1) - df['Y_log'])/(predict(model1)*19))*100
#Все коэффициенты значимы
#Оценка остаточной дисперсии = 0.07812
#Средняя ошибка аппроксимации = 1.683106%
#Исправленный коэффициент детерминации = 0.7225
#Статистика Дарбина-Уотсона = 0.77954 - положительная корреляция случайных регрессионных остатков
exp(model1$coefficients[1])
exp(model1$coefficients[2])
#С ростом цены цеплят на 1% спрос на них в среднем увеличивается на 1.74% - это противоречит экономическому смыслу
#В модели не учитывается инфлиция и рост среднедушевых доходов населения за рассматриваемые 20 лет
#Модель экономически не интерпретируема
model2 <- lm(Y_log ~ X1_log, data = df)
summary(model2)
dwtest(model2)
sum(abs(predict(model2) - df['Y_log'])/(predict(model2)*19))*100
#Все коэффициенты значимы
#Оценка остаточной дисперсии = 0.03781
#Средняя относительна ошибка аппроксимации = 0.7533616%
#Исправленный коэффициент детерминации = 0.935
#Статистика Дарбина-Уотсона = 0.7529 - положительная корреляция случайных регрессионных остатков
exp(model2$coefficients[1])
exp(model2$coefficients[2])
#При увеличении среднедушевого дохода на 1% потребление цеплят в среднем растёт на 1.33%
#Но в модели не учитвается динамика цены за рассматриваемые 20 лет
model3 <- lm(Y_log ~ X1_log + X2_log, data = df)
model3
sum(abs(predict(model3) - df['Y_log'])/(predict(model3)*19))*100
summary(model3)
dwtest(model3)
#Все коэффициенты значимы
#Оценка остаточной дисперсии = 0.02988
#Средняя относительна ошибка аппроксимации = 0.6019894%
#Исправленный коэффициент детерминации = 0.9594
#Статистика Дарбина-Уотсона = 1.5656 - отсутствие автокоррелированности отстатков
#F-статистика = 213.7 больше критического 3.59, что означает значимость уравнения регрессии
exp(model3$coefficients[1])
exp(model3$coefficients[2])
exp(model3$coefficients[3])
#С ростом среднедушевого дохода на 1% при неизменной стоимости цыплят их потребление в среднем увеличивается на 1.516%
#Увеличение стоимости цыплят на 1% при неизменном среднедушевом доходе приводит к увеличению потребления в среднем на 0.737%
model4 <- lm(Y_log ~ X2_log + X3_log + X4_log, data = df)
summary(model4)
dwtest(model4)
sum(abs(predict(model4) - df['Y_log'])/(predict(model4)*19))*100
#Коэффициент X3_log незначим
#Оценка остаточной дисперсии = 0.03772
#Средняя относительна ошибка аппроксимации = 0.7583258%
#Исправленный коэффициент детерминации = 0.9353
#Статистика Дарбина-Уотсона = 1.2348 - отсутствие автокоррелированности отстатков
exp(model4$coefficients[1])
exp(model4$coefficients[2])
exp(model4$coefficients[3])
exp(model4$coefficients[4])
#При неизменной стоимости двух товарозаменителей увеличение на 1% стоимости цыплят приводит к
#увеличению их потребления в среднем на 0.614%, а увеличение стоимости свинины или говядины на 1% при
#при неизменности цен на остальные входящие в модель продукты проводит к росту потреблеия цыплят
#в среднем на 1.267973 и 1.584158 соответственно.
#Работоспособны модели функций потребления, спроса-потребления и спроса с учетом цен на товарозаменители, но не функции спроса
#Так как из исходных данных нельзя узнать влияние инфляционных процессов на среднедушевые доходы населения,
#то наиболее экономически содержательными являются модели спроса-потребления и спроса с учетом цен на товарозаменители, 
#что подтверждается лучшими из значений статистических показателей - исправленного коэффициента детерминации и статистики Дарбина-Уотсона
#2
fit <- lm(Y~poly(X1,4), data=df)
coef(summary(fit))
income.lims <- range(df$X1)
income.grid <- seq(from=income.lims[1], to=income.lims[2])
preds <- predict(fit, newdata = list(X1 = income.grid),se = TRUE)
preds
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
#Наиболее хорошая их всех полиномиальных моделей - модель пятой степень, так как она имеет наибольшую степень из значимых моделей
#3
d1 <- density(c(3,5,7,9,9,10,11,13,17,19), kernel="epanechnikov")

plot(d1)
dens_5 <- approx(d1$x, d1$y, xout=5)$y
abline(v = dens_10, col = "red")

plot(d1)
dens_10 <-  approx(d1$x, d1$y, xout=10)$y
abline(v = dens_5, col = "blue")


d2 <- density(c(3,5,7,9,9,10,11,13,17,19), kernel="gaussian")

plot(d2)
dens_5 <- approx(d2$x, d2$y, xout=5)$y
abline(v = dens_10, col = "red")

plot(d1)
dens_10 <- approx(d2$x, d2$y, xout=10)$y
abline(v = dens_5, col = "blue")
#4
install.packages('ISLR')
library('ISLR')
data(Smarket)
tendency <- as.numeric(Smarket[,9]) - 1
str(tendency)
smarket <- Smarket[1:8]
str(smarket)
cor(smarket, method = "pearson")
model_bin<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            family = binomial(link = 'logit'),
            data=Smarket)
summary(model_bin)

fitted<-predict(model_bin,type = 'response')
min(fitted)
fitted.res<-ifelse(fitted>0.5,1,0)
fitted.res
str(as.numeric(fitted.res))
as.numeric(summary(fitted.res == tendency)[["TRUE"]])/length(tendency)
#5
install.packages("psych")
library("psych")
hdi.initial <- read.table("Desktop/hdi.txt", header=TRUE)
str(hdi.initial)
describe(hdi.initial)
#переменные имеют разный разброс - при применении метода главных комнонент их нужно стандартизировать - мы ищем переменные наибоее информативно описывающие разницу hdi между странами
hdi.num <- lapply(hdi.initial, function(x) as.numeric(gsub(",", ".", gsub("\\.", "", as.character(x)))))
hdi <- as.data.frame(hdi.num[3:5])
cor(hdi)
#присутствуют довольно сильно коррелирующие перменные, например R(sub2,sub3)=0.9022894, а остальные коэффициенты окноло 0.6: R(sub1,sub2) = 0.5918565, R(sub1,sub3) = 0.6425139.
hdi.pca <- prcomp(hdi, scale=TRUE)
hdi.pca 
pca1 <- hdi.pca$x[,1]
#первый столбик новых индексов развития челвоеческого потенциала
v1 <- hdi.pca$rotation[,1]
#веса первой комноненты - веса с которыми старые переменные входят в новую синтетическую переменную
v1
#наиболее важные переменная в первой компоненте - sub2 и sub3 - их вес -0.5982757 и -0.6095540 соответсвенно. Немногим меньше вес sub1 равный -0.5201059.
summary(hdi.pca)
#первая главная комнонента объясняет 0.8113 (81.13%) совокупной дисперсии исходного набора данных, а первая и вторая в сумме объясняют 96.83% 
cor(hdi.num$hdi, pca1)
#результат сравнения полученного результата и фактического значения индекса - 
#очень высокая - практически функциональная корреляция (с коэффициентом корреляции равном -0.98) - между индексом развития человеческого потенциала и первой главной комнонентой
plot(hdi.pca)
#доля главной комноненты объясняемая разными компонентами
biplot(hdi.pca, xlim=c(-0.1, 0.35))
#на графике по оси абсцисс отложена первая главная комнонента, а по оси ординат - вторая главная компонента
#можно увидеть наблюдения которые существенно отличаются от всех остальных - например 108 и 107
#первая главная комнонента включает в себя с большим весом все три переменные
#также можно наблюдать корреляцию между переменными - видна сильна корреляция между sub2 и sub3
#(если данные хорошо кластеризуются, то можно получить несколько кластеров)


