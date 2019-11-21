install.packages('robustbase')
install.packages('ggplot2')
install.packages('psych')
install.packages('lmtest')
install.packages('sandwich')
library('robustbase')
library('psych')
library('ggplot2')
library('lmtest')
library('sandwich')
#Task 1
edu <- education
str(edu)
edu$Region <- as.factor(edu$Region)
edu$Region <- factor(edu$Region, levels=c('1','2','3','4'),labels=c('Северо-восток','Север','Юг','Запад'))
describe(edu, omit=TRUE)
#Task 2
model <- lm(data=edu, Y~X1)
summary(model)
confint(model, level = 0.95)
#Task 3
ggplot(data = edu, aes(x=X1, y=Y))+geom_point()+geom_smooth(method=lm, se=FALSE) + labs(x = "Количество жителей в расчете на тысячу жителей, проживающих в урбинизированных райнорах", y = "Государственные расходы на образование", title = 'График регрессионной модели Y~X1')
#Task 4
model2 <- lm(data=edu, Y~X1+X2+X3)
summary(model2) 
confint(model2)
#Task 5
disp.analysis <- anova(model2)
sum.of.sqr <- disp.analysis['Sum Sq'];
disp.analysis['Var Explained'] <- sum.of.sqr / sum(sum.of.sqr)
disp.analysis['Var Explained']
#Task 6
edu <- education
reg <- edu$Region
reg <- factor(reg, levels=c('1','2','3','4'),labels=c('Северо-восток','Север','Юг','Запад'))
reg <- relevel(reg, ref='Запад')
model3 <- lm(data=edu, Y~X1+X2+X3+reg)
summary(model3)
reg <- relevel(reg, ref='Юг')
model3 <- lm(data=edu, Y~X1+X2+X3+reg)
summary(model3)
#Task 7
pred <- suppressWarnings(predict.lm(model, interval = "prediction"))
help(predict.lm)
lwr <- pred[,'lwr']
upr <- pred[, 'upr']
qplot(data = edu, X1, Y, main='График прогноза и доверительных интервалов для регрессионной модели Y~X1', xlab='Количество жителей в расчете на тысячу жителей, проживающих в урбинизированных райнорах',ylab='Государственные расходы на образование') +geom_point()+ stat_smooth(method='lm')+geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")
#Task 8
plot(model3)
#Task 9
bptest(model3)
#Task 10
coeftest(model3)
coeftest(model3,vcov. = vcovHC(model3,type="HC1"))
coeftest(model3,vcov. =vcovHC(model3,type="HC2"))

  
  