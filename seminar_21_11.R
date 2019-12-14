"Нелинейные модели
Reset-test

ai = bo + b1*x + b2*x^2 + b3*x^3 + ... + bk*x^k

H0: b2=b3=..=bk=0
Для каждой b посчитать статистику и сделать вывод

Построить график scatterplot

Нелинейная модель:
  y=f(тета,x)+эпсилонi, эпсилон распределено нормально N(0,сигма^2)

Есть 5 подходов
1)Polynomial regression
yi = bo + b1*x + b2*x^2 + b3*x^3 + ... + bk*x^k (аппроксимирует любую нелинейную зависимость)
главное слишком не переобучить модель, чтобы модель получилась слишком нелинейной
но нет строгих правил, какую степень полинома брать, делается на глаз

при росте степени полинома растет нелинейность
можем все еще пользоваться МНК (можем делать замену переменных - на линейные предикторы)

2)Step-functions

X разбиваем на кусочки ---|c1--|c2--  - для каждого интервала будет своя константа (пересечение) как и при дамми переменных

функцция индикатор 
Co(x)=I{x<c1} (1, y<c1 и 0 otherwise)
c1(x)=I{c1<=x<c}
ck(x)=I{ck<=c}

c0(x)+...+ck(x)=1

yi=b0+b1c1(xi)+..+bkck(xi)+ei (пропустили b0 как и в случае дамми переменных)

интерпретация:
xi < c1 => y_hat = b0

cj <= c <= cj+1 => y_hat = b0 + bj (+ x*гамма)

интерпртеация относительно базового уровня (как и при дамми) - превышение икса над базовым уровнем

На сколько интервалов дробить? Чем больше их - те более нелинейная модель. Оптимальное количество интервалом можно выбирать кросс-валидацией (как и в полиномной регрессии)

3)Base functions approach (Обобщение первых двух)

b1(x) ... bk(x)
bi(x) - известные произвольные функции (в полиноме выбираем в качестве нее полином, а в кусочной - индикаторы)
yi=b0+b1c1(xi)+..+bkck(xi)+ei

polynom: bj(x)=x
step: bj(x)=I{cj<=ci<=cj+1}
ln(y)=b0+b1*ln(x1) + b2*ln(x2) + ei - для примера - выбрали в качестве функции логарифм для оценки методом МНК
ln(xi)=ki

4) Regression Splines
На оси есть точки где происходят структурные сдвиги (например, пенсионный возраст на графике зависимости зарплаты от возраста - два разных полинома - до и после 65 лет - может быть даже разрыв)
p-swice polynim - кусочно линейный полином - сплайный должны быть всюду дифференцируемы

yi=b0+b11*xi+b21*xi^2+b31*xi^3+ei, xi<=c
yi=b0+b12*xi+b22*xi^2+b32*xi^3+ei, xi>c

условие дифференцируемости - regression spline
чем больше узловых точек - тем больше степень нелинейности

5) Local linear regression

отобрали три икса и выбираем все наблюдения из окрестности искса и строим обычную линейную регрессию для этого интервала
так же делаем для второй окрестности икса и рисуем линию регрессии и потом просто эти линии собираем (в примере по трем точкам получили что-то вроде параболы)
(если есть пустота между иксама, то просто продлеваем линию регрессии)


на экзамене:
1)линейная регрессия
2)все предпосылки и что делать есть они нарушаются (первые две - самые объемные темы)
3)нелинейная регрессия
4)непараметрическая регрессия
5)метод главных компонент
"
#non-linear models
install.packages('ISLR')
library('ISLR')
data(Wage)
fit = lm(wage~poly(age,4), data=Wage)
coef(summary(fit))
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2])
preds = predict(fit, newdata = list(age = age.grid),se = TRUE)
preds
se.bands = cbind(preds$fit + preds$se.fit * 2, preds$fit - preds$se.fit*2 ) 
#(2 - стандартная ошика, в проверке гипотез делили бы еще на корень из n)
plot(Wage$age, Wage$wage, xlim = agelims, cex=.5, col = 'darkgrey')
lines(age.grid, preds$fit, lwd = 2, col= 'blue')
matlines(age.grid, se.bands, lwd = 1, col = 'red', lty=3)
#ANOVA test (сравниваем разные модели)
fit.1 = lm(wage ~ poly(age, 1), data = Wage)
fit.2 = lm(wage ~ poly(age, 2), data = Wage)
fit.3 = lm(wage ~ poly(age, 3), data = Wage)
fit.4 = lm(wage ~ poly(age, 4), data = Wage)
fit.5 = lm(wage ~ poly(age, 5), data = Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)
# лучше использовать модель с полиномом 4 степени - ее p-value чуть больше 0.05 - оринетируемся на последюю, у которой есть какой-то прирост

install.packages('splines')
library('splines')
fit = lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
summary(fit)
preds = predict(fit, newdata = list(age = age.grid), se = TRUE)
plot(Wage$age, Wage$wage, xlim = agelims, col = 'darkgrey')
lines(age.grid, preds$fit, lwd = 2)
lines(age.grid, preds$fit + 2 * preds$se.fit, lty = 'dashed')
lines(age.grid, preds$fit - 2 * preds$se.fit, lty = 'dashed')

data(Auto)
summary(Auto)
fit.1 = lm(mpg~poly(acceleration, 1), data=Auto)
fit.2 = lm(mpg~poly(acceleration, 2), data=Auto)
fit.3 = lm(mpg~poly(acceleration, 3), data=Auto)
fit.4 = lm(mpg~poly(acceleration, 4), data=Auto)
fit.5 = lm(mpg~poly(acceleration, 5), data=Auto)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)



