setwd("D:/Statistica/Задания на R")

i <- 10
1 : i - 1 # так мы создадим последовательность от 1 до 10, 
# а потом вычтем единицу из каждого элемента
# 0 1 2 3 4 5 6 7 8 9

1 : (i - 1) # а вот так мы создадим последовательность от 1 до i - 1, то есть от 1 до 9. 
# 1 2 3 4 5 6 7 8 9


# Работа с векторами(теория)


1 : 67 # начало и конец последовательности


my_vector1 <- 1: 67

my_vector2 <- c(-32, 45, 67, 12.78, 129, 0, -65)

my_vector1[1]
my_vector1[3]

my_vector2[2]
my_vector2[c(1,2,3)]
1:3
my_vector2[1:3]

my_vector2[c(1, 5, 6, 7, 10)]


v_1 <- 1:5000
v_2 <- 7000:10000
the_best_vector <- c(v_1, v_2)


my_vector1 + 10
my_vector2 + 56

my_vector2 == 0

x <- 23
my_vector1 > 23

my_vector1 > x
x == 23


# Задачи

#В уже созданной переменной my_numbers сохранен вектор 
#из 20 целых чисел. Ваша задача создать новую переменную my_numbers_2,
#в которой будет сохранен 2, 5, 7, 9, 12, 16 и 20 элемент вектора my_numbers.

my_numbers_2 <- my_numbers[c(2, 5, 7, 9, 12, 16, 20)]


# Найдите сумму всех элементов вектора,
#которые больше 10. Сохраните сумму в переменную my_sum.
my_vector <- c(8, 13, 9, 18, 7, 2, 15, 2, 8, 18, 6, 8, 9, 6, 12, 11, 3, 1, 2, 14)
x <- 10
my_sum <- sum(my_vector[my_vector > x])



# Теория (Списки)

age <- c(16, 18, 22, 27)
is_maried <- c (F, F, T, T)
name <- c("Olga", "Maria", "Nastya", "Polina")

data <- list(age, is_maried, name)

#индесация списка
data[[1]]
data[[1]][1]         
data[[2]][3]



# Работа с data frame
df <- data.frame(Name = name, Age = age, Status = is_maried)

typeof(df)


# Задача

#В векторе  my_vector отберите только те наблюдения,
#которые отклоняются от среднего меньше чем на одно стандартное отклонение. 
#Сохраните эти наблюдения в новую переменную my_vector_2. 
#При этом исходный вектор my_vector оставьте без изменений.

my_vector <- c(21, 18, 21, 19, 25, 20, 17, 17, 18, 22, 17, 18,
                  18, 19, 19, 27, 21, 20, 24, 17, 15, 24, 24, 29,
                  19, 14, 21, 17, 19, 18, 18, 20, 21, 21, 19, 19,
                  17, 21, 13, 17, 13, 23, 15, 23, 24, 16, 17, 25, 24, 22)

my_vector_mean <- mean(my_vector)
my_vector_sd <- sd(my_vector) 

negative_sd <- my_vector[my_vector_mean - my_vector_sd]
positive_sd <- my_vector[my_vector_mean + my_vector_sd]

left <- my_vector[my_vector >= negative_sd]
right <- my_vector[my_vector <= positive_sd]


my_vector_2 <- my_vector[my_vector > mean(my_vector) - sd(my_vector) & 
  my_vector < mean(my_vector) + sd(my_vector)]


# Задачи с DF

#В этой задче поработаем со встроенными данными mtcars. 
#В датафрэйме mtcars создайте новую колонку (переменную) под названием even_gear, 
#в которой будут единицы, если значение переменной (gear) четное, 
#и нули если количество нечетное.

cars_data <- mtcars
data("mtcars")
cars_data$even_gear <- ifelse(cars_data$gear %% 2 == 1, 0, 1)
  
  
# Задача создать переменную - вектор mpg_4 и сохранить в нее значения расхода 
#топлива (mpg) для машин с четырьмя цилиндрами (cyl)

mpg_4 <- subset(cars_data$mpg, cars_data$cyl == 4)


# задача создать новый dataframe под названием mini_mtcars, 
# в котором будут сохранены только третья, седьмая, десятая, 
# двенадцатая и последняя строчка датафрейма mtcars.

mini_mtcars <- cars_data[c(3, 7, 10, 12, 32),]


# Создайте новую числовую переменную  new_var в данных mtcars, 
# которая содержит единицы в строчках, если в машине не меньше четырёх 
# карбюраторов (переменная "carb") или больше шести цилиндров (переменная "cyl"). 
# В строчках, в которых условие не выполняется, должны стоять нули.

mtcars$new_var <- ifelse(mtcars$carb >= 4 | mtcars$cyl > 6, 1, 0) 


# Если среднее значение вектора my_vector больше 20, 
# в переменную result сохраните "My mean is great",  
# если среднее значение my_vector меньше или равно 20,
# то в переменную result сохраните  строку "My mean is not so great".

my_vector <- c(20.67, 23.34, 22.65, 17.11, 22.1, 26.32, 20.39, 21.04, 23.78,
               31.11, 21.13, 22.44, 23.21, 27.02, 18.64, 20.9, 20.77, 20.0,
               21.29, 23.48, 18.47, 25.02, 17.04, 30.97, 12.91, 23.88, 32.95,
               8.46, 23.15, 21.05, 20.63, 19.95, 17.38, 29.35, 24.43, 23.66,
               18.32, 30.13, 19.36, 19.67, 24.23, 20.82, 18.21, 9.91, 21.45,
               18.04, 18.31, 17.18, 10.99, 10.06)

if (mean(my_vector) > 20){
  result <- "My mean is great"
  print(result)
} else { 
  result <- "My mean is not so great"
  print(result)
}


# Dataset AirPassengers

?AirPassengers
str(AirPassengers)

data("AirPassengers")

# задача создать переменную good_months и сохранить в нее число пассажиров
# только в тех месяцах, в которых это число больше, 
# чем показатель в предыдущем месяце.

ap <- as.vector(AirPassengers)
good_months <- c()
for (i in 1 : (length(AirPassengers) - 1)){
  if (AirPassengers[i+1] > AirPassengers[i]){
    good_months <- append(good_months, AirPassengers[i+1])
  } 
}


# решение с циклом:
  
good_months <- c()    
index <- 1    
for (i in 2:length(AirPassengers)) {    
  if (AirPassengers[i]>AirPassengers[i-1]){    
    good_months[index] <- AirPassengers[i]    
    index <- index + 1    
  }    
}

# решение без цикла:
  
good_months <- AirPassengers[-1][AirPassengers[-1] > AirPassengers[-144]]

?cumsum


# Для встроенных в R данных AirPassengers рассчитайте скользящее среднее 
# с интервалом сглаживания равным 10. Напечатайте получившийся результат 
# (первым значением в выводе должно быть среднее для элементов 1:10, 
# во втором значении - среднее для элементов 2:11 и т.д., 
# в последнем  - среднее для элементов 135 :144)

# Все полученные значения средних сохраните в переменную moving_average.


moving_average <- numeric(135) 
for (i in 1:(length(AirPassengers) - 9)){
  moving_average[i] <- mean(AirPassengers[i:(i + 9)])
}  

# с помощью кумулятивных сумм
n <- 10    
d <- AirPassengers    
cx <- c(0, cumsum(d))    
moving_average <- (cx[(n + 1):length(cx)] - cx[1:(length(cx) - n)]) / n
moving_average[1:10]

# Описательный статистики(теория)

?mtcars

df  <- mtcars

str(df)

df$vs  <- factor(df$vs  , labels = c("V", "S"))
df$am  <- factor(df$am  , labels = c("Auto", "Manual"))

result <- mean(df$qsec[df$cyl != 3 & df$mpg > 20])

# При помощи функции aggregate рассчитайте стандартное отклонение 
# переменной hp (лошадиные силы) и переменной disp 
# (вместимости двигателя)  у машин с автоматической и 
# ручной коробкой передач. 
descriptions_stat <- aggregate(cbind(hp, disp) ~ am, df, sd)


?airquality
str(airquality)

data(airquality)
AQ_subset <- subset(AQ, AQ&Month%in%c(7, 8, 9))
length(is.na(AQ_subset$Ozone))
aggregate(Ozone ~ Month, AQ_subset, length)

describeBy(airquality, group = airquality$Month)

iris <- iris

res <- describe(iris)
res['sd']

sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Width)
sd(iris$Species)

#sort(sapply(iris[iris$Species =='virginica',][-5], FUN=median))
d <- describeBy(iris[,1:4], group = iris$Species)$'virginica'['median']

describeBy(iris, group = iris$Species == 'virginica')




?replace

my_vector <- rnorm(30)

my_vector[sample(1:30, 10)] <- NA # на десять случайных позиций поместим NA

mean_x <- mean(my_vector[!is.na(my_vector)])

fixed_vector <- replace(my_vector, is.na(my_vector), mean_x)  # replace na elements with mean value

# another solution
# fixed_vector <- replace(my_vector, is.na(my_vector), mean(my_vector, na.rm = T))


# При помощи функции ggplot() или boxplot() постройте график boxplot, 
# используя встроенные в R данные airquality. По оси x отложите номер месяца, 
# по оси y — значения переменной Ozone.
# На графике boxplot отдельными точками отображаются наблюдения, 
# отклоняющиеся от 1 или 3 квартиля больше чем на полтора межквартильных размаха. 
# Сколько таких наблюдений присутствует в сентябре (месяц №9)?

airquality$Month <- factor(airquality$Month, labels = 5:9)

ggplot(airquality, aes(x = Month, y = Ozone)) + geom_boxplot(na.rm = T)


# Нужно построить scatterplot с помощью ggplot из ggplot2, 
# по оси x которого будет mpg, по оси y - disp, 
# а цветом отобразить переменную (hp).

plot1 <- ggplot(mtcars, aes(x = mpg, y = disp, col = hp)) + geom_point()


ggplot(iris, aes(Sepal.Length)) + geom_histogram(fill = Species)

ggplot(iris, aes(Sepal.Length, col = Species)) + geom_histogram()

ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram()

ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species))

ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(col = Species))


ggplot(iris, aes(x = Sepal.Length, y  = Sepal.Width, col = Species, 
                 size = Petal.Length)) + geom_point()



# Tables

?HairEyeColor
data("HairEyeColor")

dim(HairEyeColor)
dimnames(HairEyeColor)

# задача в переменную red_men сохранить долю рыжеволосых (Red) 
# от общего числа голубоглазых мужчин.

red_men <- prop.table(HairEyeColor)
red_men <- prop.table(HairEyeColor[,,'Male'], 2)['Red', 'Blue']

# количество зеленоглазых женщин:
sum(HairEyeColor[,'Green','Female'])


# Постройте столбчатую диаграмму распределения цвета глаз по цвету волос только у женщин из
# таблицы HairEyeColor. По оси X должен идти цвет волос, цвет столбиков 
# должен отражать цвет глаз. По оси Y - количество наблюдений.

library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
female <- subset(mydata, Sex == "Female")
obj <- ggplot(data = female, aes(x = Hair, y = Freq, fill = Eye)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

# На основе таблицы HairEyeColor создайте ещё одну таблицу,
# в которой хранится информация о распределении цвета глаз 
# у женщин-шатенок (Hair = 'Brown'). Проведите тест равномерности
# распределения цвета глаз у шатенок и выведите значение 
# хи-квадрата для этого теста.

female_brown <- HairEyeColor['Brown',, 'Female']
chisq.test(female_brown)

# Данные diamonds из библиотеки ggplot2
# При помощи критерия Хи - квадрат проверьте гипотезу о взаимосвязи качества 
# огранки бриллианта (сut) и его цвета (color). В переменную main_stat 
# сохраните значение статистики критерия Хи - квадрат. Обратите внимание, 
# main_stat должен быть вектором из одного элемента, а не списком (листом).

?diamonds
str(diamonds)

data("diamonds")
diam_tab <- table(diamonds$cut, diamonds$color)
chi <- chisq.test(diam_tab)
main_stat <- chi$statistic


#  При помощи критерия Хи - квадрат проверьте гипотезу о взаимосвязи
# цены (price) и каратов (carat) бриллиантов. 

factor_price = ifelse(diamonds$price >= mean(diamonds$price), 1, 0)
sum(is.na(factor_price))
factor_carat= ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0)
sum(is.na(factor_carat))
range(factor_carat)
range(factor_price)
price_carat_tab <- table(factor_price, factor_carat)
main_stat <- chisq.test(price_carat_tab)$statistic


# При помощи точного критерия Фишера проверьте гипотезу о взаимосвязи типа
# коробки передач (am) и типа двигателя (vs) в данных mtcars. Результат 
# выполнения критерия сохраните в переменную.Получившийся p - уровень 
# значимости сохраните в переменную fisher_test.

data("mtcars")
str(mtcars)


am_vs_tab <- table(mtcars$am, mtcars$vs)
fisher_test <- fisher.test(am_vs_tab)$p.value


# ToothGrowth

?ToothGrowth
str(ToothGrowth)

# Сравните среднее значение длины зубов свинок, которые потребляли 
# апельсиновый сок (OJ) с дозировкой 0.5 миллиграмм, со средним значением 
# длины зубов свинок, которые потребляли аскорбиновую кислоту (VC) 
# с дозировкой 2 миллиграмма. 
# Значение t - критерия сохраните в переменную t_stat.


df <- ToothGrowth

data <- subset(df, (supp == 'OJ' & dose == 0.5)|(supp == 'VC' & dose == 2.0))

t_stat <- t.test(len ~ supp, data)$statistic


df <- read.csv('lekarstva.csv')
t.test(df$Pressure_before, df$Pressure_after, paired = T)



# Сначала с помощью теста Бартлетта проверьте гомогенность 
# дисперсий двух выборок. В случае, если дисперсии значимо 
# не отличаются (с уровнем 0.05), примените тест Стьюдента, 
# иначе - непараметрический тест (Манна-Уитни). 

data <- read.table("dataset_11504_15.txt")

bartlett.test(V1 ~ V2, data)
wilcox.test(V1 ~ V2, data)


data_set <- read.table("dataset_11504_16.txt")

t.test(data_set$V1, data_set$V2, var.equal = F)


# Дисперсионный анализ ANOVA


?formula
?npk
data <- npk
model <- aov(yield ~ N*P, data)
summary(model)


model_1 <- aov(yield ~ N+P+K, data)
summary(model_1)


iris <- iris
fit_iris <- aov(Sepal.Width ~ Species, iris)
summary(fit_iris)

TukeyHSD(fit_iris)

# Проведите однофакторный дисперсионный анализ с повторными измерениями: 
# влияние типа таблетки (pill) на температуру (temperature) 
# с учётом испытуемого (patient).

pill_data <- read.csv("Pillulkin.csv")
patient_fac <- as.factor(pill_data$patient)
model_pil <- aov(temperature ~ pill + Error(patient_fac/pill), pill_data)
summary(model_pil)

model_pil_doc <- aov(temperature ~ pill * doctor + 
                       Error(patient_fac/(pill*doctor)), pill_data)
summary(model_pil_doc)



# график
library(ggplot2)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))


# Напишите функцию, которая выводит номера позиций пропущенных наблюдений в векторе.

# На вход функция получает числовой вектор с пропущенными значениями. 
# Функция возвращает новый вектор с номерами позиций пропущенных значений.


?which
NA.position <- function(x){
  index <- which(is.na(x))
  return(index)
}

my_vector <- c(1, 2, 3, NA, NA)

NA.position(my_vector)

# Напишите функцию NA.counter для подсчета пропущенных значений в векторе.

# На вход функция  NA.counter должна принимать один аргумент - числовой вектор. 
# Функция должна возвращать количество пропущенных значений.

NA.counter <- function(x){
  # put your code here  
  sum_na <- sum((is.na(x)))
  return(sum_na)
}

NA.counter(my_vector)


# Напишите функцию filtered.sum, которая на вход получает вектор с пропущенными, 
# положительными и отрицательными значениями и возвращает сумму 
# положительных элементов вектора.

filtered.sum <- function(x){
  sum(x[x >= 0], na.rm = T)
  # put your code here  
}

filtered.sum(c(NA, NA, NA, NA, NA, 4, 2, -2, -3, 2, 5, -3, 4, -2, 0, 0, 2, 4, 3))

# Напишите функцию outliers.rm, которая находит и удаляет выбросы. 
# Для обнаружения выбросов воспользуемся самым простым способом, 
# с которым вы не раз встречались, используя график Box plot. 

# Выбросами будем считать те наблюдения, которые отклоняются от 1 или 
# 3 квартиля больше чем на 1,5 *  IQR, где  IQR  - межквартильный размах.

# На вход функция получает числовой вектор x. Функция должна возвращать 
# модифицированный вектор x с удаленными выбросами. 



outliers.rm <- function(x){
  # put your code here
  q <- quantile(x, probs = c(0.25, 0.75))
  bad <<- 1.5 * IQR(x)
  x <- x[!(x < q[1] - bad | x > q[2] + bad)]
  return(x)
}

# альтернативное решение
outliers.rm <- function(x){    
  q <- quantile(x, 0.25) + quantile(x, 0.75)    
  return(x[abs(x - q/2) <= 2*IQR(x)])}


q <- quantile(vector, probs = c(0.25, 0.75))
iqr <- IQR(vector)

result <- outliers.rm(vector)



# Корреляция и простая линейная регрессия (МНК)

# Напишите функцию corr.calc, которая на вход получает data.frame 
# с двумя количественными переменными, рассчитывает коэффициент корреляции 
# Пирсона и возвращает вектор из двух значений: коэффициент корреляции 
# и p - уровень значимости.

library(psych)

corr.calc <- function(x){
  stat <<- cor.test(~ x[,1] + x[,2], x)
  result <- c(stat$estimate, stat$p.value)
  return(result)
}
# или
corr.calc <- function(test_data){    
  fit  <- cor.test(test_data[[1]], test_data[[2]])    
  r <- fitestimate    
 # p <- fitestimatep <− fitp.value    
  return(c(r, p))}

data("mtcars")
corr.calc( mtcars[, c(1,5)])
corr.calc( iris[,1:2] )


# Напишите функцию filtered.cor которая на вход получает data.frame 
# с  произвольным количеством переменных (как количественными, так 
# и любых других типов), рассчитывает коэффициенты корреляции Пирсона 
# между всеми парами количественных переменных и возвращает наибольшее 
# по модулю значение коэффициента корреляции. (То есть функция может вернуть 
# -0.9, если это наибольшая по модулю  корреляция).

step6 <- read.csv("step6.csv")


?apply
?sapply
?which.max

filtered.cor <- function(x){
  numeric_x <<- x[sapply(x, is.numeric)]
  paired_cor <<- cor(numeric_x)
  diag(paired_cor) <<- 0
  abs_cor <<- abs(paired_cor)
  max_cor <<- paired_cor[which.max(abs_cor)]
  return(max_cor)
}

filtered.cor(step6)

iris$Petal.Length <- -iris$Petal.Length # сделаем отрицательной максимальную по модулю корреляцию
filtered.cor(iris)


# Напишите функцию smart_cor, которая получает на вход dataframe 
# с двумя количественными переменными. Проверьте с помощью теста Шапиро-Уилка, 
# что данные в обеих переменных принадлежат нормальному распределению.

# Если хотя бы в одном векторе распределение переменной отличается 
# от нормального (p - value меньше 0.05), то функция должна возвращать
# коэффициент корреляции Спирмена. (Числовой вектор из одного элемента).

# Если в обоих векторах распределение переменных от нормального значимо 

smart_cor <- function(x){
    var_1_norm_test <<- shapiro.test(x[[1]])
    var_2_norm_test <<- shapiro.test(x[[2]])
    if ((var_1_norm_test$p.value < 0.05) | (var_2_norm_test$p.value < 0.05)){
      cor_test <<- cor.test(~ x[[1]] + x[[2]], method = "spearman")
     } else {
       cor_test <<- cor.test(~ x[[1]] + x[[2]], x)
    }
    return(cor_test$estimate)
}

# ИЛИ

smart_cor <- function(x){    
  if (shapiro.test(x[[1]])$p < 0.05 | shapiro.test(x[[2]])$p<0.05) {    
    return(cor.test(x[[1]], x[[2]], method = 'spearman')$estimate)    
  } else {    
    return(cor.test(x[[1]], x[[2]], method = 'pearson')$estimate)}}

data("iris")
test_data <- iris[,1:2]
smart_cor(test_data)


# постройте линейную регрессию, где - первая переменная - зависимая, 
# вторая - независимая. В ответ укажите значения регрессионных коэффициентов 
# сначала intercept затем  slope.

data <- read.table("dataset_11508_12.txt")

model_lm <- lm(V1 ~ V2, data)


# Только для бриллиантов класса Ideal (переменная cut) c числом карат 
# равным 0.46 (переменная carat) постройте линейную регрессию, где в 
# качестве зависимой переменной выступает price, в качестве предиктора - 
# переменная  depth. Сохраните коэффициенты регрессии в переменную fit_coef.

library(ggplot2)
data("diamonds")

df <- subset(diamonds, cut == "Ideal" & carat == 0.46)

model_df <- lm(price ~ depth, df)
fit_coef <- model_df$coefficients


# Напишите функцию regr.calc, которая на вход получает dataframe 
# c двумя переменными.
# Если две переменные значимо коррелируют (p - уровень значимости для 
# коэффициента корреляции Пирсона меньше 0.05), то функция строит 
# регрессионную модель, где первая переменная - зависимая, вторая - независимая. 
# Затем создает в dataframe новую переменную с назанием fit, где сохраняет 
# предсказанные моделью значения зависимой переменной. В результате функция 
# должна возвращать исходный dataframe с добавленной новой переменной fit.

regr.calc <- function(x){
  if (cor.test(x[[1]], x[[2]])$p.value < 0.05){
    model <- lm(x[[1]] ~ x[[2]])
    x$fit <- model$fitted.values
    return(x)
  } else {
    return("There is no sense in prediction")
  }
}

my_df = iris[,c(1,4)] # на вход подаем данные iris только с переменными Sepal.Length и Petal.Width
regr.calc(my_df)



#

library(ggplot2)
my_plot <- ggplot(iris, aes(Sepal.Width, Petal.Width, 
                            col = Species)) + geom_point() + 
  geom_smooth(method = "lm")



# Можественная линейная регрессия
# задача 1
test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")


fill_na <- function(x){
  model <- lm(x[[3]] ~ x[[1]] + x[[2]], x, na.action = "na.exclude")
  x$y_full <- ifelse(is.na(x$y), predict(model, x), x$y)
  return(x)
}

fill_na(test_data)


# задача 2

data("mtcars")
df <- mtcars[c("wt", "mpg", "disp", "drat", "hp")]
model <- lm(wt ~ mpg + disp + hp, df)
summary(model)


# задача 3
?attitude
data("attitude")
fit <- lm(rating ~ complaints * critical, attitude)
summary(fit)

# задача 4
mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
model <- lm(mpg ~ wt*am, mtcars)
summary(model)


ggplot(mtcars, aes(wt, mpg, col = am)) + 
  geom_point()  + 
  geom_smooth(method = 'lm')


# задача 5
model_full <- lm(rating ~ ., data = attitude) 

model_null <- lm(rating ~ 1, data = attitude)

ideal_model <- step(model_full, scope = list(model_null, model_full),
                    direction = 'backward')
summary(ideal_model)

# задача 6
anova(ideal_model, model_full)


# завдача 7
data('LifeCycleSavings')
model <- lm(sr ~ (pop15 + pop75+dpi+ddpi)^2, LifeCycleSavings)

# задача 8
?scale

beta.coef <- function(x){
  x <- scale(x)
  model <- lm(x[[1]] ~ x[[2]], x)
  return(model$coefficients)
}

beta.coef(mtcars[,c(1,3)])


# задача 9
normality.test  <- function(x){    
  return(sapply(x, FUN =  shapiro.test)['p.value',])}

normality.test(mtcars[,1:6])


# Диагностика модели
library(gvlma)
library(ggplot2)

data <- read.csv('homosc.csv')
model <- lm(DV ~ IV, data)
fit <- gvlma(model)
summary(fit)


# Напишите функцию resid.norm, которая тестирует распределение остатков 
# от модели на нормальность при помощи функции shapiro.test и создает 
# гистограмму при помощи функции ggplot() с красной заливкой "red", 
# если распределение остатков значимо отличается от нормального (p < 0.05), 
# и с зелёной заливкой "green" - если распределение остатков значимо не 
# отличается от нормального.

# На вход функция получает регрессионную модель. Функция возвращает переменную,
# в которой сохранен график ggplot.

resid.norm  <- function(fit){
  result <<- shapiro.test(fit$residuals)
  if (result$p.value < 0.05){
    plot <- ggplot(fit, aes(fit$residuals)) +
      geom_histogram(fill = 'red')
  }else{
    plot <- ggplot(fit, aes(fit$residuals)) +
      geom_histogram(fill = 'green')
  } 
  return(plot)
} 


# ИЛИ

resid.norm <- function(fit) {    
  resid.norm.pv <- shapiro.test(fitresiduals$residuals)['p.value',]    
plt <- ggplot(data.frame(fitmodel), aes(x = fitmodel),aes(x=fitresiduals)) +    
  geom_histogram(fill = ifelse(resid.norm.pv < 0.05, 'red', 'green'))    
return(plt)}

fit <- lm(mpg ~ disp, mtcars)
my_plot <- resid.norm(fit)


# Напишите функцию high.corr, которая принимает на вход датасет с 
# произвольным числом количественных переменных и возвращает вектор с 
# именами двух переменных с максимальным абсолютным значением 
# коэффициента корреляции.

high.corr <- function(x){
  cor_data <<- cor(x)
  diag(cor_data) <- 0
  result <- rownames(which(abs(cr)==max(abs(cr)), arr.ind=T))
  return(result)
}

#ИЛИ

high.corr <- function(x){    
  cr <- cor(x)    
  diag(cr) <- 0    
  return(rownames(which(abs(cr)==max(abs(cr)),arr.ind=T)))}

data("iris")
high.corr(iris[,-5])

x1 <- rnorm(30) # создадим случайную выборку
x2 <- rnorm(30) # создадим случайную выборку
x3  <- x1 + 5 # теперь коэффициент корреляции x1 и x3 равен единице
my_df <- data.frame(var1 = x1, var2 = x2, var3 = x3)
high.corr(my_df)


# Сохраните в переменную логистическую регрессионную модель, 
# где в качестве зависимой переменной выступает тип коробки 
# передач (am), в качестве предикторов переменные disp, vs, mpg.

# Значения коэффициентов регрессии сохраните в переменную log_coef.
data <- mtcars

model <- glm(am ~ disp + vs + mpg, data, family = 'binomial')
log_coef <- model$coefficients



# построить следующий график по данным ToothGrowth.
# Изобразите различия длины зубов морских свинок в различных 
# условиях дозировки и типа потребляемого продукта.

# По оси x - переменная supp.
# По оси y - переменная len.
# Цвет ящиков с усами (boxplot) - переменная dose.

data("ToothGrowth")
library("ggplot2")

str(ToothGrowth)
ToothGrowth$dose <- factor(ToothGrowth$dose, labels = c("0.5", "1", "2"))

obj <- ggplot(data = ToothGrowth, aes(supp, len, fill = dose)) + 
  geom_boxplot()


# предсказание числа поступивших, вероятность >= 0.4

df <- read.csv("data.csv")
str(df)

model <- glm(admit ~ rank * gpa, no_na_data, family = "binomial",
             na.action = "na.exclude")

vec <- predict(model, newdata=df[is.na(df$admit),], type = "response")

length(vec[vec>=0.4])
