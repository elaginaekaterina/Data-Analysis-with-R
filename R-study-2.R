setwd("D:/Statistica/Задания на R")

# скрипт apply1.R 
# скрипт apply2.R

# Напишите функцию get_negative_values, которая получает на вход dataframe 
# произвольного размера. Функция должна для каждой переменной в данных 
# проверять, есть ли в ней отрицательные значения. Если в переменной 
# отрицательных значений нет, то эта переменная нас не интересует, 
# для всех переменных, в которых есть отрицательные значения мы сохраним
# их в виде списка или матрицы, если число элементов будет одинаковым 
# в каждой переменной.


get_negative_values <- function(test_data){
  test_data[is.na(test_data)] <- 0
  neg_val <<- apply(test_data, 2, function(x) x[x < 0])
  result <<- neg_val[sapply(neg_val,length, simplify = TRUE) > 0]
}

test_data_1 <- as.data.frame(list(V1 = c(-9.7, -10, -10.5, -7.8, -8.9), 
                                V2 = c(NA, -10.2, -10.1, -9.3, -12.2), 
                                V3 = c(NA, NA, -9.3, -10.9, -9.8)))

test_data <- as.data.frame(list(V1 = c(NA, -0.5, -0.7, -8), 
                                V2 = c(-0.3, NA, -2, -1.2), 
                                V3 = c(1, 2, 3, NA)))
y <- get_negative_values(test_data_1)

dim(test_data)


# Напишите функцию na_rm которая заменяет все пропущенные значения в 
# столбцах dataframe на соответствующее среднее значение. То есть все NA 
# в первом столбце заменяются на среднее значение первого столбца 
# (рассчитанного без учета NA). Все NA второго столбца заменяются на
# среднее значение второго столбца и т.д. 




na_rm  <- function(x){
  x[is.na(x)] <- mean(x, na.rm = T)
  return(x)
}

test_data <- as.data.frame(apply(test_data_2, 2, na_rm))

class(test_data_2)

na_rm  <- function(x){    
  na_to_mean  <- function(v){    
    v[is.na(v)]  <- mean(v , na.rm = T)    
    return(v)}    
  result  <- as.data.frame(apply(x, 2, na_to_mean))}

test_data_2 <- as.data.frame(list(V1 = c(NA, NA, NA, NA, 13, 12, 9, 10, 8, 9,
                                         11, 11, 10, 12, 9), 
                                  V2 = c(NA, 12, 8, NA, 11, 11, 9, 8, 8, 10, 
                                         10, 11, 10, 10, 10), 
                                  V3 = c(NA, 5, NA, 13, 12, 11, 11, 14, 8, 12,
                                         8, 8, 10, 10, 8), 
                                  V4 = c(10, 10, 10, 10, 13, 10, 11, 7, 12, 
                                         10, 7, 10, 13, 10, 9)))
y <- na_rm(test_data_2)
y
class(y)


# Напишите функцию positive_sum, которая получает на вход dataframe с 
# произвольным количеством числовых переменных. Основная задача функции - 
# найти сумму положительных значений в каждой переменной 
# и сохранить их в список.

positive_sum <-  function(test_data){
  lapply(test_data, function (x) sum(x[x > 0], na.rm = T)) 
}

d <- data.frame(X1 = c(-1, -2, 0), X2 = c(10, 4, NA), X3 = c(-4, NA, NA))
positive_sum(d)


# Напишите функцию my_names, которая получает на вход  датафрейм и вектор 
# с именами тех генов, для которых мы хотим отобрать наблюдения 
# уровня экспрессии.

data <- as.data.frame(list(name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", 
                                         "p7@HPS4", "p7@HPS5", "p9@HPS6", 
                                         "p11@HPS7", "p10@HPS8", "p15@HPS9"), 
                                expression = c(118.84, 90.04, 106.6, 104.99, 
                                               93.2, 66.84, 90.02, 108.03, 
                                               111.83)))

names = c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")

my_names <- function (dataset, names){
  x <- sapply(dataset, function(x) grepl(paste(names, collapse = "|"), x))
  dataset <- as.data.frame(dataset[x,])
}

my_names <- function (dataset, names){    
  gs=gsub('^.*\\@','',dataset[,1])    
  return(dataset[gs %in% names,])}

y <- my_names(data, names)


# создать в данных новую числовую переменную is_outlier, которая 
# будет принимать значение 1, если наблюдение в этой строке является 
# выбросом в своей группе, и 0, если не является.
# Под выбросами будем понимать наблюдения, отклоняющиеся от среднего 
# значения в группе более чем на два стандартных отклонения этой группы. 

data("ToothGrowth")


find_outliers <- function(t){
  factor_vars <- names(which(sapply(t, is.factor)))
  num_var <- names(which(sapply(t, is.numeric)))
  #y <- function(x){
    #ifelse(x < mean(x) - 2 * sd(x) | x > mean(x) + 2 * sd(x), 1, 0)
  #}
  t %>% 
    group_by_(.dots = factor_vars) %>% 
    #mutate(vars(num_var), is_outlier = y)
    mutate_(is_outlier = interp(~ifelse(var < mean(var) - 2 * sd(var) 
                                       | var > mean(var) + 2 * sd(var), 1, 0), 
                             var = as.name(num_var)))
}


ToothGrowth$dose <- factor(ToothGrowth$dose)
find_outliers(ToothGrowth)


# Напишите функцию smart_lm, которая получает на вход data.frame с
# произвольным числом количественных переменных. Первая колонка в данных - 
# это зависимая переменная, все остальные - предикторы. На первом этапе вы 
# должны отобрать предикторы для модели.

# Функция возвращает в виде вектора коэффициенты линейной регрессии 
# построенной только для отобранных предикторов (условие нормальности
# распределения). Если таких предикторов в данных не оказалось, то 
# функция возвращает предупреждение "There are no normal variables in the data".

data("swiss")

?as.formula

library(nortest)

smart_lm <- function(x){
  #x <<- as.list(x)
  dependent_variable <<- select(x, 1)
  y <<- names(which(sapply(x, shapiro.test)['p.value',] > 0.05 ))
  y <<- y[y != names(x[1])]
  if (length(y) == 0){
    print("There are no normal variables in the data")
  } else {
    predictors <<- x[, y]
    dataset <<- data.frame(dependent_variable, predictors)
    fit <- lm(dataset[[1]] ~ ., dataset[-1])$coefficients
    return(fit)
  }
  
}

#   ИЛИ   #

smart_lm <- function(x){    
  check_norm <- sapply(x[-1], function(var) shapiro.test(var)$p.value > 0.05)    
  if (any(check_norm)){    
    x = x[, c(1, (which(check_norm) + 1))]    
    coef <- lm(x[[1]] ~ ., x[-1])$coef    
    return(coef)    
  } else{    
    return('There are no normal variables in the data')}}


shapiro.test(as.vector(test_data_1$V2))

smart_lm(swiss)

test_data_1 <- as.data.frame(list("V1" = c(20.5, 21.1, 20.6, 20, 20,
                                                      22.9, 20.8, 20.7, 17.3, 
                                                      22, 21.8, 22.9, 17.9, 
                                                      23.2, 17.1, 20, 17.4, 
                                                      21.5, 21.4, 19.5, 17.3, 
                                                      20.4, 19, 21.2, 20.9, 
                                                      22.6, 20.8, 20.3, 19.9, 
                                                      20.3), 
                                               "V2" = c(21.1, 20, 20.9, 23.3, 
                                                      20.6, 17.2, 20.4, 20.4, 
                                                      16.7, 17.3, 18.9, 20.5, 
                                                      19.3, 18.3, 20.3, 20.8, 
                                                      18.2, 22.8, 18.8, 17.6,
                                                      21, 24.3, 22, 18.4, 21.7, 
                                                      23.9, 19.4, 18.7, 
                                                      17.9, 19)))

smart_lm(test_data_1)




# Напишите функцию one_sample_t, которая получает на вход два аргумента:
# 1. Dataframe произвольного размера с произвольным числом переменных 
# различного типа.
# 2. Числовое значение среднего в генеральной совокупности.
# Ваша функция должна применять одновыборочный t - test к каждой числовой 
# переменной в данных, и сравнивать среднее значение этой переменной с 
# указанным значением среднего в генеральной совокупности 
# (второй аргумент функции).
# Функция должна возвращать список, где каждый элемент это вектор, 
# состоящий из t - значения, числа степеней свобод (df) и значения p - value.


one_sample_t <- function(test_data, general_mean){
  x <<- test_data[sapply(test_data, is.numeric)]
  t_test <<- lapply(x, function(y){
                        return(c(t.test(y,mu=general_mean)$statistic, 
                                 t.test(y,mu=general_mean)$parameter,
                                 t.test(y,mu=general_mean)$p.value))
                      })
   return(t_test)
}

data("iris")
test_data <- test_data <- as.data.frame(list(V1 = c(33, 37, 39, 44, 34, 44, 62, 
                                                    33, 36, 55), 
                                             V2 = c("A", "B", "B", "B", "A", 
                                                    "A", "B", "B", "A", "A"), 
                                             V3 = c(41, 45, 50, 40, 26, 32, 39, 
                                                    42, 39, 51), 
                                             V4 = c(40, 53, 52, 43, 50, 48, 25,
                                                    55, 34, 31), 
                                             V5 = c(48, 42, 35, 51, 39, 38, 53, 
                                                    40, 34, 39), 
                                             V6 = c("A", "A", "A", "B", "B", 
                                                    "B", "A", "A", "B", "B")))
one_sample_t(test_data, 9)


# Итак, ваша задача, написать функцию get_p_value, которая получает на вход 
# список (назовем его главным списком), каждый элемент этого списка тоже 
# список - результат выполнения функции shapiro.test 
# (смотри пример normality_tests). Ваша задача из каждого элемента главного 
# списка вытащить только p - value. В итоге функция возвращает список где
# каждый элемент - одно значение - p - value (как в примере normality_tests_p).


data("iris")

test_1 <- shapiro.test(iris$Sepal.Length)
test_2 <- shapiro.test(iris$Sepal.Width)
test_3 <- shapiro.test(iris$Petal.Length)
test_4 <- shapiro.test(iris$Petal.Width)

normality_tests <- list(test_1, test_2, test_3, test_4)

get_p_value <- function(test_list){
  result <<- sapply(test_list, function(y){ '['(y, 2)})
  return(result)
}

#   ИЛИ 
get_p_value = function(test_data){    
  sapply(test_data, '[', 2)}
 
get_p_value(normality_tests)



# dplyr
library(dplyr)
library(ggplot2)

data("diamonds")

d <- slice(diamonds, seq(1, nrow(diamonds), 2))


# Из данных mtcars отберите только четыре переменные: mpg, hp, am, vs. 
# Оставьте только те наблюдения, для которых значения mpg > 14 и hp > 100. 
# Отсортируйте получившиеся данные по убыванию переменной mpg и возьмите 
# только первые 10 строчек. Переменную mpg переименуйте в Miles per gallon, 
# а переменную hp в  Gross horsepower (обратите внимание, dplyr позволит нам 
# создать пременные с пробелами в названии). Получившийся dataframe сохраните 
# в переменную my_df.

data("mtcars")

my_df <- mtcars %>% 
  select('mpg', 'hp', 'am', 'vs') %>% 
  filter(mpg > 14, hp > 100) %>% 
  arrange(desc(mpg)) %>% 
  slice(1:10) %>% 
  rename('Miles per gallon' = mpg, 'Gross horsepower' = hp)


# Напишите функцию, all_to_factor, которая преобразует dataframe, 
# переводя все его переменные в фактор.

all_to_factor <- function(x){
  mutate_each(x, funs(as.factor(.)))
}



# задача написать функцию, которая получает на вход dataframe  с 
# произвольным числом переменных разных типов. На первом этапе функция 
# должна выполнить предобработку числовых переменных. Т.к. значение 
# логарифма мы можем рассчитать только для положительных чисел. 
# Для этого сделаем центрирование всех переменных (Rescaling), 
# только еще добавим единичку, чтобы у нас не осталось нулей.

test_data <- as_data_frame(list(V1 = c(1.5, -0.1, 2.5, -0.3, -0.8),
                             V2 = c(-0.9, -0.3, -2.4, 0.0, 0.4),
                             V3 = c(-2.8, -3.1, -1.8, 2.1, 1.9),
                             V4 = c('A', 'B', 'B', 'B', 'B')))

log_transform <- function(test_data){
    mutate_if(test_data, 
              is.numeric, 
              funs(log(((. - min(.))/(max(.) - min(.))) + 1)))
}

y <- log_transform(test_data)
y
?mutate_if 



#############_________ТЕОРИЯ______#############

# Если имена переменных в данных неизвестны
# SE & NSE

library(lazyeval)
num_var <- names(which(sapply(mini_mtcars, is.numeric)))
mutate_(mini_mtcars, new_var = interp(~(var - mean(var)) / sd(var), var = as.name(num_var)))

library(lazyeval)
var_for_group <- c("am", "vs")
var_for_filter <- "hp"
var_for_arrange <- "mpg"
var_for_mutate <- "qsec"
var_for_summirise <- "cyl"
group_by_(mtcars, .dots = var_for_group) %>% 
  filter_(interp(~var > 100, var = as.name(var_for_filter))) %>% 
  arrange_(var_for_arrange) %>% 
  mutate_(new_var = interp(~ifelse(var > mean(var), 1, 0), 
                           var = as.name(var_for_mutate))) %>% 
  summarise_(max = interp(~max(var), var = as.name(var_for_summirise)))


# сбор итоговой статистики для столбцов совпад. по имени или попадающ.
# под определенные китерии (с помощью вспомогат ф-ии vars):

by_species %>% summarise_at(vars(Petal.Width, Sepal.Width), funs(min, max))
by_species %>% summarise_at(vars(matches("Width")), funs(min, max))

# select_if() & summarise_if()

select_if(iris, is.numeric) %>% 
  summarise_all(funs(sd))


by_species %>% 
  summarise_if(function(col){ if(!is.numeric(col)) 
    return (FALSE) else mean(col)>2}, mean)


# задача будет написать функцию descriptive_stats, которая рассчитывает 
# основные описательные статистики в каждой группе наблюдений для описанного 
# выше примера. Функция получает на вход dataframe с тремя переменными 
# salary - значение заработной платы, gender - фактор с двумя градациями 
# (male, female), country - фактор с двумя градациями (England, France).
# Функция должна возвращать dataframe с описательными статистиками и 
# количеством NA, рассчитанными в каждой группе: количеств наблюдений,
# среднее значение, стандартное отклонение, медиана, первый квартиль, 
# третий квартиль, число пропущенных значений.

salary_data <- read.csv("https://stepic.org/media/attachments/course/724/salary.csv")


descriptive_stats <- function (dataset){
  group_by(dataset, gender, country) %>%
    summarise(
      n = n(),
      mean = mean(salary, na.rm = T),
      sd = sd(salary, na.rm = T),
      median = median(salary, na.rm = T), 
      first_quartile = quantile(salary, 0.25, na.rm = T), 
      third_quartile = quantile(salary, 0.75, na.rm = T), 
      na_values = sum(is.na(salary)))

}

descriptive_stats(salary_data)


# Напишите функцию, to_factors, которая получает на вход dataframe  
# с произвольным числом количественных переменных и вектор с номерами колонок, 
# которые нужно перевести в фактор.

# Для перевода числовых колонок в фактор будем использовать следующий принцип,
# если наблюдение больше среднего всей переменной то 1, иначе 0.

to_factors <- function(test_data, factors){
  y <- function(x){as.factor(ifelse(x > mean(x), 1, 0))}
  mutate_at(test_data, factors, y)

}


# ИЛИ

to_factors <- function(test_data, factors){    
  test_data[factors] <- mutate_each(test_data[factors], funs(factor(ifelse(. > mean(.), 1, 0))))    
  return(test_data)}


str(to_factors(mtcars[1:4], factors = c(1, 3)))


# Возьмем данные diamonds для работы в этой задаче. Создайте новый 
# dataframe с именем high_price, в котором будут хранится только 10 самых 
# дорогих бриллиантов каждого цвета. Также в итоговом datafrmae должны 
# храниться только две переменные color и price.



high_price <- diamonds %>% 
  group_by(color) %>% 
  select(color, price) %>% 
  arrange(desc(price)) %>% slice(1:10)

#ИЛИ

high_price <- diamonds %>% group_by(color) %>%     
  arrange(desc(price)) %>% slice(1:10) %>% select(color, price)


#____data.table________

library(data.table)

# Напишите функцию filter.expensive.available, которая принимает на вход 
# products (объект типа data.table) и вектор названий брендов, и возвращает 
# только те строчки, которые соответствуют товарам, цена которых больше или 
# равна 5000 рублей, доступны на складе, и принадлежат одному из 
# переданных брендов.

sample.products <- data.table(price = c(10000, 600000, 700000, 1000000),
                              brand = c("a", "b", "c", "d"),
                              available = c(T, T, F, T))

filter.expensive.available <- function(products, brands) {
  products[brand %in% brands & available == T & price >= 5000 * 100]
}

filter.expensive.available(sample.products, c("a", "c", "d"))

# Создайте функцию ordered.short.purchase.data, которая будет принимать
# purchases, объект data.table, и возвращать таблицу только со столбцами с 
# номером заказа и ID продукта.
# Упорядочите результат по убыванию стоимости купленного товара. 
# Возвраты (записи с отрицательным количеством предметов в позиции) 
# надо удалить.

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = 1:4,
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)

ordered.short.purchase.data <- function(purchases) {
  purchases[order(price, decreasing = T)][quantity > 0, .(ordernumber, product_id)]
                  
}

ordered.short.purchase.data(sample.purchases)

# Напишите функцию purchases.median.order.price, у которой один аргумент: 
# purchases, и которая возвращает медианную стоимость заказа (число).

# Группировку стоит проводить с помощью data.table. Записи с неположительным 
# количеством купленных товаров (возвраты) игнорировать.

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = c(1,2,2,3),
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)

purchases.median.order.price <- function(purchases) {
  purchases[quantity > 0, .(price = sum(price * quantity)), by = ordernumber][, median(price)]
}

 #    ИЛИ 
purchases.median.order.price <- function(purchases) {    
  median(purchases[quantity >= 0][, list(w = sum(price * quantity)), by=list(ordernumber)]$w)}

purchases.median.order.price(sample.purchases)


# Создайте функцию get.category.ratings, которая будет возвращать суммарный 
# оборот (с учетом скидок) каждой категории , и количество купленных предметов 
# по таблице покупок и таблице принадлежности товара к категории. 
# Если купленный товар принадлежит нескольким категориям, его необходимо 
# учитывать во всех. При решении используйте ключи.

product.category <- data.table(product_id = c(1,1,2,2,3),
                               category_id = c(1,2,1,3,3))

purchases <- data.table(product_id = c(1, 2, 3),
                        totalcents = c(100, 200, 300),
                        quantity = c(1, 1, 3))

get.category.ratings <- function(purchases, product.category) {
  setkey(purchases, product_id)
  setkey(product.category, product_id)
  res <- merge(product.category, purchases, by = "product_id")
  res[,.(totalcents = sum(totalcents), quantity = sum(quantity)), by = category_id]
  
}


get.category.ratings(purchases, product.category)


# Напишите функцию, которая будет с помощью := добавлять столбец 
# «price.portion», содержащий процент стоимости товара в заказе, с двумя
# знаками после запятой (нули после запятой не опускать). Проверяться будет 
# возвращаемая из функции таблица. Тип нового столбца - character (строка). 
# Записи с неположительным количеством товаров убрать перед расчётом.

sample.purchases <- data.table(price = c(100, 300, 50, 700, 30),
                               ordernumber = c(1,1,1,2,3),
                               quantity = c(1,1,2,1,-1),
                               product_id = 1:5)


mark.position.portion <- function(purchases) {
  purchases[quantity > 0][,"price.portion":= as.character(sprintf('%.2f', round(((price*quantity) /
                                   sum(price*quantity)) * 100, 2))),
                         by = ordernumber][]
  
}

# ИЛИ

mark.position.portion <- function(purchases) {  
  purchases <- purchases[quantity > 0]  
  purchases[, price.portion := format(round(100 * price * quantity / sum(price * quantity), 2),                                       
                                      nsmall=2,digits=2, scientific = F), by=ordernumber]}

mark.position.portion(sample.purchases)

sprintf("%.3f", round(5.2,3))

?format
?invisible



#_________Графика ggplot2________

# qplot

# Используя функцию qplot, постройте гистограмму переменной depth из 
# данных diamonds. Сохраните график в переменную depth_hist.

library(ggplot2)

data(diamonds)

depth_hist <- qplot(x = depth, data = diamonds)


# Постройте диаграмму рассеивания (scatter plot) как в указанном ниже 
# примере, результат сохраните в переменную price_carat_clarity_points.

price_carat_clarity_points <- qplot(diamonds$carat, diamonds$price, 
                                    color = diamonds$clarity)

# Используя функцию qplot, постройте график плотности переменной x 
# из данных diamonds. Сохраните график в переменную x_density.

x_density <- qplot(diamonds$x,
                   geom = "density")

# постройте график плотности переменной x для каждой группы наблюдений 
# по переменной cut из данных diamonds. Таким образом за цвет графика 
# теперь отвечает переменная cut. Сохраните результат в 
# переменную x_cut_density.

x_cut_density <- qplot(x,
                       color = cut,
                       data = diamonds,
                       geom = "density")

# построим график violin plot для переменной price в каждой группе 
# наблюдений по переменной color. Сохраните результа в переменную price_violin.

price_violin <- qplot(color, price, data = diamonds, geom = "violin")

# ggplot

# Используя данные mtcars скомбинируем два варианта отображения
# количественных данных boxplot и violin plot:
# ось x - am (нужен фактор)
# ось y - mpg
# Сохраните график в переменную my_plot.

data("mtcars")

my_plot <- ggplot(mtcars, aes(x = factor(am), y = mpg, group = factor(am))) + 
  geom_violin() +
  geom_boxplot(aes(factor(am), mpg, group = factor(am)), width = 0.2)
  #geom_violin()
  
my_plot


# Отобразите взаимосвязь между доходом (income) и числом продаж (sale),
# цветом точек указав номер магазина (shop).
# Сохраните график в переменную my_plot. Обратите внимание, что линия 
# тренда одна для всех наблюдений. Данные хранятся в переменной sales.


sales = read.csv("https://stepic.org/media/attachments/course/724/sales.csv")
str(sales)

my_plot <- ggplot(sales, aes(income, sale)) +
  geom_point(aes(col = shop), size = 2) +
  geom_smooth(col = 'blue')
  

my_plot

# При помощи функции stat_summary постройте график с доверительными 
# интервалами для демонстрации различий в доходах двух магазинов с 
# учетом времени года:
# переменная shop - ось x;
# переменная income - ось y;
# переменная season - цвет;
# geom pointrange.
# Сохраните график в переменную my_plot, дополнив предложенный код.
# Обратите внимание, что доверительные интервалы не накладываются друг на друга! 

my_plot <- ggplot(sales, aes(shop, income, col = season))+
  stat_summary(fun.data = mean_cl_boot, geom = 'pointrange',
              position = position_dodge(0.3))

my_plot


# Теперь давайте отобразим на графике различия в продажах (переменная sale), 
# в зависимости от:
# года (date) - ось x;
# и номера магазина (shop) - цвет.
# Дополните предложенный код, чтобы получился график как в примере ниже.
# Используйте функцию mean_cl_boot для построения доверительных интервалов.
# Вам также понадобится использовать три geoms: errorbar, point, line. 
# Используйте их прямо внутри функции stat_summary().


my_plot <-  ggplot(sales, aes(date, sale, col = shop))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar',
               position = position_dodge(0.5),
               width = 0.2, size = 1) + # добавим стандартную ошибку
  stat_summary(fun.data = mean_cl_boot, geom = 'point',
               position = position_dodge(0.5), size = 3) + # добавим точки
  stat_summary(fun.data = mean_cl_boot, geom = 'line',
               position = position_dodge(0.5), size = 1) # соединим линиями

my_plot



#  facet

# Используя facet_grid постройте следующий график и сохраните 
# его в переменную mpg_facet.
# ось x - переменная mpg
# facet - переменная am по строчкам и vs по столбцам

mtcars <- mutate(mtcars, 
                 am = factor(am, labels = c("A", "M")), 
                 vs = factor(vs, labels = c("V", "S")))

mpg_facet <- ggplot(mtcars, aes(mpg)) +
  geom_dotplot() +
  facet_grid(am ~ vs)

mpg_facet


# Используя данные iris, постройте график плотности для переменной 
# Sepal.Length. Разбейте график на части по переменной Species при помощи 
# facet_wrap. Результат сохраните в переменную sl_wrap.

data("iris")

sl_wrap <- ggplot(iris, aes(Sepal.Length)) +
  geom_density() +
  facet_wrap(~ Species)

sl_wrap


# Используя данные Iris, постройте график, иллюстрирующий взаимосвязь 
# переменных Sepal.Length и Sepal.Width внутри каждого вида 
# (переменной Species), при помощи facet_wrap().
# В этом задании вам потребуется использовать два geom:
# geom_point - для отображения отдельных наблюдений,
# geom_smooth - для добавления сглаживания.
# Сохраните график в переменную my_plot.

my_plot <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Species)

my_plot

# построим следующий график, чтобы выяснить есть ли различия в
# бюджетах фильмов разного жанра из года в год. Cохраните результат 
# в переменную my_plot.
# ось x - переменная Type
# ocь y - переменная Budget
# facet - переменная Year (используйте facet_grid)

myMovieData <- read.csv('myMovieData.csv')

my_plot <- ggplot(myMovieData, aes(Type, Budget)) +
  geom_boxplot() +
  facet_grid(.~Year) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

my_plot

# В этом задании мы построим график используя данные Iris. 
# Наша цель отобразить взаимосвязь переменных Sepal.Length (ось X) и 
# Petal.Length (ось Y) внутри трех групп по переменной Species. 
# Для этого постройте scaterplot, отобразите цветом значения переменной 
# Species и добавьте линейное сглаживание в каждой группе.

#Далее от вас потребуется привести график к более завершенному виду. 
# Мы переведем на русский название осей, название легенды и ее расшифровку:

# Ось X - "Длина чашелистика".
# Ось Y - "Длина лепестка".
# Название легенды - "Вид цветка".
# Расшифровка легенды: "Ирис щетинистый", "Ирис разноцветный", "Ирис виргинский".
# Также мы чуть измени отображение значений по осям.

# Значения по оси X должны начинаться с 4 и заканчиваться на 8 с шагом в единицу.
# Значения по оси Y должны начинаться с 1 и заканчиваться на 7 с шагом в единицу.

library(ggplot2)

data("iris")

iris_plot <- ggplot(iris, aes(Sepal.Length, Petal.Length, col = Species))+
  geom_point(size = 2) +
  geom_smooth()+
  scale_color_discrete(name = "Вид цветка",
                       labels = c("Ирис щетинистый", "Ирис разноцветный",
                                  "Ирис виргинский"))+
  scale_x_continuous(name = "Длина чашелистика",
                     limits = c(4, 8)) + 
  scale_y_continuous(name = "Длина лепестка", 
                     breaks = seq(1,7, 1))
iris_plot

install.packages("ggthemes")


#-----plotly----3d

teapot.coords <- fread('teapot.csv')

make.fancy.teapot <- function(teapot.coords) {
  
  i.s <- seq(0, nrow(teapot.coords) - 1, 3)
  j.s <- seq(1, nrow(teapot.coords) - 1, 3)
  k.s <- seq(2, nrow(teapot.coords) - 1, 3)
  
  plot_ly(teapot.coords, x = ~ x, y = ~y, z = ~ z, i = ~i.s, j = ~j.s, 
          k = ~k.s, type = "mesh3d")
}

make.fancy.teapot(teapot.coords)


#_______R Markdown___________

install.packages("rmarkdown")

glacier %>% 
  group_by(GEO) %>%
  #filter(MEASURE == "Annual mass balance") %>% 
  summarise(na = sum(is.na(Value)))


d <- filter(glacier, MEASURE == "Annual mass balance")
 
 
summarise(d, median(d$Value, na.rm = T)) 

median(d$Value, na.rm = T)

sum(is.na(d$Value))
