setwd("D:/Statistica")

library(dplyr)

# mosaicplot

# таблица данных 
patients <- rbind(c(18, 7), c(6, 13))
# подпись строк и столбцов
colnames(patients) <- c("Yes", "No")
rownames(patients) <- c("Placebo", "Aspirin")

mosaicplot(patients, color=T, shade=T, ylab="Thrombosis", 
           xlab="Group", cex.axis=1, main="")


# Напишите функцию most_significant, которая получает на вход dataframe 
# с произвольным количеством переменных, где каждая переменная это 
# нуклеотидная последовательность.
# Для каждой переменной мы можем проверить нулевую гипотезу о том,
# что все нуклеотиды (A, T, G, C) встречаются равновероятно внутри 
# этой последовательности. Однако, возможно, что в некоторых 
# последовательностях распределение частоты встречаемости каждого 
# нуклеотида отличается от равномерного.
# Функция должна возвращать вектор с названием переменной (или переменных), 
# в которой был получен минимальный p - уровень значимости при проверке 
# гипотезы о равномерном распределении нуклеотидов при помощи 
# критерия хи - квадрат. 

?read.csv

test_data <- read.csv(
  "https://stepic.org/media/attachments/course/524/test_data.csv", 
  stringsAsFactors = T)


str(test_data)
class(test_data)

?which

most_significant <-  function(x){
  p.val <<- sapply(x, function(y) chisq.test(table(y))$p.value)
  p.val.min <<- names(which(p.val == min(p.val)))
  #result <<- names(p.val.min)
}

#----ИЛИ-----

most_significant  <- function(test_data){    
  chisq_tests <- sapply(test_data, function(col) chisq.test(table(col))$p.value)    
  min_p  <- which(chisq_tests == min(chisq_tests))    
  return(colnames(test_data)[min_p])
}


most_significant(test_data)


# Создайте новую переменную important_cases - фактор с двумя градациями 
# ("No" и "Yes"). Переменная должна принимать значение Yes, если для данного 
# цветка значения хотя бы трех количественных переменных выше среднего. 
# В противном случае переменная important_cases  будет принимать значение No.


data("iris")

# Решение с четыре строчки

# 1. найти среднее по переменным
sapply(iris[,1:4], mean)

# 2. сравнить построчно каждое значение со средним
x <- t(apply(iris[,1:4], 1, function(x){
  ifelse(x <= sapply(iris[, 1:4], mean), 0, 1)}))

# 3. подсчитать сколько колич. перменных превышают среднее в строке
s <- apply(x, 1, sum)

# 4. создать факторную переменную, сравнить >=3 ? если да - "Yes", нет - "No"
iris$important_cases <- as.factor(ifelse(apply(x, 1, sum) >= 3, "Yes", "No"))
      
# В одну строчку:

iris$important_cases <- as.factor(ifelse(apply(t(apply(iris[,1:4], 1, function(x){
  ifelse(x <= sapply(iris[, 1:4], mean), 0, 1)})), 1, sum) >= 3, "Yes", "No"))
                                                

str(iris$important_cases)
table(iris$important_cases)

# решение в 2 строчки

importance_calc <- function(v1, v2, threshold=3){    
  ifelse(sum(v1 > v2) >= threshold, 'Yes', 'No')}    
iris$important_cases <- factor(apply(iris[1:4], 1, 
                                     importance_calc, 
                                     v2 = colMeans(iris[, 1:4])))


# Напишем функцию get_important_cases, которая принимает на вход 
# dataframe с произвольным числом количественных переменных 
# (гарантируется хотя бы две переменные). Функция должна возвращать 
# dataframe с новой переменной - фактором important_cases.
# Переменная  important_cases принимает значение Yes, если для данного 
# наблюдения больше половины количественных переменных имеют значения 
# больше среднего. В противном случае переменная important_cases 
# принимает значение No.

test_data <- data.frame(V1 = c(16, 21, 18), 
                        V2 = c(17, 7, 16), 
                        V3 = c(25, 23, 27), 
                        V4 = c(20, 22, 18), 
                        V5 = c(16, 17, 19))

test_data_1 <- as.data.frame(list(V1 = c(21, 23, 15, 18, 22, 14, 21), 
                                  V2 = c(22, 27, 19, 19, 22, 19, 18), 
                                  V3 = c(26, 24, 20, 25, 21, 11, 20), 
                                  V4 = c(6, 20, 18, 23, 11, 25, 24), 
                                  V5 = c(22, 16, 25, 15, 19, 25, 27)))


get_important_cases <- function(x){
  num_x <- x[sapply(x, is.numeric)]
  x$important_cases <- factor(ifelse(apply(t(apply(num_x, 1, function(x){
    ifelse(x > sapply(num_x, mean), 1, 0)})), 1, sum) > floor(ncol(num_x)/2), "Yes", "No"),
    levels  = c("Yes", "No"))
  return(x)
}


# второй вариант

get_important_cases  <- function(d){    
  m <-  colMeans(d)    
  compare_to_means <- apply(d, 1, function(x) as.numeric(x > m))    
  is_important <- apply(compare_to_means, 2, sum) > ncol(d)/2    
  is_important <- factor(is_important, levels = c(FALSE, TRUE), labels = c('No', 'Yes'))    
  d$important_cases <- is_important    
  return(d)
}

get_important_cases(test_data)

str(y)



# Напишите функцию stat_mode, которая получает на вход вектор из чисел 
# произвольной длины и возвращает числовой вектор с наиболее часто
# встречаемым значением. Если наиболее часто встречаемых значений несколько, 
# функция должна возвращать несколько значений моды  в виде числового вектора.

# table, max, which, names и as.numeric

?table

stat_mode <- function(x){
  as.numeric(names(which(table(x) == max(table(x)))))
}


# напишем функцию max_resid, которая получает на вход dataframe с двумя 
# переменными: типом лекарства и результатом его применения. 
# Drugs - фактор с тремя градациями: drug_1, drug_2, drug_3.     
# Result - фактор с двумя градациями: positive, negative.
# Функция должна находить ячейку таблицы сопряженности с максимальным 
# значением стандартизированного остатка и возвращать вектор из двух 
# элементов: название строчки и столбца этой ячейки.
# Для расчета стандартизированных остатков вы можете воспользоваться уже 
# знакомой вам функцией chisq.test(). Изучите справку по этой функции, 
# чтобы найти, где хранятся стандартизированные остатки.

d <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv",
              stringsAsFactors = T)

library(dplyr)

max_resid <- function(x){
  stat <- sapply(x, function(y) chisq.test(table(y))$stdres)
  stat_vec <- unlist(stat)
  #x <- dimnames(stat_vec)
  col_n <- colnames(table(x))[which(stat_vec == max(stat_vec))]
  row_n <- rownames(table(x))[which(stat_vec == max(stat_vec))]
  #rownames(table(x))[which(stat_vec == max(stat_vec), arr.ind = T)]
  c(row_n, col_n)
}

max_resid(d)

?chisq.test
?dimnames


stat <- sapply(d, function(y) chisq.test(table(y))$stdres)
stat_vec <- unlist(stat)
class(stat_vec)

#Основной способ визуализировать распределение частот номинативной
# переменной - гистограмма частот (барплот)
# Используя библиотеку ggplot2 и встроенные данные diamonds, постройте 
# график распределения частот переменной color, на котором за цвет заполнения 
# столбиков отвечает переменная cut. Сохраните код графика в переменную obj.
# В итоге должен получиться вот такой график.

library(ggplot2)
data("diamonds")

#разбиение бара на подбары: position = 'dodge'
obj <- ggplot(diamonds, aes(color, fill = factor(cut))) +
  geom_bar(position = 'dodge')
# не выводите график на печать




         