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
  col_n <- colnames(table(x))[which(stat_vec == max(stat_vec))]
  row_n <- rownames(table(x))[which(stat_vec == max(stat_vec))]
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




# logistic regression


# Task 1.
# Write a get_coefficients function that receives input 
# to a dataframe with two variables x (a factor with two variable gradations) 
# and y (a factor with two gradations). The function builds a logistic model,
# where y is the dependent variable and x is the independent variable, 
# and returns a vector with the values of the model's coefficients.

test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")

test_data <- transform(test_data, x = factor(x), y = factor(y))


get_coefficients <- function(dataset){
  model <- glm(y ~ x, dataset, family = 'binomial')
  return(exp(coef(model)))
  
}

get_coefficients(test_data)

# Task 2
# write a centered function that takes as input a dataframe and variable
# names that need to be centered as described above. The function must 
# return the same dataframe, only with the specified variables centered.

library(dplyr)

test_data_1 <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")

var_names = c("X4", "X2", "X1")


centered <- function(test_data, var_names){
  result <- mutate_at(test_data, var_names, function(x){
    x - mean(x)
  })
  return(result)
  
}

centered(test_data_1, var_names)


# Task 3
# Write a get_features function that takes a baggage data set as input.
# Builds a logistic regression where dependent variable is whether baggage 
# was forbidden and predictors are other variables and returns a vector 
# with names of statistically significant variables (p < 0.05) 
# (in a model without interaction). If there are no significant predictors 
# in the data, the function returns a string with the message 
# "Prediction makes no sense".

get_features <- function(dataset){
  fit <- glm(is_prohibited ~ ., dataset, family = "binomial")
  result <- anova(fit, test = "Chisq")["Pr(>Chi)"]
  p.v <<- result < 0.05
  if (all(p.v == F, na.rm = T)){
    print("Prediction makes no sense")
    
  } else {
    
    return(colnames(dataset)[p.v])
  }

}

class(p.v)

data_get_feat <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv", 
                          stringsAsFactors = T)
str(data_get_feat)
get_features(data_get_feat)


data_get_feat_1 <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_2.csv",
                        stringsAsFactors = T)
#str(data_get_feat_1)
get_features(data_get_feat_1)


# Task 4
# Write a function that takes two sets of data as input. The first dataframe,
# as in the previous task, contains information about the baggage already 
# inspected (prohibited or not, weight, length, width, type of bag).

# The second set of data is information about new baggage that is being 
# scanned right now. The data also contains information: weight, length, 
# width, type of bag and passenger's name (see the description of the 
# variables in the example).

# In total, your function takes two sets of data and returns the name of 
# the passenger with the most suspicious baggage. If multiple passengers 
# received the maximum probability value, then return a vector with 
# multiple names.

train <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv",
                      stringsAsFactors = T)
str(train)

data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv",
                            stringsAsFactors = T)
str(data_for_predict)

most_suspicious <- function(test_data, data_for_predict){
  fit <- glm(is_prohibited ~ ., test_data, family = "binomial")
  new_data <- predict(fit, newdata = data_for_predict, type="response")
  ind <- which(new_data == max(new_data))
  return(data_for_predict$passangers[ind])
  
}

most_suspicious(train, data_for_predict)


# Task 5
# Write a normality_test function that receives a dataframe with an
# arbitrary number of variables of different types (quantitative, strings, 
# factors) as input and checks the normality of the distribution of 
# quantitative variables. The function must return a vector of p-significance 
# values for the shapiro.test test for each scale variable.

data("iris")
test <- read.csv("https://stepic.org/media/attachments/course/524/test.csv")

normality_test <- function(dataset){
  x <- dataset[sapply(dataset, is.numeric)]
  test <- sapply(x, function(var){ shapiro.test(var)$p.value })
  return(test)
  
}


normality_test(iris)
normality_test(test)



# Task 6
# Write a smart_anova function that takes a dataframe with two variables 
# x and y as input. Variable x is a quantitative variable, variable y is a 
# factor, divides the observations into three groups.

# If the distributions in all groups are not significantly different from 
# normal, and the variances in the groups are homogeneous, the function 
# should compare the three groups using analysis of variance and return a 
# named vector with p-value, element name is "ANOVA".

# If at least in one group the distribution is significantly different 
# from normal or the variances are not homogeneous, the function compares 
# the groups using the Kruskal-Wallis test and returns a named vector with 
# a p-value, the vector name is "KW".

# The distribution will be considered significantly deviated from the 
# normal if p < 0.05 in the shapiro.test() test.

# Dispersions will be considered non-homogeneous if p < 0.05 in the
# bartlett.test() test.

smart_test <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv",
                       stringsAsFactors = F)


smart_anova <- function(test_data){
  norm_test <<- aggregate(x ~ y, test_data, function(x){shapiro.test(x)$p.value})
  homogeneous <<- bartlett.test(x ~ y, test_data)$p.value
  if ( all(norm_test > 0.05) & homogeneous > 0.05){
    fit <- aov(x ~ y, test_data)
    #p_value <- summary(fit)[[1]]$'Pr(>F)'[1]
    anov <- c(ANOVA = summary(fit)[[1]]$'Pr(>F)'[1])
    return(anov)
  } else {
    kw <- c(KW = kruskal.test(x ~ y, test_data)$p.value)
    return(kw)
  }
  
}


smart_anova(smart_test)
