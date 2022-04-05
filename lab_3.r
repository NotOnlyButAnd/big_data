# выгружаем полученную таблицу в переменную websites
websites <- read.csv(file = "C:/Users/s0153478/Downloads/websites.csv", header = TRUE, sep = ",", encoding = "UTF-8")
websites

# выбираем только переменные
websites.variables <- websites[,3:13]
websites.variables

### Дескриптивный анализ ####
# 1 - Центральная тенденция #

# среднее
mean_vec <- apply(websites.variables,2,mean)
mean_vec

# медиана
median_vec <- apply(websites.variables,2,median)
median_vec

# Create the function для моды
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# мода
mode_vec <- apply(websites.variables,2,getmode)
mode_vec

# 2 - Стандартное отклонение #
# дисперсия
disp_vec <- apply(websites.variables,2,var);disp_vec

# стандартное отклонение
stand_otkl_vec <- apply(websites.variables,2,sd);stand_otkl_vec

# 3 - Квартили #
# минимум
min_vec <- apply(websites.variables,2,min);min_vec

# максимум
max_vec <- apply(websites.variables,2,max);max_vec

# размах
razmah_vec <- max_vec - min_vec;razmah_vec

# межквартильный размах
mezhkvar_razmah_vec <- apply(websites.variables,2,IQR);mezhkvar_razmah_vec

# здесь есть квартили
summary(websites.variables)

# строим боксплот
par(mar = c(5, 10, 4, 2))
boxplot(websites.variables, horizontal = TRUE, las = 1)

#### СОРТИРОВКА ####
# сортируем строки датафрейма по столбцу instagramm.com
websites.sorted <- websites[order(websites$instagramm.com),];websites.sorted
