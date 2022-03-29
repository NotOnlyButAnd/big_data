# выгружаем полученную таблицу в переменную websites
websites <- read.csv(file = "C:/Users/s0153478/Downloads/websites.csv", header = TRUE, sep = ",", encoding = "UTF-8")
websites
rm(websites)

web_col <- colnames(websites)
web_col[3]

df_1 <- data.frame(row.names = c("minim", "maxim", "meanin"))
df_1

# 1. Мин, макс, сред по столбцам
min_vec <- apply(websites[3:13],2,min)
max_vec <- apply(websites[3:13],2,max)
mean_vec <- apply(websites[3:13],2,mean)
min_vec
max_vec
mean_vec

# Кол-во людей, отдавших предпочтение (>0.7 and <0.3)
love_vec <- apply(websites[3:13],2,function(x) length(x[x>0.7]))
love_vec

not_love_vec <- apply(websites[3:13],2,function(x) length(x[x<0.3]))
not_love_vec

sort_love <- sort(love_vec, TRUE)
sort_love

sort_not_love <- sort(not_love_vec)
sort_not_love

# график рейтинга
par(mar = c(5, 10, 4, 2))
barplot(sort_love, horiz = TRUE, las = 1, main = "Рейтинг сайтов (по предпочтениям >0.7)")

barplot(sort(mean_vec), horiz = TRUE, las = 1, 
        main = "Рейтинг сайтов (по среднему значению оценки)",
        xlim = c(0,1))

hist(websites$gmail.com, col = "green", breaks = seq(0, 1, 0.05))


#### Глупые методы поиска миин маакс меан
# библ для работы add_column()
# library(tibble)
# add_column(df_1, t, .names(web_col[i]))
# for (i in 3:13)
# {
#   temp_v1 <- c(websites[i])
#   df_1[web_col[i]] = c(min(websites[i]), max(websites[i]), mean(temp_v1))
# }
# df_1
# 
# temp_v <- c(websites[5])
# temp_v
# websites[5]
# mean(websites[2])
# min(websites[5])
# 
# for (i in websites[3:13])
# {
#   x <- c(min(i), max(i), mean(i))
#   df_1[web_col[i]] = x
# }



