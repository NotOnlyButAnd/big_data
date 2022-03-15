# выгружаем полученную таблицу в переменную websites
websites <- read.csv(file = "C:/Users/s0153478/Downloads/websites.csv", header = TRUE, sep = ",", encoding = "UTF-8")
websites
rm(websites)

web_col <- colnames(websites)
web_col[3]

df_1 <- data.frame(row.names = c("minim", "maxim", "meanin"))
df_1


# библ для работы add_column()
# library(tibble)
# add_column(df_1, t, .names(web_col[i]))
for (i in 3:13)
{
  t <- c(min(websites[i]), max(websites[i]), max(websites[i]))
  
}
df_1
