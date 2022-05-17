library(rvest)
# при создании df_2014 ругался что нет такого пакета, попробовал его установить вот так
# после этого датафрейм был создан
install.packages("vctrs")
library("vctrs")

# Норвегия, Германия, Британия, Греция, Англия
# Norway, Germany, United Kingdom, Greece, -
# на сайте https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2015&region=154
# есть только United Kingdom и Ireland. Отдельно Англия там не представлена

# читаем html 2014 года
url = read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2014')

# достаем узлы-таблицы. нам нужна с id="t2", т.е. [2]
nodes = html_nodes(url, 'table'); nodes[2]

# закидываем таблицу в датафрейм
df_2014 = html_table(nodes[2]) %>% as.data.frame(); df_2014

# почему-то не прочитался стробец Rank (заполнился NA), но это по факту номер строки
# поэтому его просто убираем
df_2014 = df_2014[][2:11]
df_2014


# выбираем из датафрейма только те строки, которые нам нужны
df_2014_new = rbind(df_2014[df_2014$Country == "Norway", ],
                    df_2014[df_2014$Country == "Germany", ],
                    df_2014[df_2014$Country == "United Kingdom", ],
                    df_2014[df_2014$Country == "Greece", ])
# чтобы просто были пронумерованы строки, а не как в изначальном датафрейме по рангам
# хотя может это потом и пригодится
rownames(df_2014_new) <- c(1,2,3,4)
df_2014_new
