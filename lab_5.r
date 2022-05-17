library(rvest)
# при создании df_2014 ругался что нет такого пакета, попробовал его установить вот так
# после этого датафрейм был создан
install.packages("vctrs")
library("vctrs")

# Норвегия, Германия, Британия, Греция, Англия
# Norway, Germany, United Kingdom, Greece, -
# на сайте https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2015&region=154
# есть только United Kingdom и Ireland. Отдельно Англия там не представлена

# т.к. страницы с уровнем жизни и их URL отличаются только годами в конце URL адреса, было принято решение
# создать функцию для избежания дублирования кода
getCountriesIndexesByYear <- function(year){
  # читаем html года year
  url = read_html(paste0('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=', toString(year)))
  
  # достаем узлы-таблицы. нам нужна с id="t2", т.е. [2]
  nodes = html_nodes(url, 'table')
  
  # закидываем таблицу в датафрейм
  df = html_table(nodes[2]) %>% as.data.frame()
  
  # почему-то не прочитался стробец Rank (заполнился NA), но это по факту номер строки
  # поэтому его просто убираем
  df = df[][2:11]
  
  # выбираем из датафрейма только те строки, которые нам нужны
  df_new = rbind(df[df$Country == "Norway", ],
                 df[df$Country == "Germany", ],
                 df[df$Country == "United Kingdom", ],
                 df[df$Country == "Greece", ])
  # чтобы просто были пронумерованы строки, а не как в изначальном датафрейме по рангам
  # хотя может это потом и пригодится
  rownames(df_new) <- c(1,2,3,4)
  df_new
}

# получаем данные в разные датфреймы по 4 странам за 2014-2021 года
df_2014 = getCountriesIndexesByYear(2014)
df_2015 = getCountriesIndexesByYear(2015)
df_2016 = getCountriesIndexesByYear(2016)
df_2017 = getCountriesIndexesByYear(2017)
df_2018 = getCountriesIndexesByYear(2018)
df_2019 = getCountriesIndexesByYear(2019)
df_2020 = getCountriesIndexesByYear(2020)
df_2021 = getCountriesIndexesByYear(2021)

