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


###########################################
##### Анализ полученных данных ############
###########################################

######################################################
### соберем данные для их удобной отрисовки на графиках
## качество жизни (1 столбец и основной)
getQuantityLifeByYears <- function(){
  quality_life_by_years = data.frame(row.names = c(1,2,3,4)) 
  quality_life_by_years["Country"] = df_2014[, 'Country']  # Вставили названия стран
  quality_life_by_years["2014"] = df_2014[, 'Quality.of.Life.Index']
  quality_life_by_years["2015"] = df_2015[, 'Quality.of.Life.Index']
  quality_life_by_years["2016"] = df_2016[, 'Quality.of.Life.Index']
  quality_life_by_years["2017"] = df_2017[, 'Quality.of.Life.Index']
  quality_life_by_years["2018"] = df_2018[, 'Quality.of.Life.Index']
  quality_life_by_years["2019"] = df_2019[, 'Quality.of.Life.Index']
  quality_life_by_years["2020"] = df_2020[, 'Quality.of.Life.Index']
  quality_life_by_years["2021"] = df_2021[, 'Quality.of.Life.Index']
  quality_life_by_years
}
q_life_by_y = getQuantityLifeByYears()
q_life_by_y

## которые "чем выше тем лучше"
# покупательская способность
getPurchasingByYears <- function(){
  df = data.frame(row.names = c(1,2,3,4)) 
  df["Country"] = df_2014[, 'Country']  # Вставили названия стран
  df["2014"] = df_2014[, 'Purchasing.Power.Index']
  df["2015"] = df_2015[, 'Purchasing.Power.Index']
  df["2016"] = df_2016[, 'Purchasing.Power.Index']
  df["2017"] = df_2017[, 'Purchasing.Power.Index']
  df["2018"] = df_2018[, 'Purchasing.Power.Index']
  df["2019"] = df_2019[, 'Purchasing.Power.Index']
  df["2020"] = df_2020[, 'Purchasing.Power.Index']
  df["2021"] = df_2021[, 'Purchasing.Power.Index']
  df
}
pur_by_y = getPurchasingByYears()
pur_by_y

# безопасность
getSecurityByYears <- function(){
  df = data.frame(row.names = c(1,2,3,4)) 
  df["Country"] = df_2014[, 'Country']  # Вставили названия стран
  df["2014"] = df_2014[, 'Safety.Index']
  df["2015"] = df_2015[, 'Safety.Index']
  df["2016"] = df_2016[, 'Safety.Index']
  df["2017"] = df_2017[, 'Safety.Index']
  df["2018"] = df_2018[, 'Safety.Index']
  df["2019"] = df_2019[, 'Safety.Index']
  df["2020"] = df_2020[, 'Safety.Index']
  df["2021"] = df_2021[, 'Safety.Index']
  df
}
sec_by_y = getSecurityByYears()
sec_by_y

# медицинское обслуживание
getHealthCareByYears <- function(){
  df = data.frame(row.names = c(1,2,3,4)) 
  df["Country"] = df_2014[, 'Country']  # Вставили названия стран
  df["2014"] = df_2014[, 'Health.Care.Index']
  df["2015"] = df_2015[, 'Health.Care.Index']
  df["2016"] = df_2016[, 'Health.Care.Index']
  df["2017"] = df_2017[, 'Health.Care.Index']
  df["2018"] = df_2018[, 'Health.Care.Index']
  df["2019"] = df_2019[, 'Health.Care.Index']
  df["2020"] = df_2020[, 'Health.Care.Index']
  df["2021"] = df_2021[, 'Health.Care.Index']
  df
}
health_by_y = getHealthCareByYears()
health_by_y

# климат
getClimateByYears <- function(){
  df = data.frame(row.names = c(1,2,3,4)) 
  df["Country"] = df_2014[, 'Country']  # Вставили названия стран
  df["2014"] = df_2014[, 'Climate.Index']
  df["2015"] = df_2015[, 'Climate.Index']
  df["2016"] = df_2016[, 'Climate.Index']
  df["2017"] = df_2017[, 'Climate.Index']
  df["2018"] = df_2018[, 'Climate.Index']
  df["2019"] = df_2019[, 'Climate.Index']
  df["2020"] = df_2020[, 'Climate.Index']
  df["2021"] = df_2021[, 'Climate.Index']
  df
}
climate_by_y = getClimateByYears()
climate_by_y

## которые "чем ниже тем лучше"
# загрязнение
getPollutionByYears <- function(){
  df = data.frame(row.names = c(1,2,3,4)) 
  df["Country"] = df_2014[, 'Country']  # Вставили названия стран
  df["2014"] = df_2014[, 'Pollution.Index']
  df["2015"] = df_2015[, 'Pollution.Index']
  df["2016"] = df_2016[, 'Pollution.Index']
  df["2017"] = df_2017[, 'Pollution.Index']
  df["2018"] = df_2018[, 'Pollution.Index']
  df["2019"] = df_2019[, 'Pollution.Index']
  df["2020"] = df_2020[, 'Pollution.Index']
  df["2021"] = df_2021[, 'Pollution.Index']
  df
}
pollut_by_y = getPollutionByYears()
pollut_by_y

# жилье \ доход
getPropToRatioByYears <- function(){
  df = data.frame(row.names = c(1,2,3,4)) 
  df["Country"] = df_2014[, 'Country']  # Вставили названия стран
  df["2014"] = df_2014[, 'Property.Price.to.Income.Ratio']
  df["2015"] = df_2015[, 'Property.Price.to.Income.Ratio']
  df["2016"] = df_2016[, 'Property.Price.to.Income.Ratio']
  df["2017"] = df_2017[, 'Property.Price.to.Income.Ratio']
  df["2018"] = df_2018[, 'Property.Price.to.Income.Ratio']
  df["2019"] = df_2019[, 'Property.Price.to.Income.Ratio']
  df["2020"] = df_2020[, 'Property.Price.to.Income.Ratio']
  df["2021"] = df_2021[, 'Property.Price.to.Income.Ratio']
  df
}
prop_to_rat_by_y = getPropToRatioByYears()
prop_to_rat_by_y

# прожиточный минимум (индекс стоимости жизни)
getCostLivingByYears <- function(){
  df = data.frame(row.names = c(1,2,3,4)) 
  df["Country"] = df_2014[, 'Country']  # Вставили названия стран
  df["2014"] = df_2014[, 'Cost.of.Living.Index']
  df["2015"] = df_2015[, 'Cost.of.Living.Index']
  df["2016"] = df_2016[, 'Cost.of.Living.Index']
  df["2017"] = df_2017[, 'Cost.of.Living.Index']
  df["2018"] = df_2018[, 'Cost.of.Living.Index']
  df["2019"] = df_2019[, 'Cost.of.Living.Index']
  df["2020"] = df_2020[, 'Cost.of.Living.Index']
  df["2021"] = df_2021[, 'Cost.of.Living.Index']
  df
}
cost_liv_by_y = getCostLivingByYears()
cost_liv_by_y

# движения на дороге
getTrafficByYears <- function(){
  df = data.frame(row.names = c(1,2,3,4)) 
  df["Country"] = df_2014[, 'Country']  # Вставили названия стран
  df["2014"] = df_2014[, 'Traffic.Commute.Time.Index']
  df["2015"] = df_2015[, 'Traffic.Commute.Time.Index']
  df["2016"] = df_2016[, 'Traffic.Commute.Time.Index']
  df["2017"] = df_2017[, 'Traffic.Commute.Time.Index']
  df["2018"] = df_2018[, 'Traffic.Commute.Time.Index']
  df["2019"] = df_2019[, 'Traffic.Commute.Time.Index']
  df["2020"] = df_2020[, 'Traffic.Commute.Time.Index']
  df["2021"] = df_2021[, 'Traffic.Commute.Time.Index']
  df
}
traffic_by_y = getTrafficByYears()
traffic_by_y
######################################################


### сперва отобразим графики показателей, которые "чем выше тем лучше"
attach(mtcars)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2)) # будет четыре графика

### Отрисовываем графики ###
par(mar = c(5, 5, 4, 2))
years = colnames(pur_by_y)[2:9]; years

plot(years, 
     pur_by_y[1, 2:9], 
     type='l', 
     col='gold', 
     main='Изменения покупательской способности
     по странам',
     cex.main = 1,   # Title size
     xlab='Года',
     ylab='Значение индекса',
     ylim=c(min(pur_by_y[4, 2:9]),max(pur_by_y[2, 2:9])+1))
lines(years, 
      pur_by_y[2, 2:9], 
      type='l', 
      col='gray')
lines(years, 
      pur_by_y[3, 2:9], 
      type='l', 
      col='blue')
lines(years, 
      pur_by_y[4, 2:9], 
      type='l', 
      col='red')

par(opar)
detach(mtcars)
