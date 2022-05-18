install.packages("rvest")
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

url = read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2014')


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
drawHigherGraphics <- function(){
  attach(mtcars)
  opar <- par(no.readonly=TRUE)
  # будет четыре графика
  par(oma = c(1,1,1,1), mfrow = c(2, 2), mar = c(1, 1, 1, 1))
  
  ### Отрисовываем графики ###
  par(mar = c(5, 5, 4, 2))
  years = colnames(pur_by_y)[2:9]; years
  
  # покупательская способоность
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
        col='green')
  lines(years, 
        pur_by_y[3, 2:9], 
        type='l', 
        col='blue')
  lines(years, 
        pur_by_y[4, 2:9], 
        type='l', 
        col='red')
  
  # безопасность
  plot(years, 
       sec_by_y[1, 2:9], 
       type='l', 
       col='gold', 
       main='Изменения индекса безопасности
     по странам',
       cex.main = 1,   # Title size
       xlab='Года',
       ylab='Значение индекса',
       ylim=c(min(sec_by_y[1, 2:9]),max(sec_by_y[2, 2:9])+1))
  lines(years, 
        sec_by_y[2, 2:9], 
        type='l', 
        col='green')
  lines(years, 
        sec_by_y[3, 2:9], 
        type='l', 
        col='blue')
  lines(years, 
        sec_by_y[4, 2:9], 
        type='l', 
        col='red')
  
  # медицина
  plot(years, 
       health_by_y[1, 2:9], 
       type='l', 
       col='gold', 
       main='Изменения индекса мед обслуживания
     по странам',
       cex.main = 1,   # Title size
       xlab='Года',
       ylab='Значение индекса',
       ylim=c(min(health_by_y[4, 2:9]),max(health_by_y[2, 2:9])+1))
  lines(years, 
        health_by_y[2, 2:9], 
        type='l', 
        col='green')
  lines(years, 
        health_by_y[3, 2:9], 
        type='l', 
        col='blue')
  lines(years, 
        health_by_y[4, 2:9], 
        type='l', 
        col='red')
  
  # климат
  plot(years[3:8], 
       climate_by_y[1, 4:9], 
       type='l', 
       col='gold', 
       main='Изменения климатического индекса
     по странам',
       cex.main = 1,   # Title size
       xlab='Года',
       ylab='Значение индекса',
       ylim=c(min(climate_by_y[1, 4:9]),max(climate_by_y[4, 4:9])+1))
  lines(years[3:8], 
        climate_by_y[2, 4:9], 
        type='l', 
        col='green')
  lines(years[3:8], 
        climate_by_y[3, 4:9], 
        type='l', 
        col='blue')
  lines(years[3:8], 
        climate_by_y[4, 4:9], 
        type='l', 
        col='red')
  
  par(opar)
  detach(mtcars)
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend('bottom',
         legend = c("Norway","Germany", "United Kingdom", "Greece"),
         col = c("gold", "green", "blue", "red"),
         lwd = 5,
         xpd = TRUE,
         horiz = TRUE,
         cex = 0.7,
         seg.len=1,
         bty = 'n')
}
drawHigherGraphics()


### затем отобразим графики показателей, которые "чем НИЖЕ тем лучше"
drawLowerGraphics <- function(){
  attach(mtcars)
  opar <- par(no.readonly=TRUE)
  # будет четыре графика
  par(oma = c(1,1,1,1), mfrow = c(2, 2), mar = c(1, 1, 1, 1))
  
  ### Отрисовываем графики ###
  par(mar = c(5, 5, 4, 2))
  years = colnames(pur_by_y)[2:9]; years
  
  # загрязнение
  plot(years, 
       pollut_by_y[1, 2:9], 
       type='l', 
       col='gold', 
       main='Изменения индекса загрязнения
     по странам',
       cex.main = 1,   # Title size
       xlab='Года',
       ylab='Значение индекса',
       ylim=c(min(pollut_by_y[1, 2:9]),max(pollut_by_y[4, 2:9])+1))
  lines(years, 
        pollut_by_y[2, 2:9], 
        type='l', 
        col='green')
  lines(years, 
        pollut_by_y[3, 2:9], 
        type='l', 
        col='blue')
  lines(years, 
        pollut_by_y[4, 2:9], 
        type='l', 
        col='red')
  
  # жилье \ доход
  plot(years, 
       prop_to_rat_by_y[1, 2:9], 
       type='l', 
       col='gold', 
       main='Изменения отношения цены жилья 
       к доходам по странам',
       cex.main = 1,   # Title size
       xlab='Года',
       ylab='Значение индекса',
       ylim=c(min(prop_to_rat_by_y[2, 2:9] - 1),max(prop_to_rat_by_y[4, 2:9])+1))
  lines(years, 
        prop_to_rat_by_y[2, 2:9], 
        type='l', 
        col='green')
  lines(years, 
        prop_to_rat_by_y[3, 2:9], 
        type='l', 
        col='blue')
  lines(years, 
        prop_to_rat_by_y[4, 2:9], 
        type='l', 
        col='red')
  
  # прожиточный минимум (стоимость жизни)
  plot(years, 
       cost_liv_by_y[1, 2:9], 
       type='l', 
       col='gold', 
       main='Изменения индекса прожиточного
     минимума по странам',
       cex.main = 1,   # Title size
       xlab='Года',
       ylab='Значение индекса',
       ylim=c(min(cost_liv_by_y[4, 2:9])-1,max(cost_liv_by_y[1, 2:9])+1))
  lines(years, 
        cost_liv_by_y[2, 2:9], 
        type='l', 
        col='green')
  lines(years, 
        cost_liv_by_y[3, 2:9], 
        type='l', 
        col='blue')
  lines(years, 
        cost_liv_by_y[4, 2:9], 
        type='l', 
        col='red')
  
  # дороги
  plot(years, 
       traffic_by_y[1, 2:9], 
       type='l', 
       col='gold', 
       main='Изменения индекса дорожного
     траффика по странам',
       cex.main = 1,   # Title size
       xlab='Года',
       ylab='Значение индекса',
       ylim=c(min(traffic_by_y[1, 2:9])-1,max(traffic_by_y[3, 2:9])+1))
  lines(years, 
        traffic_by_y[2, 2:9], 
        type='l', 
        col='green')
  lines(years, 
        traffic_by_y[3, 2:9], 
        type='l', 
        col='blue')
  lines(years, 
        traffic_by_y[4, 2:9], 
        type='l', 
        col='red')
  
  par(opar)
  detach(mtcars)
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend('bottom',
         legend = c("Norway","Germany", "United Kingdom", "Greece"),
         col = c("gold", "green", "blue", "red"),
         lwd = 5,
         xpd = TRUE,
         horiz = TRUE,
         cex = 0.7,
         seg.len=1,
         bty = 'n')
}
drawLowerGraphics()

### затем отобразим изменение рейтинга уровня жизни по всем 4 странам
drawRateGraphic <- function(){
  par(mar = c(5, 5, 4, 2))
  # рейтинг
  plot(years, 
       q_life_by_y[1, 2:9], 
       type='l', 
       col='gold', 
       main='Изменения индекса качества
     жизни по странам',
       cex.main = 1,   # Title size
       xlab='Года',
       ylab='Значение индекса',
       ylim=c(min(q_life_by_y[4, 2:9])-1,max(q_life_by_y[2, 2:9])+1))
  lines(years, 
        q_life_by_y[2, 2:9], 
        type='l', 
        col='green')
  lines(years, 
        q_life_by_y[3, 2:9], 
        type='l', 
        col='blue')
  lines(years, 
        q_life_by_y[4, 2:9], 
        type='l', 
        col='red')
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend('bottom',
         legend = c("Norway","Germany", "United Kingdom", "Greece"),
         col = c("gold", "green", "blue", "red"),
         lwd = 5,
         xpd = TRUE,
         horiz = TRUE,
         cex = 0.7,
         seg.len=1,
         bty = 'n')
}
drawRateGraphic()

##################################
######## Scrapping музеев ########
##################################
# читаем html музейский, текущую страницу
getArticlesByUrl <- function(my_url){
  url = read_html(my_url)
  #selector_name<-"ul.places-list"
  selector_name<-"h3.places-list__item-header"
  
  # достаем узлы
  result <- html_nodes(url, selector_name) %>% html_text() %>% as.array()
  
  result
}

# читаем все страницы и добавляем заголовки с них
all_articles <- getArticlesByUrl('https://tonkosti.ru/%D0%9C%D1%83%D0%B7%D0%B5%D0%B8_%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3%D0%B0?page=2#ttl')
all_articles <- append(all_articles, getArticlesByUrl('https://tonkosti.ru/%D0%9C%D1%83%D0%B7%D0%B5%D0%B8_%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3%D0%B0#ttl'))
all_articles <- append(all_articles, getArticlesByUrl('https://tonkosti.ru/%D0%9C%D1%83%D0%B7%D0%B5%D0%B8_%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3%D0%B0?page=3#ttl'))
all_articles <- append(all_articles, getArticlesByUrl('https://tonkosti.ru/%D0%9C%D1%83%D0%B7%D0%B5%D0%B8_%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3%D0%B0?page=4#ttl'))
all_articles <- append(all_articles, getArticlesByUrl('https://tonkosti.ru/%D0%9C%D1%83%D0%B7%D0%B5%D0%B8_%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3%D0%B0?page=5#ttl'))
all_articles

# получение ссылок с картинок
getUrlPicByUrl <- function(my_url){
  url = read_html(my_url)
  
  selector_name<-".places-list__item-img.places-list__item-img--rc"
  selector_name
  result <- html_nodes(url, selector_name) %>% html_attr("href")
  
  result
}
all_web_p <- getUrlPicByUrl('https://tonkosti.ru/%D0%9C%D1%83%D0%B7%D0%B5%D0%B8_%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3%D0%B0?page=2#ttl')
all_web_p <- append(all_web_p, getUrlPicByUrl('https://tonkosti.ru/%D0%9C%D1%83%D0%B7%D0%B5%D0%B8_%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3%D0%B0#ttl'))
all_web_p <- append(all_web_p, getUrlPicByUrl('https://tonkosti.ru/%D0%9C%D1%83%D0%B7%D0%B5%D0%B8_%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3%D0%B0?page=3#ttl'))
all_web_p <- append(all_web_p, getUrlPicByUrl('https://tonkosti.ru/%D0%9C%D1%83%D0%B7%D0%B5%D0%B8_%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3%D0%B0?page=4#ttl'))
all_web_p <- append(all_web_p, getUrlPicByUrl('https://tonkosti.ru/%D0%9C%D1%83%D0%B7%D0%B5%D0%B8_%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3%D0%B0?page=5#ttl'))
all_web_p
all_web_p <- paste0('https://tonkosti.ru/', all_web_p)
all_web_p

# получение адресов
getAddrByUrl <- function(my_url){
  url = read_html(my_url)
  
  selector_name<-".places-list__address.places-list__address--rc"
  selector_name
  result <- html_nodes(url, selector_name) %>% html_text() %>% as.array()
  
  result
}

all_addr <- getUrlPicByUrl('https://tonkosti.ru/%D0%9C%D1%83%D0%B7%D0%B5%D0%B8_%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3%D0%B0?page=2#ttl')
all_addr <- append(all_addr, getUrlPicByUrl('https://tonkosti.ru/%D0%9C%D1%83%D0%B7%D0%B5%D0%B8_%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3%D0%B0#ttl'))
all_addr <- append(all_addr, getUrlPicByUrl('https://tonkosti.ru/%D0%9C%D1%83%D0%B7%D0%B5%D0%B8_%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3%D0%B0?page=3#ttl'))
all_addr <- append(all_addr, getUrlPicByUrl('https://tonkosti.ru/%D0%9C%D1%83%D0%B7%D0%B5%D0%B8_%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3%D0%B0?page=4#ttl'))
all_addr <- append(all_addr, getUrlPicByUrl('https://tonkosti.ru/%D0%9C%D1%83%D0%B7%D0%B5%D0%B8_%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3%D0%B0?page=5#ttl'))
all_addr

all_museum_data <- data.frame(all_articles, all_addr, all_web_p)
