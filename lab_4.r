# импортируем данные по всем годам олимпад страны Австралия
#history.winter <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/history winter.csv", header = TRUE, sep = ",", encoding = "UTF-8");history.winter
#history.summer <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/history summer.csv", header = TRUE, sep = ",", encoding = "UTF-8");history.summer

history.winter <- read.csv(file = "C:/Users/s0153478/Downloads/lab_4_CSV's/history winter.csv", header = TRUE, sep = ",", encoding = "UTF-8");history.winter
history.summer <- read.csv(file = "C:/Users/s0153478/Downloads/lab_4_CSV's/history summer.csv", header = TRUE, sep = ",", encoding = "UTF-8");history.summer



# строим график, отображающий историю изменения олимпийских достижений во времени
par(mar = c(5, 5, 4, 2))
plot(history.summer$Год, 
     history.summer$Золото, 
     type='l', 
     col='gold', 
     main='Изменения олимпийских достижений (Австралия)',
     xlab='Года',
     ylab='Кол-во медалей',
     ylim=c(0,max(history.summer[,3:5]))+1, # пределы по оси y посчитали
     xaxt="n")  # не отображаем стандартные отметки иксов
# серебряные (лето)
lines(history.summer$Год, 
      history.summer$Серебро, 
      type='l', 
      col='gray')
# бронзовые (лето)
lines(history.summer$Год, 
      history.summer$Бронза, 
      type='l', 
      col='orange')
# золотые (зима)
lines(history.winter$Год, 
      history.winter$Золото, 
      type='l',
      lty=2,
      col='gold')
# серебряные (зима)
lines(history.winter$Год, 
      history.winter$Серебро, 
      type='l',
      lty=2,
      col='gray')
# бронзовые (зима)
lines(history.winter$Год, 
      history.winter$Бронза, 
      type='l',
      lty=2,
      col='orange')

# добавляем легенду
legend("topleft", inset=.01, 
       c("Золото (лето)","Серебро (лето)", "Бронза(лето)", "Золото (зима)","Серебро (зима)", "Бронза(зима)"),
       lty=c(1, 1, 1, 2, 2, 2), 
       col=c("gold", "gray", "orange", "gold", "gray", "orange"),
       cex=0.75,
       bty = "n")        # Box type (bty = "n" removes the box)) 

# наносим отметки годов на ось OX
at <- union(history.winter$Год, history.summer$Год);at
mtext(side = 1, text = at, at = at, 
      col = "grey20", line = 1, cex = 0.7, las=2)

#####################
# импортируем данные по плаванию Австралии
#olympic.swimming <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/swimming.csv", header = TRUE, sep = ",", encoding = "UTF-8");olympic.swimming

olympic.swimming <- read.csv(file = "C:/Users/s0153478/Downloads/lab_4_CSV's/swimming.csv", header = TRUE, sep = ",", encoding = "UTF-8");olympic.swimming


# преобразуем данные - суммируем кол-во мест 1-8
olympic.swimming.vars <- apply(olympic.swimming[3:10],1,sum);olympic.swimming.vars

# рисуем столбчатую диаграмму 
barplot(olympic.swimming.vars,
        xlab='Года',
        ylab='Кол-во мест',
        main="Кол-во мест 1-8. Австралия. Плавание",
        names.arg=olympic.swimming[,1],
        las=2,
        col=rainbow(26))

#####################
# круговая диаграмма по первым местам за все олимпиады ПЛАВАНИЕ
# выбираем ненулевые года
par(mar = c(5, 3, 4, 2))
olympic.swimming.gold0 <- olympic.swimming[olympic.swimming$Золото>0,]$Золото;olympic.swimming.gold0
olympic.swimming.gold0.years <- olympic.swimming[olympic.swimming$Золото>0,]$Год;olympic.swimming.gold0.years


pie(olympic.swimming.gold0, 
    olympic.swimming.gold0.years,
    clockwise = TRUE, # откладывать сектора по часовой стрелке
    main="Золотые медали по годам. Австралия, плавание\n(без учета нулевых годов)",
    cex=0.8, # размер подписей
    radius=1,
    col=rainbow(length(olympic.swimming.gold0)),
    init.angle=-15)
legend("topleft", 
       olympic.swimming[olympic.swimming$Золото>0,]$Место,
       cex = 0.8,
       fill = rainbow(length(olympic.swimming.gold0)),
       bty = "n")

#####################
# по женщинам и мужчинам тенденции
par(mar = c(5, 5, 4, 2))
# считываем данные
#male.winter <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/male winter.csv", header = TRUE, sep = ",", encoding = "UTF-8");male.winter
#male.summer <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/male summer.csv", header = TRUE, sep = ",", encoding = "UTF-8");male.summer
#female.winter <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/female winter.csv", header = TRUE, sep = ",", encoding = "UTF-8");female.winter
#female.summer <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/female summer.csv", header = TRUE, sep = ",", encoding = "UTF-8");female.summer

male.winter <- read.csv(file = "C:/Users/s0153478/Downloads/lab_4_CSV's/male winter.csv", header = TRUE, sep = ",", encoding = "UTF-8");male.winter
male.summer <- read.csv(file = "C:/Users/s0153478/Downloads/lab_4_CSV's/male summer.csv", header = TRUE, sep = ",", encoding = "UTF-8");male.summer
female.winter <- read.csv(file = "C:/Users/s0153478/Downloads/lab_4_CSV's/female winter.csv", header = TRUE, sep = ",", encoding = "UTF-8");female.winter
female.summer <- read.csv(file = "C:/Users/s0153478/Downloads/lab_4_CSV's/female summer.csv", header = TRUE, sep = ",", encoding = "UTF-8");female.summer



#считаем кол-во медалей всего
female.winter.sum <- apply(female.winter[,3:5],1,sum);female.winter.sum
female.summer.sum <- apply(female.summer[,3:5],1,sum);female.summer.sum
male.winter.sum <- apply(male.winter[,3:5],1,sum);male.winter.sum
male.summer.sum <- apply(male.summer[,3:5],1,sum);male.summer.sum

# сперва женщин отобразим
plot(female.summer$Год, 
     female.summer.sum, 
     type='l', 
     col='red', 
     main='Тенденция изменения кол-ва призовых мест (м/ж)',
     xlab='Года',
     ylab='Кол-во медалей',
     ylim=c(0,max(male.summer.sum)+1), # пределы по оси y посчитали
     xaxt="n")  # не отображаем стандартные отметки иксов
# зима
lines(female.winter$Год, 
      female.summer.sum, 
      type='l',
      lty=2,
      col='red')

# теперь мужики
lines(male.summer$Год, 
      male.summer.sum, 
      type='l', 
      col='blue')
# зима
lines(male.winter$Год, 
      male.summer.sum, 
      type='l',
      lty=2,
      col='blue')

# добавляем легенду
legend("topright", inset=.01, 
       c("Женщины (лето) ","Мужчины (лето)", "Женщины (зима) ","Мужчины (зима)"),
       lty=c(1, 1, 2, 2), 
       col=c("red", "blue", "red", "blue"),
       cex=0.7,
       bty = "n")        # Box type (bty = "n" removes the box)) 

at <- union(female.summer$Год, female.winter$Год);at
mtext(side = 1, 
      text = at, 
      at = at, 
      col = "grey20", line = 1, cex = 0.9, las=2)

####################
# ЗАДАНИЕ 3
# собираем инфу по 7 странам за почти последние 4 олимпуды:
years = c('2006', '2010', '2014', '2018')

#первые места
Germany_1<-c(11, 10, 8, 14)
USA_1<-c(9, 9, 9, 9)
Austria_1<-c(9, 4, 4, 5)
Russia_1<-c(8, 3, 13, 2)
Canada_1<-c(7, 14, 10, 11)
Sweden_1<-c(7, 5, 7, 7)
SouthKor_1<-c(6, 6, 3, 5)

#вторые места
Germany_2<-c(12, 13, 6, 10)
USA_2<-c(9, 15, 7, 8)
Austria_2<-c(7, 6, 8, 3)
Russia_2<-c(6, 5, 11, 6)
Canada_2<-c(10, 7, 10, 8)
Sweden_2<-c(2, 2, 7, 6)
SouthKor_2<-c(3, 6, 3, 8)

#третьи места
Germany_3<-c(6, 7, 5, 7)
USA_3<-c(7, 13, 12, 6)
Austria_3<-c(7, 6, 5, 6)
Russia_3<-c(8, 7, 9, 9)
Canada_3<-c(7, 5, 5, 10)
Sweden_3<-c(5, 4, 6, 1)
SouthKor_3<-c(2, 2, 2, 4)

# золотые медали
plot(years, Germany_1, type='b', 
     ylim = c(0, 30),
     ylab = 'Кол-во',
     xlab = 'Года',
     main = 'Кол-во золотых меделай по странам',
     col.main = 'black',
     col.lab = 'black')

lines(years, USA_1, type='b', col = 'purple')
lines(years,Austria_1, type='b', col = 'orange')
lines(years,Russia_1, type='b', col = 'red')
lines(years,Canada_1, type='b', col = 'blue')
lines(years,Sweden_1, type='b', col = 'yellow')
lines(years,SouthKor_1, type='b', col = 'green')

lbl = c('Германия','США', 'Австралия', 'Россия', 'Канада',  'Швеция', 'Южная Корея')
color = c("black","purple", 'orange', 'red', 'blue', 'yellow', 'green')

legend("topleft",
       title = 'Страны',
       lbl,lty=c(1,1,1,1,1,1,1),
       fill = color,
       bg ="white",
       bty='n')

# по призовым 3 местам
Germany_all<-Germany_1+Germany_2+Germany_3;Germany_all
USA_all<-USA_1+USA_2+USA_3;USA_all
Austria_all<-Austria_1+Austria_2+Austria_3
Russia_all<-Russia_1+Russia_2+Russia_3
Canada_all<-Canada_1+Canada_2+Canada_3
Sweden_all<-Sweden_1+Sweden_2+Sweden_3
SouthKor_all<-SouthKor_1+SouthKor_2+SouthKor_3

plot(years, Germany_all, type='b', 
     ylim = c(0, 50),
     ylab = 'Кол-во',
     xlab = 'Года',
     main = 'Кол-во призовых мест по странам',
     col.main = 'black',
     col.lab = 'black')

lines(years, USA_all, type='b', col = 'purple')
lines(years,Austria_all, type='b', col = 'orange')
lines(years,Russia_all, type='b', col = 'red')
lines(years,Canada_all, type='b', col = 'blue')
lines(years,Sweden_all, type='b', col = 'yellow')
lines(years,SouthKor_all, type='b', col = 'green')

legend("topleft",
       title = 'Страны',
       lbl,lty=c(1,1,1,1,1,1,1),
       fill = color,
       bg ="white",
       bty='n',
       cex=0.65)

####################
# ЗАДАНИЕ 4
# смотрим плавание по 5 олимудам
# 2020, 2016, 2012, 2008, 2004
years_fe_m <- c(2020, 2016, 2012, 2008, 2004)
swim_male_1 <- c(14, 17, 17, 15, 16)
swim_male_2 <- c(18, 19, 19, 17, 16)
swim_male_3 <- c(18, 16, 15, 19, 16)
swim_female_1 <- c(23, 18, 17, 19, 16)
swim_female_2 <- c(18, 16, 17, 17, 16)
swim_female_3 <- c(18, 18, 17, 17, 16)

# строим графики по достижениям в плавании М\Ж
# сперва мужчин отобразим
plot(years_fe_m, 
     swim_male_1, 
     type='l', 
     col='gold', 
     main='Кол-во медалей за последние\n5 олимпиад, плавание (м/ж)',
     xlab='Года',
     ylab='Кол-во медалей',
     ylim=c(12,24), # пределы по оси y посчитали
     xaxt="n")  # не отображаем стандартные отметки иксов
# серебро
lines(years_fe_m, 
      swim_male_2, 
      type='l',
      lty=1,
      col='gray')
# бронза
lines(years_fe_m, 
      swim_male_3, 
      type='l',
      lty=1,
      col='orange')

# теперь женщины
lines(years_fe_m, 
      swim_female_1, 
      type='l',
      lty=2,
      col='gold')
# серебро
lines(years_fe_m, 
      swim_female_2, 
      type='l',
      lty=2,
      col='gray')
# бронза
lines(years_fe_m, 
      swim_female_3, 
      type='l',
      lty=2,
      col='orange')

# добавляем легенду
legend("topleft", inset=.01, 
       c("Мужчины (1) ","Женщины (1)", "Мужчины (2) ","Женщины (2)",  "Мужчины (3) ","Женщины (3)"),
       lty=c(1, 2, 1, 2, 1, 2), 
       col=c("gold", "gold", "gray", "gray", "orange", "orange"),
       cex=0.7,
       bty = "n")        # Box type (bty = "n" removes the box)) 

at <- years_fe_m;at
mtext(side = 1, 
      text = at, 
      at = at, 
      col = "grey20", line = 1, cex = 0.9, las=2)

# рисуем столбчатую диаграмму 
fe_m_all <- data.matrix(data.frame(swim_male_1, swim_female_1, swim_male_2, swim_female_2, swim_male_3, swim_female_3))

barplot(fe_m_all,
        ylab='Кол-во медалей',
        main="Кол-во мест 1-3. М/Ж. Плавание",
        names.arg=c("Мужчины (1) ","Женщины (1)", "Мужчины (2) ","Женщины (2)",  "Мужчины (3) ","Женщины (3)"),
        las=1,
        col=rainbow(5),
        beside = TRUE,
        ylim=c(0,25))
legend("topleft", 
       c("2020","2016","2012","2008","2004"),
       cex = 0.8,
       fill = rainbow(5),
       bty = "n") 

# рисуем круговую диаграмму (по годам сделаем короче)
fe_m_all <- data.matrix(data.frame(swim_male_1 + swim_male_2 + swim_male_3, swim_female_1 + swim_female_2 + swim_female_3))
pie(fe_m_all, 
    c("М", "М", "М", "М", "М", "Ж", "Ж", "Ж", "Ж", "Ж"),
    clockwise = TRUE, # откладывать сектора по часовой стрелке
    main="Медали по годам. М/Ж, плавание",
    cex=0.8, # размер подписей
    radius=1,
    col=rainbow(5))
legend("topleft", 
       c("2020","2016","2012","2008","2004"),
       cex = 0.8,
       fill = rainbow(5),
       bty = "n")
