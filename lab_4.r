# импортируем данные по всем годам олимпад страны Австралия
history.winter <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/history winter.csv", header = TRUE, sep = ",", encoding = "UTF-8");history.winter
history.summer <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/history summer.csv", header = TRUE, sep = ",", encoding = "UTF-8");history.summer

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
olympic.swimming <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/swimming.csv", header = TRUE, sep = ",", encoding = "UTF-8");olympic.swimming

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
male.winter <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/male winter.csv", header = TRUE, sep = ",", encoding = "UTF-8");male.winter
male.summer <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/male summer.csv", header = TRUE, sep = ",", encoding = "UTF-8");male.summer
female.winter <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/female winter.csv", header = TRUE, sep = ",", encoding = "UTF-8");female.winter
female.summer <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/female summer.csv", header = TRUE, sep = ",", encoding = "UTF-8");female.summer


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
