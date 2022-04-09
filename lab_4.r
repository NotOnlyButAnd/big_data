# импортируем данные по всем годам олимпад страны Австралия
olympic.history <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/history.csv", header = TRUE, sep = ",", encoding = "UTF-8");olympic.history

# строим график, отображающий историю изменения олимпийских достижений во времени
par(mar = c(5, 5, 4, 2))
plot(olympic.history$Год, 
     olympic.history$Золото, 
     type='l', 
     col='gold', 
     main='Изменения олимпийских достижений (Австралия)',
     xlab='Года',
     ylab='Кол-во медалей',
     ylim=c(0,max(olympic.history[,3:5]))+1, # пределы по оси y посчитали
     xaxt="n")  # не отображаем стандартные отметки иксов
at = olympic.history$Год;at
mtext(side = 1, text = at, at = at, 
      col = "grey20", line = 1, cex = 0.9, las=2)
# серебряные
lines(olympic.history$Год, 
      olympic.history$Серебро, 
      type='l', 
      col='gray')
# бронзовые
lines(olympic.history$Год, 
      olympic.history$Бронза, 
      type='l', 
      col='orange')

# добавляем легенду
legend("topleft", inset=.01, 
       title="Графики медалей", c("Золото","Серебро", "Бронза"),
       lty=c(1, 1, 1), 
       col=c("gold", "gray", "orange"),
       bty = "n")        # Box type (bty = "n" removes the box)) 

#####################
# импортируем данные по плаванию Австралии
olympic.swimming <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/swimming.csv", header = TRUE, sep = ",", encoding = "UTF-8");olympic.swimming

# преобразуем данные - суммируем кол-во мест 1-8
olympic.swimming.vars <- apply(olympic.swimming[3:10],1,sum);olympic.swimming.vars

# рисуем столбчатую диаграмму 
barplot(olympic.swimming.vars,
        xlab='Года',
        ylab='Кол-во медалей',
        names.arg=olympic.swimming[,1],
        las=2,
        col=rainbow(26))

#####################
# круговая диаграмма по первым местам за все олимпиады
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
# считываем данные
male <-read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/male.csv", header = TRUE, sep = ",", encoding = "UTF-8");male
female <-read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/female.csv", header = TRUE, sep = ",", encoding = "UTF-8");female

#считаем кол-во медалей всего
female.sum <- apply(female[,3:5],1,sum);female.sum
male.sum <- apply(male[,3:5],1,sum);male.sum

# сперва женщин отобразим
plot(female$Год, 
     female.sum, 
     type='l', 
     col='red', 
     main='Тенденция изменения кол-ва призовых мест (м/ж)',
     xlab='Года',
     ylab='Кол-во медалей',
     ylim=c(0,max(male.sum)+1), # пределы по оси y посчитали
     xaxt="n")  # не отображаем стандартные отметки иксов

mtext(side = 1, 
      text = female$Год, 
      at = female$Год, 
      col = "grey20", line = 1, cex = 0.9, las=2)

# теперь мужики
lines(male$Год, 
      male.sum, 
      type='l', 
      col='blue')

# добавляем легенду
legend("topright", inset=.01, 
       c("Женщины","Мужчины"),
       lty=c(1, 1), 
       col=c("red", "blue"),
       bty = "n")        # Box type (bty = "n" removes the box)) 
