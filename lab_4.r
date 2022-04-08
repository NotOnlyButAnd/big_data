# импортируем данные по всем годам олимпад страны Австралия
olympic.history <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР4/history.csv", header = TRUE, sep = ",", encoding = "UTF-8");olympic.history

# строим график, отображающий историю изменения олимпийских достижений во времени
plot(olympic.history$Год, 
     olympic.history$Золото, 
     type='l', 
     col='gold', 
     main='Изменения олимпийских достижений (Австралия)',
     xlab='Года',
     ylab='Кол-во медалей')
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
       col=c("gold", "gray", "orange")) 
