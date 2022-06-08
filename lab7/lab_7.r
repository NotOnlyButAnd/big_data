# считываем данные из csv
olympics <- read.csv(file = "F:/Кирилл/003 УНИВЕР/3 курс/004 2 семестр/big data/ЛР7/athlete_events.csv", 
                     sep = ",", header = TRUE, dec = ',')

# выбираем плавание (мужчины)
swimming_man <- subset(olympics, Sport == 'Swimming')
swimming_man

# Удалим строки, где есть пустые значения в весе
swimming_man[swimming_man == ""] <- NA
swimming_man <- swimming_man[rowSums(is.na(swimming_man[,1:6])) == 0,]
swimming_man

library(dplyr)
# Удалим повторяющиеся строки спортсменов
swimming_man <- swimming_man %>% group_by(ID) %>% filter (! duplicated(ID))

# выбираем мужчин (вес мужчин и женщин, очевидно, отличается)
swimming_man <- swimming_man[swimming_man$Sex == 'M',]

# Вес выбираем
weight_swimming_man <- as.numeric(swimming_man$Weight)
weight_swimming_man

# Гистограмма веса пловцов
hist(weight_swimming_man, main='Вес пловцов',
     xlab='Вес', ylab='Частота',xlim = c(40,120))

# Проверка выборки на нормальность распределения с помощью Квантильно-квантильного графика (график QQ)
# (показывает распределение данных относительно ожидаемого нормального распределения)
qqnorm(weight_swimming_man)
qqline(weight_swimming_man, col='green', lwd = 5)

# Доверительные интервалы
install.packages('car')
library(car)
qqPlot(weight_swimming_man)

# Тест Стьюдента, т.к. распределение нормальное
t.test(weight_swimming_man, mu=78)

# Тест Уилкоксона
wilcox.test(weight_swimming_man, mu=78, conf.int = TRUE)

# Тест Шапиро-Уилкса для проверки на нормальность (не больше 5000 данных)
shapiro.test(weight_swimming_man[1:4999])

################################################################################

# Выборка  спортсменов водного поло (мужчин)
water_polo_man <- subset(olympics, Sex == 'M' & Sport == 'Water Polo')


# Удалим строки, где есть пустые значения в весе
water_polo_man[water_polo_man == ""] <- NA 
water_polo_man <- water_polo_man[rowSums(is.na(water_polo_man[,1:6])) == 0,]

# Удалим повторяющиеся строки спортсменов
water_polo_man <- water_polo_man %>% group_by(ID) %>% filter (! duplicated(ID))

# выберем их вес
weight_water_polo_man <- as.numeric(water_polo_man$Weight)
weight_water_polo_man

# Гистограммы веса
par(mfrow = c(2, 1))
hist(weight_swimming_man, main='Вес пловцов', xlab='Вес', ylab='Частота', xlim = c(40,120), col = "lightblue")
hist(weight_water_polo_man, main='Вес водных полоистов', xlab='Вес', ylab='Частота', xlim = c(50,140), col = "lightgreen")

# Проверка на нормальность
dev.off()
qqPlot(weight_swimming_man)
qqPlot(weight_water_polo_man)

# Средний вес у тех и других
mean(weight_swimming_man, na.rm=TRUE)
mean(weight_water_polo_man, na.rm = TRUE)

# Объединяем
swimming_polo_man <- rbind(swimming_man, water_polo_man)

# Тест на равенство дисперсий
bartlett.test(as.numeric(swimming_polo_man$Weight) ~ swimming_polo_man$Sport, data=swimming_polo_man)

# Проверка, различаются ли выбранные средние значения с помощью теста Уэлча
t.test(as.numeric(swimming_polo_man$Weight) ~ swimming_polo_man$Sport)

# Проверка, при условии, что дисперсии равны
t.test(as.numeric(swimming_polo_man$Weight) ~ swimming_polo_man$Sport, paired = FALSE, var.equal = TRUE)
