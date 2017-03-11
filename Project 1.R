setwd("C:/DATA/coursera/5-6.Reproducible Research/week 2")
library("knitr", lib.loc="C:/Program Files/R/R-3.3.1/library")
library("ggplot2", lib.loc="C:/Program Files/R/R-3.3.1/library")
library(plyr)
base <- read.table(file="activity.csv", sep=",", dec=",", header=T, stringsAsFactors=F)
head(base)
dim(base)  
summary(base)

#преобразуем в столбик дни название дней недели

base$day <- weekdays(as.Date(base$date))
base$DateTime<- as.POSIXct(base$date, format="%Y-%m-%d")


##выдергивать данные без NA
clean <- base[!is.na(base$steps),]
 
summ <- aggregate(base$steps ~ base$date, FUN=sum, )
colnames(summ)<- c("Date", "Steps")
hist(summ$Steps, breaks=5, xlab="Шаги, шт.",ylab="Количество повторов, шт.", main = "Количество шагов в день")

as.integer(mean(summ$Steps))
as.integer(median(summ$Steps))

interval <- base %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))
 

#какая средняя активность за день
clean <- base[!is.na(base$steps),]
 

##создать интервал с средним количеством шагов
intervalTable <- ddply(clean, .(interval), summarize, Avg = mean(steps))
 

##Создать график среднего числа шагов в интервале
p <- ggplot(intervalTable, aes(x=interval, y=Avg), xlab = "Интервал", ylab="Среднее количество шагов")
p + geom_point()+xlab("Интервал")+ylab("Среднее количество шагов")+ggtitle("Среднее количество шагов за интервал")
 

#максимальное количество шагов за интервал
maxSteps <- max(intervalTable$Avg)
 
##Какой интервал содержит максимальное среднее количество шагов
intervalTable[intervalTable$Avg==maxSteps,1]
 

##Количество NA в исходном наборе данных
nrow(base[is.na(base$steps),])
 

## Create the average number of steps per weekday and interval
avgTable <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))
 

## Создать набор данных со всеми NA для замещения
nadata<- base[is.na(base$steps),]
 
## Слияние данных NA c средним недельным интервалом
newdata<-merge(nadata, avgTable, by=c("interval", "day"))
 

## Переупорядочивание данных с подстрокой в том же формате что и исходные данные
newdata2<- newdata[,c(6,4,1,2,5)]
colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")
 
##Слияние средних значений из набора данных с NA и без 
mergeData <- rbind(clean, newdata2)
 

##Сумма шагов для каждой даты
summ2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum, )
colnames(summ2)<- c("Date", "Steps")
 
## Среднее количество шагов с NA
as.integer(mean(summ2$Steps))
 
## Медиана шагов с данными NA
as.integer(median(summ2$Steps))
 
## Диаграмма суммарного количества шагов в день
hist(summ2$Steps, breaks=5, xlab="Шаги", main = "Общее количество шагов с NA", col="red")
hist(summ$Steps, breaks=5, xlab="Шаги", main = "Общее количество шагов с NA", col="blue", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("red", "blue") )
 


mergeData$DayCategory <- ifelse(mergeData$day %in% c("суббота", "воскресенье"), "Weekend", "Weekday")
library(lattice) 
library("lattice", lib.loc="C:/Program Files/R/R-3.3.1/library")
intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))
xyplot(Avg~interval|DayCategory, data=intervalTable2, type="o",  layout = c(1,2),
       main="Среднее число шагов за дневной интервал", 
       ylab="Среднее число шагов", xlab="Интервал")
