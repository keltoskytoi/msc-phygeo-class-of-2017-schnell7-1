#### functionen ####

count_stunden = function(timeset, border){
  count_ = list()
  c = 1
  for(i in 1:length(border)){
    if(i <length(border)){
      count_[[i]] <- which(timeset <= border[i+1] & timeset >= border[i])
    }else{
      
    }
  }
  names(count_) = border[-length(border)]
  return(count_)
  
}

qx = function(i){
  q_x = c(numbers_jzeiten[[1 +i]],numbers_jzeiten[[5+i]],numbers_jzeiten[[9+i]],numbers_jzeiten[[13+i]],numbers_jzeiten[[17+i]],
          numbers_jzeiten[[21+i]],numbers_jzeiten[[25+i]],numbers_jzeiten[[29+i]],numbers_jzeiten[[33+i]])
  return(q_x)
}


plot_start_end = function(qx, quart){
  par(mfrow = c(2,2))
  c = 1
  for(i in qx){
    plot(start_daytime_posix[i], 1:length(i), main = quart[c], xlim = c(minuhr, maxuhr))
    points(end_daytime_posix[i]  , 1:length(i),  col = "red")
    c = c+1
  }
  par(mfrow= c(1,1))
}


plot_start_end_2 = function(qx, quart){
  par(mfrow = c(2,2))
  c = 1
  for(i in qx){
    plot(tages_zeiten_posix[1:24], sapply(count_stunden(start_daytime_posix[i], tages_zeiten_posix), length), type = "h",main = quart[c], xlim = c(minuhr, maxuhr), ylab= "count", xlab = "Tageszeit")
    lines(tages_zeiten_posix[1:24]+1800, sapply(count_stunden(end_daytime_posix[i], tages_zeiten_posix), length), type = "h", col = "red")
    c = c+1
  }
  par(mfrow=c(1,1))
}
#### start variablen ####
dauer = events[, 2] - events[,1]
dauer_num = as.numeric(dauer)

#### erstellen von datumsgrenzen ####
jahre = c()
c = 1
jahres = (2009:2017)
for (i in jahres){
  #jahre[c] <- as.character(i)
  jahre[c] <- paste0("1.1.", i)
  c = c+1
}
jahre = as.POSIXct(jahre, format= "%d.%m.%Y")


jahres_zeiten = c()
c = 1
step_jahres_zeit = c("21.3.", "21.6.", "23.9.", "21.12.")
for(k in jahres){
  for(i in step_jahres_zeit){
    jahres_zeiten[c] <- paste0(i, k)
    c=c+1
  }
}
jahres_zeiten = as.POSIXct(jahres_zeiten, format= "%d.%m.%Y")


tages_zeiten = c()
c = 1
for(i in 0:24){
  if(i <10){
  tages_zeiten[c] <- paste0("0",i, ":00:00")
  }else{
    tages_zeiten[c] <- paste0(i, ":00:00")
  }
  c= c+1
}
tages_zeiten_posix = as.POSIXct(tages_zeiten, format="%H:%M:%S")




#### zusammenhang zwischen jahreskategorien und index von events ####

numbers_j = list()
for(i in 1:length(jahre)){
  if(i < length(jahre)){
    numbers_j [[i]] <- which(events[,1] <= jahre[i+1] & events[,1] >= jahre[i])
  }else{
    
  }
}
names(numbers_j) = jahre[-length(jahre)]


numbers_jzeiten = list()
for(i in 1:length(jahres_zeiten)){
  if(i < length(jahres_zeiten)){
    numbers_jzeiten [[i]] <- which(events[,1] <= jahres_zeiten[i+1] & events[,1] >= jahres_zeiten[i])
  }else{
    #dummyzeile
    numbers_jzeiten[[i]] <- which(events[,3] == 258936289356289356289356)
  }
}
names(numbers_jzeiten) = jahres_zeiten


q_1 = qx(0)
q_2 = qx(1)
q_3 = qx(2)
q_4 = qx(3)
numbers_quartal = list(q_1, q_2, q_3, q_4)
quartale = c("spring", "summer", "fall", "winter")
names(numbers_quartal) = quartale




start_daytime <- strftime(events[,1], format="%H:%M:%S")
end_daytime <- strftime(events[,2], format="%H:%M:%S")

start_daytime_posix <- as.POSIXlt(start_daytime, format="%H:%M:%S")
end_daytime_posix <- as.POSIXlt(end_daytime, format="%H:%M:%S")
minuhr = as.POSIXct("00:00:00", format="%H:%M:%S")
maxuhr = as.POSIXct("23:59:59", format="%H:%M:%S")







#################### statistic #######################################
hist(dauer_num[which(dauer_num <1000)])
hist(dauer_num[which(dauer_num >1000)])


#Nebelereignisse insgesamt,
nebel_ges = dim(events)[1]
#Nebelstunden insgesamt,
nebelstunden_ges =sum(dauer_num)/60

#Nebelereignisse pro Jahr, 
nebel_pro_jahr = sapply(X = numbers_j, FUN = length)

plot(jahre[-9],nebel_pro_jahr, type="h")


#Nebelstunden pro Jahr,
nebelstunden_pro_jahr = sapply(numbers_j, function(x){
  sum(dauer_num[x])/60
})

plot(jahre[-9], nebelstunden_pro_jahr, type= "h")


#Nebelereignisse pro Jahreszeit,
nebel_pro_quartal = sapply(numbers_quartal, length)

plot( nebel_pro_quartal, type = "h", xaxt = "n")
axis(1, at=c(1,2,3,4), labels= quartale)


# Nebelstunden pro Jahrzeit,
nebelstunden_pro_quartal = sapply(numbers_quartal, function(x){
  sum(dauer_num[x])/60
})

plot( nebelstunden_pro_quartal, type = "h", xaxt = "n")
axis(1, at=c(1,2,3,4), labels= quartale)


#Mittlere Dauer insgesamt,
mean(dauer_num)

#pro Jahr, pro Jahreszeit (Min, Max, Mean, Perzentile -> Boxplots)
boxplot(sapply(numbers_j, function(x){
  dauer_num[x]/60
}), main= "Dauer Nebelereignisse Pro Jahr [h]")

boxplot(sapply(numbers_quartal, function(x){
  dauer_num[x]/60
}), main= "Dauer Nebelereignisse Pro Jahreszeit [h]")


#Mittlerer Beginn insgesamt, pro Jahr, pro Jahreszeit (Min, Max, Mean, Perzentile -> Boxplots)
#Mittlerer Endzeitpunkt insgesamt, pro Jahr, pro Jahreszeit (Min, Max, Mean, Perzentile -> Boxplots)




plot_start_end(numbers_quartal, quartale)
plot_start_end(numbers_j, jahre)



plot_start_end_2(numbers_quartal, quartale)
plot_start_end_2(numbers_j, jahre)


############### summary #################




nebel_ges
nebel_pro_jahr
nebel_pro_quartal

nebelstunden_ges
nebelstunden_pro_jahr
nebelstunden_pro_quartal
