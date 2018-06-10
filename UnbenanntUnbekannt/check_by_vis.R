setwd("/home/hannes/Dokumente/UniMR/Arbeit/Nebel/")
vpf730 = read.csv(file = "check_by_vis/all/all_VPF730_20Sek2.csv", header= FALSE)
# werte mit Na  c(730792,735072,739352,745071,750791,756511,762231,767951,773671,779391,)

#einlesen
vis = as.numeric(as.character(vpf730[,3]))
#messungen mit kleiner als 1km sichtweite
kleiner1 = which(vis <= 1)
lv =length(vis)

# vis_agg = c()
# for(i in 0:320181){
#   vis_agg[i+1] <- mean(vis[i*3+1], vis[i*3+2], vis[i*3+3], na.rm = T)
# }
# 
# date_ch = c()
# for(i in 1:8766994){
#   #print(i)
#   date_ch[i] <- paste(vpf730[i,1],vpf730[i,2] )
# }
# 
# miss = c(730792,735072,739352,745071,750791,756511,762231,767951,773671,779391)
# vpf730[miss,]
# vpf730[730790:730800,]
# 

date_ch = paste(vpf730[,1],vpf730[,2] )













c = c(1,23,56,43,4)
c[length(c)]

#plotspass
plotme = function(x,y, h){
  x1= liste[[x]][1]
  y1= liste[[y]][length(liste[[y]])]
  plot(c(x1:y1), vis[x1:y1], type = "l", ylim = c(0,h), main = paste0("start ", date_ch[x1], ", ende ", date_ch[y1]),  sub =  paste0(((y1-x1)/180)," stunden"))
  abline(h = 1, col = "red")
}
plot( c(15000:15500), vis[15000:15500])
plot( seq(from = 1.66, to = 166.66, by= 0.33)/60, vis[15000:15500])
abline(h = 1)

plot( vpf730[15000:15500, 2], vis[15000:15500])
kl=1
gr= 500000
plot( c(kl:gr), vis[kl:gr], type= "l")
abline(h = 1, col = "red")



#liste = list()
# i=2
# j=1
# k=1
# while (i < length(kleiner1)+1) {
#   if(kleiner1[i+1] == kleiner1[i]+1 | kleiner1[i-1] == kleiner1[i]-1 ){
#     kleiner1[i] -> liste[[j]]
#     kleiner1[i] -> liste[[j]][k]
#     k=k+1
#     i=i+1
#   }else{
#     k=1
#     j=j+1
#     i=i+1
#   }
# }


#erfasst zusammenhaegende Messungen  < 1km (ab laenge 1 ) und fugt diese in einer liste zusammen
j = 1
k = 1
switch = T
geb = c()
liste=list()
for(i in 2:(length(kleiner1)-1)){
  print(i)
  if(kleiner1[i-1] == kleiner1[i]-1){
    #kleiner1[i] -> liste[[j]]
    kleiner1[i] -> geb[k]
    k=k+1
    
  }
  if(kleiner1[i+1] == kleiner1[i]+1){
    #kleiner1[i] -> liste[[j]]
    kleiner1[i] -> geb[k]
    k=k+1
  }else{
    if(length(geb) < 1){
      kleiner1[i] -> geb[k]
    }
    as.numeric(levels(as.factor(geb))) -> liste[[j]]
    geb = c()
    k = 1
    j = j+1
    
  }

}

#bestimmt den abstand bis yum nachsten ereignis dif und die lange der sichtweite <1km len
dif = c()
len = c()
for(i in 1:(length(liste)-1)){
  dif[i] = min(liste[[i+1]]-max(liste[[i]]))
  len[i] = length(liste[[i]])
}
dif
len
dur_dis = data.frame(dif, len)
names(dur_dis) = c("dif", "dur")
dur_dis


#suche nach stellen wo sehr lange zwischen sichtweite <1km ist
#und berechne die dauer eines ereignisses
#sowie die yeit wo sichtweite uber 1km liegt 
cuts = which(dur_dis[,1] >= 360)
poss_fog = c()
no_fog = c()
pos_start = c()
pos_end = c()
for( i in 1:(length(cuts)-1)){
  a = cuts[i]+1
  b = cuts[i+1]
  c = cuts[i+1]-1
  d = cuts[i]
  poss_fog[i] <-sum(dur_dis[a:b,2], na.rm = T)
  pos_start[i] = a
  pos_end[i] = b
  if(a ==b ){
    no_fog[i] <- sum(dur_dis[d,1], na.rm = T)
    pos_start[i] = d
    pos_end[i] = d
  }else{
  no_fog[i] <- sum(dur_dis[a:c,1], na.rm = T)
  pos_start[i] = a
  pos_end[i] = c
  }
}

fog_pairs = data.frame(poss_fog, no_fog, pos_start, pos_end)
names(fog_pairs) = c("poss_fog", "no_fog", "pos_start", "pos_end")
no_fog[1:10]
poss_fog[1:10]
dur_dis[1:10,]
sum(dur_dis[23:39,2])

#muss min 30 min (30*3=90) nebel sein
long_enough = which( fog_pairs[,1] >= 90)
long_enough_fog_pairsfog_pairs[long_enough, ]

jedenfall = which(fog_pairs[long_enough,1]*0.66 > fog_pairs[long_enough,2])
jedenfall_fog_pairs = fog_pairs[long_enough,][jedenfall,]

unsicher = which(!fog_pairs[long_enough,1]*0.66 > fog_pairs[long_enough,2])
unsicher_fog_pairs = fog_pairs[long_enough, ][unsicher, ]

for(j in 1:length(unsicher)){
  j = 215
plotme(fog_pairs[long_enough, ][unsicher, ][j,3], fog_pairs[long_enough, ][unsicher, ][j,4] , 2)
}


plotme(12732  , 12794)
length(which(vis[6960000:6970000] < 1))








for ( i in 10035:10058){
  abline(v = liste[[i]][1], col = 7)
  abline(v = liste[[i]][length(liste[[i]])], col = 7)
}

los = 10035
stop = 10058

func_suchen = function(los, stop){
start = c()
end = c()
weight = c()
diff = c()
write = 1
for(i in los:stop){
  for( j in 1:(stop-i)){
    fog_time = sum(dur_dis[i : (i +j) , 2])
    no_fog_time = sum(dur_dis[i : (i +j) , 1])
    if(fog_time >= 90 && (fog_time/no_fog_time) >= 1.5 ){

    weight[write] <- (fog_time/no_fog_time)
    start[write] <- (i)
    end[write] <- (i+j)
    diff[write] <- j
    write = write +1
    }
  }
}
sub = cbind(start, end,diff,  weight)
return(sub)
}


func_suchen_start = function(sub){
  if(length(sub) >0 ){
  index = which(sub[,3] == max(sub[,3]))[1]
  return(sub[index,1:2])
  }else{
    return(NULL)
  }
}




unsicher_ident = function(los, stop) {
  erg = list()
  sub0 = func_suchen(los, stop)
  if (!is.null(sub0)) {
    erg[[1]] = func_suchen_start(sub0)
    
    if (length(func_suchen(los, erg[[1]][1])) > 0) {
      sub1 = func_suchen(los, erg[[1]][1])
      erg[[2]] = func_suchen_start(sub1)
    }
    if (length(func_suchen(erg[[1]][2], stop)) > 0) {
      sub2 = func_suchen(erg[[1]][2], stop)
      erg[[3]] = func_suchen_start(sub2)
    }
    return(erg)
  }else{
    return(NULL)
  }
}




erg = list()
for(i in 1:dim(unsicher_fog_pairs)[1]){
  erg[i] <- unsicher_ident(unsicher_fog_pairs[i,3], unsicher_fog_pairs[i,4])
}
erg

z = c()
c = 1
for(i in 1:length(erg)){
  if(length(erg[[i]]) == 2){
    z[c] <- i
    c = c+1
  }
}


los = 206
stop =229
plotme(los, stop, 4)
ga = func_suchen(los, stop)
gr = func_suchen_start(ga)
func_suchen(los, gr[1] ) 
func_suchen(gr[2], stop)
#fin
sub = data.frame(start, end,diff,  weight)
sub

plotme(10053, 10058, 3)


nebelereignisse = function(unsicher_fog_pairs, jedenfall_fog_pairs) {
  erg = list()
  for (i in 1:dim(unsicher_fog_pairs)[1]) {
    erg[i] <-
      unsicher_ident(unsicher_fog_pairs[i, 3], unsicher_fog_pairs[i, 4])
  }
  start_j = c()
  end_j = c()
  for (i in 1:dim(jedenfall_fog_pairs)[1]) {
    start_j[i] <- jedenfall_fog_pairs[i, 3]
    end_j[i] <- jedenfall_fog_pairs[i, 4]
  }
  c = 1
  start_u = c()
  end_u = c()
  for (i in 1:length(erg)) {
    if (!is.null(erg[[i]])) {
      erg[[i]][1] -> start_u[c]
      erg[[i]][2] -> end_u[c]
      c = c + 1
    }
  }
  start = sort(c(start_j, start_u))
  end = sort(c(end_j, end_u))
  
  c = 1
  date_start = c()
  date_end = c()
  for (i in start) {
    date_start[c] <- date_ch[liste[[i]][1]]
    c = c + 1
  }
  c = 1
  for (i in end) {
    date_end[c] <- date_ch [liste[[i]] [length(liste[[i]])]]
    c = c + 1
  }
  date_start = as.POSIXct(date_start, format = "%d.%m.%Y  %H:%M")
  date_end = as.POSIXct(date_end, format = "%d.%m.%Y  %H:%M")
  return(data.frame(date_start, date_end, start, end))
}


par(mfrow= c(1,2))
#par(mfrow = c(1,1))
# 1:dim(events)[1]
# 
for(i in huhu) {
  plotme((events[i,3]), (events[i,4]), 3)
  plotme((events[i,3])-1, (events[i,4]+2), 3)
  #par(main = i)
  abline(v = liste[[events[i,3]]][1], col = "green")
  abline(v = liste[[events[i,4]]][length(liste[[events[i,4]]])], col = "green")
}


test = events[,2] - events[,1]
huhu = which(test < 50)


gibtesrechtsvielnebel = c(F, F, F, F, F, T, F, F, T, F, F, F, F, F, T, T, F, F, F, F, T, F, F, T, F, T, F, F, T, T, F, T, F)
fraglich = which(gibtesrechtsvielnebel == T)

#par(mfrow= c(1,2))
par(mfrow = c(1,1))
for(j in fraglich){
  i = huhu[j]
  plotme((events[i,3]), (events[i+1,4]), 3)
  abline(v = liste[[events[i,3]]][1], col = "green")
  abline(v = liste[[events[i,4]]][length(liste[[events[i,4]]])], col = "green")
  abline(v = liste[[events[i+1,3]]][1], col = "blue")
  abline(v = liste[[events[i+1,4]]][length(liste[[events[i+1,4]]])], col = "blue")
}

pick_random = function(){
  par(mfrow= c(2,2))
  sam = sample(1:dim(events)[1], 1)
  print(sam)
p_r = events[sam,]
plotme(p_r[,3], p_r[,4], 3)
plotme(p_r[,3], p_r[,4]+1, 3)
plotme(p_r[,3], p_r[,4]+2, 3)
plotme(p_r[,3]-1, p_r[,4]+2, 3)
}


pick_random()


pdf("plots_by_date.pdf", width = 14)
par(mfrow = c(3,3))
for(i in 1:dim(events)[1]){
  #print(i)
  plot_by_date(i)
}
dev.off()


debug_plbd= c(84, 89, 90)





#statistic
dauer = events[, 2] - events[,1]
dauer_num = as.numeric(dauer)
hist(dauer_num[which(dauer_num <1000)])
hist(dauer_num[which(dauer_num >1000)])

##as.POSIXct("06-2009", format = "%M-%Y")
#nach jahr
jahre = c()
c = 1
jahres = (2009:2017)
for (i in jahres){
  #jahre[c] <- as.character(i)
  jahre[c] <- paste0("1.1.", i)
  c = c+1
}
jahre = as.POSIXct(jahre, format= "%d.%m.%Y")
jahre

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
jahres_zeiten

numbers_j = list()
for(i in 1:length(jahre)){
  if(i < length(jahre)){
    numbers_j [[i]] <- which(events[,1] <= jahre[i+1] & events[,1] >= jahre[i])
  }else{
    
  }
}
numbers_j
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
numbers_jzeiten

names(numbers_jzeiten) = jahres_zeiten

qx = function(i){
q_x = c(numbers_jzeiten[[1 +i]],numbers_jzeiten[[5+i]],numbers_jzeiten[[9+i]],numbers_jzeiten[[13+i]],numbers_jzeiten[[17+i]],
       numbers_jzeiten[[21+i]],numbers_jzeiten[[25+i]],numbers_jzeiten[[29+i]],numbers_jzeiten[[33+i]])
return(q_x)
}

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







####################################################################################
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

plot_start_end(numbers_quartal, quartale)
plot_start_end(numbers_j, jahre)


nebel_ges
nebel_pro_jahr
nebel_pro_quartal

nebelstunden_ges
nebelstunden_pro_jahr
nebelstunden_pro_quartal
