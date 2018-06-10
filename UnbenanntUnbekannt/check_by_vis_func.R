
###########phase ########
#findet die Phasen, dh zusammenhängende Sequenzen mit sichtweite < 1km
#gibt liste mit phasennummer als index und darin ist die reihennummer von den file mit den messungen

phase = function(kleiner1){
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
return(liste)
}



########durdis#######
#erstellt tab mit der Dauer bis zur nächsten Phase und die Länge der Phase
#input ist die mit phase() erstellte liste

durdis = function(liste){
  dif = c()
len = c()
for(i in 1:(length(liste)-1)){
  dif[i] = min(liste[[i+1]]-max(liste[[i]]))
  len[i] = length(liste[[i]])
}
dur_dis = data.frame(dif, len)
names(dur_dis) = c("dif", "dur")
return(dur_dis)
}


#######gruppen####
#stellt anhand des schwellenwertes (zeit die "definitiv" zwischen zwei Nebelereignissen liegt) Gruppen zusammen
#input ist das ergebnis von durdist(), schwellenwert muss dem Datenformat angepasst werden
#in diesem fall wird alle 20 sek byw 3mal pro minute gemessen dh die zahl an reihen muss mit diesem Faktor gestreckt werden
#3 -> 1minute 120 -> 40minuten 360 -> 120min|2std

gruppen = function(dur_dis, schwellenwert){
  cuts = which(dur_dis[,1] >= schwellenwert)
  poss_fog = c()
  no_fog = c()
  pos_start = c()
  pos_end = c()
  #(length(cuts)-1)
  for( i in 1:(length(cuts)-1)){
    a = cuts[i]+1
    b = cuts[i+1]
    c = cuts[i+1]-1
    d = cuts[i]
####strg x--->
     poss_fog[i] <-sum(dur_dis[a:b,2], na.rm = T)
    # pos_start[i] <- a
    # pos_end[i] <- b
    if(a == b ){
      no_fog[i] <- sum(dur_dis[d,1], na.rm = T)
      pos_start[i] <- d
      pos_end[i] <- d
      #print(paste(i ,", a ist gleich b "))
    } else if(a == c){
###strg v sinn ______
      #poss_fog[i] <-sum(dur_dis[a:b,2], na.rm = T)
      no_fog[i] <- dur_dis[c,1]
      pos_start[i] <- a
      pos_end[i] <- b
      
      #print(paste(i, ", a ist gleich c"))
### strg v //
    }else{
      no_fog[i] <- sum(dur_dis[a:c,1], na.rm = T)
      pos_start[i] = a
      pos_end[i] = c
      #print(paste(i, ", else  unten"))
    }
  }
  fog_pairs = data.frame(poss_fog, no_fog, pos_start, pos_end)
  names(fog_pairs) = c("poss_fog", "no_fog", "pos_start", "pos_end")
  return(fog_pairs)
}




#########plotfunktionen ########
#zum visualisieren; input sind Phasennummern
plotme = function(x,y, h= 3){
  x1= liste[[x]][1]
  y1= liste[[y]][length(liste[[y]])]
  plot(c(x1:y1), vis[x1:y1], type = "l", ylim = c(0,h), main = paste0("start ", date_ch[x1], ", ende ", date_ch[y1]),  sub =  paste0(((y1-x1)/180)," stunden"))
  abline(h = 1, col = "red")
}

plotevents = function(ev, h = 3){
  x = events[ev, 3]
  y = events[ev, 4]
  x1= liste[[x]][1]
  y1= liste[[y]][length(liste[[y]])]
  plot(c(x1:y1), vis[x1:y1], type = "l", ylim = c(0,h), main = paste0("start ", date_ch[x1], ", ende ", date_ch[y1]),  sub =  paste0(((y1-x1)/180)," stunden"))
  abline(h = 1, col = "red")
}


plot_by_date = function(ev , h = 3, buf = 1.5){
  buffer = 60*60*buf
  x = events[ev, 1]
  y = events[ev, 2]
  x1 = which(date_posix == x)[1]
  y1 = which(date_posix == y)[1]
  if(is.na(x1) || is.na(y1)){
    plotevents(ev)
  }else{
  
  plot(date_posix[x1:y1], vis[x1:y1], type = "l", ylim = c(0,h), main = paste0( "Event ", ev, "   ",  date_posix[x1] + buffer, " - ", date_posix[y1] - buffer) )
  #paste0( "# ", ev,  date_posix[x1] + 5400, " - ", date_posix[y1] - 5400),  sub =  paste0(((y1-x1)/180)," stunden")
  abline(h = 1, col = "red")
  abline(v = events[,1] + buffer, col = "green")
  abline(v = events[,2] - buffer, col = "green")
  }
}
############### unsicher_ident, func_suchen, func_suchen_start ############
# sollen die Gruppen, die nicht innerhalb die einfache bedingung: 
# (nebelzeit länger als 30 min UND nebelzeit * 0.66 >= nicht_nebelzeit)
# fallen identifizieren; gibt start- und endphasennummer aus


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

#
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


####################Nebelereignisse#########
#gibt Nebelereignisse mit Zeiten aus

nebelereignisse = function(unsicher_fog_pairs, jedenfall_fog_pairs) {
  erg = list()
  for (i in 1:dim(unsicher_fog_pairs)[1]) {
    erg[[i]] <- unsicher_ident(unsicher_fog_pairs[i, 3], unsicher_fog_pairs[i, 4])
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
      unlist(erg[[i]])[1] -> start_u[c]
      unlist(erg[[i]])[2] -> end_u[c]
      c = c + 1
    }
  }
  start_u = unlist(start_u)
  end_u = unlist(end_u)
  start = sort(c(start_j, start_u))
  end = sort(c(end_j, end_u))
  
  #### SICHERHALTSHALBER #### |END = END +1 | #############
  end = end+1
  ########################
  
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


##### DEBUG######
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


