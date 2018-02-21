getwd()
setwd("/media/hans/907CE7487CE727A4/ARBEITuniMR/merged_csv/")
########## breaks als Grundlage zum sortieren
breaks = read.csv(file = "../2009_2016_breakpoints.csv")
#sichern der zeiten als character in spalte 5 anfang und 6 ende
breaks[,5] <- as.character(breaks[,1])
breaks[,6] <- as.character(breaks[,4])
#umwandeln in posiX
breaks[,1] <- as.POSIXct(breaks[,5], format = "%d.%m.%Y %H:%M" )
breaks[,4] <- as.POSIXct(breaks[,6],  format = "%d.%m.%Y %H:%M")
#zeitliche dauer der events
FOG_dur = breaks[4]-breaks[1]
#da aufloesung 1min: yeit in stunde * 60 (min/stunde) = anzahl der minuten/ereignis
#entspricht der Anzahl von Zeilen wenn 1min = 1 zeile
steps_pro_event = as.numeric(FOG_dur$fogEnd)*60

###
sammeln = function(x, breaks = breaks, steps_por_event = steps_por_event){
  x_EVENTS = list()
  
  for(i in 1:dim(breaks)[1]) {
    if (length(which(x[, 1] == breaks[i, 1])) >= 1) {
      a = x[which(x[, 1] == breaks[i, 1]) : (which(x[, 1] == breaks[i, 1]) + steps_pro_event[i]) , ]
      a -> x_EVENTS[[i]]
    }
    else{
      "NA" -> x_EVENTS[[i]]
    }
  }
  return(x_EVENTS)
}


weristin = function(x){
  which(sapply(x, length) > 1)
}

datefr2col = function(col1, col2, data, format){
  
  time_data = paste(as.character(data[,col1]), as.character(data[,col2]))
  
  
  data[,1] <- as.POSIXct(time_data,  format = "%d.%m.%Y  %H:%M" )
  return(data)
}

sammelnVPF = function(x, breaks){
  x_EVENTS = list()
  
  for(i in 1:dim(breaks)[1]) {
    if (length(which(x[, 1] == breaks[i, 1])) >= 1) {
      a = x[min(which(x[, 1] == breaks[i, 1])) : max(which(x[, 1] == breaks[i, 4])) , ]
      a -> x_EVENTS[[i]]
    }
    else{
      "NA" -> x_EVENTS[[i]]
    }
  }
  return(x_EVENTS)
}


####### anpassen der nicht minuetlichen daten

vpf710 = read.csv("vpf710/20091206_20141111_VPF710_20Sek.csv", header = F)


### abschneiden der sekunden (HMS hour minute second)
split_HMS = strsplit(as.character(vpf710[,2]), split = ":")

HM = c()
for(i in 1:dim(vpf710)[1]){
  a = paste(split_HMS[[i]][1], split_HMS[[i]][2], sep = ":")
  a -> HM[i]
}

HM -> vpf710[,(length(vpf710)+1)]

vpf710 = datefr2col(1, 8, vpf710, "%d.%m.%Y  %H:%M")

saveRDS(data[,1:7], "vpf710_1step")

vpf710 = readRDS("vpf710_1step")
#vpf710[,3] <- as.numeric(as.character(vpf710[,3]))
vpf710[,5] <- as.numeric(as.character(vpf710[,5]))
vpf710[,6] <- as.numeric(as.character(vpf710[,6]))
vpf710[,7] <- as.numeric(as.character(vpf710[,7]))



vpf710_EVENTS_ohnemean = sammelnVPF(vpf710, breaks)
saveRDS(vpf710_EVENTS_ohnemean, "vpf710_EVENTS_ohnemean")
weristin(vpf710_EVENTS_ohnemean)


meanvpf710 = function(x, breaks) {
  EVENTS_1min = list()
  for(lis in 1:length(x)) {
    print(lis)
    if(length(x[[lis]])[1] > 1 ) {
      levlen = length(levels(as.factor(x[[lis]][, 1])))
      gemittelt = matrix(ncol = 5, nrow = levlen)
      dates = c()
      for(lev in 1:levlen) {
        a = x[[lis]][c(which(x[[lis]] == levels(as.factor(x[[lis]][, 1]))[lev])) ,]
        levels(as.factor(vpf710_EVENTS_ohnemean[[lis]][,1]))[lev] -> dates[lev]
        mean(a[, 3]) -> gemittelt[lev, 2]
        mean(a[, 5]) -> gemittelt[lev, 3]
        mean(a[, 6]) -> gemittelt[lev, 4]
        mean(a[, 7]) -> gemittelt[lev, 5]
      }
      gemittelt = data.frame(gemittelt)
      dates -> gemittelt[,1]
      gemittelt -> EVENTS_1min[[lis]]
    }
    else{
      "NA" -> EVENTS_1min[[lis]]
    }
  }
  return(EVENTS_1min)
}   


vpf710_EVENTS_ohnemean = readRDS("vpf710_EVENTS_ohnemean")

vpf710_EVENTS = meanvpf710(vpf710_EVENTS_ohnemean, breaks)

saveRDS(vpf710_EVENTS, "vpf710_1min_EVENTS")
vpf710_1min_EVENTS = readRDS("vpf710_1min_EVENTS")

for(lis in 1:length(x)) {
  if(length(x[[lis]]) > 1 ) {
    as.POSIXct(x[[lis]][,1]) -> x[[lis]][,1]
    #, format = "" 
  }}
saveRDS(x, "vpf710_1min_EVENTS")



######## 730

vpf730 = read.csv("vpf730/20091206_20141111_VPF730_20Sek.csv", header = F)

vpf730 = datefr2col(1, 2, vpf730, "%d.%m.%Y %H%M")

vpf730_EVENTS_ohnemean = sammelnVPF(vpf730, breaks) 

rm(vpf730)

meanvpf730 = function(x, breaks) {
  EVENTS_1min = list()
  for(lis in 1:length(x)) {
    print(lis)
    if(length(x[[lis]])[1] > 1 ) {
      levlen = length(levels(as.factor(x[[lis]][, 1])))
      gemittelt = matrix(ncol = 9, nrow = levlen)
      dates = c()
      for(lev in 1:levlen) {
        for(col in 3:10){
        a = x[[lis]][c(which(x[[lis]] == levels(as.factor(x[[lis]][, 1]))[lev])) ,]
        levels(as.factor(x[[lis]][,1]))[lev] -> dates[lev]
        mean(a[, col]) -> gemittelt[lev, (col-1)]
        }
      }
      gemittelt = data.frame(gemittelt)
      dates -> gemittelt[,1]
      gemittelt -> EVENTS_1min[[lis]]
    }
    else{
      "NA" -> EVENTS_1min[[lis]]
    }
  }
  return(EVENTS_1min)
} 

vpf730_EVENTS1 = meanvpf730(vpf730_EVENTS_ohnemean, breaks[1:90])
saveRDS(vpf730_EVENTS1, "vpf730_EVENTS1")
weristin(vpf730_EVENTS2)
         
vpf730_EVENTS2 = meanvpf730(vpf730_EVENTS_ohnemean, breaks[91:190])
saveRDS(vpf730_EVENTS2, "vpf730_EVENTS2")
weristin(vpf730_EVENTS2)
