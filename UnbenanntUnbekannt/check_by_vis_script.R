########### vis daten einlesen ###############
setwd("/home/hannes/Dokumente/UniMR/Arbeit/Nebel/")
vpf730 = read.csv(file = "check_by_vis/all/all_VPF730_20Sek2.csv", header= FALSE)

######### selektieren der werte kleiner 1 km sichtweite, datumsdaten erstellen #################
vis = as.numeric(as.character(vpf730[,3]))
kleiner1 = which(vis <= 1)
date_ch = paste(vpf730[,1],vpf730[,2] )
date_posix = as.POSIXct(date_ch,  format = "%d.%m.%Y  %H:%M")

######## nebelphasen erkennen, verhältnis von nebel und klarer sicht, nebelgruppen anhand schwellenwertes bestimmen ##################
liste = phase(kleiner1)
dur_dis = durdis(liste)
fog_pairs = gruppen(dur_dis, 360)

######### yuvor erstelle gruppen nach nebelkriterium klassifizieren #################
long_enough = which( fog_pairs[,1] >= 90)
long_enough_fog_pairs = fog_pairs[long_enough, ]

jedenfall = which(fog_pairs[long_enough,1]*0.66 > fog_pairs[long_enough,2])
jedenfall_fog_pairs = fog_pairs[long_enough,][jedenfall,]

unsicher = which(fog_pairs[long_enough,1]*0.66 < fog_pairs[long_enough,2])
unsicher_fog_pairs = fog_pairs[long_enough, ][unsicher, ]

########### nebelevents ausrechnen ###############
events = nebelereignisse(unsicher_fog_pairs, jedenfall_fog_pairs)

######## zur besser verarbeitung werden um die ereignisse ein 1,5 stunden buffer gelegt  7200 sekunden = 2 std#####
events[,5] = events[,1] - 5400
events[,6] = events[,2] + 5400
names(events)[c(5,6)] = c("plot_start", "plot_end")

