getwd()

###NAMENSERSTELLUNG######
tab = read.csv("../Rtmp/nebelEREIGNISSE.txt", header = FALSE)
#Für levels(a) (Zeile 16)
b = as.character(tab[,1])


#t
event_name = strsplit(b, split =  "_")

ersT = c()
zweiT = c()
for(i in 1:length(event_name)){
  knecht =  strsplit(event_name[[i]][2], split = "-")
  ersT[i] <- paste0(knecht[[1]][1], knecht[[1]][2])
  ruprecht =  strsplit(event_name[[i]][5], split = "-")
  zweiT[i] <- paste0(ruprecht[[1]][1], ruprecht[[1]][2])
}

file_names = c()
for(i in 1:length(event_name)){
  file_names[i] <- paste0("EVENT_", event_name[[i]][1], "_",ersT[i], "__", event_name[[i]][4],"_", zweiT[i], " = list()")
}

#EINLADUNG DER RDS##########
#cmpb_1min_CNR4_EVENTS = readRDS(file = "cmpb_1min_CNR4_EVENTS")
dieliste = list.files(path = "./events/")
read_rds = c()
for(i in 1:length(list.files(path = "./events/"))){
  read_rds[i] <- paste0(dieliste[i], "= readRDS(file =\"./events/", dieliste[i], "\")" )
}


#UEBERSENDUNG DER EVENTS####
fileX = c()
for(i in 1:length(event_name)){
  fileX[i] <- paste0("EVENT_", event_name[[i]][1], "_",ersT[i], "__", event_name[[i]][4],"_", zweiT[i])
}
send = c()
nr = 1
for(i in 1:length(read_rds)){
  for(k in 1:length(file_names)){
    send[nr] <- paste0(dieliste[i], "[[", k, "]]", " -> ", fileX[k], "[[", i, "]]")
    nr = nr+1
  }
}

#BAPTISM#####
argh= c( "cl31_1min_bckscttr", "cl31_1min_cldbase", "cl31_1min_prepro", "cldrdr_cldinfo", "cldrdr_preproc", "cldrdr_Zrangecor", "cmpb_1min_CNR4", "cmpb_1min_Temp_RH", "cmpb_1min_Temp_RH_Radi", "mrr", "usa", "vpf710_1min", "vpf7301_1min" )
taufe=c()
for(i in 1:length(fileX)){
  taufe[i] <- paste0("names(", fileX[i], ") = " , paste0("c(\"", paste0(argh, collapse = "\" , \""), "\")"))
}

#SPEICHERUNG#######

schluss = c()
for(i in 1:length(fileX)){
  schluss[i] <- paste0("saveRDS(object= ", fileX[i], ", file = \"./nebelereignisse/", fileX[i], "\")" )
}
#########SPEICHERUNG##############
write(x = c(file_names, read_rds, send, taufe, schluss), file = "klatschbumm.R")

source(file ="klatschbumm.R")
