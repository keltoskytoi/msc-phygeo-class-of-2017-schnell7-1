#%%############################ brocken #######################################
#sehr lange
#harz-brocken
#D:/UniData/py/raster
#/home/hannes/Dokumente/UniMR/py
ar = raster2array("D:/UniData/py/raster/harzi.tif")
woist(1137.05,2,ar)
#%%77,147
estand(77,147, ar, 530, 200, 128)
#hoehe:  1137.050048828125 
#dominanz:  223070.30281953712 dauer  102 sek
#prominenz:  860.0
#eigenstand:  1.1209809402150823
# literaturwerte 
#%%https://de.wikipedia.org/wiki/Brocken
#eigenstand(1141.2, 224000, 856)
#http://www.peakbagger.com/list.aspx?lid=4343
#eigenstand(1141  , 223100, 856)
#1.1232228946111869
#eigenstand(1141,223070,856)

#%%#############################################################################################################
#800er 1085.3502
ar = raster2array("D:/UniData/py/raster/harz_800.tif")
#woist(1085.3502,4,ar)
#zeile: 19 spalte: 36
#%%
estand(19,36, ar, 25, 800, 20)
#%%
hoehe:  1085.3502197265625 dominanz:  223442.1625387653 prominenz:  800.0
eigenstand:  1.1557598268199942 in: 0.015637430981284728  stunden


#================================== mti alt modul ================

hoehe:  1085.3502197265625 dominanz:  223442.1625387653 prominenz:  798.0
eigenstand:  1.1569635780041205 in: 0.047212657587386075  stunden

liefert aber gutes ergebnis fuer prominenz (853, wenn mit richtigen werten fuer hoehe gerechnet worden waere)
 
1085.3 -798
Out[185]: 287.29999999999995

1141-287.3
Out[186]: 853.7

#%%#############################################################################################################
#400er 1137.05
ar = raster2array("D:/UniData/py/raster/harz_400.tif")
woist(1137.05,2,ar)
#zeile: zeile: 38 spalte: 73
#%%
estand(38,73, ar, 25, 400, 128)
#%%
hoehe:  1137.050048828125 dominanz:  223221.86272854186 prominenz:  850.0
eigenstand:  1.1266055464032145 in: 0.5319227187108573  stunden




#%%###########################################################################################################
#600er 1137.05
ar = raster2array("D:/UniData/py/raster/harz_600.tif")
woist(1107.099,3,ar)
#zeile: zeile: 25 spalte: 48
#%%
estand(25,48, ar, 25, 600, 128)
#%%
hoehe:  1107.0989990234375 dominanz:  223684.95702661815 prominenz:  830.0
eigenstand:  1.13805604800014 in: 0.08232859853469159  stunden
#=========================================== mit alternierend ==============================
hoehe:  1107.0989990234375 dominanz:  223684.95702661815 prominenz:  822.0
eigenstand:  1.1427136955176749 in: 0.16057810848656826  stunden

wieder falsche hoehe; mit richtiger p=856 


#%%################################################################################################################
#1000er 1137.05
ar = raster2array("D:/UniData/py/raster/harz_eintausend.tif")
woist(1137.05,2,ar)
#zeile: 15 spalte: 29
#%%
estand(15,29, ar, 25, 1000, 128)
#%%
hoehe:  1137.050048828125 dominanz:  223331.59203301265 prominenz:  850.0
eigenstand:  1.1266055464032145 in: 0.009963159426572221  stunden

#===== mit alternierenden ansatz ===================================================================

hoehe:  1137.050048828125 dominanz:  223331.59203301265 prominenz:  846.0
eigenstand:  1.1288739390382525 in: 0.018778400191671003  stunden












#%%############################# kufstein #####################################
#kufstein
#D:/UniData/py
#/home/hannes/Dokumente/UniMR/py
ar = raster2array("D:/UniData/py/raster/KU_DGM10.asc")
#%%
#woist(1787.980957,6,ar)
#zeile: 1934 spalte: 1590
#
#%%
estand(1934,1590,ar,100, 10, 20)


#%%
ar = raster2array("D:/UniData/py/raster/ku_300.tif")
#%%
woist(1735.495, 3, ar)
#zeile: 64 spalte: 54
#%%
estand(64,54,ar,50,300,10)
#hoehe:  1735.495 dominanz:  2830.194339616981 prominenz:  410.0
#eigenstand:  3.191536879728998 in: 2.0505617146658512e-05  stunden
#http://www.gipfelseiten.de/gipfel.php?gipfel=frechjoch
#dominanz auf 10er raster:  2567.1969149249144
#%%
eigenstand(1787, 2567, 410)
#3.2384761548635517
#%%############################# taunus #######################################
#6std
#taunus-gr feldberg
#D:/UniData/py
#/home/hannes/Dokumente/UniMR/py
ar = raster2array("D:/UniData/py/raster/taunus.tif")
#woist(875.4925,4,ar)
##%%420,154
estand(420,154, ar, 50, 200, 128)
#%%
#hoehe:  875.4924926757812 dominanz:  101482.01811158466 prominenz:  670.0
#eigenstand:  1.2410394616298575
#http://www.thehighrisepages.de/bergtouren/na_tauns.htm
#h=879 estand=1,24 dom=101000 p=670
	


#%%sehr schnell
#taunus-kl feldberg 
#woist(821.2731,4,ar)
#zeile: 425 spalte: 150
#estand(425,150, ar, 1, 200, 128)
#hoehe:  821.2730712890625 dominanz:  1000.0 prominenz:  33.0
#eigenstand:  4.903532580216993
##http://www.thehighrisepages.de/bergtouren/na_tauns.htm
#h=825 estand=4,91 dom=930 p=35
	
#%%sehr schnell
#taunus-winterstein 
#woist(509.52810669, 8, ar)
#zeile: 367 spalte: 227
estand(367,227, ar, 1, 200, 1)
#hoehe:  509.5281066894531 dominanz:  10065.783625729295 prominenz:  164.0
#eigenstand:  3.02201742314082
#http://www.thehighrisepages.de/bergtouren/na_tauns.htm
#Steinkopf (Höchster Punkt im  Winterstein-Taunuskamm)
#h=518 estand=2,99 dom=10170  p=173
#%%sehr schnell
#taunus - johannisberg
#267.4231
#woist(267.4231,4,ar)
#zeile: 344 spalte: 247
estand(344,247,ar,1,200,1)
#hoehe:  267.423095703125 dominanz:  3000.0 prominenz:  23.0
#eigenstand:  4.548822467743755
#
#%%
#maxima im muf
#289.6425
#woist(398.55371094,8,ar)
#zeile: 84 spalte: 232
estand(84,232,ar,1,200,128)
#hoehe:  398.5537109375 dominanz:  2236.06797749979 prominenz:  65.0
#eigenstand:  4.190553333179099


