# -*- coding: utf-8 -*-
"""
Created on Thu Feb  1 13:18:06 2018

@author: hans
"""

#%%
def bestimmung_cfill(start,ar, st):
    if start[st][0]-1< 0:
        y1 = 0
    else:
        y1 = start[st][0]-1
    if start[st][0]+2 >= len(ar):
        y2 = len(ar)-1
    else:
        y2 = start[st][0]+2
    if start[st][1]-1 < 0:
        x1 = 0 
    else:
        x1 = start[st][1]-1
    if start[st][1]+2 >= len(ar[0]):
        x2 = len(ar[0])-1
    else:
        x2 = start[st][1]+2
    return(y1,y2,x1,x2)                  
#%%
def canigo(start, ar, fill, startval):
    kennichschon=start[:]
    neu=start[:]
    neustart=start
    while len(neu) > 0:
        neu=[]
        for st in range(len(start)):
            b = bestimmung_cfill(start,ar,st)
            for y in range(b[0], b[1]):
                for x in range(b[2], b[3]):
                    if ar[y][x] > ar[startval[0]][startval[1]]:
                        #print("groesseren Punkt gefunden")
                        return(True,kennichschon, fill)
                    if ar[y][x] > fill:
                        neu.append((y,x))
                        neu = list(set(neu))
        start=[]
        for i in range(len(neu)):
            if neu[i] not in kennichschon:
                start.append((neu[i][0], neu[i][1]))
                start = list(set(start))
        kennichschon = kennichschon + start
        #kennichschon = list(set(kennichschon))
        #print(len(neu), len(kennichschon), len(start))
    else:
        #print("kein weiterer Gipfel")
        start = neustart
        return(False, start, fill)
        #return(False, kennichschon, fill)        

#%%

            
def prominenz(start, ar, dwn_step):
    fillv = ar[start[0][0]][start[0][1]]
    startval = (start[0][0], start[0][1])
    gipfel = False
    fillserie = [int(dwn_step/2**y) for y in range(int(math.log(dwn_step,2)+1))]
    while gipfel == False:
        fillv = fillv - fillserie[0]
        print(fillv)
        t1 = time.clock()        
        gval = canigo(start, ar, fillv, startval)
        start = gval[1]
        gipfel = gval[0]
        neuer_fill = gval[2]
        print(neuer_fill, "im while")
        t2 = time.clock()
        print(t2-t1)
    serie_c = fillserie[1:]
    neuer_fill = neuer_fill + fillserie[1]
    print(neuer_fill, "vor for")
    for i in range(len(serie_c)-2):
        if i == 0:
            gval_sec = canigo([(startval[0], startval[1])], ar,neuer_fill, startval)
            print(gval_sec[2], gval_sec[0])
            start = gval_sec[1]
        else:
            gval_sec = canigo([(startval[0], startval[1])], ar,neuer_fill, startval)
            print(gval_sec[2], gval_sec[0])

        if gval_sec[0] == True:
            neuer_fill = gval_sec[2] + serie_c[i+1]
            print("ping", serie_c[i+1], neuer_fill)
        if gval_sec[0] == False:
            neuer_fill = gval_sec[2] - serie_c[i+1]
            print("pong", serie_c[i+1], neuer_fill)
    return(neuer_fill, "")
        
#%%        


    
#%%
    