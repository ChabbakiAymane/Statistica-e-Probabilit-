library(MASS);

#bandiera 
strisce <- 3;
#Attenzione, nella striscia centrale deve esserci un colore diverso da quello delle altre 3
colori <- 5;

#1 Quante possibili bandiere si possono formare come descritto prima?

#è possibile usare tutti i colori a disposizione per il primo posto. per
#il secondo posto invece bisognerà usare n-1 (quello usato in prima #posizione).
#per il terzo posto si portranno usare nuovamente n-1 colori (non posso
#utilizzare quello in seconda posizione ma quello in prima sì)

bandiereTot <- colori*(colori-1)*(colori-1)
bandiereTot

#2 Si considerino le bandiere appena create. Qual è la probabilità di avere una bandiera con solo due colori?

#per calcolare le bandiere con solo 2 colori possiamo usare lo stesso
#procedimento di prima: posso scegliere n colori al primo posto, n-1 colori
#al secondo posto ma devo per forza di cose scegliere il colore che si 
#trova in prima posizione per avere 2 colori e quindi avrò 1 solo colore

bandiere2Colori <- colori*(colori-1)
fractions(bandiere2Colori/bandiereTot)
