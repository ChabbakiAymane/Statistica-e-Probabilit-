library(MASS);
#Se non installata
#install.packages("combinat");
require(combinat);
#dati
matematici <- 21;
fisici <- 12;
#comitati
comitati <- 8;

#calcolo
gruppo <- matematici + fisici;
casiTotali <- dim(combn(gruppo, comitati))[2];

#1: Comitati formati da 5 matematici e 3 fisici
nMatematici <- 5;
nFisici <- 3;
casiMatematici <- dim(combn(matematici, nMatematici))[2];
casiFisici <- dim(combn(fisici, nFisici))[2];
casiFavorevoli <- casiMatematici * casiFisici;
casiFavorevoli;

#2: Probabilità di avere comitati formati da 5 matematici e 3 fisici
Pr_comitati <- casiFavorevoli/casiTotali;
fractions(Pr_comitati);

#3: Comitati formati da almeno 1 fisico e matematici > fisici

#Comitati possibili:
#1:	7 matematici + 1 fisico
	casoMat1 <- dim(combn(matematici, 7))[2];
	casoFis1 <- dim(combn(fisici, 1))[2];
	caso1 <- casoMat1 * casoFis1;
	#caso1 <- dim(combn(matematici, 7))[2] * dim(combn(fisici, 1))[2];
	
#2:	6 matematici + 2 fisici
	casoMat2 <- dim(combn(matematici, 6))[2];
	casoFis2 <- dim(combn(fisici, 2))[2];
	caso2 <- casoMat2 * casoFis2;
	#caso2 <- dim(combn(matematici, 6))[2] * dim(combn(fisici, 2))[2];
	
#3:	5 matematici + 3 fisici
	casoMat3 <- dim(combn(matematici, 5))[2];
	casoFis3 <- dim(combn(fisici, 3))[2];
	caso3 <- casoMat3 * casoFis3;
	#caso3 <- dim(combn(matematici, 5))[2] * dim(combn(fisici, 3))[2];
	
comitatiTot <- (caso1 + caso2 + caso3);
comitatiTot;

#Fare controllo per vedere se la somma di tutti i comitati da 8 persone è uguale a casiTotali
#caso4 <- (dim(combn(matematici, 4))[2]) * (dim(combn(fisici, 4))[2]);
#caso5 <- (dim(combn(matematici, 3))[2]) * (dim(combn(fisici, 5))[2]);
#caso6 <- (dim(combn(matematici, 2))[2]) * (dim(combn(fisici, 6))[2]);
#caso7 <- (dim(combn(matematici, 1))[2]) * (dim(combn(fisici, 7))[2]);
#caso8 <- (dim(combn(matematici, 0))[2]) * (dim(combn(fisici, 8))[2]);
#caso9 <- (dim(combn(matematici, 8))[2]) * (dim(combn(fisici, 1))[2]);
#sommaCasi <- (comitatiTot + caso4 + caso5 + caso6 + caso7 + caso8 + caso9);
#Somma Casi
#sommaCasi;
#Casi Totali
#casiTotali;	