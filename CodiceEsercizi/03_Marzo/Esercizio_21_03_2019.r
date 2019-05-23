library(MASS);
#dati
dadiA <- 5;
dadiB <- 2;
#gioco fermo
giocofermo <- 5;
faccedado <- 6;
#calcoli
psingolafaccia <- 1/faccedado;
pNon5 <- 1 - psingolafaccia;

#1 | Gioco si ferma a causa di A e non B
pANonVinca <- pNon5 ^ dadiA;
pVincaA <- 1 - pANonVinca;
fractions(pVincaA);

pBNonVinca <- pNon5 ^ dadiB;
pVincaB <- 1 - pBNonVinca;
fractions(pVincaB);

#calcoliamo ora i casi presi uno ad uno
pVincaSoloA <- pVincaA * pBNonVinca;
fractions(pVincaSoloA);
pVincaSoloB <- pVincaB * pANonVinca;
fractions(pVincaSoloB);
pVincaBeA <- pVincaA * pVincaB;
fractions(pVincaBeA);
pVincaNessuno <- pBNonVinca * pANonVinca;
fractions(pVincaNessuno);

#condizioniamo ora con il fatto che l'esperimento sia terminato
espTerminato <- 1 - pVincaNessuno;
pVincaSoloATerminato <- pVincaSoloA/espTerminato;
fractions(pVincaSoloATerminato);
pVincaSoloBTerminato <- pVincaSoloB/espTerminato;
fractions(pVincaSoloBTerminato);
pVincaAeBTerminato <- pVincaBeA/espTerminato;
fractions(pVincaAeBTerminato);