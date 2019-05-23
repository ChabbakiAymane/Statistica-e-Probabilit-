library(MASS);

#dati
#Urna A
BinA <- 15;
NinA <- 10;
#Urna B
BinB <- 19;
NinB <- 25;

#totali
totaleA <- BinA + NinA;
totaleB <- BinB + NinB;

#probabilità
pBinA <- BinA/totaleA;
pNinA <- NinA/totaleA;

pBinB <- BinB/totaleB;
pNinB <- NinB/totaleB;

#sposto una pallina da A a B
totaleBDopoSpostamento <- totaleB + 1;

#sposto una pallina nera da A a B
BinBSeNera <- BinB;
pBinBSeNera <- BinBSeNera/totaleBDopoSpostamento;

#sposto una pallina bianca da A a B
BinBSeBianca <- BinB + 1;
pBinBSeBianca <- BinBSeBianca/totaleBDopoSpostamento;

#1 Probabilità seconda pallina estratta sia bianca?
quesito1 <- pNinA * pBinBSeNera + pBinA * pBinBSeBianca;
fractions(quesito1);

#2 Sapendo che la seconda pallina estratta è bianca, qual è la probabilità che fosse bianca anche la prima pallina estratta
#P(B1 | B2) = (P(B2 | B1) * P(B1))/(P(B2))
pB2datoB1 <- pBinBSeBianca; #bianca se passata bianca
pB1 <- pBinA; #pescarla bianca
pB2 <- quesito1;

quesito2 <- (pB2datoB1 * pB1)/pB2;
fractions(quesito2);