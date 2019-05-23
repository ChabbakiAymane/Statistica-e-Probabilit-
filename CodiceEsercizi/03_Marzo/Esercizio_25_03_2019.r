library(MASS);
#dati
pI <- 86/100;
pNI <- 1 - pI;

# Sebbene questo test fornisca una certezza assoluta sull'assenza del parassita, è possibile che a volte individui dei falsi positivi al test
pPositivoInf <- 100/100;
pPositivoNinf <- 31/100;

# calcolo 
pNegativoInf <- 1 - pPositivoInf; 
pNegativoNif <- 1 - pPositivoNinf;

#1 la probabilità che il test dia risposta positiva:
pPositivo <- pPositivoInf * pI + pPositivoNinf * pNI;
pPositivo;
fractions(pPositivo);

#2 la probabilità che il prodotto sia effettivamente infettato dal parassita quando otteniamo una risposta positiva del test:
pInfettoPositivo <- (pPositivoInf * pI)/pPositivo;
pInfettoPositivo;
fractions(pInfettoPositivo);