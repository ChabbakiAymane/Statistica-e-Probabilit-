library(MASS)

PrLetteraEsatta <- 48/100
PrLetteraErrata1 <- (1 - PrLetteraEsatta)/2
PrLetteraErrata2 <- PrLetteraErrata1

Pr_AAAA <- 28/100
Pr_BBBB <- 33/100
Pr_CCCC <- 39/100

Pr_CCBC_dato_AAAA <- PrLetteraErrata1^4
#Pr_CCBC_dato_AAAA * Pr_AAAA

Pr_CCBC_dato_BBBB <- PrLetteraEsatta * PrLetteraErrata1^3
#Pr_CCBC_dato_BBBB * Pr_BBBB

Pr_CCBC_dato_CCCC <- PrLetteraEsatta^3 * PrLetteraErrata1
#Pr_CCBC_dato_CCCC * Pr_CCCC

#Probabilità di AAAA dato il fatto che è uscito CCBC
Pr_CCBC <- (Pr_AAAA * Pr_CCBC_dato_AAAA) + (Pr_BBBB * Pr_CCBC_dato_BBBB) + (Pr_CCCC * Pr_CCBC_dato_CCCC)

Pr_AAAA_inter_CCBC <- Pr_AAAA * Pr_CCBC_dato_AAAA

Pr_AAAA_dato_CCBC <- Pr_AAAA_inter_CCBC/Pr_CCBC
fractions(Pr_AAAA_dato_CCBC)
Pr_AAAA_dato_CCBC

#Calcolo le disposizioni con ripetizioni di n elementi in k posti
n <- 3
k <- 3

disposizioni_con_ripetizione <- n^k
disposizioni_con_ripetizione