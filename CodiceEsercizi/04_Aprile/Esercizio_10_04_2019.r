#Percentuale successo del chip
p <- 96.5
p <- p/100

#1. probabilità che sbagli la misurazione
q <- 1-p
q
q+p==1

#2. Probabilità che n siano esatte e la n+1 sia errata
n <- 23
Pr_n_esatte <- sum(dgeom(x=n,prob=q))
Pr_n_esatte
(q)*(p^n)
Pr_n_esatte == (q)*(p^n)

#3.Probabilità di ottenere un errore dopo ulteriori m simulazioni
m <- 23
Pr_m_esatte_dopo_n <- sum(dgeom(x=m-1,prob=q))
Pr_m_esatte_dopo_n

#1: 0.035 == 1 - 0.965
#2: 0.01542397 == 0.035 * (0.965)^23
#3: 0.01598339 == 0.035 * (0.965)^22