library(MASS);

#dati
dado <- c(1, 2, 3, 4);
#biglie
rosse <- 13;
nere <- 12;
capienza_sacchetto <- nere + rosse;

# calcoli intermedi
pescare_rossa <- rosse/capienza_sacchetto;
pescare_nera <- nere/capienza_sacchetto;
capita_un_numero <- 1/length(dado);

#1 evento Pr(2, R, N)
prob1 <- capita_un_numero * pescare_rossa * pescare_nera;
fractions(prob1);

#2 evento Pr(3, R, N, R)
prob2 <- capita_un_numero * pescare_rossa * pescare_nera * pescare_rossa;
fractions(prob2);

#3 evento almeno 3 R
p_3_r_r_r <- capita_un_numero * pescare_rossa * pescare_rossa * pescare_rossa;
p_4_r_r_r_r <- capita_un_numero * pescare_rossa * pescare_rossa * pescare_rossa * pescare_rossa;
p_tutte_4_unaN_3R <- 4 * capita_un_numero * pescare_rossa * pescare_rossa * pescare_rossa * pescare_rossa;
prob3 <- p_3_r_r_r + p_4_r_r_r_r + p_tutte_4_unaN_3R;
fractions(prob3);