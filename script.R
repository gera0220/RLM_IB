library(tidyverse)
library(readr)
library(leaps)
datos <- read_csv("datos.csv") %>% select(-N)
modelo1 <- lm(GLOBAL ~ ., datos)
summary(modelo1)
par(mfrow = c(2, 2))
plot(modelo1)
bs <- regsubsets(GLOBAL ~., datos, nbest = 2)
bs_summary <- summary(bs)
cbind(
  Cp = bs_summary$cp,
  R2 = bs_summary$rsq,
  adj_R2 = bs_summary$adjr2
)
modelo2 <- lm(GLOBAL ~ PMA + PAN + ELE + CLE + LES + ING, datos)
summary(modelo2)
plot(modelo2)
