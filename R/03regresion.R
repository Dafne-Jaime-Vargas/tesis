# Script 05: Análisis de regresión --------------------------------------------------------
## Autora: Dafne Jaime Vargas ----------------------------------------------
## Seminario de título II ---------------------------------------------------------------
rm(list = ls()) #Borrar todos los objetos del espacio de trabajo
# cargar paquetes ---------------------------------------------------------

pacman::p_load(tidyverse,
               sjPlot,
               performance,
               nnet)

# cargar datos  -------------------------------------------------------------------------
load("output/tablas/descriptivos/03-2_datos_analisis.RData")


# procesar los datos ------------------------------------------------------

datos <- descriptivos %>% mutate(clases = case_when(`M6_or$predclass`%in% c(1,4) ~ "clase 1 y 4 PE",
                                                    `M6_or$predclass`==3 ~ "clase 3 PB",
                                                    `M6_or$predclass`==6 ~ "clase 6 PM",
                                                    `M6_or$predclass`==2 ~ "clase 5 PA",
                                                    `M6_or$predclass`==5 ~ "clase 5 PMA",
                                                    TRUE ~ NA_character_))

#estimar la clase de referencia
#datos$clases <- relevel(datos$clases, ref = 'clase de precariedad escaza (1 y 4)')


# estimación del modelo logístico -----------------------------------------
#modelo nulo

multinom1<- multinom(clases ~ sexo, data = datos, weights = factor_XS)

summary(multinom1)
  
multinom2 <- multinom(clases ~ sexo + edad_tramos, data = datos, weights = factor_XS)

summary(multinom2)

multinom3 <- multinom(clases ~ sexo + edad_tramos + educacion, data = datos, weights = factor_XS)

summary(multinom3)


# ajuste de modelos -------------------------------------------------------
# ----- tabla de ajuste ----
aic1 <- multinom1$AIC
aic2 <- multinom2$AIC
aic3 <- multinom3$AIC
aic4 <- multinom4$AIC

log1 <- multinom1$value
log2 <- multinom2$value
log3 <- multinom3$value
log4 <- multinom4$value

p1 <- r2_mcfadden(multinom1)[1]
p2 <- r2_mcfadden(multinom2)[1]
p3 <- r2_mcfadden(multinom3)[1]
p4 <- r2_mcfadden(multinom4)[1]

v1 <- r2_mcfadden(multinom1)[2]
v2 <- r2_mcfadden(multinom2)[2]
v3 <- r2_mcfadden(multinom3)[2]
v4 <- r2_mcfadden(multinom4)[2]

ajuste_modelo_reg <-data.frame(c("Modelo 1","Modelo 2", "Modelo 3", "Modelo 4"),
                              c(aic1, aic2, aic3, aic4),
                              c(log1, log2, log3, log4),
                              c(p1[["R2"]], p2[["R2"]], p3[["R2"]], p4[["R2"]]),
                              c(v1[["R2_adjusted"]], v2[["R2_adjusted"]], v3[["R2_adjusted"]], v4[["R2_adjusted"]]))


colnames(ajuste_modelo_reg)<-c("Modelo", "AIC", "Loglikelihood", "R2 Mcfadden", "R2 Mcfadden ajustado" )

writexl::write_xlsx(ajuste_modelo_reg, "output/tablas/03_ajuste_regresion.xlsx")

# visualización del modelo ------------------------------------------------

tab_model(multinom3,
          string.intercept = "(Intercepto)",
          dv.labels = c("Modelo"),
          string.pred = "Predictores",
          p.style = "stars",
          transform = "exp",
          show.aic = T, #mostrar aic
          show.loglik = T, #mostrar likelihood
          show.reflvl = T,
          digits = 2,
          encoding = "latin9",
         # file = "output/regresion.doc",
          collapse.ci = F) #deja los ci en una fila aparte


# procesar los datos ------------------------------------------------------

datos <- descriptivos %>% mutate(clases2 = case_when(`M6_or$predclass`%in% c(1,3,4) ~ "clase 1, 3 y 4 PE",
                                                     `M6_or$predclass`%in% c(2,6) ~ "clase 2 y 6 PM",
                                                     `M6_or$predclass`==5 ~ "clase 5 PMA",
                                                     TRUE ~ NA_character_))

#estimar la clase de referencia
#datos$clases <- relevel(datos$clases, ref = 'clase de precariedad escaza (1 y 4)')


# estimación del modelo logístico -----------------------------------------
#modelo nulo

multinom_1<- multinom(clases2 ~ sexo, data = datos, weights = factor_XS)

summary(multinom_1)

multinom_2 <- multinom(clases2 ~ sexo + edad_tramos, data = datos, weights = factor_XS)

summary(multinom_2)

multinom_3 <- multinom(clases2 ~ sexo + edad_tramos + educacion, data = datos, weights = factor_XS)

summary(multinom_3)

# ajuste de modelos -------------------------------------------------------
# ----- tabla de ajuste ----
aic1 <- multinom_1$AIC
aic2 <- multinom_2$AIC
aic3 <- multinom_3$AIC

log1 <- multinom_1$value
log2 <- multinom_2$value
log3 <- multinom_3$value


p1 <- r2_mcfadden(multinom_1)[1]
p2 <- r2_mcfadden(multinom_2)[1]
p3 <- r2_mcfadden(multinom_3)[1]


v1 <- r2_mcfadden(multinom_1)[2]
v2 <- r2_mcfadden(multinom_2)[2]
v3 <- r2_mcfadden(multinom_3)[2]


ajuste_modelo_reg2 <-data.frame(c("Modelo 1","Modelo 2", "Modelo 3"),
                               c(aic1, aic2, aic3),
                               c(log1, log2, log3),
                               c(p1[["R2"]], p2[["R2"]], p3[["R2"]]),
                               c(v1[["R2_adjusted"]], v2[["R2_adjusted"]], v3[["R2_adjusted"]]))


colnames(ajuste_modelo_reg2)<-c("Modelo", "AIC", "Loglikelihood", "R2 Mcfadden", "R2 Mcfadden ajustado" )

writexl::write_xlsx(ajuste_modelo_reg2, "output/tablas/03_ajuste_regresion.xlsx")

# visualización del modelo ------------------------------------------------

tab_model(multinom_3,
          string.intercept = "(Intercepto)",
          dv.labels = c("Modelo"),
          string.pred = "Predictores",
          p.style = "stars",
          transform = "exp",
          show.aic = T, #mostrar aic
          show.loglik = T, #mostrar likelihood
          show.reflvl = T,
          digits = 2,
          encoding = "latin9",
          file = "output/regresion.doc",
          collapse.ci = F) #deja los ci en una fila aparte




