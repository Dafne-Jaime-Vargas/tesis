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

datos <- descriptivos %>% mutate(clases = case_when(`M6_or$predclass`%in% c(1,4) ~ "clase de precariedad escaza (1 y 4)",
                                                    `M6_or$predclass`==3 ~ "clase 3 de precariedad baja",
                                                    `M6_or$predclass`==6 ~ "clase 6 media",
                                                    `M6_or$predclass`==2 ~ "clase 5 alta",
                                                    `M6_or$predclass`==5 ~ "clase 5 muy alta",
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


p1<- 1-pchisq(multinom1$Chisq, multinom1$resid.df)
p2<- 1-pchisq(M2_or$Chisq, M2_or$resid.df)
p3<- 1-pchisq(M3_or$Chisq, M3_or$resid.df)
p4<- 1-pchisq(M4_or$Chisq, M4_or$resid.df)
p5<- 1-pchisq(M5_or$Chisq, M5_or$resid.df)
p6<- 1-pchisq(M6_or$Chisq, M6_or$resid.df)
p7<- 1-pchisq(M7_or$Chisq, M7_or$resid.df)
p8<- 1-pchisq(M8_or$Chisq, M8_or$resid.df)
p9<- 1-pchisq(M9_or$Chisq, M9_or$resid.df)
p10<- 1-pchisq(M10_or$Chisq, M10_or$resid.df)

# ----- tabla de ajuste ----
aic1 <- multinom1$AIC
aic2 <- multinom2$AIC
aic3 <- multinom3$AIC

log1 <- multinom1$value
log2 <- multinom2$value
log3 <- multinom3$value

p1 <- r2_mcfadden(multinom1)[1]
p2 <- r2_mcfadden(multinom2)[1]
p3 <- r2_mcfadden(multinom3)[1]

v1 <- r2_mcfadden(multinom1)[2]
v2 <- r2_mcfadden(multinom2)[2]
v3 <- r2_mcfadden(multinom3)[2]


ajuste_modelo_reg <-data.frame(c("Modelo 1","Modelo 2", "Modelo 3"),
                              c(aic1, aic2, aic3),
                              c(log1, log2, log3),
                              c(p1[["R2"]], p2[["R2"]], p3[["R2"]]),
                              c(v1[["R2_adjusted"]], v2[["R2_adjusted"]], v3[["R2_adjusted"]]))


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
          encoding = "UTF-8",
          collapse.ci = T) #deja los ci en una fila aparte





