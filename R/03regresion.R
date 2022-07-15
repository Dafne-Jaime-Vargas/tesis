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

datos <- descriptivos %>% mutate(clases = case_when(`M6_or$predclass`%in% c(1,4, 3) ~ "clase 1, 3 y 4 PE",
                                                    `M6_or$predclass`==6 ~ "clase 6 PM",
                                                    `M6_or$predclass`==2 ~ "clase 2 PA",
                                                    `M6_or$predclass`==5 ~ "clase 5 PMA",
                                                    TRUE ~ NA_character_))

#estimar la clase de referencia
datos$clases <- relevel(datos$clases, ref = 'clase 1, 3 y 4 PE')
datos$educacion <- relevel(datos$educacion, ref = 'Postgrado')
datos$edad_tramos <- relevel(datos$edad_tramos, ref = 'Adulto')
datos$sexo <- relevel(datos$sexo, ref = 'Hombre')

# estimación del modelo logístico -----------------------------------------
#modelo nulo
multinom0<- multinom(clases ~ 1, data = datos, weights = factor_XS)

summary(multinom0)

multinom1<- multinom(clases ~ sexo, data = datos, weights = factor_XS)

summary(multinom1)
  
multinom2 <- multinom(clases ~ sexo + edad_tramos, data = datos, weights = factor_XS)

summary(multinom2)

multinom3 <- multinom(clases ~ sexo + edad_tramos + educacion, data = datos, weights = factor_XS)

summary(multinom3)

multinom4 <- multinom(clases ~ sexo + edad_tramos + educacion + sexo*edad_tramos, data = datos, weights = factor_XS)

summary(multinom4)

multinom5 <- multinom(clases ~ sexo + edad_tramos + educacion + sexo*educacion, data = datos, weights = factor_XS)

summary(multinom5)

multinom6 <- multinom(clases ~ sexo + edad_tramos + educacion + edad_tramos*educacion, data = datos, weights = factor_XS)

summary(multinom6)

multinom7 <- multinom(clases ~ sexo + edad_tramos + educacion + sexo*edad_tramos + sexo*educacion, data = datos, weights = factor_XS)

summary(multinom7)

multinom8 <- multinom(clases ~ sexo + edad_tramos + educacion + sexo*edad_tramos + sexo*educacion + edad_tramos*educacion, data = datos, weights = factor_XS)

summary(multinom8)

multinom9 <- multinom(clases ~ sexo + edad_tramos + educacion + sexo*edad_tramos + sexo*educacion + edad_tramos*educacion + sexo*edad_tramos*educacion, data = datos, weights = factor_XS)

summary(multinom9)

# ajuste de modelos -------------------------------------------------------
# ----- tabla de ajuste ----
library("DescTools")
R0 <- PseudoR2(multinom0, which = c("Nagelkerke"))
R1 <- PseudoR2(multinom1, which = c("Nagelkerke"))
R2 <- PseudoR2(multinom2, which = c("Nagelkerke"))
R3 <- PseudoR2(multinom3, which = c("Nagelkerke"))
R4 <- PseudoR2(multinom4, which = c("Nagelkerke"))
R5 <- PseudoR2(multinom5, which = c("Nagelkerke"))
R6 <- PseudoR2(multinom6, which = c("Nagelkerke"))
R7 <- PseudoR2(multinom7, which = c("Nagelkerke"))
R8 <- PseudoR2(multinom8, which = c("Nagelkerke"))
R9 <- PseudoR2(multinom9, which = c("Nagelkerke"))

ajuste_modelo_reg <-data.frame(c("Modelo nulo", "Modelo 1","Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6", "Modelo 7", "Modelo 8", "Modelo 9"),
                               c(multinom0$AIC, multinom1$AIC, multinom2$AIC, multinom3$AIC, multinom4$AIC, multinom5$AIC, multinom6$AIC, multinom7$AIC, multinom8$AIC, multinom9$AIC),
                               c(multinom0$value, multinom1$value, multinom2$value, multinom3$value, multinom4$value, multinom5$value, multinom6$value, multinom7$value, multinom8$value, multinom9$value),
                               c(round(r2_mcfadden(multinom0)[["R2"]], digits = 2), round(r2_mcfadden(multinom1)[["R2"]], digits = 2), round(r2_mcfadden(multinom2)[["R2"]], digits = 2), round(r2_mcfadden(multinom3)[["R2"]], digits = 2), round(r2_mcfadden(multinom4)[["R2"]], digits = 2), round(r2_mcfadden(multinom5)[["R2"]], digits = 2), round(r2_mcfadden(multinom6)[["R2"]], digits = 2), round(r2_mcfadden(multinom7)[["R2"]], digits = 2), round(r2_mcfadden(multinom8)[["R2"]], digits = 2), round(r2_mcfadden(multinom9)[["R2"]], digits = 2)),
                               c(round(r2_mcfadden(multinom0)[["R2_adjusted"]], digits = 2), round(r2_mcfadden(multinom1)[["R2_adjusted"]], digits = 2), round(r2_mcfadden(multinom2)[["R2_adjusted"]], digits = 2), round(r2_mcfadden(multinom3)[["R2_adjusted"]], digits = 2), round(r2_mcfadden(multinom4)[["R2_adjusted"]], digits = 2), round(r2_mcfadden(multinom5)[["R2_adjusted"]], digits = 2), round(r2_mcfadden(multinom6)[["R2_adjusted"]], digits = 2), round(r2_mcfadden(multinom7)[["R2_adjusted"]], digits = 2), round(r2_mcfadden(multinom8)[["R2_adjusted"]], digits = 2), round(r2_mcfadden(multinom9)[["R2_adjusted"]], digits = 2)),
                               c(round(R0, digits = 2), round(R1, digits = 2), round(R2, digits = 2), round(R3, digits = 2), round(R4, digits = 2), round(R5, digits = 2), round(R6, digits = 2), round(R7, digits = 2), round(R8, digits = 2), round(R9, digits = 2)))

colnames(ajuste_modelo_reg)<-c("Modelo", "AIC", "Loglikelihood", "R2 Mcfadden", "R2 Mcfadden ajustado", "R2 de Nagelkerke" )

writexl::write_xlsx(ajuste_modelo_reg, "output/tablas/03_ajuste_regresion.xlsx")

# visualización del modelo ------------------------------------------------

tab_model(multinom3,
          string.intercept = "(Intercepto)",
          dv.labels = c("Modelo"),
          string.pred = "Predictores",
          p.style = "stars",
          transform = "exp",
          show.ci = .95,
          show.intercept = T,
          string.ci = "IC 95%",
          show.aic = T, #mostrar aic
          show.loglik = T, #mostrar likelihood
          show.reflvl = F,
          digits = 2,
          encoding = "latin9",
          file = "output/regresion1.doc",
          collapse.ci = F) #deja los ci en una fila aparte


# graficar ----------------------------------------------------------------

sjPlot::plot_model(multinom8, 
                   show.p = T,
                   show.values =  T,
                   ci.lvl = 0.95, 
                   show.intercept = T,
                   title = "Estimación de predictores",
                   vline.color = "green")

# guardar -----------------------------------------------------------------

save(list = ls(), file = "output/regresion.RData")


