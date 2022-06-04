# Script 02: procesamiento --------------------------------------------------------
## Autora: Dafne Jaime Vargas ----------------------------------------------
## Seminario de título II ---------------------------------------------------------------
rm(list = ls()) 
(scipen=999) #Desactivar notacion cientifica
# 1.  --------------------- cargar base y paquetes  ---------------------

pacman::p_load(dplyr,
               sjmisc,
               poLCA, 
               reshape2,
               tidyr,
               beepr, 
               writexl,
               knitr)


load("output/data/02-datos_proc.RData")

# ----  Modelos de Clase Latente ---- 

#Modelo Nulo en que la variabilidad no es explicada por una variable de clase latente

M1_or <- poLCA(f1, datos_sna, nclass = 1, maxiter = 2000, nrep = 2, na.rm = T)

set.seed(12345)

# Modelos de 2 o más clases latentes. --------------

# Se espera que cada clase latente de la variable latente sea homogénea internamente
# Y sea heterogénea en relación con las otras clases

# M2

M2_or <-poLCA(f1, datos_sna, nclass = 2, maxiter = 2000, nrep = 3, na.rm = T)

#M3
M3_or<-poLCA(f1, datos_sna, nclass = 3, maxiter = 7000, nrep = 5, na.rm = T)

#M4
M4_or<-poLCA(f1, datos_sna, nclass = 4, maxiter = 7000, nrep = 5, na.rm = T)

#M5
M5_or<-poLCA(f1, datos_sna, nclass = 5, maxiter = 45000, nrep = 5, na.rm = T) #4nrep

#M6
M6_or<-poLCA(f1, datos_sna, nclass = 6, maxiter = 45000, nrep = 7, na.rm = T)#4nrep

#M7
M7_or<-poLCA(f1, datos_sna, nclass = 7, maxiter = 45000, nrep = 7, na.rm = T) #4nrep

#M8
M8_or<-poLCA(f1, datos_sna, nclass = 8, maxiter = 450000, nrep = 7, na.rm = T) #4nreo

#M9
M9_or<-poLCA(f1, datos_sna, nclass = 9, maxiter = 450000, nrep = 7, na.rm = T) #4nrep

#M10
M10_or<-poLCA(f1, datos_sna, nclass = 10, maxiter = 450000, nrep = 7, na.rm = T) #4nrep


# guardado ----------------------------------------------------------------

# para análisis -----------------------------------------------------------

save(datos_exp, datos_sna, M1_or, M2_or, M3_or, M4_or, M5_or, M6_or, M7_or, M8_or, M9_or, M10_or, file = "output/data/03-modelos_analisis.RData")

#Calcular el p valor del chi cuadrado para 11 

p1<- 1-pchisq(M1_or$Chisq, M1_or$resid.df)
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

ajuste_modelo_or <-data.frame(c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10"),
                               c(M1_or$llik, M2_or$llik, M3_or$llik, M4_or$llik, M5_or$llik, M6_or$llik, M7_or$llik, M8_or$llik, M9_or$llik, M10_or$llik),
                               c(M1_or$Chisq, M2_or$Chisq, M3_or$Chisq, M4_or$Chisq, M5_or$Chisq, M6_or$Chisq, M7_or$Chisq, M8_or$Chisq, M9_or$Chisq, M10_or$Chisq),
                               c(M1_or$Gsq, M2_or$Gsq, M3_or$Gsq, M4_or$Gsq, M5_or$Gsq, M6_or$Gsq,  M7_or$Gsq, M8_or$Gsq, M9_or$Gsq, M10_or$Gsq),
                               c(M1_or$npar, M2_or$npar, M3_or$npar, M4_or$npar, M5_or$npar, M6_or$npar,  M7_or$npar, M8_or$npar, M9_or$npar, M10_or$npar),
                               c(M1_or$aic, M2_or$aic, M3_or$aic, M4_or$aic, M5_or$aic, M6_or$aic, M7_or$aic, M8_or$aic, M9_or$aic, M10_or$aic),
                               c(M1_or$bic, M2_or$bic, M3_or$bic, M4_or$bic, M5_or$bic,  M6_or$bic,  M7_or$bic, M8_or$bic, M9_or$bic, M10_or$bic),
                               c(M1_or$Nobs, M2_or$Nobs, M3_or$Nobs, M4_or$Nobs, M5_or$Nobs,  M6_or$Nobs,  M7_or$Nobs, M8_or$Nobs, M9_or$Nobs, M10_or$Nobs),
                               c(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10))


colnames(ajuste_modelo_or)<-c("Modelo", "Loglike", "X2", "G2", "DF", "AIC", "BIC", "N", "P-value")

writexl::write_xlsx(ajuste_modelo_or, "output/tablas/03_ajuste_modelo_final.xlsx")

# modelos escogidos -----------------------------------------------------------------

#M3

#composición clases
prob3 = reshape2::melt(M3_or$P, level=2) 

prob3=prob3 %>% 
  mutate(por = 1:3) %>% 
  pivot_wider(names_from = "por",
              values_from = "value") %>% 
  mutate(`clase1` = (round(.$`1`, digits = 3))*100,
         `clase2` = (round(.$`2`, digits = 3))*100, 
         `clase3` = (round(.$`3`, digits = 3))*100) %>% 
  dplyr::select(starts_with("clase"))



# clases

lmod3 <- reshape2::melt(M3_or$probs, level=2) 

modelo3 <- lmod3 %>% 
  pivot_wider(names_from = "Var1",
              values_from = "value") %>% 
  dplyr::select(Variable = L2, Categoria = Var2, clase1 = "class 1: ", clase2 = "class 2: ", clase3 ="class 3: ") %>% 
    mutate(Variable = car::recode(.$Variable, recodes = c("'rel_cont' = 'Relacion contractual';
                                                         'ingresos' = 'Ingresos';
                                                         'seg_accid' = 'Seguro de accidentes';
                                                         'coti_previ' = 'Cotizacion prevision';
                                                         'previ_salud' = 'Cotizacion de salud';
                                                         'afil_sindicato' = 'Afiliacion sindicato';
                                                         'hor_sem' = 'Horas semanales';
                                                         'cant_empleos' = 'Cantidad de empleos';
                                                         'cesantia' = 'indemnizacion de cesantia';
                                                         'enf_lab' = 'Enfermedad laboral'"), as.factor = T),
         clase1 = (round(.$clase1, digits = 3))*100,
         clase2 = (round(.$clase2, digits = 3))*100, 
         clase3 = (round(.$clase3, digits = 3))*100) 

M3 <- bind_rows(modelo3, prob3)



#M4---------------------------
#composición clases
prob4 <- reshape2::melt(M4_or$P, level=2) 

prob4 <- prob4 %>% 
  mutate(por = 1:4) %>% 
  pivot_wider(names_from = "por",
              values_from = "value") %>% 
  mutate(`clase1` = (round(.$`1`, digits = 3))*100,
         `clase2` = (round(.$`2`, digits = 3))*100, 
         `clase3` = (round(.$`3`, digits = 3))*100, 
         `clase4` = (round(.$`4`, digits = 3))*100)%>% 
  dplyr::select(starts_with("clase"))


lmod4 <- reshape2::melt(M4_or$probs, level=2)

modelo4 <- lmod4 %>% 
  pivot_wider(names_from = "Var1",
              values_from = "value") %>% 
  dplyr::select(Variable = L2, Categoria = Var2, clase1 = "class 1: ", 
         clase2 = "class 2: ", clase3 = "class 3: ", clase4 = "class 4: ") %>% 
  mutate(Variable = car::recode(.$Variable, recodes = c("'rel_cont' = 'Relacion contractual';
                                                         'ingresos' = 'Ingresos';
                                                         'seg_accid' = 'Seguro de accidentes';
                                                         'coti_previ' = 'Cotizacion prevision';
                                                         'previ_salud' = 'Cotizacion de salud';
                                                         'afil_sindicato' = 'Afiliacion sindicato';
                                                         'hor_sem' = 'Horas semanales';
                                                         'cant_empleos' = 'Cantidad de empleos';
                                                         'cesantia' = 'indemnizacion de cesantia';
                                                         'enf_lab' = 'Enfermedad laboral'"), as.factor = T),
         clase1 = (round(.$clase1, digits = 3))*100,
         clase2 = (round(.$clase2, digits = 3))*100,  
         clase3 = (round(.$clase3, digits = 3))*100,
         clase4 = (round(.$clase4, digits = 3))*100) 

M4 <-bind_rows( modelo4, prob4)

#M5 -------------------

prob5 <- reshape2::melt(M5_or$P, level=2) 

prob5 <- prob5 %>% 
  mutate(por = 1:5) %>% 
  pivot_wider(names_from = "por",
              values_from = "value") %>% 
  mutate(`clase1` = (round(.$`1`, digits = 3))*100,
         `clase2` = (round(.$`2`, digits = 3))*100, 
         `clase3` = (round(.$`3`, digits = 3))*100, 
         `clase4` = (round(.$`4`, digits = 3))*100, 
         `clase5` = (round(.$`5`, digits = 3))*100)%>% 
  dplyr::select(starts_with("clase"))

lmod5 <- reshape2::melt(M5_or$probs, level=2)

modelo5 <- lmod5 %>% 
  pivot_wider(names_from = "Var1",
              values_from = "value") %>% 
  dplyr::select(Variable = L2, Categoria = Var2, clase1 = "class 1: ", clase2 = "class 2: ", 
         clase3 = "class 3: ", clase4 = "class 4: ", clase5 = "class 5: ") %>% 
  mutate(Variable = car::recode(.$Variable, recodes = c("'rel_cont' = 'Relacion contractual';
                                                         'ingresos' = 'Ingresos';
                                                         'seg_accid' = 'Seguro de accidentes';
                                                         'coti_previ' = 'Cotizacion prevision';
                                                         'previ_salud' = 'Cotizacion de salud';
                                                         'afil_sindicato' = 'Afiliacion sindicato';
                                                         'hor_sem' = 'Horas semanales';
                                                         'cant_empleos' = 'Cantidad de empleos';
                                                         'cesantia' = 'indemnizacion de cesantia';
                                                         'enf_lab' = 'Enfermedad laboral'"), as.factor = T),
         clase1 = (round(.$clase1, digits = 3))*100,
         clase2 = (round(.$clase2, digits = 3))*100,
         clase3 = (round(.$clase3, digits = 3))*100,  
         clase4 = (round(.$clase4, digits = 3))*100,
         clase5 = (round(.$clase5, digits = 3))*100)

M5 <-bind_rows( modelo5,prob5)

#M6 --------------------------------

prob6 <- reshape2::melt(M6_or$P, level=2) 

prob6 <- prob6 %>% 
  mutate(por = 1:6) %>% 
  pivot_wider(names_from = "por",
              values_from = "value") %>% 
  mutate(`clase1` = (round(.$`1`, digits = 3))*100,
         `clase2` = (round(.$`2`, digits = 3))*100, 
         `clase3` = (round(.$`3`, digits = 3))*100, 
         `clase4` = (round(.$`4`, digits = 3))*100, 
         `clase5` = (round(.$`5`, digits = 3))*100, 
         `clase6` = (round(.$`6`, digits = 3))*100)%>% 
  dplyr::select(starts_with("clase"))

lmod6 <- reshape2::melt(M6_or$probs, level=2)

modelo6 <- lmod6 %>% 
  pivot_wider(names_from = "Var1",
              values_from = "value") %>% 
  dplyr::select(Variable = L2, Categoria = Var2, clase1 = "class 1: ", clase2 = "class 2: ",
         clase3 = "class 3: ", clase4 = "class 4: ", clase5 = "class 5: ", clase6 = "class 6: ") %>% 
  mutate(Variable = car::recode(.$Variable, recodes = c("'rel_cont' = 'Relacion contractual';
                                                         'ingresos' = 'Ingresos';
                                                         'seg_accid' = 'Seguro de accidentes';
                                                         'coti_previ' = 'Cotizacion prevision';
                                                         'previ_salud' = 'Cotizacion de salud';
                                                         'afil_sindicato' = 'Afiliacion sindicato';
                                                         'hor_sem' = 'Horas semanales';
                                                         'cant_empleos' = 'Cantidad de empleos';
                                                         'cesantia' = 'indemnizacion de cesantia';
                                                         'enf_lab' = 'Enfermedad laboral'"), as.factor = T),
         clase1= (round(.$clase1, digits = 3))*100,
         clase2 = (round(.$clase2, digits = 3))*100,
         clase3 = (round(.$clase3, digits = 3))*100,  
         clase4= (round(.$clase4, digits = 3))*100,
         clase5 = (round(.$clase5, digits = 3))*100,
         clase6 = (round(.$clase6, digits = 3))*100)

M6 <- bind_rows( modelo6,prob6)


#M7 -----------------------
prob7 <- reshape2::melt(M7_or$P, level=2) 

prob7 <- prob7 %>% 
  mutate(por = 1:7) %>% 
  pivot_wider(names_from = "por",
              values_from = "value") %>% 
  mutate(`clase1` = (round(.$`1`, digits = 3))*100,
         `clase2` = (round(.$`2`, digits = 3))*100, 
         `clase3` = (round(.$`3`, digits = 3))*100, 
         `clase4` = (round(.$`4`, digits = 3))*100, 
         `clase5` = (round(.$`5`, digits = 3))*100, 
         `clase6` = (round(.$`6`, digits = 3))*100, 
         `clase7` = (round(.$`7`, digits = 3))*100)%>% 
  dplyr::select(starts_with("clase"))

lmod7 <- reshape2::melt(M7_or$probs, level=2)

modelo7 <- lmod7 %>% 
  pivot_wider(names_from = "Var1",
              values_from = "value") %>% 
  dplyr::select(Variable = L2, Categoria = Var2, clase1 = "class 1: ", clase2 = "class 2: ", 
         clase3 = "class 3: ", clase4 = "class 4: ", clase5 = "class 5: ", clase6 = "class 6: ", 
         clase7 = "class 7: ") %>% 
  mutate(Variable = car::recode(.$Variable, recodes = c("'rel_cont' = 'Relacion contractual';
                                                         'ingresos' = 'Ingresos';
                                                         'seg_accid' = 'Seguro de accidentes';
                                                         'coti_previ' = 'Cotizacion prevision';
                                                         'previ_salud' = 'Cotizacion de salud';
                                                         'afil_sindicato' = 'Afiliacion sindicato';
                                                         'hor_sem' = 'Horas semanales';
                                                         'cant_empleos' = 'Cantidad de empleos';
                                                         'cesantia' = 'indemnizacion de cesantia';
                                                         'enf_lab' = 'Enfermedad laboral'"), as.factor = T),
         clase1 = (round(.$clase1, digits = 3))*100,
         clase2 = (round(.$clase2, digits = 3))*100,
         clase3 = (round(.$clase3, digits = 3))*100,  
         clase4 = (round(.$clase4, digits = 3))*100,
         clase5 = (round(.$clase5, digits = 3))*100,
         clase6 = (round(.$clase6, digits = 3))*100,
         clase7 = (round(.$clase7, digits = 3))*100)

M7 <-bind_rows( modelo7,prob7)

#M8 ----------------------------------
prob8 <- reshape2::melt(M8_or$P, level=2) 

prob8 <- prob8 %>% 
  mutate(por = 1:8) %>% 
  pivot_wider(names_from = "por",
              values_from = "value") %>% 
  mutate(`clase1` = (round(.$`1`, digits = 3))*100,
         `clase2` = (round(.$`2`, digits = 3))*100, 
         `clase3` = (round(.$`3`, digits = 3))*100, 
         `clase4` = (round(.$`4`, digits = 3))*100, 
         `clase5` = (round(.$`5`, digits = 3))*100, 
         `clase6` = (round(.$`6`, digits = 3))*100, 
         `clase7` = (round(.$`7`, digits = 3))*100, 
         `clase8` = (round(.$`8`, digits = 3))*100)


lmod8 <- reshape2::melt(M8_or$probs, level=2)

modelo8 <- lmod8 %>% 
  pivot_wider(names_from = "Var1",
              values_from = "value") %>% 
  dplyr::select(Variable = L2, Categoria = Var2, clase1 = "class 1: ", clase2 = "class 2: ", 
         clase3 = "class 3: ", clase4 = "class 4: ", clase5 = "class 5: ", clase6 = "class 6: ", 
         clase7 = "class 7: ", clase8 = "class 8: ") %>% 
  mutate(Variable = car::recode(.$Variable, recodes = c("'rel_cont' = 'Relacion contractual';
                                                         'ingresos' = 'Ingresos';
                                                         'seg_accid' = 'Seguro de accidentes';
                                                         'coti_previ' = 'Cotizacion prevision';
                                                         'previ_salud' = 'Cotizacion de salud';
                                                         'afil_sindicato' = 'Afiliacion sindicato';
                                                         'hor_sem' = 'Horas semanales';
                                                         'cant_empleos' = 'Cantidad de empleos';
                                                         'cesantia' = 'indemnizacion de cesantia';
                                                         'enf_lab' = 'Enfermedad laboral'"), as.factor = T),
         clase1 = (round(.$clase1, digits = 3))*100,
         clase2 = (round(.$clase2, digits = 3))*100,
         clase3 = (round(.$clase3, digits = 3))*100,  
         clase4 = (round(.$clase4, digits = 3))*100,
         clase5 = (round(.$clase5, digits = 3))*100,
         clase6 = (round(.$clase6, digits = 3))*100,
         clase7 = (round(.$clase7, digits = 3))*100,
         clase8 = (round(.$clase8, digits = 3))*100)

M8 <-bind_rows( modelo8,prob8)

#M9 ---------------------------------
prob9 <- reshape2::melt(M9_or$P, level=2) 

prob9 <- prob9 %>% 
  mutate(por = 1:9) %>% 
  pivot_wider(names_from = "por",
              values_from = "value") %>% 
  mutate(`clase1` = (round(.$`1`, digits = 3))*100,
         `clase2` = (round(.$`2`, digits = 3))*100, 
         `clase3` = (round(.$`3`, digits = 3))*100, 
         `clase4` = (round(.$`4`, digits = 3))*100, 
         `clase5` = (round(.$`5`, digits = 3))*100, 
         `clase6` = (round(.$`6`, digits = 3))*100, 
         `clase7` = (round(.$`7`, digits = 3))*100, 
         `clase8` = (round(.$`8`, digits = 3))*100, 
         `clase9` = (round(.$`9`, digits = 3))*100)%>% 
  dplyr::select(starts_with("clase"))

lmod9 <- reshape2::melt(M9_or$probs, level=2)

modelo9 <- lmod9 %>% 
  pivot_wider(names_from = "Var1",
              values_from = "value") %>% 
  dplyr::select(Variable = L2, Categoria = Var2, clase1 = "class 1: ", clase2 = "class 2: ", clase3 = "class 3: ", 
         clase4 = "class 4: ", clase5 = "class 5: ", clase6 = "class 6: ", 
         clase7 = "class 7: ", clase8 = "class 8: ", clase9 =  "class 9: ") %>% 
  mutate(Variable = car::recode(.$Variable, recodes = c("'rel_cont' = 'Relacion contractual';
                                                         'ingresos' = 'Ingresos';
                                                         'seg_accid' = 'Seguro de accidentes';
                                                         'coti_previ' = 'Cotizacion prevision';
                                                         'previ_salud' = 'Cotizacion de salud';
                                                         'afil_sindicato' = 'Afiliacion sindicato';
                                                         'hor_sem' = 'Horas semanales';
                                                         'cant_empleos' = 'Cantidad de empleos';
                                                         'cesantia' = 'indemnizacion de cesantia';
                                                         'enf_lab' = 'Enfermedad laboral'"), as.factor = T),
         clase1 = (round(.$clase1, digits = 3))*100,
         clase2 = (round(.$clase2, digits = 3))*100,
         clase3 = (round(.$clase3, digits = 3))*100,  
         clase4 = (round(.$clase4, digits = 3))*100,
         clase5= (round(.$clase5, digits = 3))*100,
         clase6 = (round(.$clase6, digits = 3))*100,
         clase7 = (round(.$clase7, digits = 3))*100,
         clase8 = (round(.$clase8, digits = 3))*100,
         clase9 = (round(.$clase9, digits = 3))*100) 


M9 <-bind_rows( modelo9, prob9)


#M10 ------------------------------
prob10 <- reshape2::melt(M10_or$P, level=2) 

prob10 <- prob10 %>% 
  mutate(por = 1:10) %>% 
  pivot_wider(names_from = "por",
              values_from = "value") %>% 
  mutate(`clase1` = (round(.$`1`, digits = 3))*100,
         `clase2` = (round(.$`2`, digits = 3))*100, 
         `clase3` = (round(.$`3`, digits = 3))*100, 
         `clase4` = (round(.$`4`, digits = 3))*100, 
         `clase5` = (round(.$`5`, digits = 3))*100, 
         `clase6` = (round(.$`6`, digits = 3))*100, 
         `clase7` = (round(.$`7`, digits = 3))*100, 
         `clase8` = (round(.$`8`, digits = 3))*100, 
         `clase9` = (round(.$`9`, digits = 3))*100, 
         `clase10` = (round(.$`10`, digits = 3))*100)%>% 
  dplyr::select(starts_with("clase"))


lmod10 <- reshape2::melt(M10_or$probs, level=2)

modelo10 <- lmod10 %>% 
  pivot_wider(names_from = "Var1",
              values_from = "value") %>% 
  dplyr::select(Variable = L2, Categoria = Var2, clase1 = "class 1: ", clase2 = "class 2: ", 
         clase3 = "class 3: ", clase4 = "class 4: ", clase5 = "class 5: ", clase6 = "class 6: ", 
         clase7 = "class 7: ", clase8 = "class 8: ", clase9 = "class 9: ", clase10 = "class 10: ") %>% 
  mutate(Variable = car::recode(.$Variable, recodes = c("'rel_cont' = 'Relacion contractual';
                                                         'ingresos' = 'Ingresos';
                                                         'seg_accid' = 'Seguro de accidentes';
                                                         'coti_previ' = 'Cotizacion prevision';
                                                         'previ_salud' = 'Cotizacion de salud';
                                                         'afil_sindicato' = 'Afiliacion sindicato';
                                                         'hor_sem' = 'Horas semanales';
                                                         'cant_empleos' = 'Cantidad de empleos';
                                                         'cesantia' = 'indemnizacion de cesantia';
                                                         'enf_lab' = 'Enfermedad laboral'"), as.factor = T),
         clase1 = (round(.$clase1, digits = 3))*100,
         clase2 = (round(.$clase2, digits = 3))*100,
         clase3 = (round(.$clase3, digits = 3))*100,  
         clase4 = (round(.$clase4, digits = 3))*100,
         clase5 = (round(.$clase5, digits = 3))*100,
         clase6 = (round(.$clase6, digits = 3))*100,
         clase7 = (round(.$clase7, digits = 3))*100,
         clase8 = (round(.$clase8, digits = 3))*100,
         clase9 = (round(.$clase9, digits = 3))*100,
         clase10 = (round(.$clase10, digits = 3))*100)

M10 <-bind_rows(modelo10,prob10)


# guardar -----------------------------------------------------------------


# tablas para informe -----------------------------------------------------

modelos <- list(ajuste_modelo_or, M3, M4, M5, M6, M7, M8, M9, M10)

writexl::write_xlsx(modelos, "output/tablas/03_modelos_reporte.xlsx")

save(ajuste_modelo_or, M3, M4, M5, M6, M7, M8, M9, M10,  file = "output/data/03_2_modelos_reporte.RData")

