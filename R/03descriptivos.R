rm(list = ls()) #Borrar todos los objetos del espacio de trabajo
# cargar paquetes ---------------------------------------------------------

pacman::p_load(tidyverse, haven, srvyr, sjPlot, DescTools)

# cargar datos  -------------------------------------------------------------------------
load("output/data/03-modelos_analisis.RData")
load("output/data/02-datos_proc.RData")

# opcion union

# union de datos y proporciones ----------------------------------------------------------

descriptivos <-cbind(datos_sna,  M5_or$predclass, M6_or$predclass, M7_or$predclass, M8_or$predclass, M9_or$predclass, M4_or$predclass, M3_or$predclass, M10_or$predclass)

#creación de objeto encuesta
objeto_encuesta <- descriptivos %>% 
  as_survey_design(ids = folio_n20, 
                   weights = factor_XS)


# tablas ------------------------------------------------------------------

# 
# ctable(M7_or$predclass, descriptivos$sexo, weights = datos_exp$factor_Panel_RE)
# 
# ctable(M7_or$predclass, descriptivos$edad_tramos, weights = datos_exp$factor_Panel_RE)
# 
# ctable(M7_or$predclass, descriptivos$educacion, weights = datos_exp$factor_Panel_RE )


# tabla cruzada 

sjt.xtab(objeto_encuesta$variables$`M6_or$predclass`,objeto_encuesta$variables$sexo,
         title = "Tabla 4.
         Proporciones por clase de precariedad y sexo",
         show.obs = F,
         show.row.prc = T,
         show.col.prc = T,
         weight.by = objeto_encuesta$variables$factor_XS, file = "output/tablas/descriptivos/tabla1.2.doc") 


sjt.xtab(objeto_encuesta$variables$`M6_or$predclass`,objeto_encuesta$variables$edad_tramos, 
         title = "Tabla 4.
         Proporciones por clase de precariedad y edad",
         show.obs = F,
         show.row.prc = T,
         show.col.prc = T,
         weight.by = objeto_encuesta$variables$factor_XS, file = "output/tabla2.2.doc")


sjt.xtab( objeto_encuesta$variables$`M6_or$predclass`, objeto_encuesta$variables$educacion,
          title = "Tabla 4.
         Proporciones por clase de precariedad y sexo",
          show.obs = F,
          show.row.prc = T,
          show.col.prc = T,
          weight.by = objeto_encuesta$variables$factor_XS, file = "output/tabla3.2.doc")

sjt.xtab( objeto_encuesta$variables$`M6_or$predclass`, objeto_encuesta$variables$CIUO_08_CL_1dig,
          title = "Tabla 4.
         Proporciones por clase de precariedad y ocupación",
          show.obs = F,
          show.row.prc = T,
          show.col.prc = T,
          weight.by = objeto_encuesta$variables$factor_XS, file = "output/tabla4.2.doc")

sjt.xtab( objeto_encuesta$variables$`M6_or$predclass`, objeto_encuesta$variables$CIIU_1dig_cat,
          title = "Tabla 4.
         Proporciones por clase de precariedad y actividad",
          show.obs = F,
          show.row.prc = T,
          show.col.prc = T,
          weight.by = objeto_encuesta$variables$factor_XS, file = "output/tabla5.2.doc")


# con objeto encuesta  ----------------------------------------------------

tabla_sexo <- objeto_encuesta %>% 
  group_by(sexo, objeto_encuesta$variables$`M6_or$predclass`) %>% 
  summarise(prop = survey_prop(vartype = "ci", level = 0.95, na.rm = T),
            total = survey_total(vartype = "ci", level = 0.95, na.rm = T)) %>% 
  mutate(prop_low = round(prop_low*100, digits = 1),
         prop = round(prop*100, digits = 1), 
         prop_upp = round(prop_upp*100, digits = 1)) %>% 
  select(1,2,4,3,5) %>% 
  pivot_wider(., names_from = "sexo",
              values_from = c("prop_upp", "prop", "prop_low")) %>% 
  select(1,2,4,6,3,5,7)

 
tabla_edad <- objeto_encuesta %>%
  group_by(edad_tramos,objeto_encuesta$variables$`M6_or$predclass`) %>%
  summarise(prop = survey_prop(vartype = "ci", level = 0.95, na.rm = T),
            total = survey_total(vartype = "ci", level = 0.95, na.rm = T)) %>%
  mutate(prop_low = round(prop_low*100, digits = 1),
         prop = round(prop*100, digits = 1), 
         prop_upp = round(prop_upp*100, digits = 1))%>%
  select(1,2,4,3,5) %>%
  pivot_wider(., names_from = "edad_tramos",
              values_from = c("prop_upp", "prop", "prop_low")) %>%
  select(1,2,5,8,3,6,9,4,7,10)

tabla_educacion <- objeto_encuesta %>% 
  group_by(educacion,objeto_encuesta$variables$`M6_or$predclass`) %>% 
  summarise(prop = survey_prop(vartype = "ci", level = 0.95, na.rm = T),
            total = survey_total(vartype = "ci", level = 0.95, na.rm = T)) %>% 
  mutate(prop_low = round(prop_low*100, digits = 1),
         prop = round(prop*100, digits = 1), 
         prop_upp = round(prop_upp*100, digits = 1)) %>% 
  select(1,2,4,3,5) %>% 
  pivot_wider(., names_from = "educacion",
              values_from = c("prop_upp", "prop", "prop_low")) %>% 
  select(1,5,10,15,2,7,12,3,8,13,4,9,14,6,11,16)


tablas_descriptivas <- list(tabla_edad, tabla_educacion, tabla_sexo)

writexl::write_xlsx(tablas_descriptivas, "output/tablas/descriptivos/tablas_descriptivas.xlsx")

# guardado ----------------------------------------------------------------

save(descriptivos, objeto_encuesta, file = "output/tablas/descriptivos/03-2_datos_analisis.RData")
