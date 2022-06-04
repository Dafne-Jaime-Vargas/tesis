# Script 02: procesamiento --------------------------------------------------------
## Autora: Dafne Jaime Vargas ----------------------------------------------
## Seminario de título II ---------------------------------------------------------------

rm(list = ls())
# 1. -------------------- Carga de paquetes y bases ---------------------

pacman::p_load(tidyverse, #universo de paquetes para el procesamiento
               dplyr, #manipulación de datos
               sjmisc, #estadísticos básicos
               sjlabelled, #para el etiquetado
               car) #recodificacion


load("input/data/01-datos-originales.RData")

# 2. -------------------- Selección, limpieza y descriptivos ----------------------

datos_proc_EPS <- list(datos_dinam %>% 
                         dplyr::select(b2a_SO, b2_a, b9_a, b9_b, b9_c , b4,  b6, b8, b12_t, b12, 
                                b17, b16_a, b16_b, b13, b2_a_inicio_mes, 
                                b2_a_inicio_anio, b2_a_termino_mes, b2_a_termino_anio,
                                b18, b24_a, b24_c, folio_n20, starts_with("freq"), 
                                orden, b22, b23_b, CIIU_1dig_cat), 
                       datos_2020_b3 %>% 
                         dplyr::select(f1, f1_9_e_cod, f35a, f30a, folio_n20 ),
                       datos_2020_b1 %>% 
                         dplyr::select(a8, a9, folio_n20, CINE_P_1dig, a10_b), 
                       datos_2020_b4 %>% 
                         dplyr::select( b39, folio_n20  )) %>% 
  Reduce(function(x,y) merge(x,y, by = c("folio_n20"), all.x = T), . )


#renombrar

datos <- datos_proc_EPS %>% rename(sexo=a8,
                                   edad= a9,
                                   sit_lab_fin = b2a_SO,
                                   ingresos_tramo = b12_t,
                                   sit_lab_act = b2_a,
                                   arreg_rel_cont = b9_a,
                                   rel_cont = b9_b,
                                   ingresos = b12,
                                   seg_accid = b17,
                                   coti_previ = b18,
                                   seg_cesantia = b24_a, arreg_seg_cesantia = b24_c,
                                   previ_salu = f1, arreg_previ_salud=f1_9_e_cod ,
                                   licencia_med = f30a,
                                   afil_sindicato = b16_b,  
                                   arreg_sindicato=b16_a ,
                                   hor_sem = b13,
                                   educacion = CINE_P_1dig)




# recodificacion ----------------------------------------------------------

datos_proc <-datos %>% mutate_at(vars(sit_lab_act, sit_lab_fin, previ_salu, licencia_med, ingresos_tramo, sexo, coti_previ), funs(as.numeric(.))) %>% 
  mutate(sit_lab_fina = car::recode(.$sit_lab_fin, recodes = c("1= 'Trabajando';
                            c(2) = 'Cesante';
                            c(3,4) = 'Inactivo'"), as.factor = T, 
                                    levels = c("Trabajando", "Cesante", "Inactivo")),
         cant_empleos = case_when(freq_trab == 1 ~ "1 empleo en 4 años",
                                  freq_trab %in% c(2, 3, 4) ~ "2 a 4 empleos",
                                  freq_trab %in% c(5, 6, 7, 8)~"5 a 8 empleos",
                                  freq_trab %in% c(9, 10,11,12,13,14,15,16)~"9 a 16 empleos",
                                  freq_trab == 0 ~ "0 empleos en 4 años",
                                  TRUE ~ NA_character_) %>% 
         cant_desempleo = case_when(freq_cesant %in% c(1)~ "1 desempleo en 4 años",
                                    freq_cesant %in% c(2, 3, 4) ~ "2 a 4 desempleos",
                                    freq_cesant %in% c(5, 6, 7, 8) ~"5 a 8 desempleos",
                                    freq_cesant %in% c(9, 10,11,12,13,14,15,16,17, 18,19)~"9 a 19 desempleos",
                                    freq_cesant %in% c(0)~ "0 desempleos en 4 años",
                                    TRUE ~ NA_character_),
         n_cambios_actividad = case_when(freq %in% c(1)~ "0 cambios en la actividad en 4 años",
                                         freq %in% c(2, 3, 4) ~ "2 a 4 cambios",
                                         freq %in% c(5, 6, 7, 8)~"5 a 8 cambios",
                                         freq %in% c(9, 10,11,12,13,14,15)~"10 a 15 cambios",
                                         freq %in% c(16,18,19,20,25)~ "16 a 25 cambios",
                                         TRUE ~ NA_character_),
         rel_cont = case_when(rel_cont == 1 ~ "Plazo indefinido",
                              rel_cont %in% c(2, 4, 5)  ~ "Plazo fijo",
                              rel_cont == 3 ~ "Por obra, faena o servicio", 
                              arreg_rel_cont == 3 ~ "Ausencia de relacion contractual",
                              sit_lab_fin %in% c(2, 3,4) ~ "Cesante o inactivo ",
                              TRUE ~ NA_character_),
         ingresos_imp = case_when(sit_lab_fin %in% c(2, 3,4) ~ "Cesante o inactivo",
                                  ingresos_tramo %in% c(1,2,3,4) |is.na(ingresos) ~ "menos del sueldo min",
                                  ingresos_tramo %in% c(5) | is.na(ingresos) ~ "1 sueldo min", #279.930
                                  ingresos_tramo %in% c(6,7) | is.na(ingresos) ~ "2 sueldos min", #559.860
                                  ingresos_tramo %in% c(8,9,10) | is.na(ingresos) ~ "3 sueldos min", # 839.790 aprox a 1
                                  ingresos_tramo %in% c(11,12) | is.na(ingresos) ~ "mas de tres sueldos min", #1.119.720
                                  TRUE ~ NA_character_),
         ingresos = case_when(sit_lab_fin %in% c(2, 3,4) ~ "Cesante o inactivo",
                              ingresos >=  20000 & ingresos <= 279930 | ingresos_imp== "menos del sueldo min" ~ "menos del sueldo min", #279.930
                              ingresos >= 279931 & ingresos <= 301000 | ingresos_imp == "1 sueldo min" ~ "hasta un sueldo min",
                              ingresos >= 301001 & ingresos  <= 560000 | ingresos_imp== "2 sueldos min" ~ "hasta dos sueldos min",
                              ingresos >= 560001 & ingresos  <= 840000 | ingresos_imp== "3 sueldos min" ~ "hasta tres sueldos min",
                              ingresos >= 840001  | ingresos_imp== "mas de tres sueldos min" ~ "mas de tres sueldos min",
                              TRUE ~ NA_character_),
         seg_accid = case_when(seg_accid %in% c(1, 2, 4, 3) ~ "Si",
                               seg_accid == 5  ~ "No",
                               sit_lab_fin %in% c(2, 3,4) ~ "Cesante o inactivo ",
                               TRUE ~ NA_character_),
         coti_previ = case_when(coti_previ <= 6 ~ "Si",
                                coti_previ == 7  ~ "No",
                                sit_lab_fin == 2 | sit_lab_fin %in% c(3,4) ~ "Cesante o inactivo ",
                                TRUE ~ NA_character_),
         cesantia = case_when(seg_cesantia  == 1 ~ "Si, del seguro de cesantía",
                              b23_b %in% c(1,2) ~ "Sí, indemnización del empleador",
                              seg_cesantia == 2 ~ "No, por parte del seguro de cesantía",
                              b23_b == 3 ~ "No, pero corresponde por parte del empleador",
                              b23_b == 4 ~ "No, pero no corresponde por parte del empleador",
                              is.na(seg_cesantia) | is.na(b23_b) & sit_lab_fin == 1 ~  "No ha estado en la situación",
                              TRUE ~ NA_character_),
         seg_cesantia = case_when(seg_cesantia == 1 ~ "Si",
                                  seg_cesantia == 2 ~ "No",
                                  sit_lab_fin == 1 ~  "No ha estado en la situación",
                                  TRUE ~ NA_character_),
         previ_salud = car::recode(.$previ_salu, recodes =  c("c(1, 2, 3, 4, 5) = 'Sistema publico'; 
                                                               c(7, 8) = 'Sistema privado'"), as.factor = T,
                                   levels = c("Sistema publico", "Sistema privado")),
         enf_lab = case_when(b39==1 | f35a %in% c(1,3) ~ "Si",
                             b39==2 | f35a == 2 ~ "No",
                             sit_lab_fin %in% c(2, 3,4) ~ "Cesante o inactivo ",
                             TRUE ~ NA_character_),
         licencia_med = car::recode(.$licencia_med, recodes = c("1 = 'Si'; 
                                                                2 = 'No'"), as.factor = T,
                                    levels = c("Si", "No")),
         afil_sindicato = case_when(afil_sindicato == 1 ~ "Si",
                                    afil_sindicato == 2  ~ "No",
                                    arreg_sindicato == 2  ~ "No existe sindicato",
                                    sit_lab_fin %in% c(2, 3,4) ~ "Cesante o inactivo ",
                                    TRUE ~ NA_character_),
         hor_sem = case_when(hor_sem >= 1 & hor_sem <= 30 ~ "media jornada",
                             hor_sem >= 31 & hor_sem  <= 45 ~ "jornada completa",
                             hor_sem >= 46 & hor_sem  <= 57 ~ "extra",
                             hor_sem >= 58 & hor_sem  <= 72 ~ "extralegal",
                             sit_lab_fin %in% c(2, 3,4) ~ "Cesante o inactivo",
                             TRUE ~ NA_character_ ),
         sexo = car::recode(.$sexo, recodes = c("1 = 'Hombre';
                                                2 = 'Mujer'"), as.factor = T),
         educacion = case_when(educacion == 888 ~ "Sin estudios",
                               educacion %in% c(1, 1000) ~ "Básica",
                               educacion %in% c(2,3) ~ "Media",
                               educacion %in% c(5, 6)  ~ "Superior",
                               educacion %in% c(7,8)  ~ "Postgrado",
                               TRUE  ~ NA_character_),
         edad_tramos = case_when(edad >= 22 & edad <= 29 ~ "Joven",
                                 edad >= 30 & edad <= 59 ~ "Adulto",
                                 edad >=60 ~ "Adulto mayor",
                                 TRUE ~ NA_character_)) %>% 
  mutate_at(vars(c("previ_salud")), ~(car::recode(., recodes = "c(88, 6,9, 99) = NA"))) %>% 
  dplyr::select(cant_empleos, rel_cont, ingresos, seg_accid, coti_previ,
         cesantia, previ_salud, enf_lab, afil_sindicato, hor_sem, sexo, 
         educacion, edad_tramos, folio_n20, starts_with("freq"), previ_salu, sit_lab_fina, orden) %>% 
  mutate_all(~(as_factor(.)))

# #Solución error de labels
# datos_proc$sit_lab_fin <- factor(c('Trabajando', 'Cesante','Inactivo' ), 
#                                   labels = c("Trabajando", "Cesante","Inactivo"))

#eliminación NA
sum(is.na(datos_proc))
dim(datos_proc)

datos_sna <-na.omit(datos_proc)
dim(datos_sna)

# etiquetado
source("R/02etiquetado.R")

#estimar modelo

f1 <- cbind(cant_empleos, rel_cont, ingresos, seg_accid, coti_previ,
            cesantia, previ_salud, enf_lab, afil_sindicato, hor_sem)~1

#guardar datos a utilizar

save(f1, datos_exp, datos_proc, datos_sna, file = "output/data/02-datos_proc.RData")




