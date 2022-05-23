# Script 01: Lectura de datos --------------------------------------------------------
## Autora: Dafne Jaime Vargas ----------------------------------------------
## Seminario de título II ---------------------------------------------------------------

rm(list = ls())

# cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, 
               haven, #carga datos
               dplyr, #transformación
               sjmisc) #exploración
# cargar datos ------------------------------------------------------------
datos_2020_b1 <- read_stata("../tesis/input/2020/Bases_EPS2020_presencial/01_Vivos presencial/MODULO_A_Entrevistado_in.dta")  # sus filas son entrevistados
datos_2020_b2 <- read_stata("../tesis/input/2020/Bases_EPS2020_presencial/01_Vivos presencial/MODULO_B_Historia_Laboral_in.dta") #sus filas son empleos de los entrevistados
datos_2020_b3 <- read_stata("../tesis/input/2020/Bases_EPS2020_presencial/01_Vivos presencial/MODULO_F_Entrevistado_in.dta")
datos_2020_b4 <- read_dta("../tesis/input/2020/Bases_EPS2020_presencial/01_Vivos presencial/MODULO_B_Situacion_Actual_in.dta")
datos_exp <- read_stata("../tesis/input/2020/ponderador/Factor_XS_EPS_PRES_in.dta") 

# creación dinamismo ------------------------------------------------------

#crear datos
dinamismo <- datos_2020_b2 %>%  
                  dplyr::select(folio_n20,b2a_SO,orden,b2_a_inicio_mes,
                         b2_a_inicio_anio, b2_a_termino_mes, b2_a_termino_anio ) %>%
                  mutate(situa_lab= case_when(b2a_SO == 1 ~ "Trabajando",
                                       b2a_SO == 2 ~ "Cesante",
                                       b2a_SO == 3 ~ "Buscando trabajo por 1ra vez",
                                       b2a_SO == 4 ~ "Inactivo",
                                       TRUE ~ NA_character_ ))

# #crear frecuencias (no es necesario)
# n_occur <- data.frame(table(dinamismo$folio_n20))
# 
# #guardar en excel
# writexl::write_xlsx(n_occur, "input/dinamismop1.xlsx")

# creación variables dummy para futuros cálculos de frecuencia ------------

asc <- dinamismo %>% 
  group_by(folio_n20) %>%
  mutate(freq = n())

# creación de frecuencias de trabajo --------------------------------------

asc_frq <-asc %>% group_by(folio_n20) %>%
  summarize(freq_trab = sum(b2a_SO==1), 
            freq_cesant = sum(b2a_SO==2),
            freq_inact = sum(b2a_SO %in% c(3,4)),
            orden = orden)%>% 
  ungroup() %>% 
  dplyr::select(folio_n20, freq_trab, freq_cesant, freq_inact, orden)

# asci <- asc %>%
#   filter(b2a_SO == 1) %>%
#   mutate(freq_traba = n()) %>%
#   dplyr::select(freq_trab, situa_lab, freq, folio_n20, orden)
# 
# # creación frecuencias cesantías ------------------------------------------
# asci2 <- asc %>%
#   filter(b2a_SO == 2) %>%
#   mutate(freq_cesant = n()) %>% 
#   unfilter() %>% 
#   dplyr::select(freq_cesant, situa_lab, freq, folio_n20, orden)
# 
# 
# # creación frecuencias inactivos ------------------------------------------
# asci3 <- asc %>%
#   filter(b2a_SO %in% c(3, 4)) %>%
#   mutate(freq_inact = n()) %>%
#   dplyr::select(freq_inact, situa_lab, freq, folio_n20, orden)

# unión de observaciones por id
# unido <-  bind_rows(asci, asci2, asci3)

unido <- list(asc, 
              asc_frq) %>% 
  Reduce(function(x,y) merge(x,y, by = c("folio_n20", "orden"), all.x = T), . )

# #eliminación de NA innecesarios
# unido <-  unido %>% mutate_at(vars(starts_with("freq_")), funs(as.numeric(.))) %>% 
#   mutate(freq_trab = if_else(is.na(freq_trab) & (!is.na(freq_cesant) | !is.na(freq_inact)), 99, freq_trab),
#          freq_cesant = if_else(is.na(freq_cesant) & (!is.na(freq_trab) | !is.na(freq_inact)), 99, freq_cesant),
#          freq_inact = if_else(is.na(freq_inact) & (!is.na(freq_trab) | !is.na(freq_cesant)), 99, freq_inact)) 
 
#guardar datos creados
# writexl::write_xlsx(unido, path = "input/correccion.xlsx")
# 
# #se crean variables temporales de contabilización de estado según sit_lab
# #carga 
# 
# unido <- readxl::read_excel(path = "input/correccion.xlsx")

# #los duplica eliminar
# unido2 <- list(asc %>%
#                 select(folio_n20, freq, situa_lab),
#               asci %>%
#                 select(folio_n20, freq_trab),
#               asci2 %>%
#                 select(folio_n20, freq_cesant),
#               asci3 %>%
#                 select(folio_n20, freq_inact)) %>%
#   Reduce(function(x,y) merge(x,y, by = c("folio_n20"), all = T, no.dups = F), .)


#merge daproblemas

datos_dinam  <- list(datos_2020_b2, 
                     unido %>% 
                      dplyr::select(folio_n20, orden, starts_with("freq"))) %>% 
  Reduce(function(x,y) merge(x,y, by = c("folio_n20", "orden"), all.x = T), . )



# datos_dinam <- bind_cols(datos_2020_b2, unido)
# 
# datos_dinam <- datos_dinam %>% 
#                   select(-folio_n20...52) %>% 
#                   rename(folio_n20 = folio_n20...48 )

# guardar datos -----------------------------------------------------------
save(datos_dinam , datos_exp, datos_2020_b1, datos_2020_b2, datos_2020_b3, datos_2020_b4, file = "input/data/01-datos-originales.RData")



