# Script 02: etiquetado  --------------------------------------------------------
## Autora: Dafne Jaime Vargas ----------------------------------------------
## Seminario de título II ---------------------------------------------------------------

datos_sna$cant_empleos = set_label(datos_sna$cant_empleos, "Cantidad de empleos")
datos_sna$rel_cont = set_label(datos_sna$rel_cont, "Relacion contractual")
datos_sna$ingresos = set_label(datos_sna$ingresos, "Ingresos")
datos_sna$seg_accid = set_label(datos_sna$seg_accid, "Seguro de accidentes")
datos_sna$coti_previ = set_label(datos_sna$coti_previ, "Cotización previsión")
datos_sna$cesantia = set_label(datos_sna$cesantia, "Indemnización de cesantía")
datos_sna$previ_salud = set_label(datos_sna$previ_salud, "Cotización de salud")
datos_sna$enf_lab = set_label(datos_sna$enf_lab, "Enfermedad laboral")
datos_sna$afil_sindicato = set_label(datos_sna$afil_sindicato, "Afiliación sindicato")
datos_sna$hor_sem = set_label(datos_sna$hor_sem, "Jornada laboral")
datos_sna$sit_lab_fin = set_label(datos_sna$sit_lab_fin, "Situación laboral")
datos_sna$educacion = set_label(datos_sna$educacion, "Nivel educacional")
datos_sna$edad_tramos = set_label(datos_sna$edad_tramos, "Tramos etarios")

