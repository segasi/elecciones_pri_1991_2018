### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/cargar_paquetes_setup_tema_grafica.R")

### Cargar bases de datos de resultados finales (1994-2012) y cómputos distritales (2018) ----

# Fuentes: Las bases de datos de las elecciones de 1994 a 2012 fueron obtenidas del Sistema de Consulta de la Estadística de las Elecciones (https://siceen21.ine.mx/): La base de datos de 2018 se obtuvo de la página de los respectivos cómputos ditritales (https://computos2018.ine.mx/#/presidencia/nacional/1/1/1/1)


# 1994 ----
bd_1994_casilla <- read_delim("01_bd/presidenciales/1994_SEE_PRE_NAL_CAS.csv",  
                              delim = ",", 
                              escape_double = FALSE, 
                              trim_ws = TRUE,
                              locale = locale(encoding = "latin1"))

# Revisar posibles problemas al cargar datos
problems(bd_1994_casilla) %>% 
  count(actual)

# R no detectó "problemas"


# 2000 ----
bd_2000_casilla <- read_delim("01_bd/presidenciales/2000_SEE_PRE_NAL_CAS.csv",  
                              delim = ",", 
                              escape_double = FALSE, 
                              trim_ws = TRUE,
                              locale = locale(encoding = "latin1"))
# Revisar posibles problemas al cargar datos
problems(bd_2000_casilla) %>% 
  count(actual)

# Los "problemas" consisten en que para 1 observación R esperaba valores lógicos (TRUE/FALSE) en la columna ESTATUS_ACTA y encontró el valor "Anulada"


# 2006 ----
bd_2006_casilla <- read_delim("01_bd/presidenciales/2006_SEE_PRE_NAL_CAS.csv",  
                              delim = ",", 
                              escape_double = FALSE, 
                              trim_ws = TRUE,
                              locale = locale(encoding = "latin1"))

# Revisar posibles problemas al cargar datos
problems(bd_2006_casilla) %>% 
  count(actual)

# Los "problemas" consisten en que para 11 observaciones R esperaba valores lógicos (TRUE/FALSE) en la columna ESTATUS_ACTA y encontró el valor "No instalada"


# 2012 ----
bd_2012_casilla <- read_delim("01_bd/presidenciales/2012_SEE_PRE_NAL_CAS.csv",  
                              delim = ",", 
                              escape_double = FALSE, 
                              trim_ws = TRUE,
                              locale = locale(encoding = "latin1"))

# Revisar posibles problemas al cargar datos
problems(bd_2012_casilla) %>% 
  count(actual)

# R no detectó "problemas"

# 2018 ----
cd_2018_casilla <- read_delim("01_bd/presidenciales/20180708_2130_CW_presidencia/presidencia.csv",  
                              delim = "|", 
                              escape_double = FALSE, 
                              trim_ws = TRUE, 
                              skip = 5, 
                              locale = locale(encoding = "latin1"))

# Revisar posibles problemas al cargar datos
problems(cd_2018_casilla) %>% 
  count(actual)

# Los "problemas" consisten en que 5,225 observaciones incluyen 44 columnas, y las 156,840 restantes solo 42. Las dos columnas extras son nombradas por R como X43 y X44 y todos sus valores son NAs

cd_2018_casilla %>% 
  count(...43)

cd_2018_casilla %>% 
  count(...44)

# Por este motivos borramos ambas columnas:
cd_2018_casilla <- 
  cd_2018_casilla %>% 
  select(-c(43, 44)) 


### Cargar y procesar bases de datos de catálogos municipales para la elección de 2018 ----

# Este catálogo es necesario para posteriormente poder agregar los datos a nivel municipal para esta elección

catalogo_1 <- read_excel("01_bd/cartografia/Catalogo de Secciones_13feb18.xlsx", 
                         sheet = "1")

catalogo_2 <- read_excel("01_bd/cartografia/Catalogo de Secciones_13feb18.xlsx", 
                         sheet = "2")

# Unir data frames
catalogo <- rbind(catalogo_1, catalogo_2)

# "Limpiar" nombres de variables
catalogo <- clean_names(catalogo)

# Agregar 0s a claves de edo, distrito, municipio y sección 
catalogo_secciones_mpo <- 
  catalogo %>%
  mutate(cve_edo = str_pad(entidad, 2, pad = "0"),
         cve_dtto = str_pad(distrito, 2, pad = "0"),
         cve_mpo = str_pad(municipio, 3, pad = "0"),
         cve_seccion = str_pad(seccion, 4, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = ""),
         cve_edo_mpo = paste(cve_edo, cve_mpo, sep = ""),
         cve_edo_seccion = paste(cve_edo, cve_seccion, sep = "")) 




### "Limpiar" nombres de variables ----
bd_1994_casilla <- clean_names(bd_1994_casilla)
bd_2000_casilla <- clean_names(bd_2000_casilla)
bd_2006_casilla <- clean_names(bd_2006_casilla)
bd_2012_casilla <- clean_names(bd_2012_casilla)
cd_2018_casilla <- clean_names(cd_2018_casilla)


### Función para generar claves de estado, distrito, seccion, estado-distrito y estado-seccion para bd de 1994 a 2012 ----
fun_claves <- function(df) {
  df <- df %>%
    rename(estado = nombre_estado, 
           distrito = cabecera_distrital,
           cve_casilla = casilla) %>% 
    mutate(cve_circ = str_pad(circunscripcion, 2, pad = "0"),
           cve_edo = str_pad(id_estado, 2, pad = "0"),
           cve_mpo = str_pad(id_municipio, 3, pad = "0"),
           cve_dtto = str_pad(id_distrito, 2, pad = "0"),
           cve_seccion = str_pad(seccion, 4, pad = "0"),
           cve_edo_dtto = paste(cve_edo, cve_dtto, sep = ""),
           cve_edo_mpo = paste(cve_edo, cve_mpo, sep = ""),
           cve_edo_seccion = paste(cve_edo, cve_seccion, sep = "")) %>% 
    select(-c(starts_with("id_"), circunscripcion, seccion))  %>% 
    select(estado, distrito, municipio, cve_circ:cve_edo_seccion, everything())
}



### Generar claves de estado, distrito, seccion agregándoles 0s antes del número correspondiente, y generar claves de estado-distrito y estado-seccion----- 

bd_1994_casilla <- fun_claves(bd_1994_casilla)
bd_2000_casilla <- fun_claves(bd_2000_casilla)
bd_2006_casilla <- fun_claves(bd_2006_casilla)
bd_2012_casilla <- fun_claves(bd_2012_casilla)

cd_2018_casilla  <- 
  cd_2018_casilla %>% 
  rename(estado = nombre_estado, 
         distrito = nombre_distrito) %>% 
  mutate(cve_edo = str_pad(id_estado, 2, pad = "0"),
         cve_dtto = str_pad(id_distrito, 2, pad = "0"),
         cve_seccion = seccion,
         cve_casilla = id_casilla,
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = ""),
         cve_edo_seccion = paste(cve_edo, cve_seccion, sep = ""))  %>% 
  select(-c(starts_with("id_"), seccion)) %>% 
  select(estado, distrito, cve_edo:cve_edo_seccion, everything())


### Agregar columnas para identificar a qué municipio pertenece la sección en la bd de 2018 ----
cd_2018_casilla <- 
  cd_2018_casilla %>% 
  left_join(catalogo_secciones_mpo %>% select(cve_mpo, cve_edo_mpo, nombre_municipio, cve_edo_seccion), by = "cve_edo_seccion") %>% 
  rename(municipio = nombre_municipio) %>% 
  select(estado, distrito, municipio, cve_edo, cve_mpo, cve_dtto:cve_edo_dtto, cve_edo_mpo, cve_edo_seccion, everything())


### Para los datos de 2018, reemplazar valores de "-" por NAs y cambiar el tipo de dato en columnas que contienen datos de boletas o votos ----
cd_2018_casilla <- 
  cd_2018_casilla %>% 
  mutate_at(
    # Seleccionar variables
    vars(pan:vn),
    # Reemplazar cadenas de texto por NAs
    ~ ifelse(.x == "-", NA, .x) %>%
      # Cambiar tipo de dato a numeric
      as.numeric()
  ) %>% 
  glimpse()


### Función para generar versión en altas y bajas de los nombres de los estados ----
fun_nom_minusculas <- function(df) {
  df <- df %>%
    mutate(edo_min = case_when(cve_edo == "01" ~ "Aguascalientes", 
                               cve_edo == "02" ~ "Baja California", 
                               cve_edo == "03" ~ "Baja California Sur",
                               cve_edo == "04" ~ "Campeche",
                               cve_edo == "05" ~ "Coahuila",
                               cve_edo == "06" ~ "Colima",
                               cve_edo == "07" ~ "Chiapas",
                               cve_edo == "08" ~ "Chihuahua",
                               cve_edo == "09" ~ "Ciudad de México",
                               cve_edo == "10" ~ "Durango",
                               cve_edo == "11" ~ "Guanajuato",
                               cve_edo == "12" ~ "Guerrero",
                               cve_edo == "13" ~ "Hidalgo",
                               cve_edo == "14" ~ "Jalisco",
                               cve_edo == "15" ~ "Estado de México",
                               cve_edo == "16" ~ "Michoacán",
                               cve_edo == "17" ~ "Morelos",
                               cve_edo == "18" ~ "Nayarit",
                               cve_edo == "19" ~ "Nuevo León",
                               cve_edo == "20" ~ "Oaxaca",
                               cve_edo == "21" ~ "Puebla",
                               cve_edo == "22" ~ "Querétaro",
                               cve_edo == "23" ~ "Quintana Roo",
                               cve_edo == "24" ~ "San Luis Potosí",
                               cve_edo == "25" ~ "Sinaloa",
                               cve_edo == "26" ~ "Sonora",
                               cve_edo == "27" ~ "Tabasco",
                               cve_edo == "28" ~ "Tamaulipas",
                               cve_edo == "29" ~ "Tlaxcala",
                               cve_edo == "30" ~ "Veracruz",
                               cve_edo == "31" ~ "Yucatán",
                               cve_edo == "32" ~ "Zacatecas")) %>% 
    select(estado, edo_min, everything())
}


### Generar versión en altas y bajas de los nombres de los estados ----

bd_1994_casilla <- fun_nom_minusculas(bd_1994_casilla)
bd_2000_casilla <- fun_nom_minusculas(bd_2000_casilla)
bd_2006_casilla <- fun_nom_minusculas(bd_2006_casilla)
bd_2012_casilla <- fun_nom_minusculas(bd_2012_casilla)
cd_2018_casilla <- fun_nom_minusculas(cd_2018_casilla)


### Renombrar y/o generar variables de resultados electorales ----

# Para las elecciones de 1994 a 2006 renombramos variables usando las iniciales de los respectivos candidatos presidenciales. En todas creamos la variable v_validas.

# Para las elecciones de 2012 y 2018 es necesario generar una variable que registra los votos totales por candidato por el cambio en las reglas de coaliciones, mismas que obligan a contar los votos de cada partido por separado. 

# En el caso de 2012, renombramos varias variables. 

# En 2018 creamos la variable v_nulos porque dado que Margarita Zavala declinó a su candidatura después de que se habían impreso las boletas, el INE tuvo que contabilizarlas por separado en la columna cand_ind_01. Estos votos se consideran como nulos y deben sumarse a la columna vn. Para esta elección también genero la variable v_validos.

# 1994 ----
bd_1994_casilla <- 
  bd_1994_casilla %>% 
  rename(v_dfc = pan,
         v_ezpl = pri,
         v_ccs = prd,
         v_mlo = pps,
         v_rat = pfcrn,
         v_apt = parm,
         v_pem = uno_pdm,
         v_csg = pt,
         v_jgt = pvem,
         v_nulos = num_votos_nulos) %>% 
  mutate(v_validos = total_votos - num_votos_can_nreg - v_nulos)


# 2000 ----
bd_2000_casilla <-  
  bd_2000_casilla %>% 
  rename(v_vfq = ac,
         v_flo = pri,
         v_ccs = am,
         v_mcs = pcd,
         v_pml = parm,
         v_grg = dsppn,
         v_nulos = num_votos_nulos) %>% 
  mutate(v_validos = total_votos - num_votos_can_nreg - v_nulos)


# 2006 ----
bd_2006_casilla <- 
  bd_2006_casilla %>% 
  rename(v_fch = pan,
         v_rmp = apm,
         v_amlo = pbt,
         v_rcc = nva_alianza,
         v_pmc = asdc,
         v_nulos = num_votos_nulos) %>% 
  mutate(v_validos = total_votos - num_votos_can_nreg - v_nulos)


# 2012 ----
bd_2012_casilla <- 
  bd_2012_casilla %>% 
  rename(v_jvm = pan,
         v_gq = nva_alianza, 
         v_nulos = num_votos_nulos) %>% 
  mutate(v_epn = rowSums(select(., pri, pvem, pri_pvem), na.rm = T),
         v_amlo = rowSums(select(., prd, pt, mc, prd_pt_mc, prd_pt, prd_mc, pt_mc), na.rm = T),
         v_validos = total_votos - num_votos_can_nreg - v_nulos)

# 2018 ----
cd_2018_casilla <- 
  cd_2018_casilla %>% 
  mutate(v_meade = rowSums(select(., pri, pvem, nueva_alianza, pri_pvem_na, pri_pvem, pri_na, pvem_na), na.rm = T),
         v_amlo = rowSums(select(., pt, morena, encuentro_social, pt_morena_pes, pt_morena, pt_pes, morena_pes), na.rm = T),
         v_anaya = rowSums(select(., pan, prd, movimiento_ciudadano, pan_prd_mc, pan_prd, pan_mc, prd_mc), na.rm = T),
         v_bronco = rowSums(select(., cand_ind_02), na.rm = T), 
         v_nulos = rowSums(select(., vn, cand_ind_01), na.rm = T), 
         v_validos = total_votos_calculados - v_nulos)

### Funciones para generar dataframes con datos por sección, dtto y edo ----
fun_por_seccion <- function(df) {
  df <- 
    df %>% 
    group_by(cve_edo_seccion) %>% 
    summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else first(.))) %>% 
    ungroup() 
}

fun_por_dtto <- function(df) {
  df <- 
    df %>% 
    filter(cve_dtto != "00") %>% 
    group_by(cve_edo_dtto) %>% 
    summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else first(.))) %>% 
    ungroup()
}

fun_por_mpo <- function(df) {
  df <- 
    df %>% 
    group_by(cve_edo_mpo) %>% 
    summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else first(.))) %>% 
    ungroup()
}

fun_por_edo <- function(df) {
  df <- 
    df %>% 
    group_by(cve_edo) %>% 
    summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else first(.))) %>% 
    ungroup()
}


### Generar dataframes con datos por sección, dtto y edo para cada elección ----

# 1994 ----
bd_1994_seccion <- fun_por_seccion(bd_1994_casilla)
bd_1994_dtto <- fun_por_dtto(bd_1994_casilla)
bd_1994_mpo <- fun_por_mpo(bd_1994_casilla)
bd_1994_edo <- fun_por_edo(bd_1994_casilla)

# 2000 ----
bd_2000_seccion <- fun_por_seccion(bd_2000_casilla)
bd_2000_dtto <- fun_por_dtto(bd_2000_casilla)
bd_2000_mpo <- fun_por_mpo(bd_2000_casilla)
bd_2000_edo <- fun_por_edo(bd_2000_casilla)

# 2006 ----
bd_2006_seccion <- fun_por_seccion(bd_2006_casilla)
bd_2006_dtto <- fun_por_dtto(bd_2006_casilla)
bd_2006_mpo <- fun_por_mpo(bd_2006_casilla)
bd_2006_edo <- fun_por_edo(bd_2006_casilla)

# 2012 ----
bd_2012_seccion <- fun_por_seccion(bd_2012_casilla)
bd_2012_dtto <- fun_por_dtto(bd_2012_casilla)
bd_2012_mpo <- fun_por_mpo(bd_2012_casilla)
bd_2012_edo <- fun_por_edo(bd_2012_casilla)

# 2018 ----
cd_2018_seccion <- fun_por_seccion(cd_2018_casilla)
cd_2018_dtto <- fun_por_dtto(cd_2018_casilla)
cd_2018_mpo <- fun_por_mpo(cd_2018_casilla)
cd_2018_edo <- fun_por_edo(cd_2018_casilla)


### Funciones para generar variables para contar % de votos por candidato y el % de participación en cada casilla, sección y dataframe ----

# 1994 ----
fun_porcentajes_1994 <- function(df) {
  df %>% 
    mutate(vs_dfc = round((v_dfc/total_votos)*100, 2),
           vs_ezpl = round((v_ezpl/total_votos)*100, 2),
           vs_ccs = round((v_ccs/total_votos)*100, 2),
           vs_mlo = round((v_mlo/total_votos)*100, 2),
           vs_rat = round((v_rat/total_votos)*100, 2),
           vs_apt = round((v_apt/total_votos)*100, 2),
           vs_pem = round((v_pem/total_votos)*100, 2),
           vs_csg = round((v_csg/total_votos)*100, 2),
           vs_jgt = round((v_jgt/total_votos)*100, 2))
}

# 2000 ----
fun_porcentajes_2000 <- function(df) {
  df %>% 
    mutate(vs_vfq = round((v_vfq/total_votos)*100, 2),
           vs_flo = round((v_flo/total_votos)*100, 2),
           vs_ccs = round((v_ccs/total_votos)*100, 2),
           vs_mcs = round((v_mcs/total_votos)*100, 2),
           vs_pml = round((v_pml/total_votos)*100, 2),
           vs_grg = round((v_grg/total_votos)*100, 2))
}

# 2006 ----
fun_porcentajes_2006 <- function(df) {
  df %>% 
    mutate(vs_fch = round((v_fch/total_votos)*100, 2),
           vs_amlo = round((v_amlo/total_votos)*100, 2),
           vs_rmp = round((v_rmp/total_votos)*100, 2),
           vs_rcc = round((v_rcc/total_votos)*100, 2),
           vs_pmc = round((v_pmc/total_votos)*100, 2),
           participacion = round((total_votos/lista_nominal)*100, 2))
}

# 2012 ----
fun_porcentajes_2012 <- function(df) {
  df %>% 
    mutate(vs_epn = round((v_epn/total_votos)*100, 2),
           vs_amlo = round((v_amlo/total_votos)*100, 2),
           vs_jvm = round((v_jvm/total_votos)*100, 2),
           vs_gq = round((v_gq/total_votos)*100, 2), 
           participacion = round((total_votos/lista_nominal)*100, 2))
}

# 2018 ----
fun_porcentajes_2018 <- function(df) {
  df %>% 
    mutate(vs_meade = round((v_meade/total_votos_calculados)*100, 2),
           vs_amlo = round((v_amlo/total_votos_calculados)*100, 2),
           vs_anaya = round((v_anaya/total_votos_calculados)*100, 2),
           vs_bronco = round((v_bronco/total_votos_calculados)*100, 2), 
           participacion = round((total_votos_calculados/lista_nominal_casilla)*100, 2))
}


### Generar variables para contar % de votos por candidato y el % de participación en cada casilla, sección y data frame ----

# 1994 ----
bd_1994_casilla <- fun_porcentajes_1994(bd_1994_casilla)
bd_1994_seccion <- fun_porcentajes_1994(bd_1994_seccion)
bd_1994_dtto <- fun_porcentajes_1994(bd_1994_dtto)
bd_1994_mpo <- fun_porcentajes_1994(bd_1994_mpo)
bd_1994_edo <- fun_porcentajes_1994(bd_1994_edo)

# 2000 ----
bd_2000_casilla <- fun_porcentajes_2000(bd_2000_casilla)
bd_2000_seccion <- fun_porcentajes_2000(bd_2000_seccion)
bd_2000_dtto <- fun_porcentajes_2000(bd_2000_dtto)
bd_2000_mpo <- fun_porcentajes_2000(bd_2000_mpo)
bd_2000_edo <- fun_porcentajes_2000(bd_2000_edo)

# 2006 ----
bd_2006_casilla <- fun_porcentajes_2006(bd_2006_casilla)
bd_2006_seccion <- fun_porcentajes_2006(bd_2006_seccion)
bd_2006_dtto <- fun_porcentajes_2006(bd_2006_dtto)
bd_2006_mpo <- fun_porcentajes_2006(bd_2006_mpo)
bd_2006_edo <- fun_porcentajes_2006(bd_2006_edo)

# 2012 ----
bd_2012_casilla <- fun_porcentajes_2012(bd_2012_casilla)
bd_2012_seccion <- fun_porcentajes_2012(bd_2012_seccion)
bd_2012_dtto <- fun_porcentajes_2012(bd_2012_dtto)
bd_2012_mpo <- fun_porcentajes_2012(bd_2012_mpo)
bd_2012_edo <- fun_porcentajes_2012(bd_2012_edo)

# 2018 ----
cd_2018_casilla <- fun_porcentajes_2018(cd_2018_casilla)
cd_2018_seccion <- fun_porcentajes_2018(cd_2018_seccion)
cd_2018_dtto <- fun_porcentajes_2018(cd_2018_dtto)
cd_2018_mpo <- fun_porcentajes_2018(cd_2018_mpo)
cd_2018_edo <- fun_porcentajes_2018(cd_2018_edo)


### Funciones para generar variables que identifiquen al 1ro., 2ndo y 3er. lugar en cada casilla, sección, mpo, dtto y edo ----

# 1994 ----
fun_ranking_1994 <- function(df) {
  as_tibble(data.frame(df, t(apply(-df[, 28:36], 1, rank, ties.method='min')))) %>%
    mutate(primero = case_when(vs_dfc.1 == 1 ~ "Fernández de Ceballos",
                               vs_ezpl.1 == 1 ~ "Zedillo",
                               vs_ccs.1 == 1 ~ "Cárdenas",
                               vs_mlo.1 == 1 ~ "Lombardo",
                               vs_rat.1 == 1 ~ "Aguilar Talamantes",
                               vs_apt.1 == 1 ~ "Pérez Treviño",
                               vs_pem.1 == 1 ~ "Madero",
                               vs_csg.1 == 1 ~ "Soto",
                               vs_jgt.1 == 1 ~ "González Tórres",
                               TRUE ~ "Empate"),
           segundo = case_when(vs_dfc.1 == 2 ~ "Fernández de Ceballos",
                               vs_ezpl.1 == 2 ~ "Zedillo",
                               vs_ccs.1 == 2 ~ "Cárdenas",
                               vs_mlo.1 == 2 ~ "Lombardo",
                               vs_rat.1 == 2 ~ "Aguilar Talamantes",
                               vs_apt.1 == 2 ~ "Pérez Treviño",
                               vs_pem.1 == 2 ~ "Madero",
                               vs_csg.1 == 2 ~ "Soto",
                               vs_jgt.1 == 2 ~ "González Tórres",
                               TRUE ~ "Empate"),
           tercero = case_when(vs_dfc.1 == 3 ~ "Fernández de Ceballos",
                               vs_ezpl.1 == 3 ~ "Zedillo",
                               vs_ccs.1 == 3 ~ "Cárdenas",
                               vs_mlo.1 == 3 ~ "Lombardo",
                               vs_rat.1 == 3 ~ "Aguilar Talamantes",
                               vs_apt.1 == 3 ~ "Pérez Treviño",
                               vs_pem.1 == 3 ~ "Madero",
                               vs_csg.1 == 3 ~ "Soto",
                               vs_jgt.1 == 3 ~ "González Tórres",
                               TRUE ~ "Empate"))
}

# 2000 ----
fun_ranking_2000 <- function(df) {
  as_tibble(data.frame(df, t(apply(-df[, 25:30], 1, rank, ties.method='min')))) %>% 
    mutate(primero = case_when(vs_vfq.1 == 1 ~ "Fox",
                               vs_flo.1 == 1 ~ "Labastida",
                               vs_ccs.1 == 1 ~ "Cárdenas",
                               vs_mcs.1 == 1 ~ "Camacho",
                               vs_pml.1 == 1 ~ "Muñoz Ledo",
                               vs_grg.1 == 1 ~ "Rincón Gallardo"),
           segundo = case_when(vs_vfq.1 == 2 ~ "Fox",
                               vs_flo.1 == 2 ~ "Labastida",
                               vs_ccs.1 == 2 ~ "Cárdenas",
                               vs_mcs.1 == 2 ~ "Camacho",
                               vs_pml.1 == 2 ~ "Muñoz Ledo",
                               vs_grg.1 == 2 ~ "Rincón Gallardo"),
           tercero = case_when(vs_vfq.1 == 3 ~ "Fox",
                               vs_flo.1 == 3 ~ "Labastida",
                               vs_ccs.1 == 3 ~ "Cárdenas",
                               vs_mcs.1 == 3 ~ "Camacho",
                               vs_pml.1 == 3 ~ "Muñoz Ledo",
                               vs_grg.1 == 3 ~ "Rincón Gallardo"))
}

# 2006 ----
fun_ranking_2006 <- function(df) {
  as_tibble(data.frame(df, t(apply(-df[, 26:30], 1, rank, ties.method='min')))) %>% 
    mutate(primero = case_when(vs_fch.1 == 1 ~ "Calderón",
                               vs_rmp.1 == 1 ~ "Madrazo",
                               vs_amlo.1 == 1 ~ "AMLO",
                               vs_rcc.1 == 1 ~ "Campa",
                               vs_pmc.1 == 1 ~ "Mercado"),
           segundo = case_when(vs_fch.1 == 2 ~ "Calderón",
                               vs_rmp.1 == 2 ~ "Madrazo",
                               vs_amlo.1 == 2 ~ "AMLO",
                               vs_rcc.1 == 2 ~ "Campa",
                               vs_pmc.1 == 2 ~ "Mercado"),
           tercero = case_when(vs_fch.1 == 3 ~ "Calderón",
                               vs_rmp.1 == 3 ~ "Madrazo",
                               vs_amlo.1 == 3 ~ "AMLO",
                               vs_rcc.1 == 3 ~ "Campa",
                               vs_pmc.1 == 3 ~ "Mercado"))
}


# 2012 ----
fun_ranking_2012 <- function(df) {
  as_tibble(data.frame(df, t(apply(-df[, 37:40], 1, rank, ties.method='min')))) %>% 
    mutate(primero = case_when(vs_jvm.1 == 1 ~ "Vázquez Mota",
                               vs_epn.1 == 1 ~ "Peña Nieto",
                               vs_amlo.1 == 1 ~ "AMLO",
                               vs_gq.1 == 1 ~ "Quadri"),
           segundo = case_when(vs_jvm.1 == 2 ~ "Vázquez Mota",
                               vs_epn.1 == 2 ~ "Peña Nieto",
                               vs_amlo.1 == 2 ~ "AMLO",
                               vs_gq.1 == 2 ~ "Quadri"),
           tercero = case_when(vs_jvm.1 == 3 ~ "Vázquez Mota",
                               vs_epn.1 == 3 ~ "Peña Nieto",
                               vs_amlo.1 == 3 ~ "AMLO",
                               vs_gq.1 == 3 ~ "Quadri"))
}


# 2018 ----
fun_ranking_2018 <- function(df) {
  as_tibble(data.frame(df, t(apply(-df[, 55:58], 1, rank, ties.method='min')))) %>% 
    mutate(primero = case_when(vs_anaya.1 == 1 ~ "Anaya",
                               vs_meade.1 == 1 ~ "Meade",
                               vs_amlo.1 == 1 ~ "AMLO",
                               vs_bronco.1 == 1 ~ "El Bronco"),
           segundo = case_when(vs_anaya.1 == 2 ~ "Anaya",
                               vs_meade.1 == 2 ~ "Meade",
                               vs_amlo.1 == 2 ~ "AMLO",
                               vs_bronco.1 == 2 ~ "El Bronco"),
           tercero = case_when(vs_anaya.1 == 3 ~ "Anaya",
                               vs_meade.1 == 3 ~ "Meade",
                               vs_amlo.1 == 3 ~ "AMLO",
                               vs_bronco.1 == 3 ~ "El Bronco"))
}


### Generar variables que identifiquen al 1ro., 2ndo y 3er. lugar en cada casilla, sección, mpo, dtto y edo ----

# 1994 ----
bd_1994_casilla <- fun_ranking_1994(bd_1994_casilla)
bd_1994_seccion <- fun_ranking_1994(bd_1994_seccion)
bd_1994_dtto <- fun_ranking_1994(bd_1994_dtto)
bd_1994_mpo <- fun_ranking_1994(bd_1994_mpo)
bd_1994_edo <- fun_ranking_1994(bd_1994_edo)

# 2000 ----
bd_2000_casilla <- fun_ranking_2000(bd_2000_casilla)
bd_2000_seccion <- fun_ranking_2000(bd_2000_seccion)
bd_2000_dtto <- fun_ranking_2000(bd_2000_dtto)
bd_2000_mpo <- fun_ranking_2000(bd_2000_mpo)
bd_2000_edo <- fun_ranking_2000(bd_2000_edo)

# 2006 ----
bd_2006_casilla <- fun_ranking_2006(bd_2006_casilla)
bd_2006_seccion <- fun_ranking_2006(bd_2006_seccion)
bd_2006_dtto <- fun_ranking_2006(bd_2006_dtto)
bd_2006_mpo <- fun_ranking_2006(bd_2006_mpo)
bd_2006_edo <- fun_ranking_2006(bd_2006_edo)

# 2012 ----
bd_2012_casilla <- fun_ranking_2012(bd_2012_casilla)
bd_2012_seccion <- fun_ranking_2012(bd_2012_seccion)
bd_2012_dtto <- fun_ranking_2012(bd_2012_dtto)
bd_2012_mpo <- fun_ranking_2012(bd_2012_mpo)
bd_2012_edo <- fun_ranking_2012(bd_2012_edo)

# 2018 ----
cd_2018_casilla <- fun_ranking_2018(cd_2018_casilla)
cd_2018_seccion <- fun_ranking_2018(cd_2018_seccion)
cd_2018_dtto <- fun_ranking_2018(cd_2018_dtto)
cd_2018_mpo <- fun_ranking_2018(cd_2018_mpo)
cd_2018_edo <- fun_ranking_2018(cd_2018_edo)



### Funciones para calcular el margen de victoria entre el candidato presidencial ganador y el segundo lugar en cada casilla, sección, mpo, dtto y edo ----

# 1994 ----
fun_margenes_1994 <- function(df) {
  df %>% 
    mutate(margen_victoria = case_when(primero == "Zedillo" & segundo == "Aguilar Talamantes" ~ vs_ezpl - vs_rat,
                                       primero == "Zedillo" & segundo == "Cárdenas" ~ vs_ezpl - vs_ccs,
                                       primero == "Zedillo" & segundo == "Fernández de Ceballos" ~ vs_ezpl - vs_dfc,
                                       primero == "Zedillo" & segundo == "González Tórres" ~ vs_ezpl - vs_jgt,
                                       primero == "Zedillo" & segundo == "Lombardo" ~ vs_ezpl - vs_mlo,
                                       primero == "Zedillo" & segundo == "Madero" ~ vs_ezpl - vs_pem,
                                       primero == "Zedillo" & segundo == "Pérez Treviño" ~ vs_ezpl - vs_apt,
                                       primero == "Zedillo" & segundo == "Soto" ~ vs_ezpl - vs_csg))
}

# 2000 ----  
fun_margenes_2000 <- function(df) {
  df %>% 
    mutate(margen_victoria = case_when(primero == "Fox" & segundo == "Labastida" ~ vs_vfq - vs_flo,
                                       primero == "Fox" & segundo == "Cárdenas" ~ vs_vfq - vs_ccs,
                                       primero == "Fox" & segundo == "Camacho" ~ vs_vfq - vs_mcs,
                                       primero == "Fox" & segundo == "Muñoz Ledo" ~ vs_vfq - vs_pml,
                                       primero == "Fox" & segundo == "Rincón Gallardo" ~ vs_vfq - vs_grg))
}


# 2006 ----  
fun_margenes_2006 <- function(df) {
  df %>% 
    mutate(margen_victoria = case_when(primero == "Calderón" & segundo == "Madrazo" ~ vs_fch - vs_rmp,
                                       primero == "Calderón" & segundo == "AMLO" ~ vs_fch - vs_amlo,
                                       primero == "Calderón" & segundo == "Campa" ~ vs_fch - vs_rcc,
                                       primero == "Calderón" & segundo == "Mercado" ~ vs_fch - vs_pmc))
}


# 2012 ----  
fun_margenes_2012 <- function(df) {
  df %>% 
    mutate(margen_victoria = case_when(primero == "Peña Nieto" & segundo == "Vázquez Mota" ~ vs_epn - vs_jvm,
                                       primero == "Peña Nieto" & segundo == "AMLO" ~ vs_epn - vs_amlo,
                                       primero == "Peña Nieto" & segundo == "Quadri" ~ vs_epn - vs_gq))
}

# 2018 ----
fun_margenes_2018 <- function(df) {
  df %>% 
    mutate(margen_victoria = case_when(primero == "AMLO" & segundo == "Anaya" ~ vs_amlo - vs_anaya,
                                       primero == "AMLO" & segundo == "Meade" ~ vs_amlo - vs_meade,
                                       primero == "AMLO" & segundo == "El Bronco" ~ vs_amlo - vs_bronco))
}



### Generar variables que midan el margen de victoria del candidato ganador sobre el segundo lugar en cada casilla, sección, mpo, dtto y edo ----

# 1994 ----
bd_1994_casilla <- fun_margenes_1994(bd_1994_casilla)
bd_1994_seccion <- fun_margenes_1994(bd_1994_seccion)
bd_1994_dtto <- fun_margenes_1994(bd_1994_dtto)
bd_1994_mpo <- fun_margenes_1994(bd_1994_mpo)
bd_1994_edo <- fun_margenes_1994(bd_1994_edo)

# 2000 ----
bd_2000_casilla <- fun_margenes_2000(bd_2000_casilla)
bd_2000_seccion <- fun_margenes_2000(bd_2000_seccion)
bd_2000_dtto <- fun_margenes_2000(bd_2000_dtto)
bd_2000_mpo <- fun_margenes_2000(bd_2000_mpo)
bd_2000_edo <- fun_margenes_2000(bd_2000_edo)

# 2006 ----
bd_2006_casilla <- fun_margenes_2006(bd_2006_casilla)
bd_2006_seccion <- fun_margenes_2006(bd_2006_seccion)
bd_2006_dtto <- fun_margenes_2006(bd_2006_dtto)
bd_2006_mpo <- fun_margenes_2006(bd_2006_mpo)
bd_2006_edo <- fun_margenes_2006(bd_2006_edo)

# 2012 ----
bd_2012_casilla <- fun_margenes_2012(bd_2012_casilla)
bd_2012_seccion <- fun_margenes_2012(bd_2012_seccion)
bd_2012_dtto <- fun_margenes_2012(bd_2012_dtto)
bd_2012_mpo <- fun_margenes_2012(bd_2012_mpo)
bd_2012_edo <- fun_margenes_2012(bd_2012_edo)

# 2018 ----
cd_2018_casilla <- fun_margenes_2018(cd_2018_casilla)
cd_2018_seccion <- fun_margenes_2018(cd_2018_seccion)
cd_2018_dtto <- fun_margenes_2018(cd_2018_dtto)
cd_2018_mpo <- fun_margenes_2018(cd_2018_mpo)
cd_2018_edo <- fun_margenes_2018(cd_2018_edo)


### Cálculo de casillas, secciones, mpos, dttos y edos ganados por cada candidato ----

# Casillas ----
por_casilla_ganadas_1994 <- 
  bd_1994_casilla %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 1994,
         unidad = "Casillas")

por_casilla_ganadas_2000 <- 
  bd_2000_casilla %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2000,
         unidad = "Casillas")

por_casilla_ganadas_2006 <- 
  bd_2006_casilla %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2006,
         unidad = "Casillas")

por_casilla_ganadas_2012 <- 
  bd_2012_casilla %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2012,
         unidad = "Casillas")

por_casilla_ganadas_2018 <- 
  cd_2018_casilla %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2018,
         unidad = "Casillas")

# Secciones ----
por_seccion_ganadas_1994 <- 
  bd_1994_seccion %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 1994,
         unidad = "Secciones")

por_seccion_ganadas_2000 <- 
  bd_2000_seccion %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2000,
         unidad = "Secciones")

por_seccion_ganadas_2006 <- 
  bd_2006_seccion %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2006,
         unidad = "Secciones")

por_seccion_ganadas_2012 <- 
  bd_2012_seccion %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2012,
         unidad = "Secciones")

por_seccion_ganadas_2018 <- 
  cd_2018_seccion %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2018,
         unidad = "Secciones")

# Municipios ----
por_mpo_ganadas_1994 <- 
  bd_1994_mpo %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 1994,
         unidad = "Municipios")

por_mpo_ganadas_2000 <- 
  bd_2000_mpo %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2000,
         unidad = "Municipios")

por_mpo_ganadas_2006 <- 
  bd_2006_mpo %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2006,
         unidad = "Municipios")

por_mpo_ganadas_2012 <- 
  bd_2012_mpo %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2012,
         unidad = "Municipios")

por_mpo_ganadas_2018 <- 
  cd_2018_mpo %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2018,
         unidad = "Municipios")

# Distrito ----
por_dtto_ganadas_1994 <- 
  bd_1994_dtto %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 1994,
         unidad = "Distritos")

por_dtto_ganadas_2000 <- 
  bd_2000_dtto %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2000,
         unidad = "Distritos")

por_dtto_ganadas_2006 <- 
  bd_2006_dtto %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2006,
         unidad = "Distritos")

por_dtto_ganadas_2012 <- 
  bd_2012_dtto %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2012,
         unidad = "Distritos")

por_dtto_ganadas_2018 <- 
  cd_2018_dtto %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2018,
         unidad = "Distritos")

# Estado ----
por_edo_ganadas_1994 <- 
  bd_1994_edo %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 1994,
         unidad = "Estados")

por_edo_ganadas_2000 <- 
  bd_2000_edo %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2000,
         unidad = "Estados")

por_edo_ganadas_2006 <- 
  bd_2006_edo %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2006,
         unidad = "Estados")

por_edo_ganadas_2012 <- 
  bd_2012_edo %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2012,
         unidad = "Estados")

por_edo_ganadas_2018 <- 
  cd_2018_edo %>% 
  count(primero, sort = T) %>% 
  mutate(por = (n/sum(n))*100,
         ranking = rank(-por),
         año = 2018,
         unidad = "Estados")

### Unir dataframes con # y % de casillas ganadas por c/candidato en c/elección ----
bd_por_candidatos <- 
  bind_rows(por_casilla_ganadas_1994, por_casilla_ganadas_2000, por_casilla_ganadas_2006, por_casilla_ganadas_2012, por_casilla_ganadas_2018, por_seccion_ganadas_1994, por_seccion_ganadas_2000, por_seccion_ganadas_2006, por_seccion_ganadas_2012, por_seccion_ganadas_2018, por_mpo_ganadas_1994, por_mpo_ganadas_2000, por_mpo_ganadas_2006, por_mpo_ganadas_2012, por_mpo_ganadas_2018, por_dtto_ganadas_1994, por_dtto_ganadas_2000, por_dtto_ganadas_2006, por_dtto_ganadas_2012, por_dtto_ganadas_2018, por_edo_ganadas_1994, por_edo_ganadas_2000, por_edo_ganadas_2006, por_edo_ganadas_2012, por_edo_ganadas_2018)
