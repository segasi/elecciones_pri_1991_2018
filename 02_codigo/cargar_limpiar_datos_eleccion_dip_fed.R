### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/cargar_paquetes_setup_tema_grafica.R")

### Cargar datos de los resultados de la elección de diputados federales de mayoría relativa a nivel distrital, 1991-2018 ----

# Cargar datos de las elecciones de 1991 a 2015 ----

# Las bases de datos para 1991 a 2015 fueron obtenidas del Sistema de Consulta de la Estadística de las Elecciones Federales 2014-2015 (http://siceef.ine.mx/atlas.html). Las opciones en los campos de consulta fueron: (i) Tipo de elección = Diputados MR; (ii) Año de la elección = 1991 | 1994 | ... | 2012 | 2015; (iii) Entidad: Nacional por distrito

# 1991 ----
df_dtto_1991 <-  
  read_csv("01_bd/diputaciones_mr/dip_fed_mr_dtto_1991.csv") %>%
  clean_names() %>% 
  select(-distrito_4) %>% 
  rename(distrito = distrito_3) %>% 
  mutate(cve_edo = str_pad(estado, 2, pad = "0"),
         cve_dtto = str_pad(distrito, 2, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = "")) %>% 
  select(cve_edo_dtto, cve_edo, estado, nombre_estado, cve_dtto, distrito, everything()) %>% 
  filter(!is.na(pan)) # Filtrar renglón correspondiente al dtto. 5 de Coahuila, cuya elección fue anulada y se llevó a cabo una extraordinaria en 1992

# Datos de la elección extraordinaria en Coahuila en 1992
df_dtto_1992_extra_coah <-  
  read_csv("01_bd/diputaciones_mr/dip_fed_mr_dtto_1992_extra_coah.csv") %>%
  clean_names() %>% 
  select(-distrito_4) %>% 
  rename(distrito = distrito_3) %>% 
  mutate(cve_edo = str_pad(estado, 2, pad = "0"),
         cve_dtto = str_pad(distrito, 2, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = "")) %>% 
  select(cve_edo_dtto, cve_edo, estado, nombre_estado, cve_dtto, distrito, everything())


# Unir datos
df_dtto_1991 <- rbind(df_dtto_1991, df_dtto_1992_extra_coah)

# 1994 ----
df_dtto_1994 <-  
  read_csv("01_bd/diputaciones_mr/dip_fed_mr_dtto_1994.csv") %>%
  clean_names() %>% 
  select(-distrito_4) %>% 
  rename(distrito = distrito_3) %>% 
  mutate(cve_edo = str_pad(estado, 2, pad = "0"),
         cve_dtto = str_pad(distrito, 2, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = "")) %>% 
  select(cve_edo_dtto, cve_edo, estado, nombre_estado, cve_dtto, distrito, everything()) %>% 
  filter(!is.na(pan)) # Filtrar renglón correspondiente a los dttos. 9 de Puebla y 22 de Veracruz, cuya elección fue anulada y se llevó a cabo una extraordinaria en 1995

# Datos de la elección extraordinaria en Puebla en 1995
df_dtto_1995_extra_pue <-  
  read_csv("01_bd/diputaciones_mr/dip_fed_mr_dtto_1995_extra_pue.csv") %>%
  clean_names() %>% 
  select(-distrito_4) %>% 
  rename(distrito = distrito_3) %>% 
  mutate(cve_edo = str_pad(estado, 2, pad = "0"),
         cve_dtto = str_pad(distrito, 2, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = "")) %>% 
  select(cve_edo_dtto, cve_edo, estado, nombre_estado, cve_dtto, distrito, everything())

# Datos de la elección extraordinaria en Veracruz en 1995
df_dtto_1995_extra_ver <-  
  read_csv("01_bd/diputaciones_mr/dip_fed_mr_dtto_1995_extra_ver.csv") %>%
  clean_names() %>% 
  select(-distrito_4) %>% 
  rename(distrito = distrito_3) %>% 
  mutate(cve_edo = str_pad(estado, 2, pad = "0"),
         cve_dtto = str_pad(distrito, 2, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = "")) %>% 
  select(cve_edo_dtto, cve_edo, estado, nombre_estado, cve_dtto, distrito, everything())


# Unir datos
df_dtto_1994 <- rbind(df_dtto_1994, df_dtto_1995_extra_pue, df_dtto_1995_extra_ver)


# 1997 ----
df_dtto_1997 <-  
  read_csv("01_bd/diputaciones_mr/dip_fed_mr_dtto_1997.csv") %>%
  clean_names() %>% 
  mutate(cve_edo = str_pad(estado, 2, pad = "0"),
         cve_dtto = str_pad(distrito, 2, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = "")) %>% 
  select(cve_edo_dtto, cve_edo, estado, nombre_estado, cve_dtto, distrito, everything())

# 2000 ----
df_dtto_2000 <-  
  read_csv("01_bd/diputaciones_mr/dip_fed_mr_dtto_2000.csv") %>%
  clean_names() %>% 
  mutate(cve_edo = str_pad(estado, 2, pad = "0"),
         cve_dtto = str_pad(distrito, 2, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = "")) %>% 
  select(cve_edo_dtto, cve_edo, estado, nombre_estado, cve_dtto, distrito, everything())

# 2003 ----
df_dtto_2003 <-  
  read_csv("01_bd/diputaciones_mr/dip_fed_mr_dtto_2003.csv") %>%
  clean_names() %>% 
  mutate(cve_edo = str_pad(estado, 2, pad = "0"),
         cve_dtto = str_pad(distrito, 2, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = "")) %>% 
  select(cve_edo_dtto, cve_edo, estado, nombre_estado, cve_dtto, distrito, everything())

# Datos de la elección extraordinaria en Coahuila en 2003
df_dtto_2003_extra_coah <-  
  read_csv("01_bd/diputaciones_mr/dip_fed_mr_dtto_2003_extra_coah.csv") %>%
  clean_names() %>% 
  mutate(cve_edo = str_pad(estado, 2, pad = "0"),
         cve_dtto = str_pad(distrito, 2, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = ""),
         pri = NA,
         pvem = NA,
         psn = NA,
         pas = NA,
         mp = NA,
         fc = NA) %>% 
  select(cve_edo_dtto, cve_edo, estado, nombre_estado, cve_dtto, distrito, everything())

# Datos de la elección extraordinaria en Michoacán en 2003
df_dtto_2003_extra_mich <-  
  read_csv("01_bd/diputaciones_mr/dip_fed_mr_dtto_2003_extra_mich.csv") %>%
  clean_names() %>% 
  mutate(cve_edo = str_pad(estado, 2, pad = "0"),
         cve_dtto = str_pad(distrito, 2, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = ""),
         pri = NA,
         pvem = NA,
         psn = NA,
         pas = NA,
         mp = NA,
         fc = NA) %>% 
  select(cve_edo_dtto, cve_edo, estado, nombre_estado, cve_dtto, distrito, everything())

# Unir datos
df_dtto_2003 <- rbind(df_dtto_2003, df_dtto_2003_extra_coah, df_dtto_2003_extra_mich)

# 2006 ----
df_dtto_2006 <-  
  read_csv("01_bd/diputaciones_mr/dip_fed_mr_dtto_2006.csv") %>%
  clean_names() %>% 
  mutate(cve_edo = str_pad(estado, 2, pad = "0"),
         cve_dtto = str_pad(distrito, 2, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = "")) %>% 
  select(cve_edo_dtto, cve_edo, estado, nombre_estado, cve_dtto, distrito, everything())

# 2009 ----
df_dtto_2009 <-  
  read_csv("01_bd/diputaciones_mr/dip_fed_mr_dtto_2009.csv") %>% 
  clean_names() %>% 
  mutate(cve_edo = str_pad(estado, 2, pad = "0"),
         cve_dtto = str_pad(distrito, 2, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = "")) %>% 
  select(cve_edo_dtto, cve_edo, estado, nombre_estado, cve_dtto, distrito, everything())

# 2012 ----
df_dtto_2012 <- 
  read_csv("01_bd/diputaciones_mr/dip_fed_mr_dtto_2012.csv") %>% 
  clean_names() %>% 
  mutate(cve_edo = str_pad(estado, 2, pad = "0"),
         cve_dtto = str_pad(distrito, 2, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = "")) %>% 
  select(cve_edo_dtto, cve_edo, estado, nombre_estado, cve_dtto, distrito, everything())

# 2015 ----
df_dtto_2015 <- 
  read_csv("01_bd/diputaciones_mr/dip_fed_mr_dtto_2015.csv") %>% 
  clean_names() %>% 
  mutate(cve_edo = str_pad(estado, 2, pad = "0"),
         cve_dtto = str_pad(distrito, 2, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = "")) %>% 
  select(cve_edo_dtto, cve_edo, estado, nombre_estado, cve_dtto, distrito, everything())

# 2018 ----
# La base de datos para 2018 fue obtenida de https://www.ine.mx/wp-content/uploads/2018/09/Tabla-DIPUTADOS-para-entregar-a-DEPPP-2018-08-20.xlsx

cd_df_dtto_2018 <- 
  read_xlsx("01_bd/diputaciones_mr/Tabla-DIPUTADOS-para-entregar-a-DEPPP-2018-08-20.xlsx", 
            range = "a3:p303", 
            sheet = "Diputados MR") %>% 
  clean_names() %>% 
  rename(nombre_estado = entidad_federativa)

# Generar data frame que contenga los nombres de las entidades y sus claves; para ello uso los datos de 2015
datos_cve_edos <-  
  df_dtto_2015 %>% 
  count(nombre_estado, estado) %>% 
  select(-n)

# Generar columnas que contengan claves de edo, dtto y edo_dtto 
cd_df_dtto_2018 <- 
  cd_df_dtto_2018 %>% 
  left_join(datos_cve_edos, by = "nombre_estado") %>% 
  select(estado, everything())  %>% 
  mutate(cve_edo = str_pad(estado, 2, pad = "0"),
         cve_dtto = str_pad(distrito, 2, pad = "0"),
         cve_edo_dtto = paste(cve_edo, cve_dtto, sep = "")) %>% 
  select(cve_edo_dtto, cve_edo, estado, nombre_estado, cve_dtto, distrito, everything())


### Redistribución de los votos obtenidos en la boleta por los dos o más partidos que integran una coalición, entre los diferentes partidos que la constituyen ----

# Este ajuste es necesario para las elecciones de 2009 a 2015 por la reforma electoral de 2008. Para 2018 no hace falta porque la base de datos utilizada ya considera esta redistribución.

# En este proceso seguimos las reglas establecidas en el artículo 311, inciso c) de la LEGIPE: "En su caso, se sumarán los votos que hayan sido emitidos a favor de dos o más partidos coaligados y que por esa causa hayan sido consignados por separado en el apartado correspondiente del acta de escrutinio y cómputo de casilla. La suma distrital de tales votos se distribuirá igualitariamente entre los partidos que integran la coalición; de existir fracción, los votos correspondientes se asignarán a los partidos de más alta votación"

# Nuestros resultados no son exactamente iguales a los del INE, porque sólo calculamos el cociente y no asignamos los votos restantes al partido con más alta votación. Si hay una diferencia, nuestros cálculos siempre son menores a los del INE y esta diferencia no es mayor a 10 votos.

# 2009 ----
df_dtto_2009 <- 
  df_dtto_2009 %>% 
  # Reemplazar 0s por NAs en las variables que registran votos por dos o más partidos
  mutate(across(.cols = primero_mexico:salvemos_mexico,
                .fns = ~ ifelse(test = is.na(.x), 
                                yes = 0, 
                                no = .x))) %>% 
  # Redistribuir votos obtenidos en boletas donde los electores eligieron más de una opción
  mutate(pri = pri + primero_mexico %/% 2,
         pvem = pvem + primero_mexico %/% 2,
         pt = pt + salvemos_mexico %/% 2,
         convergencia = convergencia + salvemos_mexico %/% 2) %>% 
  # Eliminar columnas con votos obtenidos individualmente o en coalición por partidos
  select(-c(primero_mexico, salvemos_mexico))


# 2012 ----
df_dtto_2012 <- 
  df_dtto_2012 %>% 
  # Reemplazar 0s por NAs en las variables que registran votos por dos o más partidos
  mutate(across(.cols = pri_pvem:pt_mc,
                .fns = ~ ifelse(test = is.na(.x), 
                                yes = 0, 
                                no = .x))) %>% 
  # Redistribuir votos obtenidos en boletas donde los electores eligieron más de una opción
  mutate(pri = pri + pri_pvem %/% 2,
         prd = prd + prd_pt_mc %/% 3 + prd_pt %/% 2 + prd_mc %/% 2,
         pvem = pvem + pri_pvem %/% 2,
         pt = pt + prd_pt_mc %/% 3 + prd_pt %/% 2 + pt_mc %/% 2,
         movimiento_ciudadano = movimiento_ciudadano + prd_pt_mc %/% 3 + prd_mc %/% 2  + pt_mc %/% 2) %>% 
  # Eliminar columnas con votos obtenidos individualmente o en coalición por partidos
  select(-c(pri_pvem:pt_mc)) 


# 2015 ----
df_dtto_2015 <- 
  df_dtto_2015 %>% 
  # Reemplazar 0s por NAs en las variables que registran votos por dos o más partidos
  mutate(across(.cols = pan_nueva_alianza:prd_pt,
                .fns = ~ ifelse(test = is.na(.x), 
                                yes = 0, 
                                no = .x))) %>%
  # Redistribuir votos obtenidos en boletas donde los electores eligieron más de una opción
  mutate(pan = pan + pan_nueva_alianza %/% 2,
         pri = pri + pri_pvem %/% 2,
         prd = prd + prd_pt %/% 2,
         pvem = pvem + pri_pvem %/% 2,
         pt = pt + prd_pt %/% 2,
         nueva_alianza = nueva_alianza + pan_nueva_alianza %/% 2) %>% 
  # Eliminar columnas con votos obtenidos individualmente o en coalición por partidos
  select(-c(pan_nueva_alianza:prd_pt))



### Generar variables con el % de votos obtenidos por cada partido o coalición en los 300 distritos, 1991 a 2018 ----

# 1991
df_dtto_1991 <- 
  df_dtto_1991 %>%
  mutate(across(.cols = pan:pt,
                .fns = ~ round((.x/total_votos)*100, 2), 
                .names = 'vs_{col}'))

# 1994
df_dtto_1994 <- 
  df_dtto_1994 %>%
  mutate(across(.cols = pan:pvem,
                .fns = ~ round((.x/total_votos)*100, 2), 
                .names = 'vs_{col}')) 

# 1997
df_dtto_1997 <- 
  df_dtto_1997 %>%
  mutate(across(.cols = pan:pdm,
                .fns = ~ round((.x/total_votos)*100, 2), 
                .names = 'vs_{col}'))

# 2000
df_dtto_2000 <- 
  df_dtto_2000 %>%
  mutate(across(.cols = ac:dsppn,
                .fns = ~ round((.x/total_votos)*100, 2), 
                .names = 'vs_{col}'))

# 2003
df_dtto_2003 <- 
  df_dtto_2003 %>%
  mutate(across(.cols = pan:apt,
                .fns = ~ round((.x/total_votos)*100, 2), 
                .names = 'vs_{col}')) 

# 2006 
df_dtto_2006 <- 
  df_dtto_2006 %>%
  mutate(across(.cols = pan:asdc,
                .fns = ~ round((.x/total_votos)*100, 2), 
                .names = 'vs_{col}'))

# 2009 
df_dtto_2009 <- 
  df_dtto_2009 %>%
  mutate(across(.cols = pan:psd,
                .fns = ~ round((.x/total_votos)*100, 2), 
                .names = 'vs_{col}')) 

# 2012 
df_dtto_2012 <- 
  df_dtto_2012 %>%
  mutate(across(.cols = pan:nueva_alianza,
                .fns = ~ round((.x/total_votos)*100, 2), 
                .names = 'vs_{col}'))

# 2015 
df_dtto_2015 <- 
  df_dtto_2015 %>% 
  mutate(across(.cols = pan:candidato_independiente_2,
                .fns = ~ round((.x/total_votos)*100, 2), 
                .names = 'vs_{col}'))
# 2018
cd_df_dtto_2018 <- 
  cd_df_dtto_2018 %>% 
  mutate(across(.cols = pan:cand_ind_2,
                .fns = ~ round((.x/total_votos)*100, 2), 
                .names = 'vs_{col}'))


### Generar variables que ranken la posición (dado el número de votos) de cada partido en los 300 distritos, 1991 a 2018 ----

# 1991
df_dtto_1991 <- 
  as_tibble(data.frame(df_dtto_1991, t(apply(-df_dtto_1991[, 10:19], 1, rank, ties.method='min')))) %>%
  rename_at(.vars = vars(ends_with(".1")), 
            .funs = list(~ str_replace(., ".1", "_ranking"))) %>%
  mutate(pp_ganador = case_when(pan_ranking == 1 ~ "PAN",
                                pri_ranking == 1 ~ "PRI",
                                pps_ranking == 1 ~ "PPS",
                                prd_ranking == 1 ~ "PRD",
                                pfcrn_ranking == 1 ~ "PFCRN",
                                parm_ranking == 1 ~ "PARM",
                                pdm_ranking == 1 ~ "PDM",
                                prt_ranking == 1 ~ "PRT",
                                pem_ranking == 1 ~ "PEM",
                                pt_ranking == 1 ~ "PT"))

# 1994
df_dtto_1994 <- 
  as_tibble(data.frame(df_dtto_1994, t(apply(-df_dtto_1994[, 10:18], 1, rank, ties.method='min')))) %>%
  rename_at(.vars = vars(ends_with(".1")), 
            .funs = list(~ str_replace(., ".1", "_ranking"))) %>%
  mutate(pp_ganador = case_when(pan_ranking == 1 ~ "PAN",
                                pri_ranking == 1 ~ "PRI",
                                pps_ranking == 1 ~ "PPS",
                                prd_ranking == 1 ~ "PRD",
                                pfcrn_ranking == 1 ~ "PFCRN",
                                parm_ranking == 1 ~ "PARM",
                                uno_pdm_ranking == 1 ~ "PDM",
                                pvem_ranking == 1 ~ "PVEM",
                                pt_ranking == 1 ~ "PT")) 


# 1997
df_dtto_1997 <- 
  as_tibble(data.frame(df_dtto_1997, t(apply(-df_dtto_1997[, 10:17], 1, rank, ties.method='min')))) %>%
  rename_at(.vars = vars(ends_with(".1")), 
            .funs = list(~ str_replace(., ".1", "_ranking"))) %>%
  mutate(pp_ganador = case_when(pan_ranking == 1 ~ "PAN",
                                pri_ranking == 1 ~ "PRI",
                                prd_ranking == 1 ~ "PRD",
                                pc_ranking == 1 ~ "PC",
                                pvem_ranking == 1 ~ "PVEM",
                                pt_ranking == 1 ~ "PT",
                                pps_ranking == 1 ~ "PPS",
                                pdm_ranking == 1 ~ "PDM"))


# 2000
df_dtto_2000 <- 
  as_tibble(data.frame(df_dtto_2000, t(apply(-df_dtto_2000[, 10:15], 1, rank, ties.method='min')))) %>%
  rename_at(.vars = vars(ends_with(".1")), 
            .funs = list(~ str_replace(., ".1", "_ranking"))) %>%
  mutate(pp_ganador = case_when(ac_ranking == 1 ~ "Alianza por el cambio",
                                pri_ranking == 1 ~ "PRI",
                                am_ranking == 1 ~ "Alianza por México",
                                pcd_ranking == 1 ~ "PCD",
                                parm_ranking == 1 ~ "PARM",
                                dsppn_ranking == 1 ~ "Democracia Social")) 

# 2003
df_dtto_2003 <- 
  as_tibble(data.frame(df_dtto_2003, t(apply(-df_dtto_2003[, 10:21], 1, rank, ties.method='min')))) %>%
  rename_at(.vars = vars(ends_with(".1")), 
            .funs = list(~ str_replace(., ".1", "_ranking"))) %>%
  mutate(pp_ganador = case_when(pan_ranking == 1 ~ "PAN",
                                pri_ranking == 1 ~ "PRI",
                                prd_ranking == 1 ~ "PRD",
                                pvem_ranking == 1 ~ "PVEM",
                                pt_ranking == 1 ~ "PT",
                                convergencia_ranking == 1 ~ "Convergencia",
                                psn_ranking == 1 ~ "PSN",
                                pas_ranking == 1 ~ "PAS",
                                mp_ranking == 1 ~ "México Posible",
                                plm_ranking == 1 ~ "PLM",
                                fc_ranking == 1 ~ "Fuerza Ciudadana",
                                apt_ranking == 1 ~ "Alianza para todos"))


# 2006 
df_dtto_2006 <- 
  as_tibble(data.frame(df_dtto_2006, t(apply(-df_dtto_2006[, 10:14], 1, rank, ties.method='min')))) %>%
  rename_at(.vars = vars(ends_with(".1")), 
            .funs = list(~ str_replace(., ".1", "_ranking"))) %>%
  mutate(pp_ganador = case_when(pan_ranking == 1 ~ "PAN",
                                apm_ranking == 1 ~ "Alianza por México",
                                pbt_ranking == 1 ~ "Por el bien de todos",
                                nueva_alianza_ranking == 1 ~ "PANAL",
                                asdc_ranking == 1 ~ "PASDC"))


# 2009 
df_dtto_2009 <- 
  as_tibble(data.frame(df_dtto_2009, t(apply(-df_dtto_2009[, 10:17], 1, rank, ties.method='min')))) %>% 
  rename_at(.vars = vars(ends_with(".1")), 
            .funs = list(~ str_replace(., ".1", "_ranking"))) %>%
  mutate(pp_ganador = case_when(pan_ranking == 1 ~ "PAN",
                                pri_ranking == 1 ~ "PRI",
                                prd_ranking == 1 ~ "PRD",
                                pvem_ranking == 1 ~ "PVEM",
                                pt_ranking == 1 ~ "PT",
                                convergencia_ranking == 1 ~ "Convergencia",
                                nueva_alianza_ranking == 1 ~ "PANAL",
                                psd_ranking == 1 ~ "PSD"))

# 2012 
df_dtto_2012 <- 
  as_tibble(data.frame(df_dtto_2012, t(apply(-df_dtto_2012[, 10:16], 1, rank, ties.method='min')))) %>% 
  rename_at(.vars = vars(ends_with(".1")), 
          .funs = list(~ str_replace(., ".1", "_ranking"))) %>%
  mutate(pp_ganador = case_when(pan_ranking == 1 ~ "PAN",
                                pri_ranking == 1 ~ "PRI",
                                prd_ranking == 1 ~ "PRD",
                                pvem_ranking == 1 ~ "PVEM",
                                pt_ranking == 1 ~ "PT",
                                movimiento_ciudadano_ranking == 1 ~ "Mov. Ciudadano",
                                nueva_alianza_ranking == 1 ~ "PANAL"))

# 2015 
df_dtto_2015 <- 
  as_tibble(data.frame(df_dtto_2015, t(apply(-df_dtto_2015[, 10:21], 1, rank, ties.method='min')))) %>% 
  rename_at(.vars = vars(ends_with(".1")), 
            .funs = list(~ str_replace(., ".1", "_ranking"))) %>%
  mutate(pp_ganador = case_when(pan_ranking == 1 ~ "PAN",
                                pri_ranking == 1 ~ "PRI",
                                prd_ranking == 1 ~ "PRD",
                                pvem_ranking == 1 ~ "PVEM",
                                pt_ranking == 1 ~ "PT",
                                movimiento_ciudadano_ranking == 1 ~ "Mov. Ciudadano",
                                nueva_alianza_ranking == 1 ~ "PANAL",
                                partido_humanista_ranking == 1 ~ "PH",
                                encuentro_social_ranking == 1 ~ "PES",
                                morena_ranking == 1 ~ "Morena", 
                                candidato_independiente_ranking.1 == 1 | candidato_independiente_2_ranking == 1 ~ "Candidato independiente"))


# 2018
cd_df_dtto_2018 <- 
  as_tibble(data.frame(cd_df_dtto_2018, t(apply(-cd_df_dtto_2018[, 7:17], 1, rank, ties.method='min')))) %>% 
  rename_at(.vars = vars(ends_with(".1")), 
            .funs = list(~ str_replace(., ".1", "_ranking"))) %>%
  mutate(pp_ganador = case_when(pan_ranking == 1 ~ "PAN",
                                pri_ranking == 1 ~ "PRI",
                                prd_ranking == 1 ~ "PRD",
                                pvem_ranking == 1 ~ "PVEM",
                                pt_ranking == 1 ~ "PT",
                                mc_ranking == 1 ~ "Mov. Ciudadano",
                                na_ranking == 1 ~ "PANAL",
                                morena_ranking == 1 ~ "Morena", 
                                es_ranking == 1 ~ "PES",
                                cand_ind_ranking.1 == 1 | cand_ind_2_ranking == 1 ~ "Candidato independiente")) 


### Generar variable con el margen de victoria del partido o coalición ganador de cada distrito sobre el segundo lugar ----

# 1991
df_dtto_1991 <- 
  df_dtto_1991 %>% 
  mutate(margen_victoria = case_when(pp_ganador == "PRI" & pan_ranking == 2 ~ vs_pri - vs_pan,
                                     pp_ganador == "PRI" & prd_ranking == 2 ~ vs_pri - vs_prd,
                                     pp_ganador == "PRI" & parm_ranking == 2 ~ vs_pri - vs_parm,
                                     pp_ganador == "PRI" & pfcrn_ranking == 2 ~ vs_pri - vs_pfcrn,
                                     pp_ganador == "PRI" & pps_ranking == 2 ~ vs_pri - vs_pps,
                                     pp_ganador == "PRI" & pt_ranking == 2 ~ vs_pri - vs_pt,
                                     pp_ganador == "PAN" & pri_ranking == 2 ~ vs_pan - vs_pri,
                                     pp_ganador == "PAN" & prd_ranking == 2 ~ vs_pan - vs_prd))

# 1994
df_dtto_1994 <- 
  df_dtto_1994 %>% 
  mutate(margen_victoria = case_when(pp_ganador == "PRI" & pan_ranking == 2 ~ vs_pri - vs_pan,
                                     pp_ganador == "PRI" & prd_ranking == 2 ~ vs_pri - vs_prd,
                                     pp_ganador == "PRI" & parm_ranking == 2 ~ vs_pri - vs_parm,
                                     pp_ganador == "PRI" & pfcrn_ranking == 2 ~ vs_pri - vs_pfcrn,
                                     pp_ganador == "PRI" & pps_ranking == 2 ~ vs_pri - vs_pps,
                                     pp_ganador == "PRI" & pt_ranking == 2 ~ vs_pri - vs_pt,
                                     pp_ganador == "PAN" & pri_ranking == 2 ~ vs_pan - vs_pri,
                                     pp_ganador == "PAN" & prd_ranking == 2 ~ vs_pan - vs_prd,
                                     pp_ganador == "PRD" & pri_ranking == 2 ~ vs_prd - vs_pri))


# 1997
df_dtto_1997 <- 
  df_dtto_1997 %>% 
  mutate(margen_victoria = case_when(pp_ganador == "PRI" & pan_ranking == 2 ~ vs_pri - vs_pan,
                                     pp_ganador == "PRI" & prd_ranking == 2 ~ vs_pri - vs_prd,
                                     pp_ganador == "PRI" & pt_ranking == 2 ~ vs_pri - vs_pt,
                                     pp_ganador == "PAN" & pri_ranking == 2 ~ vs_pan - vs_pri,
                                     pp_ganador == "PAN" & prd_ranking == 2 ~ vs_pan - vs_prd,
                                     pp_ganador == "PRD" & pri_ranking == 2 ~ vs_prd - vs_pri,
                                     pp_ganador == "PRD" & pan_ranking == 2 ~ vs_prd - vs_pan,
                                     pp_ganador == "PT" & pri_ranking == 2 ~ vs_pt - vs_pri))


# 2000
df_dtto_2000 <- 
  df_dtto_2000 %>% 
  mutate(margen_victoria = case_when(pp_ganador == "PRI" & ac_ranking == 2 ~ vs_pri - vs_ac,
                                     pp_ganador == "PRI" & am_ranking == 2 ~ vs_pri - vs_am,
                                     pp_ganador == "Alianza por el cambio" & pri_ranking == 2 ~ vs_ac - vs_pri,
                                     pp_ganador == "Alianza por el cambio" & am_ranking == 2 ~ vs_ac - vs_am,
                                     pp_ganador == "Alianza por México" & pri_ranking == 2 ~ vs_am - vs_pri,
                                     pp_ganador == "Alianza por México" & ac_ranking == 2 ~ vs_am - vs_ac))


# 2003
df_dtto_2003 <- 
  df_dtto_2003 %>% 
  mutate(margen_victoria = case_when(pp_ganador == "PRI" & pan_ranking == 2 ~ vs_pri - vs_pan,
                                     pp_ganador == "PRI" & prd_ranking == 2 ~ vs_pri - vs_prd,
                                     pp_ganador == "PRI" & pt_ranking == 2 ~ vs_pri - vs_pt,
                                     pp_ganador == "PRI" & pas_ranking == 2 ~ vs_pri - vs_pas,
                                     pp_ganador == "Alianza para todos" & pan_ranking == 2 ~ vs_apt - vs_pan,
                                     pp_ganador == "Alianza para todos" & prd_ranking == 2 ~ vs_apt - vs_prd,
                                     pp_ganador == "Alianza para todos" & pt_ranking == 2 ~ vs_apt - vs_pt,
                                     pp_ganador == "PAN" & pri_ranking == 2 ~ vs_pan - vs_pri,
                                     pp_ganador == "PAN" & apt_ranking == 2 ~ vs_pan - vs_apt,
                                     pp_ganador == "PAN" & prd_ranking == 2 ~ vs_pan - vs_prd,
                                     pp_ganador == "PAN" & pvem_ranking == 2 ~ vs_pan - vs_pvem,
                                     pp_ganador == "PRD" & pri_ranking == 2 ~ vs_prd - vs_pri,
                                     pp_ganador == "PRD" & apt_ranking == 2 ~ vs_prd - vs_apt,
                                     pp_ganador == "PRD" & pan_ranking == 2 ~ vs_prd - vs_pan,
                                     pp_ganador == "PRD" & convergencia_ranking == 2 ~ vs_prd - vs_convergencia))

# 2006 
df_dtto_2006 <- 
  df_dtto_2006 %>% 
  mutate(margen_victoria = case_when(pp_ganador == "Alianza por México" & pan_ranking == 2 ~ vs_apm - vs_pan,
                                     pp_ganador == "Alianza por México" & pbt_ranking == 2 ~ vs_apm - vs_pbt,
                                     pp_ganador == "PAN" & apm_ranking == 2 ~ vs_pan - vs_apm,
                                     pp_ganador == "PAN" & pbt_ranking == 2 ~ vs_pan - vs_pbt,
                                     pp_ganador == "Por el bien de todos" & apm_ranking == 2 ~ vs_pbt - vs_apm,
                                     pp_ganador == "Por el bien de todos" & pan_ranking == 2 ~ vs_pbt - vs_pan))

# 2009 
df_dtto_2009 <- 
  df_dtto_2009 %>% 
  mutate(margen_victoria = case_when(pp_ganador == "PRI" & pan_ranking == 2 ~ vs_pri - vs_pan,
                                     pp_ganador == "PRI" & prd_ranking == 2 ~ vs_pri - vs_prd,
                                     pp_ganador == "PRI" & pt_ranking == 2 ~ vs_pri - vs_pt,
                                     pp_ganador == "PRI" & pvem_ranking == 2 ~ vs_pri - vs_pvem,
                                     pp_ganador == "PRI" & convergencia_ranking == 2 ~ vs_pri - vs_convergencia,
                                     pp_ganador == "PRI" & nueva_alianza_ranking == 2 ~ vs_pri - vs_nueva_alianza,
                                     pp_ganador == "PAN" & pri_ranking == 2 ~ vs_pan - vs_pri,
                                     pp_ganador == "PAN" & prd_ranking == 2 ~ vs_pan - vs_prd,
                                     pp_ganador == "PAN" & pvem_ranking == 2 ~ vs_pan - vs_pvem,
                                     pp_ganador == "PRD" & pri_ranking == 2 ~ vs_prd - vs_pri,
                                     pp_ganador == "PRD" & pan_ranking == 2 ~ vs_prd - vs_pan,
                                     pp_ganador == "PRD" & pt_ranking == 2 ~ vs_prd - vs_pt,
                                     pp_ganador == "PRD" & convergencia_ranking == 2 ~ vs_prd - vs_convergencia,
                                     pp_ganador == "PT" & prd_ranking == 2 ~ vs_pt - vs_prd))

# 2012 
df_dtto_2012 <- 
  df_dtto_2012 %>% 
  mutate(margen_victoria = case_when(pp_ganador == "PRI" & pan_ranking == 2 ~ vs_pri - vs_pan,
                                     pp_ganador == "PRI" & prd_ranking == 2 ~ vs_pri - vs_prd,
                                     pp_ganador == "PRI" & pt_ranking == 2 ~ vs_pri - vs_pt,
                                     pp_ganador == "PRI" & pvem_ranking == 2 ~ vs_pri - vs_pvem,
                                     pp_ganador == "PRI" & movimiento_ciudadano_ranking == 2 ~ vs_pri - vs_movimiento_ciudadano,
                                     pp_ganador == "PRI" & nueva_alianza_ranking == 2 ~ vs_pri - vs_nueva_alianza,
                                     pp_ganador == "PAN" & pri_ranking == 2 ~ vs_pan - vs_pri,
                                     pp_ganador == "PAN" & prd_ranking == 2 ~ vs_pan - vs_prd,
                                     pp_ganador == "PAN" & pvem_ranking == 2 ~ vs_pan - vs_pvem,
                                     pp_ganador == "PRD" & pri_ranking == 2 ~ vs_prd - vs_pri,
                                     pp_ganador == "PRD" & pan_ranking == 2 ~ vs_prd - vs_pan,
                                     pp_ganador == "PRD" & pt_ranking == 2 ~ vs_prd - vs_pt,
                                     pp_ganador == "PRD" & pvem_ranking == 2 ~ vs_prd - vs_pvem,
                                     pp_ganador == "PVEM" & pri_ranking == 2 ~ vs_pvem - vs_pri))

# 2015 
df_dtto_2015 <- 
  df_dtto_2015 %>% 
  mutate(margen_victoria = case_when(pp_ganador == "PRI" & pan_ranking == 2 ~ vs_pri - vs_pan,
                                     pp_ganador == "PRI" & prd_ranking == 2 ~ vs_pri - vs_prd,
                                     pp_ganador == "PRI" & pt_ranking == 2 ~ vs_pri - vs_pt,
                                     pp_ganador == "PRI" & pvem_ranking == 2 ~ vs_pri - vs_pvem,
                                     pp_ganador == "PRI" & movimiento_ciudadano_ranking == 2 ~ vs_pri - vs_movimiento_ciudadano,
                                     pp_ganador == "PRI" & nueva_alianza_ranking == 2 ~ vs_pri - vs_nueva_alianza,
                                     pp_ganador == "PRI" & morena_ranking == 2 ~ vs_pri - vs_morena,
                                     pp_ganador == "PRI" & encuentro_social_ranking == 2 ~ vs_pri - vs_encuentro_social,
                                     pp_ganador == "PRI" & candidato_independiente_ranking.1 == 2 ~ vs_pri - vs_candidato_independiente_1,
                                     pp_ganador == "PAN" & pri_ranking == 2 ~ vs_pan - vs_pri,
                                     pp_ganador == "PAN" & prd_ranking == 2 ~ vs_pan - vs_prd,
                                     pp_ganador == "PAN" & pvem_ranking == 2 ~ vs_pan - vs_pvem,
                                     pp_ganador == "PAN" & movimiento_ciudadano_ranking == 2 ~ vs_pan - vs_movimiento_ciudadano,
                                     pp_ganador == "PAN" & morena_ranking == 2 ~ vs_pan - vs_morena,
                                     pp_ganador == "PRD" & pri_ranking == 2 ~ vs_prd - vs_pri,
                                     pp_ganador == "PRD" & pan_ranking == 2 ~ vs_prd - vs_pan,
                                     pp_ganador == "PRD" & pt_ranking == 2 ~ vs_prd - vs_pt,
                                     pp_ganador == "PRD" & pvem_ranking == 2 ~ vs_prd - vs_pvem,
                                     pp_ganador == "PRD" & morena_ranking == 2 ~ vs_prd - vs_morena,
                                     pp_ganador == "PVEM" & pri_ranking == 2 ~ vs_pvem - vs_pri,
                                     pp_ganador == "Mov. Ciudadano" & pri_ranking == 2 ~ vs_movimiento_ciudadano - vs_pri,
                                     pp_ganador == "Mov. Ciudadano" & pan_ranking == 2 ~ vs_movimiento_ciudadano - vs_pan,
                                     pp_ganador == "PANAL" & pri_ranking == 2 ~ vs_nueva_alianza - vs_pri,
                                     pp_ganador == "Morena" & pan_ranking == 2 ~ vs_morena - vs_pan,
                                     pp_ganador == "Morena" & pri_ranking == 2 ~ vs_morena - vs_pri,
                                     pp_ganador == "Morena" & prd_ranking == 2 ~ vs_morena - vs_prd,
                                     pp_ganador == "Morena" & pvem_ranking == 2 ~ vs_morena - vs_pvem,
                                     pp_ganador == "Morena" & movimiento_ciudadano_ranking == 2 ~ vs_morena - vs_movimiento_ciudadano,
                                     pp_ganador == "Morena" & candidato_independiente_ranking.1 == 2 ~ vs_morena - vs_candidato_independiente_1,
                                     pp_ganador == "Candidato independiente" & pri_ranking == 2 ~ vs_candidato_independiente_1 - vs_pri))

# 2018
cd_df_dtto_2018 <- 
  cd_df_dtto_2018 %>% 
  mutate(margen_victoria = case_when(pp_ganador == "Morena" & pan_ranking == 2 ~ vs_morena - vs_pan,
                                     pp_ganador == "Morena" & pri_ranking == 2 ~ vs_morena - vs_pri,
                                     pp_ganador == "Morena" & prd_ranking == 2 ~ vs_morena - vs_prd,
                                     pp_ganador == "Morena" & pvem_ranking == 2 ~ vs_morena - vs_pvem,
                                     pp_ganador == "Morena" & mc_ranking == 2 ~ vs_morena - vs_mc,
                                     pp_ganador == "Morena" & cand_ind_ranking.1 == 2 ~ vs_morena - vs_cand_ind_1,
                                     pp_ganador == "PRI" & morena_ranking == 2 ~ vs_pri - vs_morena,
                                     pp_ganador == "PRI" & pan_ranking == 2 ~ vs_pri - vs_pan,
                                     pp_ganador == "PAN" & pri_ranking == 2 ~ vs_pan - vs_pri,
                                     pp_ganador == "PAN" & morena_ranking == 2 ~ vs_pan - vs_morena,
                                     pp_ganador == "PAN" & mc_ranking == 2 ~ vs_pan - vs_mc,
                                     pp_ganador == "PRD" & morena_ranking == 2 ~ vs_pan - vs_prd,
                                     pp_ganador == "Mov. Ciudadano" & morena_ranking == 2 ~ vs_pan - vs_mc,
                                     pp_ganador == "PVEM" & morena_ranking == 2 ~ vs_pvem - vs_morena,
                                     pp_ganador == "Candidato independiente" & morena_ranking == 2 ~ vs_cand_ind_1 - vs_morena,
                                     pp_ganador == "Candidato independiente" & pan_ranking == 2 ~ vs_cand_ind_1 - vs_pan))


### Generar data frame con % de votos por partido en las elecciones de diputados federales de 2015 y 2018 ----

# 2015
por_dip_fed_2015 <- 
  df_dtto_2015 %>% 
  summarise_at(.vars = vars(pan:total_votos), 
               .funs = list( ~ sum(., na.rm = T))) %>% 
  rename(mc = movimiento_ciudadano,
         na = nueva_alianza,
         es = encuentro_social) %>% 
  gather(key = "partido",
         value = "votos_2015",
         -total_votos) %>% 
  rename(total_votos_2015 = total_votos) %>% 
  select(partido, votos_2015, total_votos_2015) %>% 
  mutate(por_2015 = round((votos_2015/total_votos_2015)*100, 1))

# 2018
por_dip_fed_2018 <- 
  cd_df_dtto_2018 %>% 
  summarise_at(.vars = vars(pan:total_votos), 
               .funs = list( ~ sum(., na.rm = T))) %>% 
  gather(key = "partido",
         value = "votos_2018",
         -total_votos) %>% 
  rename(total_votos_2018 = total_votos) %>% 
  select(partido, votos_2018, total_votos_2018) %>% 
  mutate(por_2018 = round((votos_2018/total_votos_2018)*100, 1))

### Unir data frames y generar nuevo df llamado por_dip_fed_15_18 ----
por_dip_fed_15_18 <- 
  por_dip_fed_2015 %>% 
  left_join(por_dip_fed_2018, by = "partido") %>% 
  filter(!is.na(por_2018)) %>% 
  select(partido, por_2015, por_2018) %>% 
  gather(key = "año",
         value = "por",
         -partido) %>% 
  mutate(año = ifelse(str_detect(año, "8"), 2018, 2015),
         año = as.numeric(año),
         partido = case_when(partido == "pan" ~ "PAN",
                             partido == "pri" ~ "PRI",
                             partido == "prd" ~ "PRD",
                             partido == "pvem" ~ "PVEM",
                             partido == "pt" ~ "PT",
                             partido == "mc" ~ "MC",
                             partido == "na" ~ "NA",
                             partido == "morena" ~ "Morena",
                             partido == "es" ~ "PES"))  


### Cargar datos de la composición de la Cámara de Diputados por legislatura, 1991 a 2015 ----

bd_91 <- 
  read_delim("01_bd/integracion_c_de_dip/camaraDiputados_1991_1994.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>% clean_names() %>% filter(partido != "TOTAL") %>% 
  mutate(año_eleccion = 1991,
         fecha = as_date("1991-09-27"), # Fuente de la fecha: https://portalanterior.ine.mx/documentos/TRANSP/docs/consejo-general/acuer-resol/90-91/aop2270991a.htm
         fuente = "IFE")

bd_94 <- 
  read_delim("01_bd/integracion_c_de_dip/camaraDiputados_1994_1997.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>% clean_names() %>% filter(partido != "TOTAL") %>% 
  mutate(año_eleccion = 1994,
         fecha = as_date("1994-08-21"), # Fuente de la fecha: https://portalanterior.ine.mx/documentos/TRANSP/docs/consejo-general/acuer-resol/agos94/AOP6210894A.hlm
         fuente = "IFE")

bd_97 <- 
  read_delim("01_bd/integracion_c_de_dip/camaraDiputados_1997_2000.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>% clean_names() %>% filter(partido != "TOTAL") %>% 
  mutate(año_eleccion = 1997,
         fecha = as_date("1997-08-21"), # Fuente de la fecha: https://portalanterior.ine.mx/documentos/TRANSP/docs/consejo-general/acuer-resol/ago97/AOP7210897A1.htm
         fuente = "IFE")

bd_00 <- 
  read_delim("01_bd/integracion_c_de_dip/camaraDiputados_2000_2003.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>% clean_names() %>% filter(partido != "TOTAL") %>% 
  mutate(año_eleccion = 2000,
         fecha = as_date("2000-08-23"), # Fuente de la fecha: https://portalanterior.ine.mx/documentos/TRANSP/docs/consejo-general/acuer-resol/230800/aop2230800a.htm
         fuente = "IFE")

bd_03 <- 
  read_delim("01_bd/integracion_c_de_dip/camaraDiputados_2003_2006.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>% clean_names() %>% filter(partido != "TOTAL") %>% 
  mutate(año_eleccion = 2003,
         fecha = as_date("2003-08-22"), # https://portalanterior.ine.mx/documentos/TRANSP/docs/consejo-general/acuerdos/2003/22AGO03/220803ap19.pdf
         fuente = "IFE")

bd_06 <- 
  read_csv("01_bd/integracion_c_de_dip/camaraDiputados_2006_2009.csv", trim_ws = TRUE) %>% clean_names() %>% filter(partido != "TOTAL") %>% 
  mutate(año_eleccion = 2006,
         fecha = as_date("2006-08-23"), # Fuente de la fecha: https://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/DS/DS-CG/DS-SesionesCG/CG-acuerdos/2006/23agosto/CGe230806ap2.pdf
         fuente = "IFE")

bd_09 <- 
  read_csv("01_bd/integracion_c_de_dip/camaraDiputados_2009_2012.csv", trim_ws = TRUE) %>% clean_names() %>% filter(partido != "TOTAL") %>% 
  mutate(año_eleccion = 2009,
         fecha = as_date("2009-08-28"), # Fuente de la fecha: https://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/DS/DS-CG/DS-SesionesCG/CG-acuerdos/2009/agosto/09_agosto/CGe210809apunico.pdf y https://portalanterior.ine.mx/archivos3/portal/historico/contenido/interiores/menuitem.e811f8875df20fd417bed910d08600a0-id-4c17ed3926263210VgnVCM1000000c68000aRCRD/
         fuente = "IFE")

bd_12 <- 
  read_delim("01_bd/integracion_c_de_dip/camaraDiputados_2012_2015.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>% clean_names() %>% filter(partido != "TOTAL") %>% 
  mutate(año_eleccion = 2012,
         fecha = as_date("2012-08-23"), # Fuente de la fecha: https://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/DS/DS-CG/DS-SesionesCG/CG-acuerdos/2012/Agosto/CGext201208-23/CGe230812ap_2.pdf
         fuente = "IFE")

bd_15 <- 
  read_delim("01_bd/integracion_c_de_dip/camaraDiputados_2015_2018.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>% clean_names() %>% filter(partido != "TOTAL") %>% 
  mutate(año_eleccion = 2015,
         fecha = as_date("2015-08-13"), # Fuente de la fecha: https://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/DS/DS-GacetasElectorales_INE/2015/Gaceta-009/GE_009_002.pdf
         fuente = "INE")



### Unir dataframes con datos de 1991 a 2015 -----
bd <- do.call(rbind, mget(ls(pattern = 'bd_')))

### Dip. fed. por partido, por principio y total, 1991-2015 ----
df_pp <- 
  bd %>% 
  mutate(diputados_rp = ifelse(diputados_rp == "No aplica", NA, diputados_rp),
         diputados_rp = as.numeric(diputados_rp),
         partido = ifelse(partido == "Candidato Independiente", "C. Ind.", ifelse(partido == "Convergencia" | partido == "Movimiento Ciudadano", "MC",  ifelse(partido == "Nueva Alianza", "PANAL",  ifelse(partido == "Encuentro Social", "PES", partido))))) %>% 
  group_by(año_eleccion, partido, fecha, fuente) %>% 
  summarise(mr = sum(diputados_mr),
            rp = sum(diputados_rp),
            total = sum(total)) %>% 
  ungroup() %>% 
  select(año_eleccion, partido, mr, rp, total, fecha, fuente)


### Generar dataframe con datos de 2018 de acuerdo con el INE (23 de agosto) ----
# Fuente: https://repositoriodocumental.ine.mx/xmlui/bitstream/handle/123456789/98206/CGor201808-23-ap-5.pdf

df_pp_ine_2018 <- 
  tribble(
  ~año_eleccion, ~partido,  ~mr, ~rp,
  2018, "PAN", 40, 41,
  2018, "PRI", 7, 38,
  2018, "PRD", 9, 12,
  2018, "PT", 58, 3,
  2018, "PVEM", 5, 11,
  2018, "MC", 17, 10,
  2018, "MORENA", 106, 85,
  2018, "PES", 56, 0,
  2018, "PANAL", 2, 0
) %>% 
  mutate(total = mr + rp, 
         fecha = as_date("2018-08-23"),
         fuente = "INE") 

### Unir datos pre-2018 con los de 2018 ----
df_pp <- 
  rbind(df_pp, df_pp_ine_2018)

