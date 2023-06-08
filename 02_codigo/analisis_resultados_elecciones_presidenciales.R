### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/cargar_paquetes_setup_tema_grafica.R")

### Cargar y procesar datos ----
source("02_codigo/cargar_limpiar_datos_eleccion_presidencial.R")


### Gráfico 1: Distribución del porcentaje de votos obtenido por cada candidato presidencial priista en casillas, 1994-2018 ----

# 1994 ----
foo <- 
  bd_1994_casilla %>% 
  mutate(promedio = mean(vs_ezpl, na.rm = T))

g_1994 <- 
  foo %>% 
  ggplot() +
  geom_density(aes(vs_ezpl), color = "grey70", fill = "grey70") +
  geom_vline(aes(xintercept = promedio)) +
  annotate(geom = "text", x = unique(foo$promedio) + 2, y = 0.05, label = paste("Promedio: ", round(unique(foo$promedio), 2), "%", sep = ""), hjust = 0, family = "Times New Roman", size = 8) +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-1, 102), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.0001, 0.062), expand = c(0, 0)) +
  labs(title = "1994",
       x = NULL,
       y = "Densidad\n") +
  tema +
  theme(plot.title = element_text(size = 28, face = "bold", margin = margin(30,0,20,0), family = "Times New Roman", color = "grey25"),
        axis.text = element_text(size = 20, face = "bold", family = "Times New Roman"),
        axis.title = element_text(size = 24, face = "bold", family = "Times New Roman"))

# 2000 ----
foo <- 
  bd_2000_casilla %>% 
  mutate(promedio = mean(vs_flo, na.rm = T)) 

g_2000 <-
  foo %>% 
  ggplot() +
  geom_density(aes(vs_flo), color = "grey70", fill = "grey70") +
  geom_vline(aes(xintercept = promedio)) +
  annotate(geom = "text", x = unique(foo$promedio) + 2, y = 0.05, label = paste("Promedio: ", round(unique(foo$promedio), 2), "%", sep = ""), hjust = 0, family = "Times New Roman", size = 8) +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-1, 102), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.0001, 0.062), expand = c(0, 0)) +
  labs(title = "2000",
       x = NULL,
       y = "Densidad\n") +
  tema +
  theme(plot.title = element_text(size = 28, face = "bold", margin = margin(30,0,20,0), family = "Times New Roman", color = "grey25"),
        axis.text = element_text(size = 20, face = "bold", family = "Times New Roman"),
        axis.title = element_text(size = 24, face = "bold", family = "Times New Roman"),
        axis.title.y = element_text(color = "white"))


# 2006 ----
foo <- 
  bd_2006_casilla %>% 
  mutate(promedio = mean(vs_rmp, na.rm = T)) 

g_2006 <-
  foo %>% 
  ggplot() +
  geom_density(aes(vs_rmp), color = "grey70", fill = "grey70") +
  geom_vline(aes(xintercept = promedio)) +
  annotate(geom = "text", x = unique(foo$promedio) + 2, y = 0.05, label = paste("Promedio: ", round(unique(foo$promedio), 2), "%", sep = ""), hjust = 0, family = "Times New Roman", size = 8) +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-1, 102), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.0001, 0.062), expand = c(0, 0)) +
  labs(title = "2006",
       x = NULL,
       y = "Densidad\n") +
  tema +
  theme(plot.title = element_text(size = 28, face = "bold", margin = margin(30,0,20,0), family = "Times New Roman", color = "grey25"),
        axis.text = element_text(size = 20, face = "bold", family = "Times New Roman"),
        axis.title = element_text(size = 24, face = "bold", family = "Times New Roman"),
        axis.title.y = element_text(color = "white"))

# 2012 ----
foo <-
  bd_2012_casilla %>% 
  mutate(promedio = mean(vs_epn, na.rm = T)) 

g_2012 <-
  foo %>% 
  ggplot() +
  geom_density(aes(vs_epn), color = "grey70", fill = "grey70") +
  geom_vline(aes(xintercept = promedio)) +
  annotate(geom = "text", x = unique(foo$promedio) + 2, y = 0.05, label = paste("Promedio: ", round(unique(foo$promedio), 2), "%", sep = ""), hjust = 0, family = "Times New Roman", size = 8) +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-1, 102), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.0001, 0.062), expand = c(0, 0)) +
  labs(title = "2012",
       x = NULL,
       y = "Densidad\n") +
  tema +
  theme(plot.title = element_text(size = 28, face = "bold", margin = margin(30,0,20,0), family = "Times New Roman", color = "grey25"),
        axis.text = element_text(size = 20, face = "bold", family = "Times New Roman"),
        axis.title = element_text(size = 24, face = "bold", family = "Times New Roman"),
        axis.title.y = element_text(color = "white"))

# 2018 ----
foo <- 
  cd_2018_casilla %>% 
  mutate(promedio = mean(vs_meade, na.rm = T))

g_2018 <-
  foo %>% 
  ggplot() +
  geom_density(aes(vs_meade), color = "grey70", fill = "grey70") +
  geom_vline(aes(xintercept = promedio)) +
  annotate(geom = "text", x = unique(foo$promedio) + 2, y = 0.05, label = paste("Promedio: ", round(unique(foo$promedio), 2), "%", sep = ""), hjust = 0, family = "Times New Roman", size = 8) +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-1, 102), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.0001, 0.062), expand = c(0, 0)) +
  labs(title = "2018",
       x = "\nPorcentaje de votos",
       y = "Densidad\n") +
  tema +
  theme(plot.title = element_text(size = 28, face = "bold", margin = margin(30,0,20,0), family = "Times New Roman", color = "grey25"),
        axis.text = element_text(size = 20, face = "bold", family = "Times New Roman"),
        axis.title = element_text(size = 24, face = "bold", family = "Times New Roman"),
        axis.title.y = element_text(color = "white"))

# Unir gráficas ----
grafica <- plot_grid(g_1994, g_2000, g_2006, g_2012, g_2018, nrow = 5)

# Generar título ----
titulo <- ggdraw() + draw_label("Gráfico 1 Porcentaje de votos obtenidos por el candidato\n                            presidencial del PRI en casillas, 1994-2018                           ", fontface = 'bold', size = 36, hjust = 0.5, fontfamily = "Times New Roman", colour = "grey20")  

# Generar gráfica ----
plot_grid(titulo, grafica, ncol = 1, rel_heights = c(0.06, 0.9))

ggsave("03_graficas/grafico_01_distribucion_porcentaje_votos_1994_2018.jpg", dpi = 320, width = 15, height = 22)


### Gráfico 2: % de casillas/secciones/dttos/edos en donde el candidato presidencial ganador obtuvo el mayor número de votos ----

# Casillas ----
comp_casillas <- 
  bd_por_candidatos %>% 
  filter(primero %in% c("Zedillo", "Labastida", "Madrazo", "Peña Nieto", "Meade"),
         unidad == "Casillas") %>% 
  mutate(etiqueta_grandes = ifelse(por > 5, paste(round(por, 1), "%", sep = ""), ""),
         etiqueta_pequeños = ifelse(por < 5, paste(round(por, 1), "%", sep = ""), "")) %>% 
  ggplot(aes(fct_reorder(primero, año), por)) +
  geom_col(fill = "grey70") +
  geom_text(aes(label = etiqueta_grandes), vjust = 1.5, color = "white", fontface = "bold", size = 6, family = "Times New Roman") +
  geom_text(aes(label = etiqueta_pequeños), vjust = -0.7, color = "grey50", fontface = "bold", size = 6, family = "Times New Roman") +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(title = str_wrap(str_to_upper("Casillas"), width = 60),
       x = NULL, 
       y = NULL) +
  tema +
  theme(axis.text.y = element_blank(), 
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 22, color = "grey40"))


# Secciones ----
comp_secciones <- 
  bd_por_candidatos %>%
  filter(primero %in% c("Zedillo", "Labastida", "Madrazo", "Peña Nieto", "Meade"),
         unidad == "Secciones") %>% 
  mutate(etiqueta_grandes = ifelse(por > 7, paste(round(por, 1), "%", sep = ""), ""),
         etiqueta_pequeños = ifelse(por < 7, paste(round(por, 1), "%", sep = ""), "")) %>% 
  ggplot(aes(fct_reorder(primero, año), por)) +
  geom_col(fill = "grey70") +
  geom_text(aes(label = etiqueta_grandes), vjust = 1.5, color = "white", fontface = "bold", size = 6, family = "Times New Roman") +
  geom_text(aes(label = etiqueta_pequeños), vjust = -0.7, color = "grey50", fontface = "bold", size = 6, family = "Times New Roman") +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(title = str_wrap(str_to_upper("Secciones"), width = 60),
       x = NULL, 
       y = NULL) +
  tema +
  theme(axis.text.y = element_blank(), 
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 22, color = "grey40"))


# Distritos ----
comp_dttos <- 
  bd_por_candidatos %>%
  filter(primero %in% c("Zedillo", "Labastida", "Madrazo", "Peña Nieto", "Meade"),
         unidad == "Distritos") %>% 
  add_row(primero = "Meade", n = 0, por = 0, ranking = 4, año = 2018, unidad = "Distritos") %>% 
  mutate(etiqueta_grandes = ifelse(por > 5, paste(round(por, 1), "%", sep = ""), ""),
         etiqueta_pequeños = ifelse(por < 5, paste(round(por, 1), "%", sep = ""), "")) %>% 
  ggplot(aes(fct_reorder(primero, año), por)) +
  geom_col(fill = "grey70") +
  geom_text(aes(label = etiqueta_grandes), vjust = 1.5, color = "white", fontface = "bold", size = 6) +
  geom_text(aes(label = etiqueta_pequeños), vjust = -0.7, color = "grey50", fontface = "bold", size = 6, family = "Times New Roman") +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(title = str_wrap(str_to_upper("Distritos"), width = 60),
       x = NULL, 
       y = NULL) +
  tema +
  theme(axis.text.y = element_blank(), 
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 22, color = "grey40"))


# Estados ----
comp_edos <- 
  bd_por_candidatos %>%
  filter(primero %in% c("Zedillo", "Labastida", "Madrazo", "Peña Nieto", "Meade"),
         unidad == "Estados") %>% 
  add_row(primero = "Madrazo", n = 0, por = 0, ranking = 4, año = 2006, unidad = "Estados") %>%
  add_row(primero = "Meade", n = 0, por = 0, ranking = 4, año = 2018, unidad = "Estados") %>% 
  mutate(etiqueta_grandes = ifelse(por > 5, paste(round(por, 1), "%", sep = ""), ""),
         etiqueta_pequeños = ifelse(por < 5, paste(round(por, 1), "%", sep = ""), "")) %>% 
  ggplot(aes(fct_reorder(primero, año), por)) +
  geom_col(fill = "grey70") +
  geom_text(aes(label = etiqueta_grandes), vjust = 1.5, color = "white", fontface = "bold", size = 6, family = "Times New Roman") +
  geom_text(aes(label = etiqueta_pequeños), vjust = -0.7, color = "grey50", fontface = "bold", size = 6, family = "Times New Roman") +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(title = str_wrap(str_to_upper("Estados"), width = 60),
       x = NULL, 
       y = NULL) +
  tema +
  theme(axis.text.y = element_blank(), 
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 22, color = "grey40"))

# Unir gráficas
grafica <- plot_grid(comp_casillas, comp_secciones, comp_dttos, comp_edos, ncol = 2)

# Generar título
titulo <- ggdraw() + draw_label("Gráfico 2 Porcentaje de ____ en donde el candidato presidencial\n                     del PRI obtuvo el mayor número de votos, 1994-2018                       ", fontface = 'bold', size = 34, hjust = 0.5, fontfamily = "Times New Roman", colour = "grey20")  

# Generar gráfica
plot_grid(titulo, grafica, ncol = 1, rel_heights = c(0.1, 0.9)) 

ggsave("03_graficas/grafico_02_comparacion_porcentaje_de____ganados.jpg", dpi = 320, width = 15, height = 10)


### Gráfico 3: Cambio en el % de votos obtenidos por los candidatos presidenciales priístas entre 1994 y 2018, por estado ----

# 1994 vs. 2000
g_94_00 <- 
  bd_1994_edo %>% 
  select(edo_min, vs_ezpl) %>% 
  left_join(bd_2000_edo %>% select(edo_min, vs_flo), by = "edo_min") %>% 
  gather(key = año, 
         value = por, 
         -edo_min) %>% 
  mutate(año = ifelse(año == "vs_ezpl", 1994, 2000),
         fecha = make_date(año, 7, 1)) %>% 
  ggplot(aes(fecha, por, group = edo_min)) +
  geom_line(size = 1, alpha = 0.7, color = "grey70") +
  labs(title = str_wrap("\n1994 vs. 2000", width = 80),
       x = NULL,
       y = "% ") +
  scale_x_date(limits = c(as_date("1994-07-01"), as_date("2000-07-01")), breaks = c(as_date("1994-07-01"), as_date("2000-07-01")),  date_labels = ("'%y"), expand = c(0, 0)) +  
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10)) +
  tema +
  theme(panel.grid.major.x = element_blank(),
        axis.title.y = element_text(angle = 0),
        plot.title = element_text(size = 22, color = "grey40", hjust = 0.5))

# 2000 vs. 2006
g_00_06 <- 
  bd_2000_edo %>% 
  select(edo_min, vs_flo) %>% 
  left_join(bd_2006_edo %>% select(edo_min, vs_rmp), by = "edo_min") %>% 
  gather(key = año, 
         value = por, 
         -edo_min) %>% 
  mutate(año = ifelse(año == "vs_flo", 2000, 2006),
         fecha = make_date(año, 7, 1)) %>% 
  ggplot(aes(fecha, por, group = edo_min)) +
  geom_line(size = 1, alpha = 0.7, color = "grey70") +
  labs(title = str_wrap("\n2000 vs. 2006", width = 80),
       x = NULL,
       y = NULL) +
  scale_x_date(limits = c(as_date("2000-07-01"), as_date("2006-07-01")), breaks = c(as_date("2000-07-01"), as_date("2006-07-01")),  date_labels = ("'%y"), expand = c(0, 0)) +  
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10)) +
  tema +
  theme(panel.grid.major.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 22, color = "grey40", hjust = 0.5))

# 2006 vs. 2012
g_06_12 <- 
  bd_2006_edo %>% 
  select(edo_min, vs_rmp) %>% 
  left_join(bd_2012_edo %>% select(edo_min, vs_epn), by = "edo_min") %>% 
  gather(key = año, 
         value = por, 
         -edo_min) %>% 
  mutate(año = ifelse(año == "vs_rmp", 2006, 2012),
         fecha = make_date(año, 7, 1)) %>% 
  ggplot(aes(fecha, por, group = edo_min)) +
  geom_line(size = 1, alpha = 0.7, color = "grey70") +
  labs(title = str_wrap("\n2006 vs. 2012", width = 80),
       x = NULL,
       y = NULL) +
  scale_x_date(limits = c(as_date("2006-07-01"), as_date("2012-07-01")), breaks = c(as_date("2006-07-01"), as_date("2012-07-01")),  date_labels = ("'%y"), expand = c(0, 0)) +  
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10)) +
  tema +
  theme(panel.grid.major.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 22, color = "grey40", hjust = 0.5))

# 2012 vs. 2018
g_12_18 <- 
  bd_2012_edo %>% 
  select(edo_min, vs_epn) %>% 
  left_join(cd_2018_edo %>% select(edo_min, vs_meade), by = "edo_min") %>% 
  gather(key = año, 
         value = por, 
         -edo_min) %>% 
  mutate(año = ifelse(año == "vs_epn", 2012, 2018),
         fecha = make_date(año, 7, 1)) %>% 
  ggplot(aes(fecha, por, group = edo_min)) +
  geom_line(size = 1, alpha = 0.7, color = "grey70") +
  labs(title = str_wrap("\n2012 vs. 2018", width = 80),
       x = NULL,
       y = " %") +
  scale_x_date(limits = c(as_date("2012-07-01"), as_date("2018-07-01")), breaks = c(as_date("2012-07-01"), as_date("2018-07-01")),  date_labels = ("'%y"), expand = c(0, 0)) +  
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10), sec.axis = dup_axis()) +
  tema +
  theme(panel.grid.major.x = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.left = element_blank(), 
        axis.title.y.right = element_text(angle = 0, vjust = 1),
        plot.title = element_text(size = 22, color = "grey40", hjust = 0.5))

# Unir gráficas
grafica <- plot_grid(g_94_00, NULL, g_00_06, NULL, g_06_12, NULL, g_12_18, nrow = 1, rel_widths = c(1, 0.05, 1, 0.05, 1, 0.05, 1, 0.05))

# Generar título
titulo <- ggdraw() + draw_label("Gráfico 3 Cambio en el porcentaje de votos obtenidos por el  \n       candidato presidencial del PRI entre 1994 y 2018, por estado        ", fontface = 'bold', size = 34, hjust = 0.5, fontfamily = "Times New Roman", colour = "grey20")  

# Generar gráfica
plot_grid(titulo, grafica, ncol = 1, rel_heights = c(0.15, 0.9)) 

ggsave("03_graficas/grafico_03_cambio_porcentaje_1994_2018.jpg", dpi = 320, width = 15, height = 10)


### Generar nuevo objeto después de unir y transformar datos de las elecciones de 1994 a 2018 ----

# Esto es necesario para hacer los cálculos que presentamos a continuación
bd_presi_todos <- 
  # Unir
  bd_1994_edo %>% 
  select(edo_min, vs_ezpl) %>% 
  left_join(bd_2000_edo %>% select(edo_min, vs_flo), by = "edo_min") %>% 
  left_join(bd_2006_edo %>% select(edo_min, vs_rmp), by = "edo_min") %>% 
  left_join(bd_2012_edo %>% select(edo_min, vs_epn), by = "edo_min") %>% 
  left_join(cd_2018_edo %>% select(edo_min, vs_meade), by = "edo_min") %>% 
  # Tranformar estructura de los datos
  pivot_longer(names_to = "candidato",
               values_to = "porcentaje",
               -edo_min) 


### Calcular cambio en el % de votos obtenido por cada candidato priísta en una elección vs. el % del candidato en la elección previa ----
bd_presi_todos %>% 
  # Generar variable año
  mutate(año = case_when(candidato == "vs_ezpl" ~ 1994,
                         candidato == "vs_flo" ~ 2000,
                         candidato == "vs_rmp" ~ 2006,
                         candidato == "vs_epn" ~ 2012,
                         candidato == "vs_meade" ~ 2018)) %>% 
  # Ordenar las observaciones por año y estado
  arrange(edo_min, año) %>% 
  # Calcular cambio en %
  group_by(edo_min) %>% 
  mutate(cambio = porcentaje - lag(porcentaje)) %>% 
  ungroup() %>% 
  select(año, everything()) %>% 
  print(n = Inf)


### Calcular el ranking del % de votos obtenido por cada candidato priísta en cada estado ----
bd_presi_todos %>% 
  # Calcular el ranking para cada estado
  group_by(edo_min) %>% 
  mutate(ranking = rank(-porcentaje, ties.method = "first")) %>% 
  ungroup() %>% 
  # Ordenar las observaciones por candidato 
  arrange(candidato) %>% 
  print(n = Inf)

