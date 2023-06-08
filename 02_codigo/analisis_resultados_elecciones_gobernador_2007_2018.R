### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/cargar_paquetes_setup_tema_grafica.R")

### Cargar datos ----
pri_gober <- 
  read_excel("01_bd/gubernaturas/resultados_pri_gobernador.xlsx")

### Gráfico 6: Porcentaje de votos ganados por el PRI en las elecciones a la gubernatura celebradas en las dos rondas entre 2007 y 2018 ----
set.seed(2)
pri_gober %>% 
  # Eliminar la observación de Puebla de 2010 para mantener las dos últimas observaciones de cada entiddd 
  filter(num_eleccion != 0) %>% 
  mutate(estado_bis = case_when(estado == "Aguascalientes" ~ "Ags.",
                                estado == "Baja California Sur" ~ "BCS",
                                estado == "Ciudad de México" ~ "CDMX",
                                estado == "Estado de México" ~ "Edo. de México",
                                estado == "Quintana Roo" ~ "Q. Roo",
                                estado == "San Luis Potosí" ~ "SLP",
                                TRUE  ~ estado), 
         etiqueta_edo = ifelse(num_eleccion == 2, estado_bis, "")) %>% 
  ggplot(aes(x = num_eleccion, y = porcentaje_votos, group = estado)) +
  geom_line(size = 2, color = "grey70", alpha = 0.8) +
  geom_point(size = 3.5, color = "grey70", alpha = 0.8) +
  geom_text_repel(aes(label = str_wrap(etiqueta_edo, width = 40)), family = "Times New Roman", nudge_x = 1.5, hjust = 1, size = 6, fontface = "bold", color = "grey50", direction = "both", segment.alpha = 0.4, segment.color = "grey70") +
  scale_x_continuous(limits = c(1, 2.13), breaks = 1:2, labels = c("Primera ronda\n(2007 - 2016)", "Segunda ronda\n(2013 - 2018)")) +
  scale_y_continuous(limits = c(-2, 72),
                     breaks = seq(0, 70, 10), 
                     expand = c(0, 0)) +
  labs(title = str_wrap("Gráfico 6 Porcentaje de votos ganados por el PRI en las elecciones a la gubernatura celebradas en las dos rondas entre 2007 y 2018", width = 48),
       subtitle = str_wrap("La gráfica incluye el porcentaje de votos obtenidos por el PRI compitiendo de forma independiente o en coalición.", width = 75),
       x = "\n",
       y = "Porcentaje\n") +
  tema +
  theme(plot.title = element_text(size = 38, face = "bold"),
        plot.subtitle = element_text(size = 25, color = "grey60"),
        plot.caption = element_text(size = 18),
        axis.title = element_text(size = 30, face = "bold"),
        axis.text = element_text(size = 25, face = "bold")) 

ggsave("03_graficas/grafico_06_cambio_porcentaje_votos_gobernador_dos_rondas.png", dpi = 200, height = 22.5, width = 14.5)



### Gráfico 7: Porcentaje de votos obtenidos por el PRI en las elecciones a la gubernatura celebradas entre 2007 y 2018 ----

# Obtener lista de entidades después de ordenar ----
lista_edos <- 
  pri_gober %>% 
  mutate(fecha_eleccion = ymd(fecha_eleccion),
         año = year(ymd(fecha_eleccion))) %>% 
  arrange(año,
          porcentaje_votos) %>% 
  mutate(id = row_number()) %>% 
  select(estado) %>% 
  pull()

# Graficar ----
pri_gober %>% 
  mutate(fecha_eleccion = ymd(fecha_eleccion),
         año = year(ymd(fecha_eleccion))) %>% 
  arrange(año,
          porcentaje_votos) %>% 
  mutate(id = row_number(), 
         pri_gano = ifelse(pri_gano == 1, "Ganó el PRI", "Ganó otro partido")) %>% 
  ggplot(aes(x = id, 
             y = porcentaje_votos)) +
  annotate(geom = "rect", xmin = 1.5, xmax = 7.5, ymin = 0, ymax = 70, fill = "grey90", alpha = 0.5) +
  annotate(geom = "rect", xmin = 18.5, xmax = 24.5, ymin = 0, ymax = 70, fill = "grey90", alpha = 0.5) +
  annotate(geom = "rect", xmin = 31.5, xmax = 32.5, ymin = 0, ymax = 70, fill = "grey90", alpha = 0.5) +
  annotate(geom = "rect", xmin = 40.5, xmax = 53.5, ymin = 0, ymax = 70, fill = "grey90", alpha = 0.5) +
  annotate(geom = "rect", xmin = 56.5, xmax = 65.5, ymin = 0, ymax = 70, fill = "grey90", alpha = 0.5) +
  annotate(geom = "text", x = 1, y = 65, label = "2007", size = 4.5, fontface = "bold", family = "Times New Roman", color = "grey40", angle = 90) +
  annotate(geom = "text", x = 4.5, y = 65, label = "2009", size = 5, fontface = "bold", family = "Times New Roman", color = "grey40") +
  annotate(geom = "text", x = 13, y = 65, label = "2010", size = 5, fontface = "bold", family = "Times New Roman", color = "grey40") +
  annotate(geom = "text", x = 21.5, y = 65, label = "2011", size = 5, fontface = "bold", family = "Times New Roman", color = "grey40") +
  annotate(geom = "text", x = 28, y = 65, label = "2012", size = 5, fontface = "bold", family = "Times New Roman", color = "grey40") +
  annotate(geom = "text", x = 32, y = 65, label = "2013", size = 4.5, fontface = "bold", family = "Times New Roman", color = "grey40", angle = 90) +
  annotate(geom = "text", x = 36.5, y = 65, label = "2015", size = 5, fontface = "bold", family = "Times New Roman", color = "grey40") +
  annotate(geom = "text", x = 47, y = 65, label = "2016", size = 5, fontface = "bold", family = "Times New Roman", color = "grey40") +
  annotate(geom = "text", x = 55, y = 65, label = "2017", size = 5, fontface = "bold", family = "Times New Roman", color = "grey40") +
  annotate(geom = "text", x = 61, y = 65, label = "2018", size = 5, fontface = "bold", family = "Times New Roman", color = "grey40") +
  geom_col(aes(fill = as.factor(pri_gano)), size = 0.5) +
  geom_hline(yintercept = seq(0, 70, 10), color = "grey85", linetype = 2, size = 0.3) +
  scale_x_continuous(breaks = 1:65, labels = lista_edos, expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 90, 10), expand = c(0, 0)) +
  scale_fill_manual(values = c("grey50", "grey80")) +
  labs(title = str_wrap("Gráfico 7 Porcentaje de votos obtenidos por el PRI en las elecciones a la gubernatura celebradas entre 2007 y 2018", width = 70),
       subtitle = str_wrap("La gráfica incluye el porcentaje de votos obtenidos por el PRI compitiendo de forma independiente o en coalición. Las barras están ordenadas de forma creciente de acuerdo con el porcentaje de votos obtenidos por el PRI o su coalición en cada año.", width = 130),
       x = "\n",
       y = "Porcentaje\n", 
       fill = NULL) +
  tema +
  theme(plot.title = element_text(size = 35, face = "bold", margin = margin(10,0,10,0)),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 18),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15),
        axis.text.y = element_text(size = 22),
        panel.grid.major = element_blank(),
        legend.position = c(0.935, 0.8),
        legend.key.height = unit(0.8, "cm")) 

ggsave("03_graficas/grafico_07_porcentaje_pri_elecciones_gobernador_ordenada.png", dpi = 200, height = 11, width = 17)


### Calcular el cambio en el % de votos obtenidos por el PRI y sus coaliciones en las dos elecciones gubernatoriales celebradas en cada entidad entre 2007 y 2018 ----

pri_gober %>% 
  # Eliminar la observación de Puebla de 2010 para mantener las dos últimas observaciones de cada entiddd
  filter(num_eleccion != 0) %>% 
  # Calcular cambio en el % de votos obtenido en cada estado de una elección a otra
  group_by(estado) %>% 
  transmute(cambio = porcentaje_votos - lag(porcentaje_votos)) %>% 
  ungroup() %>% 
  # Eliminar observaciones de la primera elección, todas con valor NA
  filter(!is.na(cambio)) %>% 
  # Ordenar observaciones de acuerdo con valor de la variable cambio
  arrange(cambio) %>% 
  # Calcular el promedio de la variable cambio
  mutate(promedio = mean(cambio)) %>% 
  print(n = Inf)


### Calcular el número de elecciones gubernatoriales ganadas/perdidas por el PRI y sus coaliciones en cada ciclo electoral entre 2007 y 2018 ----
pri_gober %>% 
  # Eliminar la observación de Puebla de 2010 para mantener las dos últimas observaciones de cada entiddd
  filter(num_eleccion != 0) %>% 
  # Calcular el número de elecciones ganadas/perdidas en cada ciclo electoral
  group_by(num_eleccion) %>% 
  count(pri_gano) %>% 
  ungroup()


### Calcular el número de elecciones gubernatoriales ganadas/perdidas por el PRI y sus coaliciones en 2018 ----
pri_gober %>% 
  filter(fecha_eleccion > "2017-12-31") %>% 
  # Calcular el número de elecciones ganadas/perdidas
  count(pri_gano)


### Calcular el número de elecciones gubernatoriales de 2018 en las que el PRI y sus coaliciones obtuvieron 25% o menos de los votos ----
pri_gober %>% 
  filter(fecha_eleccion > "2017-12-31",
         porcentaje_votos <= 25)

### Calcular el número de elecciones gubernatoriales entre 2007 y 2017 en las que el PRI y sus coaliciones obtuvieron 25% o menos de los votos ----
pri_gober %>% 
  filter(fecha_eleccion > "2007-01-01" & fecha_eleccion < "2017-12-31",
         porcentaje_votos <= 25)

