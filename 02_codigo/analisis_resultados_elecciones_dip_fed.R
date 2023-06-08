### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/cargar_paquetes_setup_tema_grafica.R")

### Cargar y procesar datos ----
source("02_codigo/cargar_limpiar_datos_eleccion_dip_fed.R")

### Gráfica del cambio en % de votos por partido político en las elecciones de diputados federales de 2015 y 2018, resaltando al PRI ----

# Gráfica no incluida en el texto

por_dip_fed_15_18 %>% 
  arrange(partido, año) %>% 
  group_by(partido) %>% 
  mutate(cambio_por = por - lag(por)) %>% 
  ungroup() %>% 
  mutate(etiqueta = ifelse(año == 2018, partido, ""),
         etiqueta_por = ifelse(año == 2018 & partido == "PRI", paste(por, "%", sep = ""), ""),
         etiqueta_cambio = ifelse(año == 2018 & partido == "PRI", paste("\n(Δ", cambio_por, "%)",sep = ""), ""), 
         color_linea = ifelse(partido == "PRI", "Sí", "No")) %>% 
  ggplot(aes(año, por, group = partido, color = color_linea)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = etiqueta), nudge_x = 0.2, size = 6.5, family = "Trebuchet MS Bold", color = "grey40") +
  geom_text(aes(label = etiqueta_por), x = 2018.2, y = 15, size = 5.5, family = "Trebuchet MS Bold", color = "grey40") +
  geom_text(aes(label = etiqueta_cambio), x = 2018.2, y = 14.5, size = 5.5, family = "Trebuchet MS Bold", color = "salmon") +
  scale_x_continuous(breaks = c(2015, 2018), limits = c(2015, 2018.2)) +
  scale_y_continuous(breaks = seq(0, 40, 5), limits = c(0, 41)) +
  scale_color_manual(values = c("grey50", "grey70")) +
  labs(title = str_wrap(str_to_upper("cambio en el % de votos obtendios por cada partido político en las elecciones de diputados federales de 2015 y 2018"), width = 65),
       x = NULL,
       y = "Porcentaje\n") +
  tema +
  theme(panel.grid.major.x = element_blank(),
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 22),
        legend.position = "none") 

### Gráfico 4: Porcentaje de votos obtenido por el PRI o la coalición que encabezó en la elección de diputados federales, 1991-2018 ----

# 1991 ----
df_dtto_1991 %>% 
  count(pp_ganador, sort = T)

foo <- 
  df_dtto_1991 %>% 
  mutate(promedio = mean(vs_pri, na.rm = T))

g_1991 <- 
  foo %>% 
  ggplot() +
  geom_density(aes(vs_pri), color = "grey70", fill = "grey70") +
  geom_vline(aes(xintercept = promedio)) +
  annotate(geom = "text", x = unique(foo$promedio) - 40, y = 0.07, label = paste("Promedio: ", round(unique(foo$promedio), 2), "%", sep = ""), hjust = 0, family = "Times New Roman", size = 8) +
  scale_x_continuous(breaks = seq(0, 90, 10), limits = c(-1, 92), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.0001, 0.085), expand = c(0, 0)) +
  labs(title = "1991",
       x = "\nMargen porcentual sobre el segundo lugar",
       y = "Densidad\n") +
  tema +
  theme(axis.text = element_text(size = 18, face = "bold", family = "Times New Roman"),
        axis.title = element_text(size = 24, face = "bold", family = "Times New Roman"),
        axis.title.x = element_text(size = 24, face = "bold", family = "Times New Roman", color ="white"))


# 1994 ----
df_dtto_1994 %>% 
  count(pp_ganador, sort = T)

foo <- 
  df_dtto_1994 %>% 
  mutate(promedio = mean(vs_pri, na.rm = T))

g_1994 <- 
  foo %>% 
  ggplot() +
  geom_density(aes(vs_pri), color = "grey70", fill = "grey70") +
  geom_vline(aes(xintercept = promedio)) +
  annotate(geom = "text", x = unique(foo$promedio) + 1, y = 0.07, label = paste("Promedio: ", round(unique(foo$promedio), 2), "%", sep = ""), hjust = 0, family = "Times New Roman", size = 8) +
  scale_x_continuous(breaks = seq(0, 90, 10), limits = c(-1, 92), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.0001, 0.085), expand = c(0, 0)) +
  labs(title = "1994",
       x = "\nMargen porcentual sobre el segundo lugar",
       y = "Densidad\n") +
  tema +
  theme(axis.text = element_text(size = 18, face = "bold", family = "Times New Roman"),
        axis.title = element_text(size = 24, face = "bold", family = "Times New Roman", color = "white"))


# 1997 ----
df_dtto_1997 %>% 
  count(pp_ganador, sort = T)

foo <- 
  df_dtto_1997 %>% 
  mutate(promedio = mean(vs_pri, na.rm = T))

g_1997 <- 
  foo %>% 
  ggplot() +
  geom_density(aes(vs_pri), color = "grey70", fill = "grey70") +
  geom_vline(aes(xintercept = promedio)) +
  annotate(geom = "text", x = unique(foo$promedio) + 1, y = 0.07, label = paste("Promedio: ", round(unique(foo$promedio), 2), "%", sep = ""), hjust = 0, family = "Times New Roman", size = 8) +
  scale_x_continuous(breaks = seq(0, 90, 10), limits = c(-1, 92), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.0001, 0.085), expand = c(0, 0)) +
  labs(title = "1997",
       x = "\nMargen porcentual sobre el segundo lugar",
       y = "Densidad\n") +
  tema +
  theme(axis.text = element_text(size = 18, face = "bold", family = "Times New Roman"),
        axis.title = element_text(size = 24, face = "bold", family = "Times New Roman", color = "white"))


# 2000 ----
df_dtto_2000 %>% 
  count(pp_ganador, sort = T)

foo <- 
  df_dtto_2000 %>% 
  mutate(promedio = mean(vs_pri, na.rm = T))

g_2000 <- 
  foo %>% 
  ggplot() +
  geom_density(aes(vs_pri), color = "grey70", fill = "grey70") + 
  geom_vline(aes(xintercept = promedio)) +
  annotate(geom = "text", x = unique(foo$promedio) + 1, y = 0.07, label = paste("Promedio: ", round(unique(foo$promedio), 2), "%", sep = ""), hjust = 0, family = "Times New Roman", size = 8) +
  scale_x_continuous(breaks = seq(0, 90, 10), limits = c(-1, 92), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.0001, 0.085), expand = c(0, 0)) +
  labs(title = "2000",
       x = "\nMargen porcentual sobre el segundo lugar",
       y = "Densidad\n") +
  tema +
  theme(axis.text = element_text(size = 18, face = "bold", family = "Times New Roman"),
        axis.title = element_text(size = 24, face = "bold", family = "Times New Roman", color = "white"))


# 2003 ----
df_dtto_2003 %>% 
  count(pp_ganador, sort = T)

foo <- 
  df_dtto_2003 %>% 
  select(vs_pri, vs_apt) %>% 
  mutate(vs_pri_apt = ifelse(!is.na(vs_pri), vs_pri, vs_apt), 
         promedio = mean(vs_pri_apt, na.rm = T))

g_2003 <- 
  foo %>% 
  ggplot() +
  geom_density(aes(vs_pri_apt), color = "grey70", fill = "grey70") +
  geom_vline(aes(xintercept = promedio)) +
  annotate(geom = "text", x = unique(foo$promedio) + 1, y = 0.07, label = paste("Promedio: ", round(unique(foo$promedio), 2), "%", sep = ""), hjust = 0, family = "Times New Roman", size = 8) +
  scale_x_continuous(breaks = seq(0, 90, 10), limits = c(-1, 92), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.0001, 0.085), expand = c(0, 0)) +
  labs(title = "2003 - PRI + Alianza por Todos",
       x = "\nMargen porcentual sobre el segundo lugar",
       y = "Densidad\n") +
  tema +
  theme(axis.text = element_text(size = 18, face = "bold", family = "Times New Roman"),
        axis.title = element_text(size = 24, face = "bold", family = "Times New Roman", color = "white"))


# 2006 ----
df_dtto_2006 %>% 
  count(pp_ganador, sort = T)

foo <- 
  df_dtto_2006 %>% 
  mutate(promedio = mean(vs_apm, na.rm = T))

g_2006 <- 
  foo %>% 
  ggplot() +
  geom_density(aes(vs_apm), color = "grey70", fill = "grey70") +
  geom_vline(aes(xintercept = promedio)) +
  annotate(geom = "text", x = unique(foo$promedio) + 1, y = 0.07, label = paste("Promedio: ", round(unique(foo$promedio), 2), "%", sep = ""), hjust = 0, family = "Times New Roman", size = 8) +
  scale_x_continuous(breaks = seq(0, 90, 10), limits = c(-1, 92), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.0001, 0.085), expand = c(0, 0)) +
  labs(title = "2006 - Alianza por México",
       x = "\nMargen porcentual sobre el segundo lugar",
       y = "Densidad\n") +
  tema +
  theme(axis.text = element_text(size = 18, face = "bold", family = "Times New Roman"),
        axis.title = element_text(size = 24, face = "bold", family = "Times New Roman", color = "white"))


# 2009 ----
df_dtto_2009 %>% 
  count(pp_ganador, sort = T)

foo <- 
  df_dtto_2009 %>% 
  mutate(promedio = mean(vs_pri, na.rm = T))

g_2009 <- 
  foo %>% 
  ggplot() +
  geom_density(aes(vs_pri), color = "grey70", fill = "grey70") +
  geom_vline(aes(xintercept = promedio)) +
  annotate(geom = "text", x = unique(foo$promedio) + 1, y = 0.07, label = paste("Promedio: ", round(unique(foo$promedio), 2), "%", sep = ""), hjust = 0, family = "Times New Roman", size = 8) +
  scale_x_continuous(breaks = seq(0, 90, 10), limits = c(-1, 92), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.0001, 0.085), expand = c(0, 0)) +
  labs(title = "2009",
       x = "\nMargen porcentual sobre el segundo lugar",
       y = "Densidad\n") +
  tema +
  theme(axis.text = element_text(size = 18, face = "bold", family = "Times New Roman"),
        axis.title = element_text(size = 24, face = "bold", family = "Times New Roman", color = "white"))



# 2012 ----
df_dtto_2012 %>% 
  count(pp_ganador, sort = T)

foo <- 
  df_dtto_2012 %>% 
  mutate(promedio = mean(vs_pri, na.rm = T))

g_2012 <- 
  foo %>% 
  ggplot() +
  geom_density(aes(vs_pri), color = "grey70", fill = "grey70") +
  geom_vline(aes(xintercept = promedio)) +
  annotate(geom = "text", x = unique(foo$promedio) + 2, y = 0.07, label = paste("Promedio: ", round(unique(foo$promedio), 2), "%", sep = ""), hjust = 0, family = "Times New Roman", size = 8) +
  scale_x_continuous(breaks = seq(0, 90, 10), limits = c(-1, 92), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.0001, 0.085), expand = c(0, 0)) +
  labs(title = "2012",
       x = "\nMargen porcentual sobre el segundo lugar",
       y = "Densidad\n") +
  tema +
  theme(axis.text = element_text(size = 18, face = "bold", family = "Times New Roman"),
        axis.title = element_text(size = 24, face = "bold", family = "Times New Roman", color = "white"))


# 2015 ----
df_dtto_2015 %>% 
  count(pp_ganador, sort = T)

foo <- 
  df_dtto_2015 %>% 
  mutate(promedio = mean(vs_pri, na.rm = T))

g_2015 <- 
  foo %>% 
  ggplot() +
  geom_density(aes(vs_pri), color = "grey70", fill = "grey70") +
  geom_vline(aes(xintercept = promedio)) +
  annotate(geom = "text", x = unique(foo$promedio) + 1, y = 0.07, label = paste("Promedio: ", round(unique(foo$promedio), 2), "%", sep = ""), hjust = 0, family = "Times New Roman", size = 8) +
  scale_x_continuous(breaks = seq(0, 90, 10), limits = c(-1, 92), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.0001, 0.085), expand = c(0, 0)) +
  labs(title = "2015",
       x = "\nMargen porcentual sobre el segundo lugar",
       y = "Densidad\n") +
  tema +
  theme(axis.text = element_text(size = 18, face = "bold", family = "Times New Roman"),
        axis.title = element_text(size = 24, face = "bold", family = "Times New Roman", color = "white"))


# 2018 ----
cd_df_dtto_2018 %>% 
  count(pp_ganador, sort = T)

foo <- 
  cd_df_dtto_2018 %>% 
  mutate(promedio = mean(vs_pri, na.rm = T))

g_2018 <- 
  cd_df_dtto_2018 %>% 
  mutate(promedio = mean(vs_pri, na.rm = T)) %>% 
  ggplot() +
  geom_density(aes(vs_pri), color = "grey70", fill = "grey70") +
  geom_vline(aes(xintercept = promedio)) +
  annotate(geom = "text", x = unique(foo$promedio) + 1, y = 0.07, label = paste("Promedio: ", round(unique(foo$promedio), 2), "%", sep = ""), hjust = 0, family = "Times New Roman", size = 8) +
  scale_x_continuous(breaks = seq(0, 90, 10), limits = c(-1, 92), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.0001, 0.085), expand = c(0, 0)) +
  labs(title = "2018",
       x = "\nPorcentaje de votos",
       y = "Densidad\n") +
  tema +
  theme(axis.text = element_text(size = 18, face = "bold", family = "Times New Roman"),
        axis.title.y = element_text(size = 24, face = "bold", family = "Times New Roman", color = "white"))

# Unir gráficas ----
grafica <- plot_grid(g_1991, g_1994, g_1997, g_2000, g_2003, g_2006, g_2009, g_2012, g_2015, g_2018, nrow = 5, ncol = 2)

# Generar título ----
titulo <- ggdraw() + draw_label("Gráfico 4 Porcentaje de votos obtenido por el PRI a nivel    \ndistrital en las elecciones de diputados fedrales, 1991-2018", fontface = 'bold', size = 34, hjust = 0.5, fontfamily = "Times New Roman", colour = "grey20")  

# Generar gráfica ----
plot_grid(titulo, grafica, ncol = 1, rel_heights = c(0.08, 0.9)) 

ggsave("03_graficas/grafico_04_porcetaje_votos_pri_en_dttos_mas_votos.jpg", dpi = 320, width = 15, height = 22)


### Calcular la desviación estándar del % de votos obtenido por los candidatos a diputados del PRI -o la coalición que encabezó- entre 1991 y 2018 ----

# 1991
df_dtto_1991 %>% 
  summarise(des_est = sd(vs_pri, na.rm = T))

# 1994
df_dtto_1994 %>% 
  summarise(des_est = sd(vs_pri, na.rm = T))

# 1997
df_dtto_1997 %>% 
  summarise(des_est = sd(vs_pri, na.rm = T))

# 2000
df_dtto_2000 %>% 
  summarise(des_est = sd(vs_pri, na.rm = T))

# 2003
df_dtto_2003 %>% 
  select(vs_pri, vs_apt) %>% 
  mutate(vs_pri_apt = ifelse(!is.na(vs_pri), vs_pri, vs_apt)) %>% 
  summarise(des_est = sd(vs_pri_apt, na.rm = T))

# 2006
df_dtto_2006 %>% 
  summarise(des_est = sd(vs_apm, na.rm = T))

# 2009
df_dtto_2009 %>% 
  summarise(des_est = sd(vs_pri, na.rm = T))

# 2012
df_dtto_2012 %>% 
  summarise(des_est = sd(vs_pri, na.rm = T))

# 2015
df_dtto_2015 %>% 
  summarise(des_est = sd(vs_pri, na.rm = T))

# 2018
cd_df_dtto_2018 %>% 
  summarise(des_est = sd(vs_pri, na.rm = T))


### Calcular el porcentaje de candidatas y candidatos a una diputación federal del PRI que obtuvieron menos del 21% de los votos en cada elección analizada ----

# 1991
df_dtto_1991 %>% 
  mutate(vs_pri_menor_21 = ifelse(test = vs_pri < 21, 
                                  yes = 1, 
                                  no = 0)) %>% 
  summarise(por_dttos_vs_pri_menor_21 = sum(x = vs_pri_menor_21)/300*100)


# 1994
df_dtto_1994 %>% 
  mutate(vs_pri_menor_21 = ifelse(test = vs_pri < 21, 
                                  yes = 1, 
                                  no = 0)) %>% 
  summarise(por_dttos_vs_pri_menor_21 = sum(x = vs_pri_menor_21)/300*100)


# 1997
df_dtto_1997 %>% 
  mutate(vs_pri_menor_21 = ifelse(test = vs_pri < 21, 
                                  yes = 1, 
                                  no = 0)) %>% 
  summarise(por_dttos_vs_pri_menor_21 = sum(x = vs_pri_menor_21)/300*100)


# 2000
df_dtto_2000 %>% 
  mutate(vs_pri_menor_21 = ifelse(test = vs_pri < 21, 
                                  yes = 1, 
                                  no = 0)) %>% 
  summarise(por_dttos_vs_pri_menor_21 = sum(x = vs_pri_menor_21)/300*100)


# 2003
df_dtto_2003 %>% 
  select(vs_pri, vs_apt) %>% 
  mutate(vs_pri_apt = ifelse(!is.na(vs_pri), vs_pri, vs_apt),
         vs_pri_apt_menor_21 = ifelse(test = vs_pri_apt < 21, 
                                      yes = 1, 
                                      no = 0)) %>% 
  summarise(por_dttos_vs_pri_apt_menor_21 = sum(x = vs_pri_apt_menor_21)/300*100)

# 2006
df_dtto_2006 %>% 
  mutate(vs_apm_menor_21 = ifelse(test = vs_apm < 21, 
                                  yes = 1, 
                                  no = 0)) %>% 
  summarise(por_dttos_vs_apm_menor_21 = sum(x = vs_apm_menor_21)/300*100)

# 2009
df_dtto_2009 %>% 
  mutate(vs_pri_menor_21 = ifelse(test = vs_pri < 21, 
                                  yes = 1, 
                                  no = 0)) %>% 
  summarise(por_dttos_vs_pri_menor_21 = sum(x = vs_pri_menor_21)/300*100)

# 2012
df_dtto_2012 %>% 
  mutate(vs_pri_menor_21 = ifelse(test = vs_pri < 21, 
                                  yes = 1, 
                                  no = 0)) %>% 
  summarise(por_dttos_vs_pri_menor_21 = sum(x = vs_pri_menor_21)/300*100)

# 2015
df_dtto_2015 %>% 
  mutate(vs_pri_menor_21 = ifelse(test = vs_pri < 21, 
                                  yes = 1, 
                                  no = 0)) %>% 
  summarise(por_dttos_vs_pri_menor_21 = sum(x = vs_pri_menor_21)/300*100)

# 2018
cd_df_dtto_2018 %>% 
  mutate(vs_pri_menor_21 = ifelse(test = vs_pri < 21, 
                                  yes = 1, 
                                  no = 0)) %>% 
  summarise(por_dttos_vs_pri_menor_21 = sum(x = vs_pri_menor_21)/300*100)


### Gráfico 5: Número de distritos en los que el PRI obtuvo el mayor número de votos en la elección de diputados federales, 1991-2018 ----

# 1991
dtto_ganados_1991 <- 
  df_dtto_1991 %>% 
  count(pp_ganador, sort = T) %>% 
  mutate(ranking = rank(-n),
         año = 1991) 

# 1994
dtto_ganados_1994 <- 
  df_dtto_1994 %>% 
  count(pp_ganador, sort = T) %>% 
  mutate(ranking = rank(-n),
         año = 1994) 

# 1997
dtto_ganados_1997 <- 
  df_dtto_1997 %>% 
  count(pp_ganador, sort = T) %>% 
  mutate(ranking = rank(-n),
         año = 1997) 

# 2000
dtto_ganados_2000 <- 
  df_dtto_2000 %>% 
  count(pp_ganador, sort = T) %>% 
  mutate(ranking = rank(-n),
         año = 2000) 

# 2003
dtto_ganados_2003 <- 
  df_dtto_2003 %>% 
  count(pp_ganador, sort = T) %>% 
  filter(pp_ganador %in% c("PRI", "Alianza para todos")) %>% # En este año consideramos tanto los dttos ganados por el PRI como por la "Alianza para todos", integrada por el PRI y el PVEM
  summarise(n = sum(n),
            pp_ganador = first(pp_ganador)) %>% 
  mutate(ranking = rank(-n),
         año = 2003)

# 2006
dtto_ganados_2006 <- 
  df_dtto_2006 %>% 
  count(pp_ganador, sort = T) %>% 
  mutate(ranking = rank(-n),
         año = 2006) 

# 2009
dtto_ganados_2009 <- 
  df_dtto_2009 %>% 
  count(pp_ganador, sort = T) %>% 
  mutate(ranking = rank(-n),
         año = 2009) 

# 2012
dtto_ganados_2012 <- 
  df_dtto_2012 %>% 
  count(pp_ganador, sort = T) %>% 
  mutate(ranking = rank(-n),
         año = 2012) 

# 2015
dtto_ganados_2015 <- 
  df_dtto_2015 %>% 
  count(pp_ganador, sort = T)  %>% 
  mutate(ranking = rank(-n),
         año = 2015) 

# 2018
dtto_ganados_2018 <- 
  cd_df_dtto_2018 %>% 
  count(pp_ganador, sort = T)  %>% 
  mutate(ranking = rank(-n),
         año = 2018) 

# Unir data frames
dtto_ganados <- rbind(dtto_ganados_1991, dtto_ganados_1994, dtto_ganados_1997, dtto_ganados_2000, dtto_ganados_2003, dtto_ganados_2006, dtto_ganados_2009, dtto_ganados_2012, dtto_ganados_2015, dtto_ganados_2018)

# Gráfica 
dtto_ganados %>% 
  filter(pp_ganador == "PRI" | año == 2006 & pp_ganador == "Alianza por México") %>% 
  mutate(etiqueta_pp = case_when(!año %in% c(2003, 2006) ~ pp_ganador,
                                 TRUE ~ ""),
         etiqueta_coal = case_when(año == 2006 ~ "Alianza por\nMéxico",  
                                   año == 2003 ~ "PRI + Alianza\npara Todos",
                                   TRUE ~ ""),
         etiqueta_n_gde = ifelse(n > 20, n, ""),
         etiqueta_n_chi = ifelse(n < 20, n, ""),
         etiqueta_por_gde = ifelse(n > 20, paste("(", round((n/300)*100, 1), "%)", sep = ""), ""),
         etiqueta_por_chi = ifelse(n < 20, paste("(", round((n/300)*100, 1), "%)", sep = ""), "")) %>% 
  ggplot(aes(año, n)) +
  geom_col(fill = "grey70") +
  geom_text(aes(label = etiqueta_n_gde), family = "Times New Roman", vjust = 1.7, color = "white", size = 8) +
  geom_text(aes(label = etiqueta_n_chi), family = "Times New Roman", vjust = -2.2, color = "grey60", size = 8) +
  geom_text(aes(label = etiqueta_por_gde), family = "Times New Roman", vjust = 4.2, color = "white", size = 6) +
  geom_text(aes(label = etiqueta_por_chi), family = "Times New Roman", vjust = -1, color = "grey60", size = 6) +

  geom_text(aes(label = etiqueta_coal), y = 35, family = "Times New Roman", vjust = 1.7, color = "white", size = 5.5) +
  scale_x_continuous(breaks = seq(1991, 2018, 3), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#377eb8", "#a50300", "#377eb8", "grey70")) +
  labs(title = str_wrap("Gráfico 5 Número de distritos en los que el PRI obtuvo el mayor número de votos en la elección de diputados federales, 1991-2018", width = 73),
       x = NULL,
       y = "Núm. de dttos. ganados\n") +
  tema  +
  theme(plot.title = element_text(size = 32, face = "bold", margin = margin(10,0,20,0), family= "Times New Roman", color = "grey25"),panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 22),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) 

ggsave("03_graficas/grafico_05_dtto_donde_pri_obtuvo_mayor_numero_de_votos.jpg", width = 15, height = 10, dpi = 100)
