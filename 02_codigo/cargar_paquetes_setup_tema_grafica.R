### Paquetes ----
library(pacman)
p_load(cowplot, extrafont, ggrepel, janitor, readxl, scales, tidyverse)


### Setup general ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)
theme_set(theme_gray())

### Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", color = "grey35"),
        plot.title = element_text(size = 24, face = "bold", margin = margin(10,0,20,0), family= "Times New Roman", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family="Times New Roman"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family="Times New Roman"),
        legend.text = element_text(size = 14, family="Times New Roman"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family="Times New Roman"),
        axis.text = element_text(size = 16, face = "bold", family="Times New Roman"))
