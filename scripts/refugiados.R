library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidytext)
library(here)

############################## LIMPIEZA #######################################

###-----------LIMPIEZA BASE UNHCR REFUGIADOS --------------###

base_refugiados <- read_csv("bases/persons_of_concern.csv")
  
#Modificamos el nombre de Syria para que sea mas sencillo 

base_refugiados <- base_refugiados %>%
  mutate(`Country of Origin` = recode(`Country of Origin`,
                                      "Syrian Arab Rep." = "Syria" ))


# Eliminamos las filas donde el país de asilo es el mismo que el de origen

base_refugiados <- base_refugiados %>% 
  filter(`Country of Asylum ISO` != `Country of Origin ISO`)

# Eliminamos filas con 0 refugiados

base_refugiados <- base_refugiados %>% 
  filter(Refugees > 0)

#Modificamos los nombres para que sean mas intuitivos: 

base_refugiados <- base_refugiados %>% 
  rename(
    año        = Year,
    origen     = `Country of Origin`,
    origen_id  = `Country of Origin ISO`,
    pais_asilo = `Country of Asylum`,
    asilo_id   = `Country of Asylum ISO`,
    refugiados = Refugees
  )


#miramos un resumen rapido de como quedo la base 
glimpse(base_refugiados)


write_csv(base_refugiados, "bases/refugiados_clean.csv")




###-----------LIMPIEZA BASE GDP --------------###

base_gdp   <- read_csv("bases/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_121663.csv", skip = 4)

cols_anios <- as.character(2010:2023)

base_gdp <- base_gdp %>%
  select(`Country Name`, `Country Code`, all_of(cols_anios)) %>%
  pivot_longer(
    cols = all_of(cols_anios),
    names_to = "año",
    values_to = "gdp_pc"
  ) %>%
  mutate(año = as.integer(año)) %>%
  rename(
    pais    = `Country Name`,
    pais_id = `Country Code`
  ) %>%
  filter(!is.na(gdp_pc))

#miramos un resumen rapido de como quedo la base 
glimpse(base_gdp)

write_csv(base_gdp, "bases/gdp_clean.csv")


###-----------UNIMOS AMBAS BASES PARA TENER EL GDP DE TODOS LOS PAISES  --------------###

base_merged <- base_refugiados %>%
  left_join(base_gdp, by = c("asilo_id" = "pais_id", "año" = "año"))


# Verificamos cuántas filas quedaron sin GDP
sum(is.na(base_merged$gdp_pc))

#Tenemos 13 filas sin GDP lo cual es poco pero podemos ver exactamente cuales son: 
  
base_merged %>%
  filter(is.na(gdp_pc)) %>%
  select(año, pais_asilo, asilo_id) %>%
  distinct()

#Al ser principálmente los paises de donde vienen los refugiados (Syria y Yemen) no es relevante. Los otros casos 
#son Sudan del sur y Cuba lo cual tampoco no influye ya que no son los principales lugares hacia donde van los refugiados. 


###-----------MIRAMOS COMO SE RELACIONA EL NIVEL DEL GDP Y LA CANTIDAD DE REFUGIADOS--------------###

# Agrupamos por país de asilo y calculamos el total de refugiados recibidos entre 2010 y 2023 y el GDP per cápita promedio del período


gdpxrefugiados_anual <- base_merged %>%
  group_by(año, pais_asilo) %>%
  summarise(
    
#Total de refugiados recibidos por año
    total_refugiados = sum(refugiados, na.rm = TRUE),
    
#GDP per cápita del país ese año
    gdp_pc = mean(gdp_pc, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  
#Eliminamos faltantes
  filter(
    !is.na(gdp_pc),
    !is.na(total_refugiados)
  )


# Clasificamos países por nivel de GDP

gdpxrefugiados_anual <- gdpxrefugiados_anual %>%
  mutate(
    
#Dividimos en 3 grupos
    categoria_gdp = ntile(gdp_pc, 3)
  ) %>%
  mutate(
    categoria_gdp = case_when(
      categoria_gdp == 1 ~ "Países pobres",
      categoria_gdp == 2 ~ "Países medianos",
      categoria_gdp == 3 ~ "Países ricos"
    )
  )


# Calculamos promedio anual

gdpxrefugiados_anual <- gdpxrefugiados_anual %>%
  group_by(año, categoria_gdp) %>%
  summarise(
    
#Promedio de refugiados por grupo
    promedio_refugiados = mean(total_refugiados, na.rm = TRUE),
    
    .groups = "drop"
  )

# Creamos gráfico de líneas


grafico_gdp_categoria <- gdpxrefugiados_anual %>%
  ggplot(aes(
    
#Eje x: años
    x = año,
    
#Eje y: promedio refugiados
    y = promedio_refugiados,
    
#Color por categoría
    color = categoria_gdp
  )) +
  
#Líneas
  geom_line(
    linewidth = 1.5
  ) +
  
#Puntos
  geom_point(
    size = 3
  ) +
  
#Escala eje y
  scale_y_continuous(labels = scales::comma) +
  
#Títulos
  labs(
    title = "Evolución de refugiados recibidos según nivel de GDP",
    subtitle = "Promedio anual de refugiados por categoría de países",
    x = "Año",
    y = "Promedio de refugiados recibidos",
    color = "Nivel de GDP",
    caption = "Fuente: UNHCR + World Bank"
  ) +
  
#Tema
  theme_minimal(base_size = 14)

#Mostramos gráfico
grafico_gdp_categoria


###----------CREAMOS LA CARPETA OUTPUT DONDE IRAN LOS GRAFICOS DE TODOS LOS SCRIPTS-------# 

output_dir <- here("output")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


#Guardamos el gráfico

ggsave(
  filename = file.path(output_dir, "gdp_categoria_refugiados.png"),
  plot = grafico_gdp_categoria,
  width = 10,
  height = 7,
  dpi = 300
)

###-------------GRAFICO TOP PAISES QUE RECIBEN REFUGIADOS--------###

#Observamos hacia donde van los refugiados de Colombia, Yemen, Syria y Nigeria

#Ahora buscamos donde fueron los paises que recibieron mas refugiados desde el año 2010 hasta el año 2023 

grafico_refugiados <- base_merged %>%
  group_by(origen, pais_asilo) %>%
  summarise(
    total_refugiados = sum(refugiados, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(origen, desc(total_refugiados)) %>%
  group_by(origen) %>%
  slice_max(total_refugiados, n = 10)


###--------------GRAFICOS------------### 

# Graficamos los principales países de destino para cada país de origen

grafico_refugiados <- grafico_refugiados %>%
  ggplot(aes(
    
# En el eje x ponemos el país de destino, ordenado dentro de cada país de origen
    x = reorder_within(
      pais_asilo,
      total_refugiados,
      origen
    ),
    
# En el eje y la cantidad total de refugiados
    y = total_refugiados
  )) +
  
# Barras horizontales
  geom_col(fill = "#2166ac") +
  
# Separamos en un panel por cada país de origen
  facet_wrap(~ origen, scales = "free") +
  
# Rotamos el gráfico para que los nombres de países sean legibles
  coord_flip() +
  
# Ajustamos correctamente las etiquetas del eje x
  scale_x_reordered() +
  
# Formateamos el eje y con comas para los miles
  scale_y_continuous(labels = scales::comma) +
  
# Agregamos títulos
  labs(
    title = "Top 10 países de destino por país de origen (2010-2023)",
    x = NULL,
    y = "Total de refugiados"
  ) +
  
  theme_minimal()

#Mostramos gráfico
grafico_refugiados

# Guardamos el gráfico en la carpeta output

ggsave(
  filename = file.path(output_dir, "top_destinos_refugiados.png"),
  plot = grafico_refugiados,
  width = 14,
  height = 10,
  dpi = 300
)

