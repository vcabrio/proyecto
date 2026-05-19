########################################################################
##################### DESPLAZAMIENTO INTERNO ##########################


install.packages(c("tidyverse", "readxl", "janitor", "skimr"))

library(tidyverse)   # dplyr, ggplot2, tidyr, etc.
library(readxl)      # leer Excel
library(janitor)     # clean_names()
library(ggplot2)


# =================================================================

ruta_ucdp  <- ("../proyecto/bases/BattleDeaths_v25_1_conf.csv")
ruta_idmc  <- ("../proyecto/bases/IDMC_Internal_Displacement_Conflict-Violence_Disasters.xlsx")

# Países de interés
paises_interes <- c("Colombia", "Yemen", "Syria", "Nigeria")

# Rango de años 
anio_inicio <- 2010
anio_fin    <- 2023


######### BASE Variable Independiente: muertes por conflicto Interno ################

# ----Cargar datos -------------------------------------------------------
ucdp_raw <- read_csv(ruta_ucdp, show_col_types = FALSE)

# ---- Estandarizar nombres de columnas -----------------------------------
ucdp_raw <- ucdp_raw %>% clean_names()

# ---- Filtrar países de interés -----------------------------------------
# Yemen aparece como "Yemen (North Yemen)" en UCDP → se normaliza
ucdp_filtrado <- ucdp_raw %>%
  mutate(
    pais = case_when(
      str_detect(location_inc, "Yemen")    ~ "Yemen",
      str_detect(location_inc, "Syria")    ~ "Syria",
      location_inc == "Colombia"           ~ "Colombia",
      location_inc == "Nigeria"            ~ "Nigeria",
      TRUE                                 ~ NA_character_
    )
  ) %>%
  filter(!is.na(pais))

# ---- Filtrar rango de años ---------------------------------------------
ucdp_filtrado <- ucdp_filtrado %>%
  filter(year >= anio_inicio, year <= anio_fin)

# ---- Agregar fatalidades por país-año -----------------------------------
# bd_best = estimación central de muertes en batalla (mejor proxy de intensidad)

ucdp_panel <- ucdp_filtrado %>%
  group_by(pais, year) %>%
  summarise(
    fatalidades_best = sum(bd_best, na.rm = TRUE),
    fatalidades_low  = sum(bd_low,  na.rm = TRUE),
    fatalidades_high = sum(bd_high, na.rm = TRUE),
    n_conflictos     = n(), # número de conflictos activos ese año
    .groups = "drop"
  ) %>% 
  rename(anio = year)

# ---- Variables derivadas de conflicto -----------------------------------
# Intensidad categórica (útil para robustez)
# 0 = sin conflicto, 1 = conflicto menor (<1000 muertes), 2 = guerra (≥1000)
ucdp_panel <- ucdp_panel %>%
  mutate(
    intensidad_cat = case_when(
      fatalidades_best == 0    ~ 0L,
      fatalidades_best < 1000  ~ 1L,
      fatalidades_best >= 1000 ~ 2L
    ),
    dummy_conflicto = if_else(fatalidades_best > 0, 1L, 0L),
    # Log de fatalidades (evita distribución muy sesgada); +1 para evitar log(0)
    log_fatalidades = log(fatalidades_best + 1)
  )
print(ucdp_panel)


# =============================================================================
# =============================================================================
############ BASE Variable Dependiente: Desplazamiento interno ###############

idmc_raw <- read_excel(ruta_idmc, sheet = "1_Displacement_data")

# Limpiar nombres de columnas 
idmc_raw <- idmc_raw %>% clean_names()

# Seleccionar y renombrar variables relevantes -----------------------
# Columnas clave según la estructura del IDMC:
#   - name → nombre del país
#   - iso3 → código ISO (útil para merge)
#   - year → año
#   - conflict_stock_displacement → stock total de IDPs (personas acumuladas)
#   - conflict_internal_displacements → nuevos desplazamientos ese año 

idmc_panel <- idmc_raw %>%
  select(
    iso3,
    pais        = name,
    anio        = year,
    idp_stock   = conflict_stock_displacement,# n°de personas acumulado
    idp_nuevos  = conflict_internal_displacements # nuevos desplazados(flujo)
  ) %>%
  filter(pais %in% paises_interes,
         anio >= anio_inicio,
         anio <= anio_fin)

# Como es excel, puede leer las comas y dar datos erroneos, por lo que
# remuevo todas las comas en ambas columnas de personas desplazadas acumuladas 
# y aún desplazadas (idp_stock) y nuevos episodios de desplazamiento ese año (idp_nuevo)

idmc_panel <- idmc_panel %>%
  mutate(
    idp_stock  = str_remove_all(as.character(idp_stock), ","),
    idp_nuevos = str_remove_all(as.character(idp_nuevos), ","),
    
    idp_stock  = as.numeric(idp_stock),
    idp_nuevos = as.numeric(idp_nuevos),
    
    anio = as.integer(anio)
  )%>% 
  mutate(
    stock_na  = is.na(idp_stock),
    nuevos_na = is.na(idp_nuevos),
    # Log de desplazamientos (VD principal); +1 para evitar log(0)
    log_idp_stock  = log(idp_stock  + 1),
    log_idp_nuevos = log(idp_nuevos + 1)
  )

print(idmc_panel)

################################################################################
###################### Merge de ambos paneles ##################################

# Combinación pais-año

panel_final <- idmc_panel %>%
  left_join(
    ucdp_panel,
    by = c("pais", "anio")
  ) %>%
  arrange(pais, anio)

summary(panel_final)

###########################################################################
###### Grafico: relación conflicto x desplazamientos#######################

output_dir <- here("output")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

grafico_conflicto <- ggplot(
  panel_final,
  aes(
    x = log_fatalidades,
    y = log_idp_nuevos
  )
) +
  
  # puntos: casos pais-año
  geom_point(
    color = "steelblue",
    size = 3,
    alpha = 0.8
  ) +
  
  # línea de tendencia
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "darkred",
    linewidth = 1
  ) +
  
  # un gráfico por país
  facet_wrap(~ pais, scales = "free") +
  
  labs(
    title = "Relación entre conflicto armado y desplazamiento interno",
    subtitle = "País-año (2010–2023)",
    x = "Fatalidades por conflicto",
    y = "Nuevos desplazamientos internos"
  ) +
  
  theme_minimal(base_size = 13)
print(grafico_conflicto)


ggsave(
  filename = file.path(
    output_dir,
    "grafico_conflicto_desplazamiento.png"
  ),
  plot = grafico_conflicto,
  width = 14,
  height = 10,
  dpi = 300
)

# =============================================================================
# CÓMO LEER EL SCATTER PLOT
# =============================================================================

# El gráfico muestra la relación entre:
#   - la intensidad del conflicto armado
#   - y el desplazamiento interno

# Eje X:
#   log_fatalidades
# → representa las muertes por conflicto armado.
# Más a la derecha = mayor intensidad del conflicto.

# Eje Y:
#   log_idp_nuevos
# → representa nuevos desplazamientos internos.
# Más arriba = más personas desplazadas.

# Cada punto representa:
#   un país en un año determinado (país-año).

# La línea roja es una línea de tendencia lineal.
# Resume la relación promedio entre ambas variables.

# Interpretación:
# Si la línea asciende y los puntos muestran una tendencia positiva,
# significa que mayores niveles de conflicto se asocian con
# mayores niveles de desplazamiento interno.

# Se usan logaritmos para:
#   - reducir la influencia de valores extremos,
#   - mejorar la visualización,
#   - y hacer comparables países con escalas muy distintas.


# =============================================================================
# TABLA POR PAÍS Y AÑO
# =============================================================================

tabla_anual <- panel_final %>%
  group_by(pais, anio) %>%
  summarise(
    
    # Fatalidades por conflicto
    fatalidades = round(
      mean(fatalidades_best, na.rm = TRUE),
      0
    ),
    
    # Nuevos desplazamientos internos
    desplazamientos = round(
      mean(idp_nuevos, na.rm = TRUE),
      0
    ),
    
    .groups = "drop"
  ) %>%
  arrange(pais, anio)

# Print de tabla
print(tabla_anual)









