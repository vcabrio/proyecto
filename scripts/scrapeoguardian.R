# ============================================================
# The Guardian API + IDMC
# Cobertura mediática vs desplazamiento forzado
# Siria, Yemen, Nigeria, Colombia vs USA, Ucrania
##############################################################
install.packages("guardianapi")

file.edit("~/.Renviron")

Sys.getenv("GUARDIAN_API_KEY")


# PAQUETES 

library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(readxl)
library(httr)

# CONFIGURACIÓN

OUTDIR    <- "output/data"
IDMC_PATH <- "bases/IDMC_Internal_Displacement_Conflict-Violence_Disasters.xlsx"
API_KEY   <- Sys.getenv("GUARDIAN_API_KEY")

#FUNCIÓN CACHE

cache_query <- function(filename, fetch_fn, overwrite = FALSE) {
  filepath <- paste0(filename, ".rds")
  dir_path <- dirname(filepath)
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    message(paste("Created directory:", dir_path))
  }
  
  if (file.exists(filepath) && !overwrite) {
    message(paste("Loading cached:", filepath))
    return(readRDS(filepath))
  }
  
  message(paste("Fetching and caching:", filepath))
  data <- fetch_fn()
  saveRDS(data, filepath)
  return(data)
}

# FUNCIÓN GUARDIAN API

query_guardian <- function(query, year, api_key) {
  
  all_results <- list()
  page        <- 1
  total_pages <- 1
  
  while (page <= total_pages) {
    Sys.sleep(0.5)
    
    resp <- tryCatch(
      GET(
        "https://content.guardianapis.com/search",
        query = list(
          q            = query,
          `from-date`  = paste0(year, "-01-01"),
          `to-date`    = paste0(year, "-12-31"),
          `page-size`  = 200,
          page         = page,
          format       = "json",
          `api-key`    = api_key
        ),
        timeout(30)
      ),
      error = function(e) {
        message(paste("    Error en página", page, ":", conditionMessage(e)))
        return(NULL)
      }
    )
    
    if (is.null(resp) || status_code(resp) != 200) {
      message(paste("    Error HTTP:", status_code(resp)))
      break
    }
    
    data        <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
    total_pages <- data$response$pages
    results     <- data$response$results
    
    if (is.null(results) || nrow(results) == 0) break
    
    all_results[[page]] <- results |>
      select(id, webPublicationDate, sectionName) |>
      mutate(webPublicationDate = as.Date(substr(webPublicationDate, 1, 10)))
    
    message(paste("    Página", page, "de", total_pages, "— artículos:", nrow(results)))
    page <- page + 1
  }
  
  if (length(all_results) == 0) {
    return(tibble(date = as.Date(NA), article_count = NA_integer_))
  }
  
  # Agregar por mes
  bind_rows(all_results) |>
    mutate(month = floor_date(webPublicationDate, "month")) |>
    group_by(month) |>
    summarise(article_count = n(), .groups = "drop") |>
    rename(date = month)
}

#############################################################

# COBERTURA MEDIÁTICA X CONFLICTO Y AÑO 

message("\n══ BLOQUE 1: Querying The Guardian API ════════════════════")

conflicts <- list(
  syria    = 'Syria AND (displaced OR refugee OR crisis OR war)',
  yemen    = 'Yemen AND (displaced OR refugee OR crisis OR famine OR war)',
  nigeria  = 'Nigeria AND (displaced OR crisis OR conflict)',
  colombia = 'Colombia AND (displaced OR crisis OR conflict OR guerrilla)',
  usa      = '"United States" AND (election OR economy OR politics)',
  ukraine  = 'Ukraine AND (war OR conflict OR displaced OR refugee)'
)

years <- 2017:2023

message(paste("  Conflicts :", paste(names(conflicts), collapse = ", ")))
message(paste("  Years     :", min(years), "→", max(years)))
message(paste("  Total requests:", length(conflicts) * length(years)))

guardian_raw <- map_dfr(years, function(yr) {
  map_dfr(names(conflicts), function(case) {
    
    message(sprintf("  [%s - %d]", toupper(case), yr))
    
    df <- cache_query(
      filename = file.path(OUTDIR, "guardian_api", paste0(case, "_", yr)),
      fetch_fn = function() query_guardian(
        query   = conflicts[[case]],
        year    = yr,
        api_key = API_KEY
      )
    )
    
    df |> mutate(conflict = case, year = yr)
  })
})

# Agregar a nivel anual
guardian_annual <- guardian_raw |>
  filter(!is.na(date), !is.na(article_count)) |>
  mutate(year = year(date)) |>
  group_by(conflict, year) |>
  summarise(
    coverage_total = sum(article_count,  na.rm = TRUE),
    coverage_mean  = mean(article_count, na.rm = TRUE),
    coverage_max   = max(article_count,  na.rm = TRUE),
    .groups = "drop"
  )

write_csv(guardian_annual, file.path(OUTDIR, "guardian_annual.csv"))

message("── Guardian completo ──────────────────────────────────────")
message(paste("  - Rows     :", nrow(guardian_annual)))
message(paste("  - Conflicts:", n_distinct(guardian_annual$conflict)))
message(paste("  - Período  :", min(guardian_annual$year), "→", max(guardian_annual$year)))

##############################################################
# DESPLAZAMIENTO FORZADO 

message("\n══ BLOQUE 2: Loading IDMC displacement data ═══════════════")


target_countries <- c("SYR", "COL", "NGA", "YEM", "UKR")
country_labels   <- c(SYR = "syria", COL = "colombia", NGA = "nigeria", YEM = "yemen", UKR= "ukraine")

idmc_raw <- read_excel(IDMC_PATH, sheet = "1_Displacement_data")

displacement_df <- idmc_raw |>
  filter(ISO3 %in% target_countries) |>
  transmute(
    iso3      = ISO3,
    conflict  = country_labels[ISO3],
    year      = as.integer(Year),
    idp_stock = as.numeric(`Conflict Stock Displacement`),
    idp_flow  = as.numeric(`Conflict Internal Displacements`)
  ) |>
  distinct(conflict, year, .keep_all = TRUE) |>
  filter(year %in% years) |>
  arrange(conflict, year)

write_csv(displacement_df, file.path(OUTDIR, "displacement_idmc.csv"))

message("── IDMC completo ──────────────────────────────────────────")
message(paste("  - Rows     :", nrow(displacement_df)))
message(paste("  - Countries:", paste(unique(displacement_df$conflict), collapse = ", ")))
message(paste("  - Período  :", min(displacement_df$year), "→", max(displacement_df$year)))

#######################################################################
# COBERTURA + DESPLAZAMIENTO

message("\n══ BLOQUE 3: Merging datasets ══════════════════════════════")

final_df <- guardian_annual |>
  left_join(displacement_df, by = c("conflict", "year")) |>
  mutate(
    country_type = case_when(
      conflict %in% c("syria", "yemen", "nigeria", "colombia") ~ "conflict_country",
      conflict %in% c("usa", "ukraine")                        ~ "reference_country"
    ),
    coverage_per_million = if_else(
      !is.na(idp_stock) & idp_stock > 0,
      coverage_total / (idp_stock / 1e6),
      NA_real_
    )
  )

write_csv(final_df, file.path(OUTDIR, "final_merged.csv"))

message("── Merge completo ─────────────────────────────────────────")
message(paste("  - Rows     :", nrow(final_df)))
message(paste("  - Columns  :", ncol(final_df)))
message("─────────────────────────────────────────────────────────")
message("✅ Pipeline completo. Archivos guardados en outputsdata/")
message("─────────────────────────────────────────────────────────")

print(final_df)
#####


names(idmc_raw)


unique(idmc_raw$Name)

###### GUARDO ARCHIVO: 
write_csv(final_df, "output/data/final_merged.csv")
# Te muestra la ruta exacta donde está guardado
file.path(getwd(), "output/data/final_merged.csv")


########GRÁFICO#####

library(ggplot2)
library(dplyr)
library(scales)
library(readr)

final_df <- read_csv("output/data/final_merged.csv")

# 2022: 

df_2022 <- final_df |>
  filter(year == 2022) |>
  mutate(
    conflict = case_when(
      conflict == "syria"    ~ "Siria",
      conflict == "yemen"    ~ "Yemen",
      conflict == "nigeria"  ~ "Nigeria",
      conflict == "colombia" ~ "Colombia",
      conflict == "usa"      ~ "EE.UU.",
      conflict == "ukraine"  ~ "Ucrania"
    ),
    conflict = factor(conflict, levels = c("Ucrania", "EE.UU.", "Siria", "Yemen", "Nigeria", "Colombia"))
  )

# GRÁFICO 2022: 

ggplot(df_2022, aes(x = conflict, y = coverage_total, fill = country_type)) +
  
  geom_col(width = 0.6) +
  
  geom_text(aes(label = coverage_total), vjust = -0.5, fontface = "bold", size = 5) +
  
  scale_fill_manual(
    values = c("conflict_country"  = "#4E79A7",
               "reference_country" = "#F28E2B"),
    labels = c("País en conflicto humanitario", "País occidental de referencia")
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    title    = "Cobertura mediática internacional por país (2022)",
    subtitle = "Número de artículos publicados en The Guardian",
    x        = NULL,
    y        = "Artículos publicados",
    fill     = NULL,
    caption  = "Fuente: The Guardian API (2022)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", size = 16),
    plot.subtitle      = element_text(color = "grey40", size = 12),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank()
  )

ggsave("output/cobertura_2022.png", width = 12, height = 7, dpi = 300)
message("Gráfico guardado en output/cobertura_2022.png")


# GRÁFICO 2017

df_2017 <- final_df |>
  filter(year == 2017) |>
  mutate(
    conflict = case_when(
      conflict == "syria"    ~ "Siria",
      conflict == "yemen"    ~ "Yemen",
      conflict == "nigeria"  ~ "Nigeria",
      conflict == "colombia" ~ "Colombia",
      conflict == "usa"      ~ "EE.UU.",
      conflict == "ukraine"  ~ "Ucrania"
    ),
    conflict = factor(conflict, levels = c("Ucrania", "EE.UU.", "Siria", "Yemen", "Nigeria", "Colombia"))
  )

ggplot(df_2017, aes(x = conflict, y = coverage_total, fill = country_type)) +
  
  geom_col(width = 0.6) +
  
  geom_text(aes(label = coverage_total), vjust = -0.5, fontface = "bold", size = 5) +
  
  scale_fill_manual(
    values = c("conflict_country"  = "#4E79A7",
               "reference_country" = "#F28E2B"),
    labels = c("País en conflicto humanitario", "País occidental de referencia")
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    title    = "Cobertura mediática internacional por país (2017)",
    subtitle = "Número de artículos publicados en The Guardian",
    x        = NULL,
    y        = "Artículos publicados",
    fill     = NULL,
    caption  = "Fuente: The Guardian API (2017)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", size = 16),
    plot.subtitle      = element_text(color = "grey40", size = 12),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank()
  )

ggsave("output/cobertura_2017.png", width = 12, height = 7, dpi = 300)
message("Gráfico guardado en output/cobertura_2017.png")


#######

#GRÁFICO 2018: 

df_2018 <- final_df |>
  filter(year == 2018) |>
  mutate(
    conflict = case_when(
      conflict == "syria"    ~ "Siria",
      conflict == "yemen"    ~ "Yemen",
      conflict == "nigeria"  ~ "Nigeria",
      conflict == "colombia" ~ "Colombia",
      conflict == "usa"      ~ "EE.UU.",
      conflict == "ukraine"  ~ "Ucrania"
    ),
    conflict = factor(conflict, levels = c("Ucrania", "EE.UU.", "Siria", "Yemen", "Nigeria", "Colombia"))
  )


ggplot(df_2018, aes(x = conflict, y = coverage_total, fill = country_type)) +
  
  geom_col(width = 0.6) +
  
  geom_text(aes(label = coverage_total), vjust = -0.5, fontface = "bold", size = 5) +
  
  scale_fill_manual(
    values = c("conflict_country"  = "#4E79A7",
               "reference_country" = "#F28E2B"),
    labels = c("País en conflicto humanitario", "País occidental de referencia")
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    title    = "Cobertura mediática internacional por país (2018)",
    subtitle = "Número de artículos publicados en The Guardian",
    x        = NULL,
    y        = "Artículos publicados",
    fill     = NULL,
    caption  = "Fuente: The Guardian API (2018)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", size = 16),
    plot.subtitle      = element_text(color = "grey40", size = 12),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank()
  )

ggsave("output/cobertura_2018.png", width = 12, height = 7, dpi = 300)
message("Gráfico guardado en output/cobertura_2018.png")

#GRÁFICO 2019

df_2019 <- final_df |>
  filter(year == 2019) |>
  mutate(
    conflict = case_when(
      conflict == "syria"    ~ "Siria",
      conflict == "yemen"    ~ "Yemen",
      conflict == "nigeria"  ~ "Nigeria",
      conflict == "colombia" ~ "Colombia",
      conflict == "usa"      ~ "EE.UU.",
      conflict == "ukraine"  ~ "Ucrania"
    ),
    conflict = factor(conflict, levels = c("Ucrania", "EE.UU.", "Siria", "Yemen", "Nigeria", "Colombia"))
  )

ggplot(df_2019, aes(x = conflict, y = coverage_total, fill = country_type)) +
  
  geom_col(width = 0.6) +
  
  geom_text(aes(label = coverage_total), vjust = -0.5, fontface = "bold", size = 5) +
  
  scale_fill_manual(
    values = c("conflict_country"  = "#4E79A7",
               "reference_country" = "#F28E2B"),
    labels = c("País en conflicto humanitario", "País occidental de referencia")
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    title    = "Cobertura mediática internacional por país (2019)",
    subtitle = "Número de artículos publicados en The Guardian",
    x        = NULL,
    y        = "Artículos publicados",
    fill     = NULL,
    caption  = "Fuente: The Guardian API (2019)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", size = 16),
    plot.subtitle      = element_text(color = "grey40", size = 12),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank()
  )

ggsave("output/cobertura_2019.png", width = 12, height = 7, dpi = 300)
message("Gráfico guardado en output/cobertura_2019.png")


#GRÁFICO 2020: 

df_2020 <- final_df |>
  filter(year == 2020) |>
  mutate(
    conflict = case_when(
      conflict == "syria"    ~ "Siria",
      conflict == "yemen"    ~ "Yemen",
      conflict == "nigeria"  ~ "Nigeria",
      conflict == "colombia" ~ "Colombia",
      conflict == "usa"      ~ "EE.UU.",
      conflict == "ukraine"  ~ "Ucrania"
    ),
    conflict = factor(conflict, levels = c("Ucrania", "EE.UU.", "Siria", "Yemen", "Nigeria", "Colombia"))
  )


ggplot(df_2020, aes(x = conflict, y = coverage_total, fill = country_type)) +
  
  geom_col(width = 0.6) +
  
  geom_text(aes(label = coverage_total), vjust = -0.5, fontface = "bold", size = 5) +
  
  scale_fill_manual(
    values = c("conflict_country"  = "#4E79A7",
               "reference_country" = "#F28E2B"),
    labels = c("País en conflicto humanitario", "País occidental de referencia")
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    title    = "Cobertura mediática internacional por país (2020)",
    subtitle = "Número de artículos publicados en The Guardian",
    x        = NULL,
    y        = "Artículos publicados",
    fill     = NULL,
    caption  = "Fuente: The Guardian API (2020)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", size = 16),
    plot.subtitle      = element_text(color = "grey40", size = 12),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank()
  )

ggsave("output/cobertura_2020.png", width = 12, height = 7, dpi = 300)
message("Gráfico guardado en output/cobertura_2020.png")



#GRÁFICO 2021: 

df_2021 <- final_df |>
  filter(year == 2021) |>
  mutate(
    conflict = case_when(
      conflict == "syria"    ~ "Siria",
      conflict == "yemen"    ~ "Yemen",
      conflict == "nigeria"  ~ "Nigeria",
      conflict == "colombia" ~ "Colombia",
      conflict == "usa"      ~ "EE.UU.",
      conflict == "ukraine"  ~ "Ucrania"
    ),
    conflict = factor(conflict, levels = c("Ucrania", "EE.UU.", "Siria", "Yemen", "Nigeria", "Colombia"))
  )


ggplot(df_2021, aes(x = conflict, y = coverage_total, fill = country_type)) +
  
  geom_col(width = 0.6) +
  
  geom_text(aes(label = coverage_total), vjust = -0.5, fontface = "bold", size = 5) +
  
  scale_fill_manual(
    values = c("conflict_country"  = "#4E79A7",
               "reference_country" = "#F28E2B"),
    labels = c("País en conflicto humanitario", "País occidental de referencia")
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    title    = "Cobertura mediática internacional por país (2021)",
    subtitle = "Número de artículos publicados en The Guardian",
    x        = NULL,
    y        = "Artículos publicados",
    fill     = NULL,
    caption  = "Fuente: The Guardian API (2021)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", size = 16),
    plot.subtitle      = element_text(color = "grey40", size = 12),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank()
  )

ggsave("output/cobertura_2021.png", width = 12, height = 7, dpi = 300)
message("Gráfico guardado en output/cobertura_2021.png")



#GRÁFICO 2023: 

df_2023 <- final_df |>
  filter(year == 2023) |>
  mutate(
    conflict = case_when(
      conflict == "syria"    ~ "Siria",
      conflict == "yemen"    ~ "Yemen",
      conflict == "nigeria"  ~ "Nigeria",
      conflict == "colombia" ~ "Colombia",
      conflict == "usa"      ~ "EE.UU.",
      conflict == "ukraine"  ~ "Ucrania"
    ),
    conflict = factor(conflict, levels = c("Ucrania", "EE.UU.", "Siria", "Yemen", "Nigeria", "Colombia"))
  )

ggplot(df_2023, aes(x = conflict, y = coverage_total, fill = country_type)) +
  
  geom_col(width = 0.6) +
  
  geom_text(aes(label = coverage_total), vjust = -0.5, fontface = "bold", size = 5) +
  
  scale_fill_manual(
    values = c("conflict_country"  = "#4E79A7",
               "reference_country" = "#F28E2B"),
    labels = c("País en conflicto humanitario", "País occidental de referencia")
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    title    = "Cobertura mediática internacional por país (2023)",
    subtitle = "Número de artículos publicados en The Guardian",
    x        = NULL,
    y        = "Artículos publicados",
    fill     = NULL,
    caption  = "Fuente: The Guardian API (2023)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", size = 16),
    plot.subtitle      = element_text(color = "grey40", size = 12),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank()
  )

ggsave("output/cobertura_2023.png", width = 12, height = 7, dpi = 300)
message("Gráfico guardado en output/cobertura_2023.png")


###### 
grafico_1 <- ggplot(final_df,
       aes(x = idp_stock,
           y = coverage_total,
           color = conflict)) +
  geom_point(size = 3, alpha = .8) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    x = "Personas desplazadas internamente",
    y = "Artículos en The Guardian",
    title = "Desplazamiento interno y cobertura mediática"
  )
ggsave("output/grafico_1.png", width = 12, height = 7, dpi = 300)
message("Gráfico guardado en output/grafico_1.png")
#####

grafico_2 <- ratio_df <- final_df %>%
  group_by(conflict) %>%
  summarise(
    cobertura_media = mean(coverage_per_million, na.rm = TRUE)
  )

ggplot(ratio_df,
       aes(x = reorder(conflict, cobertura_media),
           y = cobertura_media)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    x = "",
    y = "Artículos por millón de desplazados",
    title = "Atención mediática relativa"
  )
ggsave("output/grafico_2.png", width = 12, height = 7, dpi = 300)
message("Gráfico guardado en output/grafico_2.png")

######

grafico_3 <- datos <- guardian_annual %>%
  group_by(conflict) %>%
  summarise(
    cobertura = sum(coverage_total, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(datos,
       aes(x = reorder(conflict, cobertura),
           y = cobertura)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Cobertura total en The Guardian (2017-2023)",
    x = "",
    y = "Número de artículos"
  )

ggsave("output/grafico_3.png", width = 12, height = 7, dpi = 300)
message("Gráfico guardado en output/grafico_3.png")












