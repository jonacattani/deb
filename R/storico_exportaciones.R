# Storico dell'export di Trinidad e Tobago:
#   1. valore totale esportato per anno (colonne)
#   2. evoluzione dei principali partner dell'ultimo anno (un pannello per paese)
#
# Input: lo stesso Excel "largo" di grafico_exportaciones.R
# (partner_iso, partner_desc, 2020, 2021, ...; valori in US$).
#
# Pacchetti: install.packages(c("tidyverse", "readxl", "scales", "ragg"))
# Opzionale, per i nomi dei paesi in spagnolo: install.packages("countrycode")

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)

# Parametri -------------------------------------------------------------------

file_excel   <- "C:/Users/giova/OneDrive/Desktop/CEPAL/Network Analysis/Casos de Estudio/Trinidad and Tobago/Data/TTO_exportaciones.xlsx"
cartella_out <- "C:/Users/giova/OneDrive/Desktop/CEPAL/Network Analysis/Casos de Estudio/Trinidad and Tobago/Graficos"
n_partner    <- 8      # quanti partner nello storico (i principali dell'ultimo anno)
fonte        <- "Fuente: elaboraci\u00f3n propia con datos de UN Comtrade."

colore_barre   <- "#2a78d6"
colore_testo   <- "#0b0b0b"
colore_testo_2 <- "#52514e"
colore_griglia <- "#e6e5e1"

dir.create(cartella_out, showWarnings = FALSE, recursive = TRUE)

# Dati ------------------------------------------------------------------------

export <- read_excel(file_excel) |>
  pivot_longer(-c(partner_iso, partner_desc),
               names_to = "anno", values_to = "valore") |>
  filter(!is.na(valore), valore > 0) |>
  mutate(anno = as.integer(anno))

nomi_es <- if (requireNamespace("countrycode", quietly = TRUE)) {
  suppressWarnings(countrycode::countrycode(export$partner_iso, "iso3c", "cldr.short.es"))
} else {
  NA_character_
}
export <- export |>
  mutate(
    paese = coalesce(nomi_es, partner_desc),
    paese = recode(paese, "USA" = "United States")
  )

num_es <- function(x, acc = 1) number(x, accuracy = acc, big.mark = ".", decimal.mark = ",")

anni <- sort(unique(export$anno))
ultimo_anno <- max(anni)
periodo <- paste0(min(anni), "-", ultimo_anno)

tema <- theme_minimal(base_size = 11) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", size = 15, colour = colore_testo,
                              margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, colour = colore_testo_2,
                                 margin = margin(b = 14)),
    plot.caption = element_text(size = 8.5, colour = colore_testo_2, hjust = 0,
                                margin = margin(t = 12)),
    axis.text = element_text(size = 9, colour = colore_testo_2),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = colore_griglia, linewidth = 0.4),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(18, 22, 14, 14)
  )

# 1. Totale esportato per anno ------------------------------------------------

totale <- export |>
  group_by(anno) |>
  summarise(valore = sum(valore), .groups = "drop") |>
  mutate(var_pct = 100 * (valore / lag(valore) - 1))

g_totale <- ggplot(totale, aes(x = factor(anno), y = valore)) +
  geom_col(fill = colore_barre, width = 0.6) +
  geom_text(aes(label = num_es(valore / 1e6)), vjust = -0.6,
            size = 3.6, colour = colore_testo, fontface = "bold") +
  geom_text(aes(label = if_else(is.na(var_pct), "",
                                paste0(if_else(var_pct >= 0, "+", ""),
                                       num_es(var_pct, 0.1), "%"))),
            vjust = -2.6, size = 3.1, colour = colore_testo_2) +
  scale_y_continuous(labels = \(x) num_es(x / 1e6),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = paste0("Trinidad y Tobago: valor total exportado, ", periodo),
    subtitle = "Millones de US$ y variaci\u00f3n respecto al a\u00f1o anterior.",
    x = NULL, y = NULL, caption = fonte
  ) +
  tema

ggsave(file.path(cartella_out, paste0("TTO_exportaciones_total_", periodo, ".png")),
       g_totale, width = 9, height = 5.5, dpi = 300, bg = "white")

# 2. Evoluzione dei principali partner ----------------------------------------
# Un pannello per paese, ognuno con la propria scala: gli Stati Uniti valgono
# molto di piu' degli altri e con una scala unica le altre linee sarebbero piatte.

top_iso <- export |>
  filter(anno == ultimo_anno) |>
  slice_max(valore, n = n_partner, with_ties = FALSE) |>
  pull(partner_iso)

storico <- export |>
  filter(partner_iso %in% top_iso) |>
  complete(nesting(partner_iso, paese), anno = anni) |>   # anni mancanti = NA
  group_by(partner_iso) |>
  mutate(ordine = valore[anno == ultimo_anno]) |>
  ungroup() |>
  mutate(paese = fct_reorder(paese, ordine, .desc = TRUE))

# etichette solo sul primo e sull'ultimo anno: a sinistra del primo punto e a
# destra dell'ultimo, cosi' non si sovrappongono alla linea
estremi <- storico |>
  filter(!is.na(valore)) |>
  group_by(paese) |>
  filter(anno %in% range(anno)) |>
  mutate(primo = anno == min(anno)) |>
  ungroup() |>
  mutate(x_testo = if_else(primo, anno - 0.18, anno + 0.18),
         allinea = if_else(primo, 1, 0))

g_storico <- ggplot(storico, aes(x = anno, y = valore)) +
  geom_line(colour = colore_barre, linewidth = 0.9, na.rm = TRUE) +
  geom_point(colour = colore_barre, size = 2, na.rm = TRUE) +
  geom_text(data = estremi,
            aes(x = x_testo, label = num_es(valore / 1e6), hjust = allinea),
            size = 3, colour = colore_testo) +
  facet_wrap(~ paese, ncol = 4, scales = "free_y") +
  coord_cartesian(clip = "off") +   # le etichette possono uscire dal pannello
  scale_x_continuous(breaks = range(anni), expand = expansion(add = 1.1)) +
  scale_y_continuous(labels = \(x) num_es(x / 1e6), limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = paste0("Trinidad y Tobago: exportaciones a sus principales destinos, ", periodo),
    subtitle = paste0(
      "Millones de US$. Los ", n_partner, " principales destinos de ", ultimo_anno,
      ". Cada panel tiene su propia escala vertical."
    ),
    x = NULL, y = NULL, caption = fonte
  ) +
  tema +
  theme(
    strip.text = element_text(face = "bold", size = 10.5, colour = colore_testo,
                              hjust = 0),
    panel.spacing.x = unit(14, "pt"),
    panel.spacing.y = unit(16, "pt"),
    axis.text.x = element_text(size = 8.5)
  )

ggsave(file.path(cartella_out, paste0("TTO_exportaciones_socios_", periodo, ".png")),
       g_storico, width = 11, height = 6.5, dpi = 300, bg = "white")

message("Grafici salvati in ", cartella_out)
