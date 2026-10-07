# Storico dell'export o dell'import di Trinidad e Tobago (vedi `flusso`):
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

# "exportaciones" oppure "importaciones": cambia testi dei grafici e nomi dei file
flusso <- "exportaciones"

testi <- list(
  exportaciones = list(
    barre   = "principales destinos de exportaci\u00f3n",
    quota   = "total exportado",
    totale  = "valor total exportado",
    socios  = "exportaciones a sus principales destinos",
    partner = "destinos"
  ),
  importaciones = list(
    barre   = "principales or\u00edgenes de importaci\u00f3n",
    quota   = "total importado",
    totale  = "valor total importado",
    socios  = "importaciones desde sus principales or\u00edgenes",
    partner = "or\u00edgenes"
  )
)[[flusso]]

file_excel   <- paste0("C:/Users/giova/OneDrive/Desktop/CEPAL/Network Analysis/Casos de Estudio/Trinidad and Tobago/Data/TTO_", flusso, "_2006_2025.xlsx")
cartella_out <- "C:/Users/giova/OneDrive/Desktop/CEPAL/Network Analysis/Casos de Estudio/Trinidad and Tobago/Graficos"
n_partner    <- 8      # quanti partner nello storico (i principali dell'ultimo anno)
fonte        <- "Fuente: elaboraci\u00f3n propia con datos de UN Comtrade."

colore_barre   <- "#2a78d6"
colore_testo   <- "#0b0b0b"
colore_testo_2 <- "#52514e"
colore_griglia <- "#e6e5e1"

dir.create(cartella_out, showWarnings = FALSE, recursive = TRUE)

# Dati ------------------------------------------------------------------------

dati <- read_excel(file_excel) |>
  pivot_longer(-c(partner_iso, partner_desc),
               names_to = "anno", values_to = "valore") |>
  filter(!is.na(valore), valore > 0) |>
  mutate(anno = as.integer(anno))

nomi_es <- if (requireNamespace("countrycode", quietly = TRUE)) {
  suppressWarnings(countrycode::countrycode(dati$partner_iso, "iso3c", "cldr.short.es"))
} else {
  NA_character_
}
dati <- dati |>
  mutate(
    paese = coalesce(nomi_es, partner_desc),
    paese = recode(paese, "USA" = "United States")
  )

num_es <- function(x, acc = 1) number(x, accuracy = acc, big.mark = ".", decimal.mark = ",")

anni <- sort(unique(dati$anno))
ultimo_anno <- max(anni)
periodo <- paste0(min(anni), "-", ultimo_anno)

# Con molti anni (es. 20) i grafici si allargano e le etichette si riducono
molti_anni <- length(anni) > 10

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

totale <- dati |>
  group_by(anno) |>
  summarise(valore = sum(valore), .groups = "drop") |>
  mutate(var_pct = 100 * (valore / lag(valore) - 1))

g_totale <- ggplot(totale, aes(x = factor(anno), y = valore)) +
  geom_col(fill = colore_barre, width = 0.6) +
  geom_text(aes(label = num_es(valore / 1e6)), vjust = -0.6,
            size = if (molti_anni) 2.8 else 3.6, colour = colore_testo,
            fontface = "bold") +
  # la variazione % solo se c'e' spazio (fino a 10 anni)
  geom_text(aes(label = if_else(is.na(var_pct) | molti_anni, "",
                                paste0(if_else(var_pct >= 0, "+", ""),
                                       num_es(var_pct, 0.1), "%"))),
            vjust = -2.6, size = 3.1, colour = colore_testo_2) +
  scale_y_continuous(labels = \(x) num_es(x / 1e6),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = paste0("Trinidad y Tabago: ", testi$totale, ", ", periodo),
    subtitle = if (molti_anni) "Millones de US$." else
      "Millones de US$ y variaci\u00f3n respecto al a\u00f1o anterior.",
    x = NULL, y = NULL, caption = fonte
  ) +
  tema

ggsave(file.path(cartella_out, paste0("TTO_", flusso, "_total_", periodo, ".png")),
       g_totale, width = if (molti_anni) 12 else 9, height = 5.5, dpi = 300, bg = "white")

# 2. Evoluzione dei principali partner ----------------------------------------
# Un pannello per paese, ognuno con la propria scala: gli Stati Uniti valgono
# molto di piu' degli altri e con una scala unica le altre linee sarebbero piatte.

top_iso <- dati |>
  filter(anno == ultimo_anno) |>
  slice_max(valore, n = n_partner, with_ties = FALSE) |>
  pull(partner_iso)

storico <- dati |>
  filter(partner_iso %in% top_iso) |>
  complete(nesting(partner_iso, paese), anno = anni) |>   # anni mancanti = NA
  group_by(partner_iso) |>
  mutate(ordine = valore[anno == ultimo_anno]) |>
  ungroup() |>
  mutate(paese = fct_reorder(paese, ordine, .desc = TRUE))

# distanza tra punto ed etichetta, proporzionale al numero di anni
scarto <- 0.035 * diff(range(anni))

# etichette solo sul primo e sull'ultimo anno: a sinistra del primo punto e a
# destra dell'ultimo, cosi' non si sovrappongono alla linea
estremi <- storico |>
  filter(!is.na(valore)) |>
  group_by(paese) |>
  filter(anno %in% range(anno)) |>
  mutate(primo = anno == min(anno)) |>
  ungroup() |>
  mutate(x_testo = if_else(primo, anno - scarto, anno + scarto),
         allinea = if_else(primo, 1, 0))

g_storico <- ggplot(storico, aes(x = anno, y = valore)) +
  geom_line(colour = colore_barre, linewidth = if (molti_anni) 0.7 else 0.9, na.rm = TRUE) +
  geom_point(colour = colore_barre, size = if (molti_anni) 1.2 else 2, na.rm = TRUE) +
  geom_text(data = estremi,
            aes(x = x_testo, label = num_es(valore / 1e6), hjust = allinea),
            size = 3, colour = colore_testo) +
  facet_wrap(~ paese, ncol = 4, scales = "free_y") +
  coord_cartesian(clip = "off") +   # le etichette possono uscire dal pannello
  scale_x_continuous(breaks = range(anni), expand = expansion(add = 0.2 * diff(range(anni)) + 0.1)) +
  scale_y_continuous(labels = \(x) num_es(x / 1e6), limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = paste0("Trinidad y Tabago: ", testi$socios, ", ", periodo),
    subtitle = paste0(
      "Millones de US$. Los ", n_partner, " principales ", testi$partner, " de ", ultimo_anno,
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

ggsave(file.path(cartella_out, paste0("TTO_", flusso, "_socios_", periodo, ".png")),
       g_storico, width = 11, height = 6.5, dpi = 300, bg = "white")

message("Grafici salvati in ", cartella_out)
