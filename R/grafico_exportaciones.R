# Grafici a barre orizzontali: principali destinazioni dell'export di
# Trinidad e Tobago, un'immagine PNG per anno.
#
# Input: Excel "largo" con colonne partner_iso, partner_desc, 2020, 2021, ...
# (valori in US$). Output: un PNG per anno nella cartella `cartella_out`.
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

file_excel  <- "C:/Users/giova/OneDrive/Desktop/CEPAL/Network Analysis/Casos de Estudio/Trinidad and Tobago/Data/TTO_exportaciones.xlsx"
cartella_out <- "C:/Users/giova/OneDrive/Desktop/CEPAL/Network Analysis/Casos de Estudio/Trinidad and Tobago/Graficos"
n_top       <- 15
fonte       <- "Fuente: elaboraci\u00f3n propia con datos de UN Comtrade."

colore_barre <- "#2a78d6"
colore_testo <- "#0b0b0b"
colore_testo_2 <- "#52514e"
colore_griglia <- "#e6e5e1"

dir.create(cartella_out, showWarnings = FALSE, recursive = TRUE)

# Dati ------------------------------------------------------------------------

export <- read_excel(file_excel) |>
  pivot_longer(-c(partner_iso, partner_desc),
               names_to = "anno", values_to = "valore") |>
  filter(!is.na(valore), valore > 0) |>
  mutate(anno = as.integer(anno))

# Nomi dei paesi in spagnolo se c'è countrycode, altrimenti quelli di Comtrade
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

# Accenti scritti come \u00f3 (= ó) per evitare problemi di codifica del file

# Stessa scala in tutti i grafici: asse da 0 al valore massimo di tutti gli anni
max_globale <- max(export$valore)

# Formato numeri spagnolo: 1.234,5
num_es <- function(x, acc = 1) number(x, accuracy = acc, big.mark = ".", decimal.mark = ",")

# Grafico per un anno ---------------------------------------------------------

grafico_anno <- function(a) {
  dati_anno <- export |> filter(anno == a)
  totale <- sum(dati_anno$valore)

  top <- dati_anno |>
    slice_max(valore, n = n_top, with_ties = FALSE) |>
    mutate(
      quota = valore / totale,
      etichetta = paste0(num_es(valore / 1e6), "  (", num_es(100 * quota, 0.1), "%)"),
      paese = fct_reorder(paese, valore)
    )

  quota_top <- sum(top$quota)

  ggplot(top, aes(x = valore, y = paese)) +
    geom_col(fill = colore_barre, width = 0.62) +
    geom_text(aes(label = etichetta), hjust = 0, nudge_x = max_globale * 0.012,
              size = 3.3, colour = colore_testo_2) +
    scale_x_continuous(
      labels = \(x) num_es(x / 1e6),
      limits = c(0, max_globale * 1.25),      # +25% di spazio per le etichette
      expand = expansion(mult = 0)
    ) +
    labs(
      title = paste0("Trinidad y Tobago: principales destinos de exportaci\u00f3n, ", a),
      subtitle = paste0(
        "Millones de US$ y participaci\u00f3n en el total exportado.\n",
        "Los ", nrow(top), " principales socios concentran el ",
        num_es(100 * quota_top, 0.1), "% del total (US$ ",
        num_es(totale / 1e6), " millones)."
      ),
      x = NULL, y = NULL,
      caption = fonte
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(face = "bold", size = 15, colour = colore_testo,
                                margin = margin(b = 4)),
      plot.subtitle = element_text(size = 10, colour = colore_testo_2,
                                   margin = margin(b = 14)),
      plot.caption = element_text(size = 8.5, colour = colore_testo_2, hjust = 0,
                                  margin = margin(t = 12)),
      axis.text.y = element_text(size = 10.5, colour = colore_testo),
      axis.text.x = element_text(size = 9, colour = colore_testo_2),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = colore_griglia, linewidth = 0.4),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(18, 22, 14, 14)
    )
}

# Esporta un PNG per anno -----------------------------------------------------
# Tutti i grafici ricevono la stessa larghezza per i nomi dei paesi, cosi'
# l'area delle barre resta nella stessa posizione in ogni immagine.

anni <- sort(unique(export$anno))
grafici <- lapply(anni, \(a) ggplotGrob(grafico_anno(a)))
larghezze <- do.call(grid::unit.pmax, lapply(grafici, \(g) g$widths))

for (i in seq_along(anni)) {
  grafici[[i]]$widths <- larghezze
  file_png <- file.path(cartella_out, paste0("TTO_exportaciones_", anni[i], ".png"))
  ggsave(file_png, grafici[[i]], width = 9, height = 6.5, dpi = 300, bg = "white")
  message("Salvato: ", file_png)
}
