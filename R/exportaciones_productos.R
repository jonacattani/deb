# Cosa esporta (o importa) Trinidad e Tobago: composizione per prodotto.
#
# Input: Excel con cmdCode (HS a 6 cifre), cmdDesc e una colonna per anno
# (valori in US$), come TTO_exportaciones_por_producto.xlsx o
# TTO_importaciones_por_producto.xlsx.
#
# Produce due grafici:
#   1. i principali prodotti nell'ultimo anno (barre orizzontali)
#   2. composizione per grandi gruppi di prodotti, anno per anno
#
# Pacchetti: install.packages(c("tidyverse", "readxl", "scales", "ragg"))

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)

# Parametri -------------------------------------------------------------------

# "exportaciones" oppure "importaciones": cambia gruppi, testi e nomi dei file
flusso <- "exportaciones"

file_excel   <- paste0("C:/Users/giova/OneDrive/Desktop/CEPAL/Network Analysis/Casos de Estudio/Trinidad and Tobago/Data/TTO_", flusso, "_por_producto.xlsx")
cartella_out <- "C:/Users/giova/OneDrive/Desktop/CEPAL/Network Analysis/Casos de Estudio/Trinidad and Tobago/Graficos"
n_top <- 15
fonte <- "Fuente: elaboraci\u00f3n propia con datos de UN Comtrade."

colore_testo   <- "#0b0b0b"
colore_testo_2 <- "#52514e"
colore_griglia <- "#e6e5e1"

dir.create(cartella_out, showWarnings = FALSE, recursive = TRUE)

# Gruppi di prodotti per capitolo HS (prime due cifre) ---------------------------
# Gruppi diversi per export e import, perche' i due panieri sono molto diversi.

gruppo_hs <- function(codice) {
  cap <- as.integer(substr(codice, 1, 2))
  if (flusso == "exportaciones") {
    case_when(
      cap == 27                ~ "Hidrocarburos (cap. 27)",
      cap %in% c(28, 29, 31)   ~ "Petroqu\u00edmica y fertilizantes (cap. 28, 29, 31)",
      cap %in% c(72, 73)       ~ "Hierro y acero (cap. 72, 73)",
      cap <= 24                ~ "Alimentos y bebidas (cap. 1-24)",
      TRUE                     ~ "Resto"
    )
  } else {
    case_when(
      cap %in% c(84, 85)       ~ "Maquinaria y equipo el\u00e9ctrico (cap. 84, 85)",
      cap %in% 86:89           ~ "Veh\u00edculos y embarcaciones (cap. 86-89)",
      cap <= 24                ~ "Alimentos y bebidas (cap. 1-24)",
      cap %in% 28:40           ~ "Qu\u00edmicos, f\u00e1rmacos y pl\u00e1sticos (cap. 28-40)",
      cap %in% c(25, 26, 72:83) ~ "Minerales y metales (cap. 25, 26, 72-83)",
      cap == 27                ~ "Combustibles (cap. 27)",
      TRUE                     ~ "Resto"
    )
  }
}
livelli_gruppi <- if (flusso == "exportaciones") {
  c("Hidrocarburos (cap. 27)",
    "Petroqu\u00edmica y fertilizantes (cap. 28, 29, 31)",
    "Hierro y acero (cap. 72, 73)",
    "Alimentos y bebidas (cap. 1-24)",
    "Resto")
} else {
  c("Maquinaria y equipo el\u00e9ctrico (cap. 84, 85)",
    "Veh\u00edculos y embarcaciones (cap. 86-89)",
    "Alimentos y bebidas (cap. 1-24)",
    "Qu\u00edmicos, f\u00e1rmacos y pl\u00e1sticos (cap. 28-40)",
    "Minerales y metales (cap. 25, 26, 72-83)",
    "Combustibles (cap. 27)",
    "Resto")
}
# palette categorica in ordine fisso; "Resto" sempre grigio
palette_cat <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300")
colori_gruppi <- setNames(
  c(palette_cat[seq_len(length(livelli_gruppi) - 1)], "#b4b3ad"),
  livelli_gruppi
)

testi <- list(
  exportaciones = list(prodotti = "principales productos de exportaci\u00f3n",
                       totale = "total exportado",
                       gruppi = "exportaciones por grupo de productos",
                       verbo = "exportado"),
  importaciones = list(prodotti = "principales productos de importaci\u00f3n",
                       totale = "total importado",
                       gruppi = "importaciones por grupo de productos",
                       verbo = "importado")
)[[flusso]]

# Nomi brevi in spagnolo per i prodotti principali (gli altri: descrizione Comtrade)
nomi_prodotti <- c(
  "271111" = "Gas natural licuado (GNL)",
  "281410" = "Amon\u00edaco anhidro",
  "290511" = "Metanol",
  "720310" = "Hierro de reducci\u00f3n directa",
  "270900" = "Petr\u00f3leo crudo",
  "310280" = "Soluci\u00f3n de urea y nitrato de amonio (UAN)",
  "271019" = "Combustibles pesados y otros aceites de petr\u00f3leo",
  "310210" = "Urea",
  "271012" = "Aceites livianos de petr\u00f3leo (gasolinas)",
  "271119" = "Otros gases de petr\u00f3leo licuados",
  "220210" = "Bebidas no alcoh\u00f3licas azucaradas",
  "210390" = "Salsas y condimentos",
  "190490" = "Preparaciones a base de cereales",
  "271112" = "Propano licuado",
  "271113" = "Butano licuado",
  "293361" = "Melamina",
  "190410" = "Cereales inflados o tostados",
  "481810" = "Papel higi\u00e9nico",
  "701090" = "Envases de vidrio",
  "220300" = "Cerveza de malta",
  "180631" = "Chocolate relleno",
  # importazioni
  "260112" = "Mineral de hierro aglomerado (pellets)",
  "870340" = "Veh\u00edculos h\u00edbridos (gasolina y el\u00e9ctrico)",
  "300490" = "Medicamentos",
  "210690" = "Preparaciones alimenticias n.c.p.",
  "870421" = "Camionetas y camiones di\u00e9sel (hasta 5 t)",
  "870380" = "Veh\u00edculos el\u00e9ctricos",
  "870322" = "Autom\u00f3viles a gasolina (1.000-1.500 cc)",
  "870323" = "Autom\u00f3viles a gasolina (1.500-3.000 cc)",
  "843049" = "Maquinaria de perforaci\u00f3n",
  "890190" = "Buques de carga",
  "040690" = "Quesos",
  "848180" = "V\u00e1lvulas y grifos",
  "230990" = "Alimentos para animales",
  "170199" = "Az\u00facar refinada",
  "020230" = "Carne bovina deshuesada congelada",
  "100199" = "Trigo",
  "150790" = "Aceite de soja refinado",
  "722830" = "Barras de acero aleado",
  "841590" = "Partes de aire acondicionado",
  "847989" = "Otras m\u00e1quinas y aparatos mec\u00e1nicos",
  "730890" = "Estructuras de hierro o acero",
  "841199" = "Partes de turbinas de gas",
  "961900" = "Pa\u00f1ales y productos higi\u00e9nicos",
  "200410" = "Papas preparadas congeladas"
)

# Dati ------------------------------------------------------------------------

dati <- read_excel(file_excel, col_types = "text") |>
  pivot_longer(-c(cmdCode, cmdDesc), names_to = "anno", values_to = "valore") |>
  mutate(anno = as.integer(anno), valore = as.numeric(valore)) |>
  filter(!is.na(valore), valore > 0, nchar(cmdCode) == 6) |>
  mutate(gruppo = factor(gruppo_hs(cmdCode), levels = livelli_gruppi))

ultimo_anno <- max(dati$anno)
anni <- sort(unique(dati$anno))

num_es <- function(x, acc = 1) number(x, accuracy = acc, big.mark = ".", decimal.mark = ",")

tema <- theme_minimal(base_size = 11) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", size = 15, colour = colore_testo,
                              margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, colour = colore_testo_2,
                                 margin = margin(b = 10)),
    plot.caption = element_text(size = 8.5, colour = colore_testo_2, hjust = 0,
                                margin = margin(t = 12)),
    axis.text = element_text(size = 9, colour = colore_testo_2),
    axis.text.y = element_text(size = 10, colour = colore_testo),
    panel.grid.minor = element_blank(),
    legend.position = "top", legend.justification = "left",
    legend.text = element_text(size = 9.5, colour = colore_testo),
    legend.key.size = unit(10, "pt"),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(18, 22, 14, 14)
  )

# 1. Principali prodotti nell'ultimo anno ----------------------------------------

dati_ultimo <- dati |> filter(anno == ultimo_anno)
totale_ultimo <- sum(dati_ultimo$valore)

top <- dati_ultimo |>
  slice_max(valore, n = n_top, with_ties = FALSE) |>
  mutate(
    nome = coalesce(nomi_prodotti[cmdCode], substr(cmdDesc, 1, 45)),
    nome = paste0(nome, " (", cmdCode, ")"),
    nome = fct_reorder(nome, valore),
    quota = valore / totale_ultimo,
    etichetta = paste0(num_es(valore / 1e6), "  (", num_es(100 * quota, 0.1), "%)")
  )

g_prodotti <- ggplot(top, aes(x = valore, y = nome, fill = gruppo)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = etichetta), hjust = 0, nudge_x = max(top$valore) * 0.012,
            size = 3.2, colour = colore_testo_2) +
  scale_fill_manual(values = colori_gruppi, drop = TRUE, name = NULL) +
  scale_x_continuous(labels = \(x) num_es(x / 1e6),
                     expand = expansion(mult = c(0, 0.2))) +
  guides(fill = guide_legend(nrow = if (length(livelli_gruppi) > 5) 3 else 2)) +
  labs(
    title = paste0("Trinidad y Tobago: ", testi$prodotti, ", ", ultimo_anno),
    subtitle = paste0(
      "Millones de US$ y participaci\u00f3n en el ", testi$totale, " (US$ ",
      num_es(totale_ultimo / 1e6), " millones). C\u00f3digo SA a 6 d\u00edgitos entre par\u00e9ntesis.\n",
      "Los ", n_top, " principales productos concentran el ",
      num_es(100 * sum(top$quota), 0.1), "% del total."
    ),
    x = NULL, y = NULL, caption = fonte
  ) +
  tema +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = colore_griglia, linewidth = 0.4))

ggsave(file.path(cartella_out, paste0("TTO_", flusso, "_productos_", ultimo_anno, ".png")),
       g_prodotti, width = 10.5, height = 7, dpi = 300, bg = "white")

# 2. Composizione per grandi gruppi, anno per anno -------------------------------

per_gruppo <- dati |>
  group_by(anno, gruppo) |>
  summarise(valore = sum(valore), .groups = "drop") |>
  group_by(anno) |>
  mutate(quota = valore / sum(valore)) |>
  ungroup()

totali <- per_gruppo |>
  group_by(anno) |>
  summarise(valore = sum(valore), .groups = "drop")

g_gruppi <- ggplot(per_gruppo, aes(x = factor(anno), y = valore,
                                   fill = fct_rev(gruppo))) +
  geom_col(width = 0.62, colour = "white", linewidth = 0.4) +
  # quota % dentro i segmenti abbastanza grandi
  geom_text(aes(label = if_else(quota >= 0.04, paste0(num_es(100 * quota), "%"), "")),
            position = position_stack(vjust = 0.5), size = 3.1, colour = colore_testo,
            fontface = "bold") +
  geom_text(data = totali, aes(x = factor(anno), y = valore,
                               label = num_es(valore / 1e6)),
            inherit.aes = FALSE, vjust = -0.6, size = 3.5, fontface = "bold",
            colour = colore_testo) +
  scale_fill_manual(values = colori_gruppi, breaks = livelli_gruppi, name = NULL) +
  scale_y_continuous(labels = \(x) num_es(x / 1e6),
                     expand = expansion(mult = c(0, 0.08))) +
  guides(fill = guide_legend(nrow = if (length(livelli_gruppi) > 5) 3 else 2)) +
  labs(
    title = paste0("Trinidad y Tobago: ", testi$gruppi, ", ",
                   min(anni), "-", ultimo_anno),
    subtitle = paste0(
      "Millones de US$. Sobre cada barra, el ", testi$totale, "; dentro, la participaci\u00f3n ",
      "de cada grupo (si supera el 4%)."
    ),
    x = NULL, y = NULL, caption = fonte
  ) +
  tema +
  theme(axis.text.x = element_text(size = 10, colour = colore_testo),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = colore_griglia, linewidth = 0.4))

ggsave(file.path(cartella_out, paste0("TTO_", flusso, "_grupos_", min(anni), "_", ultimo_anno, ".png")),
       g_gruppi, width = 10, height = 6.5, dpi = 300, bg = "white")

message("Grafici salvati in ", cartella_out)
