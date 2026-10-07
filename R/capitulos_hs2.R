# Export o import di Trinidad e Tobago per capitolo HS a 2 cifre.
#
# Input: Excel con cmdCode (capitolo HS a 2 cifre), cmdDesc e una colonna per
# anno (valori in US$), come TTO_exportaciones_HS2.xlsx / TTO_importaciones_HS2.xlsx.
#
# Produce due grafici:
#   1. i principali capitoli nell'ultimo anno (barre orizzontali, colore per gruppo)
#   2. evoluzione dei principali capitoli (un pannello per capitolo)
#
# Nota: dal 2025 Comtrade usa l'edizione HS 2022, con descrizioni un po' diverse
# per alcuni capitoli (15, 16, 24, 84, 88). Per questo i dati si uniscono per
# codice e i nomi vengono dalla tabella in spagnolo qui sotto.
#
# Pacchetti: install.packages(c("tidyverse", "readxl", "scales", "ragg"))

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)

# Parametri -------------------------------------------------------------------

# "exportaciones" oppure "importaciones"
flusso <- "exportaciones"

file_excel   <- paste0("C:/Users/giova/OneDrive/Desktop/CEPAL/Network Analysis/Casos de Estudio/Trinidad and Tobago/Data/TTO_", flusso, "_HS2.xlsx")
cartella_out <- "C:/Users/giova/OneDrive/Desktop/CEPAL/Network Analysis/Casos de Estudio/Trinidad and Tobago/Graficos"
n_top     <- 15   # capitoli nel grafico a barre
n_storico <- 8    # capitoli nello storico
fonte <- "Fuente: elaboraci\u00f3n propia con datos de UN Comtrade."

colore_testo   <- "#0b0b0b"
colore_testo_2 <- "#52514e"
colore_griglia <- "#e6e5e1"
colore_linea   <- "#2a78d6"

dir.create(cartella_out, showWarnings = FALSE, recursive = TRUE)

# Nomi brevi dei capitoli HS in spagnolo -----------------------------------------

nomi_capitoli <- c(
  "01" = "Animales vivos",
  "02" = "Carne",
  "03" = "Pescados y mariscos",
  "04" = "L\u00e1cteos, huevos y miel",
  "05" = "Otros productos de origen animal",
  "06" = "Plantas y flores",
  "07" = "Hortalizas",
  "08" = "Frutas y frutos secos",
  "09" = "Caf\u00e9, t\u00e9 y especias",
  "10" = "Cereales",
  "11" = "Productos de moliner\u00eda",
  "12" = "Semillas y frutos oleaginosos",
  "13" = "Gomas y resinas vegetales",
  "14" = "Materias trenzables vegetales",
  "15" = "Grasas y aceites",
  "16" = "Preparaciones de carne y pescado",
  "17" = "Az\u00facares y confiter\u00eda",
  "18" = "Cacao y sus preparaciones",
  "19" = "Preparaciones de cereales y panader\u00eda",
  "20" = "Preparaciones de hortalizas y frutas",
  "21" = "Preparaciones alimenticias diversas",
  "22" = "Bebidas",
  "23" = "Residuos alimentarios y piensos",
  "24" = "Tabaco",
  "25" = "Sal, azufre, piedras y cemento",
  "26" = "Minerales metal\u00edferos",
  "27" = "Combustibles minerales (petr\u00f3leo y gas)",
  "28" = "Qu\u00edmicos inorg\u00e1nicos (incl. amon\u00edaco)",
  "29" = "Qu\u00edmicos org\u00e1nicos (incl. metanol)",
  "30" = "Productos farmac\u00e9uticos",
  "31" = "Abonos (fertilizantes)",
  "32" = "Tintes, pinturas y barnices",
  "33" = "Aceites esenciales y cosm\u00e9ticos",
  "34" = "Jabones y detergentes",
  "35" = "Materias albuminoideas y colas",
  "36" = "Explosivos",
  "37" = "Productos fotogr\u00e1ficos",
  "38" = "Productos qu\u00edmicos diversos",
  "39" = "Pl\u00e1sticos",
  "40" = "Caucho",
  "41" = "Pieles y cueros",
  "42" = "Manufacturas de cuero",
  "43" = "Peleter\u00eda",
  "44" = "Madera",
  "45" = "Corcho",
  "46" = "Cester\u00eda",
  "47" = "Pasta de papel",
  "48" = "Papel y cart\u00f3n",
  "49" = "Productos editoriales",
  "50" = "Seda",
  "51" = "Lana",
  "52" = "Algod\u00f3n",
  "53" = "Otras fibras vegetales",
  "54" = "Filamentos sint\u00e9ticos",
  "55" = "Fibras sint\u00e9ticas discontinuas",
  "56" = "Guata y cordeles",
  "57" = "Alfombras",
  "58" = "Tejidos especiales",
  "59" = "Tejidos recubiertos",
  "60" = "Tejidos de punto",
  "61" = "Prendas de vestir de punto",
  "62" = "Prendas de vestir (excepto de punto)",
  "63" = "Otros art\u00edculos textiles",
  "64" = "Calzado",
  "65" = "Sombreros",
  "66" = "Paraguas y bastones",
  "67" = "Plumas y flores artificiales",
  "68" = "Manufacturas de piedra y cemento",
  "69" = "Productos cer\u00e1micos",
  "70" = "Vidrio",
  "71" = "Piedras y metales preciosos",
  "72" = "Hierro y acero",
  "73" = "Manufacturas de hierro o acero",
  "74" = "Cobre",
  "75" = "N\u00edquel",
  "76" = "Aluminio",
  "78" = "Plomo",
  "79" = "Cinc",
  "80" = "Esta\u00f1o",
  "81" = "Otros metales comunes",
  "82" = "Herramientas y cuchiller\u00eda",
  "83" = "Manufacturas diversas de metal",
  "84" = "Maquinaria y aparatos mec\u00e1nicos",
  "85" = "M\u00e1quinas y aparatos el\u00e9ctricos",
  "86" = "Material ferroviario",
  "87" = "Veh\u00edculos autom\u00f3viles",
  "88" = "Aeronaves",
  "89" = "Barcos y embarcaciones",
  "90" = "Instrumentos de \u00f3ptica y medida",
  "91" = "Relojer\u00eda",
  "92" = "Instrumentos musicales",
  "93" = "Armas y municiones",
  "94" = "Muebles",
  "95" = "Juguetes y art\u00edculos deportivos",
  "96" = "Manufacturas diversas",
  "97" = "Objetos de arte y antig\u00fcedades",
  "99" = "Mercanc\u00edas no especificadas"
)

# Gruppi di capitoli (stessi di exportaciones_productos.R) -----------------------

gruppo_hs <- function(cap) {
  cap <- as.integer(cap)
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
palette_cat <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300")
colori_gruppi <- setNames(
  c(palette_cat[seq_len(length(livelli_gruppi) - 1)], "#b4b3ad"),
  livelli_gruppi
)

testi <- list(
  exportaciones = list(titolo = "exportaciones por cap\u00edtulo del SA",
                       totale = "total exportado",
                       storico = "evoluci\u00f3n de los principales cap\u00edtulos de exportaci\u00f3n"),
  importaciones = list(titolo = "importaciones por cap\u00edtulo del SA",
                       totale = "total importado",
                       storico = "evoluci\u00f3n de los principales cap\u00edtulos de importaci\u00f3n")
)[[flusso]]

# Dati ------------------------------------------------------------------------
# Si somma per codice: unisce le righe duplicate dovute al cambio di edizione HS.

dati <- read_excel(file_excel, col_types = "text") |>
  pivot_longer(-c(cmdCode, cmdDesc), names_to = "anno", values_to = "valore") |>
  mutate(anno = as.integer(anno), valore = as.numeric(valore),
         cmdCode = sprintf("%02d", as.integer(cmdCode))) |>
  group_by(cmdCode, anno) |>
  summarise(valore = sum(valore, na.rm = TRUE), .groups = "drop") |>
  filter(valore > 0) |>
  mutate(
    capitolo = paste0(cmdCode, " ", coalesce(nomi_capitoli[cmdCode], "")),
    gruppo = factor(gruppo_hs(cmdCode), levels = livelli_gruppi)
  )

anni <- sort(unique(dati$anno))
ultimo_anno <- max(anni)
periodo <- paste0(min(anni), "-", ultimo_anno)

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

# 1. Principali capitoli nell'ultimo anno ----------------------------------------

dati_ultimo <- dati |> filter(anno == ultimo_anno)
totale_ultimo <- sum(dati_ultimo$valore)

top <- dati_ultimo |>
  slice_max(valore, n = n_top, with_ties = FALSE) |>
  mutate(
    capitolo = fct_reorder(capitolo, valore),
    quota = valore / totale_ultimo,
    etichetta = paste0(num_es(valore / 1e6), "  (", num_es(100 * quota, 0.1), "%)")
  )

g_capitoli <- ggplot(top, aes(x = valore, y = capitolo, fill = gruppo)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = etichetta), hjust = 0, nudge_x = max(top$valore) * 0.012,
            size = 3.2, colour = colore_testo_2) +
  scale_fill_manual(values = colori_gruppi, drop = TRUE, name = NULL) +
  scale_x_continuous(labels = \(x) num_es(x / 1e6),
                     expand = expansion(mult = c(0, 0.2))) +
  guides(fill = guide_legend(nrow = if (length(livelli_gruppi) > 5) 3 else 2)) +
  labs(
    title = paste0("Trinidad y Tobago: ", testi$titolo, ", ", ultimo_anno),
    subtitle = paste0(
      "Millones de US$ y participaci\u00f3n en el ", testi$totale, " (US$ ",
      num_es(totale_ultimo / 1e6), " millones).\n",
      "Los ", n_top, " principales cap\u00edtulos concentran el ",
      num_es(100 * sum(top$quota), 0.1), "% del total."
    ),
    x = NULL, y = NULL, caption = fonte
  ) +
  tema +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = colore_griglia, linewidth = 0.4))

ggsave(file.path(cartella_out, paste0("TTO_", flusso, "_HS2_", ultimo_anno, ".png")),
       g_capitoli, width = 10.5, height = 7, dpi = 300, bg = "white")

# 2. Evoluzione dei principali capitoli -----------------------------------------
# Un pannello per capitolo, ognuno con la propria scala verticale.

top_cap <- dati_ultimo |>
  slice_max(valore, n = n_storico, with_ties = FALSE) |>
  pull(cmdCode)

storico <- dati |>
  filter(cmdCode %in% top_cap) |>
  complete(nesting(cmdCode, capitolo), anno = anni) |>
  group_by(cmdCode) |>
  mutate(ordine = valore[anno == ultimo_anno]) |>
  ungroup() |>
  mutate(capitolo = fct_reorder(capitolo, ordine, .desc = TRUE))

scarto <- 0.035 * diff(range(anni))
estremi <- storico |>
  filter(!is.na(valore)) |>
  group_by(capitolo) |>
  filter(anno %in% range(anno)) |>
  mutate(primo = anno == min(anno)) |>
  ungroup() |>
  mutate(x_testo = if_else(primo, anno - scarto, anno + scarto),
         allinea = if_else(primo, 1, 0))

g_storico <- ggplot(storico, aes(x = anno, y = valore)) +
  geom_line(colour = colore_linea, linewidth = 0.9, na.rm = TRUE) +
  geom_point(colour = colore_linea, size = 2, na.rm = TRUE) +
  geom_text(data = estremi,
            aes(x = x_testo, label = num_es(valore / 1e6), hjust = allinea),
            size = 3, colour = colore_testo) +
  facet_wrap(~ capitolo, ncol = 4, scales = "free_y") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks = range(anni),
                     expand = expansion(add = 0.2 * diff(range(anni)) + 0.1)) +
  scale_y_continuous(labels = \(x) num_es(x / 1e6), limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = paste0("Trinidad y Tobago: ", testi$storico, ", ", periodo),
    subtitle = paste0(
      "Millones de US$. Los ", n_storico, " principales cap\u00edtulos del SA en ",
      ultimo_anno, ". Cada panel tiene su propia escala vertical."
    ),
    x = NULL, y = NULL, caption = fonte
  ) +
  tema +
  theme(
    strip.text = element_text(face = "bold", size = 10, colour = colore_testo, hjust = 0),
    panel.spacing.x = unit(14, "pt"),
    panel.spacing.y = unit(16, "pt"),
    axis.text.y = element_text(size = 9, colour = colore_testo_2),
    axis.text.x = element_text(size = 8.5),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = colore_griglia, linewidth = 0.4)
  )

ggsave(file.path(cartella_out, paste0("TTO_", flusso, "_HS2_evolucion_", periodo, ".png")),
       g_storico, width = 12, height = 6.5, dpi = 300, bg = "white")

message("Grafici salvati in ", cartella_out)
