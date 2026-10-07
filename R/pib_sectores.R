# Composizione del PIL di Trinidad e Tobago per settore (prezzi costanti).
# Fonte: CSO, Table 2.2A2 "Gross Domestic Product by Economic Activity -
# Percentage Contribution (Constant Prices)".
#
# Produce cinque grafici:
#   1. composizione del PIL nell'ultimo anno (barre orizzontali)
#   2. cambiamento della struttura tra il primo e l'ultimo anno (dumbbell)
#   3. peso del settore energetico nel PIL nel tempo (aree impilate)
#   4. composizione del settore energetico nell'ultimo anno (barre per sottosettore)
#   5. composizione del PIL con la parte energetica evidenziata in ogni settore
#
# Pacchetti: install.packages(c("tidyverse", "readxl", "scales", "ragg"))

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)

# Parametri -------------------------------------------------------------------

file_excel   <- "C:/Users/giova/OneDrive/Desktop/CEPAL/Network Analysis/Casos de Estudio/Trinidad and Tobago/Data/Constant-Price-AGDP-2025-Percentage-Contribution.xlsx"
cartella_out <- "C:/Users/giova/OneDrive/Desktop/CEPAL/Network Analysis/Casos de Estudio/Trinidad and Tobago/Graficos"
fonte <- "Fuente: elaboraci\u00f3n propia con datos de la Central Statistical Office (CSO) de Trinidad y Tobago."

colore_barre   <- "#2a78d6"
colore_chiaro  <- "#a9c8ef"   # stessa tonalita' piu' chiara, per l'anno iniziale
colore_testo   <- "#0b0b0b"
colore_testo_2 <- "#52514e"
colore_griglia <- "#e6e5e1"
# palette categorica (5 colori, ordine fisso) per i sottosettori energetici
colori_energia <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4")

dir.create(cartella_out, showWarnings = FALSE, recursive = TRUE)

# Nomi dei settori in spagnolo, per codice ISIC ---------------------------------

nomi_settori <- c(
  A = "Agricultura, silvicultura y pesca",
  B = "Explotaci\u00f3n de minas y canteras",
  C = "Industria manufacturera",
  D = "Electricidad y gas",
  E = "Agua y saneamiento",
  F = "Construcci\u00f3n",
  G = "Comercio y reparaciones",
  H = "Transporte y almacenamiento",
  I = "Alojamiento y servicios de comida",
  J = "Informaci\u00f3n y comunicaciones",
  K = "Actividades financieras y de seguros",
  L = "Actividades inmobiliarias",
  M = "Actividades profesionales y t\u00e9cnicas",
  N = "Servicios administrativos y de apoyo",
  O = "Administraci\u00f3n p\u00fablica",
  P = "Ense\u00f1anza",
  Q = "Salud y asistencia social",
  R = "Artes y entretenimiento",
  S = "Otras actividades de servicios",
  T = "Servicio dom\u00e9stico"
)

# Sottosettori energetici (righe "Of which" in fondo alla tabella), raggruppati
gruppi_energia <- c(
  B1 = "Petr\u00f3leo crudo y condensado",
  B2 = "Petr\u00f3leo crudo y condensado",
  B3 = "Gas natural",
  C1 = "Refinaci\u00f3n y GNL",
  C2 = "Petroqu\u00edmica",
  G1 = "Distribuci\u00f3n y servicios petroleros",
  B4 = "Distribuci\u00f3n y servicios petroleros",
  B5 = "Distribuci\u00f3n y servicios petroleros"
)

# Dati ------------------------------------------------------------------------
# Riga 3 = anni, dalla riga 5 = settori. Colonna 1 = nome, colonna 2 = codice ISIC.

grezzo <- read_excel(file_excel, col_names = FALSE, skip = 2)
anni <- as.integer(unlist(grezzo[1, -(1:2)]))

pil <- grezzo[-1, ] |>
  setNames(c("settore_en", "isic", anni)) |>
  mutate(isic = trimws(isic)) |>
  filter(!is.na(isic), isic != "0") |>
  pivot_longer(-c(settore_en, isic), names_to = "anno", values_to = "quota") |>
  mutate(anno = as.integer(anno), quota = as.numeric(quota))

settori <- pil |>
  filter(isic %in% names(nomi_settori)) |>
  mutate(settore = nomi_settori[isic])

primo_anno  <- min(anni)
ultimo_anno <- max(anni)

num_es <- function(x, acc = 0.1) number(x, accuracy = acc, big.mark = ".", decimal.mark = ",")

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
    axis.text.y = element_text(size = 10, colour = colore_testo),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(18, 22, 14, 14)
  )

# 1. Composizione nell'ultimo anno ----------------------------------------------

ultimo <- settori |>
  filter(anno == ultimo_anno) |>
  mutate(settore = fct_reorder(settore, quota))

imposte <- pil |> filter(isic == "T&S", anno == ultimo_anno) |> pull(quota)

g_composizione <- ggplot(ultimo, aes(x = quota, y = settore)) +
  geom_col(fill = colore_barre, width = 0.62) +
  geom_text(aes(label = paste0(num_es(quota), "%")), hjust = 0,
            nudge_x = 0.3, size = 3.2, colour = colore_testo_2) +
  scale_x_continuous(labels = label_number(decimal.mark = ",", big.mark = ".", suffix = "%"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = paste0("Trinidad y Tobago: composici\u00f3n del PIB por actividad econ\u00f3mica, ",
                   ultimo_anno),
    subtitle = paste0(
      "Participaci\u00f3n porcentual en el PIB a precios constantes. ",
      "Los impuestos netos de subvenciones representan el ", num_es(imposte), "% restante."
    ),
    x = NULL, y = NULL, caption = fonte
  ) +
  tema +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = colore_griglia, linewidth = 0.4))

ggsave(file.path(cartella_out, paste0("TTO_PIB_composicion_", ultimo_anno, ".png")),
       g_composizione, width = 9, height = 7.5, dpi = 300, bg = "white")

# 2. Cambiamento della struttura: primo vs ultimo anno ---------------------------

confronto <- settori |>
  filter(anno %in% c(primo_anno, ultimo_anno)) |>
  select(settore, anno, quota) |>
  pivot_wider(names_from = anno, values_from = quota, names_prefix = "a") |>
  rename(inizio = paste0("a", primo_anno), fine = paste0("a", ultimo_anno)) |>
  mutate(
    variazione = fine - inizio,
    settore = fct_reorder(settore, fine),
    etichetta = paste0(if_else(variazione >= 0, "+", ""), num_es(variazione), " p.p.")
  )

g_confronto <- ggplot(confronto, aes(y = settore)) +
  geom_segment(aes(x = inizio, xend = fine, yend = settore),
               colour = colore_griglia, linewidth = 2.2) +
  geom_point(aes(x = inizio), colour = colore_chiaro, size = 3) +
  geom_point(aes(x = fine), colour = colore_barre, size = 3) +
  geom_text(aes(x = pmax(inizio, fine), label = etichetta), hjust = 0,
            nudge_x = 0.6, size = 3.1, colour = colore_testo_2) +
  scale_x_continuous(labels = label_number(decimal.mark = ",", big.mark = ".", suffix = "%"),
                     expand = expansion(mult = c(0.02, 0.14))) +
  labs(
    title = paste0("Trinidad y Tobago: cambio en la estructura del PIB, ",
                   primo_anno, " y ", ultimo_anno),
    subtitle = paste0(
      "Participaci\u00f3n porcentual en el PIB a precios constantes.\n",
      "Punto claro: ", primo_anno, "; punto oscuro: ", ultimo_anno,
      ". A la derecha, variaci\u00f3n en puntos porcentuales."
    ),
    x = NULL, y = NULL, caption = fonte
  ) +
  tema +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = colore_griglia, linewidth = 0.4))

ggsave(file.path(cartella_out, paste0("TTO_PIB_cambio_", primo_anno, "_", ultimo_anno, ".png")),
       g_confronto, width = 9, height = 7.5, dpi = 300, bg = "white")

# 3. Peso del settore energetico nel tempo ----------------------------------------

energia <- pil |>
  filter(isic %in% names(gruppi_energia)) |>
  mutate(gruppo = gruppi_energia[isic]) |>
  group_by(anno, gruppo) |>
  summarise(quota = sum(quota, na.rm = TRUE), .groups = "drop") |>
  mutate(gruppo = factor(gruppo, levels = unique(gruppi_energia)))

totale_energia <- energia |>
  group_by(anno) |>
  summarise(quota = sum(quota), .groups = "drop")

# etichette a destra dell'ultimo anno, al centro di ogni fascia
etichette_energia <- energia |>
  filter(anno == ultimo_anno) |>
  arrange(desc(gruppo)) |>
  mutate(y = cumsum(quota) - quota / 2,
         testo = paste0(gruppo, ": ", num_es(quota), "%"))

g_energia <- ggplot(energia, aes(x = anno, y = quota, fill = gruppo)) +
  geom_area(colour = "white", linewidth = 0.4) +
  geom_line(data = totale_energia, aes(x = anno, y = quota), inherit.aes = FALSE,
            colour = colore_testo, linewidth = 0.5) +
  geom_text(data = totale_energia |> filter(anno %in% c(primo_anno, ultimo_anno)),
            aes(x = anno, y = quota, label = paste0(num_es(quota), "%")),
            inherit.aes = FALSE, vjust = -0.8, size = 3.4, fontface = "bold",
            colour = colore_testo) +
  geom_text(data = etichette_energia, aes(x = ultimo_anno + 0.25, y = y, label = testo),
            inherit.aes = FALSE, hjust = 0, size = 3.1, colour = colore_testo_2) +
  scale_fill_manual(values = colori_energia, name = NULL) +
  scale_x_continuous(breaks = seq(primo_anno, ultimo_anno, by = 2),
                     expand = expansion(add = c(0.2, 4.2))) +
  scale_y_continuous(labels = label_number(decimal.mark = ",", big.mark = ".", suffix = "%"),
                     expand = expansion(mult = c(0, 0.1))) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0("Trinidad y Tobago: peso del sector energ\u00e9tico en el PIB, ",
                   primo_anno, "-", ultimo_anno),
    subtitle = paste0(
      "Participaci\u00f3n porcentual en el PIB a precios constantes. ",
      "La l\u00ednea negra indica el total del sector energ\u00e9tico."
    ),
    x = NULL, y = NULL, caption = fonte
  ) +
  tema +
  theme(legend.position = "top", legend.justification = "left",
        legend.text = element_text(size = 9.5, colour = colore_testo),
        legend.key.size = unit(10, "pt"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = colore_griglia, linewidth = 0.4))

ggsave(file.path(cartella_out, paste0("TTO_PIB_energia_", primo_anno, "_", ultimo_anno, ".png")),
       g_energia, width = 11, height = 6.5, dpi = 300, bg = "white")

# 4. Composizione del settore energetico nell'ultimo anno -------------------------

nomi_energia <- c(
  B1 = "Extracci\u00f3n de petr\u00f3leo crudo",
  B2 = "Extracci\u00f3n de condensado",
  B3 = "Extracci\u00f3n de gas natural",
  B4 = "Servicios de apoyo petrolero y asfalto",
  B5 = "Servicios de apoyo petrolero y asfalto",
  C1 = "Refinaci\u00f3n (incl. GNL)",
  C2 = "Petroqu\u00edmica",
  G1 = "Distribuci\u00f3n de petr\u00f3leo y gas natural"
)
fase_energia <- c(
  B = "Extracci\u00f3n (miner\u00eda)",
  C = "Transformaci\u00f3n (manufactura)",
  G = "Distribuci\u00f3n (comercio)"
)

sottosettori <- pil |>
  filter(isic %in% names(nomi_energia), anno == ultimo_anno) |>
  mutate(nome = nomi_energia[isic], fase = substr(isic, 1, 1)) |>
  group_by(nome, fase) |>
  summarise(quota = sum(quota), .groups = "drop") |>
  mutate(
    nome = fct_reorder(nome, quota),
    fase = factor(fase_energia[fase], levels = fase_energia),
    quota_settore = 100 * quota / sum(quota),
    etichetta = paste0(num_es(quota), "% del PIB  (", num_es(quota_settore, 1),
                       "% del sector)")
  )

g_sottosettori <- ggplot(sottosettori, aes(x = quota, y = nome, fill = fase)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = etichetta), hjust = 0, nudge_x = 0.08,
            size = 3.2, colour = colore_testo_2) +
  scale_fill_manual(values = colori_energia[1:3], name = NULL) +
  scale_x_continuous(labels = label_number(decimal.mark = ",", big.mark = ".", suffix = "%"),
                     expand = expansion(mult = c(0, 0.45))) +
  labs(
    title = paste0("Trinidad y Tobago: composici\u00f3n del sector energ\u00e9tico, ",
                   ultimo_anno),
    subtitle = paste0(
      "Participaci\u00f3n porcentual en el PIB a precios constantes. El sector energ\u00e9tico ",
      "representa el ", num_es(sum(sottosettori$quota)), "% del PIB.\n",
      "Entre par\u00e9ntesis, participaci\u00f3n en el total del sector energ\u00e9tico."
    ),
    x = NULL, y = NULL, caption = fonte
  ) +
  tema +
  theme(legend.position = "top", legend.justification = "left",
        legend.text = element_text(size = 9.5, colour = colore_testo),
        legend.key.size = unit(10, "pt"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = colore_griglia, linewidth = 0.4))

ggsave(file.path(cartella_out, paste0("TTO_PIB_energia_composicion_", ultimo_anno, ".png")),
       g_sottosettori, width = 10, height = 5.5, dpi = 300, bg = "white")

# 5. PIL per settore con la parte energetica evidenziata -------------------------
# L'energia non e' una sezione ISIC: sta dentro minas (B), manufactura (C) e
# comercio (G). Qui ogni barra e' divisa in parte energetica e resto.

parte_energia <- pil |>
  filter(isic %in% names(nomi_energia), anno == ultimo_anno) |>
  mutate(sezione = substr(isic, 1, 1)) |>
  group_by(sezione) |>
  summarise(energia = sum(quota), .groups = "drop")

evidenziato <- ultimo |>
  mutate(settore = as.character(settore)) |>
  left_join(parte_energia, by = c("isic" = "sezione")) |>
  mutate(energia = coalesce(energia, 0), resto = quota - energia,
         settore = fct_reorder(settore, quota)) |>
  pivot_longer(c(energia, resto), names_to = "parte", values_to = "valore") |>
  mutate(parte = factor(parte, levels = c("resto", "energia"),
                        labels = c("Resto de la actividad", "Sector energ\u00e9tico")))

etichette_evid <- evidenziato |>
  group_by(settore) |>
  summarise(totale = sum(valore),
            energia = sum(valore[parte == "Sector energ\u00e9tico"]), .groups = "drop") |>
  mutate(testo = if_else(energia > 0,
                         paste0(num_es(totale), "%  (energ\u00eda: ", num_es(energia), "%)"),
                         paste0(num_es(totale), "%")))

g_evidenziato <- ggplot(evidenziato, aes(x = valore, y = settore, fill = parte)) +
  geom_col(width = 0.62) +
  geom_text(data = etichette_evid, aes(x = totale, y = settore, label = testo),
            inherit.aes = FALSE, hjust = 0, nudge_x = 0.3, size = 3.1,
            colour = colore_testo_2) +
  scale_fill_manual(values = c("Resto de la actividad" = colore_chiaro,
                               "Sector energ\u00e9tico" = colore_barre),
                    breaks = c("Sector energ\u00e9tico", "Resto de la actividad"),
                    name = NULL) +
  scale_x_continuous(labels = label_number(decimal.mark = ",", big.mark = ".", suffix = "%"),
                     expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = paste0("Trinidad y Tobago: composici\u00f3n del PIB y peso de la energ\u00eda, ",
                   ultimo_anno),
    subtitle = paste0(
      "Participaci\u00f3n porcentual en el PIB a precios constantes. ",
      "La parte energ\u00e9tica suma el ", num_es(sum(parte_energia$energia)), "% del PIB:\n",
      "extracci\u00f3n en miner\u00eda, refinaci\u00f3n y petroqu\u00edmica en manufactura, ",
      "distribuci\u00f3n de combustibles en comercio."
    ),
    x = NULL, y = NULL, caption = fonte
  ) +
  tema +
  theme(legend.position = "top", legend.justification = "left",
        legend.text = element_text(size = 9.5, colour = colore_testo),
        legend.key.size = unit(10, "pt"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = colore_griglia, linewidth = 0.4))

ggsave(file.path(cartella_out, paste0("TTO_PIB_composicion_energia_", ultimo_anno, ".png")),
       g_evidenziato, width = 10, height = 7.5, dpi = 300, bg = "white")

message("Grafici salvati in ", cartella_out)
