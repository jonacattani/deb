# ---- Cella 1: intestazione ----
# ==============================================================================
# Esportazioni di Trinidad e Tobago verso gli Stati Uniti via container
# Fonte: U.S. Census Bureau (USA Trade Online), importazioni USA da TTO, 2025
# ==============================================================================

library(tidyverse)
library(readxl)
library(scales)
library(writexl)

# Cartelle e file
cartella_data    <- "C:/Users/giova/OneDrive/Desktop/CEPAL/Network Analysis/Casos de Estudio/Trinidad and Tobago/Data"
cartella_grafici <- "C:/Users/giova/OneDrive/Desktop/CEPAL/Network Analysis/Casos de Estudio/Trinidad and Tobago/Graficos"
file_census      <- file.path(cartella_data, "Exportaciones_TTO_US_Census.xlsx")

# Dati
exp_tto <- read_excel(file_census)
col_valore <- "Customs Containerized Vessel Value (Gen) ($US)"
anno <- unique(exp_tto$Time)     # un solo anno nel file

# Testi comuni ai grafici
fonte <- "Fuente: elaboración propia con datos del U.S. Census Bureau (USA Trade Online)."

# Formato numeri spagnolo: 1.234,5
num_es <- function(x, acc = 1) number(x, accuracy = acc, big.mark = ".", decimal.mark = ",")

# Colori e tema comuni ai grafici
colore_barre   <- "#2a78d6"
colore_testo   <- "#0b0b0b"
colore_testo_2 <- "#52514e"
colore_griglia <- "#e6e5e1"

tema_barre <- theme_minimal(base_size = 11) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", size = 15, colour = colore_testo,
                              lineheight = 1.1, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10, colour = colore_testo_2,
                                 lineheight = 1.15, margin = margin(b = 14)),
    plot.caption = element_text(size = 8.5, colour = colore_testo_2, hjust = 0,
                                lineheight = 1.2, margin = margin(t = 14)),
    axis.text.y = element_text(size = 10.5, colour = colore_testo, margin = margin(r = 4)),
    axis.text.x = element_text(size = 9, colour = colore_testo_2),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = colore_griglia, linewidth = 0.4),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(20, 40, 16, 16)
  )

# Nomi brevi dei capitoli del Sistema Armonizzato in spagnolo
nomi_capitoli <- c(
  "01" = "Animales vivos", "02" = "Carne", "03" = "Pescados y mariscos", "04" = "Lácteos, huevos y miel",
  "05" = "Otros productos de origen animal", "06" = "Plantas y flores", "07" = "Hortalizas",
  "08" = "Frutas y frutos secos", "09" = "Café, té y especias", "10" = "Cereales",
  "11" = "Productos de molinería", "12" = "Semillas y frutos oleaginosos",
  "13" = "Gomas y resinas vegetales", "14" = "Materias trenzables vegetales",
  "15" = "Grasas y aceites", "16" = "Preparaciones de carne y pescado", "17" = "Azúcares y confitería",
  "18" = "Cacao y sus preparaciones", "19" = "Preparaciones de cereales y panadería",
  "20" = "Preparaciones de hortalizas y frutas", "21" = "Preparaciones alimenticias diversas",
  "22" = "Bebidas", "23" = "Residuos alimentarios y piensos", "24" = "Tabaco",
  "25" = "Sal, azufre, piedras y cemento", "26" = "Minerales metalíferos",
  "27" = "Combustibles minerales (petróleo y gas)", "28" = "Químicos inorgánicos (incl. amoníaco)",
  "29" = "Químicos orgánicos (incl. metanol)", "30" = "Productos farmacéuticos",
  "31" = "Abonos (fertilizantes)", "32" = "Tintes, pinturas y barnices", "33" = "Aceites esenciales y cosméticos",
  "34" = "Jabones y detergentes", "35" = "Materias albuminoideas y colas",
  "36" = "Explosivos", "37" = "Productos fotográficos", "38" = "Productos químicos diversos",
  "39" = "Plásticos", "40" = "Caucho", "41" = "Pieles y cueros", "42" = "Manufacturas de cuero",
  "43" = "Peletería", "44" = "Madera", "45" = "Corcho", "46" = "Cestería",
  "47" = "Pasta de papel", "48" = "Papel y cartón", "49" = "Productos editoriales",
  "50" = "Seda", "51" = "Lana", "52" = "Algodón", "53" = "Otras fibras vegetales",
  "54" = "Filamentos sintéticos", "55" = "Fibras sintéticas discontinuas",
  "56" = "Guata y cordeles", "57" = "Alfombras", "58" = "Tejidos especiales",
  "59" = "Tejidos recubiertos", "60" = "Tejidos de punto", "61" = "Prendas de vestir de punto",
  "62" = "Prendas de vestir (excepto de punto)", "63" = "Otros artículos textiles",
  "64" = "Calzado", "65" = "Sombreros", "66" = "Paraguas y bastones", "67" = "Plumas y flores artificiales",
  "68" = "Manufacturas de piedra y cemento", "69" = "Productos cerámicos",
  "70" = "Vidrio", "71" = "Piedras y metales preciosos", "72" = "Hierro y acero",
  "73" = "Manufacturas de hierro o acero", "74" = "Cobre", "75" = "Níquel",
  "76" = "Aluminio", "78" = "Plomo", "79" = "Cinc", "80" = "Estaño", "81" = "Otros metales comunes",
  "82" = "Herramientas y cuchillería", "83" = "Manufacturas diversas de metal",
  "84" = "Maquinaria y aparatos mecánicos", "85" = "Máquinas y aparatos eléctricos",
  "86" = "Material ferroviario", "87" = "Vehículos automóviles", "88" = "Aeronaves",
  "89" = "Barcos y embarcaciones", "90" = "Instrumentos de óptica y medida",
  "91" = "Relojería", "92" = "Instrumentos musicales", "93" = "Armas y municiones",
  "94" = "Muebles", "95" = "Juguetes y artículos deportivos", "96" = "Manufacturas diversas",
  "97" = "Objetos de arte y antigüedades", "98" = "Disposiciones especiales de clasificación",
  "99" = "Mercancías no especificadas"
)

# ---- Cella 2: nomi dei porti ----
# Dizionario: porto del Census -> nome del nodo in nodes_P_global (colonna "puerto")
# I porti non presenti (aeroporti, valichi di frontiera, porti fuori dalla rete) restano NA
diz_porti <- tribble(
  ~Port,                                 ~puerto,
  "Port Everglades, FL (Port)",          "Port Everglades",
  "Charleston, SC (Port)",               "Charleston",
  "Houston, TX (Port)",                  "Houston",
  "San Juan, PR (Port)",                 "San Juan",
  "Savannah, GA (Port)",                 "Savannah",
  "New York, NY (Port)",                 "New York",
  "Newark, NJ (Port)",                   "Newark",
  "Miami, FL (Port)",                    "Miami",
  "Oakland, CA (Port)",                  "Oakland",
  "West Palm Beach, FL (Port)",          "West Palm Beach",
  "Jacksonville, FL (Port)",             "Jacksonville",
  "Mobile, AL (Port)",                   "Mobile",
  "Charlotte Amalie, VI (Port)",         "Charlotte Amalie",
  "Long Beach, CA (Port)",               "Long Beach",
  "Philadelphia, PA (Port)",             "Philadelphia",
  "Gulfport, MS (Port)",                 "Gulfport",
  "Los Angeles, CA (Port)",              "Los Angeles (incl San Pedro)",
  "Baltimore, MD (Port)",                "Baltimore",
  "Wilmington, NC (Port)",               "Wilmington (NC)",
  "Norfolk-Newport News, VA (Port)",     "Norfolk (incl. Portsmouth)",
  "Port Hueneme, CA (Port)",             "Hueneme",
  "New Orleans, LA (Port)",              "New Orleans",
  "Tampa, FL (Port)",                    "Tampa",
  "Boston, MA (Port)",                   "Boston",
  "Wilmington, DE (Port)",               "Wilmington (DEL)",
  "Freeport, TX (Port)",                 "Freeport (US - Texas)",
  "Anchorage, AK (Port)",                "Anchorage"
)

# Controllo: tutti i nomi del dizionario esistono in nodes_P_global
nodes <- read_csv(file.path(cartella_data, "nodes_P_global.csv"), show_col_types = FALSE)
stopifnot(all(diz_porti$puerto %in% nodes$puerto))

# Aggiunge la colonna "puerto" al dataset
exp_tto <- exp_tto %>%
  left_join(diz_porti, by = "Port")

# Controllo: porti con valore > 0 senza corrispondenza nella rete
exp_tto %>%
  filter(is.na(puerto)) %>%
  group_by(Port) %>%
  summarise(value = sum(.data[[col_valore]], na.rm = TRUE), .groups = "drop") %>%
  filter(value > 0)

# ---- Cella 3: classifica per prodotto ed esportazione ----
# Classifica per prodotto (capitolo HS a 2 cifre), tutti i porti
per_prodotto <- exp_tto %>%
  mutate(codice = substr(Commodities, 1, 2)) %>%
  group_by(codice) %>%
  summarise(value = sum(.data[[col_valore]], na.rm = TRUE), .groups = "drop") %>%
  filter(value > 0) %>%
  mutate(
    capitulo = coalesce(nomi_capitoli[codice], codice),
    quota = 100 * value / sum(value)
  ) %>%
  select(codice, capitulo, value, quota) %>%
  arrange(desc(value))

per_prodotto

# Esportazione
write_xlsx(per_prodotto, file.path(cartella_data, "TTO_US_contenedores_por_producto.xlsx"))

# ---- Cella 4: grafico ----
n_top  <- 15
titolo <- "Trinidad y Tobago: principales productos exportados a Estados Unidos por buque portacontenedores"
nota   <- "Capítulos del Sistema Armonizado. Valor de importación declarado en aduanas de Estados Unidos."

totale <- sum(per_prodotto$value)

top <- per_prodotto %>%
  slice_max(value, n = n_top, with_ties = FALSE) %>%
  mutate(
    capitulo = fct_reorder(capitulo, value),
    etichetta = paste0(num_es(value / 1e6, 0.1), "  (", num_es(quota, 0.1), "%)")
  )

g <- ggplot(top, aes(x = value, y = capitulo)) +
  geom_col(fill = colore_barre, width = 0.62) +
  geom_text(aes(label = etichetta), hjust = 0, nudge_x = max(top$value) * 0.015,
            size = 3.3, colour = colore_testo_2) +
  scale_x_continuous(labels = \(x) num_es(x / 1e6),
                     expand = expansion(mult = c(0, 0.25))) +
  coord_cartesian(clip = "off") +
  labs(
    title = str_wrap(paste0(titolo, ", ", anno), width = 70),
    subtitle = paste0(
      "Millones de US$ y participación en el total exportado por contenedor (US$ ",
      num_es(totale / 1e6, 0.1), " millones).\n",
      "Los ", nrow(top), " principales capítulos concentran el ",
      num_es(sum(top$quota), 0.1), "% del total."
    ),
    x = NULL, y = NULL,
    caption = paste0(nota, "\n", fonte)
  ) +
  tema_barre

ggsave(file.path(cartella_grafici, "TTO_US_contenedores_productos.png"),
       g, width = 10, height = 7, dpi = 300, bg = "white")
