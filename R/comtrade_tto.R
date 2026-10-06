# Principali partner commerciali di Trinidad e Tobago (UN Comtrade API)
#
# Requisiti:
#   install.packages(c("comtradr", "dplyr", "tidyr", "readr"))
#   Chiave API in .Renviron:  COMTRADE_PRIMARY=la_tua_chiave
#   (vedi .Renviron.example; poi riavvia R)
#
# Output in output/:
#   partner_tto_dichiarati.csv   commercio dichiarato da Trinidad e Tobago
#   partner_tto_mirror.csv       lo stesso visto dai paesi partner (dati speculari)
#   top_partner_tto.csv          classifica partner per flusso e anno
#   prodotti_top_partner.csv     capitoli HS (2 cifre) scambiati con i top partner
#   modo_trasporto_tto.csv       ripartizione per modo di trasporto (se dichiarata)

library(comtradr)
library(dplyr)
library(tidyr)
library(readr)

# Parametri -------------------------------------------------------------------

paese    <- "TTO"       # ISO3 di Trinidad e Tobago (codice Comtrade 780)
anni     <- 2019:2024   # massimo 12 anni per chiamata
n_top    <- 10          # quanti partner tenere nella classifica
anno_hs  <- 2023        # anno per il dettaglio prodotti e trasporto
cartella <- "output"

if (Sys.getenv("COMTRADE_PRIMARY") == "") {
  stop("Manca COMTRADE_PRIMARY: aggiungi la chiave a .Renviron e riavvia R.")
}
dir.create(cartella, showWarnings = FALSE)

# Helper ----------------------------------------------------------------------

# Se la chiamata non produce righe comtradr restituisce data.frame(count = 0):
# meglio fermarsi subito con un messaggio chiaro.
scarica <- function(...) {
  dati <- ct_get_data(..., verbose = TRUE)
  if (!"primary_value" %in% names(dati)) {
    stop("Comtrade non ha restituito dati per questa richiesta.")
  }
  as_tibble(dati)
}

classifica <- function(dati, n = n_top) {
  dati |>
    group_by(ref_year, flow_desc) |>
    mutate(
      quota_pct = 100 * primary_value / sum(primary_value, na.rm = TRUE),
      rango = min_rank(desc(primary_value))
    ) |>
    filter(rango <= n) |>
    arrange(ref_year, flow_desc, rango) |>
    ungroup()
}

# 1. Commercio dichiarato da Trinidad e Tobago --------------------------------

dichiarati <- scarica(
  reporter = paese,
  partner = "all_countries",   # tutti i paesi, senza aggregati come "World"
  flow_direction = c("Import", "Export"),
  commodity_code = "TOTAL",
  start_date = min(anni),
  end_date = max(anni)
)

partner_dichiarati <- dichiarati |>
  select(ref_year, flow_desc, partner_iso, partner_desc, primary_value)

write_csv(partner_dichiarati, file.path(cartella, "partner_tto_dichiarati.csv"))

anni_disponibili <- sort(unique(partner_dichiarati$ref_year))
message("Anni dichiarati da TTO: ", paste(anni_disponibili, collapse = ", "))

# 2. Dati speculari (mirror): cosa dichiarano i partner -----------------------
# Trinidad e Tobago pubblica con ritardo: i dati dei partner coprono anche
# gli anni mancanti. Import del partner da TTO ~ export di TTO e viceversa.

mirror <- scarica(
  reporter = "all_countries",
  partner = paese,
  flow_direction = c("Import", "Export"),
  commodity_code = "TOTAL",
  start_date = min(anni),
  end_date = max(anni)
)

partner_mirror <- mirror |>
  transmute(
    ref_year,
    # ribalta il flusso nel punto di vista di TTO
    flow_desc = if_else(flow_desc == "Import", "Export", "Import"),
    partner_iso = reporter_iso,
    partner_desc = reporter_desc,
    primary_value
  )

write_csv(partner_mirror, file.path(cartella, "partner_tto_mirror.csv"))

# 3. Classifica dei principali partner ----------------------------------------

top_partner <- bind_rows(
  dichiarati = partner_dichiarati,
  mirror = partner_mirror,
  .id = "fonte"
) |>
  group_by(fonte) |>
  group_modify(~ classifica(.x)) |>
  ungroup()

write_csv(top_partner, file.path(cartella, "top_partner_tto.csv"))

# 4. Cosa si scambia con i top partner (capitoli HS a 2 cifre) -----------------

capitoli_hs <- sprintf("%02d", setdiff(1:97, 77))   # il capitolo 77 non esiste

# solo codici accettati come partner (esclude ad es. "Areas, nes")
iso_validi <- ct_get_ref_table("partner")$iso_3

top_iso <- partner_dichiarati |>
  filter(ref_year == anno_hs, partner_iso %in% iso_validi) |>
  group_by(partner_iso) |>
  summarise(totale = sum(primary_value, na.rm = TRUE), .groups = "drop") |>
  slice_max(totale, n = n_top) |>
  pull(partner_iso)

if (length(top_iso) == 0) {
  warning("Nessun dato TTO per ", anno_hs, ": cambia 'anno_hs'.")
} else {
  prodotti <- scarica(
    reporter = paese,
    partner = top_iso,
    flow_direction = c("Import", "Export"),
    commodity_code = capitoli_hs,
    start_date = anno_hs,
    end_date = anno_hs
  ) |>
    select(ref_year, flow_desc, partner_iso, partner_desc,
           cmd_code, cmd_desc, primary_value, net_wgt) |>
    arrange(flow_desc, partner_iso, desc(primary_value))

  write_csv(prodotti, file.path(cartella, "prodotti_top_partner.csv"))
}

# 5. Modo di trasporto (il dato piu' vicino ai porti che Comtrade offre) -------
# Comtrade NON contiene i porti. Alcuni paesi dichiarano il modo di trasporto
# (mare, aereo, condotta...); se TTO non lo fa, qui resta solo "TOTAL".

trasporto <- scarica(
  reporter = paese,
  partner = "all_countries",
  flow_direction = c("Import", "Export"),
  commodity_code = "TOTAL",
  mode_of_transport = "everything",
  start_date = anno_hs,
  end_date = anno_hs
) |>
  select(ref_year, flow_desc, partner_iso, partner_desc,
         mot_code, mot_desc, primary_value)

write_csv(trasporto, file.path(cartella, "modo_trasporto_tto.csv"))

trasporto |>
  group_by(flow_desc, mot_desc) |>
  summarise(valore = sum(primary_value, na.rm = TRUE), .groups = "drop") |>
  print()

message("File salvati in ", normalizePath(cartella))
