# Partner commerciali di Trinidad e Tobago (UN Comtrade)

Script R che usa l'API di UN Comtrade (pacchetto [`comtradr`](https://docs.ropensci.org/comtradr/))
per trovare i principali partner commerciali di Trinidad e Tobago.

## Avvio

1. `install.packages(c("comtradr", "dplyr", "tidyr", "readr"))`
2. Copia `.Renviron.example` in `.Renviron` e inserisci la tua chiave (`COMTRADE_PRIMARY`), poi riavvia R.
3. `source("R/comtrade_tto.R")`. I risultati finiscono in `output/`.

I parametri (anni, numero di partner, anno per il dettaglio prodotti) sono in cima allo script.

## Cosa produce

| File | Contenuto |
|---|---|
| `partner_tto_dichiarati.csv` | import/export per partner dichiarati da Trinidad e Tobago |
| `partner_tto_mirror.csv` | gli stessi flussi dichiarati dai partner (utile per gli anni che TTO non ha ancora pubblicato) |
| `top_partner_tto.csv` | i primi N partner per anno e flusso, con quota % |
| `prodotti_top_partner.csv` | capitoli HS a 2 cifre scambiati con i primi partner |
| `modo_trasporto_tto.csv` | ripartizione per modo di trasporto, se dichiarata |

## Porti

Comtrade **non** contiene dati sui porti: il massimo è il modo di trasporto
(mare, aereo, condotta...), che non tutti i paesi dichiarano. Per i porti servono
altre fonti, ad esempio:

- **US Census Bureau – International Trade API** (`api.census.gov`): import/export USA
  per porto USA e paese partner, utile perché gli Stati Uniti sono un partner principale.
- **Eurostat – statistiche marittime** (`mar_go_*`): merci per porto UE e area partner.
- **Central Statistical Office di Trinidad e Tobago** e le autorità portuali
  (Port of Port of Spain, Point Lisas) per i porti lato TTO.
