library(dplyr)
library(readxl)
library(janitor)

cut_comunas <- read.csv2("datos/comunas_chile_cut.csv")

plebiscito_2022_0 <- read_xlsx("datos/2022_PlebiscitoConstitucional_DatosPlebiscito.xlsx")

plebiscito_2022_1 <- plebiscito_2022_0 |> 
  janitor::row_to_names(4) |> 
  clean_names()

# corregir nombres de comunas
plebiscito_2022_2 <- plebiscito_2022_1 |> 
  select(-region, -nro_region) |> 
  # corregir comunas
  mutate(comuna = recode(comuna,
                         "PAIHUANO" = "PAIGUANO",
                         "AYSEN" = "AISEN",
                         "COYHAIQUE" = "COIHAIQUE",
                         "TREHUACO" = "TREGUACO",
                         "MARCHIGUE" = "MARCHIHUE",
                         "OLLAGUE" = "OLLAGÜE",
                         "CABO DE HORNOS(EX-NAVARINO)" = "CABO DE HORNOS")) |> 
  # agregar cut
  left_join(cut_comunas |> 
              select(comuna, cut_comuna) |> 
              mutate(comuna = chartr("ÁÉÍÓÚ", "AEIOU", toupper(comuna))), #toupper(comuna)), 
                     by = "comuna") |> 
  select(opciones, seleccion, votos, cut_comuna) |> 
  left_join(cut_comunas, by = "cut_comuna")

# sumar por comuna
plebiscito_2022_3 <- plebiscito_2022_2 |> 
  mutate(votos = as.numeric(votos)) |> 
  summarize(votos = sum(votos), 
            .by = c(cut_comuna, comuna, cut_region, region, opciones))
    
# listo
plebiscito_2022_3