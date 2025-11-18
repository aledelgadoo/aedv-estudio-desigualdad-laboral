library(readr)
library(dplyr)
library(openxlsx)
library(eurostat)
library(geojsonio)

dataset.name <- "nama_10r_2lp10"


## DATASET ORIGINAL ##

# Ruta local donde guardar el CSV
data_path <- paste0("data/", dataset.name, ".csv")

# Descargar dataset desde Eurostat
data_raw <- get_eurostat(dataset.name, time_format = "raw")

# Guardar localmente como CSV (solo la primera vez)
write_csv(data_raw, paste0("data/raw/", dataset.name, ".csv"))


## INFO DATASET ##
# Leer el Excel remoto
datasets <- read.xlsx("https://ctim.ulpgc.es/AEDV/data/Eurostat.datasets.xlsx") %>%
    as_tibble()  %>% 
    filter(code == dataset.name)   # Filtra la fila que te interesa

# Guardar la fila filtrada como CSV local
write_csv(datasets, paste0("data/processed/", dataset.name, "_info.csv"))

## ARCHIVOS GEOJ ##
geoj0 <- geojson_read("https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_20M_2024_4326_LEVL_0.geojson",  what = "sp")
geoj1 <- geojson_read("https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_20M_2024_4326_LEVL_1.geojson",  what = "sp")
geoj2 <- geojson_read("https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_20M_2024_4326_LEVL_2.geojson",  what = "sp")

# Guardamos los objetos Espaciales (mapas)
saveRDS(geoj0, "data/geoj0.rds")
saveRDS(geoj1, "data/geoj1.rds")
saveRDS(geoj2, "data/geoj2.rds")