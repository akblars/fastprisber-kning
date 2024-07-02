## Läser in paket ----
library(pxweb)
library(rKolada)
library(dplyr)
library(writexl)
library(dplyr)
library(tidyverse)
library(openxlsx)
library(readxl)
## Hämtar volymutveckling ----
pxweb_query_list <- 
  list("Region"=c(
                   "01",
                   "03",
                   "04",
                   "05",
                   "06",
                   "07",
                   "08",
                   "09",
                   "10",
                   "12",
                   "13",
                   "14",
                   "17",
                   "18",
                   "19",
                   "20",
                   "21",
                   "22",
                   "23",
                   "24",
                   "25"),
       "ContentsCode"=c("NR0105AI"),
       "Tid"=c("*"))

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/NR/NR0105/NR0105A/NR0105ENS2010T01A",
            query = pxweb_query_list)

volymutveckling <- as.data.frame(px_data)
volymutveckling <- volymutveckling %>%
  rename(volymutveckling = `BRP, volymutveckling i procent`) %>%
  mutate(volymutveckling = volymutveckling/100+1)
## Hämtar BRP ----
pxweb_query_list <- 
  list("Region"=c(
                   "01",
                   "03",
                   "04",
                   "05",
                   "06",
                   "07",
                   "08",
                   "09",
                   "10",
                   "12",
                   "13",
                   "14",
                   "17",
                   "18",
                   "19",
                   "20",
                   "21",
                   "22",
                   "23",
                   "24",
                   "25"),
       "ContentsCode"=c("NR0105AP",
                        "NR0105AQ"),
       "SNI2007"=c("A01-F43",
                   "G45-T98"),
       "Tid"=c("*"))

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/NR/NR0105/NR0105A/NR0105ENS2010T03A",
            query = pxweb_query_list)

brp <- as.data.frame(px_data)

brp <- brp %>%
  group_by(region, år) %>%
  summarize(brp_löpande = sum(`BRP, löpande priser, mnkr`),
            medelantal_sysselsatta = sum(`Medelantal sysselsatta, personer i 1000-tal`)) %>%
  ungroup() %>%
  inner_join(volymutveckling)

brp <- as_tibble(brp)

## Bestämmer basår och skapar funktionen "fastpris" ----

basår <- 2022 # Bestämmer vilket basår vi ska utgå ifrån

fastpris <- function(geografi){
  
brp <- brp %>%
  filter(region == geografi)

brpbasår <- brp %>%
  filter(år == basår)

brp_efter_basår <- brp %>% 
  filter(år >= basår) %>%
  mutate(volymutveckling2 = if_else(år == basår, 1, volymutveckling)) %>%
  mutate(volymutveckling2 = cumprod(volymutveckling2))


brp_innan_basår <- brp %>% 
  filter(år <= basår) %>%
  mutate(volymutveckling2 = lead(volymutveckling)) %>%
  filter(år != basår) %>%
  arrange(desc(år)) %>%
  mutate(volymutveckling2 = cumprod(volymutveckling2)) %>%
  arrange(år)

brp_fastpris <- rbind(brp_innan_basår, brp_efter_basår) %>%
  mutate(brp_fastprisberäknad = if_else(år == basår, brpbasår$brp_löpande, if_else(år > basår, brpbasår$brp_löpande*volymutveckling2, brpbasår$brp_löpande
                                                                           /volymutveckling2)))%>%
  select(-volymutveckling2)

} # Skapar funktionen fastpris()

## Använder map för att köra funktionen fastpris för alla regioner, och binda samman i ett dataset ----
län <- unique(brp$region)

datalista <- map(län, fastpris)

data <- do.call(rbind, datalista)

write_xlsx(data, "fastpris.xlsx")

