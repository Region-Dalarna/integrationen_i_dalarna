if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       here)

# OBS! Ska sättas till FALSE när skriptet går i produktion - men kan stängas av genom att sätta till TRUE för att se att alla skript fungerar som de ska
# skriptet är till för att hantera rcurl-fel och inte vanliga fel som ju inte blir bättre av att man försöker flera gånger. =)
# hoppa_over_felhantering = TRUE

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

Output_mapp = here("Data","/")
Output_mapp_figur = here("Figurer","/")
vald_region = "20"
valt_lan = "20"
spara_diagram_som_bildfiler = FALSE

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

# Utbildningsnivå (bakgrund och åldersgrupper) - hämtad från Figur 20 Kompetensförsörjningsrapporten
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_utb_bakgr_alder_NMS.R", encoding="UTF-8")
gg_utbniva_bakgrund_alder <- diag_utb_niva_bakgr_alder(output_mapp_figur = Output_mapp_figur,
                                                       diag_utb_bakgrund = TRUE,
                                                       diag_utb_alder = TRUE,
                                                       skapa_fil = spara_diagram_som_bildfiler,
                                                       returnera_figur = TRUE,
                                                       returnera_data = TRUE)


etablering_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/etableringstid.xlsx")

# ## Befolkningspyramid
# source("G:/skript/diagram/diag_befpyramid.R")
# gg_befpyramid <- diag_befpyramid(geo_vekt = c(vald_region),
#                                  jmfr_linje = "ar",
#                                  jmfr_ar = "1968",
#                                  output_mapp = Output_mapp_figur)



# # Utbildningsnivå från 85 och framåt uppdelat på kön. Data hämtas i detta fall från GGplot-objektet (när data används i markdown) FEL
# source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_utbniva_flera_diagram_scb.R")
# gg_utbniva_85 <- diag_utbniva_tidserie_och_lansjmfr(region_vekt = c("20"),
#                                                     output_mapp = Output_mapp_figur,
#                                                     diagram_capt = "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna",
#                                                     skapa_fil = spara_diagram_som_bildfiler,
#                                                     diag_hogutb_over_tid = TRUE,
#                                                     diag_lagutb_over_tid = TRUE,
#                                                     diag_andel_alla_utbnivaer = TRUE,
#                                                     vald_utb_niva = "hogutb")
