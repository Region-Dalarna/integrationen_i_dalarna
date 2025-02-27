if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       here)

# OBS! Ska sättas till FALSE när skriptet går i produktion - men kan stängas av genom att sätta till TRUE för att se att alla skript fungerar som de ska
# skriptet är till för att hantera rcurl-fel och inte vanliga fel som ju inte blir bättre av att man försöker flera gånger. =)
hoppa_over_felhantering = TRUE


source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

# Utbildningsnivå från 85 och framåt uppdelat på kön. Data hämtas i detta fall från GGplot-objektet (när data används i markdown) FEL
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_utbniva_flera_diagram_scb.R")
#gg_utbniva_85 <- diag_utbniva_tidserie_och_lansjmfr(region_vekt = c("20"),
                                                    output_mapp = Output_mapp_figur,
                                                    diagram_capt = "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna",
                                                    skapa_fil = spara_diagram_som_bildfiler,
                                                    diag_hogutb_over_tid = TRUE,
                                                    diag_lagutb_over_tid = TRUE,
                                                    diag_andel_alla_utbnivaer = TRUE,
                                                    vald_utb_niva = "hogutb")
