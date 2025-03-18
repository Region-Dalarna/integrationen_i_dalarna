if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       here,
       scales)

# OBS! Ska sättas till FALSE när skriptet går i produktion - men kan stängas av genom att sätta till TRUE för att se att alla skript fungerar som de ska
# skriptet är till för att hantera rcurl-fel och inte vanliga fel som ju inte blir bättre av att man försöker flera gånger. =)
# hoppa_over_felhantering = TRUE

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

Output_mapp = here("Data","/")
Output_mapp_figur = here("Figurer","/")
vald_region = "20"
valt_lan = "20"
spara_diagram_som_bildfiler = FALSE

# Asylsökande 1984-senaste obseravation
source(here("skript/","asylsokande_antal_1984_.R"))
gg_asylsokande_antal <- diagram_asylsokande_tidsserie(output_mapp_figur = Output_mapp_figur,
                                                      spara_figur = spara_diagram_som_bildfiler,
                                                      returnera_data= TRUE)


asylsokande_max_ar = max(asylsokande_df$år)
asylsokande_2015 = format(asylsokande_df %>% filter(år == 2015) %>% .$Antal,big.mark = " ")
asylsokande_senaste_ar_antal = format(asylsokande_df %>% filter(år == max(år)) %>% .$Antal,big.mark = " ")

# Antal utrikes födda och förändring av antalet utrikes/inrikes födda i kommuner samt kumulativ summa på länsnivå
source(here("skript/","diag_utrikes_antal_forandring_kommun.R"))
gg_antal_utrikes <- diagram_utrikes_fodda_tidsserie(output_mapp_figur = Output_mapp_figur,
                                                    spara_figur = spara_diagram_som_bildfiler,
                                                    returnera_data= TRUE)

min_ar_utrikes_antal = min(antal_utrikes_region_df$år)
max_ar_utrikes_antal = max(antal_utrikes_region_df$år)
min_antal_utrikes = format(antal_utrikes_region_df %>% filter(år == min_ar_utrikes_antal) %>%  .$Antal,big.mark = " ")
max_antal_utrikes = antal_utrikes_region_df %>% filter(år == max_ar_utrikes_antal) %>%  .$Antal %>% format(big.mark = " ")

min_ar_utrikes_kumulativ <- min(antal_forandring_lan_kumulativ$år)
max_ar_utrikes_kumulativ <- max(antal_forandring_lan_kumulativ$år)
kumulativ_summa_inrikes <- format(abs(antal_forandring_lan_kumulativ %>% filter(födelseregion == "Född i Sverige",år==max(år)) %>%  .$kumulativ_summa),big.mark = " ")
kumulativ_summa_utrikes <- format(abs(antal_forandring_lan_kumulativ %>% filter(födelseregion == "Utrikes född",år==max(år)) %>%  .$kumulativ_summa),big.mark = " ")


# Största födelseland bland utrikes födda i Dalarna
source(here("skript/","storsta_fodelseland_antal.R"))
gg_storsta_fodelseland <- diagram_storsta_fodelseland(output_mapp_figur = Output_mapp_figur,
                                                      spara_figur = spara_diagram_som_bildfiler,
                                                      returnera_data= TRUE)


fodelseland_forsta_ar <- min(storsta_fodelseland_df$år)
fodelseland_senaste_ar <- max(storsta_fodelseland_df$år)

############################################################
########## Befolkningspyramid för Inrikes/utrikes ##########
############################################################

source("G:/skript/diagram/diag_befpyramid.R")
gg_befpyramid <- diag_befpyramid(geo_vekt = vald_region,
                                 jmfr_linje = "utr_inr",
                                 output_mapp = Output_mapp_figur)

##########################################################################################################################
# Befolkningsgörändring över tid fördelad på komponenterna födelseöverskott, inrikes flyttnetto och invandringsöverskott #
##########################################################################################################################
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_befutv_per_komponent_ar_scb.R")
gg_befforandring_komponenter <- diag_befutv_per_komponent_ar(region_vekt = vald_region,
                                                             x_axis_visa_var_xe_etikett = 4,
                                                             output_mapp = Output_mapp_figur,
                                                             facet_x_axis_storlek = 8,
                                                             skriv_till_diagramfil  = spara_diagram_som_bildfiler,
                                                             returnera_dataframe_global_environment = TRUE)

########################
# Arbetsmarknadsstatus #
########################

######################################
## Sysselsättningsstatus, tidsserie ##
######################################
source(here("skript/","sysselsattningsgrad_tidsserie_linje.R"))
gg_sysselsattningsgrad_tidsserie <- diag_sysselsattningsgrad_tidsserie(output_mapp = Output_mapp_figur,
                                                                       skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                                       returnera_data_rmarkdown= TRUE)

##########
## Län ##
#########
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diagram_arbetsmarknadsstatus_senastear.R")
gg_diagram_arbetsmarknadsstatus_lan <- diagram_arbetsmarknadsstatus(region_vekt = hamtaAllaLan(),
                                                                       kon_klartext = c("kvinnor","män"),
                                                                       alder_klartext = "20-64 år",
                                                                       valda_farger = diagramfarger("kon"),
                                                                       fodelseregion_klartext_vekt = c("inrikes född", "utrikes född"),
                                                                       diag_arbetskraftsdeltagande = FALSE,
                                                                       returnera_data = TRUE,
                                                                       spara_figur = FALSE,
                                                                       data_namm = "arbetsmarknadsstatus_lan_df")

# Variabler sysselsättningsgrad län

# Sysselsättningsgrad inrikes Sverige
syssgrad_inrikes_man_Sverige <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "inrikes född",region == "Sverige",variabel == "sysselsättningsgrad" ) %>%  .$varde)

syssgrad_inrikes_kvinna_Sverige <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född",region == "Sverige",variabel == "sysselsättningsgrad" ) %>%  .$varde)

# Sysselsättningsgrad utrikes Dalarna
syssgrad_utrikes_man_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född",region == "Dalarna",variabel == "sysselsättningsgrad" ) %>%  .$varde)

syssgrad_utrikes_kvinna_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född",region == "Dalarna",variabel == "sysselsättningsgrad" ) %>%  .$varde)

syssgrad_utrikes_skillnad_Dalarna <- round(arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född",region == "Dalarna",variabel == "sysselsättningsgrad" ) %>%  .$varde-arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född",region == "Dalarna",variabel == "sysselsättningsgrad" ) %>%  .$varde,0)

# Variabler arbetslöshet lan

# Arbetslöshet inrikes/utrikes och skillnad inrikes/utrikes Dalarna
arblosthet_inrikes_man_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "inrikes född",region == "Sverige",variabel == "arbetslöshet" ) %>%  .$varde)

arblosthet_inrikes_kvinna_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född",region == "Sverige",variabel == "arbetslöshet" ) %>%  .$varde)

arblosthet_utrikes_man_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född",region == "Sverige",variabel == "arbetslöshet" ) %>%  .$varde)

arblosthet_utrikes_kvinna_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född",region == "Sverige",variabel == "arbetslöshet" ) %>%  .$varde)

arblosthet_kvinnor_utrikes_inrikes_skillnad_Dalarna <- round(arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född",region == "Dalarna",variabel == "arbetslöshet" ) %>%  .$varde-arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född",region == "Dalarna",variabel == "arbetslöshet") %>%  .$varde,0)

arblosthet_man_utrikes_inrikes_skillnad_Dalarna <- round(arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född",region == "Dalarna",variabel == "arbetslöshet" ) %>%  .$varde-arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "inrikes född",region == "Dalarna",variabel == "arbetslöshet") %>%  .$varde,0)

# Högst arbetslöshet län utrikes
arblosthet_utrikes_man_max_lan <- arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region

arblosthet_utrikes_man_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
                                         filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde)

arblosthet_utrikes_kvinna_max_lan <- arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region

arblosthet_utrikes_kvinna_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
                                            filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde)

# Lägst arbetslöshet län utrikes

arblosthet_utrikes_man_min_lan <- arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$region

arblosthet_utrikes_man_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
                                           filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$varde)

arblosthet_utrikes_kvinna_min_lan <- arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$region

arblosthet_utrikes_kvinna_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
                                              filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$varde)


############
## Kommun ##
############
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diagram_arbetsmarknadsstatus_senastear.R")
gg_diagram_arbetsmarknadsstatus_kommun <- diagram_arbetsmarknadsstatus(region_vekt = hamtakommuner(),
                                                                       kon_klartext = c("kvinnor","män"),
                                                                       alder_klartext = "20-64 år",
                                                                       valda_farger = diagramfarger("kon"),
                                                                       fodelseregion_klartext_vekt = c("inrikes född", "utrikes född"),
                                                                       diag_arbetskraftsdeltagande = FALSE,
                                                                       returnera_data = TRUE,
                                                                       spara_figur = FALSE,
                                                                       data_namm = "arbetsmarknadsstatus_kommun_df")

# Variabler sysselsättningsgrad kommun

# Högst sysselsättningsgrad
syssgrad_utrikes_man_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
  filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == max(varde)) %>%  .$region

syssgrad_utrikes_man_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
  filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == max(varde)) %>%  .$varde)

syssgrad_utrikes_kvinna_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
  filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == max(varde)) %>%  .$region

syssgrad_utrikes_kvinna_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
  filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == max(varde)) %>%  .$varde)

# Lägst sysselsättningsgrad

syssgrad_utrikes_man_min_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
  filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == min(varde)) %>%  .$region

syssgrad_utrikes_man_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
                                         filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == min(varde)) %>%  .$varde)

syssgrad_utrikes_kvinna_min_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
  filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == min(varde)) %>%  .$region

syssgrad_utrikes_kvinna_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
                                         filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == min(varde)) %>%  .$varde)

# Variabler arbetslöshet kommun

# Utrikes

arblosthet_utrikes_man_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region

arblosthet_utrikes_man_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
                                         filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde)

arblosthet_utrikes_kvinna_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region

arblosthet_utrikes_kvinna_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
                                         filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde)

arblosthet_utrikes_man_min_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$region

arblosthet_utrikes_man_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
                                           filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$varde)

arblosthet_utrikes_kvinna_min_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$region

arblosthet_utrikes_kvinna_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
                                              filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$varde)

# Inrikes

arblosthet_inrikes_man_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "inrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region

arblosthet_inrikes_man_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "inrikes född") %>%
                                           filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde)

arblosthet_inrikes_kvinna_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region

arblosthet_inrikes_kvinna_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född") %>%
                                              filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde)

########################
# Långtidsarbetslöshet #
########################

source(here("skript/","langtidsarbetsloshet_kon.R"))
gg_langtidsarbetsloshet <- diagram_langtidsarbetslohet_tidsserie(output_mapp_figur = Output_mapp_figur,
                                                                spara_figur = spara_diagram_som_bildfiler,
                                                                returnera_data= TRUE)

langtidsarbetsloshet_ar_min = långtidsarbetslöshet$ar %>% min()
langtidsarbetsloshet_ar_max = långtidsarbetslöshet$ar %>% max()
langtidsarbetsloshet_kvinnor_min = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="kvinnor",ar==min(ar)) %>%  .$varde,1))
langtidsarbetsloshet_kvinnor_max = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="kvinnor",ar==max(ar)) %>%  .$varde,1))
langtidsarbetsloshet_man_min = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="män",ar==min(ar)) %>%  .$varde,1))
langtidsarbetsloshet_man_max = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="män",ar==max(ar)) %>%  .$varde,1))

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

# Utbildningsnivå (bakgrund och åldersgrupper) - hämtad från Figur 20 Kompetensförsörjningsrapporten
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_utb_bakgr_alder_NMS.R", encoding="UTF-8")
gg_utbniva_bakgrund_alder <- diag_utb_niva_bakgr_alder(output_mapp_figur = Output_mapp_figur,
                                                       diag_utb_bakgrund = TRUE,
                                                       diag_utb_alder = TRUE,
                                                       skapa_fil = spara_diagram_som_bildfiler,
                                                       returnera_figur = TRUE,
                                                       returnera_data = TRUE)

#Laddade in det här för att kunna köra raderna 396 och framåt i Rmd-filen
#etablering_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/etableringstid.xlsx")
source(here("skript/","etablering_kon_utbildningsniva.R"))
gg_etablering <- diag_etablering_utb_kon_scb(output_mapp = Output_mapp_figur,
                                            skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                            returnera_data_rmarkdown = TRUE)


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

#############################################
##### Förvärvsinkomst för inrikes/utrikes ####
#############################################
source(here("skript/","forvarvsinkomst_utrikes.R"))
gg_forvarvsinkomst_utrikes <- diag_inkomst_bakgrund_scb(output_mapp = Output_mapp_figur,
                                                        skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                        returnera_data_rmarkdown = TRUE)

#############################################
#### Utbildningsnivå för inrikes/utrikes ####
#############################################
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_utbniva_inr_utr_fodda_lan_scb.R")
gg_utbniva_bakgrund <- funktion_upprepa_forsok_om_fel( function() {
  diag_utbniva_inr_utr_fodda_kon_lan(skriv_diagramfil = spara_diagram_som_bildfiler,
                                     output_mapp = Output_mapp_figur,
                                     returnera_df_rmarkdown = TRUE)
})
