diagram_ekonomisk_standard <-function(region_vekt = c("20"),# Max 1,
                                     alder_klartext = "20-64 år",			 #  Finns: "20- år", "20-64 år", "65- år", "20-29 år", "30-49 år", "50-64 år", "65-79 år", "80- år"
                                     output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp för figur
                                     skriv_diagrambildfil = FALSE, # Sparar figuren till output_mapp_figur
                                     returnera_figur = TRUE, # Returnerar en figur
                                     jmf_ar = 2017, # År att jämföra senaste år med
                                     antal_lander = 10, # Antal länder som skall visas i diagrammet
                                     diag_fargvekt = NA,
                                     returnera_dataframe_global_environment = FALSE) # Skall data returneras)
{

  ## =================================================================================================================
  # Ett diagram för största födelseland bland utrikes födda i regionen. Går att jämföra tre år (första, sista och jämförelseår)
  # eller bara titta på sista år
  # =================================================================================================================
  # Skript som skapar tre diagram kopplade till låg ekonomisk standard

  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here)

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_ekonomisk_standard_region_alder_sysselsattning_utlbakgrund_inkomsttyp_tid_HE0110__HE0110F_scb.R", encoding = "utf-8", echo = FALSE)

  options(dplyr.summarise.inform = FALSE)

  gg_list <- list()

  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- diagramfarger("rus_sex")
    } else {
      diag_fargvekt <- hue_pal()(9)
    }
  }

  # Hämta data
  ekonomisk_standard_bakgrund_df <- hamta_ekonomisk_standard_region_alder_sysselsattning_utlbakgrund_inkomsttyp_tid_scb(region_vekt = c(region_vekt),
                                                                                                               alder_klartext = alder_klartext,
                                                                                                               inkomsttyp_klartext = "disponibel inkomst per k.e. inkl. kapitalvinst",
                                                                                                               sysselsattning_klartext = c("samtliga personer", "förvärvsarbetande", "icke förvärvsarbetande"),
                                                                                                               cont_klartext = "Inkomst < 60 procent",
                                                                                                               utlbakgrund_klartext = c("utrikes födda","född i Sverige"),
                                                                                                               tid_koder = c(jmf_ar,"9999")) %>%
    mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE),
           sysselsättning = paste0(toupper(substr(sysselsättning,1,1)),substr(sysselsättning,2,nchar(sysselsättning))),
           `utländsk/svensk bakgrund` = ifelse(`utländsk/svensk bakgrund` == "född i Sverige","inrikes födda",`utländsk/svensk bakgrund`)) %>%
      rename(bakgrund = `utländsk/svensk bakgrund`)

  if(returnera_dataframe_global_environment == TRUE){
    assign("lag_ek_standard_bakgrund_df", ekonomisk_standard_bakgrund_df, envir = .GlobalEnv)
  }
  diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"

  ekonomisk_standard_bakgrund_df$sysselsättning <- factor(ekonomisk_standard_bakgrund_df$sysselsättning,
                                                          levels = c("Samtliga personer","Förvärvsarbetande","Icke förvärvsarbetande"))

  regioner <- paste(unique(ekonomisk_standard_bakgrund_df$region), collapse = "_")

  diagram_titel = paste0("Andel personer i hushåll med låg ekonomisk standard i ",unique(ekonomisk_standard_bakgrund_df$region))
  diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Med låg ekonomisk standard menas att\ninkomsten är lägre än 60 procent av medianen."
  diagramfilnamn <- paste0("diagram_lagekstandard_bakgrund_",regioner,".png")

  gg_obj <- SkapaStapelDiagram(skickad_df = ekonomisk_standard_bakgrund_df,
                               skickad_x_var = "sysselsättning",
                               skickad_y_var = "Inkomst < 60 procent",
                               skickad_x_grupp = "år",
                               output_mapp = output_mapp_figur,
                               filnamn_diagram = diagramfilnamn,
                               diagram_capt = diagram_capt,
                               diagram_titel = diagram_titel,
                               diagram_facet = TRUE,
                               # manual_x_axis_text_vjust = 1,
                               # manual_x_axis_text_hjust = 1,
                               facet_grp =  "bakgrund",
                               x_axis_lutning = 0,
                               facet_scale = "fixed",
                               procent_0_100_10intervaller = TRUE,
                               facet_legend_bottom = TRUE,
                               manual_color = diag_fargvekt,
                               manual_y_axis_title = "procent",
                               lagg_pa_logga = FALSE,
                               skriv_till_diagramfil = skriv_diagrambildfil)

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")


  if(returnera_figur == TRUE){
    return(gg_list)
  }

}
