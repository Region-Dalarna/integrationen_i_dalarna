diag_sysselsattningsgrad_tidsserie <- function(region = "20", # Enbart ett i taget.
                                               visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                               logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                               output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                               skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                               excel_mapp = NA,                                   # mapp där excelfil ska sparas, NA = sparas ingen fil
                                               returnera_data_rmarkdown = FALSE,
                                               demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
        ) {


  # =======================================================================================================================
  #
  # Ett diagram för sysselsättningsgrad som linjediagram över tid
  #
  #
  #
  # =======================================================================================================================

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/medellivslangd_aterstaende_vid_30 år_alder_Dalarna_ar2012-2016_2019-2023.png")
    walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    stop_tyst()
  }

  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue)

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_syss_rams_bas_fran_ar_1993_region_inrikesutrikes_kon_tid_RAMSForvInt03_RAMSForvInt04_RamsForvInt04N_ArRegArbStatus_scb.R")

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 readxl)

  # Före 2022
  sysselsattningsgrad_tidsserie_df <- hamta_rams_bas_region_inrikesutrikes_kon_tid_scb(region_vekt = region,
                                                                                       kon_klartext = c("kvinnor", "män"),
                                                                                       inrikesutrikes_klartext = c("inrikes födda", "utrikes födda"),
                                                                                       tid_koder = "*") %>%
              mutate(kombo = paste0(födelseregion," ",kön),
                     region = skapa_kortnamn_lan(region)) %>%
                select(år,region,kombo,ålder,sysselsättningsgrad)


  if(returnera_data_rmarkdown == TRUE){
    assign("sysselsattningsgrad_tidsserie_df", sysselsattningsgrad_tidsserie_df, envir = .GlobalEnv)
  }

  gg_list <- list()


  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."

  diagramtitel <- paste0("Sysselsättningsgrad ",unique(sysselsattningsgrad_tidsserie_df$ålder), " i ",unique(sysselsattningsgrad_tidsserie_df$region))
  #diagramtitel <- str_wrap(diagramtitel,60)
  diagramfilnamn <- paste0("sysselsattningsgrad_tidsserie_",unique(sysselsattningsgrad_tidsserie_df$region),".png")

  # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
  gg_obj <- SkapaLinjeDiagram(skickad_df =sysselsattningsgrad_tidsserie_df,
                               skickad_x_var = "år",
                               skickad_y_var = "sysselsättningsgrad",
                               skickad_x_grupp = "kombo",
                               # manual_x_axis_text_vjust=0.9,
                               manual_color = diagramfarger("rus_sex"),
                               diagram_titel = diagramtitel,
                               diagram_capt =  diagram_capt,
                               manual_y_axis_title = "procent",
                               y_axis_100proc = TRUE,
                               x_axis_lutning = 45,
                               legend_kolumner = 2,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               lagg_pa_logga = visa_logga_i_diagram,
                               skriv_till_diagramfil = skriv_diagrambildfil)


  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")

  return(gg_list)

}
