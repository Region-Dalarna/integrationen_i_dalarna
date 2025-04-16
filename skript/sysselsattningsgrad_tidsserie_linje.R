diag_sysselsattningsgrad_tidsserie <- function(region = "20", # Enbart ett i taget.
                                               diag_stapel = TRUE,
                                               diag_linje = TRUE,
                                               visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                               logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                               output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                               skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                               returnera_data_rmarkdown = FALSE,
                                               demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
        ) {


  # =======================================================================================================================
  #
  # Två diagram:
  #  - sysselsättningsgrad som ett linjediagram över tid och ett stapeldiagram där sysselsättningsgrad
  #  - Stapeldiagram där man kan jämföra sysselsättningsgraden för senaste år med ett valfritt år
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
  sysselsatta <- hamta_rams_bas_region_inrikesutrikes_kon_tid_scb(region_vekt = region,
                                                                                       kon_klartext = c("*"),
                                                                                       inrikesutrikes_klartext = c("inrikes födda", "utrikes födda"),
                                                                                       tid_koder = "*") %>%
              mutate(kombo = paste0(födelseregion," ",kön),
                     region = skapa_kortnamn_lan(region)) %>%
                select(år,region,kön,födelseregion,kombo,ålder,sysselsättningsgrad)

  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_data_arbetsmarknadsstatus_bas_ar_prel.R")
  sysselsatta_bas_preliminär = hamta_arbetsmarknadsstatus_bas_ar_prel(region = region,
                                                                      kon_klartext = c("*"),
                                                                      fodelseregion_klartext = c("inrikes född", "utrikes född"),
                                                                      cont_klartext = "sysselsättningsgrad",
                                                                      alder_klartext = "20-64 år") %>%
    mutate(region = skapa_kortnamn_lan(region),
           kön = ifelse(kön == "totalt","kvinnor och män",kön),
           födelseregion = case_when(
             födelseregion == "inrikes född" ~ "inrikes födda",
             födelseregion == "utrikes född" ~ "utrikes födda",
             TRUE ~ födelseregion
           ),
           kombo = paste0(födelseregion," ",kön)) %>%
      select(år,region,kön,födelseregion,kombo,ålder,sysselsättningsgrad)


  sysselsattningsgrad_tidsserie_df <- rbind(sysselsatta, sysselsatta_bas_preliminär %>% filter(!(år%in%unique(sysselsatta$år))))

  gg_list <- list()

  if(diag_stapel){
    diagram_capt <- "Källa: SCB, RAMS och BAS.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Data för senaste år är preliminär."

    if(returnera_data_rmarkdown == TRUE){
      assign("sysselsattningsgrad_tidsserie_jmf_2017_df", sysselsattningsgrad_tidsserie_df %>% filter(år %in% c(2017,max(år)),kön == "kvinnor och män"), envir = .GlobalEnv)
    }

    diagramtitel <- paste0("Sysselsättningsgrad ",unique(sysselsattningsgrad_tidsserie_df$ålder), " i ",unique(sysselsattningsgrad_tidsserie_df$region))
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("sysselsattningsgrad_jmf_2017_",unique(sysselsattningsgrad_tidsserie_df$region),".png")

    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaStapelDiagram(skickad_df =sysselsattningsgrad_tidsserie_df %>%
                                  filter(år %in% c(2017,max(år)),
                                         kön == "kvinnor och män"),
                                 skickad_x_var = "år",
                                 skickad_y_var = "sysselsättningsgrad",
                                 skickad_x_grupp = "födelseregion",
                                 # manual_x_axis_text_vjust=0.9,
                                 manual_color = diagramfarger("rus_sex"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 y_axis_100proc = TRUE,
                                 x_axis_lutning = 0,
                                 #legend_kolumner = 2,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 skriv_till_diagramfil = skriv_diagrambildfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }


  if(diag_linje){
    #diagram_capt <- "Källa: SCB, RAMS och BAS.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Data för senaste år är preliminär."

    if(returnera_data_rmarkdown == TRUE){
      assign("sysselsattningsgrad_tidsserie_df", sysselsattningsgrad_tidsserie_df %>% filter(kön!="kvinnor och män"), envir = .GlobalEnv)
    }

    diagramtitel <- paste0("Sysselsättningsgrad ",unique(sysselsattningsgrad_tidsserie_df$ålder), " i ",unique(sysselsattningsgrad_tidsserie_df$region))
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("sysselsattningsgrad_tidsserie_",unique(sysselsattningsgrad_tidsserie_df$region),".png")

    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaLinjeDiagram(skickad_df =sysselsattningsgrad_tidsserie_df %>%
                                  filter(kön!="kvinnor och män"),
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
  }

  return(gg_list)

}
