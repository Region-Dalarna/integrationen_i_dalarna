diag_arbetsloshet_kommun <- function(region = "20", # Enbart ett län i taget.
                                      visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                      logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                      output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                      skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                      returnera_data_rmarkdown = FALSE,
                                      demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {


  # =======================================================================================================================
  #
  # Ett diagram för förvärvsinkomst kopplad till bakgrund (vistelsetid)
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
  p_load(tidyverse)

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_ArbStatusM_scb.R")

  vald_region = skapa_kortnamn_lan(hamtaregion_kod_namn(region)$region)

  arblosa_bakgr <- hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_scb(region_vekt = hamtakommuner(lan = region,tamedlan = TRUE,tamedriket = TRUE),
                                                                                     alder_klartext = "20-64 år",
                                                                                     kon_klartext = c("kvinnor","män"),
                                                                                     fodelseregion_klartext = c("inrikes född", "utrikes född"),
                                                                                     cont_klartext = "arbetslöshet",
                                                                                     tid_koder = "9999") %>%
    mutate(region = region %>% skapa_kortnamn_lan(byt_ut_riket_mot_sverige = TRUE)) %>%
    manader_bearbeta_scbtabeller()


  if(returnera_data_rmarkdown == TRUE){
    assign("arblosa_bakgr_df", arblosa_bakgr, envir = .GlobalEnv)
  }

  gg_list <- list()


  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."

  diagram_titel <- paste0("Arbetslöshet (", unique(arblosa_bakgr$ålder), ") i ",vald_region, " i ", unique(arblosa_bakgr$månad_år))
  diagramfil <- "andel_arbetslosa_kommun.png"

  # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
  gg_obj <- SkapaStapelDiagram(skickad_df = arblosa_bakgr,
                               skickad_x_var = "region",
                               skickad_y_var = "arbetslöshet",
                               skickad_x_grupp = "kön",
                               facet_sort = FALSE,
                               x_axis_sort_value = FALSE,
                               x_axis_sort_grp = 3,
                               facet_scale = "fixed",
                               diagram_facet = TRUE,
                               facet_grp = "födelseregion",
                               vand_sortering = TRUE,
                               facet_legend_bottom = FALSE,
                               diagram_titel = diagram_titel,
                               #y_axis_100proc = TRUE,
                               diagram_capt = diagram_capt,
                               stodlinjer_avrunda_fem = TRUE,
                               manual_y_axis_title = "procent",
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_color = diagramfarger("rus_sex"),
                               skriv_till_diagramfil = skriv_diagrambildfil,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfil)


  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")

  return(gg_list)

}
