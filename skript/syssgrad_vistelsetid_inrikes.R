diag_sysselsattningsgrad_vistelsetid_inrikes_scb <- function(region = "20", # Enbart ett i taget.
                                                             visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                                             logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                                             output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                                             skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                                             returnera_data_rmarkdown = FALSE,
                                                             demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
                      ) {


  # =======================================================================================================================
  #
  # Ett diagram för sysselsättningsgrad kopplad till bakgrund (vistelsetid)
  #
  # Lite oklart vilket åldersspann det gäller. Kolla upp
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
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_IntGr1KomKonUtb_ny_BAS_scb.R")

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 readxl)

  # Hämtar data
  syssgrad_df <- hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_scb_ny (region_vekt = region,
                                                                                 kon_klartext = c("män", "kvinnor"),
                                                                                 utbniv_klartext = "samtliga utbildningsnivåer",
                                                                                 bakgrvar_klartext = c("födelseregion: Sverige","vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"),
                                                                                 cont_klartext = "Andel sysselsatta",
                                                                                 tid_koder = "9999") %>%
    mutate(bakgrundsvariabel = case_when(
      bakgrundsvariabel == "vistelsetid 0-1 år" ~ "0-1 år",
      bakgrundsvariabel == "vistelsetid 2-3 år" ~ "2-3 år",
      bakgrundsvariabel == "vistelsetid 4-9 år" ~ "4-9 år",
      bakgrundsvariabel == "vistelsetid 10- år" ~ "10- år",
      bakgrundsvariabel == "födelseregion: Sverige" ~ "Inrikes född",
      TRUE ~ bakgrundsvariabel
    ))


  if(returnera_data_rmarkdown == TRUE){
    assign("syssgrad_vistelsetid_inrikes_df", syssgrad_df, envir = .GlobalEnv)
  }

  gg_list <- list()


  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."

  # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
  syssgrad_df$bakgrundsvariabel <- factor(syssgrad_df$bakgrundsvariabel, levels = c("0-1 år","2-3 år",
                                                                                      "4-9 år","10- år",
                                                                                      "Inrikes född"))

  diagramtitel <- paste0("Sysselsättningsgrad i Dalarna"," ",max(syssgrad_df$år)," efter vistelsetid")
  #diagramtitel <- str_wrap(diagramtitel,60)
  diagramfilnamn <- paste0("sysselsattningsgrad_vistelsetid_inrikes.png")

  # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
  gg_obj <- SkapaStapelDiagram(skickad_df =syssgrad_df %>%
                                 filter(kön != "totalt"),
                               skickad_x_var = "bakgrundsvariabel",
                               skickad_y_var = "Andel sysselsatta",
                               skickad_x_grupp = "kön",
                               # manual_x_axis_text_vjust=0.9,
                               manual_color = diagramfarger("kon"),
                               diagram_titel = diagramtitel,
                               diagram_capt =  diagram_capt,
                               manual_y_axis_title = "procent",
                               manual_x_axis_title = "Vistelsetid i Sverige",
                               y_axis_100proc = TRUE,
                               x_axis_lutning = 0,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               lagg_pa_logga = visa_logga_i_diagram,
                               skriv_till_diagramfil = skriv_diagrambildfil)


  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")

  return(gg_list)

}
