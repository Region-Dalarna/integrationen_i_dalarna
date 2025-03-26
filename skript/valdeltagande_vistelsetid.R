#test <- diag_valdeltagande_vistelsetid_inrikes_scb(typ_av_val = c("Valdeltagande i val till riksdag","Valdeltagande i val till region", "Valdeltagande i val till kommun"))

diag_valdeltagande_vistelsetid_inrikes_scb <- function(region = "20", # Enbart ett i taget.
                                                       visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                                       typ_av_val = "Valdeltagande i val till riksdag",# Finns även: "Valdeltagande i val till region", "Valdeltagande i val till kommun"
                                                       logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                                       output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                                       skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                                       returnera_data_rmarkdown = FALSE,
                                                       demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {


  # =======================================================================================================================
  #
  # Tre diagram för valdeltagande baserat på vistelsetid, ett diagram per typ av val
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


  gg_list <- list()

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_integration_valdeltagande_region_bakgrund_kon_tid_IntGr11Riket1_IntGr11Lan1_IntGr11Kom1_scb.R")

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 readxl)

  # Hämtar data
  valdeltagande_df <- hamta_integration_valdeltagande_region_bakgrund_kon_tid_scb (region_vekt = region,
                                                                                   kon_klartext = "män och kvinnor",
                                                                                   bakgrund_klartext = c("födelseregion: Sverige","samtliga utrikes födda", "vistelsetid 10- år", "vistelsetid < 10 år"),
                                                                                   cont_klartext = "*",
                                                                                   tid_koder = "*") %>%
    mutate(variabel = case_when(
      variabel == "vistelsetid < 10 år" ~ "< 10 år",
      variabel == "vistelsetid 10- år" ~ "10- år",
      variabel == "födelseregion: Sverige" ~ "Inrikes född",
      TRUE ~ variabel
    )) %>%
      mutate(region = skapa_kortnamn_lan(region),
             val = str_extract(val, "^[^,]+"))


  if(returnera_data_rmarkdown == TRUE){
    assign("valdeltagande_df", valdeltagande_df, envir = .GlobalEnv)
  }



  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."

  # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
  valdeltagande_df$variabel <- factor(valdeltagande_df$variabel, levels = c("< 10 år","10- år",
                                                              "samtliga utrikes födda","Inrikes född"))

  skapa_diagram <- function(val_vilket){

    diagramtitel <- paste0(val_vilket," i ",unique(valdeltagande_df$region)," efter vistelsetid")
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0(gsub(" ", "_", val_vilket),".png")

    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaStapelDiagram(skickad_df =valdeltagande_df %>%
                                   filter(val == val_vilket),
                                 skickad_x_var = "variabel",
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "år",
                                 # manual_x_axis_text_vjust=0.9,
                                 manual_color = diagramfarger("rus_sex"),
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

  retur_list <- map(typ_av_val, ~skapa_diagram(val_vilket = .x)) %>% purrr::flatten()

  return(retur_list)

}
