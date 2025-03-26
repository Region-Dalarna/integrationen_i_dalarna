diag_sysselsattningsgrad_vistelsetid_inrikes_scb <- function(region = "20", # Enbart ett i taget.
                                                             diag_vistelsetid = TRUE,
                                                             diag_utbniva = TRUE,
                                                             visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                                             logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                                             output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                                             skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                                             returnera_data_rmarkdown = FALSE,
                                                             demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {


  # =======================================================================================================================
  #
  # Ett diagram för boende (hyresrätt, bostadsrätt etc) kopplat till bakgrund (vistelsetid)
  #
  # Lite oklart med åldersspann
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
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_integration_boende_region_kon_bakgrund_tid_IntGr6LanKon_IntGr6RikKon_scb.R")

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 readxl)

  if(diag_vistelsetid){

    tid_koder = "9999"
    # Av oklar anledning saknas mycket data för senaste år. Jag gör därför ett enklare uttag för senaste år och om det saknas data väljs året innan
    boende_test <- hamta_integration_region_kon_bakgrund_tid_scb (region_vekt = region,
                                                                kon_klartext = "män och kvinnor",
                                                                bakgrund_klartext = c("födelseregion: Sverige","vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"),
                                                                cont_klartext = c("Andel boende i egna hem, procent", "Andel boende i hyresrätt, procent", "Andel boende i bostadsrätt, procent"),
                                                                tid_koder = tid_koder)

    # Check if there exists na:s in any of the variables in the datafram boende_test
    if(length(which(is.na(boende_test)))!= 0 ){
      tid_koder = as.integer(unique(boende_test$år))-1
    }


    # Hämtar data
    boende_df <- hamta_integration_region_kon_bakgrund_tid_scb (region_vekt = region,
                                                                kon_klartext = "män och kvinnor",
                                                                bakgrund_klartext = c("födelseregion: Sverige","vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"),
                                                                cont_klartext = c("Andel boende i egna hem, procent", "Andel boende i hyresrätt, procent", "Andel boende i bostadsrätt, procent"),
                                                                tid_koder = tid_koder) %>%
      mutate(variabel = case_when(
        variabel == "vistelsetid 0-1 år" ~ "0-1 år",
        variabel == "vistelsetid 2-3 år" ~ "2-3 år",
        variabel == "vistelsetid 4-9 år" ~ "4-9 år",
        variabel == "vistelsetid 10- år" ~ "10- år",
        variabel == "födelseregion: Sverige" ~ "Inrikes född",
        TRUE ~ variabel
      )) %>%
      mutate(region = skapa_kortnamn_lan(region))


    if(returnera_data_rmarkdown == TRUE){
      assign("boendtyp_df", boende_df, envir = .GlobalEnv)
    }



    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."

    # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
    boende_df$variabel <- factor(boende_df$variabel, levels = c("0-1 år","2-3 år",
                                                                                      "4-9 år","10- år",
                                                                                      "Inrikes född"))

    diagramtitel <- paste0("Boende per upplåtelseform i ",unique(boende_df$region)," ",max(boende_df$år)," efter vistelsetid")
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("boendetyp_vistelsetid_inrikes.png")

    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaStapelDiagram(skickad_df =boende_df %>%
                                   filter(kön != "totalt"),
                                 skickad_x_var = "variabel",
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "bakgrund",
                                 # manual_x_axis_text_vjust=0.9,
                                 manual_color = diagramfarger("rus_sex"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_title = "Vistelsetid i Sverige",
                                 geom_position_stack = TRUE,
                                 y_axis_100proc = TRUE,
                                 x_axis_lutning = 0,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 skriv_till_diagramfil = skriv_diagrambildfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }

  if(diag_utbniva){

    # Hämtar data. Av någon oklar anledning får man dubbletter för utrikes födda, varför distinct används på slutet
    syssgrad_df <- hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_scb_ny (region_vekt = region,
                                                                                  kon_klartext = c("män och kvinnor"),
                                                                                  utbniv_klartext =  c("utbildningsnivå: förgymnasial utbildning", "utbildningsnivå: gymnasial utbildning", "utbildningsnivå: eftergymnasial utbildning"),
                                                                                  bakgrvar_klartext = c("födelseregion: Sverige","samtliga utrikes födda invandrare"),
                                                                                  cont_klartext = "Andel sysselsatta",
                                                                                  tid_koder = "9999") %>%
      mutate(utbildningsnivå = sub("utbildningsnivå: ", "", utbildningsnivå),
             utbildningsnivå = str_to_sentence(utbildningsnivå),
             bakgrundsvariabel = case_when(
               bakgrundsvariabel == "födelseregion: Sverige" ~ "Inrikes född",
               bakgrundsvariabel == "samtliga utrikes födda invandrare" ~ "Utrikes född",
               TRUE ~ bakgrundsvariabel
             )) %>% distinct()


    if(returnera_data_rmarkdown == TRUE){
      assign("syssgrad_utrikes_inrikes_utbniva_df", syssgrad_df, envir = .GlobalEnv)
    }

    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."

    # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
    syssgrad_df$utbildningsnivå <- factor(syssgrad_df$utbildningsnivå, levels = c("Förgymnasial utbildning",
                                                                                  "Gymnasial utbildning",
                                                                                  "Eftergymnasial utbildning"))

    diagramtitel <- paste0("Sysselsättningsgrad i Dalarna"," ",max(syssgrad_df$år)," efter utbildningsnivå")
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("sysselsattningsgrad_inrikes_utrikes_utbniva.png")

    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaStapelDiagram(skickad_df =syssgrad_df ,
                                 skickad_x_var = "utbildningsnivå",
                                 skickad_y_var = "Andel sysselsatta",
                                 skickad_x_grupp = "bakgrundsvariabel",
                                 # manual_x_axis_text_vjust=0.9,
                                 manual_color = diagramfarger("rus_sex"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 y_axis_100proc = TRUE,
                                 x_axis_lutning = 0,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 skriv_till_diagramfil = skriv_diagrambildfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }

  return(gg_list)

}
