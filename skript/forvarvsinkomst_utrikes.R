diag_inkomst_bakgrund_scb <- function(region = "20", # Enbart ett i taget.
                                        diag_kon = TRUE, # Skapar ett diagram där länen jämförs för för vald vistelsetid
                                        diag_utbildning = TRUE,
                                        diag_tidsserie = TRUE, # Skapar ett diagram
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
  # Tre diagram kopplade till invandringsetablering som används i Rus-uppföljningen
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
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_forvarvsinkomst_region_kon_fodelseregion_vistelsetid_HE0110_scb.R")

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 readxl)

  # Före 2022
  forvarvsinkomst_df <- hamta_forvarvsinkomst_region_kon_fodelseregion_vistelsetid_scb(region_vekt = region,
                                                                                kon_klartext = "*",
                                                                                alder_klartext = "20-64 år",
                                                                                vistelsetiduf_klartext = "*",
                                                                                cont_klartext = "Medianinkomst, tkr",
                                                                                tid_koder = "9999") %>%
    rename(vistelsetid = `vistelsetid år`) %>%
      mutate(vistelsetid = ifelse(födelseregion == "födda i Sverige","Inrikes född",vistelsetid)) %>%
       filter(!(is.na(`Medianinkomst, tkr`)),
              födelseregion %in% c("födda i Sverige","utrikes födda"),
              vistelsetid != "samtliga") %>%
        mutate(vistelsetid = case_when(
          vistelsetid == "1-2 år i Sverige" ~ "1-2 år",
          vistelsetid == "3-4 år i Sverige" ~ "3-4 år",
          vistelsetid == "5-9 år i Sverige" ~ "5-9 år",
          vistelsetid == "10-19 år i Sverige" ~ "10-19 år",
          vistelsetid == "20- år i Sverige" ~ "20- år",
          TRUE ~ vistelsetid
        ))


  if(returnera_data_rmarkdown == TRUE){
    assign("forvarvsinkomst_df", forvarvsinkomst_df, envir = .GlobalEnv)
  }

  gg_list <- list()

  if(diag_kon == TRUE){
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."

    # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
    forvarvsinkomst_df$vistelsetid <- factor(forvarvsinkomst_df$vistelsetid, levels = c("1-2 år","3-4 år",
                                                                                        "5-9 år","10-19 år",
                                                                                        "20- år","Inrikes född"))

    diagramtitel <- paste0("Andel förvärvsarbetande 20-65 år bland utrikes födda i Dalarna"," ",max(etablering_df$år)," efter vistelsetid")
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("etablering_lan_kon.png")

    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaStapelDiagram(skickad_df =forvarvsinkomst_df %>%
                                          filter(kön != "totalt"),
                                 skickad_x_var = "vistelsetid",
                                 skickad_y_var = "Medianinkomst, tkr",
                                 skickad_x_grupp = "kön",
                                 # manual_x_axis_text_vjust=0.9,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_x_axis_title = "Vistelsetid i Sverige",
                                 y_axis_100proc = FALSE,
                                 x_axis_lutning = 0,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 skriv_till_diagramfil = skriv_diagrambildfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }

  if(diag_utbildning == TRUE){

    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."
    diagramtitel <- paste0("Andel förvärvsarbetande 20-65 år bland utrikes födda i Dalarna"," ",max(etablering_df$år))
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("etablering_lan_utb.png")

    gg_obj <- SkapaStapelDiagram(skickad_df =etablering_df %>%
                                   filter(år == max(år),
                                          kön != "män och kvinnor",
                                          utbildningsnivå %in% c("utbildningsnivå: gymnasial utbildning" ,"utbildningsnivå: eftergymnasial utbildning")),
                                 skickad_x_var = "bakgrundsvariabel",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "kön",
                                 manual_x_axis_title = "Vistelsetid i Sverige",
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = TRUE,
                                 facet_grp = "utbildningsnivå",
                                 facet_legend_bottom = TRUE,
                                 facet_scale = "fixed",
                                 x_axis_sort_value = FALSE,
                                 y_axis_100proc = TRUE,
                                 x_axis_lutning = 0,
                                 manual_y_axis_title="procent",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 skriv_till_diagramfil = skriv_diagrambildfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }

  if(diag_tidsserie == TRUE){

    diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Fram till och med 2021, 20-64 år och data från RAMS. Från 2022, 20-65 år och data från BAS."

    etablering_df_tid <- etablering_df %>%
      filter(bakgrundsvariabel != "10- år",
             utbildningsnivå == "samtliga utbildningsnivåer",
             kön == "män och kvinnor")

    # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
    etablering_df_tid$bakgrundsvariabel <- factor(etablering_df_tid$bakgrundsvariabel, levels = c("0-1 år","2-3 år",
                                                                                                  "4-9 år"))

    diagramtitel <- paste0("Andel förvärvsarbetande bland utrikes födda i Dalarna efter vistelsetid i Sverige")
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("etablering_tid.png")

    gg_obj <- SkapaStapelDiagram(skickad_df =etablering_df_tid,
                                 skickad_x_var = "år",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "bakgrundsvariabel",
                                 manual_color = diagramfarger("rus_sex"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 x_axis_sort_value = FALSE,
                                 x_axis_lutning = 45,
                                 y_axis_100proc = TRUE,
                                 manual_y_axis_title="procent",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 skriv_till_diagramfil = skriv_diagrambildfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }

  return(gg_list)

}
