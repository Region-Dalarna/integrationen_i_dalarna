diag_boendytyp_vistelsetid_inrikes_scb <- function(region = "20", # Enbart ett i taget.
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
      ),
      bakgrund = case_when(
        bakgrund == "Andel boende i egna hem, procent" ~ "Äganderätt",
        bakgrund == "Andel boende i hyresrätt, procent" ~ "Hyresrätt",
        bakgrund == "Andel boende i bostadsrätt, procent" ~ "Bostadsrätt",
        TRUE ~ bakgrund
      )
      ) %>%
      mutate(region = skapa_kortnamn_lan(region))

    boende_df <- pivot_wider(boende_df, names_from=bakgrund, values_from=varde) %>%
         mutate(Okänd = 100 - Äganderätt - Hyresrätt - Bostadsrätt) %>%
          pivot_longer(cols=6:9,names_to = "bakgrund",values_to = "varde")



    if(returnera_data_rmarkdown == TRUE){
      assign("boendtyp_df", boende_df, envir = .GlobalEnv)
    }



    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."

    # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
    boende_df$variabel <- factor(boende_df$variabel, levels = c("0-1 år","2-3 år",
                                                                                      "4-9 år","10- år",
                                                                                      "Inrikes född"))

    boende_df$bakgrund <- factor(boende_df$bakgrund, levels = c("Okänd","Hyresrätt","Bostadsrätt","Äganderätt"))

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
                                 legend_vand_ordning = TRUE,
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
