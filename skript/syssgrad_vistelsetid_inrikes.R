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
  # Bara paket, ingen source() mot funktioner-/hamta_data-reporna och inget
  # p_load(tidyverse). Anropas med fullt namespace (dplyr::filter() osv.) i
  # stället för library().
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("tidyverse")


  gg_list <- list()

  if(diag_vistelsetid){

    # Hämtar data
    syssgrad_df <- pxweb2r::pxweb2_get_data(
      table = "TAB6384",
      query = list(
        Region = region,
        Kon = c("män", "kvinnor"),
        UtbNiv = "samtliga utbildningsnivåer",
        BakgrVar = c("födelseregion: Sverige","vistelsetid 0–1 år", "vistelsetid 2–3 år", "vistelsetid 4–9 år", "vistelsetid 10– år"),
        ContentsCode = "Andel sysselsatta",
        Tid = "9999"
      ))  |>
      dplyr::mutate(bakgrundsvariabel = dplyr::case_when(
        bakgrundsvariabel == "vistelsetid 0–1 år" ~ "0–1 år",
        bakgrundsvariabel == "vistelsetid 2–3 år" ~ "2–3 år",
        bakgrundsvariabel == "vistelsetid 4–9 år" ~ "4–9 år",
        bakgrundsvariabel == "vistelsetid 10– år" ~ "10– år",
        bakgrundsvariabel == "födelseregion: Sverige" ~ "Inrikes född",
        TRUE ~ bakgrundsvariabel
      ))


    if(returnera_data_rmarkdown == TRUE){
      assign("syssgrad_vistelsetid_inrikes_df", syssgrad_df, envir = .GlobalEnv)
    }

    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."

    # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
    syssgrad_df$bakgrundsvariabel <- factor(syssgrad_df$bakgrundsvariabel, levels = c("0–1 år","2–3 år",
                                                                                        "4–9 år","10– år",
                                                                                        "Inrikes född"))

    diagramtitel <- paste0("Sysselsättningsgrad i Dalarna"," ",max(syssgrad_df$år)," efter vistelsetid")
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("sysselsattningsgrad_vistelsetid_inrikes.png")

    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = syssgrad_df  |>
                                             dplyr::filter(kön != "totalt"),
                                           skickad_x_var = "bakgrundsvariabel",
                                           skickad_y_var = "value",
                                           skickad_x_grupp = "kön",
                                           # manual_x_axis_text_vjust=0.9,
                                           manual_color = rddiagram::diagramfarger("kon"),
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
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn |> stringr::str_remove(".png")
  }

  if(diag_utbniva){

    # Hämtar data. Av någon oklar anledning får man dubbletter för utrikes födda, varför distinct används på slutet
    syssgrad_df <- pxweb2r::pxweb2_get_data(
      table = "TAB6384",
      query = list(
        Region = region,
        Kon = "män och kvinnor",
        UtbNiv = c("utbildningsnivå: förgymnasial utbildning", "utbildningsnivå: gymnasial utbildning", "utbildningsnivå: eftergymnasial utbildning"),
        BakgrVar = c("födelseregion: Sverige","samtliga utrikes födda invandrare"),
        ContentsCode = "Andel sysselsatta",
        Tid = "9999"
      ))  |>
      dplyr::mutate(utbildningsnivå = sub("utbildningsnivå: ", "", utbildningsnivå),
                    utbildningsnivå = stringr::str_to_sentence(utbildningsnivå),
                    bakgrundsvariabel = dplyr::case_when(
                      bakgrundsvariabel == "födelseregion: Sverige" ~ "Inrikes född",
                      bakgrundsvariabel == "samtliga utrikes födda invandrare" ~ "Utrikes född",
                      TRUE ~ bakgrundsvariabel
                    )) |> distinct()


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
    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df =syssgrad_df ,
                                             skickad_x_var = "utbildningsnivå",
                                             skickad_y_var = "value",
                                             skickad_x_grupp = "bakgrundsvariabel",
                                             # manual_x_axis_text_vjust=0.9,
                                             manual_color = rddiagram::diagramfarger("rus_sex"),
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
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn |> stringr::str_remove(".png")
  }

  return(gg_list)

}
