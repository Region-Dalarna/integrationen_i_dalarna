diag_andel_utrikes_scb <- function(region = "20", # Enbart ett i taget.
                                   visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                   logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                   start_ar = "2000",                                 # Startår för jämförelse. Senaste år jämförs med denna (tidigast 2000 och max 1 år)
                                   output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                   skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                   valda_farger = rddiagram::diagramfarger("rus_sex"),
                                   x_axis_storlek = 10.5,
                                   ta_bort_diagramtitel = FALSE,
                                   ta_bort_caption = FALSE,
                                   returnera_data_rmarkdown = FALSE,
                                   demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {


  # =======================================================================================================================
  #
  # Ett diagram för förvärvsinkomst kopplad till bakgrund (vistelsetid)
  #
  # Uppdaterad med ny version av PXweb mm Jon 2026-09-21
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

  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  region_namn <- rdverktyg::skapa_kortnamn_lan(hamtaregion_kod_namn(region)$region)
  # Används till diagrammet förändring län

  # Hämtar data

  inte_ckm_df <- pxweb2r::pxweb2_get_data(
    table = c("TAB4823"),
    query = list(
      Region = rdverktyg::hamtakommuner(region),
      Alder = NA,
      Kon = NA,
      Fodelseregion = c("utrikes född","född i Sverige"),
      ContentsCode = "*",
      Tid = c(start_ar)
    ))

  ckm_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6645",
    query = list(
      Region = rdverktyg::hamtakommuner(region),
      Alder = "totalt, samtliga åldrar",
      Kon = "totalt, samtliga män och kvinnor",
      Fodelseregion = c("utrikes född","född i Sverige"),
      ContentsCode = "*",
      Tid = "*"
    )) |>
    select(-ålder,-kön)

  andel_utrikes_df <- base::rbind(inte_ckm_df, ckm_df) |>
    tidyr::pivot_wider(names_from = födelseregion, values_from = value) |>
      dplyr::mutate(andel_utrikes = (`utrikes född` / (`född i Sverige` + `utrikes född`)) * 100) |>
        dplyr::select(region, år, andel_utrikes) |>
          dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))


  if(returnera_data_rmarkdown == TRUE){
    assign("andel_utrikes_df", andel_utrikes_df, envir = .GlobalEnv)
  }

  diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  diagram_titel = paste0("Andel utrikes födda")
  diagramfilnamn <- paste0("utrikes_fodda_andel_",region_namn,".png")

  if(ta_bort_diagramtitel){
    diagram_titel = ""
  }

  if(ta_bort_caption){
    diagram_capt = ""
  }

  # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
  gg_obj <- SkapaStapelDiagram(skickad_df = andel_utrikes_df,
                               skickad_x_var = "region",
                               skickad_y_var = "andel_utrikes",
                               skickad_x_grupp = "år",
                               manual_color = valda_farger,
                               diagram_titel = diagram_titel,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               diagram_capt =  diagram_capt,
                               output_mapp = "output_mapp_figur",
                               stodlinjer_avrunda_fem = TRUE,
                               x_axis_storlek = x_axis_storlek,
                               vand_sortering = TRUE,
                               x_axis_sort_value = TRUE,
                               x_axis_sort_grp = 2,
                               manual_y_axis_title = "procent",
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = FALSE)


  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn |> stringr::str_remove(".png")

  return(gg_list)

}
