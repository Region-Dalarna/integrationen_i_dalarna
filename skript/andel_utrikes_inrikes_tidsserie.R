diag_bef_inr_utr_tid <- function(region = "20", # Enbart ett län i taget.
                                 diag_andel = TRUE, # Andel inrikes/utrikes födda i arbetsför ålder
                                 diag_antal = FALSE, # Antal "-"
                                 visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                 logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                 output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                 skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                 returnera_data_rmarkdown = FALSE,
                                 demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {


  # =======================================================================================================================
  # Diagram för antalet inrikes/utrikes födda i arbetsför ålder
  # Ändrat från Född i Sverige till född i Sverige då SCB gjort någon ändring
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
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bef_region_alder_kon_fodelseregion_tid_InrUtrFoddaRegAlKon_scb.R")

  diagram_capt <- "Källa: Befolkningsregistret i SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  gg_list <- list()

  bef_bakgr <- hamta_bef_region_alder_kon_fodelseregion_tid_scb(region_vekt = region,
                                                                alder_koder  = "*",
                                                                kon_klartext = NA,
                                                                fodelseregion_klartext = c("Född i Sverige", "Utrikes född"),
                                                                tid_koder = "*") %>%
    mutate(alder_grupp = skapa_aldersgrupper(ålder,c(20,65))) %>%
    group_by(år,region,födelseregion,alder_grupp) %>%
    summarise(Antal = sum(Antal, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(födelseregion = ifelse(födelseregion == "född i Sverige", "Inrikes född", "Utrikes född")) %>%
    filter(alder_grupp == unique(alder_grupp)[2] )

  if(diag_andel == TRUE){

    andel_utrikes_bakgr_df <- bef_bakgr %>%
      pivot_wider(names_from = "födelseregion", values_from = "Antal") %>%
      mutate(`Utrikes födda` = round(`Utrikes född` / (`Inrikes född` + `Utrikes född`) * 100,1),
             `Inrikes födda` = round(`Inrikes född` / (`Inrikes född` + `Utrikes född`) * 100,1)) %>%
      pivot_longer(cols = c(`Utrikes födda`, `Inrikes födda`),
                   names_to = "födelseregion",
                   values_to = "andel")



    if(returnera_data_rmarkdown == TRUE){
      assign("andel_utrikes_inrikes_bakgr_df", andel_utrikes_bakgr_df, envir = .GlobalEnv)
    }

    andel_utrikes_bakgr_df$födelseregion <- factor(andel_utrikes_bakgr_df$födelseregion, levels = c("Utrikes födda","Inrikes födda"))

    diagramtitel <- paste0("Andel i arbetsför ålder (",unique(bef_bakgr$alder_grupp), ") som är inrikes/utrikes födda i ",skapa_kortnamn_lan(unique(bef_bakgr$region)))
    diagramfilnamn <- paste0("befolkning_utr_inr_andel_",skapa_kortnamn_lan(unique(bef_bakgr$region)),".png")

    gg_obj <- SkapaStapelDiagram(skickad_df = andel_utrikes_bakgr_df,
                                 skickad_x_var = "år",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "födelseregion",
                                 manual_color = diagramfarger("rus_sex"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 45,
                                 procent_0_100_10intervaller = TRUE,
                                 geom_position_stack = TRUE,
                                 legend_vand_ordning = TRUE,
                                 diagram_liggande = FALSE,
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 manual_y_axis_title = "procent",
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")

  }

  if(diag_antal == TRUE){

    if(returnera_data_rmarkdown == TRUE){
      assign("antal_utrikes_inrikes_bakgr_df", bef_bakgr, envir = .GlobalEnv)
    }

    bef_bakgr$födelseregion <- factor(bef_bakgr$födelseregion, levels = c("Utrikes född","Inrikes född"))

    diagramtitel <- paste0("Befolkning i arbetsför ålder (",unique(bef_bakgr$alder_grupp), ") i ",skapa_kortnamn_lan(unique(bef_bakgr$region)))
    diagramfilnamn <- paste0("befolkning_utr_inr_antal_",skapa_kortnamn_lan(unique(bef_bakgr$region)),".png")

    gg_obj <- SkapaStapelDiagram(skickad_df = bef_bakgr,
                                 skickad_x_var = "år",
                                 skickad_y_var = "Antal",
                                 skickad_x_grupp = "födelseregion",
                                 manual_color = diagramfarger("rus_sex"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 45,
                                 geom_position_stack = TRUE,
                                 legend_vand_ordning = TRUE,
                                 diagram_liggande = FALSE,
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)



    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }

  return(gg_list)

}
