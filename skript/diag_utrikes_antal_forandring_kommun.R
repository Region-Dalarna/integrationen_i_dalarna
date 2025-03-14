diagram_utrikes_fodda_tidsserie <-function(region_vekt = c("20"),# Max 1, län
                                           kon_klartext = NA, # #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor"
                                           diag_antal = TRUE,
                                           diag_forandring_kommuner = TRUE,
                                           output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp för figur
                                           spara_figur = FALSE, # Sparar figuren till output_mapp_figur
                                           fodelseregion_klartext = "*", # NA = tas inte med i uttaget,  Finns: "Född i Sverige", "Utrikes född"
                                           tid_koder = "*", # Finns från 2000 och framåt
                                           returnera_figur = TRUE, # Returnerar en figur
                                           valda_farger = diagramfarger("rus_sex"),
                                           returnera_data = FALSE) # Skall data returneras)
{

  ## =================================================================================================================
  # Funktion som skapar två diagram, ett för antal utrikes födda i valt län, ett för förändring i utrikes födda mellan första och sista år i länets kommuner
  # =================================================================================================================
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx,
         pxweb)

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bef_region_alder_kon_fodelseregion_tid_InrUtrFoddaRegAlKon_scb.R")

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c()
  region_namn <- skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region)

  if(diag_forandring_kommuner) region_vekt = hamtakommuner(region_vekt,tamedriket = FALSE)

  # Hämta data
  antal_inrikes_utrikes_df <- hamta_bef_region_alder_kon_fodelseregion_tid_scb(region_vekt = region_vekt,
                                                                       alder_koder = NA,
                                                                       kon_klartext = kon_klartext,
                                                                       tid_koder = tid_koder) %>%
    mutate(region = skapa_kortnamn_lan(region))


  if(diag_antal){
    antal_utrikes_region_df <- antal_inrikes_utrikes_df %>% filter(födelseregion == "Utrikes född",region == region_namn)

    if(returnera_data == TRUE){
      assign("antal_utrikes_region_df", antal_utrikes_region_df, envir = .GlobalEnv)
    }

    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
    diagram_titel = paste0("Antal utrikes födda i ",region_namn)
    diagramfilnamn <- paste0("utrikes_fodda_antal_",region_namn,".png")

    gg_obj <- SkapaStapelDiagram(skickad_df = antal_utrikes_region_df ,
                                 skickad_x_var = "år",
                                 skickad_y_var = "Antal",
                                 manual_color = valda_farger,
                                 diagram_titel = diagram_titel,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 diagram_capt =  diagram_capt,
                                 output_mapp = output_mapp_figur,
                                 stodlinjer_avrunda_fem = TRUE,
                                 #x_axis_visa_var_xe_etikett = 2,
                                 manual_y_axis_title = "",
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }

  if(diag_forandring_kommuner){

    first_year <- min(antal_inrikes_utrikes_df$år)
    last_year <- max(antal_inrikes_utrikes_df$år)

    # Beräknar förändring i antal
    antal_forandring_df <- antal_inrikes_utrikes_df %>%
      filter(år %in% c(min(år),max(år))) %>%
        pivot_wider(names_from = år, values_from = Antal) %>%
          mutate(forandring = get(last_year) - get(first_year))



    # Calculate share of utrikes födda by using a pivot wider
    # andel_utrikes_df <- antal_inrikes_utrikes_df %>%
    #   pivot_wider(names_from = födelseregion, values_from = Antal) %>%
    #     mutate(andel_utrikes = (`Utrikes född`/`Född i Sverige`)*100) %>%
    #       select(region,år,andel_utrikes)

    if(returnera_data == TRUE){
      assign("antal_forandring_df", antal_forandring_df, envir = .GlobalEnv)
    }

    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
    diagram_titel = paste0("Befolkningsförändring ",first_year,"-",last_year," i ",region_namn)
    diagramfilnamn <- paste0("befolkningsforandring_",region_namn,".png")

    gg_obj <- SkapaStapelDiagram(skickad_df = antal_forandring_df %>%
                                   filter((region != region_namn)),
                                 skickad_x_var = "region",
                                 skickad_y_var = "forandring",
                                 skickad_x_grupp = "födelseregion",
                                 manual_color = valda_farger,
                                 geom_position_stack = TRUE,
                                 diagram_titel = diagram_titel,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 diagram_capt =  diagram_capt,
                                 output_mapp = output_mapp_figur,
                                 stodlinjer_avrunda_fem = TRUE,
                                 #x_axis_visa_var_xe_etikett = 2,
                                 manual_y_axis_title = "",
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }



  if(returnera_figur == TRUE){
    return(gg_list)
  }

}
