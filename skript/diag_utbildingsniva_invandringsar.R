diag_utbsniva_invandringsar <-function(output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp för figur
                                                 diag_utb = TRUE,
                                                 diag_invandringsar = TRUE,
                                                 skriv_diagrambildfil = FALSE, # Sparar figuren till output_mapp_figur
                                                 returnera_figur = TRUE, # Returnerar en figur
                                                 valda_farger = diagramfarger("rus_sex"),
                                                 returnera_data_rmarkdown = FALSE) # Skall data returneras)
      {

  ## =================================================================================================================
  # En tidsserie för antal asylsökande i Sverige
  # Från Supercross. Txd-filer finns i projektet Region Dalarna, under projekt/integrationsrapporten
  # Öppnar man dessa öppnas Supercross
  # =================================================================================================================
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx,
         pxweb,
         tidyverse,
         glue)

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c()

  if(diag_utb){

    input_mapp <- "G:/skript/projekt/data/integration"
    files <- list.files(input_mapp, pattern = "utbildning_invandringsar_alt", full.names = TRUE)

    file_info <- file.info(files)
    latest_file <- rownames(file_info)[which.max(file_info$mtime)]

    utb_invandringsar_df <- read.xlsx(latest_file)

    # Calculate share within each invandringsar
    utb_invandringsar_df <- utb_invandringsar_df %>%
      group_by(Invandringsar,Utbildningsniva) %>%
        summarize(Antal = sum(Antal, na.rm = TRUE)) %>%
          mutate(andel = round(Antal / sum(Antal) * 100, 2)) %>%
            ungroup() %>%
              mutate(Aldersgrupp = "20-64 år",
                     Vistelsetid = case_when(
                       Invandringsar == "1928 - 2014" ~ "10+ år",
                       Invandringsar == "2015 - 2020" ~ "4-9 år",
                       Invandringsar == "2021 - 2022" ~ "2-3 år",
                       Invandringsar == "2023 - 2024" ~ "0-1 år",
                       Invandringsar == "Ej invandrat" ~ "Inrikes född"
                     )
                     )

    utb_invandringsar_df$Utbildningsniva <- trimws(utb_invandringsar_df$Utbildningsniva)

    if(returnera_data_rmarkdown == TRUE){
      assign("utb_invandringsar_df", utb_invandringsar_df, envir = .GlobalEnv)
    }

    utb_invandringsar_df$Utbildningsniva <- factor(utb_invandringsar_df$Utbildningsniva,
                                             levels = c("Förgymnasial utbildning",
                                                        "Gymnasial utbildning",
                                                        "Eftergymnasial utbildning kortare än 3 år",
                                                        "Eftergymnasial utbildning 3 år eller längre",
                                                        "Uppgift saknas"))

    utb_invandringsar_df$Vistelsetid <- factor(utb_invandringsar_df$Vistelsetid,
                                               levels = c("0-1 år",
                                                        "2-3 år",
                                                        "4-9 år",
                                                        "10+ år",
                                                        "Inrikes född"))

    diagram_capt = "Källa: SCB\nBearbetning: Samhällsanalys, Region Dalarna"
    diagram_titel = glue("Utbildningsnivå i Dalarna år 2024 efter invandringsår ({unique(utb_invandringsar_df$Aldersgrupp)})")
    diagramfilnamn <- "utb_niva_invandringsar.png"

    gg_obj <- SkapaStapelDiagram(skickad_df = utb_invandringsar_df,
                                 skickad_x_var = "Vistelsetid",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "Utbildningsniva",
                                 manual_color = valda_farger,
                                 diagram_titel = diagram_titel,
                                 procent_0_100_10intervaller = TRUE,
                                 diagram_capt =  diagram_capt,
                                 output_mapp = output_mapp_figur,
                                 stodlinjer_avrunda_fem = TRUE,
                                 x_axis_lutning = 0,
                                 legend_rader = 2,
                                 #x_axis_visa_var_xe_etikett = 2,
                                 manual_y_axis_title = "procent",
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }

  if(diag_invandringsar){
    input_mapp <- "G:/skript/projekt/data/integration" # Mapp med asylsökande data
    files <- list.files(input_mapp, pattern = "invandringsar_antal", full.names = TRUE)

    file_info <- file.info(files)
    latest_file <- rownames(file_info)[which.max(file_info$mtime)]

    invandringsar_antal_df <- read.xlsx(latest_file) %>%
      mutate(Invandringsar= Invandringsar %>% as.character())

    if(returnera_data_rmarkdown == TRUE){
      assign("invandringsar_antal_df", invandringsar_antal_df, envir = .GlobalEnv)
    }

    diagram_capt = "Källa: SCB\nBearbetning: Samhällsanalys, Region Dalarna"
    diagram_titel = glue("Utrikes födda personer i Dalarna år 2024 efter invandringsår")
    diagramfilnamn <- "invandringsar_antal.png"

    gg_obj <- SkapaStapelDiagram(skickad_df = invandringsar_antal_df,
                                 skickad_x_var = "Invandringsar",
                                 skickad_y_var = "Antal",
                                 #skickad_x_grupp = "Utbildningsniva",
                                 manual_color = valda_farger,
                                 diagram_titel = diagram_titel,
                                 #procent_0_100_10intervaller = TRUE,
                                 diagram_capt =  diagram_capt,
                                 output_mapp = output_mapp_figur,
                                 stodlinjer_avrunda_fem = TRUE,
                                 x_axis_lutning = 90,
                                 legend_rader = 2,
                                 x_axis_visa_var_xe_etikett = 10,
                                 manual_y_axis_title = "",
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }

    return(gg_list)

}
