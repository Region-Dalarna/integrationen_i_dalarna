diag_arbetsloshetstid_bakgrund <-function(output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp för figur
                                       skriv_diagrambildfil = FALSE, # Sparar figuren till output_mapp_figur
                                       returnera_figur = TRUE, # Returnerar en figur
                                       valda_farger = diagramfarger("rus_sex"),
                                       returnera_data_rmarkdown = FALSE) # Skall data returneras)
{

  ## =================================================================================================================
  # Data för senaste år (för tillfället 2023) för andel långtidsarbetslösa (av arbetslösa)
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



    input_mapp <- "G:/skript/projekt/data/integration"
    files <- list.files(input_mapp, pattern = "langtidsarbetsloshet_alt", full.names = TRUE)

    file_info <- file.info(files)
    latest_file <- rownames(file_info)[which.max(file_info$mtime)]

    # Diverse fix med data
    langtidsarbetsloshet_df <- read.xlsx(latest_file) %>%
      mutate(År = "2023",
             Aldersgrupp = "20-64 år") %>%
      select(År, Aldersgrupp,Bakgrund, Kon,Arbetsloshetsstatus,Antal) %>%
        filter(Arbetsloshetsstatus != "Är ej öppet arbetslös")

    # Lägger till en grupp totalt där könen summeras. Används i första hand för texten i markdown
    langtidsarbetsloshet_totalt <- langtidsarbetsloshet_df %>%
      group_by(År, Aldersgrupp,Bakgrund,Arbetsloshetsstatus) %>%
        summarize(Antal = sum(Antal, na.rm = TRUE)) %>%
          ungroup() %>%
            mutate(Kon = "Totalt")

    langtidsarbetsloshet_df <- rbind(langtidsarbetsloshet_df,langtidsarbetsloshet_totalt)


    # Beräknar andel inom grupperna
    langtidsarbetsloshet_df <- langtidsarbetsloshet_df %>%
      group_by(År, Aldersgrupp, Kon,Bakgrund,Arbetsloshetsstatus) %>%
      summarize(Antal = sum(Antal, na.rm = TRUE)) %>%
      mutate(andel = round(Antal / sum(Antal) * 100, 2)) %>%
      ungroup()

    if(returnera_data_rmarkdown == TRUE){
      assign("arbetsloshetstid_df", langtidsarbetsloshet_df, envir = .GlobalEnv)
    }

    langtidsarbetsloshet_df$Arbetsloshetsstatus <- factor(langtidsarbetsloshet_df$Arbetsloshetsstatus,
                                                          levels = c("< 6 månader",
                                                                     "6-12 månader",
                                                                     "12-24 månader",
                                                                     "24- månader"))



    diagram_capt = "Källa: SCB\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: I diagrammet redovisas andel av arbetslösa."
    diagram_titel = glue("Tid som arbetslös ({unique(langtidsarbetsloshet_df$Aldersgrupp)}) i Dalarna år {unique(langtidsarbetsloshet_df$År)}")
    diagramfilnamn <- "langtidsarbetsloshet.png"

    gg_obj <- SkapaStapelDiagram(skickad_df = langtidsarbetsloshet_df %>%
                                   filter(Kon != "Totalt"),
                                 skickad_x_var = "Arbetsloshetsstatus",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "Kon",
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagram_titel,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 diagram_facet = TRUE,
                                 facet_grp = "Bakgrund",
                                 facet_scale = "fixed",
                                 procent_0_100_10intervaller = TRUE,
                                 diagram_capt =  diagram_capt,
                                 output_mapp = output_mapp_figur,
                                 stodlinjer_avrunda_fem = TRUE,
                                 x_axis_lutning = 45,
                                 facet_legend_bottom = TRUE,
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_title = "Tid som arbetslös",
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")


  return(gg_list)

}
