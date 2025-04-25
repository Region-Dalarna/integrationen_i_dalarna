diag_fohm <- function(region = "20", # Enbart ett län i taget.
                                 visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                 alder_soc_rel = "16- år", # Finns även "16-84 år"
                                 tid_jmf = c("2005","2010","2015","2020","9999"),			 # "*" = alla år, "9999" = senaste, finns: "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2018", "2020", "2021", "2022", "2024"
                                 sociala_relationer_klartext = "Låg tillit till samhällets institutioner",			 #  Finns: "Avstått från att gå ut ensam på grund av rädsla", "Utsatt för fysiskt våld eller hot om våld", "Utsatt för fysiskt våld", "Utsatt för hot om våld", "Saknar emotionellt stöd", "Saknar praktiskt stöd", "Lågt socialt deltagande", "Svårt att lita på andra", "Utsatt för kränkande behandling eller bemötande", "Låg tillit till samhällets institutioner"
                                 logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                 output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                 skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                 returnera_data_rmarkdown = FALSE,
                                 demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {


  # =======================================================================================================================
  # Diagram för antalet inrikes/utrikes födda i arbetsför ålder
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


  kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Totalt", "Kvinnor", "Män"

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_sociala_relationer_riket_fodelseland_kon_ar_hlv1soccfod_fohm.R")


  sociala_relationer_df <- hamta_sociala_relationer_fodelseland_alder_kon_ar (alder_klartext = alder_soc_rel,
                                                                kon_klartext = kon_klartext,
                                                                fodelseland_klartext = "*",
                                                                andel_och_konfidensintervall_klartext = "Andel",
                                                                tid_koder = "9999",
                                                                sociala_relationer_klartext = sociala_relationer_klartext) %>%
    filter(Födelseland != "Totalt")


  if(returnera_data_rmarkdown == TRUE){
    assign("sociala_relationer_df", sociala_relationer_df, envir = .GlobalEnv)
  }

  gg_list <- list()


  diagram_capt <- "Källa: Folkhälsomyndighetens öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."

  sociala_relationer_df$Födelseland<- factor(andel_utrikes_bakgr_df$födelseregion, levels = c("Sverige","Övriga Norden","Övriga Europa","Övriga Världen"))

  diagramtitel <- paste0(sociala_relationer_klartext," i Sverige år ", max(sociala_relationer_df$År))

  namn <- chartr("åäö", "aao", tolower(str_replace_all(sociala_relationer_klartext, " ", "_")))
  diagramfilnamn <- paste0(namn,".png")

  gg_obj <- SkapaStapelDiagram(skickad_df = sociala_relationer_df %>%
                                 filter(Kön != "Totalt"),
                               skickad_x_var = "Födelseland",
                               skickad_y_var = last(names(sociala_relationer_df)),
                               skickad_x_grupp = "Kön",
                               manual_color = diagramfarger("kon"),
                               diagram_titel = diagramtitel,
                               diagram_capt = diagram_capt,
                               x_axis_lutning = 45,
                               procent_0_100_10intervaller = TRUE,
                               #geom_position_stack = TRUE,
                               legend_vand_ordning = TRUE,
                               diagram_liggande = FALSE,
                               manual_x_axis_text_hjust = 1,
                               manual_x_axis_text_vjust = 1,
                               manual_y_axis_title = "procent",
                               manual_x_axis_title = "Födelseland",
                               stodlinjer_avrunda_fem = TRUE,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = skriv_diagrambildfil)



  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")

  return(gg_list)

}
