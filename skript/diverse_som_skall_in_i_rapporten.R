# Har lagts till
source("https://raw.githubusercontent.com/Region-Dalarna/sarbarhetsanalys/refs/heads/main/Skript/diagram_arbetsloshet_76.R")
gg_arb_76 <- diagram_data_arbetsloshet_76(region_vekt =c("00","20"),
                                          output_mapp_data = "Output_mapp_data",
                                          output_mapp_figur = "Output_mapp_figur",
                                          vald_farg = diagramfarger("rus_sex"),
                                          spara_figur = FALSE,
                                          returnera_figur = TRUE,
                                          returnera_data = TRUE)

source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_ArbStatusM_scb.R")
arblosa_bakgr <- hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_scb(region_vekt = hamtakommuner(tamedlan = TRUE,tamedriket = TRUE),
                                                                                   alder_klartext = "20-64 år",
                                                                                   kon_klartext = c("kvinnor","män"),
                                                                                   fodelseregion_klartext = c("inrikes född", "utrikes född"),
                                                                                   cont_klartext = "arbetslöshet",
                                                                                   tid_koder = "9999") %>%
  mutate(region = region %>% skapa_kortnamn_lan(byt_ut_riket_mot_sverige = TRUE)) %>%
    manader_bearbeta_scbtabeller()

diagram_titel <- paste0("Arbetslöshet (", unique(arblosa_bakgr$ålder), ") i Dalarna i ", unique(arblosa_bakgr$månad_år))
diagramfil <- "andel_arbetslosa_kommun.png"

gg_obj <- SkapaStapelDiagram(skickad_df = arblosa_bakgr,
                             skickad_x_var = "region",
                             skickad_y_var = "arbetslöshet",
                             skickad_x_grupp = "kön",
                             facet_sort = FALSE,
                             x_axis_sort_value = FALSE,
                             x_axis_sort_grp = 3,
                             facet_scale = "fixed",
                             diagram_facet = TRUE,
                             facet_grp = "födelseregion",
                             vand_sortering = TRUE,
                             diagram_titel = diagram_titel,
                             #y_axis_100proc = TRUE,
                             diagram_capt = "diagram_capt",
                             stodlinjer_avrunda_fem = TRUE,
                             manual_y_axis_title = "procent",
                             manual_x_axis_text_vjust = 1,
                             manual_x_axis_text_hjust = 1,
                             manual_color = diagramfarger("rus_sex"),
                             skriv_till_diagramfil = FALSE,
                             output_mapp = "output_mapp",
                             filnamn_diagram = "diagramfil")


# Länkar till potentiellt intressant data - andel som studerar och inte gör det kopplat till ålder och vistelsetid (UVAS)
# Klart!
#https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003H/IntGr8LanKON1N/

# Hur fort man blir klar med SFI kopplat till utbildning
# Har lagt till diagram och skrivit text
#https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003H/IntGr8LanKON3/table/tableViewLayout1/


