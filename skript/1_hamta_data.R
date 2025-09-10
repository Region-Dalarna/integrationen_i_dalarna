# Skript som hämtar data och skapar figurer/variabler som används för att skapa markdown-rapporten. Det finns två alternativ för skriptet:

# 1: Kör skriptet utan att uppdatera data - sätt variabeln uppdatera_data till FALSE. Då läses den senast sparade versionen av R-studio global environment in.
# Detta är ett bra alternativ om man enbart vill ändra text eller liknande, men inte uppdatera data.

# 2: Uppdatera data - sätt variabeln uppdatera_data till FALSE. Då uppdateras data, alla figurer skapas på nytt och en ny enviroment sparas.
# Tar längre tid (ett par minuter) och medför en risk att text inte längre är aktuell då figurer har ändrats.


if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       here,
       scales,
       patchwork,
       glue)

# Skall data uppdateras? Annars läses data in från en sparad global environment-fil.
uppdatera_data = FALSE

if(uppdatera_data == TRUE){

  cat("Hämtning av data påbörjad")
  start_time <- Sys.time()

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

  Output_mapp = here("Data","/")
  Output_mapp_figur = here("Figurer","/")
  vald_region = "20"
  valt_lan = "20"
  spara_diagram_som_bildfiler = FALSE

  # Antal utrikes/inrikes födda i arbetsför ålder över tid
  source(here("skript/","andel_utrikes_inrikes_tidsserie.R"))
  gg_andel_utrikes_inrikes <- diag_bef_inr_utr_tid(output_mapp = Output_mapp_figur,
                                                   skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                   returnera_data_rmarkdown= TRUE)

  andel_utrikes_inrikes_min_ar <- min(andel_utrikes_inrikes_bakgr_df$år)
  andel_utrikes_inrikes_max_ar <- max(andel_utrikes_inrikes_bakgr_df$år)
  andel_utrikes_min_ar <- gsub("\\.",",",andel_utrikes_inrikes_bakgr_df %>% filter(år == andel_utrikes_inrikes_min_ar) %>% filter(födelseregion == "Utrikes födda") %>%  .$andel)
  andel_utrikes_max_ar <- gsub("\\.",",",andel_utrikes_inrikes_bakgr_df %>% filter(år == andel_utrikes_inrikes_max_ar) %>% filter(födelseregion == "Utrikes födda") %>%  .$andel)

  # andel_utrikes_inrikes_min_ar <- min(bef_bakgr_df$år)
  # antal_utrikes_inrikes_max_ar <- max(bef_bakgr_df$år)
  # antal_utrikes_forsta_ar <- format(bef_bakgr_df %>% filter(år == antal_utrikes_inrikes_min_ar) %>% filter(födelseregion == "Utrikes född") %>%  .$Antal,big.mark = " ")
  # antal_utrikes_max_ar <- format(bef_bakgr_df %>% filter(år == antal_utrikes_inrikes_max_ar) %>% filter(födelseregion == "Utrikes född") %>%  .$Antal,big.mark = " ")
  # antal_utrikes_skillnad <- format(bef_bakgr_df %>% filter(år == antal_utrikes_inrikes_max_ar) %>% filter(födelseregion == "Utrikes född") %>%  .$Antal - bef_bakgr_df %>% filter(år == antal_utrikes_inrikes_min_ar) %>% filter(födelseregion == "Utrikes född") %>%  .$Antal,big.mark = " ")
  #
  # antal_inrikes_forsta_ar <- format(bef_bakgr_df %>% filter(år == antal_utrikes_inrikes_min_ar) %>% filter(födelseregion == "Inrikes född") %>%  .$Antal,big.mark = " ")
  # antal_inrikes_max_ar <- format(bef_bakgr_df %>% filter(år == antal_utrikes_inrikes_max_ar) %>% filter(födelseregion == "Inrikes född") %>%  .$Antal,big.mark = " ")
  # antal_inrikes_skillnad <- format(abs(bef_bakgr_df %>% filter(år == antal_utrikes_inrikes_max_ar) %>% filter(födelseregion == "Inrikes född") %>%  .$Antal - bef_bakgr_df %>% filter(år == antal_utrikes_inrikes_min_ar) %>% filter(födelseregion == "Inrikes född") %>%  .$Antal),big.mark = " ")

  # Asylsökande 1984-senaste obseravation
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_antal_asylsokande_1984_IntRap.R")
  gg_asylsokande_antal <- diagram_asylsokande_tidsserie(output_mapp_figur = Output_mapp_figur,
                                                        spara_figur = spara_diagram_som_bildfiler,
                                                        returnera_data= TRUE)


  asylsokande_max_ar = max(asylsokande_df$år)
  asylsokande_2015 = format(asylsokande_df %>% filter(år == 2015) %>% .$Antal,big.mark = " ")
  asylsokande_senaste_ar_antal = format(asylsokande_df %>% filter(år == max(år)) %>% .$Antal,big.mark = " ")

  # Antal utrikes födda och förändring av antalet utrikes/inrikes födda i kommuner samt kumulativ summa på länsnivå och prognos (4)
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_bef_inrikes_utrikes_antal_forandring_prognos_IntRap.R")
  # Utan diagramtitel. Slås ihop med ett annat diagram
  gg_antal_utrikes_utan_titel <- diagram_utrikes_fodda_tidsserie(output_mapp_figur = Output_mapp_figur,
                                                      spara_figur = spara_diagram_som_bildfiler,
                                                      ta_bort_diagramtitel = TRUE,
                                                      diag_antal = FALSE,
                                                      x_axis_storlek = 9.5,
                                                      diag_forandring_lan = FALSE,
                                                      diag_forandring_prognos = FALSE,
                                                      returnera_data= TRUE)

  # Används till titel för sammanslaget diagram nedan
  min_ar <- names(antal_forandring_df)[4]
  max_ar <- names(antal_forandring_df)[5]

  # Med diagramtitel (de tre övriga)
  gg_antal_utrikes <- diagram_utrikes_fodda_tidsserie(output_mapp_figur = Output_mapp_figur,
                                                      diag_forandring_kommuner = FALSE,
                                                      spara_figur = spara_diagram_som_bildfiler,
                                                      returnera_data= TRUE,
                                                      alder_grupp = c(20,65),
                                                      prognos_ar = 2040)

  min_ar_utrikes_antal = min(antal_utrikes_region_df$år)
  max_ar_utrikes_antal = max(antal_utrikes_region_df$år)
  min_antal_utrikes = format(antal_utrikes_region_df %>% filter(år == min_ar_utrikes_antal) %>%  .$Antal,big.mark = " ")
  max_antal_utrikes = antal_utrikes_region_df %>% filter(år == max_ar_utrikes_antal) %>%  .$Antal %>% format(big.mark = " ")

  # Antal kommuner där befolkningen ökat totalt
  forandring_totalt_antal = antal_forandring_df %>% group_by(region) %>% summarize(netto = sum(forandring))
  antal_okning_kommun_total <- forandring_totalt_antal %>% filter(region!= "Dalarna",netto > 0) %>% nrow() # Antal kommuner som ökat i befolkning

  # Vilka kommuner har en ökande befolning inrikes
  kommuner_okning_inrikes <- str_c(rev(antal_forandring_df %>% filter(region!= "Dalarna",födelseregion == "Född i Sverige",forandring > 0) %>% .$region), collapse = " och ") # Antal kommuner som ökat i befolkning och är utrikes födda

  # Antal inrikes. Enbart för att få data
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bef_region_alder_kon_fodelseregion_tid_InrUtrFoddaRegAlKon_scb.R")
  antal_inrikes_df <- hamta_bef_region_alder_kon_fodelseregion_tid_scb(region_vekt = "20",
                                                                               alder_koder = NA,
                                                                               kon_klartext =  NA,
                                                                               tid_koder = c("2000","9999")) %>%
    mutate(region = skapa_kortnamn_lan(region))  %>% filter(födelseregion == "Född i Sverige",region == "Dalarna")

  min_ar_inrikes_antal = min(antal_inrikes_df$år)
  max_ar_inrikes_antal = max(antal_inrikes_df$år)
  min_antal_inrikes = format(antal_inrikes_df %>% filter(år == min_ar_inrikes_antal) %>%  .$Antal,big.mark = " ")
  max_antal_inrikes = antal_inrikes_df %>% filter(år == max_ar_inrikes_antal) %>%  .$Antal %>% format(big.mark = " ")

  andel_utrikes <- round((antal_utrikes_region_df %>% filter(år == max_ar_utrikes_antal) %>%  .$Antal)/(antal_utrikes_region_df %>% filter(år == max_ar_utrikes_antal) %>%  .$Antal+antal_inrikes_df %>% filter(år == max_ar_inrikes_antal) %>%  .$Antal)*100,0)

  min_ar_utrikes_kumulativ <- min(antal_forandring_lan_kumulativ$år)
  max_ar_utrikes_kumulativ <- max(antal_forandring_lan_kumulativ$år)
  kumulativ_summa_inrikes <- format(abs(antal_forandring_lan_kumulativ %>% filter(födelseregion == "Född i Sverige",år==max(år)) %>%  .$kumulativ_summa),big.mark = " ")
  kumulativ_summa_utrikes <- format(abs(antal_forandring_lan_kumulativ %>% filter(födelseregion == "Utrikes född",år==max(år)) %>%  .$kumulativ_summa),big.mark = " ")

  prognos_ar <- 2040
  min_ar_utrikes_kumulativ_prognos <- min(antal_forandring_prognos_kumulativ$år)
  max_ar_utrikes_kumulativ_prognos <- max(antal_forandring_prognos_kumulativ %>% filter(år<= prognos_ar) %>% .$år)
  kumulativ_summa_inrikes_max_prognos <- format(round(antal_forandring_prognos_kumulativ %>% filter(födelseregion == "inrikes födda",år==max_ar_utrikes_kumulativ_prognos) %>%  .$kumulativ_summa,0),big.mark = " ")
  kumulativ_summa_inrikes_max_prognos_abs <- format(abs(round(antal_forandring_prognos_kumulativ %>% filter(födelseregion == "inrikes födda",år==max_ar_utrikes_kumulativ_prognos) %>%  .$kumulativ_summa,0)),big.mark = " ")
  kumulativ_summa_utrikes_max_prognos <- format(round(antal_forandring_prognos_kumulativ %>% filter(födelseregion == "utrikes födda",år==max_ar_utrikes_kumulativ_prognos) %>%  .$kumulativ_summa,0),big.mark = " ")
  kumulativ_summa_utrikes_max_prognos_abs <- format(abs(round(antal_forandring_prognos_kumulativ %>% filter(födelseregion == "utrikes födda",år==max_ar_utrikes_kumulativ_prognos) %>%  .$kumulativ_summa,0)),big.mark = " ")

  # Andel utrikes födda (diagram)

  source(here("skript/","andel_utrikes.R"))
  gg_andel_utrikes <- diag_andel_utrikes_scb(output_mapp = Output_mapp_figur,
                                             skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                             start_ar = "2000",
                                             valda_farger = diagramfarger("rus_sex")[3:4],
                                             x_axis_storlek = 9.5,
                                             ta_bort_diagramtitel = TRUE,
                                             ta_bort_caption = TRUE,
                                             returnera_data_rmarkdown= TRUE)

  andel_utrikes_forsta_ar <- min(andel_utrikes_df$år)
  andel_utrikes_senaste_ar <- max(andel_utrikes_df$år)

  andel_utrikes_Sverige_sista_ar <- round(andel_utrikes_df %>% filter(år == andel_utrikes_senaste_ar,region == "Sverige") %>%  .$andel_utrikes,0)
  andel_utrikes_kommun_hogst_sista_ar <- andel_utrikes_df %>%filter(!(region%in%c("Sverige","Dalarna"))) %>% filter(år == andel_utrikes_senaste_ar ,andel_utrikes==max(andel_utrikes)) %>%  .$region
  andel_utrikes_kommun_hogst_sista_ar_varde <- round(andel_utrikes_df %>% filter(år == andel_utrikes_senaste_ar,region == andel_utrikes_kommun_hogst_sista_ar) %>%  .$andel_utrikes,0)
  andel_utrikes_kommun_hogst_forsta_ar_varde <- round(andel_utrikes_df %>% filter(år == min(år),region == andel_utrikes_kommun_hogst_sista_ar) %>%  .$andel_utrikes,0)

  andel_utrikes_kommun_hogst_forandring_varde <- round(andel_utrikes_df %>% filter(år == andel_utrikes_senaste_ar,region == andel_utrikes_kommun_hogst_sista_ar) %>%  .$andel_utrikes-andel_utrikes_df %>% filter(år == min(år),region == andel_utrikes_kommun_hogst_sista_ar) %>%  .$andel_utrikes,0)

  andel_utrikes_smedjebacken_forandring_varde <- round(andel_utrikes_df %>% filter(år == andel_utrikes_senaste_ar,region == "Smedjebacken") %>%  .$andel_utrikes-andel_utrikes_df %>% filter(år == min(år),region == "Smedjebacken") %>%  .$andel_utrikes,0)

  andel_utrikes_Gagnef_forandring_varde <- round(andel_utrikes_df %>% filter(år == andel_utrikes_senaste_ar,region == "Gagnef") %>%  .$andel_utrikes-andel_utrikes_df %>% filter(år == min(år),region == "Gagnef") %>%  .$andel_utrikes,0)

  andel_utrikes_Borlänge_sista_ar <- round(andel_utrikes_df %>% filter(år == andel_utrikes_senaste_ar,region == "Borlänge") %>%  .$andel_utrikes,0)
  andel_utrikes_Avesta_sista_ar <- round(andel_utrikes_df %>% filter(år == andel_utrikes_senaste_ar,region == "Avesta") %>%  .$andel_utrikes,0)

  andel_utrikes_kommun_lagst_sista_ar <- andel_utrikes_df %>%filter(!(region%in%c("Sverige","Dalarna")),år == andel_utrikes_senaste_ar) %>% filter(andel_utrikes==min(andel_utrikes)) %>%  .$region
  andel_utrikes_kommun_lagst_sista_ar_varde <- round(andel_utrikes_df %>% filter(år == andel_utrikes_senaste_ar,region == andel_utrikes_kommun_lagst_sista_ar) %>%  .$andel_utrikes,0)
  andel_utrikes_kommun_lagst_forsta_ar_varde <- round(andel_utrikes_df %>% filter(år == min(år),region == andel_utrikes_kommun_lagst_sista_ar) %>%  .$andel_utrikes,0)

  # Kombinerar två av diagrammen

  gg_kombo <- gg_antal_utrikes_utan_titel$befolkningsforandring_Dalarna+gg_andel_utrikes$utrikes_fodda_andel_Dalarna+
    plot_annotation(
      title = glue("Befolkningsförändring {min_ar}-{max_ar} och andel utrikes födda i Dalarna"),
      theme = theme(plot.title = element_text(hjust = 0.5,size = 18))
    )

  if(spara_diagram_som_bildfiler){
    ggsave(output_mapp_figur)
  }
  # Största födelseland bland utrikes födda i Dalarna
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_storsta_fodelseland_IntRap.R")
  gg_storsta_fodelseland <- diagram_storsta_fodelseland(output_mapp_figur = Output_mapp_figur,
                                                        spara_figur = spara_diagram_som_bildfiler,
                                                        returnera_data= TRUE)


  fodelseland_forsta_ar <- min(storsta_fodelseland_df$år)
  fodelseland_senaste_ar <- max(storsta_fodelseland_df$år)

  storsta_fodelseland_senaste_ar <- storsta_fodelseland_df %>% filter(år == fodelseland_senaste_ar) %>% filter(Antal == max(Antal)) %>%  .$födelseregion
  storsta_fodelseland_senaste_ar_antal <- format(storsta_fodelseland_df %>% filter(år == fodelseland_senaste_ar) %>% filter(Antal == max(Antal)) %>%  .$Antal,big.mark = " ")
  andra_fodelseland_senaste_ar <- storsta_fodelseland_df %>% filter(år == fodelseland_senaste_ar,födelseregion != storsta_fodelseland_senaste_ar) %>% filter(Antal == max(Antal)) %>%  .$födelseregion
  andra_fodelseland_senaste_ar_antal <- format(storsta_fodelseland_df %>% filter(år == fodelseland_senaste_ar,födelseregion != storsta_fodelseland_senaste_ar) %>% filter(Antal == max(Antal)) %>%  .$Antal,big.mark = " ")

  ############################################################
  ########## Antal utrikes födda efter invandringsår #########
  ############################################################

  source(here("skript","diag_utbildingsniva_invandringsar.R"))
  gg_antal_invandringsar <- diag_utbsniva_invandringsar(output_mapp = Output_mapp_figur,
                                                           skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                           diag_utb = FALSE,
                                                           returnera_data_rmarkdown = TRUE)

  invandringsar_antal_df_max_ar <- invandringsar_antal_df %>% filter(Antal== max(Antal)) %>% .$Invandringsar
  invandringsar_antal_df_max_antal <- invandringsar_antal_df %>% filter(Antal== max(Antal)) %>% .$Antal
  invandringsar_senaste <- max(invandringsar_antal_df$Invandringsar)
  invandringsar_senaste_antal <- format(invandringsar_antal_df %>% filter(Invandringsar== invandringsar_senaste) %>% .$Antal,big.mark = " ")


  ############################################################
  ########## Befolkningspyramid för Inrikes/utrikes ##########
  ############################################################

  source("G:/skript/diagram/diag_befpyramid.R")
  gg_befpyramid <- diag_befpyramid(geo_vekt = vald_region,
                                   jmfr_linje = "utr_inr",
                                   output_mapp = Output_mapp_figur)

  ##########################################################################################################################
  # Befolkningsförändring över tid fördelad på komponenterna födelseöverskott, inrikes flyttnetto och invandringsöverskott #
  ##########################################################################################################################
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_befutv_per_komponent_ar_scb.R")
  gg_befforandring_komponenter <- diag_befutv_per_komponent_ar(region_vekt = vald_region,
                                                               x_axis_visa_var_xe_etikett = 4,
                                                               output_mapp = Output_mapp_figur,
                                                               facet_x_axis_storlek = 8,
                                                               skriv_till_diagramfil  = spara_diagram_som_bildfiler,
                                                               returnera_dataframe_global_environment = TRUE)

  befforandring_komponent_min_ar <- min(befutv_per_komponent_ar_scb_df$år)
  befforandring_komponent_max_ar <- max(befutv_per_komponent_ar_scb_df$år)

  fodelseunderskott_netto <- format(abs(sum(befutv_per_komponent_ar_scb_df %>% filter(förändringar == "Födelseöverskott") %>%  .$personer)),big.mark = " ")
  inrikes_flyttningsoverskott_netto <- format(abs(sum(befutv_per_komponent_ar_scb_df %>% filter(förändringar == "Inrikes flyttnetto") %>%  .$personer)),big.mark = " ")
  invandringsoverskott_netto <- format(sum(befutv_per_komponent_ar_scb_df %>% filter(förändringar == "Invandringsöverskott") %>%  .$personer),big.mark = " ")

  ####################################
  ### SFI kopplat till vistelsetid ###
  ####################################
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_SFI_godk%C3%A4nd_vistelsetid_kon_tid_IntRap.R")
  gg_SFI <- diag_SFI_bakgrund(output_mapp  = Output_mapp_figur,
                              skriv_diagrambildfil = spara_diagram_som_bildfiler,
                              returnera_data_rmarkdown = TRUE)

  sfi_min_ar <- min(SFI_df$år)
  sfi_max_ar <- max(SFI_df$år)

  # Könsuppdelat
  sfi_kvinnor_forgymnasial <- SFI_df %>% filter(kön == "kvinnor",variabel == "Förgymnasial utbildning") %>% filter(år == max(år)) %>% .$`Vistelsetid för godkända i sfi, median i antal dagar`
  sfi_kvinnor_gymnasial <- SFI_df %>% filter(kön == "kvinnor",variabel == "Gymnasial utbildning") %>% filter(år == max(år)) %>% .$`Vistelsetid för godkända i sfi, median i antal dagar`
  sfi_kvinnor_eftergymnasial <- SFI_df %>% filter(kön == "kvinnor",variabel == "Eftergymnasial utbildning") %>% filter(år == max(år)) %>% .$`Vistelsetid för godkända i sfi, median i antal dagar`

  sfi_man_forgymnasial <- SFI_df %>% filter(kön == "män",variabel == "Förgymnasial utbildning") %>% filter(år == max(år)) %>% .$`Vistelsetid för godkända i sfi, median i antal dagar`
  sfi_man_gymnasial <- SFI_df %>% filter(kön == "män",variabel == "Gymnasial utbildning") %>% filter(år == max(år)) %>% .$`Vistelsetid för godkända i sfi, median i antal dagar`
  sfi_man_eftergymnasial <- SFI_df %>% filter(kön == "män",variabel == "Eftergymnasial utbildning") %>% filter(år == max(år)) %>% .$`Vistelsetid för godkända i sfi, median i antal dagar`

  # Tidsserie
  sfi_forstaar_varde_forgymnasial <- SFI_df %>% filter(kön == "män och kvinnor",variabel == "Förgymnasial utbildning") %>% filter(år == sfi_min_ar) %>% .$`Vistelsetid för godkända i sfi, median i antal dagar`
  sfi_forstaar_varde_gymnasial <- SFI_df %>% filter(kön == "män och kvinnor",variabel == "Gymnasial utbildning") %>% filter(år == sfi_min_ar) %>% .$`Vistelsetid för godkända i sfi, median i antal dagar`
  sfi_forstaar_varde_eftergymnasial <- SFI_df %>% filter(kön == "män och kvinnor",variabel == "Eftergymnasial utbildning") %>% filter(år == sfi_min_ar) %>% .$`Vistelsetid för godkända i sfi, median i antal dagar`

  sfi_sistaar_varde_forgymnasial <- SFI_df %>% filter(kön == "män och kvinnor",variabel == "Förgymnasial utbildning") %>% filter(år == sfi_max_ar) %>% .$`Vistelsetid för godkända i sfi, median i antal dagar`
  sfi_sistaar_varde_gymnasial <- SFI_df %>% filter(kön == "män och kvinnor",variabel == "Gymnasial utbildning") %>% filter(år == sfi_max_ar) %>% .$`Vistelsetid för godkända i sfi, median i antal dagar`
  sfi_sistaar_varde_eftergymnasial <- SFI_df %>% filter(kön == "män och kvinnor",variabel == "Eftergymnasial utbildning") %>% filter(år == sfi_max_ar) %>% .$`Vistelsetid för godkända i sfi, median i antal dagar`


  ###########################
  ## Utb-nivå vistelsetid ##
  ##########################
  source(here("skript","diag_utbildingsniva_invandringsar.R"))
  gg_utb_niva_invandringsar <- diag_utbsniva_invandringsar(output_mapp = Output_mapp_figur,
                                                           skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                           diag_invandringsar = TRUE,
                                                           returnera_data_rmarkdown = TRUE)

  utb_niva_vistelsetid_2_3_eftergym <- round(utb_invandringsar_df %>% filter(Utbildningsniva == "Eftergymnasial utbildning 3 år eller längre",Vistelsetid == "2-3 år") %>% .$andel,0)


  ########################
  # Arbetsmarknadsstatus #
  ########################

  # Arbetslöshet från 1976 och framåt
  source("https://raw.githubusercontent.com/Region-Dalarna/sarbarhetsanalys/refs/heads/main/Skript/diagram_arbetsloshet_76.R")
  gg_arb_76 <- diagram_data_arbetsloshet_76(region_vekt =c("00","20"),
                                            output_mapp_figur = Output_mapp_figur,
                                            vald_farg = diagramfarger("rus_sex"),
                                            spara_figur = FALSE,
                                            returnera_figur = TRUE,
                                            returnera_data = TRUE)

  arbloshet_76_senaste_ar <- max(arbetsloshet_76$år)


  # Arbetslöshet uppdelat på kön, bakgrund och kommun
  source(here("skript/","arbetsloshet_kommun_bakgr.R"))
  gg_arb_bakgr <- diag_arbetsloshet_kommun(output_mapp = Output_mapp_figur,
                                           skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                           returnera_data_rmarkdown= TRUE)

  arb_bakgr_manad_ar <- unique(arblosa_bakgr_df$månad_år)
  arb_bakgr_utrikes_kvinnor_max <- arblosa_bakgr_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>% filter(arbetslöshet == max(arbetslöshet)) %>%  .$region
  arb_bakgr_utrikes_kvinnor_max_varde <- gsub("\\.",",",arblosa_bakgr_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>% filter(arbetslöshet == max(arbetslöshet)) %>%  .$arbetslöshet)
  arb_bakgr_inrikes_kvinnor_max_varde <-  gsub("\\.",",",arblosa_bakgr_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född",region == arb_bakgr_utrikes_kvinnor_max) %>% filter(arbetslöshet == max(arbetslöshet)) %>%  .$arbetslöshet)
  arb_bakgr_utrikes_män_max <- arblosa_bakgr_df %>% filter(kön == "män",födelseregion == "utrikes född") %>% filter(arbetslöshet == max(arbetslöshet)) %>%  .$region
  arb_bakgr_utrikes_män_max_varde <- gsub("\\.",",",arblosa_bakgr_df %>% filter(kön == "män",födelseregion == "utrikes född") %>% filter(arbetslöshet == max(arbetslöshet)) %>%  .$arbetslöshet)
  arb_bakgr_inrikes_män_max_varde <-  gsub("\\.",",",arblosa_bakgr_df %>% filter(kön == "män",födelseregion == "inrikes född",region == arb_bakgr_utrikes_män_max) %>% filter(arbetslöshet == max(arbetslöshet)) %>%  .$arbetslöshet)

  arb_bakgr_utrikes_kvinnor_min <- arblosa_bakgr_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>% filter(arbetslöshet == min(arbetslöshet)) %>%  .$region
  arb_bakgr_utrikes_kvinnor_min_varde <- gsub("\\.",",",arblosa_bakgr_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>% filter(arbetslöshet == min(arbetslöshet)) %>%  .$arbetslöshet)
  arb_bakgr_inrikes_kvinnor_min_varde <-  gsub("\\.",",",arblosa_bakgr_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född",region == arb_bakgr_utrikes_kvinnor_min) %>% filter(arbetslöshet == min(arbetslöshet)) %>%  .$arbetslöshet)
  arb_bakgr_utrikes_män_min <- arblosa_bakgr_df %>% filter(kön == "män",födelseregion == "utrikes född") %>% filter(arbetslöshet == min(arbetslöshet)) %>%  .$region
  arb_bakgr_utrikes_män_min_varde <- gsub("\\.",",",arblosa_bakgr_df %>% filter(kön == "män",födelseregion == "utrikes född") %>% filter(arbetslöshet == min(arbetslöshet)) %>%  .$arbetslöshet)
  arb_bakgr_inrikes_män_min_varde <-  gsub("\\.",",",arblosa_bakgr_df %>% filter(kön == "män",födelseregion == "inrikes född",region == arb_bakgr_utrikes_män_min) %>% filter(arbetslöshet == min(arbetslöshet)) %>%  .$arbetslöshet)

  # Utbildningsnivå kopplat till matchning
  # source("https://raw.githubusercontent.com/Region-Dalarna/socioekonomisk_analys_nms/refs/heads/main/skript/socioek_matchning_bakgr_utbniva.R")
  # gg_matchning_bakgr <- skapa_matcning_utbniva_bakgrund_diagram(returnera_dataframe_global_environment = TRUE,
  #                                                               output_mapp = NA)

  # Arbetslöshet tidsserie

  # source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_arbetsmarknadsstatus_tidsserie_SCB.R")
  # gg_arbetsloshet_tidsserie <- diagram_arbetsmarknadsstatus_tidsserie (spara_figur = spara_diagram_som_bildfiler,
  #                                                                      output_mapp_figur = Output_mapp_figur,
  #                                                                      returnera_data = TRUE,
  #                                                                      marginal_yaxis_facet = c(0.02,0.02),
  #                                                                      diagram_ej_upp = FALSE,
  #                                                                      diagram_facet = TRUE,
  #                                                                      returnera_figur = TRUE)

  source("G:/skript/diagram/diag_bas_syss_arblosa_inr_utr_fodda_manad.R")
  gg_arbetsloshet_tidsserie <- diag_bas_arblosa_inr_utr_fodda_manad(vald_region = valt_lan,
                                                                    skriv_diagram = spara_diagram_som_bildfiler,
                                                                    dataetiketter = c(FALSE),
                                                                    x_axis_visa_var_xe_etikett = 6,
                                                                    returnera_dataframe_global_environment = TRUE,
                                                                    output_mapp = Output_mapp_figur)



  arbetsloshet_tidsserie_ar <-  unique(last(arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="inrikes född") %>% .$år))
  arbetsloshet_tidsserie_manad <- unique(last(arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="inrikes född") %>% .$månad))
  arbetsloshet_tidserie_Dalarna_inrikes_varde <- gsub("\\.",",",last(arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="inrikes född") %>% .$arbetslöshet))
  arbetsloshet_tidserie_Dalarna_utrikes_varde <- gsub("\\.",",",last(arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="utrikes född") %>% .$arbetslöshet))

  # Hämta antal arbetslösa (till texten enbart)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_ArbStatusM_scb.R")
  arbstatus_df <- hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_scb(region_vekt = "20",
                                                                                    alder_klartext = unique(arbetsmarknadsstatus_tidsserie$ålder),
                                                                                    kon_klartext = "totalt",
                                                                                    fodelseregion_klartext = unique(arbetsmarknadsstatus_tidsserie$födelseregion),
                                                                                    cont_klartext = "antal arbetslösa",
                                                                                    wide_om_en_contvar = FALSE,
                                                                                    tid_koder = "9999") %>%
    manader_bearbeta_scbtabeller()

  arblosa_utrikes <- format(arbstatus_df %>% filter(födelseregion == "utrikes född") %>% .$varde,big.mark = " ")
  arblosa_inrikes <- format(arbstatus_df %>% filter(födelseregion == "inrikes född") %>% .$varde,big.mark = " ")
  arblosa_manad <- unique(arbstatus_df$månad)
  arblosa_ar <- unique(arbstatus_df$år)
  arblosa_alder <- unique(arbstatus_df$ålder)

  #######################
  ## Tid som arbetslös ##
  #######################
  source(here("skript/","diag_arbetsloshetstid_bakgrund.R"))
  gg_arbetsloshetstid <- diag_arbetsloshetstid_bakgrund(output_mapp = Output_mapp_figur,
                                                      skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                      returnera_data_rmarkdown= TRUE)

  arbetsloshetstid_ar <- unique(arbetsloshetstid_df$År)
  arbesloshetstid_aldersgrupp <- unique(arbetsloshetstid_df$Aldersgrupp)

  arbetsloshetstid_inrikes_6_manader <- round(arbetsloshetstid_df %>% filter(Bakgrund == "Inrikes född",Arbetsloshetsstatus == "< 6 månader",Kon == "Totalt") %>%  .$andel,0)
  arbetsloshetstid_inrikes_24_manader <- round(arbetsloshetstid_df %>% filter(Bakgrund == "Inrikes född",Arbetsloshetsstatus == "24- månader",Kon == "Totalt") %>%  .$andel,0)
  arbetsloshetstid_inrikes_minst_12_manader <- round(arbetsloshetstid_df %>% filter(Bakgrund == "Inrikes född",Arbetsloshetsstatus %in% c("12-24 månader","24- månader"),Kon == "Totalt") %>%  .$andel %>% sum(),0)

  arbetsloshetstid_utrikes_6_manader <- round(arbetsloshetstid_df %>% filter(Bakgrund == "Utrikes född",Arbetsloshetsstatus == "< 6 månader",Kon == "Totalt") %>%  .$andel,0)
  arbetsloshetstid_utrikes_24_manader <- round(arbetsloshetstid_df %>% filter(Bakgrund == "Utrikes född",Arbetsloshetsstatus == "24- månader",Kon == "Totalt") %>%  .$andel,0)
  arbetsloshetstid_utrikes_minst_12_manader <- round(arbetsloshetstid_df %>% filter(Bakgrund == "Utrikes född",Arbetsloshetsstatus %in% c("12-24 månader","24- månader"),Kon == "Totalt") %>%  .$andel %>% sum(),0)


  ######################################
  ## Sysselsättningsstatus, tidsserie ##
  ######################################
  source(here("skript/","sysselsattningsgrad_tidsserie_linje.R"))
  gg_sysselsattningsgrad_tidsserie <- diag_sysselsattningsgrad_tidsserie(output_mapp = Output_mapp_figur,
                                                                         diag_stapel = TRUE,
                                                                         diag_linje = TRUE,
                                                                         skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                                         returnera_data_rmarkdown= TRUE)

  # Data som relaterar till diagram från 1993.
  syssgrad_93_forsta_ar <- min(sysselsattningsgrad_tidsserie_df$år)
  syssgrad_93_senaste_ar <- max(sysselsattningsgrad_tidsserie_df$år)

  syssgrad_93_kvinnor_inrikes_forsta_ar_procent <- gsub("\\.",",",sysselsattningsgrad_tidsserie_df %>% filter(kön == "kvinnor",födelseregion == "inrikes födda",år == min(år)) %>%  .$sysselsättningsgrad)
  syssgrad_93_kvinnor_inrikes_max_ar_procent <- gsub("\\.",",",sysselsattningsgrad_tidsserie_df %>% filter(kön == "kvinnor",födelseregion == "inrikes födda",år == max(år)) %>%  .$sysselsättningsgrad)
  syssgrad_93_kvinnor_utrikes_forsta_ar_procent <- gsub("\\.",",",sysselsattningsgrad_tidsserie_df %>% filter(kön == "kvinnor",födelseregion == "utrikes födda",år == min(år)) %>%  .$sysselsättningsgrad)
  syssgrad_93_kvinnor_utrikes_max_ar_procent <- gsub("\\.",",",sysselsattningsgrad_tidsserie_df %>% filter(kön == "kvinnor",födelseregion == "utrikes födda",år == max(år)) %>%  .$sysselsättningsgrad)

  syssgrad_93_män_inrikes_forsta_ar_procent <- gsub("\\.",",",sysselsattningsgrad_tidsserie_df %>% filter(kön == "män",födelseregion == "inrikes födda",år == min(år)) %>%  .$sysselsättningsgrad)
  syssgrad_93_män_inrikes_max_ar_procent <- gsub("\\.",",",sysselsattningsgrad_tidsserie_df %>% filter(kön == "män",födelseregion == "inrikes födda",år == max(år)) %>%  .$sysselsättningsgrad)
  syssgrad_93_män_utrikes_forsta_ar_procent <- gsub("\\.",",",sysselsattningsgrad_tidsserie_df %>% filter(kön == "män",födelseregion == "utrikes födda",år == min(år)) %>%  .$sysselsättningsgrad)
  syssgrad_93_män_utrikes_max_ar_procent <- gsub("\\.",",",sysselsattningsgrad_tidsserie_df %>% filter(kön == "män",födelseregion == "utrikes födda",år == max(år)) %>%  .$sysselsättningsgrad)

  syssgrad_tidsserie_forsta_ar <- min(sysselsattningsgrad_tidsserie_jmf_2017_df$år)
  syssgrad_tidsserie_senaste_ar <- max(sysselsattningsgrad_tidsserie_jmf_2017_df$år)
  syssgrad_tidsserie_utrikes_min_ar <- gsub("\\.",",",sysselsattningsgrad_tidsserie_jmf_2017_df %>% filter(födelseregion == "utrikes födda",år == min(år)) %>%  .$sysselsättningsgrad)
  syssgrad_tidsserie_utrikes_max_ar <- gsub("\\.",",",sysselsattningsgrad_tidsserie_jmf_2017_df %>% filter(födelseregion == "utrikes födda",år == max(år)) %>%  .$sysselsättningsgrad)
  syssgrad_tidsserie_inrikes_min_ar <- gsub("\\.",",",sysselsattningsgrad_tidsserie_jmf_2017_df %>% filter(födelseregion == "inrikes födda",år == min(år)) %>%  .$sysselsättningsgrad)
  syssgrad_tidsserie_inrikes_max_ar <- gsub("\\.",",",sysselsattningsgrad_tidsserie_jmf_2017_df %>% filter(födelseregion == "inrikes födda",år == max(år)) %>%  .$sysselsättningsgrad)
  #############################################
  ## Sysselsättninggrad, vistelsetid/inrikes ##
  #############################################
  source(here("skript/","syssgrad_vistelsetid_inrikes.R"))
  gg_syssgrad_vistelsetid_utb <- diag_sysselsattningsgrad_vistelsetid_inrikes_scb(output_mapp = Output_mapp_figur,
                                                                                  diag_vistelsetid = TRUE,
                                                                                  diag_utbniva = TRUE,
                                                                                  skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                                                  returnera_data_rmarkdown= TRUE)

  syssgrad_vistelsetid_ar <- unique(syssgrad_vistelsetid_inrikes_df$år)
  syssgrad_forgym_utrikes <- gsub("\\.",",",syssgrad_utrikes_inrikes_utbniva_df %>% filter(utbildningsnivå == "Förgymnasial utbildning",bakgrundsvariabel == "Utrikes född") %>%  .$`Andel sysselsatta`)
  syssgrad_forgym_inrikes <- gsub("\\.",",",syssgrad_utrikes_inrikes_utbniva_df %>% filter(utbildningsnivå == "Förgymnasial utbildning",bakgrundsvariabel == "Inrikes född") %>%  .$`Andel sysselsatta`)
  syssgrad_forgym_skillnad <- gsub("\\.",",",syssgrad_utrikes_inrikes_utbniva_df %>% filter(utbildningsnivå == "Förgymnasial utbildning",bakgrundsvariabel == "Inrikes född") %>%  .$`Andel sysselsatta`-syssgrad_utrikes_inrikes_utbniva_df %>% filter(utbildningsnivå == "Förgymnasial utbildning",bakgrundsvariabel == "Utrikes född") %>%  .$`Andel sysselsatta`)

  syssgrad_gym_utrikes <- gsub("\\.",",",syssgrad_utrikes_inrikes_utbniva_df %>% filter(utbildningsnivå == "Gymnasial utbildning",bakgrundsvariabel == "Utrikes född") %>%  .$`Andel sysselsatta`)
  syssgrad_gym_inrikes <- gsub("\\.",",",syssgrad_utrikes_inrikes_utbniva_df %>% filter(utbildningsnivå == "Gymnasial utbildning",bakgrundsvariabel == "Inrikes född") %>%  .$`Andel sysselsatta`)
  syssgrad_gym_skillnad <- gsub("\\.",",",syssgrad_utrikes_inrikes_utbniva_df %>% filter(utbildningsnivå == "Gymnasial utbildning",bakgrundsvariabel == "Inrikes född") %>%  .$`Andel sysselsatta`-syssgrad_utrikes_inrikes_utbniva_df %>% filter(utbildningsnivå == "Gymnasial utbildning",bakgrundsvariabel == "Utrikes född") %>%  .$`Andel sysselsatta`)

  syssgrad_eftergym_utrikes <- gsub("\\.",",",syssgrad_utrikes_inrikes_utbniva_df %>% filter(utbildningsnivå == "Eftergymnasial utbildning",bakgrundsvariabel == "Utrikes född") %>%  .$`Andel sysselsatta`)
  syssgrad_eftergym_inrikes <- gsub("\\.",",",syssgrad_utrikes_inrikes_utbniva_df %>% filter(utbildningsnivå == "Eftergymnasial utbildning",bakgrundsvariabel == "Inrikes född") %>%  .$`Andel sysselsatta`)
  syssgrad_eftergym_skillnad <- gsub("\\.",",",syssgrad_utrikes_inrikes_utbniva_df %>% filter(utbildningsnivå == "Eftergymnasial utbildning",bakgrundsvariabel == "Inrikes född") %>%  .$`Andel sysselsatta`-syssgrad_utrikes_inrikes_utbniva_df %>% filter(utbildningsnivå == "Eftergymnasial utbildning",bakgrundsvariabel == "Utrikes född") %>%  .$`Andel sysselsatta`)

  ##########
  ## Län ##
  #########
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diagram_arbetsmarknadsstatus_senastear.R")
  gg_diagram_arbetsmarknadsstatus_lan <- diagram_arbetsmarknadsstatus(region_vekt = hamtaAllaLan(),
                                                                         kon_klartext = c("kvinnor","män"),
                                                                         alder_klartext = "20-64 år",
                                                                         valda_farger = diagramfarger("kon"),
                                                                         fodelseregion_klartext_vekt = c("inrikes född", "utrikes född"),
                                                                         diag_arbetskraftsdeltagande = FALSE,
                                                                         returnera_data = TRUE,
                                                                         spara_figur = FALSE,
                                                                         data_namm = "arbetsmarknadsstatus_lan_df")

  # Variabler sysselsättningsgrad län
  arbetsloshesstatus_manad <- unique(arbetsmarknadsstatus_lan_df$manad_long)
  arbetsloshesstatus_ar <- unique(arbetsmarknadsstatus_lan_df$ar)


  # Sysselsättningsgrad inrikes Sverige
  syssgrad_inrikes_man_Sverige <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "inrikes född",region == "Sverige",variabel == "sysselsättningsgrad" ) %>%  .$varde)

  syssgrad_inrikes_kvinna_Sverige <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född",region == "Sverige",variabel == "sysselsättningsgrad" ) %>%  .$varde)

  # Sysselsättningsgrad utrikes Dalarna
  syssgrad_utrikes_man_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född",region == "Dalarna",variabel == "sysselsättningsgrad" ) %>%  .$varde)

  syssgrad_utrikes_kvinna_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född",region == "Dalarna",variabel == "sysselsättningsgrad" ) %>%  .$varde)

  syssgrad_utrikes_skillnad_Dalarna <- round(arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född",region == "Dalarna",variabel == "sysselsättningsgrad" ) %>%  .$varde-arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född",region == "Dalarna",variabel == "sysselsättningsgrad" ) %>%  .$varde,0)

  # Variabler arbetslöshet lan

  # Arbetslöshet inrikes/utrikes och skillnad inrikes/utrikes Dalarna
  arblosthet_inrikes_man_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "inrikes född",region == "Dalarna",variabel == "arbetslöshet" ) %>%  .$varde)

  arblosthet_inrikes_kvinna_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född",region == "Dalarna",variabel == "arbetslöshet" ) %>%  .$varde)

  arblosthet_utrikes_man_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född",region == "Dalarna",variabel == "arbetslöshet" ) %>%  .$varde)

  arblosthet_utrikes_kvinna_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född",region == "Dalarna",variabel == "arbetslöshet" ) %>%  .$varde)

  arblosthet_kvinnor_utrikes_inrikes_skillnad_Dalarna <- round(arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född",region == "Dalarna",variabel == "arbetslöshet" ) %>%  .$varde-arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född",region == "Dalarna",variabel == "arbetslöshet") %>%  .$varde,0)

  arblosthet_man_utrikes_inrikes_skillnad_Dalarna <- round(arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född",region == "Dalarna",variabel == "arbetslöshet" ) %>%  .$varde-arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "inrikes född",region == "Dalarna",variabel == "arbetslöshet") %>%  .$varde,0)

  # Högst arbetslöshet län utrikes
  arblosthet_utrikes_man_max_lan <- arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
    filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region

  arblosthet_utrikes_man_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
                                           filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde)

  arblosthet_utrikes_kvinna_max_lan <- arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
    filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region

  arblosthet_utrikes_kvinna_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
                                              filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde)

  # Lägst arbetslöshet län utrikes

  arblosthet_utrikes_man_min_lan <- arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
    filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$region

  arblosthet_utrikes_man_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
                                             filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$varde)

  arblosthet_utrikes_kvinna_min_lan <- arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
    filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$region

  arblosthet_utrikes_kvinna_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
                                                filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$varde)


  ############
  ## Kommun ##
  ############
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diagram_arbetsmarknadsstatus_senastear.R")
  gg_diagram_arbetsmarknadsstatus_kommun <- diagram_arbetsmarknadsstatus(region_vekt = hamtakommuner(),
                                                                         kon_klartext = c("kvinnor","män"),
                                                                         alder_klartext = "20-64 år",
                                                                         valda_farger = diagramfarger("kon"),
                                                                         fodelseregion_klartext_vekt = c("inrikes född", "utrikes född"),
                                                                         diag_arbetskraftsdeltagande = FALSE,
                                                                         returnera_data = TRUE,
                                                                         spara_figur = FALSE,
                                                                         data_namm = "arbetsmarknadsstatus_kommun_df")

  # Variabler sysselsättningsgrad kommun

  # Högst sysselsättningsgrad
  syssgrad_utrikes_man_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
    filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == max(varde)) %>%  .$region

  syssgrad_utrikes_man_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
    filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == max(varde)) %>%  .$varde)

  syssgrad_utrikes_kvinna_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
    filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == max(varde)) %>%  .$region

  syssgrad_utrikes_kvinna_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
    filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == max(varde)) %>%  .$varde)

  # Lägst sysselsättningsgrad

  syssgrad_utrikes_man_min_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
    filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == min(varde)) %>%  .$region

  syssgrad_utrikes_man_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
                                           filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == min(varde)) %>%  .$varde)

  syssgrad_utrikes_kvinna_min_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
    filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == min(varde)) %>%  .$region

  syssgrad_utrikes_kvinna_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
                                           filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == min(varde)) %>%  .$varde)

  # Variabler arbetslöshet kommun

  # Utrikes

  arblosthet_utrikes_man_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
    filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region

  arblosthet_utrikes_man_kommun_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
                                           filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde)

  arblosthet_utrikes_kvinna_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
    filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region

  arblosthet_utrikes_kvinna_kommun_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
                                           filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde)

  arblosthet_utrikes_man_min_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
    filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$region

  arblosthet_utrikes_man_kommun_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
                                             filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$varde)

  arblosthet_utrikes_kvinna_min_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
    filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$region

  arblosthet_utrikes_kvinna_kommun_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
                                                filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$varde)

  # Inrikes

  arblosthet_inrikes_man_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "inrikes född") %>%
    filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region

  if(length(arblosthet_inrikes_man_max_kommun)>1){
    arblosthet_inrikes_man_max_kommun <- paste(arblosthet_inrikes_man_max_kommun, collapse = " och ")
  }


  arblosthet_inrikes_man_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "inrikes född") %>%
                                             filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde)

  arblosthet_inrikes_kvinna_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född") %>%
    filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region

  if(length(arblosthet_inrikes_kvinna_max_kommun)>1){
    arblosthet_inrikes_kvinna_max_kommun <- paste(arblosthet_inrikes_kvinna_max_kommun, collapse = " och ")
  }

  arblosthet_inrikes_kvinna_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född") %>%
                                                filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde)

  ########################
  # Långtidsarbetslöshet #
  ########################

  # Används inte längre i rapporten
  #source(here("skript/","langtidsarbetsloshet_kon.R"))
  # source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_langtidsarbetsloshet_kolada_kon.R")
  # gg_langtidsarbetsloshet <- diagram_langtidsarbetslohet_tidsserie(output_mapp_figur = Output_mapp_figur,
  #                                                                 spara_figur = spara_diagram_som_bildfiler,
  #                                                                 returnera_data= TRUE)
  #
  # langtidsarbetsloshet_ar_min = långtidsarbetslöshet$ar %>% min()
  # langtidsarbetsloshet_ar_max = långtidsarbetslöshet$ar %>% max()
  # langtidsarbetsloshet_kvinnor_min = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="kvinnor",ar==min(ar)) %>%  .$varde,1))
  # langtidsarbetsloshet_kvinnor_max = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="kvinnor",ar==max(ar)) %>%  .$varde,1))
  # langtidsarbetsloshet_man_min = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="män",ar==min(ar)) %>%  .$varde,1))
  # langtidsarbetsloshet_man_max = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="män",ar==max(ar)) %>%  .$varde,1))

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

  # Utbildningsnivå (bakgrund och åldersgrupper) - hämtad från Figur 20 Kompetensförsörjningsrapporten
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_utb_bakgr_alder_NMS.R", encoding="UTF-8")
  gg_utbniva_bakgrund_alder <- diag_utb_niva_bakgr_alder(output_mapp_figur = Output_mapp_figur,
                                                         diag_utb_bakgrund = TRUE,
                                                         diag_utb_alder = TRUE,
                                                         skapa_fil = spara_diagram_som_bildfiler,
                                                         returnera_figur = TRUE,
                                                         returnera_data = TRUE)

  utb_niva_bakgrund_alder_ar <- unique(utb_niva_bakgrund$Ar)

  #Laddade in det här för att kunna köra raderna 396 och framåt i Rmd-filen
  #etablering_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/etableringstid.xlsx")
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_etableringstid_kon_lan_tidsserie_KvMa_IntRap.R")
  #source(here("skript/","etablering_kon_utbildningsniva.R"))
  gg_etablering <- diag_etablering_utb_kon_scb(output_mapp = Output_mapp_figur,
                                               utbildningsniva_jmf = c("utbildningsnivå: förgymnasial utbildning","utbildningsnivå: gymnasial utbildning" ,"utbildningsnivå: eftergymnasial utbildning"), # Finns även "samtliga utbildningsnivåer", "utbildningsnivå: förgymnasial utbildning", Skriv i den ordning de skall visas i diagram
                                               facet_kolumner = 2,# Välj antalet kolumner som skall visas i Facet-diagramet (diag_utbildning)
                                              skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                              returnera_data_rmarkdown = TRUE)

  eftergym_0_1_kvinna <- round(etablering_df %>% filter(kön == "kvinnor",bakgrundsvariabel == "0-1 år",år == max(år),utbildningsnivå == "utbildningsnivå: eftergymnasial utbildning") %>% .$andel,0)
  eftergym_0_1_man <- round(etablering_df %>% filter(kön == "män",bakgrundsvariabel == "0-1 år",år == max(år),utbildningsnivå == "utbildningsnivå: eftergymnasial utbildning") %>% .$andel,0)
  eftergym_10_kvinna <- round(etablering_df %>% filter(kön == "kvinnor",bakgrundsvariabel == "10- år",år == max(år),utbildningsnivå == "utbildningsnivå: eftergymnasial utbildning") %>% .$andel,0)
  samtliga_inrikes_kvinna <- round(etablering_df %>% filter(kön == "kvinnor",bakgrundsvariabel == "Inrikes född",år == max(år),utbildningsnivå == "samtliga utbildningsnivåer") %>% .$andel,0)
  gym_0_1_kvinna <- round(etablering_df %>% filter(kön == "kvinnor",bakgrundsvariabel == "0-1 år",år == max(år),utbildningsnivå == "utbildningsnivå: gymnasial utbildning") %>% .$andel,0)
  gym_0_1_man <-  round(etablering_df %>% filter(kön == "män",bakgrundsvariabel == "0-1 år",år == max(år),utbildningsnivå == "utbildningsnivå: gymnasial utbildning") %>% .$andel,0)
  forgym_0_1_kvinna <- round(etablering_df %>% filter(kön == "kvinnor",bakgrundsvariabel == "0-1 år",år == max(år),utbildningsnivå == "utbildningsnivå: förgymnasial utbildning") %>% .$andel,0)
  forgym_0_1_man <- round(etablering_df %>% filter(kön == "män",bakgrundsvariabel == "0-1 år",år == max(år),utbildningsnivå == "utbildningsnivå: förgymnasial utbildning") %>% .$andel,0)

  # # Utbildningsnivå från 85 och framåt uppdelat på kön. Data hämtas i detta fall från GGplot-objektet (när data används i markdown) FEL
  # source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_utbniva_flera_diagram_scb.R")
  # gg_utbniva_85 <- diag_utbniva_tidserie_och_lansjmfr(region_vekt = c("20"),
  #                                                     output_mapp = Output_mapp_figur,
  #                                                     diagram_capt = "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna",
  #                                                     skapa_fil = spara_diagram_som_bildfiler,
  #                                                     diag_hogutb_over_tid = TRUE,
  #                                                     diag_lagutb_over_tid = TRUE,
  #                                                     diag_andel_alla_utbnivaer = TRUE,
  #                                                     vald_utb_niva = "hogutb")

  #############################################
  ##### Förvärvsinkomst för inrikes/utrikes ####
  #############################################
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_forvarvsinkomst_bakgrund_vistelsetid_IntRap.R")
  gg_forvarvsinkomst_utrikes <- diag_inkomst_bakgrund_scb(output_mapp = Output_mapp_figur,
                                                          skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                          returnera_data_rmarkdown = TRUE)


  forvarvsinkomst_ar <- unique(forvarvsinkomst_df$år)
  forvarvsinkomst_inrikes_kvinnor <- gsub("\\.",",",forvarvsinkomst_df %>% filter(kön == "kvinnor",födelseregion == "födda i Sverige") %>%  .$`Medianinkomst, tkr`)
  forvarvsinkomst_inrikes_man <- gsub("\\.",",",forvarvsinkomst_df %>% filter(kön == "män",födelseregion == "födda i Sverige") %>%  .$`Medianinkomst, tkr`)

  #############################
  ##### Ekonomisk standard ####
  #############################
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_ek_standard_boende_valdeltagande_vistelsetid_IntRap.R")
  gg_ek_standard <- diagram_diverse_vistelsetid(diag_boendetyp = FALSE,
                                                diag_valdeltagande = FALSE,
                                                output_mapp = Output_mapp_figur,
                                                skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                returnera_dataframe_global_environment = TRUE)

  ek_standard_min_ar <- min(lag_ek_standard_bakgrund_df$år)
  ek_standard_max_ar <- max(lag_ek_standard_bakgrund_df$år)
  ek_standard_utrikes_andel_max_ar_samtliga <- round(lag_ek_standard_bakgrund_df %>% filter(år == ek_standard_max_ar,sysselsättning == "Samtliga personer",bakgrund == "utrikes födda") %>%  .$`Inkomst < 60 procent`,0)
  ek_standadard_forandring_utrikes <- abs(round(lag_ek_standard_bakgrund_df %>% filter(år == ek_standard_max_ar,sysselsättning == "Samtliga personer",bakgrund == "utrikes födda") %>%  .$`Inkomst < 60 procent`-lag_ek_standard_bakgrund_df %>% filter(år == ek_standard_min_ar,sysselsättning == "Samtliga personer",bakgrund == "utrikes födda") %>%  .$`Inkomst < 60 procent`,0))
  ek_standard_inrikes_andel_max_ar_samtliga <- round(lag_ek_standard_bakgrund_df %>% filter(år == ek_standard_max_ar,sysselsättning == "Samtliga personer",bakgrund == "inrikes födda") %>%  .$`Inkomst < 60 procent`,0)

  ######################################
  ##### Huvudsaklig förvärvskälla  #####
  ######################################
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_huv_ink_bakgrund_kon_IntRap.R")
  gg_huv_inkomst <- diag_huv_ink_kalla_bakgrund_scb (output_mapp = Output_mapp_figur,
                                                     diag_kon = FALSE,
                                                     skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                     returnera_data_rmarkdown  = TRUE)

  huv_inkomst_ar <- unique(huv_ink_df$år)
  huv_inkomst_manad <- unique(huv_ink_df$månad)

  saknar_ersattning_inrikes_andel <- gsub("\\.",",",round(huv_ink_df %>% filter(födelseregion == "inrikes född", kön == "totalt",`huvudsaklig inkomstkälla` == "Saknar ersättningar") %>% .$andel,0))
  saknar_ersattning_utrikes_andel <- gsub("\\.",",",round(huv_ink_df %>% filter(födelseregion == "utrikes född", kön == "totalt",`huvudsaklig inkomstkälla` == "Saknar ersättningar") %>% .$andel,0))

  ek_stod_inrikes_andel <- gsub("\\.",",",round(huv_ink_df %>% filter(födelseregion == "inrikes född", kön == "totalt",`huvudsaklig inkomstkälla` == "Ekonomiskt stöd") %>% .$andel,0))
  ek_stod_utrikes_andel <- gsub("\\.",",",round(huv_ink_df %>% filter(födelseregion == "utrikes född", kön == "totalt",`huvudsaklig inkomstkälla` == "Ekonomiskt stöd") %>% .$andel,0))

  ######################################
  #####       Ekonomiskt stöd      #####
  ######################################
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_ek_stod_bakgrund.R")
  gg_ek_stod <- diagram_ek_stod_bakgrund_SCB (output_mapp = Output_mapp_figur,
                                              diag_totalt = FALSE,
                                              skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                              returnera_data_rmarkdown = TRUE)

  ek_stod_manad_ar_forsta <- first(ekonomiskt_stod_df$månad_år)
  ek_stod_manad_ar_sista <- last(ekonomiskt_stod_df$månad_år)

  ek_stod_skillnad_forsta <- ekonomiskt_stod_df %>% filter(månad_år==first(månad_år)) %>% filter(födelseregion=="utrikes född") %>% .$antal - ekonomiskt_stod_df %>% filter(månad_år==first(månad_år)) %>% filter(födelseregion=="inrikes född") %>% .$antal
  ek_stod_skillnad_senaste <- ekonomiskt_stod_df %>% filter(månad_år==last(månad_år)) %>% filter(födelseregion=="utrikes född") %>% .$antal - ekonomiskt_stod_df %>% filter(månad_år==last(månad_år)) %>% filter(födelseregion=="inrikes född") %>% .$antal

  #############################################
  #### Utbildningsnivå för inrikes/utrikes ####
  #############################################
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_utbniva_inr_utr_fodda_lan_scb.R")
  gg_utbniva_bakgrund <- funktion_upprepa_forsok_om_fel( function() {
    diag_utbniva_inr_utr_fodda_kon_lan(skriv_diagramfil = spara_diagram_som_bildfiler,
                                       output_mapp = Output_mapp_figur,
                                       returnera_df_rmarkdown = TRUE)
  })

  utbniva_bakgr_ar <- utbniva_bakgr_kon_df$år %>% unique()

  #############################################
  ####      Behörighet gymnasiet och högskola          ####
  #############################################
  #source(here("skript/","gymnasiebehorighet_kon_vistelsetid.R"))
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_gym_hogskola_behorighet_vistelsetid_IntRap.R")
  ar <- as.character(seq(2005, 2021, by = 2))
  gg_gym_hogskola_behorighet <- diag_gymnasiebehorighet_mm (output_mapp = Output_mapp_figur,
                                                            diag_kon_gym= TRUE,
                                                            jmf_ar = ar,
                                                            diag_kon_hogskola = TRUE,
                                                            diag_vistelsetid_gym = TRUE,
                                                            skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                            returnera_data_rmarkdown = TRUE)


  # Gymnasiet, jämför år

  ar_min <-  min(ar)
  ar_max <- max(behorighet_gym_df$år)

  # Totalt
  inrikes_behorighet_gym_ar_min_bada <- gsub("\\.",",",round(behorighet_gym_df %>% filter(kön == "pojkar och flickor",variabel == "Inrikes född",år== ar_min) %>%  .$`Andel_behoriga`,0))
  inrikes_behorighet_gym_ar_max_bada <- gsub("\\.",",",round(behorighet_gym_df %>% filter(kön == "pojkar och flickor",variabel == "Inrikes född",år== ar_max) %>%  .$`Andel_behoriga`,0))

  utrikes_behorighet_gym_ar_max_pojkar <- gsub("\\.",",",round(behorighet_gym_df %>% filter(kön == "pojkar",variabel == "Utrikes född",år== ar_max) %>%  .$`Andel_behoriga`,0))
  utrikes_behorighet_gym_ar_2011_pojkar <- gsub("\\.",",",round(behorighet_gym_df %>% filter(kön == "pojkar",variabel == "Utrikes född",år== "2011") %>%  .$`Andel_behoriga`,0))
  utrikes_behorighet_gym_ar_max_flickor <- gsub("\\.",",",round(behorighet_gym_df %>% filter(kön == "flickor",variabel == "Utrikes född",år== ar_max) %>%  .$`Andel_behoriga`,0))

  # Uppdelat på vistelsetid
  # Skillnad mellan vistelsetider (för pojkar och flickor)
  utrikes_skillnad_10_jmf_4_9 <- round(behorighet_gym_df %>% filter(kön == "pojkar och flickor",variabel == "10- år",år== ar_max) %>%  .$`Andel_behoriga` - behorighet_gym_df %>% filter(kön == "pojkar och flickor",variabel == "4-9 år",år== ar_max) %>%  .$`Andel_behoriga`,0)
  utrikes_skillnad_4_9_jmf_2_3 <- round(behorighet_gym_df %>% filter(kön == "pojkar och flickor",variabel == "4-9 år",år== ar_max) %>%  .$`Andel_behoriga` - behorighet_gym_df %>% filter(kön == "pojkar och flickor",variabel == "2-3 år",år== ar_max) %>%  .$`Andel_behoriga`,0)
  utrikes_2_3_andel <- round(behorighet_gym_df %>% filter(kön == "pojkar och flickor",variabel == "2-3 år",år== ar_max) %>%  .$`Andel_behoriga`,0)

  # Högskola, jämför år
  inrikes_behorighet_hog_ar_max_man <- gsub("\\.",",",round(behorighet_hogskola_df %>% filter(kön == "män",variabel == "Inrikes född",år== ar_max) %>%  .$`Andel_behoriga`,0))
  utrikes_behorighet_hog_ar_max_man <- gsub("\\.",",",round(behorighet_hogskola_df %>% filter(kön == "män",variabel == "Utrikes född",år== ar_max) %>%  .$`Andel_behoriga`,0))
  inrikes_behorighet_hog_ar_max_kvinnor <- gsub("\\.",",",round(behorighet_hogskola_df %>% filter(kön == "kvinnor",variabel == "Inrikes född",år== ar_max) %>%  .$`Andel_behoriga`,0))
  utrikes_behorighet_hog_ar_max_kvinnor <- gsub("\\.",",",round(behorighet_hogskola_df %>% filter(kön == "kvinnor",variabel == "Utrikes född",år== ar_max) %>%  .$`Andel_behoriga`,0))

  utrikes_behorighet_hog_ar_2021_man <- gsub("\\.",",",round(behorighet_hogskola_df %>% filter(kön == "män",variabel == "Utrikes född",år== 2021) %>%  .$`Andel_behoriga`,0))


  # Skillnad mot 2013
  skillnad_2013_senaste_man <- round(behorighet_hogskola_df %>% filter(kön == "män",variabel == "Inrikes född",år== 2013) %>%  .$`Andel_behoriga` - behorighet_hogskola_df %>% filter(kön == "män",variabel == "Inrikes född",år== ar_max) %>%  .$`Andel_behoriga`,0)

  ############################
  ####      UVAS      ########
  ############################
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_uvas_vistelsetid_kon_tid_IntRap.R")
  gg_uvas <- diag_UVAS_bakgrund_vistelsetid  (output_mapp = Output_mapp_figur,
                                              skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                              returnera_data_rmarkdown = TRUE)

  uvas_forsta_ar <- min(UVAS_df$år)
  uvas_senaste_ar <- max(UVAS_df$år)

  # Ändrar så att sysselsättnings-variabeln enbart är 16-19 år eller 20-25 år
  UVAS_df <- UVAS_df %>%
    mutate(sysselsattning = str_extract(sysselsattning, "\\d{2}-\\d{2} år"))

  # Diagram kön (bara senaste år)
  uvas_16_19_kvinnor_0_1 <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "0-1 år",kön == "kvinnor",sysselsattning == "16-19 år" ) %>%  .$varde,0)
  uvas_16_19_kvinnor_2_3 <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "2-3 år",kön == "kvinnor",sysselsattning == "16-19 år" ) %>%  .$varde,0)
  uvas_16_19_kvinnor_4_9 <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "4-9 år",kön == "kvinnor",sysselsattning == "16-19 år" ) %>%  .$varde,0)
  uvas_16_19_kvinnor_10_ <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "10- år",kön == "kvinnor",sysselsattning == "16-19 år" ) %>%  .$varde,0)
  uvas_16_19_kvinnor_inrikes <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "Inrikes född",kön == "kvinnor",sysselsattning == "16-19 år" ) %>%  .$varde,0)

  uvas_20_25_kvinnor_0_1 <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "0-1 år",kön == "kvinnor",sysselsattning == "20-25 år" ) %>%  .$varde,0)
  uvas_20_25_kvinnor_2_3 <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "2-3 år",kön == "kvinnor",sysselsattning == "20-25 år" ) %>%  .$varde,0)
  uvas_20_25_kvinnor_4_9 <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "4-9 år",kön == "kvinnor",sysselsattning == "20-25 år" ) %>%  .$varde,0)
  uvas_20_25_kvinnor_10_ <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "10- år",kön == "kvinnor",sysselsattning == "20-25 år" ) %>%  .$varde,0)
  uvas_20_25_kvinnor_inrikes <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "Inrikes född",kön == "kvinnor",sysselsattning == "20-25 år" ) %>%  .$varde,0)

  uvas_16_19_man_0_1 <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "0-1 år",kön == "män",sysselsattning == "16-19 år" ) %>%  .$varde,0)
  uvas_16_19_man_2_3 <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "2-3 år",kön == "män",sysselsattning == "16-19 år" ) %>%  .$varde,0)
  uvas_16_19_man_4_9 <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "4-9 år",kön == "män",sysselsattning == "16-19 år" ) %>%  .$varde,0)
  uvas_16_19_man_10_ <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "10- år",kön == "män",sysselsattning == "16-19 år" ) %>%  .$varde,0)
  uvas_16_19_man_inrikes <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "Inrikes född",kön == "män",sysselsattning == "16-19 år" ) %>%  .$varde,0)

  uvas_20_25_man_0_1 <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "0-1 år",kön == "män",sysselsattning == "20-25 år" ) %>%  .$varde,0)
  uvas_20_25_man_2_3 <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "2-3 år",kön == "män",sysselsattning == "20-25 år" ) %>%  .$varde,0)
  uvas_20_25_man_4_9 <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "4-9 år",kön == "män",sysselsattning == "20-25 år" ) %>%  .$varde,0)
  uvas_20_25_man_10_ <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "10- år",kön == "män",sysselsattning == "20-25 år" ) %>%  .$varde,0)
  uvas_20_25_man_inrikes <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "Inrikes född",kön == "män",sysselsattning == "20-25 år" ) %>%  .$varde,0)


  # Diagram tidsserie
  uvas_16_19_forsta_ar_0_1 <- round(UVAS_df %>% filter(år == uvas_forsta_ar,variabel == "0-1 år",kön == "män och kvinnor",sysselsattning == "16-19 år" ) %>%  .$varde,0)
  uvas_20_25_forsta_ar_0_1 <- round(UVAS_df %>% filter(år == uvas_forsta_ar,variabel == "0-1 år",kön == "män och kvinnor",sysselsattning == "20-25 år" ) %>%  .$varde,0)

  uvas_16_19_senaste_ar_0_1 <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "0-1 år",kön == "män och kvinnor",sysselsattning == "16-19 år" ) %>%  .$varde,0)
  uvas_20_25_senaste_ar_0_1 <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "0-1 år",kön == "män och kvinnor",sysselsattning == "20-25 år" ) %>%  .$varde,0)

  uvas_16_19_forsta_ar_4_9 <- round(UVAS_df %>% filter(år == uvas_forsta_ar,variabel == "4-9 år",kön == "män och kvinnor",sysselsattning == "16-19 år" ) %>%  .$varde,0)
  uvas_20_25_forsta_ar_4_9 <- round(UVAS_df %>% filter(år == uvas_forsta_ar,variabel == "4-9 år",kön == "män och kvinnor",sysselsattning == "20-25 år" ) %>%  .$varde,0)

  uvas_16_19_senaste_ar_4_9 <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "4-9 år",kön == "män och kvinnor",sysselsattning == "16-19 år" ) %>%  .$varde,0)
  uvas_20_25_senaste_ar_4_9 <- round(UVAS_df %>% filter(år == uvas_senaste_ar,variabel == "4-9 år",kön == "män och kvinnor",sysselsattning == "20-25 år" ) %>%  .$varde,0)


  ###########################################
  #######      Inrikes flyttar       ########
  ###########################################

  # Inrikes flyttningar, generellt
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_flytt_inrikes_utrikes_netto_SCB.R", encoding="UTF-8")
  gg_flytt <- diagram_inr_utr_flytt(region_vekt = "20",
                                    spara_figur = spara_diagram_som_bildfiler,
                                    diag_flyttnetto = FALSE,
                                    diag_uppdelat = TRUE,
                                    #tid = c(2000:9999),
                                    returnera_data = TRUE,
                                    output_mapp_figur = Output_mapp_figur)

  flytt_ar <- max(flytt_df$år)
  utrikes_overskott_senaste <- flytt_df %>% filter(region=="Dalarnas län",variabel == "Invandringsöverskott",år==max(år)) %>% .$varde %>% sum()


  # Inrikes flyttningar, bakgrund
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_flytt_inrikes_bakgrund_SCB.R", encoding="UTF-8")
  gg_flytt_bakgrund <- diag_inr_flyttnetto_inr_utr_fodda(region_vekt = "20",
                                                         skriv_diagram = spara_diagram_som_bildfiler,
                                                         output_mapp =  Output_mapp_figur,
                                                         returnera_data = TRUE,
                                                         fixa_y_axel_varden_jamna_tal = FALSE,
                                                         farg_vekt = diagramfarger("rus_sex")[2:1])
  flytt_senaste_ar <- max(flytt_bakgrund_df$år)
  flytt_senaste_inrikes <- flytt_bakgrund_df %>% filter(region == "Dalarna",år == max(år),födelseregion == "Född i Sverige") %>% .$Inrikes_flyttnetto
  flytt_senaste_utrikes <- abs(flytt_bakgrund_df %>% filter(region == "Dalarna",år == max(år),födelseregion == "Utrikes född") %>% .$Inrikes_flyttnetto)

  # Utflytt sedan 2017. Notera att det är i absoluta tal
  utflytt_utrikes_sedan_2017 <- format(abs(sum(flytt_bakgrund_df %>% filter(region == "Dalarna",år>2016,födelseregion == "Utrikes född") %>% .$Inrikes_flyttnetto)),big.mark = " ")
  flytt_inrikes_sedan_2017 <- format(sum(flytt_bakgrund_df %>% filter(region == "Dalarna",år>2016,födelseregion == "Född i Sverige") %>% .$Inrikes_flyttnetto),big.mark = " ")




  ############################################
  #### Boende per upplåtelseform      ########
  ############################################
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_ek_standard_boende_valdeltagande_vistelsetid_IntRap.R")
  #source(here("skript/","boendetyp_vistelsetid.R"))
  gg_boendetyp_upplatelseform <- diagram_diverse_vistelsetid (output_mapp = Output_mapp_figur,
                                                              diag_ek_standard = FALSE,
                                                              diag_valdeltagande = FALSE,
                                                              skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                              returnera_dataframe_global_environment = TRUE)

  boendetyp_ar <- max(boendtyp_df$år)
  boendetyp_inrikes_äga <- gsub("\\.",",",round(sum(boendtyp_df %>% filter(variabel == "Inrikes född",bakgrund %in% c("Bostadsrätt","Äganderätt"))%>%  .$varde),0))
  boendetyp_10_ar_äga <- gsub("\\.",",",round(sum(boendtyp_df %>% filter(variabel == "10- år",bakgrund %in% c("Bostadsrätt","Äganderätt"))%>%  .$varde),0))

  ############################################
  #######        Trångboddhet         ########
  ############################################
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_trangboddhet_inrikes_utrikes_IntRap.R")
  gg_trangboddhet <- diag_trangboddhet_inrikes_utrikes(output_mapp = Output_mapp_figur,
                                                       skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                       diag_antal = TRUE, # Antal
                                                       diag_andel = FALSE, # Andel, summerar till 100 procent
                                                       returnera_data_rmarkdown = TRUE)

  trangboddhet_min_ar <- min(trangboddhet_df$år)
  trangboddhet_max_ar <- max(trangboddhet_df$år)

  trangboddhet_forsta_ar_inrikes_antal <- format(trangboddhet_df %>% filter(år == trangboddhet_min_ar,födelseregion == "Inrikes född",trångboddhet == "Trångbodda") %>%  .$varde,big.mark = " ")
  trangboddhet_sista_ar_inrikes_antal <- format(trangboddhet_df %>% filter(år == trangboddhet_max_ar,födelseregion == "Inrikes född",trångboddhet == "Trångbodda") %>%  .$varde,big.mark = " ")

  trangboddhet_forsta_ar_inrikes_andel <- round(trangboddhet_df %>% filter(år == trangboddhet_min_ar,födelseregion == "Inrikes född",trångboddhet == "Trångbodda") %>%  .$andel,0)
  trangboddhet_sista_ar_inrikes_andel <- round(trangboddhet_df %>% filter(år == trangboddhet_max_ar,födelseregion == "Inrikes född",trångboddhet == "Trångbodda") %>%  .$andel,0)

  trangboddhet_forsta_ar_utrikes_antal <- format(trangboddhet_df %>% filter(år == trangboddhet_min_ar,födelseregion == "Utrikes född",trångboddhet == "Trångbodda") %>%  .$varde,big.mark = " ")
  trangboddhet_sista_ar_utrikes_antal <- format(trangboddhet_df %>% filter(år == trangboddhet_max_ar,födelseregion == "Utrikes född",trångboddhet == "Trångbodda") %>%  .$varde,big.mark = " ")

  trangboddhet_forsta_ar_utrikes_andel <- round(trangboddhet_df %>% filter(år == trangboddhet_min_ar,födelseregion == "Utrikes född",trångboddhet == "Trångbodda") %>%  .$andel,0)
  trangboddhet_sista_ar_utrikes_andel <- round(trangboddhet_df %>% filter(år == trangboddhet_max_ar,födelseregion == "Utrikes född",trångboddhet == "Trångbodda") %>%  .$andel,0)

  #############################################################
  #### Sociala relationer mm Folkhälsomyndigheten      ########
  #############################################################
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_sociala_rel_sjalvskattad_halsa_inrikes_utrikes_int_rap.R")
  gg_fohm <- diag_fohm (output_mapp = Output_mapp_figur,
                        skriv_diagrambildfil = spara_diagram_som_bildfiler,
                        returnera_data_rmarkdown = TRUE)

  sjalvskattad_forsta_ar <- min(sjalvskattad_halsa_tid_df$År)
  sjalvskattad_senaste_ar <- max(sjalvskattad_halsa_tid_df$År)

  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_sociala_rel_sjalvskattad_halsa_inrikes_utrikes_int_rap.R")
  gg_fohm_alt_tillit <- diag_fohm (output_mapp = Output_mapp_figur,
                                   diag_sjalvskattad_halsa_tid = FALSE,
                                   diag_sjalvskattad_halsa_kon = FALSE,
                                   sociala_relationer_klartext = "Svårt att lita på andra",
                                   skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                   returnera_data_rmarkdown = TRUE)

  # Tillit till andra
  tillit_andra_ar <- unique(svart_att_lita_pa_andra_df$År)
  tillit_andra_min_kvinnor_grupp <- svart_att_lita_pa_andra_df %>% filter(Kön == "Kvinnor",År == min(År)) %>% filter(`Sociala relationer efter födelseland, kön och år` == min(`Sociala relationer efter födelseland, kön och år`)) %>%  .$Födelseland
  tillit_andra_min_kvinnor_grupp <- paste0(tolower(substr(tillit_andra_min_kvinnor_grupp, 1, 1)), substr(tillit_andra_min_kvinnor_grupp, 2, nchar(tillit_andra_min_kvinnor_grupp)))
  tillit_andra_min_kvinnor_varde <- round(svart_att_lita_pa_andra_df %>% filter(Kön == "Kvinnor",År == min(År)) %>%  .$`Sociala relationer efter födelseland, kön och år` %>% min(),0)

  tillit_andra_min_man_grupp <- svart_att_lita_pa_andra_df %>% filter(Kön == "Män",År == min(År)) %>% filter(`Sociala relationer efter födelseland, kön och år` == min(`Sociala relationer efter födelseland, kön och år`)) %>%  .$Födelseland
  tillit_andra_min_man_grupp <- paste0(tolower(substr(tillit_andra_min_man_grupp, 1, 1)), substr(tillit_andra_min_man_grupp, 2, nchar(tillit_andra_min_man_grupp)))
  tillit_andra_min_man_varde <- round(svart_att_lita_pa_andra_df %>% filter(Kön == "Män",År == min(År)) %>%  .$`Sociala relationer efter födelseland, kön och år` %>% min(),0)

  tillit_andra_max_kvinnor_grupp <- svart_att_lita_pa_andra_df %>% filter(Kön == "Kvinnor",År == max(År)) %>% filter(`Sociala relationer efter födelseland, kön och år` == max(`Sociala relationer efter födelseland, kön och år`)) %>%  .$Födelseland
  tillit_andra_max_kvinnor_grupp <- paste0(tolower(substr(tillit_andra_max_kvinnor_grupp, 1, 1)), substr(tillit_andra_max_kvinnor_grupp, 2, nchar(tillit_andra_max_kvinnor_grupp)))
  tillit_andra_max_kvinnor_varde <- round(svart_att_lita_pa_andra_df %>% filter(Kön == "Kvinnor",År == max(År)) %>%  .$`Sociala relationer efter födelseland, kön och år` %>% max(),0)

  tillit_andra_max_man_grupp <- svart_att_lita_pa_andra_df %>% filter(Kön == "Män",År == max(År)) %>% filter(`Sociala relationer efter födelseland, kön och år` == max(`Sociala relationer efter födelseland, kön och år`)) %>%  .$Födelseland
  tillit_andra_max_man_grupp <- paste0(tolower(substr(tillit_andra_max_man_grupp, 1, 1)), substr(tillit_andra_max_man_grupp, 2, nchar(tillit_andra_max_man_grupp)))
  tillit_andra_max_man_varde <- round(svart_att_lita_pa_andra_df %>% filter(Kön == "Män",År == max(År)) %>%  .$`Sociala relationer efter födelseland, kön och år` %>% max(),0)

  ############################################
  #### Valdeltagande vistelsetid      ########
  ############################################
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_ek_standard_boende_valdeltagande_vistelsetid_IntRap.R")
  #source(here("skript/","valdeltagande_vistelsetid.R"))
  gg_valdeltagande_vistelsetid <- diagram_diverse_vistelsetid(output_mapp = Output_mapp_figur,
                                                              diag_ek_standard = FALSE,
                                                              diag_boendetyp = FALSE,
                                                              typ_av_val = c("Valdeltagande i val till riksdag, procent","Valdeltagande i val till region, procent", "Valdeltagande i val till kommun, procent"),
                                                              skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                              returnera_dataframe_global_environment  = TRUE)

  forsta_ar_valdeltagande <- min(valdeltagande_df$år)
  sista_ar_valdeltagande <- max(valdeltagande_df$år)

  ##### Riksdag
  valdeltagande_riksdag_inrikes_senaste_varde <- round(valdeltagande_df %>% filter(variabel == "Inrikes född",kön == "män och kvinnor") %>% filter(år == sista_ar_valdeltagande) %>% .$`Valdeltagande i val till riksdag, procent`,0)
  valdeltagande_riksdag_utrikes_senaste_varde <- round(valdeltagande_df %>% filter(variabel == "samtliga utrikes födda",kön == "män och kvinnor") %>% filter(år == sista_ar_valdeltagande) %>% .$`Valdeltagande i val till riksdag, procent`,0)

  valdeltagande_riksdag_under_10_senaste_varde <- round(valdeltagande_df %>% filter(variabel == "< 10 år",kön == "män och kvinnor") %>% filter(år == sista_ar_valdeltagande) %>% .$`Valdeltagande i val till riksdag, procent`,0)
  valdeltagande_riksdag_over_10_senaste_varde <- round(valdeltagande_df %>% filter(variabel == "10- år",kön == "män och kvinnor") %>% filter(år == sista_ar_valdeltagande) %>% .$`Valdeltagande i val till riksdag, procent`,0)

  valdeltagande_riksdag_inrikes_forsta_varde <- round(valdeltagande_df %>% filter(variabel == "Inrikes född",kön == "män och kvinnor") %>% filter(år == forsta_ar_valdeltagande) %>% .$`Valdeltagande i val till riksdag, procent`,0)
  valdeltagande_riksdag_utrikes_forsta_varde <- round(valdeltagande_df %>% filter(variabel == "samtliga utrikes födda",kön == "män och kvinnor") %>% filter(år == forsta_ar_valdeltagande) %>% .$`Valdeltagande i val till riksdag, procent`,0)

  valdeltagande_riksdag_under_10_forsta_varde <- round(valdeltagande_df %>% filter(variabel == "< 10 år",kön == "män och kvinnor") %>% filter(år == forsta_ar_valdeltagande) %>% .$`Valdeltagande i val till riksdag, procent`,0)
  valdeltagande_riksdag_over_10_forsta_varde <- round(valdeltagande_df %>% filter(variabel == "10- år",kön == "män och kvinnor") %>% filter(år == forsta_ar_valdeltagande) %>% .$`Valdeltagande i val till riksdag, procent`,0)

  # Region/kommun - typ samma värden så gör ingen skillnad
  # valdeltagande_region_senaste_ar <- max(valdeltagande_df %>% filter(val == "Valdeltagande i val till region") %>% .$år)
  # valdeltagande_region_forsta_ar <- min(valdeltagande_df %>% filter(val == "Valdeltagande i val till region") %>% .$år)

  valdeltagande_region_inrikes_senaste_varde <- round(valdeltagande_df %>% filter(variabel == "Inrikes född",kön == "män och kvinnor") %>% filter(år == sista_ar_valdeltagande) %>% .$`Valdeltagande i val till region, procent`,0)
  valdeltagande_region_utrikes_senaste_varde <- round(valdeltagande_df %>% filter(variabel == "samtliga utrikes födda",kön == "män och kvinnor") %>% filter(år == sista_ar_valdeltagande) %>% .$`Valdeltagande i val till region, procent`,0)

  valdeltagande_region_under_10_senaste_varde <- round(valdeltagande_df %>% filter(variabel == "< 10 år",kön == "män och kvinnor") %>% filter(år == sista_ar_valdeltagande) %>% .$`Valdeltagande i val till region, procent`,0)
  valdeltagande_region_over_10_senaste_varde <- round(valdeltagande_df %>% filter(variabel == "10- år",kön == "män och kvinnor") %>% filter(år == sista_ar_valdeltagande) %>% .$`Valdeltagande i val till region, procent`,0)

  valdeltagande_region_inrikes_forsta_varde <- round(valdeltagande_df %>% filter(variabel == "Inrikes född",kön == "män och kvinnor") %>% filter(år == forsta_ar_valdeltagande) %>% .$`Valdeltagande i val till region, procent`,0)
  valdeltagande_region_utrikes_forsta_varde <- round(valdeltagande_df %>% filter(variabel == "samtliga utrikes födda",kön == "män och kvinnor") %>% filter(år == forsta_ar_valdeltagande) %>% .$`Valdeltagande i val till region, procent`,0)

  valdeltagande_region_under_10_forsta_varde <- round(valdeltagande_df %>% filter(variabel == "< 10 år",kön == "män och kvinnor") %>% filter(år == forsta_ar_valdeltagande) %>% .$`Valdeltagande i val till region, procent`,0)
  valdeltagande_region_over_10_forsta_varde <- round(valdeltagande_df %>% filter(variabel == "10- år",kön == "män och kvinnor") %>% filter(år == forsta_ar_valdeltagande) %>% .$`Valdeltagande i val till region, procent`,0)

  valdeltagande_skillnad_under10 <- round(valdeltagande_df %>% filter(variabel == "< 10 år",kön == "män och kvinnor") %>% filter(år == sista_ar_valdeltagande) %>% .$`Valdeltagande i val till riksdag, procent`-valdeltagande_df %>% filter(variabel == "< 10 år",kön == "män och kvinnor") %>% filter(år == sista_ar_valdeltagande) %>% .$`Valdeltagande i val till region, procent`,0)
  valdeltagande_skillnad_over10 <- round(valdeltagande_df %>% filter(variabel == "10- år",kön == "män och kvinnor") %>% filter(år == sista_ar_valdeltagande) %>% .$`Valdeltagande i val till riksdag, procent`-valdeltagande_df %>% filter(variabel == "10- år",kön == "män och kvinnor") %>% filter(år == sista_ar_valdeltagande) %>% .$`Valdeltagande i val till region, procent`,0)

  # Tidigare, innan det blev fel. Låt vara kvar ifall det ändras igen
  ##### Riksdag
  # valdeltagande_riksdag_senaste_ar <- max(valdeltagande_df %>% filter(val == "Valdeltagande i val till riksdag") %>% .$år)
  # valdeltagande_riksdag_forsta_ar <- min(valdeltagande_df %>% filter(val == "Valdeltagande i val till riksdag") %>% .$år)
  #
  # valdeltagande_riksdag_inrikes_senaste_varde <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till riksdag",variabel == "Inrikes född",kön == "män och kvinnor") %>% filter(år == valdeltagande_riksdag_senaste_ar) %>% .$varde,0)
  # valdeltagande_riksdag_utrikes_senaste_varde <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till riksdag",variabel == "samtliga utrikes födda",kön == "män och kvinnor") %>% filter(år == valdeltagande_riksdag_senaste_ar) %>% .$varde,0)
  #
  # valdeltagande_riksdag_under_10_senaste_varde <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till riksdag",variabel == "< 10 år",kön == "män och kvinnor") %>% filter(år == valdeltagande_riksdag_senaste_ar) %>% .$varde,0)
  # valdeltagande_riksdag_over_10_senaste_varde <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till riksdag",variabel == "10- år",kön == "män och kvinnor") %>% filter(år == valdeltagande_riksdag_senaste_ar) %>% .$varde,0)
  #
  # valdeltagande_riksdag_inrikes_forsta_varde <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till riksdag",variabel == "Inrikes född",kön == "män och kvinnor") %>% filter(år == valdeltagande_riksdag_forsta_ar) %>% .$varde,0)
  # valdeltagande_riksdag_utrikes_forsta_varde <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till riksdag",variabel == "samtliga utrikes födda",kön == "män och kvinnor") %>% filter(år == valdeltagande_riksdag_forsta_ar) %>% .$varde,0)
  #
  # valdeltagande_riksdag_under_10_forsta_varde <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till riksdag",variabel == "< 10 år",kön == "män och kvinnor") %>% filter(år == valdeltagande_riksdag_forsta_ar) %>% .$varde,0)
  # valdeltagande_riksdag_over_10_forsta_varde <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till riksdag",variabel == "10- år",kön == "män och kvinnor") %>% filter(år == valdeltagande_riksdag_forsta_ar) %>% .$varde,0)
  #
  # # Region/kommun - typ samma värden så gör ingen skillnad
  # valdeltagande_region_senaste_ar <- max(valdeltagande_df %>% filter(val == "Valdeltagande i val till region") %>% .$år)
  # valdeltagande_region_forsta_ar <- min(valdeltagande_df %>% filter(val == "Valdeltagande i val till region") %>% .$år)
  #
  # valdeltagande_region_inrikes_senaste_varde <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till region",variabel == "Inrikes född",kön == "män och kvinnor") %>% filter(år == valdeltagande_region_senaste_ar) %>% .$varde,0)
  # valdeltagande_region_utrikes_senaste_varde <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till region",variabel == "samtliga utrikes födda",kön == "män och kvinnor") %>% filter(år == valdeltagande_region_senaste_ar) %>% .$varde,0)
  #
  # valdeltagande_region_under_10_senaste_varde <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till region",variabel == "< 10 år",kön == "män och kvinnor") %>% filter(år == valdeltagande_region_senaste_ar) %>% .$varde,0)
  # valdeltagande_region_over_10_senaste_varde <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till region",variabel == "10- år",kön == "män och kvinnor") %>% filter(år == valdeltagande_region_senaste_ar) %>% .$varde,0)
  #
  # valdeltagande_region_inrikes_forsta_varde <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till region",variabel == "Inrikes född",kön == "män och kvinnor") %>% filter(år == valdeltagande_region_forsta_ar) %>% .$varde,0)
  # valdeltagande_region_utrikes_forsta_varde <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till region",variabel == "samtliga utrikes födda",kön == "män och kvinnor") %>% filter(år == valdeltagande_region_forsta_ar) %>% .$varde,0)
  #
  # valdeltagande_region_under_10_forsta_varde <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till region",variabel == "< 10 år",kön == "män och kvinnor") %>% filter(år == valdeltagande_region_forsta_ar) %>% .$varde,0)
  # valdeltagande_region_over_10_forsta_varde <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till region",variabel == "10- år",kön == "män och kvinnor") %>% filter(år == valdeltagande_region_forsta_ar) %>% .$varde,0)
  #
  # valdeltagande_skillnad_under10 <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till riksdag",variabel == "< 10 år",kön == "män och kvinnor") %>% filter(år == valdeltagande_riksdag_senaste_ar) %>% .$varde-valdeltagande_df %>% filter(val == "Valdeltagande i val till region",variabel == "< 10 år",kön == "män och kvinnor") %>% filter(år == valdeltagande_region_senaste_ar) %>% .$varde,0)
  # valdeltagande_skillnad_over10 <- round(valdeltagande_df %>% filter(val == "Valdeltagande i val till riksdag",variabel == "10- år",kön == "män och kvinnor") %>% filter(år == valdeltagande_riksdag_senaste_ar) %>% .$varde-valdeltagande_df %>% filter(val == "Valdeltagande i val till region",variabel == "10- år",kön == "män och kvinnor") %>% filter(år == valdeltagande_region_senaste_ar) %>% .$varde,0)
  #
  save.image(file = "G:/skript/projekt/environments/integrationen_i_Dalarna.RData")

  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  cat(sprintf("Hämtning av data klar: Det tog %.2f sekunder.", elapsed_time))
  cat("\n\n")


}else{
  load("G:/skript/projekt/environments/integrationen_i_Dalarna.RData")
}
