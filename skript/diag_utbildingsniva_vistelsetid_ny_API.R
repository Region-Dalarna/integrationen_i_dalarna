diag_utbniva_vistelsetid_lan <- function(
    region_vekt = "20",                       # Val av region, t.ex. "20" = Dalarna, "00" = riket. Kommuner finns också.
    gruppera_namn = NA,                       # NA = gör ingenting, annars namn på gruppering som regionerna slås ihop till
    tid_koder = "9999",                       # "*" = alla år, "9999" = senaste år, finns: "2024", "2025"
    diagram_capt = "SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    output_mapp = NA,
    returnera_df_rmarkdown = FALSE,
    visa_dataetiketter = FALSE,
    skriv_diagramfil = TRUE,
    diag_fargvekt = NA
) {

  # ======================================================================================================
  #
  # Diagram som visar utbildningsnivå efter vistelsetid i Sverige (utrikes födda 0–3, 4–9, 10+ år samt
  # inrikes födda), kvinnor och män totalt. Fyra utbildningsnivåer inkl. uppgift saknas.
  # Ursprungligen skapat 17 okt 2024 av Peter (inrikes/utrikes födda, TAB4648 + TAB4655).
  #
  # Bytt till TAB6833 (antal) - andelar beräknas med "totalt, samtliga utbildningsnivåer" som nämnare.
  # Bakgrundsvariabel bytt från inrikes/utrikes född till vistelsetid, ingen uppdelning på kön. / Claude 2026-09-23
  # ======================================================================================================

  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")

  gg_list <- list()
  tabell_id <- "TAB6833"

  if (all(is.na(output_mapp))) {
    if (dir.exists(rdverktyg::utskriftsmapp())) {
      output_mapp <- rdverktyg::utskriftsmapp()
    } else {
      skriv_diagramfil <- FALSE
    }
  }

  if (all(is.na(diag_fargvekt))) diag_fargvekt <- c(rddiagram::diagramfarger("rus_sex")[1:3], "#A6A6A6")

  # =============================================== Välj värden ===============================================

  utb_niva_txt <- c("totalt, samtliga utbildningsnivåer",
                    "förgymnasial utbildning", "gymnasial utbildning", "eftergymnasial utbildning",
                    "uppgift om utbildningsnivå saknas")

  # vistelsetid hämtas via mönster på klartext (en dash i "0–3" m.m.)
  bakgr_meta <- pxweb2r::pxweb2_get_values(tabell_id, "BakgrVar", quiet = TRUE)
  inrikes_rad <- bakgr_meta[stringr::str_detect(bakgr_meta$label, "^inrikes född"), ]
  if (nrow(inrikes_rad) > 1) inrikes_rad <- inrikes_rad[stringr::str_detect(inrikes_rad$label, "oavsett"), ]
  vistelse_rad <- bakgr_meta[stringr::str_detect(bakgr_meta$label, "år i Sverige, utrikes född"), ]

  bakgr_valda <- dplyr::bind_rows(vistelse_rad, inrikes_rad)
  if (nrow(bakgr_valda) != 4) {
    print(bakgr_meta, n = Inf)
    stop("Förväntade 4 bakgrundsvärden (3 vistelsetider + inrikes född), hittade ", nrow(bakgr_valda), ". Se listan ovan.")
  }

  giltiga_ar <- pxweb2r::pxweb2_get_values(tabell_id, "Tid", quiet = TRUE)$code
  tid_vekt <- if (identical(tid_koder, "9999")) max(giltiga_ar) else if (identical(tid_koder, "*")) giltiga_ar else as.character(tid_koder)[as.character(tid_koder) %in% giltiga_ar]
  if (length(tid_vekt) == 0) stop("Inga giltiga år valda. Finns: ", paste(giltiga_ar, collapse = ", "))

  # =============================================== API-uttag ===============================================

  px_df <- pxweb2r::pxweb2_get_data(
    table = tabell_id,
    query = list(
      Region = region_vekt,
      UtbildningsNiva = utb_niva_txt,
      Kon = "totalt, kvinnor och män",
      BakgrVar = bakgr_valda$code,
      ContentsCode = "000008LZ",
      Tid = tid_vekt
    ),
    quiet = TRUE)

  hitta_kol <- function(df, varden) {
    kol <- names(df)[purrr::map_lgl(df, ~ is.character(.x) && any(.x %in% varden))]
    if (length(kol) == 0) stop("Hittade ingen kolumn med värdena: ", paste(varden, collapse = ", "))
    kol[1]
  }

  integration_df <- px_df |>
    dplyr::rename(regionkod = region_kod,
                  utbildning = !!hitta_kol(px_df, utb_niva_txt),
                  vistelsetid = !!hitta_kol(px_df, bakgr_valda$label),
                  antal = value) |>
    dplyr::select(dplyr::any_of(c("regionkod", "region", "år", "vistelsetid", "utbildning", "antal")))

  # gruppering av flera regioner - summeras som antal innan andelar beräknas
  if (!is.na(gruppera_namn)) {
    integration_df <- integration_df |>
      dplyr::group_by(år, vistelsetid, utbildning) |>
      dplyr::summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(regionkod = "gg", region = gruppera_namn) |>
      dplyr::relocate(regionkod, region, .before = 1)

    region_vekt <- "gg"
  }

  # andelar med totalt (inkl. uppgift saknas) som nämnare, snygga etiketter
  integration_df <- integration_df |>
    dplyr::group_by(regionkod, region, år, vistelsetid) |>
    dplyr::mutate(varde = antal / antal[utbildning == "totalt, samtliga utbildningsnivåer"] * 100) |>
    dplyr::ungroup() |>
    dplyr::filter(utbildning != "totalt, samtliga utbildningsnivåer") |>
    dplyr::mutate(
      utbildning = dplyr::recode(utbildning, "uppgift om utbildningsnivå saknas" = "uppgift saknas"),
      utbildning = factor(utbildning, levels = c("förgymnasial utbildning", "gymnasial utbildning",
                                                 "eftergymnasial utbildning", "uppgift saknas")),
      vistelsetid = dplyr::if_else(stringr::str_detect(vistelsetid, "^inrikes"),
                                   "Inrikes födda",
                                   stringr::str_extract(vistelsetid, "^.*?år"))   # "0–3 år", "4–9 år", "10+ år"
    )

  # ordning på x-axeln: vistelsetider i stigande ordning, inrikes födda sist
  vistelse_niv <- unique(integration_df$vistelsetid[integration_df$vistelsetid != "Inrikes födda"])
  vistelse_niv <- vistelse_niv[order(as.numeric(stringr::str_extract(vistelse_niv, "^\\d+")))]
  integration_df <- dplyr::mutate(integration_df, vistelsetid = factor(vistelsetid, levels = c(vistelse_niv, "Inrikes födda")))

  if (returnera_df_rmarkdown == TRUE) {
    assign("utbniva_vistelsetid_df", integration_df, envir = .GlobalEnv)
  }

  skapa_diagram <- function(chart_df, skickad_regionkod) {

    chart_df <- dplyr::filter(chart_df, regionkod %in% skickad_regionkod)

    region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(chart_df$region)))
    region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(chart_df$regionkod), returnera_text = TRUE, returtext = region_start)
    region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(chart_df$regionkod), returnera_text = TRUE, returtext = region_txt)
    regionfil_txt <- region_txt
    region_txt <- paste0(" i ", region_txt)

    ar_txt <- if (min(chart_df$år) == max(chart_df$år)) max(chart_df$år) else paste0(min(chart_df$år), "-", max(chart_df$år))

    diagramtitel <- glue::glue("Utbildningsnivå (20-74 år){region_txt} år {ar_txt} efter vistelsetid")
    diagramfil <- glue::glue("utbniva_vistelsetid_{regionfil_txt}_ar{ar_txt}.png")

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = chart_df,
                                            skickad_x_var = "vistelsetid",
                                            skickad_y_var = "varde",
                                            skickad_x_grupp = "utbildning",
                                            diagram_titel = diagramtitel,
                                            diagram_capt = diagram_capt,
                                            stodlinjer_avrunda_fem = TRUE,
                                            filnamn_diagram = diagramfil,
                                            dataetiketter = visa_dataetiketter,
                                            manual_y_axis_title = "procent",
                                            x_axis_lutning = 0,
                                            manual_color = diag_fargvekt,
                                            output_mapp = output_mapp,
                                            skriv_till_diagramfil = skriv_diagramfil
    )

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")
    return(gg_list)
  }

  if (length(region_vekt) > 1) {
    retur_list <- purrr::flatten(purrr::map(unique(region_vekt), ~ skapa_diagram(chart_df = integration_df, skickad_regionkod = .x)))
  } else {
    retur_list <- skapa_diagram(chart_df = integration_df, skickad_regionkod = region_vekt)
  }
  return(retur_list)
}
