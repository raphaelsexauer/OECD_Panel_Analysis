# ============================================================
# OECD-PANELANALYSE 2015-2023
#
# HAUPTANALYSE
# Hospital beds je 1.000 Einwohner
#        -> reale Gesundheitsausgaben pro Kopf
#        -> Gesundheitsausgaben in % des BIP
#
# Kontrollen:
#   - reales BIP pro Kopf, konstante PPP
#   - Anteil Bevölkerung >=65 Jahre
#
# Modell:
#   Länder-FE + Jahres-FE
#   Standardfehler auf Länderebene geclustert
# Analysen mit R Studio durchgeführt (Stand 3.11.26). 
# Exceldateien müssen vor Nutzung des Skrips importiert werden
# ============================================================

# ============================================================
# 1. Pakete
# ============================================================
library(dplyr)
library(tidyr)
library(fixest)
# ============================================================
# 2. Einstellungen
# ============================================================
START_YEAR <- 2015
END_YEAR   <- 2023
MIN_YEARS  <- 7
OECD_COUNTRIES <- c(
  "AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE",
  "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL",
  "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX",
  "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN",
  "ESP", "SWE", "CHE", "TUR", "GBR", "USA"
)
YEARS <- as.character(START_YEAR:END_YEAR)
# ============================================================
# 3. Ländernamen in OECD-Codes
# ============================================================
country_map <- c(
  "Australia" = "AUS",
  "Austria" = "AUT",
  "Belgium" = "BEL",
  "Canada" = "CAN",
  "Chile" = "CHL",
  "Colombia" = "COL",
  "Costa Rica" = "CRI",
  "Czechia" = "CZE",
  "Denmark" = "DNK",
  "Estonia" = "EST",
  "Finland" = "FIN",
  "France" = "FRA",
  "Germany" = "DEU",
  "Greece" = "GRC",
  "Hungary" = "HUN",
  "Iceland" = "ISL",
  "Ireland" = "IRL",
  "Israel" = "ISR",
  "Italy" = "ITA",
  "Japan" = "JPN",
  "Korea" = "KOR",
  "Latvia" = "LVA",
  "Lithuania" = "LTU",
  "Luxembourg" = "LUX",
  "Mexico" = "MEX",
  "Netherlands" = "NLD",
  "New Zealand" = "NZL",
  "Norway" = "NOR",
  "Poland" = "POL",
  "Portugal" = "PRT",
  "Slovak Republic" = "SVK",
  "Slovenia" = "SVN",
  "Spain" = "ESP",
  "Sweden" = "SWE",
  "Switzerland" = "CHE",
  "Türkiye" = "TUR",
  "Turkey" = "TUR",
  "United Kingdom" = "GBR",
  "United States" = "USA"
)

# ============================================================
# 4. Zahlen bereinigen
# ============================================================
parse_oecd <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  x <- trimws(as.character(x))
  x[x %in% c("", "..", "...", "NA", "N/A")] <- NA
  # OECD-Fußnoten entfernen, z.B. B 766,6 / D 4.030,4
  x <- gsub("[A-Za-z*†‡]", "", x)
  x <- gsub("\\s+", "", x)
  both <- !is.na(x) &
    grepl("\\.", x) &
    grepl(",", x, fixed = TRUE)
  x[both] <- gsub(".", "", x[both], fixed = TRUE)
  x[both] <- gsub(",", ".", x[both], fixed = TRUE)
  comma_only <- !is.na(x) &
    grepl(",", x, fixed = TRUE) &
    !grepl("\\.", x)
  x[comma_only] <- gsub(
    ",", ".", x[comma_only], fixed = TRUE
  )
  suppressWarnings(as.numeric(x))
}

# ============================================================
# 5. Wide -> Long
# ============================================================

wide_to_long <- function(dat, variable) {
  dat <- as.data.frame(dat)
  names(dat) <- trimws(names(dat))
  names(dat) <- sub(
    "^X(20[0-9]{2})$",
    "\\1",
    names(dat)
  )
  # Jahres-Spalten
  year_cols <- YEARS[YEARS %in% names(dat)]
  
  if (length(year_cols) != length(YEARS)) {
    stop(
      paste0(
        variable,
        ": Jahres-Spalten 2015-2023 fehlen.\n",
        "Vorhandene Spalten: ",
        paste(names(dat), collapse = " | ")
      ),
      call. = FALSE
    )
  }
  # Länder-Spalte
  country_candidates <- names(dat)[
    grepl(
      "Reference area|REF_AREA|Country|Land",
      names(dat),
      ignore.case = TRUE
    )
  ]
  
  country_col <- if (length(country_candidates) > 0) {
    country_candidates[1]
  } else {
    names(dat)[1]
  }
  
  country_raw <- trimws(
    as.character(dat[[country_col]])
  )
  country_raw <- gsub(
    "^·\\s*",
    "",
    country_raw
  )
  # Länder -> ISO-Code
  country_code <- rep(
    NA_character_,
    length(country_raw)
  )
  iso_in_brackets <- grepl(
    "^\\([A-Z]{3}\\)",
    country_raw
  )
  country_code[iso_in_brackets] <- sub(
    "^\\(([A-Z]{3})\\).*",
    "\\1",
    country_raw[iso_in_brackets]
  )
  country_code[!iso_in_brackets] <- unname(
    country_map[
      country_raw[!iso_in_brackets]
    ]
  )
  out <- data.frame(
    country = country_code,
    dat[, year_cols, drop = FALSE],
    check.names = FALSE
  )
  # Nur OECD-Länder
  out <- out[
    !is.na(out$country) &
      out$country %in% OECD_COUNTRIES,
    ,
    drop = FALSE
  ]
  
  if (nrow(out) == 0) {
    stop(
      paste0(
        variable,
        ": Keine OECD-Länder erkannt.\n",
        "Beispielwerte: ",
        paste(
          head(country_raw, 10),
          collapse = " | "
        )
      ),
      call. = FALSE
    )
  }

  out <- out |>
    pivot_longer(
      cols = all_of(year_cols),
      names_to = "year",
      values_to = "value"
    ) |>
    mutate(
      year = as.integer(year),
      value = parse_oecd(value)
    ) |>
    select(
      country,
      year,
      value
    )
  # ----------------------------------------------------------
  # Duplikate behandeln
  # ----------------------------------------------------------
  
  out_unique <- out |>
    distinct(
      country,
      year,
      value
    )
  
  conflicts <- out_unique |>
    count(
      country,
      year,
      name = "n"
    ) |>
    filter(n > 1)
  
  if (nrow(conflicts) > 0) {
    
    conflict_text <- conflicts |>
      mutate(
        x = paste(country, year, sep = "-")
      ) |>
      pull(x) |>
      paste(collapse = ", ")
    
    stop(
      paste0(
        variable,
        ": Unterschiedliche Werte für dasselbe Land/Jahr.\n",
        "Betroffene Beobachtungen: ",
        conflict_text
      ),
      call. = FALSE
    )
  }
  out_unique <- out_unique |>
    arrange(country, year)
  names(out_unique)[3] <- variable
  out_unique
}
# ============================================================
# 6. HAUPTVARIABLEN
# ============================================================
# ============================================================
# 6a. Reale Gesundheitsausgaben pro Kopf
# ============================================================

health_pc_raw <- as.data.frame(
  OECD_GESUNDHEITSAUSGABENPROKOPF
)

names(health_pc_raw) <- trimws(names(health_pc_raw))

# Jahresnamen vereinheitlichen
names(health_pc_raw) <- sub(
  "^X(20[0-9]{2})$",
  "\\1",
  names(health_pc_raw)
)

unit_col <- names(health_pc_raw)[2]

health_pc_raw <- health_pc_raw[
  trimws(
    as.character(health_pc_raw[[unit_col]])
  ) ==
    "Euros per person, PPP converted, 2020",
  ,
  drop = FALSE
]

# Sicherheitsprüfung
if (nrow(health_pc_raw) == 0) {
  stop(
    paste0(
      "Keine Zeilen mit der gewünschten Einheit gefunden.\n",
      "Verwendete Spalte: ", unit_col, "\n",
      "Gefundene Einheiten:\n",
      paste(
        unique(
          trimws(
            as.character(
              OECD_GESUNDHEITSAUSGABENPROKOPF[[2]]
            )
          )
        ),
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}

# Long-Format
health_pc <- wide_to_long(
  health_pc_raw,
  "health_exp_pc_real_ppp"
)

# Kontrolle
print(health_pc)
dim(health_pc)

# ------------------------------------------------------------
# 6b. Gesundheitsausgaben in % BIP
# ------------------------------------------------------------

health_gdp <- wide_to_long(
  OECD_BIP,
  "health_exp_gdp"
)

# ------------------------------------------------------------
# 6c. Hospital beds je 1.000 Einwohner
# ------------------------------------------------------------

beds <- wide_to_long(
  OECD_BEDSPER1000,
  "hospital_beds_pc"
)

# ------------------------------------------------------------
# 6d. Reales GDP pro Kopf
# ------------------------------------------------------------

gdp <- wide_to_long(
  OECD_GDP_Per_Person,
  "real_gdp_pc_ppp"
)

# ------------------------------------------------------------
# 6e. Bevölkerung >=65 Jahre
# ------------------------------------------------------------

age65 <- wide_to_long(
  OECD_AGE65,
  "share_65plus"
)

# ============================================================
# 7. DATENABDECKUNG
# ============================================================
cat("\n============================================\n")
cat("DATENABDECKUNG\n")
cat("============================================\n")

cat(
  "Health expenditure/Kopf: ",
  nrow(health_pc),
  "\n",
  sep = ""
)

cat(
  "Health expenditure/% BIP: ",
  nrow(health_gdp),
  "\n",
  sep = ""
)

cat(
  "Hospital beds: ",
  nrow(beds),
  "\n",
  sep = ""
)

cat(
  "GDP/Kopf: ",
  nrow(gdp),
  "\n",
  sep = ""
)

cat(
  "Bevölkerung >=65: ",
  nrow(age65),
  "\n",
  sep = ""
)

cat("Fehlendes Land in beds:\n")
print(setdiff(OECD_COUNTRIES, unique(beds$country)))

cat("\nLänder in beds:\n")
print(sort(unique(beds$country)))

cat("\nFehlende Land-Jahr-Kombinationen in beds:\n")

expected <- expand.grid(
  country = OECD_COUNTRIES,
  year = START_YEAR:END_YEAR
)

missing_beds <- expected |>
  anti_join(
    beds,
    by = c("country", "year")
  )

print(missing_beds)
# Rohdaten prüfen: alle Länderbezeichnungen mit "Kingdom" oder "United"
uk_rows <- OECD_BEDSPER1000[
  grepl(
    "Kingdom|United",
    as.character(OECD_BEDSPER1000[[1]]),
    ignore.case = TRUE
  ),
  ,
  drop = FALSE
]

print(uk_rows)
names(OECD_BEDSPER1000)
grep(
  "United Kingdom|Kingdom",
  as.character(OECD_BEDSPER1000[[1]]),
  ignore.case = TRUE,
  value = TRUE
)
# ============================================================
# WELCHE 9 BEOBACHTUNGEN FALLEN AUS DEN REGRESSIONEN?
# ============================================================

model_missing <- analysis |>
  mutate(
    missing_beds = is.na(hospital_beds_pc),
    missing_gdp = is.na(real_gdp_pc_ppp),
    missing_age65 = is.na(share_65plus),
    missing_health_pc = is.na(health_exp_pc_real_ppp),
    missing_health_gdp = is.na(health_exp_gdp),
    model_pc_complete =
      !is.na(hospital_beds_pc) &
      !is.na(real_gdp_pc_ppp) &
      !is.na(share_65plus) &
      !is.na(health_exp_pc_real_ppp),
    model_gdp_complete =
      !is.na(hospital_beds_pc) &
      !is.na(real_gdp_pc_ppp) &
      !is.na(share_65plus) &
      !is.na(health_exp_gdp)
  ) |>
  filter(
    !model_pc_complete | !model_gdp_complete
  ) |>
  select(
    country,
    year,
    hospital_beds_pc,
    health_exp_pc_real_ppp,
    health_exp_gdp,
    real_gdp_pc_ppp,
    share_65plus,
    missing_beds,
    missing_gdp,
    missing_age65
  )

print(model_missing, n = Inf)

analysis |>
  summarise(
    beds_NA = sum(is.na(hospital_beds_pc)),
    health_pc_NA = sum(is.na(health_exp_pc_real_ppp)),
    health_gdp_NA = sum(is.na(health_exp_gdp)),
    gdp_NA = sum(is.na(real_gdp_pc_ppp)),
    age65_NA = sum(is.na(share_65plus))
  )
# ============================================================
# 8. PANEL ZUSAMMENFÜHREN
# ============================================================

panel <- health_pc |>
  inner_join(
    health_gdp,
    by = c("country", "year")
  ) |>
  inner_join(
    beds,
    by = c("country", "year")
  ) |>
  inner_join(
    gdp,
    by = c("country", "year")
  ) |>
  inner_join(
    age65,
    by = c("country", "year")
  ) |>
  filter(
    year >= START_YEAR,
    year <= END_YEAR
  ) |>
  arrange(
    country,
    year
  )

# ============================================================
# 9. DATENQUALITÄT
# ============================================================

cat("\n============================================\n")
cat("PANEL\n")
cat("============================================\n")

cat(
  "Länder:        ",
  n_distinct(panel$country),
  "\n",
  sep = ""
)

cat(
  "Jahre:         ",
  n_distinct(panel$year),
  "\n",
  sep = ""
)

cat(
  "Beobachtungen: ",
  nrow(panel),
  "\n",
  sep = ""
)

if (nrow(panel) == 0) {
  stop(
    "Das zusammengeführte Panel enthält keine Beobachtungen.",
    call. = FALSE
  )
}

# ============================================================
# 10. Mindestens 7 vollständige Jahre je Land
# ============================================================

coverage <- panel |>
  count(
    country,
    name = "n_years"
  ) |>
  mutate(
    eligible = n_years >= MIN_YEARS
  ) |>
  arrange(
    desc(n_years),
    country
  )

cat("\n============================================\n")
cat("LÄNDERABDECKUNG\n")
cat("============================================\n")

print(coverage)

eligible_countries <- coverage |>
  filter(eligible) |>
  pull(country)

analysis <- panel |>
  filter(
    country %in% eligible_countries
  )

# ============================================================
# 11. HAUPTSTICHPROBE
# ============================================================

cat("\n============================================\n")
cat("HAUPTANALYSE\n")
cat("============================================\n")

cat(
  "Länder:        ",
  n_distinct(analysis$country),
  "\n",
  sep = ""
)

cat(
  "Jahre:         ",
  n_distinct(analysis$year),
  "\n",
  sep = ""
)

cat(
  "Beobachtungen: ",
  nrow(analysis),
  "\n",
  sep = ""
)

if (nrow(analysis) == 0) {
  stop(
    "Keine ausreichende Panel-Stichprobe vorhanden.",
    call. = FALSE
  )
}

# ============================================================
# 12. DESKRIPTIVE STATISTIK
# ============================================================

descriptive <- analysis |>
  summarise(
    countries = n_distinct(country),
    observations = n(),
    years = n_distinct(year),

    hospital_beds_mean =
      mean(hospital_beds_pc, na.rm = TRUE),

    health_exp_pc_mean =
      mean(health_exp_pc_real_ppp, na.rm = TRUE),

    health_exp_gdp_mean =
      mean(health_exp_gdp, na.rm = TRUE),

    gdp_pc_mean =
      mean(real_gdp_pc_ppp, na.rm = TRUE),

    share_65plus_mean =
      mean(share_65plus, na.rm = TRUE)
  )

print(descriptive)

# ============================================================
# 13. HAUPTMODELL 1
# Log reale Gesundheitsausgaben pro Kopf
# ============================================================

model_pc <- feols(
  log(health_exp_pc_real_ppp) ~
    hospital_beds_pc +
    log(real_gdp_pc_ppp) +
    share_65plus |
    country +
    year,
  data = analysis,
  cluster = ~country
)


# ============================================================
# 14. HAUPTMODELL 2
#
# Gesundheitsausgaben in % BIP
# ============================================================

model_gdp <- feols(
  health_exp_gdp ~
    hospital_beds_pc +
    log(real_gdp_pc_ppp) +
    share_65plus |
    country +
    year,
  data = analysis,
  cluster = ~country
)

# ============================================================
# 15. ERGEBNISSE
# ============================================================

cat("\n============================================\n")
cat("MODELL 1\n")
cat("Log Gesundheitsausgaben pro Kopf\n")
cat("============================================\n")

print(summary(model_pc))

cat("\n============================================\n")
cat("MODELL 2\n")
cat("Gesundheitsausgaben % BIP\n")
cat("============================================\n")

print(summary(model_gdp))

cat("\n============================================\n")
cat("MODELLVERGLEICH\n")
cat("============================================\n")

etable(
  model_pc,
  model_gdp,
  headers = c(
    "Log Gesundheitsausgaben/Kopf",
    "Gesundheitsausgaben % BIP"
  )
)

# ============================================================
# 16.  HC3 INPATIENT VS OUTPATIENT
# ============================================================

if (exists("OECD_Referenzlaender", inherits = TRUE)) {
  try({

    ref <- as.data.frame(OECD_Referenzlaender)
    names(ref) <- trimws(names(ref))

    # Jahres-Spalten
    ref_years <- YEARS[YEARS %in% names(ref)]

    # Relevante Spalten suchen
    ref_country_col <- names(ref)[
      grepl(
        "Reference area|REF_AREA|Country|Land",
        names(ref),
        ignore.case = TRUE
      )
    ][1]

    ref_function_col <- names(ref)[
      grepl(
        "FUNCTION|Health function",
        names(ref),
        ignore.case = TRUE
      )
    ][1]

    ref_mode_col <- names(ref)[
      grepl(
        "MODE_PROVISION|Mode of provision",
        names(ref),
        ignore.case = TRUE
      )
    ][1]

    ref_financing_col <- names(ref)[
      grepl(
        "FINANCING_SCHEME|Financing scheme",
        names(ref),
        ignore.case = TRUE
      )
    ][1]

    ref_provider_col <- names(ref)[
      grepl(
        "PROVIDER|Healthcare provider",
        names(ref),
        ignore.case = TRUE
      )
    ][1]

    if (
      !is.na(ref_country_col) &&
      !is.na(ref_function_col) &&
      !is.na(ref_mode_col) &&
      length(ref_years) == 9
    ) {

      ref_country_raw <- trimws(
        as.character(ref[[ref_country_col]])
      )

      ref_country <- unname(
        country_map[ref_country_raw]
      )

      ref_iso <- grepl(
        "^\\([A-Z]{3}\\)",
        ref_country_raw
      )

      ref_country[ref_iso] <- sub(
        "^\\(([A-Z]{3})\\).*",
        "\\1",
        ref_country_raw[ref_iso]
      )

      hc3 <- ref |>
        mutate(
          country = ref_country,
          function_text =
            as.character(.data[[ref_function_col]]),
          mode_text =
            as.character(.data[[ref_mode_col]])
        ) |>
        filter(
          country %in% c(
            "EST", "FIN", "DEU",
            "JPN", "KOR", "ROU", "SWE"
          ),
          grepl(
            "HC3",
            function_text,
            fixed = TRUE
          ),
          grepl(
            "HBEDT|HBOUT",
            mode_text
          )
        )

      if (!is.na(ref_financing_col)) {
        hc3 <- hc3 |>
          filter(
            grepl(
              "_T|Total",
              .data[[ref_financing_col]],
              ignore.case = TRUE
            )
          )
      }

      if (!is.na(ref_provider_col)) {
        hc3 <- hc3 |>
          filter(
            grepl(
              "_T|Total",
              .data[[ref_provider_col]],
              ignore.case = TRUE
            )
          )
      }

      hc3 <- hc3 |>
        select(
          country,
          mode_text,
          all_of(ref_years)
        ) |>
        pivot_longer(
          cols = all_of(ref_years),
          names_to = "year",
          values_to = "value"
        ) |>
        mutate(
          year = as.integer(year),
          value = parse_oecd(value),
          mode = case_when(
            grepl("HBEDT", mode_text) ~ "inpatient",
            grepl("HBOUT", mode_text) ~ "outpatient",
            TRUE ~ NA_character_
          )
        ) |>
        filter(!is.na(mode))

      hc3_share <- hc3 |>
        pivot_wider(
          names_from = mode,
          values_from = value
        ) |>
        mutate(
          hc3_total = inpatient + outpatient,
          hc3_inpatient_share =
            inpatient / hc3_total
        ) |>
        arrange(country, year)

      cat("\n============================================\n")
      cat("HC3: INPATIENT VS OUTPATIENT\n")
      cat("============================================\n")

      print(hc3_share)

      write.csv(
        hc3_share,
        "OECD_HC3_Referenzlaender_2015_2023.csv",
        row.names = FALSE,
        na = ""
      )
    }

  }, silent = TRUE)
}


# ============================================================
# 17. OPTIONAL: Health_expend_2023_Abb1
# ============================================================

if (exists("Health_expend_2023_Abb1", inherits = TRUE)) {

  composition_2023 <- as.data.frame(
    Health_expend_2023_Abb1
  )

  cat("\n============================================\n")
  cat("HEALTH EXPENDITURE 2023 - ABBILDUNG 1\n")
  cat("============================================\n")

  print(composition_2023)

  write.csv(
    composition_2023,
    "Health_expend_2023_Abb1.csv",
    row.names = FALSE,
    na = ""
  )
}

# ============================================================
# 18. EXPORT
# ============================================================

write.csv(
  analysis,
  "OECD_panel_analysis_2015_2023.csv",
  row.names = FALSE,
  na = ""
)

write.csv(
  coverage,
  "OECD_panel_coverage_2015_2023.csv",
  row.names = FALSE,
  na = ""
)

write.csv(
  descriptive,
  "OECD_panel_descriptive_2015_2023.csv",
  row.names = FALSE,
  na = ""
)

capture.output(
  etable(model_pc, model_gdp),
  file = "OECD_panel_results_2015_2023.txt"
)

saveRDS(
  list(
    model_pc = model_pc,
    model_gdp = model_gdp
  ),
  "OECD_panel_models_2015_2023.rds"
)
# ============================================================
# 19. ENDE
# ============================================================

cat(
  "\n============================================\n",
  "ANALYSE ABGESCHLOSSEN\n",
  "============================================\n"
)

