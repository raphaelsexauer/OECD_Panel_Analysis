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
# ============================================================
library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)

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
# 1. LÄNDERNAMEN -> OECD-CODES
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
  "Czech Republic" = "CZE",
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
  "Republic of Korea" = "KOR",
  "Korea, Republic of" = "KOR",
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
  "Slovakia" = "SVK",
  "Slovenia" = "SVN",
  "Spain" = "ESP",
  "Sweden" = "SWE",
  "Switzerland" = "CHE",
  "Türkiye" = "TUR",
  "Turkey" = "TUR",
  "United Kingdom" = "GBR",
  "United States" = "USA",
  "United States of America" = "USA",
  "Romania" = "ROU"
)


# ============================================================
# 2. HILFSFUNKTION: LÄNDER -> ISO-CODE
# ============================================================

country_to_iso <- function(x) {
  
  raw <- trimws(as.character(x))
  
  raw[raw %in% c("", "..", "...", "NA", "N/A")] <- NA_character_
  
  # Führende OECD-Ausgabezeichen entfernen
  raw <- gsub("^·\\s*", "", raw)
  
  code <- rep(NA_character_, length(raw))
  
  # "(DEU) Germany"
  iso_in_brackets <- !is.na(raw) &
    grepl("^\\([A-Z]{3}\\)", raw)
  
  code[iso_in_brackets] <- sub(
    "^\\(([A-Z]{3})\\).*",
    "\\1",
    raw[iso_in_brackets]
  )
  
  # Normale Ländernamen
  normal <- !is.na(raw) & !iso_in_brackets
  
  code[normal] <- unname(
    country_map[raw[normal]]
  )
  
  # Robuste Sonderregeln für häufige OECD-Bezeichnungen
  code[
    !is.na(raw) &
      grepl(
        "United Kingdom|Great Britain",
        raw,
        ignore.case = TRUE
      )
  ] <- "GBR"
  
  code[
    !is.na(raw) &
      grepl(
        "United States",
        raw,
        ignore.case = TRUE
      )
  ] <- "USA"
  
  code[
    !is.na(raw) &
      grepl(
        "^Korea|Republic of Korea",
        raw,
        ignore.case = TRUE
      )
  ] <- "KOR"
  
  code[
    !is.na(raw) &
      grepl(
        "Türkiye|Turkey",
        raw,
        ignore.case = TRUE
      )
  ] <- "TUR"
  
  code[
    !is.na(raw) &
      grepl(
        "Slovak",
        raw,
        ignore.case = TRUE
      )
  ] <- "SVK"
  
  code[
    !is.na(raw) &
      grepl(
        "Czech",
        raw,
        ignore.case = TRUE
      )
  ] <- "CZE"
  
  code
}


# ============================================================
# 3. ZAHLEN AUS OECD-EXCEL BEREINIGEN
# ============================================================

parse_oecd <- function(x) {
  
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  
  x <- trimws(as.character(x))
  
  x[x %in% c("", "..", "...", "NA", "N/A")] <- NA_character_
  
  # OECD-Fußnoten entfernen
  # z.B. B 766,6 / D 4.030,4
  x <- gsub("[A-Za-z*†‡]", "", x)
  
  # Leerzeichen entfernen
  x <- gsub("\\s+", "", x)
  
  # 4.030,4 -> 4030.4
  both <- !is.na(x) &
    grepl("\\.", x) &
    grepl(",", x, fixed = TRUE)
  
  x[both] <- gsub(
    ".",
    "",
    x[both],
    fixed = TRUE
  )
  
  x[both] <- gsub(
    ",",
    ".",
    x[both],
    fixed = TRUE
  )
  
  # 766,6 -> 766.6
  comma_only <- !is.na(x) &
    grepl(",", x, fixed = TRUE) &
    !grepl("\\.", x)
  
  x[comma_only] <- gsub(
    ",",
    ".",
    x[comma_only],
    fixed = TRUE
  )
  
  suppressWarnings(
    as.numeric(x)
  )
}


# ============================================================
# 4. WIDE -> LONG
# ============================================================

wide_to_long <- function(dat, variable) {
  
  dat <- as.data.frame(dat)
  
  names(dat) <- trimws(names(dat))
  
  # Falls R Jahresnamen als X2015 importiert hat
  names(dat) <- sub(
    "^X(20[0-9]{2})$",
    "\\1",
    names(dat)
  )
  
  # Jahres-Spalten
  year_cols <- YEARS[
    YEARS %in% names(dat)
  ]
  
  if (length(year_cols) != length(YEARS)) {
    
    stop(
      paste0(
        variable,
        ": Jahres-Spalten 2015-2023 fehlen.\n",
        "Vorhandene Spalten: ",
        paste(
          names(dat),
          collapse = " | "
        )
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
  
  country_col <- if (
    length(country_candidates) > 0
  ) {
    
    country_candidates[1]
    
  } else {
    
    names(dat)[1]
  }
  
  country_raw <- trimws(
    as.character(
      dat[[country_col]]
    )
  )
  
  # Länder -> ISO
  country_code <- country_to_iso(
    country_raw
  )
  
  out <- data.frame(
    country = country_code,
    dat[, year_cols, drop = FALSE],
    check.names = FALSE
  )
  
  # Nur definierte OECD-Länder
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
  
  # Wide -> Long
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
  # Duplikate 
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
    filter(
      n > 1
    )
  
  if (nrow(conflicts) > 0) {
    
    conflict_text <- conflicts |>
      mutate(
        x = paste(
          country,
          year,
          sep = "-"
        )
      ) |>
      pull(x) |>
      paste(
        collapse = ", "
      )
    
    stop(
      paste0(
        variable,
        ": Unterschiedliche Werte für dasselbe ",
        "Land/Jahr.\n",
        "Betroffene Beobachtungen: ",
        conflict_text
      ),
      call. = FALSE
    )
  }
  
  out_unique <- out_unique |>
    arrange(
      country,
      year
    )
  
  names(out_unique)[3] <- variable
  
  out_unique
}


# ============================================================
# 7. PRÜFUNG 
# ============================================================

required_objects <- c(
  "OECD_GESUNDHEITSAUSGABENPROKOPF",
  "OECD_BIP",
  "OECD_BEDSPER1000",
  "OECD_GDP_Per_Person",
  "OECD_AGE65"
)

missing_objects <- required_objects[
  !vapply(
    required_objects,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_objects) > 0) {
  
  stop(
    paste0(
      "Folgende bereits importierte R-Objekte fehlen:\n",
      paste(
        missing_objects,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 8. HAUPTVARIABLEN
# ============================================================


# ============================================================
# 8a. GESUNDHEITSAUSGABEN PRO KOPF
# ============================================================

health_pc_raw <- as.data.frame(
  OECD_GESUNDHEITSAUSGABENPROKOPF
)

names(health_pc_raw) <- trimws(
  names(health_pc_raw)
)

# Jahresnamen vereinheitlichen
names(health_pc_raw) <- sub(
  "^X(20[0-9]{2})$",
  "\\1",
  names(health_pc_raw)
)

if (ncol(health_pc_raw) < 2) {
  
  stop(
    "OECD_GESUNDHEITSAUSGABENPROKOPF enthält weniger als 2 Spalten.",
    call. = FALSE
  )
}

unit_col <- names(health_pc_raw)[2]

health_pc_raw <- health_pc_raw[
  trimws(
    as.character(
      health_pc_raw[[unit_col]]
    )
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
      "Verwendete Spalte: ",
      unit_col,
      "\n",
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

cat("\n============================================\n")
cat("HEALTH EXPENDITURE / KOPF\n")
cat("============================================\n")

print(
  health_pc,
  n = 20
)

cat(
  "\nBeobachtungen: ",
  nrow(health_pc),
  "\n",
  sep = ""
)


# ============================================================
# 8b. GESUNDHEITSAUSGABEN IN % BIP
# ============================================================

health_gdp <- wide_to_long(
  OECD_BIP,
  "health_exp_gdp"
)


# ============================================================
# 8c. HOSPITAL BEDS JE 1.000 EINWOHNER
# ============================================================

beds <- wide_to_long(
  OECD_BEDSPER1000,
  "hospital_beds_pc"
)


# ============================================================
# 8d. REALES GDP PRO KOPF
# ============================================================

gdp <- wide_to_long(
  OECD_GDP_Per_Person,
  "real_gdp_pc_ppp"
)


# ============================================================
# 8e. BEVÖLKERUNG >=65 JAHRE
# ============================================================

age65 <- wide_to_long(
  OECD_AGE65,
  "share_65plus"
)


# ============================================================
# 9. DATENABDECKUNG DER EINZELVARIABLEN
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


# ============================================================
# 10. OECD-LÄNDER OHNE BETTENREIHE
# ============================================================

AVAILABLE_BEDS_COUNTRIES <- intersect(
  OECD_COUNTRIES,
  unique(beds$country)
)

EXCLUDED_BEDS_COUNTRIES <- setdiff(
  OECD_COUNTRIES,
  AVAILABLE_BEDS_COUNTRIES
)

cat("\n============================================\n")
cat("LÄNDER OHNE HOSPITAL-BEDS-REIHE\n")
cat("============================================\n")

if (length(EXCLUDED_BEDS_COUNTRIES) > 0) {
  
  print(
    EXCLUDED_BEDS_COUNTRIES
  )
  
} else {
  
  cat("Keine OECD-Länder ausgeschlossen.\n")
}


# ============================================================
# 11. FEHLENDE LAND-JAHR-KOMBINATIONEN IN BETTEN
# ============================================================

expected <- expand.grid(
  country = OECD_COUNTRIES,
  year = START_YEAR:END_YEAR,
  stringsAsFactors = FALSE
)

missing_beds <- expected |>
  anti_join(
    beds,
    by = c(
      "country",
      "year"
    )
  )

cat("\n============================================\n")
cat("FEHLENDE LAND-JAHR-KOMBINATIONEN IN BEDS\n")
cat("============================================\n")

if (nrow(missing_beds) > 0) {
  
  print(
    as_tibble(missing_beds),
    n = Inf
  )
  
} else {
  
  cat("Keine fehlenden Land-Jahr-Kombinationen.\n")
}
# ============================================================
# 12. PANEL ZUSAMMENFÜHREN
# ============================================================

panel <- health_pc |>
  inner_join(
    health_gdp,
    by = c(
      "country",
      "year"
    )
  ) |>
  inner_join(
    beds,
    by = c(
      "country",
      "year"
    )
  ) |>
  inner_join(
    gdp,
    by = c(
      "country",
      "year"
    )
  ) |>
  inner_join(
    age65,
    by = c(
      "country",
      "year"
    )
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
# 13. PANEL-DATENQUALITÄT
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
# 14. VOLLSTÄNDIGKEIT DER MODELLVARIABLEN
# ============================================================

panel <- panel |>
  mutate(
    complete_model =
      !is.na(hospital_beds_pc) &
      !is.na(health_exp_pc_real_ppp) &
      !is.na(health_exp_gdp) &
      !is.na(real_gdp_pc_ppp) &
      !is.na(share_65plus)
  )


# ============================================================
# 15. DATENABDECKUNG JE LAND
# Einschluss:
# mindestens 7 vollständige Modelljahre.
# ============================================================

coverage <- panel |>
  group_by(country) |>
  summarise(
    n_complete = sum(
      complete_model
    ),
    n_total = n(),
    eligible = n_complete >= MIN_YEARS,
    .groups = "drop"
  ) |>
  arrange(
    desc(n_complete),
    country
  )

cat("\n============================================\n")
cat("LÄNDERABDECKUNG – VOLLSTÄNDIGE MODELLJAHRE\n")
cat("============================================\n")

print(
  coverage,
  n = Inf
)

# Verfügbare Einheiten prüfen
cat("\nVerfügbare Einheiten:\n")
print(sort(unique(na.omit(ref$combined_text))))
# ============================================================
# 16. ELIGIBLE / AUSGESCHLOSSENE LÄNDER
# ============================================================

eligible_countries <- coverage |>
  filter(
    eligible
  ) |>
  pull(
    country
  )

excluded_countries <- coverage |>
  filter(
    !eligible
  )

cat("\n============================================\n")
cat("AUSGESCHLOSSENE LÄNDER WEGEN < 7 VOLLSTÄNDIGER JAHRE\n")
cat("============================================\n")

if (nrow(excluded_countries) > 0) {
  
  print(
    excluded_countries,
    n = Inf
  )
  
} else {
  
  cat("Keine Länder ausgeschlossen.\n")
}


# ============================================================
# 17. HAUPTSTICHPROBE
#
# Es werden nur Länder mit >= 7 vollständigen Modelljahren
# und nur vollständige Land-Jahr-Beobachtungen verwendet.
# ============================================================

analysis <- panel |>
  filter(
    country %in% eligible_countries,
    complete_model
  ) |>
  arrange(
    country,
    year
  )


# ============================================================
# 18. HAUPTANALYSE – KONTROLLE
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
# 19. FEHLENDE BEOBACHTUNGEN – DOKUMENTATION
#
# ============================================================

model_missing <- panel |>
  mutate(
    missing_beds =
      is.na(hospital_beds_pc),
    
    missing_health_pc =
      is.na(health_exp_pc_real_ppp),
    
    missing_health_gdp =
      is.na(health_exp_gdp),
    
    missing_gdp =
      is.na(real_gdp_pc_ppp),
    
    missing_age65 =
      is.na(share_65plus)
  ) |>
  filter(
    !complete_model
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
    missing_health_pc,
    missing_health_gdp,
    missing_gdp,
    missing_age65
  )

cat("\n============================================\n")
cat("FEHLENDE MODELLBEOBACHTUNGEN IM URSPRÜNGLICHEN PANEL\n")
cat("============================================\n")

if (nrow(model_missing) > 0) {
  
  print(
    model_missing,
    n = Inf
  )
  
} else {
  
  cat("Keine fehlenden Modellbeobachtungen.\n")
}


# ============================================================
# 20. MISSINGNESS-KONTROLLE
# ============================================================

missing_summary <- panel |>
  summarise(
    beds_NA =
      sum(is.na(hospital_beds_pc)),
    
    health_pc_NA =
      sum(is.na(health_exp_pc_real_ppp)),
    
    health_gdp_NA =
      sum(is.na(health_exp_gdp)),
    
    gdp_NA =
      sum(is.na(real_gdp_pc_ppp)),
    
    age65_NA =
      sum(is.na(share_65plus))
  )

cat("\n============================================\n")
cat("MISSINGNESS\n")
cat("============================================\n")

print(
  missing_summary
)


# ============================================================
# 21. PLAUSIBILITÄTSPRÜFUNGEN
# ============================================================

# Positive Werte für Log-Variablen erforderlich
if (any(
  analysis$health_exp_pc_real_ppp <= 0,
  na.rm = TRUE
)) {
  
  stop(
    "health_exp_pc_real_ppp enthält Werte <= 0.",
    call. = FALSE
  )
}

if (any(
  analysis$real_gdp_pc_ppp <= 0,
  na.rm = TRUE
)) {
  
  stop(
    "real_gdp_pc_ppp enthält Werte <= 0.",
    call. = FALSE
  )
}

# Keine fehlenden Modellvariablen
if (anyNA(
  analysis[
    c(
      "hospital_beds_pc",
      "health_exp_pc_real_ppp",
      "health_exp_gdp",
      "real_gdp_pc_ppp",
      "share_65plus"
    )
  ]
)) {
  
  stop(
    "analysis enthält trotz Bereinigung noch NA-Werte.",
    call. = FALSE
  )
}


# ============================================================
# 22. DESKRIPTIVE STATISTIK
# ============================================================

descriptive <- analysis |>
  summarise(
    countries =
      n_distinct(country),
    
    observations =
      n(),
    
    years =
      n_distinct(year),
    
    hospital_beds_mean =
      mean(
        hospital_beds_pc,
        na.rm = TRUE
      ),
    
    health_exp_pc_mean =
      mean(
        health_exp_pc_real_ppp,
        na.rm = TRUE
      ),
    
    health_exp_gdp_mean =
      mean(
        health_exp_gdp,
        na.rm = TRUE
      ),
    
    gdp_pc_mean =
      mean(
        real_gdp_pc_ppp,
        na.rm = TRUE
      ),
    
    share_65plus_mean =
      mean(
        share_65plus,
        na.rm = TRUE
      )
  )

cat("\n============================================\n")
cat("DESKRIPTIVE STATISTIK\n")
cat("============================================\n")

print(
  descriptive
)


# ============================================================
# 23. HAUPTMODELL 1
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
# 24. HAUPTMODELL 2
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
# 25. ERGEBNISSE MODELL 1
# ============================================================

cat("\n============================================\n")
cat("MODELL 1\n")
cat("Log Gesundheitsausgaben pro Kopf\n")
cat("============================================\n")

print(
  summary(model_pc)
)


# ============================================================
# 26. ERGEBNISSE MODELL 2
# ============================================================

cat("\n============================================\n")
cat("MODELL 2\n")
cat("Gesundheitsausgaben % BIP\n")
cat("============================================\n")

print(
  summary(model_gdp)
)


# ============================================================
# 27. MODELLVERGLEICH
# ============================================================

cat("\n============================================\n")
cat("MODELLVERGLEICH\n")
cat("============================================\n")

model_table <- etable(
  model_pc,
  model_gdp,
  headers = c(
    "Log Gesundheitsausgaben/Kopf",
    "Gesundheitsausgaben % BIP"
  )
)

print(
  model_table
)


# ============================================================
# 28. EFFEKTGRÖSSE HOSPITAL BEDS – MODELL 1
# ============================================================

beds_beta_pc <- coef(
  model_pc
)["hospital_beds_pc"]

beds_effect_pc_percent <-
  100 * (
    exp(beds_beta_pc) - 1
  )

cat("\n============================================\n")
cat("EFFEKTGRÖSSE HOSPITAL BEDS – MODELL 1\n")
cat("============================================\n")

cat(
  "Koeffizient: ",
  round(
    beds_beta_pc,
    6
  ),
  "\n",
  sep = ""
)

cat(
  "Exakter Prozent-Effekt je +1 Bett/1.000: ",
  round(
    beds_effect_pc_percent,
    3
  ),
  "%\n",
  sep = ""
)


# ============================================================
# 29. HC3 INPATIENT VS OUTPATIENT
# ============================================================

if (
  exists(
    "OECD_Referenzlaender",
    inherits = TRUE
  )
) {
  
  tryCatch({
    
    ref <- as.data.frame(
      OECD_Referenzlaender
    )
    
    names(ref) <- trimws(
      names(ref)
    )
    
    # Jahresnamen vereinheitlichen
    names(ref) <- sub(
      "^X(20[0-9]{2})$",
      "\\1",
      names(ref)
    )
    
    ref_years <- YEARS[
      YEARS %in% names(ref)
    ]
    
    # Relevante Spalten suchen
    ref_country_candidates <- names(ref)[
      grepl(
        "Reference area|REF_AREA|Country|Land",
        names(ref),
        ignore.case = TRUE
      )
    ]
    
    ref_function_candidates <- names(ref)[
      grepl(
        "FUNCTION|Health function",
        names(ref),
        ignore.case = TRUE
      )
    ]
    
    ref_mode_candidates <- names(ref)[
      grepl(
        "MODE_PROVISION|Mode of provision",
        names(ref),
        ignore.case = TRUE
      )
    ]
    
    ref_financing_candidates <- names(ref)[
      grepl(
        "FINANCING_SCHEME|Financing scheme",
        names(ref),
        ignore.case = TRUE
      )
    ]
    
    ref_provider_candidates <- names(ref)[
      grepl(
        "PROVIDER|Healthcare provider",
        names(ref),
        ignore.case = TRUE
      )
    ]
    
    ref_country_col <-
      if (length(ref_country_candidates) > 0) {
        ref_country_candidates[1]
      } else {
        NA_character_
      }
    
    ref_function_col <-
      if (length(ref_function_candidates) > 0) {
        ref_function_candidates[1]
      } else {
        NA_character_
      }
    
    ref_mode_col <-
      if (length(ref_mode_candidates) > 0) {
        ref_mode_candidates[1]
      } else {
        NA_character_
      }
    
    ref_financing_col <-
      if (length(ref_financing_candidates) > 0) {
        ref_financing_candidates[1]
      } else {
        NA_character_
      }
    
    ref_provider_col <-
      if (length(ref_provider_candidates) > 0) {
        ref_provider_candidates[1]
      } else {
        NA_character_
      }
    
    
    # --------------------------------------------------------
    # Grundvoraussetzungen
    # --------------------------------------------------------
    
    if (
      !is.na(ref_country_col) &&
      !is.na(ref_function_col) &&
      !is.na(ref_mode_col) &&
      length(ref_years) == length(YEARS)
    ) {
      
      ref_country_raw <- trimws(
        as.character(
          ref[[ref_country_col]]
        )
      )
      
      ref_country <- country_to_iso(
        ref_country_raw
      )
      
      hc3 <- ref |>
        mutate(
          country = ref_country,
          
          function_text =
            as.character(
              .data[[ref_function_col]]
            ),
          
          mode_text =
            as.character(
              .data[[ref_mode_col]]
            )
        ) |>
        filter(
          country %in% c(
            "EST",
            "FIN",
            "DEU",
            "JPN",
            "KOR",
            "ROU",
            "SWE"
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
      
      # ------------------------------------------------------
      # Finanzierungsschema auf Total begrenzen
      # ------------------------------------------------------
      
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
            grepl(
              "HBEDT",
              mode_text
            ) ~ "inpatient",
            
            grepl(
              "HBOUT",
              mode_text
            ) ~ "outpatient",
            
            TRUE ~ NA_character_
          )
        ) |>
        filter(
          !is.na(mode)
        )
      
      
      hc3 <- hc3 |>
        distinct(
          country,
          year,
          mode,
          value
        )
      
      hc3_conflicts <- hc3 |>
        group_by(
          country,
          year,
          mode
        ) |>
        summarise(
          n_values =
            n_distinct(
              value,
              na.rm = TRUE
            ),
          .groups = "drop"
        ) |>
        filter(
          n_values > 1
        )
      
      if (nrow(hc3_conflicts) > 0) {
        
        cat(
          "\n============================================\n"
        )
        
        cat(
          "HC3-BLOCK ÜBERSPRUNGEN\n"
        )
        
        cat(
          "============================================\n"
        )
        
        cat(
          paste0(
            "Es existieren widersprüchliche Werte für ",
            "dieselbe Land/Jahr/Mode-Kombination.\n",
            "Betroffene Kombinationen: ",
            nrow(hc3_conflicts),
            "\n"
          )
        )
        
      } else {
        
        # ----------------------------------------------------
        # Wide-Format
        # ----------------------------------------------------
        
        hc3_share <- hc3 |>
          select(
            country,
            year,
            mode,
            value
          ) |>
          pivot_wider(
            names_from = mode,
            values_from = value
          ) |>
          mutate(
            hc3_total =
              inpatient +
              outpatient,
            
            hc3_inpatient_share =
              inpatient /
              hc3_total
          ) |>
          arrange(
            country,
            year
          )
        
        # ----------------------------------------------------
        # Nur Beobachtungen mit beiden Kategorien
        # ----------------------------------------------------
        
        hc3_share <- hc3_share |>
          filter(
            !is.na(inpatient),
            !is.na(outpatient),
            !is.na(hc3_total),
            hc3_total > 0
          )
        
        cat(
          "\n============================================\n"
        )
        
        cat(
          "HC3: INPATIENT VS OUTPATIENT\n"
        )
        
        cat(
          "============================================\n"
        )
        
        if (nrow(hc3_share) > 0) {
          
          print(
            hc3_share,
            n = Inf
          )
          
          write.csv(
            hc3_share,
            "OECD_HC3_Referenzlaender_2015_2023.csv",
            row.names = FALSE,
            na = ""
          )
          
        } else {
          
          cat(
            "Keine vollständigen HC3-Land-Jahr-Beobachtungen.\n"
          )
        }
      }
      
    } else {
      
      cat(
        "\n============================================\n"
      )
      
      cat(
        "HC3-BLOCK ÜBERSPRUNGEN\n"
      )
      
      cat(
        "============================================\n"
      )
      
      cat(
        "Benötigte Spalten oder Jahreswerte nicht vollständig vorhanden.\n"
      )
    }
    
  }, error = function(e) {
    
    cat(
      "\n============================================\n"
    )
    
    cat(
      "HC3-BLOCK ÜBERSPRUNGEN\n"
    )
    
    cat(
      "============================================\n"
    )
    
    cat(
      "Fehler im optionalen HC3-Block:\n"
    )
    
    cat(
      conditionMessage(e),
      "\n"
    )
    
    cat(
      "Die Hauptanalyse wurde erfolgreich fortgesetzt.\n"
    )
  })
}


# ============================================================
# 30. OPTIONAL: Health_expend_2023_Abb1
#
# Nur deskriptiv.
# ============================================================

if (
  exists(
    "Health_expend_2023_Abb1",
    inherits = TRUE
  )
) {
  
  composition_2023 <- as.data.frame(
    Health_expend_2023_Abb1
  )
  
  cat("\n============================================\n")
  cat("HEALTH EXPENDITURE 2023 - ABBILDUNG 1\n")
  cat("============================================\n")
  
  print(
    composition_2023
  )
  
  write.csv(
    composition_2023,
    "Health_expend_2023_Abb1.csv",
    row.names = FALSE,
    na = ""
  )
}


# ============================================================
# 31. EXPORT
# ============================================================


# Hauptanalyse
write.csv(
  analysis,
  "OECD_panel_analysis_2015_2023.csv",
  row.names = FALSE,
  na = ""
)


# Vollständiges Panel vor Complete-Case-Selektion
write.csv(
  panel,
  "OECD_panel_full_2015_2023.csv",
  row.names = FALSE,
  na = ""
)


# Länderabdeckung
write.csv(
  coverage,
  "OECD_panel_coverage_2015_2023.csv",
  row.names = FALSE,
  na = ""
)


# Ausgeschlossene Länder
write.csv(
  excluded_countries,
  "OECD_panel_excluded_countries_2015_2023.csv",
  row.names = FALSE,
  na = ""
)


# Fehlende Beobachtungen
write.csv(
  model_missing,
  "OECD_panel_missing_observations_2015_2023.csv",
  row.names = FALSE,
  na = ""
)


# Missingness-Übersicht
write.csv(
  missing_summary,
  "OECD_panel_missing_summary_2015_2023.csv",
  row.names = FALSE,
  na = ""
)


# Deskriptive Statistik
write.csv(
  descriptive,
  "OECD_panel_descriptive_2015_2023.csv",
  row.names = FALSE,
  na = ""
)


# Modellvergleich
capture.output(
  etable(
    model_pc,
    model_gdp,
    headers = c(
      "Log Gesundheitsausgaben/Kopf",
      "Gesundheitsausgaben % BIP"
    )
  ),
  file = "OECD_panel_results_2015_2023.txt"
)


# Modelle speichern
saveRDS(
  list(
    model_pc = model_pc,
    model_gdp = model_gdp,
    analysis = analysis,
    panel = panel,
    coverage = coverage,
    descriptive = descriptive,
    model_missing = model_missing,
    missing_summary = missing_summary
  ),
  "OECD_panel_models_2015_2023.rds"
)


# ============================================================
# 32. ABSCHLUSSKONTROLLE
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "ANALYSE ABGESCHLOSSEN\n"
)

cat(
  "============================================\n"
)

cat(
  "Finale Länderzahl: ",
  n_distinct(analysis$country),
  "\n",
  sep = ""
)

cat(
  "Finale Beobachtungszahl: ",
  nrow(analysis),
  "\n",
  sep = ""
)

cat(
  "Zeitraum: ",
  min(analysis$year),
  "-",
  max(analysis$year),
  "\n",
  sep = ""
)

cat(
  "Excluded wegen < ",
  MIN_YEARS,
  " vollständigen Jahren: ",
  nrow(excluded_countries),
  "\n",
  sep = ""
)

cat(
  "Fehlende Modellbeobachtungen im finalen Sample: ",
  sum(
    !complete.cases(
      analysis[
        c(
          "hospital_beds_pc",
          "health_exp_pc_real_ppp",
          "health_exp_gdp",
          "real_gdp_pc_ppp",
          "share_65plus"
        )
      ]
    )
  ),
  "\n",
  sep = ""
)

cat(
  "============================================\n"
)

# ============================================================
# 33. Grafiken
# ============================================================
# ============================================================
# GRAFIKEN OECD-PANELANALYSE 2015-2023
#
# Voraussetzungen:
#   analysis
#   model_pc
#   model_gdp
#
# müssen bereits aus dem Hauptskript vorhanden sein.
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(fixest)


# ============================================================
# 0. AUSGABEVERZEICHNIS
# ============================================================

dir.create(
  "figures",
  showWarnings = FALSE
)


# ============================================================
# ABBILDUNG 1
# ENTWICKLUNG VON BETTENDICHTE UND GESUNDHEITSAUSGABEN
#
# OECD-Mittelwerte je Jahr
# Darstellung als Index:
# 2015 = 100
#
# Vorteil:
# Unterschiedliche Einheiten werden vermieden.
# ============================================================

trend_data <- analysis |>
  group_by(year) |>
  summarise(
    hospital_beds =
      mean(
        hospital_beds_pc,
        na.rm = TRUE
      ),
    
    health_exp_pc =
      mean(
        health_exp_pc_real_ppp,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(
      hospital_beds,
      health_exp_pc
    ),
    names_to = "variable",
    values_to = "value"
  ) |>
  group_by(variable) |>
  arrange(year) |>
  mutate(
    index_2015 =
      100 * value /
      value[year == START_YEAR][1]
  ) |>
  ungroup() |>
  mutate(
    variable = case_when(
      variable == "hospital_beds" ~
        "Krankenhausbetten je 1.000 Einwohner",
      
      variable == "health_exp_pc" ~
        "Reale Gesundheitsausgaben pro Kopf",
      
      TRUE ~ variable
    )
  )


fig1 <- ggplot(
  trend_data,
  aes(
    x = year,
    y = index_2015
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2
  ) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ variable,
    ncol = 1,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = START_YEAR:END_YEAR
  ) +
  labs(
    title =
      "Entwicklung von Krankenhausbettendichte und Gesundheitsausgaben",
    
    subtitle =
      "Ungewichtete OECD-Mittelwerte; Index 2015 = 100",
    
    x = NULL,
    
    y =
      "Index (2015 = 100)"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    plot.title =
      element_text(
        face = "bold"
      ),
    
    strip.text =
      element_text(
        face = "bold"
      ),
    
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )


print(fig1)

ggsave(
  filename =
    "figures/Abb1_Entwicklung_Betten_Gesundheitsausgaben.png",
  
  plot = fig1,
  
  width = 8,
  
  height = 7,
  
  dpi = 300
)


# ============================================================
# ABBILDUNG 2
#
# FE-RESIDUALISIERTER ZUSAMMENHANG
#
# Y:
#   log(reale Gesundheitsausgaben/Kopf)
#
# X:
#   Hospital beds je 1.000
#
# Herausgerechnet werden:
#   - Länder-FE
#   - Jahres-FE
#   - reales GDP/Kopf
#   - Anteil >=65 Jahre
#
# Dies visualisiert den partiellen Zusammenhang,
# der dem Koeffizienten hospital_beds_pc in Modell 1
# entspricht.
# ============================================================


# Modell nur für Outcome ohne Hospital beds
model_pc_without_beds <- feols(
  log(health_exp_pc_real_ppp) ~
    log(real_gdp_pc_ppp) +
    share_65plus |
    country +
    year,
  data = analysis
)


# Modell nur für Hospital beds
# mit denselben Kontrollvariablen und FE
model_beds_without_outcome <- feols(
  hospital_beds_pc ~
    log(real_gdp_pc_ppp) +
    share_65plus |
    country +
    year,
  data = analysis
)


# Residuen in analysis übernehmen
fe_plot_data <- analysis |>
  mutate(
    residual_beds =
      resid(
        model_beds_without_outcome
      ),
    
    residual_health_pc =
      resid(
        model_pc_without_beds
      )
  )


# Koeffizient und p-Wert aus dem Hauptmodell
beta_beds <-
  coef(
    model_pc
  )[["hospital_beds_pc"]]

p_beds <-
  summary(model_pc)$coeftable[
    "hospital_beds_pc",
    "Pr(>|t|)"
  ]


fig2 <- ggplot(
  fe_plot_data,
  aes(
    x = residual_beds,
    y = residual_health_pc
  )
) +
  geom_point(
    alpha = 0.55,
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title =
      "Partieller Zusammenhang zwischen Krankenhausbettendichte und Gesundheitsausgaben",
    
    subtitle =
      paste0(
        "Länder- und Jahres-FE sowie BIP/Kopf und Altersstruktur herausgerechnet; ",
        "β = ",
        round(beta_beds, 3),
        ", p = ",
        format.pval(
          p_beds,
          digits = 3,
          eps = 0.001
        )
      ),
    
    x =
      "FE-residualisierte Krankenhausbettendichte",
    
    y =
      "FE-residualisierte log. Gesundheitsausgaben/Kopf"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    plot.title =
      element_text(
        face = "bold"
      )
  )


print(fig2)

ggsave(
  filename =
    "figures/Abb2_FE_residualisierter_Zusammenhang.png",
  
  plot = fig2,
  
  width = 8,
  
  height = 6,
  
  dpi = 300
)


# ============================================================
# ABBILDUNGEN
# ============================================================
# ============================================================
# GRAFIKEN OECD-PANELANALYSE 2015-2023
#
# Voraussetzungen:
#   analysis
#   model_pc
#   model_gdp
#
# müssen bereits aus dem Hauptskript vorhanden sein.
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(fixest)


# ============================================================
# 0. AUSGABEVERZEICHNIS
# ============================================================

dir.create(
  "figures",
  showWarnings = FALSE
)


# ============================================================
# ABBILDUNG 1
# ENTWICKLUNG VON BETTENDICHTE UND GESUNDHEITSAUSGABEN
#
# OECD-Mittelwerte je Jahr
# Darstellung als Index:
# 2015 = 100
#
# Vorteil:
# Unterschiedliche Einheiten werden vermieden.
# ============================================================

trend_data <- analysis |>
  group_by(year) |>
  summarise(
    hospital_beds =
      mean(
        hospital_beds_pc,
        na.rm = TRUE
      ),
    
    health_exp_pc =
      mean(
        health_exp_pc_real_ppp,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(
      hospital_beds,
      health_exp_pc
    ),
    names_to = "variable",
    values_to = "value"
  ) |>
  group_by(variable) |>
  arrange(year) |>
  mutate(
    index_2015 =
      100 * value /
      value[year == START_YEAR][1]
  ) |>
  ungroup() |>
  mutate(
    variable = case_when(
      variable == "hospital_beds" ~
        "Krankenhausbetten je 1.000 Einwohner",
      
      variable == "health_exp_pc" ~
        "Reale Gesundheitsausgaben pro Kopf",
      
      TRUE ~ variable
    )
  )


fig1 <- ggplot(
  trend_data,
  aes(
    x = year,
    y = index_2015
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2
  ) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ variable,
    ncol = 1,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = START_YEAR:END_YEAR
  ) +
  labs(
    title =
      "Entwicklung von Krankenhausbettendichte und Gesundheitsausgaben",
    
    subtitle =
      "Ungewichtete OECD-Mittelwerte; Index 2015 = 100",
    
    x = NULL,
    
    y =
      "Index (2015 = 100)"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    plot.title =
      element_text(
        face = "bold"
      ),
    
    strip.text =
      element_text(
        face = "bold"
      ),
    
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )


print(fig1)

ggsave(
  filename =
    "figures/Abb1_Entwicklung_Betten_Gesundheitsausgaben.png",
  
  plot = fig1,
  
  width = 8,
  
  height = 7,
  
  dpi = 300
)


# ============================================================
# ABBILDUNG 2
#
# FE-RESIDUALISIERTER ZUSAMMENHANG
#
# Y:
#   log(reale Gesundheitsausgaben/Kopf)
#
# X:
#   Hospital beds je 1.000
#
# Herausgerechnet werden:
#   - Länder-FE
#   - Jahres-FE
#   - reales GDP/Kopf
#   - Anteil >=65 Jahre
#
# Dies visualisiert den partiellen Zusammenhang,
# der dem Koeffizienten hospital_beds_pc in Modell 1
# entspricht.
# ============================================================


# Modell nur für Outcome ohne Hospital beds
model_pc_without_beds <- feols(
  log(health_exp_pc_real_ppp) ~
    log(real_gdp_pc_ppp) +
    share_65plus |
    country +
    year,
  data = analysis
)


# Modell nur für Hospital beds
# mit denselben Kontrollvariablen und FE
model_beds_without_outcome <- feols(
  hospital_beds_pc ~
    log(real_gdp_pc_ppp) +
    share_65plus |
    country +
    year,
  data = analysis
)


# Residuen in analysis übernehmen
fe_plot_data <- analysis |>
  mutate(
    residual_beds =
      resid(
        model_beds_without_outcome
      ),
    
    residual_health_pc =
      resid(
        model_pc_without_beds
      )
  )


# Koeffizient und p-Wert aus dem Hauptmodell
beta_beds <-
  coef(
    model_pc
  )[["hospital_beds_pc"]]

p_beds <-
  summary(model_pc)$coeftable[
    "hospital_beds_pc",
    "Pr(>|t|)"
  ]


fig2 <- ggplot(
  fe_plot_data,
  aes(
    x = residual_beds,
    y = residual_health_pc
  )
) +
  geom_point(
    alpha = 0.55,
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title =
      "Partieller Zusammenhang zwischen Krankenhausbettendichte und Gesundheitsausgaben",
    
    subtitle =
      paste0(
        "Länder- und Jahres-FE sowie BIP/Kopf und Altersstruktur herausgerechnet; ",
        "β = ",
        round(beta_beds, 3),
        ", p = ",
        format.pval(
          p_beds,
          digits = 3,
          eps = 0.001
        )
      ),
    
    x =
      "FE-residualisierte Krankenhausbettendichte",
    
    y =
      "FE-residualisierte log. Gesundheitsausgaben/Kopf"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    plot.title =
      element_text(
        face = "bold"
      )
  )


print(fig2)

ggsave(
  filename =
    "figures/Abb2_FE_residualisierter_Zusammenhang.png",
  
  plot = fig2,
  
  width = 8,
  
  height = 6,
  
  dpi = 300
)


# ============================================================
# ABBILDUNG 3
#
# KOEFFIZIENT DER KRANKENHAUSBETTENDICHTE
# IN DEN BEIDEN HAUPTMODELLEN
#
# 95%-Konfidenzintervalle
#
# Achtung:
# Die Koeffizienten sind aufgrund unterschiedlicher
# abhängiger Variablen NICHT direkt in ihrer absoluten
# Größe miteinander vergleichbar.
# ============================================================


beta_pc <-
  coef(
    model_pc
  )[["hospital_beds_pc"]]

se_pc <-
  summary(model_pc)$coeftable[
    "hospital_beds_pc",
    "Std. Error"
  ]

beta_gdp <-
  coef(
    model_gdp
  )[["hospital_beds_pc"]]

se_gdp <-
  summary(model_gdp)$coeftable[
    "hospital_beds_pc",
    "Std. Error"
  ]


coef_data <- tibble(
  model = c(
    "Reale Gesundheitsausgaben/Kopf",
    "Gesundheitsausgaben in % BIP"
  ),
  
  estimate = c(
    beta_pc,
    beta_gdp
  ),
  
  se = c(
    se_pc,
    se_gdp
  )
) |>
  mutate(
    conf_low =
      estimate -
      1.96 * se,
    
    conf_high =
      estimate +
      1.96 * se
  )


fig3 <- ggplot(
  coef_data,
  aes(
    x = estimate,
    y = model
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  geom_errorbarh(
    aes(
      xmin = conf_low,
      xmax = conf_high
    ),
    height = 0.15
  ) +
  geom_point(
    size = 3
  ) +
  facet_wrap(
    ~ model,
    scales = "free_x"
  ) +
  labs(
    title =
      "Geschätzter Zusammenhang der Krankenhausbettendichte mit den Gesundheitsausgaben",
    
    subtitle =
      "Koeffizienten mit 95%-Konfidenzintervallen",
    
    x =
      "Koeffizient Krankenhausbetten je 1.000 Einwohner",
    
    y = NULL
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    plot.title =
      element_text(
        face = "bold"
      ),
    
    axis.text.y =
      element_blank(),
    
    axis.ticks.y =
      element_blank(),
    
    strip.text =
      element_text(
        face = "bold"
      )
  )


print(fig3)

ggsave(
  filename =
    "figures/Abb3_Koeffizient_Hospital_Beds.png",
  
  plot = fig3,
  
  width = 9,
  
  height = 5,
  
  dpi = 300
)


# ============================================================
# ENDE
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "GRAFIKEN ERSTELLT\n"
)

cat(
  "============================================\n"
)

cat(
  "Abb. 1: Entwicklung 2015-2023\n"
)

cat(
  "Abb. 2: FE-residualisierter Zusammenhang\n"
)

cat(
  "Abb. 3: Hospital-Beds-Koeffizient mit 95%-KI\n"
)

cat(
  "Ausgabeverzeichnis: figures/\n"
)

cat(
  "============================================\n"
)

# ============================================================
# OECD-PANELANALYSE 2015-2023
# DESKRIPTIVE GRAFIKEN / TRENDS / AMBULANTISIERUNG
#
# Voraussetzung:
#   analysis
#   model_pc
#   model_gdp
#
# aus dem Hauptskript müssen bereits vorhanden sein.
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

dir.create(
  "figures",
  showWarnings = FALSE
)


# ============================================================
# 1. TREND: HOSPITAL BEDS JE 1.000 EINWOHNER
#
# Ungewichteter Mittelwert über die Länder im jeweiligen Jahr.
# Zusätzlich Anzahl der verfügbaren Länder.
# ============================================================

trend_beds <- analysis |>
  group_by(year) |>
  summarise(
    mean_beds =
      mean(
        hospital_beds_pc,
        na.rm = TRUE
      ),
    
    median_beds =
      median(
        hospital_beds_pc,
        na.rm = TRUE
      ),
    
    n_countries =
      sum(
        !is.na(hospital_beds_pc)
      ),
    
    .groups = "drop"
  )


fig1 <- ggplot(
  trend_beds,
  aes(
    x = year,
    y = mean_beds
  )
) +
  geom_area(
    alpha = 0.18
  ) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point(
    size = 3
  ) +
  geom_line(
    aes(
      y = median_beds
    ),
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  geom_text(
    aes(
      label = round(mean_beds, 2)
    ),
    vjust = -0.8,
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = START_YEAR:END_YEAR
  ) +
  labs(
    title =
      "Krankenhausbettendichte in den untersuchten OECD-Ländern",
    
    subtitle =
      "Jährlicher ungewichteter Mittelwert; gestrichelte Linie = Median",
    
    x = NULL,
    
    y =
      "Krankenhausbetten je 1.000 Einwohner"
  ) +
  theme_minimal(
    base_size = 13
  ) +
  theme(
    plot.title =
      element_text(
        face = "bold",
        size = 16
      ),
    
    plot.subtitle =
      element_text(
        size = 11
      ),
    
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(fig1)

ggsave(
  "figures/Abb1_Trend_Hospital_Beds_2015_2023.png",
  fig1,
  width = 9,
  height = 6,
  dpi = 300
)


# ============================================================
# 2. TREND: REALE GESUNDHEITSAUSGABEN PRO KOPF
#
# Absolute Werte in PPP-Einheiten.
# ============================================================

trend_health_pc <- analysis |>
  group_by(year) |>
  summarise(
    mean_health_pc =
      mean(
        health_exp_pc_real_ppp,
        na.rm = TRUE
      ),
    
    median_health_pc =
      median(
        health_exp_pc_real_ppp,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


fig2 <- ggplot(
  trend_health_pc,
  aes(
    x = year,
    y = mean_health_pc
  )
) +
  geom_area(
    alpha = 0.18
  ) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point(
    size = 3
  ) +
  geom_line(
    aes(
      y = median_health_pc
    ),
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  geom_text(
    aes(
      label = format(
        round(mean_health_pc),
        big.mark = ".",
        scientific = FALSE
      )
    ),
    vjust = -0.8,
    size = 3.2
  ) +
  scale_x_continuous(
    breaks = START_YEAR:END_YEAR
  ) +
  labs(
    title =
      "Reale Gesundheitsausgaben pro Kopf",
    
    subtitle =
      "Jährlicher OECD-Mittelwert; gestrichelte Linie = Median",
    
    x = NULL,
    
    y =
      "Gesundheitsausgaben pro Kopf"
  ) +
  theme_minimal(
    base_size = 13
  ) +
  theme(
    plot.title =
      element_text(
        face = "bold",
        size = 16
      ),
    
    plot.subtitle =
      element_text(
        size = 11
      ),
    
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(fig2)

ggsave(
  "figures/Abb2_Trend_Gesundheitsausgaben_Kopf_2015_2023.png",
  fig2,
  width = 9,
  height = 6,
  dpi = 300
)


# ============================================================
# 3. TREND: GESUNDHEITSAUSGABEN IN % DES BIP
# ============================================================

trend_health_gdp <- analysis |>
  group_by(year) |>
  summarise(
    mean_health_gdp =
      mean(
        health_exp_gdp,
        na.rm = TRUE
      ),
    
    median_health_gdp =
      median(
        health_exp_gdp,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


fig3 <- ggplot(
  trend_health_gdp,
  aes(
    x = year,
    y = mean_health_gdp
  )
) +
  geom_area(
    alpha = 0.18
  ) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point(
    size = 3
  ) +
  geom_line(
    aes(
      y = median_health_gdp
    ),
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  geom_text(
    aes(
      label = paste0(
        round(
          mean_health_gdp,
          1
        ),
        "%"
      )
    ),
    vjust = -0.8,
    size = 3.2
  ) +
  scale_x_continuous(
    breaks = START_YEAR:END_YEAR
  ) +
  labs(
    title =
      "Gesundheitsausgaben als Anteil des BIP",
    
    subtitle =
      "Jährlicher OECD-Mittelwert; gestrichelte Linie = Median",
    
    x = NULL,
    
    y =
      "Gesundheitsausgaben (% des BIP)"
  ) +
  theme_minimal(
    base_size = 13
  ) +
  theme(
    plot.title =
      element_text(
        face = "bold",
        size = 16
      ),
    
    plot.subtitle =
      element_text(
        size = 11
      ),
    
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(fig3)

ggsave(
  "figures/Abb3_Trend_Gesundheitsausgaben_BIP_2015_2023.png",
  fig3,
  width = 9,
  height = 6,
  dpi = 300
)


# ============================================================
# 4. VERÄNDERUNG DER KRANKENHAUSBETTENDICHTE
#
# Vergleich 2015 vs. 2023.
#
# Länder mit fehlendem Wert in 2015 oder 2023 werden
# für diese Abbildung ausgeschlossen.
# ============================================================

country_change_beds <- analysis |>
  filter(
    year %in% c(
      START_YEAR,
      END_YEAR
    )
  ) |>
  select(
    country,
    year,
    hospital_beds_pc
  ) |>
  pivot_wider(
    names_from = year,
    values_from = hospital_beds_pc,
    names_prefix = "beds_"
  ) |>
  filter(
    !is.na(
      .data[[paste0("beds_", START_YEAR)]]
    ),
    !is.na(
      .data[[paste0("beds_", END_YEAR)]]
    )
  ) |>
  mutate(
    abs_change =
      .data[[paste0("beds_", END_YEAR)]] -
      .data[[paste0("beds_", START_YEAR)]],
    
    pct_change =
      100 *
      (
        .data[[paste0("beds_", END_YEAR)]] /
          .data[[paste0("beds_", START_YEAR)]] -
          1
      )
  ) |>
  arrange(
    pct_change
  )


fig4 <- ggplot(
  country_change_beds,
  aes(
    x = reorder(
      country,
      pct_change
    ),
    y = pct_change
  )
) +
  geom_col(
    aes(
      fill = pct_change < 0
    ),
    width = 0.72
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.8
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "TRUE" = "#D95F02",
      "FALSE" = "#1B9E77"
    ),
    guide = "none"
  ) +
  geom_text(
    aes(
      label = paste0(
        ifelse(
          pct_change > 0,
          "+",
          ""
        ),
        round(
          pct_change,
          1
        ),
        "%"
      )
    ),
    hjust = ifelse(
      country_change_beds$pct_change < 0,
      1.1,
      -0.1
    ),
    size = 3
  ) +
  labs(
    title =
      "Veränderung der Krankenhausbettendichte 2015–2023",
    
    subtitle =
      "Relative Veränderung je Land; Länder mit fehlenden Randjahren ausgeschlossen",
    
    x = NULL,
    
    y =
      "Veränderung gegenüber 2015 (%)"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    plot.title =
      element_text(
        face = "bold",
        size = 16
      ),
    
    axis.text.y =
      element_text(
        size = 9
      )
  )

print(fig4)

ggsave(
  "figures/Abb4_Veraenderung_Hospital_Beds_2015_2023.png",
  fig4,
  width = 9,
  height = 10,
  dpi = 300
)


# ============================================================
# 5. AMBULANTISIERUNG / STRUKTURELLE VERSCHIEBUNG
#
# X:
#   Veränderung Krankenhausbetten 2015-2023
#
# Y:
#   Veränderung reale Gesundheitsausgaben/Kopf 2015-2023
#
# Interpretation:
#   rechts/unten = steigende Ausgaben bei sinkender
#   stationärer Kapazitätsdichte.
#
# WICHTIG:
#   Dies ist ein DESKRIPTIVER Strukturindikator,
#   kein kausaler Nachweis von Ambulantisierung.
# ============================================================

country_change <- analysis |>
  filter(
    year %in% c(
      START_YEAR,
      END_YEAR
    )
  ) |>
  select(
    country,
    year,
    hospital_beds_pc,
    health_exp_pc_real_ppp
  ) |>
  pivot_wider(
    names_from = year,
    values_from = c(
      hospital_beds_pc,
      health_exp_pc_real_ppp
    ),
    names_sep = "_"
  ) |>
  filter(
    !is.na(
      .data[[paste0(
        "hospital_beds_pc_",
        START_YEAR
      )]]
    ),
    !is.na(
      .data[[paste0(
        "hospital_beds_pc_",
        END_YEAR
      )]]
    ),
    !is.na(
      .data[[paste0(
        "health_exp_pc_real_ppp_",
        START_YEAR
      )]]
    ),
    !is.na(
      .data[[paste0(
        "health_exp_pc_real_ppp_",
        END_YEAR
      )]]
    )
  ) |>
  mutate(
    
    beds_change_pct =
      100 *
      (
        .data[[paste0(
          "hospital_beds_pc_",
          END_YEAR
        )]] /
          .data[[paste0(
            "hospital_beds_pc_",
            START_YEAR
          )]] -
          1
      ),
    
    health_exp_change_pct =
      100 *
      (
        .data[[paste0(
          "health_exp_pc_real_ppp_",
          END_YEAR
        )]] /
          .data[[paste0(
            "health_exp_pc_real_ppp_",
            START_YEAR
          )]] -
          1
      )
  )


# Durchschnittliche Veränderungen
mean_beds_change <-
  mean(
    country_change$beds_change_pct,
    na.rm = TRUE
  )

mean_health_change <-
  mean(
    country_change$health_exp_change_pct,
    na.rm = TRUE
  )


fig5 <- ggplot(
  country_change,
  aes(
    x = beds_change_pct,
    y = health_exp_change_pct
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  geom_point(
    aes(
      size =
        abs(
          health_exp_change_pct
        ) +
        abs(
          beds_change_pct
        ),
      color =
        beds_change_pct < 0
    ),
    alpha = 0.8
  ) +
  geom_text(
    aes(
      label = country
    ),
    vjust = -0.8,
    size = 3
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "TRUE" = "#D95F02",
      "FALSE" = "#1B9E77"
    ),
    labels = c(
      "FALSE" = "Bettendichte steigt",
      "TRUE" = "Bettendichte sinkt"
    ),
    name = NULL
  ) +
  labs(
    title =
      "Gesundheitsausgaben steigen trotz sinkender stationärer Kapazität",
    
    subtitle =
      paste0(
        "Deskriptiver Vergleich der Veränderung 2015–2023; ",
        "Mittelwert Betten: ",
        round(
          mean_beds_change,
          1
        ),
        "%; Gesundheitsausgaben/Kopf: ",
        round(
          mean_health_change,
          1
        ),
        "%"
      ),
    
    x =
      "Veränderung Krankenhausbetten je 1.000 Einwohner (%)",
    
    y =
      "Veränderung reale Gesundheitsausgaben pro Kopf (%)"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    plot.title =
      element_text(
        face = "bold",
        size = 16
      ),
    
    plot.subtitle =
      element_text(
        size = 10.5
      )
  )


print(fig5)

ggsave(
  "figures/Abb5_Ambulantisierung_Betten_vs_Ausgaben.png",
  fig5,
  width = 9,
  height = 7,
  dpi = 300
)


# ============================================================
# 6. ZUSÄTZLICHE KENNZAHLEN FÜR DIE ARBEIT
# ============================================================

cat("\n============================================\n")
cat("DESKRIPTIVE VERÄNDERUNGEN 2015-2023\n")
cat("============================================\n")

cat(
  "Mittlere Veränderung Hospital Beds: ",
  round(
    mean_beds_change,
    2
  ),
  "%\n",
  sep = ""
)

cat(
  "Mittlere Veränderung reale Gesundheitsausgaben/Kopf: ",
  round(
    mean_health_change,
    2
  ),
  "%\n",
  sep = ""
)

cat(
  "Länder mit sinkender Bettendichte: ",
  sum(
    country_change$beds_change_pct < 0,
    na.rm = TRUE
  ),
  " von ",
  nrow(country_change),
  "\n",
  sep = ""
)

cat(
  "Länder mit steigender Bettendichte: ",
  sum(
    country_change$beds_change_pct > 0,
    na.rm = TRUE
  ),
  " von ",
  nrow(country_change),
  "\n",
  sep = ""
)

cat(
  "Länder mit sinkender Bettendichte UND steigenden Gesundheitsausgaben: ",
  sum(
    country_change$beds_change_pct < 0 &
      country_change$health_exp_change_pct > 0,
    na.rm = TRUE
  ),
  " von ",
  nrow(country_change),
  "\n",
  sep = ""
)

cat(
  "============================================\n"
)

# ==========================================================================================================================
# HC3 / AMBULANTISIERUNG
# Referenzländer:
# Finnland, Schweden, Deutschland, Japan,
# Italien, Südkorea, Estland, Rumänien
#
# Grafiken:
#   1. Entwicklung des Outpatient-Anteils 2015-2023
#   2. Veränderung des Outpatient-Anteils 2015-2023
#   3. Outpatient-Anteil vs. Veränderung Hospital Beds
#
# WICHTIG:
#   HBOUT = Outpatient
#   HBEDT = Inpatient
#
# Der Outpatient-Anteil wird hier definiert als:
#
#   HBOUT / (HBOUT + HBEDT)
#
# Damit wird gezielt die Verschiebung zwischen
# ambulantem und stationärem Modus innerhalb dieser
# beiden Versorgungsformen betrachtet.
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================
# HC3-ANALYSE / AMBULANTISIERUNG 2015-2023
#
# Referenzländer:
# Finnland
# Schweden
# Deutschland
# Japan
# Italien
# Südkorea
# Estland
# Rumänien
#
# Ziel:
#   Entwicklung des Anteils OUTPATIENT an
#   OUTPATIENT + INPATIENT
#
# Zusätzlich:
#   - Veränderung 2015 -> 2023
#   - farbliche Kodierung:
#       Grün = Outpatient nimmt zu
#       Rot   = Outpatient nimmt ab
#   - Referenzländer hervorgehoben
#   - Zusammenhang mit Krankenhausbettendichte
# ============================================================

# ============================================================
# OECD-SHA / HC3-ANALYSE
# AMBULANTISIERUNG 2015-2023
#
# Datensatz:
#   OECD_Referenzlaender
#
# Referenzländer:
#   Finnland
#   Schweden
#   Deutschland
#   Japan
#   Italien
#   Südkorea
#   Estland
#   Rumänien
#
# Ziel:
#
#   OUTPATIENT-ANTEIL =
#
#       Outpatient
#       --------------------------
#       Outpatient + Inpatient
#
# Farbcodierung:
#   Grün = Outpatient nimmt zu
#   Rot   = Outpatient nimmt ab
#
# Zusätzlich:
#   - Veränderung 2015 -> 2023
#   - Hospital Beds als struktureller Gegenindikator
#   - numerische Ergebnistabelle
#
# WICHTIG:
#   Keine Vermischung unterschiedlicher OECD-Dimensionen.
# ============================================================


# ============================================================
# 1. PAKETE
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)


# ============================================================
# 2. EINSTELLUNGEN
# ============================================================

START_YEAR <- 2015
END_YEAR   <- 2023

YEARS <- as.character(
  START_YEAR:END_YEAR
)


# ============================================================
# 3. REFERENZLÄNDER
# ============================================================

reference_countries <- c(
  "FIN",
  "SWE",
  "DEU",
  "JPN",
  "ITA",
  "KOR",
  "EST",
  "ROU"
)

country_labels <- c(
  FIN = "Finnland",
  SWE = "Schweden",
  DEU = "Deutschland",
  JPN = "Japan",
  ITA = "Italien",
  KOR = "Südkorea",
  EST = "Estland",
  ROU = "Rumänien"
)


# ============================================================
# 4. AUSGABEVERZEICHNIS
# ============================================================

dir.create(
  "figures",
  showWarnings = FALSE
)


# ============================================================
# 5. DATENSATZ PRÜFEN
# ============================================================

if (
  !exists(
    "OECD_Referenzlaender",
    inherits = TRUE
  )
) {
  
  stop(
    "OECD_Referenzlaender ist nicht in R vorhanden.",
    call. = FALSE
  )
}


# WICHTIG:
# Der Nutzer hat explizit:
# OECD_Referenzlaender <- data.frame(OECD_Referenzlaender)
# verwendet.
#
# Wir setzen deshalb sicherheitshalber erneut ein data.frame.

ref <- as.data.frame(
  OECD_Referenzlaender,
  stringsAsFactors = FALSE
)


# ============================================================
# 6. OECD-SHA-SPALTEN DIREKT IDENTIFIZIEREN
#
# Die tatsächlich importierte Tabelle hat laut print():
#
# (COMBINED_UNIT_MEASURE) Combined unit of measure
# (REF_AREA) Reference area
# (FUNCTION) Health function
# (MODE_PROVISION) Mode of provision
# (PROVIDER) Healthcare provider
#
# Die Codes und Bezeichnungen stehen jeweils in EINER
# gemeinsamen Spalte.
# ============================================================


# ============================================================
# 6.1 Spaltennamen prüfen
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "OECD-SHA-SPALTEN\n"
)

cat(
  "============================================\n"
)

print(
  names(ref)
)


# ============================================================
# 6.2 Exakte OECD-Spalten identifizieren
# ============================================================

combined_col <- names(ref)[
  grepl(
    "^\\(COMBINED_UNIT_MEASURE\\)",
    names(ref)
  )
][1]

ref_area_col <- names(ref)[
  grepl(
    "^\\(REF_AREA\\)",
    names(ref)
  )
][1]

function_col <- names(ref)[
  grepl(
    "^\\(FUNCTION\\)",
    names(ref)
  )
][1]

mode_col <- names(ref)[
  grepl(
    "^\\(MODE_PROVISION\\)",
    names(ref)
  )
][1]

provider_col <- names(ref)[
  grepl(
    "^\\(PROVIDER\\)",
    names(ref)
  )
][1]


# ============================================================
# 6.3 Sicherheitsprüfung
# ============================================================

identified_columns <- c(
  combined_col,
  ref_area_col,
  function_col,
  mode_col,
  provider_col
)

identified_roles <- c(
  "Combined unit",
  "REF_AREA",
  "FUNCTION",
  "MODE_PROVISION",
  "PROVIDER"
)

column_check <- data.frame(
  role = identified_roles,
  column = identified_columns,
  stringsAsFactors = FALSE
)

cat(
  "\n============================================\n"
)

cat(
  "ERKANNTE OECD-SHA-SPALTEN\n"
)

cat(
  "============================================\n"
)

print(
  column_check
)


if (
  anyNA(identified_columns)
) {
  
  stop(
    paste0(
      "Mindestens eine notwendige OECD-SHA-Spalte ",
      "konnte nicht erkannt werden.\n\n",
      paste(
        names(ref),
        collapse = " | "
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 7. JAHRESSPALTEN PRÜFEN
# ============================================================

year_cols <- YEARS[
  YEARS %in% names(ref)
]


if (
  length(year_cols) != length(YEARS)
) {
  
  # Falls R X2015 usw. importiert hätte
  names(ref) <- sub(
    "^X(20[0-9]{2})$",
    "\\1",
    names(ref)
  )
  
  year_cols <- YEARS[
    YEARS %in% names(ref)
  ]
}


if (
  length(year_cols) != length(YEARS)
) {
  
  stop(
    paste0(
      "Nicht alle Jahreswerte 2015-2023 vorhanden.\n",
      "Vorhanden: ",
      paste(
        year_cols,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 8. TEXT NORMALISIEREN
# ============================================================

normalise_text <- function(x) {
  
  x <- as.character(
    x
  )
  
  x <- trimws(
    x
  )
  
  x <- str_squish(
    x
  )
  
  x[
    is.na(x) |
      x %in% c(
        "",
        "..",
        "...",
        "NA",
        "N/A"
      )
  ] <- NA_character_
  
  x
}


# ============================================================
# 9. OECD-ZAHLEN PARSEN
# ============================================================

parse_oecd_local <- function(x) {
  
  if (
    is.numeric(x)
  ) {
    
    return(
      as.numeric(x)
    )
  }
  
  x <- normalise_text(
    x
  )
  
  x <- gsub(
    "[A-Za-z*†‡]",
    "",
    x
  )
  
  x <- gsub(
    "\\s+",
    "",
    x
  )
  
  # 4.030,4 -> 4030.4
  both <- !is.na(x) &
    grepl(
      "\\.",
      x
    ) &
    grepl(
      ",",
      x,
      fixed = TRUE
    )
  
  x[both] <- gsub(
    ".",
    "",
    x[both],
    fixed = TRUE
  )
  
  x[both] <- gsub(
    ",",
    ".",
    x[both],
    fixed = TRUE
  )
  
  # 766,6 -> 766.6
  comma_only <- !is.na(x) &
    grepl(
      ",",
      x,
      fixed = TRUE
    ) &
    !grepl(
      "\\.",
      x
    )
  
  x[comma_only] <- gsub(
    ",",
    ".",
    x[comma_only],
    fixed = TRUE
  )
  
  suppressWarnings(
    as.numeric(x)
  )
}


# ============================================================
# 10. BASISVARIABLEN ANLEGEN
# ============================================================

ref <- ref |>
  mutate(
    
    combined_text =
      normalise_text(
        .data[[combined_col]]
      ),
    
    ref_area_text =
      normalise_text(
        .data[[ref_area_col]]
      ),
    
    function_text =
      normalise_text(
        .data[[function_col]]
      ),
    
    mode_text =
      normalise_text(
        .data[[mode_col]]
      ),
    
    provider_text =
      normalise_text(
        .data[[provider_col]]
      )
  )


# ============================================================
# 11. LANDESCODE AUS REF_AREA
#
# Beispiel:
#   "(EST) Estonia" -> "EST"
# ============================================================

ref <- ref |>
  mutate(
    
    country =
      ifelse(
        grepl(
          "^\\([A-Z]{3}\\)",
          ref_area_text
        ),
        
        sub(
          "^\\(([A-Z]{3})\\).*",
          "\\1",
          ref_area_text
        ),
        
        NA_character_
      )
  )


# ============================================================
# 12. MODE DIREKT AUS MODE_PROVISION
#
# Beispiel:
#   "(HBOUT) Outpatient" -> outpatient
#   "(HBEDT) Inpatient"  -> inpatient
# ============================================================

ref <- ref |>
  mutate(
    
    mode = case_when(
      
      grepl(
        "^\\(HBOUT\\)",
        mode_text,
        ignore.case = TRUE
      ) ~
        "outpatient",
      
      grepl(
        "^\\(HBEDT\\)",
        mode_text,
        ignore.case = TRUE
      ) ~
        "inpatient",
      
      TRUE ~
        NA_character_
    )
  )


# ============================================================
# 13. PROVIDER = TOTAL
#
# In deinem Datensatz:
#   "(_T) Total"
#
# Wir verwenden den CODE direkt und nicht einfach
# grepl("Total"), damit Unterkategorien nicht eingeschlossen
# werden.
# ============================================================

ref <- ref |>
  mutate(
    
    provider_total =
      grepl(
        "^\\(_T\\)",
        provider_text
      )
  )


# ============================================================
# 14. HC3 FUNKTION
#
# Es wird explizit der OECD-Code HC3 gesucht.
# ============================================================

ref <- ref |>
  mutate(
    
    function_HC3 =
      grepl(
        "^\\(HC3\\)",
        function_text,
        ignore.case = TRUE
      )
  )

# ============================================================
# 15. HC3-DATENBASIS MIT DER ZUSÄTZLICHEN DIMENSION "J"
#
# WICHTIG:
# Die importierte OECD-Tabelle enthält neben
#   COMBINED_UNIT_MEASURE
#   REF_AREA
#   FUNCTION
#   MODE_PROVISION
#   PROVIDER
#
# noch die zusätzliche Spalte:
#   J
#
# Diese Dimension darf NICHT ignoriert werden.
# ============================================================


# ============================================================
# 15.1 J-SPALTE PRÜFEN
# ============================================================

if (
  !"J" %in% names(ref)
) {
  
  stop(
    paste0(
      "Die zusätzliche OECD-Dimension 'J' wurde nicht gefunden.\n",
      "Vorhandene Spalten:\n",
      paste(
        names(ref),
        collapse = " | "
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 15.2 J NORMALISIEREN
# ============================================================

ref$J_text <- normalise_text(
  ref$J
)


# ============================================================
# 16. VORFILTERUNG DER RELEVANTEN HC3-DATEN
#
# Noch KEINE Auswahl der J-Ausprägung.
# ============================================================

hc3_base <- ref |>
  filter(
    
    country %in%
      reference_countries,
    
    function_HC3,
    
    provider_total,
    
    !is.na(mode),
    
    combined_text ==
      selected_unit
  )


if (
  nrow(hc3_base) == 0
) {
  
  stop(
    "Nach der bisherigen Filterung wurden keine HC3-Daten gefunden.",
    call. = FALSE
  )
}


# ============================================================
# 17. ALLE AUSPRÄGUNGEN VON J ANZEIGEN
# ============================================================

j_values <- hc3_base |>
  distinct(
    J_text
  ) |>
  arrange(
    J_text
  )


cat(
  "\n============================================\n"
)

cat(
  "HC3: AUSPRÄGUNGEN DER DIMENSION J\n"
)

cat(
  "============================================\n"
)

print(
  as_tibble(
    j_values
  ),
  n = Inf
)


# ============================================================
# 18. HC3 LONG-FORMAT
#
# J bleibt vollständig erhalten.
# ============================================================

hc3_long_all <- hc3_base |>
  select(
    country,
    J_text,
    mode,
    all_of(year_cols)
  ) |>
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "value"
  ) |>
  mutate(
    
    year =
      as.integer(
        year
      ),
    
    value =
      parse_oecd_local(
        value
      )
  ) |>
  filter(
    !is.na(value)
  )


# ============================================================
# 19. DUPLIKATE INNERHALB VON
#
# country × J × year × mode
#
# entfernen wir NICHT automatisch.
#
# Falls es sie gibt, liegt noch eine weitere Dimension vor.
# ============================================================

dup_check_all <- hc3_long_all |>
  count(
    country,
    J_text,
    year,
    mode
  ) |>
  filter(
    n > 1
  )


if (
  nrow(dup_check_all) > 0
) {
  
  cat(
    "\n============================================\n"
  )
  
  cat(
    "WARNUNG: J REICHT NOCH NICHT ZUR EINDÄUTIGKEIT\n"
  )
  
  cat(
    "============================================\n"
  )
  
  cat(
    "Es existieren weiterhin Mehrfachwerte innerhalb ",
    "country × J × year × mode.\n",
    sep = ""
  )
  
  print(
    as_tibble(
      dup_check_all
    ),
    n = Inf
  )
  
  # Keine Vermischung verschiedener Werte.
  # Stattdessen werden nur exakte identische Duplikate entfernt.
  hc3_long_all <- hc3_long_all |>
    distinct(
      country,
      J_text,
      year,
      mode,
      value
    )
  
}


# ============================================================
# 20. ZÄHLEN:
# WELCHE J-AUSPRÄGUNG HAT DIE BESTE
# INPATIENT/OUTPATIENT-ABDECKUNG?
# ============================================================

j_coverage <- hc3_long_all |>
  group_by(
    J_text,
    country,
    year
  ) |>
  summarise(
    
    has_inpatient =
      any(
        mode == "inpatient"
      ),
    
    has_outpatient =
      any(
        mode == "outpatient"
      ),
    
    .groups = "drop"
  ) |>
  group_by(
    J_text
  ) |>
  summarise(
    
    complete_pairs =
      sum(
        has_inpatient &
          has_outpatient
      ),
    
    countries =
      n_distinct(
        country[
          has_inpatient &
            has_outpatient
        ]
      ),
    
    years =
      n_distinct(
        year[
          has_inpatient &
            has_outpatient
        ]
      ),
    
    .groups = "drop"
  ) |>
  arrange(
    desc(
      complete_pairs
    ),
    desc(
      countries
    ),
    desc(
      years
    )
  )


# ============================================================
# 21. J-COVERAGE AUSGEBEN
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "HC3: J-AUSPRÄGUNGEN NACH VOLLSTÄNDIGKEIT\n"
)

cat(
  "============================================\n"
)

print(
  as_tibble(
    j_coverage
  ),
  n = Inf
)


# ============================================================
# 22. J-AUSPRÄGUNG AUSWÄHLEN
#
# Primäres Kriterium:
#   möglichst viele vollständige
#   INPATIENT + OUTPATIENT Land-Jahr-Paare
#
# Sekundär:
#   Anzahl Länder
#   Anzahl Jahre
#
# Falls mehrere J-Werte exakt gleich gut sind,
# wird KEINE willkürliche Auswahl vorgenommen.
# ============================================================

if (
  nrow(j_coverage) == 0
) {
  
  stop(
    "Keine J-Ausprägung mit Inpatient- und Outpatient-Werten gefunden.",
    call. = FALSE
  )
}


best_complete_pairs <-
  j_coverage$complete_pairs[1]


best_j_candidates <- j_coverage |>
  filter(
    complete_pairs ==
      best_complete_pairs,
    
    countries ==
      max(
        countries[
          complete_pairs ==
            best_complete_pairs
        ]
      ),
    
    years ==
      max(
        years[
          complete_pairs ==
            best_complete_pairs
        ]
      )
  )


# ============================================================
# 23. BEI MEHREREN GLEICH GUTEN J-WERTEN:
#     TOTAL BEVORZUGEN
# ============================================================

if (
  nrow(best_j_candidates) > 1
) {
  
  total_candidates <- best_j_candidates |>
    filter(
      !is.na(J_text),
      grepl(
        "Total|_T",
        J_text,
        ignore.case = TRUE
      )
    )
  
  if (
    nrow(total_candidates) == 1
  ) {
    
    selected_J <-
      total_candidates$J_text[1]
    
  } else {
    
    # Falls ein leerer J-Wert existiert,
    # diesen als Gesamt-/Hauptreihe bevorzugen.
    blank_candidate <- best_j_candidates |>
      filter(
        is.na(J_text) |
          J_text == ""
      )
    
    if (
      nrow(blank_candidate) == 1
    ) {
      
      selected_J <-
        blank_candidate$J_text[1]
      
    } else {
      
      cat(
        "\n============================================\n"
      )
      
      cat(
        "MEHRERE GLEICH GUTE J-AUSPRÄGUNGEN\n"
      )
      
      cat(
        "============================================\n"
      )
      
      print(
        as_tibble(
          best_j_candidates
        ),
        n = Inf
      )
      
      stop(
        paste0(
          "Es existieren mehrere gleich gut geeignete ",
          "J-Ausprägungen. Keine automatische Auswahl ",
          "vorgenommen, um unterschiedliche OECD-Reihen ",
          "nicht zu vermischen."
        ),
        call. = FALSE
      )
    }
  }
  
} else {
  
  selected_J <-
    best_j_candidates$J_text[1]
}


# ============================================================
# 24. AUSGEWÄHLTE J-AUSPRÄGUNG AUSGEBEN
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "AUSGEWÄHLTE J-AUSPRÄGUNG\n"
)

cat(
  "============================================\n"
)

cat(
  "J = ",
  ifelse(
    is.na(selected_J),
    "<NA>",
    selected_J
  ),
  "\n",
  sep = ""
)

cat(
  "Vollständige Land-Jahr-Paare: ",
  best_complete_pairs,
  "\n",
  sep = ""
)


# ============================================================
# 25. FINALE HC3-AUSWAHL
# ============================================================

if (
  is.na(selected_J)
) {
  
  hc3_final <- hc3_long_all |>
    filter(
      is.na(J_text)
    )
  
} else {
  
  hc3_final <- hc3_long_all |>
    filter(
      J_text ==
        selected_J
    )
}


# ============================================================
# 26. FINALE DUPLIKATKONTROLLE
# ============================================================

final_dup_check <- hc3_final |>
  count(
    country,
    year,
    mode
  ) |>
  filter(
    n > 1
  )


if (
  nrow(final_dup_check) > 0
) {
  
  cat(
    "\n============================================\n"
  )
  
  cat(
    "HC3-SERIE NOCH NICHT EINDEUTIG\n"
  )
  
  cat(
    "============================================\n"
  )
  
  print(
    as_tibble(
      final_dup_check
    ),
    n = Inf
  )
  
  stop(
    "Auch nach Berücksichtigung von J sind einzelne Land-Jahr-Mode-Kombinationen mehrfach vorhanden. Keine automatische Aggregation durchgeführt.",
    call. = FALSE
  )
}


# ============================================================
# 27. INPATIENT / OUTPATIENT SEPARIEREN
# ============================================================

hc3_inpatient <- hc3_final |>
  filter(
    mode ==
      "inpatient"
  ) |>
  select(
    country,
    year,
    inpatient =
      value
  )


hc3_outpatient <- hc3_final |>
  filter(
    mode ==
      "outpatient"
  ) |>
  select(
    country,
    year,
    outpatient =
      value
  )

# ============================================================
# 28. SENSITIVITÄTSANALYSE
# ============================================================

# ============================================================
# 33. SENSITIVITÄTSANALYSE OHNE PANDEMIEJAHRE
#     Ausschluss 2020 und 2021
# ============================================================

library(dplyr)
library(fixest)

# ------------------------------------------------------------
# 33a. Sensitivitätsstichprobe
# ------------------------------------------------------------

analysis_no_covid <- analysis |>
  filter(!year %in% c(2020, 2021)) |>
  arrange(country, year)

cat("\n============================================\n")
cat("SENSITIVITÄTSANALYSE: OHNE 2020 UND 2021\n")
cat("============================================\n")

cat("Länder:", n_distinct(analysis_no_covid$country), "\n")
cat("Beobachtungen:", nrow(analysis_no_covid), "\n")
cat(
  "Verbleibende Jahre:",
  paste(sort(unique(analysis_no_covid$year)), collapse = ", "),
  "\n"
)

# Prüfen, ob ausreichend Beobachtungen verbleiben
stopifnot(nrow(analysis_no_covid) > 0)

# ------------------------------------------------------------
# 33b. Modell 1:
# Logarithmierte reale Gesundheitsausgaben pro Kopf
# ------------------------------------------------------------

model_pc_no_covid <- feols(
  log(health_exp_pc_real_ppp) ~
    hospital_beds_pc +
    log(real_gdp_pc_ppp) +
    share_65plus |
    country + year,
  data = analysis_no_covid,
  cluster = ~country
)

# ------------------------------------------------------------
# 33c. Modell 2:
# Gesundheitsausgaben in % des BIP
# ------------------------------------------------------------

model_gdp_no_covid <- feols(
  health_exp_gdp ~
    hospital_beds_pc +
    log(real_gdp_pc_ppp) +
    share_65plus |
    country + year,
  data = analysis_no_covid,
  cluster = ~country
)

# ------------------------------------------------------------
# 33d. Ergebnisse ausgeben
# ------------------------------------------------------------

cat("\n--- Modell 1: Reale Ausgaben pro Kopf ---\n")
print(summary(model_pc_no_covid))

cat("\n--- Modell 2: Gesundheitsausgaben in % BIP ---\n")
print(summary(model_gdp_no_covid))

# ------------------------------------------------------------
# 33e. Vergleich mit der Hauptanalyse
# ------------------------------------------------------------

sensitivity_table <- etable(
  model_pc,
  model_pc_no_covid,
  model_gdp,
  model_gdp_no_covid,
  headers = c(
    "Pro Kopf: Hauptanalyse",
    "Pro Kopf: ohne Pandemie",
    "% BIP: Hauptanalyse",
    "% BIP: ohne Pandemie"
  ),
  keep = "hospital_beds_pc|log\\(real_gdp_pc_ppp\\)|share_65plus"
)

cat("\n--- Modellvergleich ---\n")
print(sensitivity_table)

# ------------------------------------------------------------
# 33f. Ergebnisse exportieren
# ------------------------------------------------------------

capture.output(
  sensitivity_table,
  file = "OECD_sensitivity_no_covid_2015_2023.txt"
)

write.csv(
  analysis_no_covid,
  "OECD_analysis_no_covid_2015_2023.csv",
  row.names = FALSE,
  na = ""
)

saveRDS(
  list(
    model_pc_main = model_pc,
    model_gdp_main = model_gdp,
    model_pc_no_covid = model_pc_no_covid,
    model_gdp_no_covid = model_gdp_no_covid,
    analysis_no_covid = analysis_no_covid
  ),
  "OECD_sensitivity_no_covid_2015_2023.rds"
)


# ============================================================
# 34. ZUSATZANALYSE: NUR VORPANDEMIE 2015-2019
# ============================================================

analysis_pre_covid <- analysis |>
  filter(year <= 2019) |>
  arrange(country, year)

model_pc_pre_covid <- feols(
  log(health_exp_pc_real_ppp) ~
    hospital_beds_pc +
    log(real_gdp_pc_ppp) +
    share_65plus |
    country + year,
  data = analysis_pre_covid,
  cluster = ~country
)

model_gdp_pre_covid <- feols(
  health_exp_gdp ~
    hospital_beds_pc +
    log(real_gdp_pc_ppp) +
    share_65plus |
    country + year,
  data = analysis_pre_covid,
  cluster = ~country
)

cat("\n--- Vorpandemie: Ausgaben pro Kopf ---\n")
print(summary(model_pc_pre_covid))

cat("\n--- Vorpandemie: Ausgaben in % BIP ---\n")
print(summary(model_gdp_pre_covid))

pre_covid_table <- etable(
  model_pc,
  model_pc_no_covid,
  model_pc_pre_covid,
  model_gdp,
  model_gdp_no_covid,
  model_gdp_pre_covid,
  headers = c(
    "Pro Kopf: 2015-2023",
    "Pro Kopf: ohne 2020-2021",
    "Pro Kopf: 2015-2019",
    "% BIP: 2015-2023",
    "% BIP: ohne 2020-2021",
    "% BIP: 2015-2019"
  ),
  keep = "hospital_beds_pc|log\\(real_gdp_pc_ppp\\)|share_65plus"
)

print(pre_covid_table)

capture.output(
  pre_covid_table,
  file = "OECD_sensitivity_all_periods_2015_2023.txt"
)

# ============================================================
# 28. EXPLIZITE EXISTENZPRÜFUNG
# ============================================================

if (
  nrow(hc3_inpatient) == 0
) {
  
  stop(
    "Keine INPATIENT-Werte in der ausgewählten J-Datenreihe.",
    call. = FALSE
  )
}

if (
  nrow(hc3_outpatient) == 0
) {
  
  stop(
    "Keine OUTPATIENT-Werte in der ausgewählten J-Datenreihe.",
    call. = FALSE
  )
}


# ============================================================
# 29. INPATIENT + OUTPATIENT
# ============================================================

hc3_share <- hc3_inpatient |>
  inner_join(
    hc3_outpatient,
    by = c(
      "country",
      "year"
    )
  ) |>
  filter(
    !is.na(inpatient),
    !is.na(outpatient)
  ) |>
  mutate(
    
    hc3_total =
      inpatient +
      outpatient,
    
    outpatient_share =
      outpatient /
      hc3_total,
    
    country_name =
      unname(
        country_labels[
          country
        ]
      )
  ) |>
  filter(
    hc3_total > 0,
    is.finite(
      outpatient_share
    )
  ) |>
  arrange(
    country,
    year
  )


# ============================================================
# 30. FINALE HC3-DATENBASIS PRÜFEN
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "FINALE HC3-DATENBASIS\n"
)

cat(
  "============================================\n"
)

cat(
  "Beobachtungen: ",
  nrow(hc3_share),
  "\n",
  sep = ""
)

cat(
  "Länder: ",
  n_distinct(
    hc3_share$country
  ),
  "\n",
  sep = ""
)

cat(
  "Jahre: ",
  n_distinct(
    hc3_share$year
  ),
  "\n",
  sep = ""
)

cat(
  "Inpatient: ",
  nrow(
    hc3_inpatient
  ),
  "\n",
  sep = ""
)

cat(
  "Outpatient: ",
  nrow(
    hc3_outpatient
  ),
  "\n",
  sep = ""
)


print(
  as_tibble(
    hc3_share
  ),
  n = Inf
)

# ============================================================
# 31. VERÄNDERUNG 2015 -> 2023
# ============================================================

hc3_change <- hc3_share |>
  filter(
    year %in% c(
      START_YEAR,
      END_YEAR
    )
  ) |>
  select(
    country,
    country_name,
    year,
    outpatient_share,
    outpatient,
    inpatient
  ) |>
  pivot_wider(
    names_from = year,
    values_from = c(
      outpatient_share,
      outpatient,
      inpatient
    ),
    names_glue = "{.value}_{year}"
  )


# ============================================================
# 32. NUR LÄNDER MIT BEIDEN RANDJAHREN
# ============================================================

required_change_cols <- c(
  paste0(
    "outpatient_share_",
    START_YEAR
  ),
  paste0(
    "outpatient_share_",
    END_YEAR
  ),
  paste0(
    "outpatient_",
    START_YEAR
  ),
  paste0(
    "outpatient_",
    END_YEAR
  )
)

hc3_change <- hc3_change |>
  filter(
    if_all(
      all_of(required_change_cols),
      ~ !is.na(.x)
    )
  )


# ============================================================
# 33. VERÄNDERUNG BERECHNEN
# ============================================================

share2015_col <- paste0(
  "outpatient_share_",
  START_YEAR
)

share2023_col <- paste0(
  "outpatient_share_",
  END_YEAR
)

out2015_col <- paste0(
  "outpatient_",
  START_YEAR
)

out2023_col <- paste0(
  "outpatient_",
  END_YEAR
)

hc3_change <- hc3_change |>
  mutate(
    
    share_2015_pct =
      100 * .data[[share2015_col]],
    
    share_2023_pct =
      100 * .data[[share2023_col]],
    
    change_share_pp =
      share_2023_pct -
      share_2015_pct,
    
    change_outpatient_pct =
      ifelse(
        .data[[out2015_col]] > 0,
        
        100 * (
          .data[[out2023_col]] /
            .data[[out2015_col]] -
            1
        ),
        
        NA_real_
      ),
    
    trend = case_when(
      
      change_share_pp > 0 ~
        "Outpatient nimmt zu",
      
      change_share_pp < 0 ~
        "Outpatient nimmt ab",
      
      TRUE ~
        "Unverändert"
    ),
    
    reference = TRUE
  )


# ============================================================
# 34. ERGEBNISTABELLE
# ============================================================

hc3_results_table <- hc3_change |>
  select(
    
    country,
    country_name,
    
    outpatient_share_2015 =
      share_2015_pct,
    
    outpatient_share_2023 =
      share_2023_pct,
    
    change_share_pp,
    
    outpatient_2015 =
      all_of(out2015_col),
    
    outpatient_2023 =
      all_of(out2023_col),
    
    change_outpatient_pct,
    
    trend
  ) |>
  arrange(
    desc(change_share_pp)
  )


cat(
  "\n============================================\n"
)

cat(
  "HC3 / AMBULANTISIERUNG – ERGEBNISTABELLE\n"
)

cat(
  "============================================\n"
)

print(
  as_tibble(
    hc3_results_table
  ),
  n = Inf
)


# ============================================================
# 35. WELCHE REFERENZLÄNDER FEHLEN?
#
# Sehr wichtig für die Dokumentation.
# ============================================================

available_reference_countries <- unique(
  hc3_change$country
)

missing_reference_countries <- setdiff(
  reference_countries,
  available_reference_countries
)

cat(
  "\n============================================\n"
)

cat(
  "REFERENZLÄNDER OHNE VOLLSTÄNDIGEN 2015/2023-VERGLEICH\n"
)

cat(
  "============================================\n"
)

if (
  length(missing_reference_countries) > 0
) {
  
  missing_reference_table <- tibble(
    
    country =
      missing_reference_countries,
    
    country_name =
      unname(
        country_labels[
          missing_reference_countries
        ]
      )
    
  )
  
  print(
    missing_reference_table,
    n = Inf
  )
  
} else {
  
  cat(
    "Alle Referenzländer verfügbar.\n"
  )
}


# ============================================================
# 36. GRAFIK 1
#
# OUTPATIENT-ANTEIL 2015 -> 2023
#
# Grün = Zunahme
# Rot   = Abnahme
# ============================================================

plot_slope <- hc3_change |>
  select(
    
    country,
    country_name,
    trend,
    
    share_2015_pct,
    
    share_2023_pct
    
  ) |>
  pivot_longer(
    
    cols = c(
      share_2015_pct,
      share_2023_pct
    ),
    
    names_to = "time",
    
    values_to = "share"
    
  ) |>
  mutate(
    
    year = case_when(
      
      time ==
        "share_2015_pct" ~
        START_YEAR,
      
      time ==
        "share_2023_pct" ~
        END_YEAR,
      
      TRUE ~
        NA_real_
    )
  )


fig1 <- ggplot(
  plot_slope,
  aes(
    x = year,
    y = share,
    group = country_name,
    color = trend
  )
) +
  
  geom_line(
    linewidth = 1.4
  ) +
  
  geom_point(
    size = 3.5
  ) +
  
  geom_text(
    data =
      plot_slope |>
      filter(
        year == START_YEAR
      ),
    aes(
      label =
        paste0(
          country_name,
          " ",
          round(
            share,
            1
          ),
          "%"
        )
    ),
    hjust = 1.05,
    size = 3.4,
    show.legend = FALSE
  ) +
  
  geom_text(
    data =
      plot_slope |>
      filter(
        year == END_YEAR
      ),
    aes(
      label =
        paste0(
          country_name,
          " ",
          round(
            share,
            1
          ),
          "%"
        )
    ),
    hjust = -0.05,
    size = 3.4,
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      
      "Outpatient nimmt zu" =
        "#1B9E77",
      
      "Outpatient nimmt ab" =
        "#D95F02",
      
      "Unverändert" =
        "#7570B3"
      
    )
  ) +
  
  scale_x_continuous(
    
    breaks = c(
      START_YEAR,
      END_YEAR
    ),
    
    expand =
      expansion(
        mult = c(
          0.20,
          0.20
        )
      )
  ) +
  
  scale_y_continuous(
    
    labels =
      function(x) {
        paste0(
          round(
            x,
            0
          ),
          "%"
        )
      }
  ) +
  
  labs(
    
    title =
      "Entwicklung des ambulanten Anteils der HC3-Ausgaben",
    
    subtitle =
      "Veränderung zwischen 2015 und 2023",
    
    x = NULL,
    
    y =
      "Outpatient-Anteil an Inpatient + Outpatient",
    
    color = NULL
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    
    plot.title =
      element_text(
        face = "bold",
        size = 16
      ),
    
    plot.subtitle =
      element_text(
        size = 11
      ),
    
    legend.position =
      "bottom",
    
    panel.grid.minor =
      element_blank()
  )


print(
  fig1
)


ggsave(
  "figures/Abb_HC3_Outpatient_Trend_2015_2023.png",
  fig1,
  width = 10,
  height = 7,
  dpi = 300
)


# ============================================================
# 37. GRAFIK 2
#
# VERÄNDERUNG OUTPATIENT-ANTEIL IN PROZENTPUNKTEN
# ============================================================

hc3_change_plot <- hc3_results_table |>
  arrange(
    change_share_pp
  ) |>
  mutate(
    
    country_name =
      factor(
        country_name,
        levels =
          country_name
      )
  )


fig2 <- ggplot(
  hc3_change_plot,
  aes(
    x = country_name,
    y = change_share_pp,
    fill = trend
  )
) +
  
  geom_hline(
    yintercept = 0,
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  
  geom_col(
    width = 0.70
  ) +
  
  geom_text(
    aes(
      label =
        paste0(
          ifelse(
            change_share_pp > 0,
            "+",
            ""
          ),
          round(
            change_share_pp,
            1
          ),
          " pp"
        )
    ),
    hjust =
      ifelse(
        hc3_change_plot$change_share_pp >= 0,
        -0.15,
        1.15
      ),
    size = 3.5
  ) +
  
  scale_fill_manual(
    values = c(
      
      "Outpatient nimmt zu" =
        "#1B9E77",
      
      "Outpatient nimmt ab" =
        "#D95F02",
      
      "Unverändert" =
        "#7570B3"
      
    )
  ) +
  
  coord_flip() +
  
  labs(
    
    title =
      "Veränderung des ambulanten Anteils 2015–2023",
    
    subtitle =
      "Positive Werte = zunehmende ambulante Gewichtung",
    
    x = NULL,
    
    y =
      "Veränderung des Outpatient-Anteils (Prozentpunkte)",
    
    fill = NULL
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    
    plot.title =
      element_text(
        face = "bold",
        size = 16
      ),
    
    plot.subtitle =
      element_text(
        size = 11
      ),
    
    legend.position =
      "bottom"
  )


print(
  fig2
)


ggsave(
  "figures/Abb_HC3_Outpatient_Veraenderung_2015_2023.png",
  fig2,
  width = 10,
  height = 7,
  dpi = 300
)


# ============================================================
# 38. HOSPITAL BEDS 2015 -> 2023
# ============================================================

if (
  exists(
    "analysis",
    inherits = TRUE
  )
) {
  
  beds_reference <- analysis |>
    filter(
      country %in% reference_countries,
      year %in% c(
        START_YEAR,
        END_YEAR
      )
    ) |>
    select(
      country,
      year,
      hospital_beds_pc
    ) |>
    pivot_wider(
      names_from = year,
      values_from = hospital_beds_pc,
      names_prefix = "beds_"
    )
  
  # Dynamische Spaltennamen
  beds_2015_col <- paste0(
    "beds_",
    START_YEAR
  )
  
  beds_2023_col <- paste0(
    "beds_",
    END_YEAR
  )
  
  # Nur Länder mit beiden Zeitpunkten behalten
  if (
    all(
      c(
        beds_2015_col,
        beds_2023_col
      ) %in%
      names(beds_reference)
    )
  ) {
    
    beds_reference <- beds_reference |>
      filter(
        !is.na(.data[[beds_2015_col]]),
        !is.na(.data[[beds_2023_col]])
      ) |>
      mutate(
        change_beds_abs =
          .data[[beds_2023_col]] -
          .data[[beds_2015_col]],
        
        change_beds_pct =
          100 *
          (
            .data[[beds_2023_col]] /
              .data[[beds_2015_col]] -
              1
          ),
        
        direction =
          case_when(
            change_beds_abs < 0 ~
              "Rückgang",
            change_beds_abs > 0 ~
              "Zunahme",
            TRUE ~
              "Unverändert"
          )
      ) |>
      arrange(
        change_beds_pct
      )
    
    cat(
      "\n============================================================\n"
    )
    cat(
      "HOSPITAL BEDS 2015 -> 2023\n"
    )
    cat(
      "============================================================\n"
    )
    
    print(
      as_tibble(
        beds_reference
      ),
      n = Inf
    )
    
  } else {
    
    warning(
      "Die Spalten für 2015 und/oder 2023 fehlen in beds_reference."
    )
  }
  
} else {
  
  warning(
    "Objekt 'analysis' nicht vorhanden."
  )
}
    
# ============================================================
# 39. GRAFIK 3
#
# AMBULANTISIERUNG VS.
# STATIONÄRE KAPAZITÄT
# ============================================================

ambulant_plot <- hc3_results_table |>
  select(
    country,
    country_name,
    change_share_pp,
    trend
  ) |>
  inner_join(
    beds_reference |>
      select(
        country,
        beds_2015,
        beds_2023,
        change_beds_abs,
        change_beds_pct,
        direction
      ),
    by = "country"
  )

if (
  nrow(ambulant_plot) > 0
) {
  
  fig3 <- ggplot(
    ambulant_plot,
    aes(
      x = change_beds_pct,
      y = change_share_pp
    )
  ) +
    
    geom_hline(
      yintercept = 0,
      linewidth = 0.7,
      linetype = "dashed"
    ) +
    
    geom_vline(
      xintercept = 0,
      linewidth = 0.7,
      linetype = "dashed"
    ) +
    
    geom_point(
      aes(
        fill = trend
      ),
      shape = 21,
      size = 5,
      color = "black"
    ) +
    
    geom_text(
      aes(
        label = country_name
      ),
      vjust = -1,
      size = 3.5
    ) +
    
    scale_fill_manual(
      values = c(
        "Outpatient nimmt zu" = "#1B9E77",
        "Outpatient nimmt ab" = "#D95F02",
        "Unverändert" = "#7570B3"
      )
    ) +
    
    labs(
      title =
        "Ambulantisierung und stationäre Kapazität",
      
      subtitle =
        "Veränderung 2015–2023",
      
      x =
        "Veränderung Krankenhausbetten je 1.000 Einwohner (%)",
      
      y =
        "Veränderung Outpatient-Anteil (Prozentpunkte)",
      
      fill = NULL
    ) +
    
    theme_minimal(
      base_size = 13
    ) +
    
    theme(
      plot.title =
        element_text(
          face = "bold",
          size = 16
        ),
      
      plot.subtitle =
        element_text(
          size = 11
        ),
      
      legend.position =
        "bottom"
    )
  
  print(fig3)
  
  ggsave(
    "figures/Abb_HC3_Ambulantisierung_vs_Hospital_Beds.png",
    fig3,
    width = 9,
    height = 7,
    dpi = 300
  )
  
} else {
  
  warning(
    "Keine gemeinsamen Länder für Ambulantisierungs- und Bettenanalyse."
  )
}

# ============================================================
# 40. EXPORT
# ============================================================

# ------------------------------------------------------------
# 40.1 Datenbasis HC3
# ------------------------------------------------------------

write.csv(
  hc3_share,
  "OECD_HC3_Ambulantisierung_Datenbasis_2015_2023.csv",
  row.names = FALSE,
  na = ""
)


# ------------------------------------------------------------
# 40.2 Ergebnisse Referenzländer
# ------------------------------------------------------------

write.csv(
  hc3_results_table,
  "OECD_HC3_Ambulantisierung_Referenzlaender_2015_2023.csv",
  row.names = FALSE,
  na = ""
)


# ------------------------------------------------------------
# 40.3 Datenabdeckung erzeugen
# ------------------------------------------------------------

coverage_hc3 <- hc3_share |>
  group_by(country) |>
  summarise(
    n_years = n_distinct(year),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    complete_2015_2023 =
      first_year <= START_YEAR &
      last_year >= END_YEAR &
      n_years >= (END_YEAR - START_YEAR + 1)
  ) |>
  arrange(country)


# ------------------------------------------------------------
# 40.4 Datenabdeckung exportieren
# ------------------------------------------------------------

write.csv(
  coverage_hc3,
  "OECD_HC3_Datenabdeckung_2015_2023.csv",
  row.names = FALSE,
  na = ""
)


# ------------------------------------------------------------
# 40.5 Kontrolle
# ------------------------------------------------------------

cat(
  "\n============================================================\n"
)
cat(
  "EXPORT ABGESCHLOSSEN\n"
)
cat(
  "============================================================\n"
)

print(
  as_tibble(coverage_hc3),
  n = Inf
)

# ============================================================
# 41. ABSCHLUSSAUSGABE
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "HC3-ANALYSE ABGESCHLOSSEN\n"
)

cat(
  "============================================\n"
)

cat(
  "Ausgewählte Einheit: ",
  selected_unit,
  "\n",
  sep = ""
)

cat(
  "Ausgewählte J-Ausprägung: ",
  selected_J,
  "\n",
  sep = ""
)

cat(
  "Länder mit vollständigem 2015/2023-Vergleich: ",
  nrow(
    hc3_change
  ),
  "\n",
  sep = ""
)

cat(
  "============================================\n"
)

# ============================================================
# 42. FINALE GESAMTAUSGABE ALLER RELEVANTEN RESULTATE
# ============================================================

cat(
  "\n\n============================================================\n"
)
cat(
  "FINALE GESAMTERGEBNISSE OECD-PANELANALYSE 2015-2023\n"
)
cat(
  "============================================================\n\n"
)


# ------------------------------------------------------------
# A. STICHPROBE
# ------------------------------------------------------------

cat("A. STICHPROBE UND DATENQUALITÄT\n")
cat("------------------------------------------------------------\n")

cat(
  "Länder im finalen Sample: ",
  n_distinct(analysis$country),
  "\n",
  sep = ""
)

cat(
  "Beobachtungen: ",
  nrow(analysis),
  "\n",
  sep = ""
)

cat(
  "Zeitraum: ",
  min(analysis$year),
  "-",
  max(analysis$year),
  "\n",
  sep = ""
)

cat(
  "Ausgeschlossene Länder: ",
  paste(
    excluded_countries$country,
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "Fehlende Modellbeobachtungen im finalen Sample: ",
  sum(
    !complete.cases(
      analysis[
        c(
          "hospital_beds_pc",
          "health_exp_pc_real_ppp",
          "health_exp_gdp",
          "real_gdp_pc_ppp",
          "share_65plus"
        )
      ]
    )
  ),
  "\n",
  sep = ""
)

cat(
  "Fehlende Beds-Beobachtungen im vollständigen Panel: ",
  sum(
    is.na(panel$hospital_beds_pc)
  ),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------
# B. DESKRIPTIVE STATISTIK
# ------------------------------------------------------------

cat("B. DESKRIPTIVE STATISTIK\n")
cat("------------------------------------------------------------\n")

print(
  as_tibble(descriptive),
  n = Inf
)

cat("\n")


# ------------------------------------------------------------
# C. HAUPTMODELL 1
# ------------------------------------------------------------

cat("C. MODELL 1 – REALE GESUNDHEITSAUSGABEN PRO KOPF\n")
cat("------------------------------------------------------------\n")

model_pc_ct <- summary(model_pc)$coeftable

cat(
  "Hospital beds: β = ",
  round(
    model_pc_ct["hospital_beds_pc", "Estimate"],
    6
  ),
  "; SE = ",
  round(
    model_pc_ct["hospital_beds_pc", "Std. Error"],
    6
  ),
  "; p = ",
  format.pval(
    model_pc_ct["hospital_beds_pc", "Pr(>|t|)"],
    digits = 4,
    eps = 0.001
  ),
  "\n",
  sep = ""
)

cat(
  "Reales GDP/Kopf: β = ",
  round(
    model_pc_ct["log(real_gdp_pc_ppp)", "Estimate"],
    6
  ),
  "; SE = ",
  round(
    model_pc_ct["log(real_gdp_pc_ppp)", "Std. Error"],
    6
  ),
  "; p = ",
  format.pval(
    model_pc_ct["log(real_gdp_pc_ppp)", "Pr(>|t|)"],
    digits = 4,
    eps = 0.001
  ),
  "\n",
  sep = ""
)

cat(
  "Anteil >=65 Jahre: β = ",
  round(
    model_pc_ct["share_65plus", "Estimate"],
    6
  ),
  "; SE = ",
  round(
    model_pc_ct["share_65plus", "Std. Error"],
    6
  ),
  "; p = ",
  format.pval(
    model_pc_ct["share_65plus", "Pr(>|t|)"],
    digits = 4,
    eps = 0.001
  ),
  "\n",
  sep = ""
)

cat(
  "Exakter Effekt je +1 Bett/1.000 Einwohner: ",
  round(
    beds_effect_pc_percent,
    3
  ),
  "%\n",
  sep = ""
)

cat(
  "Within R²: ",
  round(
    r2(
      model_pc,
      type = "wr2"
    ),
    5
  ),
  "\n\n",
  sep = ""
)

# ------------------------------------------------------------
# D. HAUPTMODELL 2
# ------------------------------------------------------------

cat("D. MODELL 2 – GESUNDHEITSAUSGABEN IN % DES BIP\n")
cat("------------------------------------------------------------\n")

model_gdp_ct <- summary(model_gdp)$coeftable

cat(
  "Hospital beds: β = ",
  round(
    model_gdp_ct["hospital_beds_pc", "Estimate"],
    6
  ),
  "; SE = ",
  round(
    model_gdp_ct["hospital_beds_pc", "Std. Error"],
    6
  ),
  "; p = ",
  format.pval(
    model_gdp_ct["hospital_beds_pc", "Pr(>|t|)"],
    digits = 4,
    eps = 0.001
  ),
  "\n",
  sep = ""
)

cat(
  "Reales GDP/Kopf: β = ",
  round(
    model_gdp_ct["log(real_gdp_pc_ppp)", "Estimate"],
    6
  ),
  "; SE = ",
  round(
    model_gdp_ct["log(real_gdp_pc_ppp)", "Std. Error"],
    6
  ),
  "; p = ",
  format.pval(
    model_gdp_ct["log(real_gdp_pc_ppp)", "Pr(>|t|)"],
    digits = 4,
    eps = 0.001
  ),
  "\n",
  sep = ""
)

cat(
  "Anteil >=65 Jahre: β = ",
  round(
    model_gdp_ct["share_65plus", "Estimate"],
    6
  ),
  "; SE = ",
  round(
    model_gdp_ct["share_65plus", "Std. Error"],
    6
  ),
  "; p = ",
  format.pval(
    model_gdp_ct["share_65plus", "Pr(>|t|)"],
    digits = 4,
    eps = 0.001
  ),
  "\n",
  sep = ""
)

cat(
  "Within R²: ",
  round(
    r2(
      model_gdp,
      type = "wr2"
    ),
    5
  ),
  "\n\n",
  sep = ""
)

# ------------------------------------------------------------
# E. DESKRIPTIVE VERÄNDERUNG 2015 -> 2023
# ------------------------------------------------------------

cat("E. DESKRIPTIVE VERÄNDERUNGEN 2015–2023\n")
cat("------------------------------------------------------------\n")

cat(
  "Mittlere Veränderung Hospital Beds: ",
  round(
    mean_beds_change,
    2
  ),
  "%\n",
  sep = ""
)

cat(
  "Mittlere Veränderung reale Gesundheitsausgaben/Kopf: ",
  round(
    mean_health_change,
    2
  ),
  "%\n",
  sep = ""
)

cat(
  "Länder mit sinkender Bettendichte: ",
  sum(
    country_change$beds_change_pct < 0,
    na.rm = TRUE
  ),
  " von ",
  nrow(country_change),
  "\n",
  sep = ""
)

cat(
  "Länder mit steigender Bettendichte: ",
  sum(
    country_change$beds_change_pct > 0,
    na.rm = TRUE
  ),
  " von ",
  nrow(country_change),
  "\n",
  sep = ""
)

cat(
  "Sinkende Bettendichte UND steigende Gesundheitsausgaben: ",
  sum(
    country_change$beds_change_pct < 0 &
      country_change$health_exp_change_pct > 0,
    na.rm = TRUE
  ),
  " von ",
  nrow(country_change),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------
# F. VOLLSTÄNDIGE LÄNDERVERÄNDERUNGEN
# ------------------------------------------------------------

cat("F. LÄNDERVERÄNDERUNGEN 2015–2023\n")
cat("------------------------------------------------------------\n")

print(
  as_tibble(
    country_change |>
      select(
        country,
        beds_change_pct,
        health_exp_change_pct
      ) |>
      arrange(
        beds_change_pct
      )
  ),
  n = Inf
)

cat("\n")


# ------------------------------------------------------------
# G. REFERENZLÄNDER – HOSPITAL BEDS
# ------------------------------------------------------------

cat("G. REFERENZLÄNDER – HOSPITAL BEDS\n")
cat("------------------------------------------------------------\n")

print(
  as_tibble(
    beds_reference |>
      arrange(
        change_beds_pct
      )
  ),
  n = Inf
)

cat("\n")


# ------------------------------------------------------------
# H. HC3 – OUTPATIENT VS. INPATIENT
# ------------------------------------------------------------

if (
  exists(
    "hc3_results_table",
    inherits = TRUE
  )
) {
  
  cat(
    "H. HC3 – OUTPATIENT/INPATIENT\n"
  )
  
  cat(
    "------------------------------------------------------------\n"
  )
  
  cat(
    "Ausgewählte Einheit: ",
    selected_unit,
    "\n",
    sep = ""
  )
  
  cat(
    "Ausgewählte J-Ausprägung: ",
    selected_J,
    "\n",
    sep = ""
  )
  
  cat(
    "Vollständige Länder: ",
    nrow(hc3_results_table),
    "\n\n",
    sep = ""
  )
  
  print(
    as_tibble(
      hc3_results_table
    ),
    n = Inf
  )
  
  cat("\n")
  
  cat(
    "Nicht verfügbare Referenzländer: ",
    paste(
      missing_reference_countries,
      collapse = ", "
    ),
    "\n\n",
    sep = ""
  )
}


# ------------------------------------------------------------
# I. JÄHRLICHE TRENDS – NUMERISCHE AUSGABE
# ------------------------------------------------------------

cat("I. JÄHRLICHE OECD-TRENDS\n")
cat("------------------------------------------------------------\n")

cat("\nHospital Beds:\n")
print(
  as_tibble(trend_beds),
  n = Inf
)

cat("\nReale Gesundheitsausgaben/Kopf:\n")
print(
  as_tibble(trend_health_pc),
  n = Inf
)

cat("\nGesundheitsausgaben % BIP:\n")
print(
  as_tibble(trend_health_gdp),
  n = Inf
)


# ------------------------------------------------------------
# J. HC3-DATENABDECKUNG
# ------------------------------------------------------------

if (
  exists(
    "coverage_hc3",
    inherits = TRUE
  )
) {
  
  cat(
    "\nJ. HC3-DATENABDECKUNG\n"
  )
  
  cat(
    "------------------------------------------------------------\n"
  )
  
  print(
    as_tibble(
      coverage_hc3
    ),
    n = Inf
  )
}


