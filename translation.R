#def version exploratory clean
#=========================================================
#---- 0) Libraries & data loading ------------------------
#=========================================================

suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, ggplot2, readr, stringr, tidyr, purrr.
  library(lubridate)   # date handling
  library(stringr)     # extra string tools (if needed)
  library(scales)      # pretty scales (percent, etc.)
})
accidents <- read.csv("data/Accidents_de_trànsit_amb_morts_o_ferits_greus_a_Catalunya_20251202.csv")
accidents

#=========================================================
#---- 1) Basic feature engineering on main data ----------
#      - date
#      - clean 'via'
#      - toll_period
#      - toll_corridor
#      - road_family
#=========================================================

accidents %>%
  select(where(is.character)) %>%
  imap(~ {
    u <- unique(.x)
    n_u <- length(u)
    if (n_u < 50) {
      cat("\n==== Spalte:", .y, "====\n")
      cat("Anzahl eindeutiger Werte:", n_u, "\n")
      print(u)
    }
    invisible(NULL)
  })
################

maps <- list(
  zone = list(
    cat = c("Zona urbana", "Carretera"),
    eng = c("Urban area", "Road")
  ),
  D_ACC_AMB_FUGA = list(
    cat = c("No", "Si", "Sense Especificar"),
    eng = c("No", "Yes", "Unspecified")
  ),
  D_BOIRA = list(
    cat = c("No n'hi ha", "Si"),
    eng = c("None", "Yes")
  ),
  D_CARACT_ENTORN = list(
    cat = c("Desmunt", "A nivell", "Sense Especificar", "Mixt", "Terraplé"),
    eng = c("Uneven", "Level", "Unspecified", "Mixed", "Embankment")
  ),
  D_CARRIL_ESPECIAL = list(
    cat = c(
      "No n'hi ha", "Carril habilitat en sentit contrari habitual", "Habilitació voral/carril addiciol",
      "Carril lent", "Carril d'alentiment", "Altres",
      "Carril bus", "Carril central",
      "Carril bici", "Carril avançament",
      "Carril reversible", "Carril acceleració",
      "Sense Especificar"
    ),
    eng = c(
      "None", "Lane enabled in opposite direction", "Shoulder/additional lane",
      "Slow lane", "Deceleration lane", "Others",
      "Bus lane", "Central lane",
      "Bike lane", "Overtaking lane",
      "Reversible lane", "Acceleration lane",
      "Unspecified"
    )
  ),
  D_CIRCULACIO_MESURES_ESP = list(
    cat = c(
      "No n'hi ha", "Obres", "Serveis de neteja o manteniment",
      "Esdeveniment extraordinari", "Accident trànsit anterior",
      "Cons", "Control policial"
    ),
    eng = c(
      "None", "Roadworks", "Cleaning or maintenance services",
      "Extraordinary event", "Previous traffic accident",
      "Cones", "Police control"
    )
  ),
  D_CLIMATOLOGIA = list(
    cat = c(
      "Bon temps", "Pluja forta", "Pluja dèbil", "Nevant",
      "Sense especificar", "Calamarsa"
    ),
    eng = c(
      "Good weather", "Heavy rain", "Light rain", "Snowing",
      "Unspecified", "Hail"
    )
  ),
  D_FUNC_ESP_VIA = list(
    cat = c(
      "Sense funció especial", "Variant", "Travessera",
      "Ronda, cinturó o circumval·lació", "Sense especificar"
    ),
    eng = c(
      "No special function", "Bypass", "Crossing",
      "Ring road or beltway", "Unspecified"
    )
  ),
  D_GRAVETAT = list(
    cat = c("Accident greu", "Accident mortal"),
    eng = c("Serious accident", "Fatal accident")
  ),
  D_INFLUIT_BOIRA = list(
    cat = c("No", "Sense especificar", "Si"),
    eng = c("No", "Unspecified", "Yes")
  ),
  D_INFLUIT_CARACT_ENTORN = list(
    cat = c("No", "Si", "Sense especificar"),
    eng = c("No", "Unspecified", "Yes")
  ),
  D_INFLUIT_CIRCULACIO = list(
    cat = c("No", "Si", "Sense especificar"),
    eng = c("No", "Unspecified", "Yes")
  ),
  D_INFLUIT_ESTAT_CLIMA = list(
    cat = c("No", "Si", "Sense especificar"),
    eng = c("No", "Unspecified", "Yes")
  ),
  D_INFLUIT_INTEN_VENT = list(
    cat = c("No", "Sense especificar", "Si"),
    eng = c("No", "Unspecified", "Yes")
  ),
  D_INFLUIT_LLUMINOSITAT = list(
    cat = c("No", "Si", "Sense especificar"),
    eng = c("No", "Unspecified", "Yes")
  ),
  D_INFLUIT_MESU_ESP = list(
    cat = c("No", "Si", "Sense especificar"),
    eng = c("No", "Unspecified", "Yes")
  ),
  D_INFLUIT_OBJ_CALCADA = list(
    cat = c("No", "Si", "Sense especificar"),
    eng = c("No", "Yes", "Unspecified")
  ),
  D_INFLUIT_SOLCS_RASES = list(
    cat = c("No", "Si", "Sense especificar"),
    eng = c("No", "Yes", "Unspecified")
  ),
  D_INFLUIT_VISIBILITAT = list(
    cat = c("No", "Si", "Sense especificar"),
    eng = c("No", "Yes", "Unspecified")
  ),
  D_INTER_SECCIO = list(
    cat = c("Arribant o eixint intersecció fins 50m", "Dintre intersecció", "En secció"),
    eng = c("Approaching or leaving intersection up to 50m", "Inside intersection", "On section")
  ),
  D_LIMIT_VELOCITAT = list(
    cat = c("Genérica via", "Senyal velocitat"),
    eng = c("Generic road", "Speed sign")
  ),
  D_LLUMINOSITAT = list(
    cat = c(
      "De nit, il·luminació artificial suficient",
      "De dia, dia clar",
      "De nit, sense llum artificial",
      "De dia, dia fosc",
      "Alba o capvespre",
      "De nit, il·lumició artificial insuficient",
      "Sense especificar"
    ),
    eng = c(
      "At night, sufficient artificial lighting",
      "Daytime, clear day",
      "At night, no artificial light",
      "Daytime, dark day",
      "Dawn or dusk",
      "At night, insufficient artificial lighting",
      "Unspecified"
    )
  ),
  D_REGULACIO_PRIORITAT = list(
    cat = c(
      "Sols norma prioritat de pas",
      "Senyal Stop o cedeix pas",
      "Semàfor",
      "Sols marques viàries (inclou pas vianants)",
      "Perso autoritzada",
      "Altres"
    ),
    eng = c(
      "Only right-of-way rule",
      "Stop or yield sign",
      "Traffic light",
      "Only road markings (including pedestrian crossing)",
      "Authorized person",
      "Others"
    )
  ),
  D_SENTITS_VIA = list(
    cat = c("Un sol sentit", "Doble sentit", "Sense especificar"),
    eng = c("One-way", "Two-way", "Unspecified")
  ),
  D_SUBTIPUS_ACCIDENT = list(
    cat = c(
      "Encalç",
      "Resta sortides de via",
      "Col·lisió frontal",
      "Envestida (frontal lateral)",
      "Caiguda en la via",
      "Atropellament",
      "Fregament o col·lisió lateral",
      "Xoc contra objecte/obstacle sense sortida prèvia de via",
      "Altres",
      "Sortida de via amb xoc o col·lisió",
      "Sortida de via amb bolcada",
      "Sortida de via amb atropellament",
      "Xoc amb animal a la calçada",
      "Sense Especificar"
    ),
    eng = c(
      "Rear-end collision",
      "Other road departures",
      "Head-on collision",
      "Frontal-lateral collision",
      "Fall onto the roadway",
      "Pedestrian collision",
      "Side swipe or lateral collision",
      "Collision with object/obstacle without prior road departure",
      "Other",
      "Road departure with crash or collision",
      "Road departure with rollover",
      "Road departure with pedestrian collision",
      "Collision with animal on roadway",
      "Unspecified"
    )
  ),
  D_SUBTIPUS_TRAM = list(
    cat = c(
      "Intersecció en T o Y",
      "Giratòria",
      "Encreuament o intersecció en X o +",
      "Enllaç d'entrada o eixida",
      "Sense especificar",
      "Pas a nivell"
    ),
    eng = c(
      "T or Y intersection",
      "Roundabout",
      "Crossroad or X/+ intersection",
      "Entry or exit link",
      "Unspecified",
      "Level crossing"
    )
  ),
  D_SUBZO = list(
    cat = c("Zo urba", "Carretera", "Travessera"),
    eng = c("Urban area", "Road", "Crossing")
  ),
  D_SUPERFICIE = list(
    cat = c("Sec i net", "Relliscós", "Mullat", "Inundat", "Nevat", "Gelat", "Sense especificar"),
    eng = c("Dry and clean", "Slippery", "Wet", "Flooded", "Snowy", "Icy", "Unspecified")
  ),
  D_TIPUS_VIA = list(
    cat = c(
      "Via urbana( inclou carrer i carrer residencial)",
      "Carretera convencional",
      "Altres",
      "Autopista",
      "Autovia",
      "Camí rural/pista forestal"
    ),
    eng = c(
      "Urban road (including street and residential street)",
      "Conventional road",
      "Others",
      "Motorway",
      "Highway",
      "Rural road/forest track"
    )
  ),
  D_TITULARITAT_VIA = list(
    cat = c("Estatal", "Municipal", "Autonòmica", "Provincial", "Altres", "Sense Especificar"),
    eng = c("State", "Municipal", "Regional", "Provincial", "Others", "Unspecified")
  ),
  D_TRACAT_ALTIMETRIC = list(
    cat = c("Pla", "Rampa o pendent", "Sense especificar", "Canvi rasant", "Gual"),
    eng = c("Flat", "Slope or incline", "Unspecified", "Crest change", "Ford")
  ),
  D_VENT = list(
    cat = c("Calma, vent molt suau", "Vent moderat", "Vent fort", "Sense especificar"),
    eng = c("Calm, very light wind", "Moderate wind", "Strong wind", "Unspecified")
  ),
  grupDiaLab = list(
    cat = c("Feiners", "CapDeSetmana"),
    eng = c("Weekdays", "Weekend")
  ),
  grupHor = list(
    cat = c("Nit", "Tarda", "Matí"),
    eng = c("Night", "Afternoon", "Morning")
  ),
  tipAcc = list(
    cat = c(
      "Col.lisió de vehicles en marxa",
      "Sortida de la calcada sense especificar",
      "Bolcada a la calcada",
      "Atropellament",
      "Col.lisió d'un vehicle contra un obstacle de la calcada",
      "Altres",
      "Sense especificar"
    ),
    eng = c(
      "Collision of moving vehicles",
      "Road departure (unspecified)",
      "Rollover on the road",
      "Pedestrian hit",
      "Collision of a vehicle against a road obstacle",
      "Others",
      "Unspecified"
    )
  ),
  tipDia = list(
    cat = c("dill-dij", "dg", "dis", "div"),
    eng = c("Mon-Thu", "Sun", "Sat", "Fri")
  )
)


for (col_name in names(accidents)) {
  if (col_name %in% names(maps)) {
    map_cat <- maps[[col_name]]$cat
    map_eng <- maps[[col_name]]$eng
    accidents[[col_name]] <- as.character(accidents[[col_name]])
    accidents[[col_name]][accidents[[col_name]] %in% map_cat] <- 
      map_eng[match(accidents[[col_name]][accidents[[col_name]] %in% map_cat], map_cat)]
  }
}

cols_original <- c(
  "any", "zone", "dat", "via", "pk", "nommun", "nomcom", "nomdem",
  "F_MORTS", "F_FERITS_GREUS", "F_FERITS_LLEUS", "F_VICTIMES",
  "F_UNITATS_IMPLICADES", "F_VIANANTS_IMPLICADES", "F_BICICLETES_IMPLICADES",
  "F_CICLOMOTORS_IMPLICADES", "F_MOTOCICLETES_IMPLICADES",
  "F_VEH_LLEUGERS_IMPLICADES", "F_VEH_PESANTS_IMPLICADES",
  "F_ALTRES_UNIT_IMPLICADES", "F_UNIT_DESC_IMPLICADES",
  "C_VELOCITAT_VIA", "D_ACC_AMB_FUGA", "D_BOIRA", "D_CARACT_ENTORN",
  "D_CARRIL_ESPECIAL", "D_CIRCULACIO_MESURES_ESP", "D_CLIMATOLOGIA",
  "D_FUNC_ESP_VIA", "D_GRAVETAT", "D_INFLUIT_BOIRA",
  "D_INFLUIT_CARACT_ENTORN", "D_INFLUIT_CIRCULACIO",
  "D_INFLUIT_ESTAT_CLIMA", "D_INFLUIT_INTEN_VENT", "D_INFLUIT_LLUMINOSITAT",
  "D_INFLUIT_MESU_ESP", "D_INFLUIT_OBJ_CALCADA", "D_INFLUIT_SOLCS_RASES",
  "D_INFLUIT_VISIBILITAT", "D_INTER_SECCIO", "D_LIMIT_VELOCITAT",
  "D_LLUMINOSITAT", "D_REGULACIO_PRIORITAT", "D_SENTITS_VIA",
  "D_SUBTIPUS_ACCIDENT", "D_SUBTIPUS_TRAM", "D_SUBZO", "D_SUPERFICIE",
  "D_TIPUS_VIA", "D_TITULARITAT_VIA", "D_TRACAT_ALTIMETRIC", "D_VENT",
  "grupDiaLab", "hor", "grupHor", "tipAcc", "tipDia"
)

# 2. Übersetzte englische Spaltennamen
cols_eng <- c(
  "Year", "Zone type", "Date", "Road", "Kilometer marker", "Municipality",
  "County", "Accident location", "Number of fatalities", "Number of serious injuries",
  "Number of minor injuries", "Total victims", "Number of vehicles involved",
  "Number of pedestrians involved", "Number of bicycles involved",
  "Number of mopeds involved", "Number of motorcycles involved",
  "Number of light vehicles involved", "Number of heavy vehicles involved",
  "Number of other units involved", "Number of unknown units involved",
  "Speed limit", "Hit-and-run", "Fog present", "Site characteristics",
  "Special lane presence", "Special traffic measures", "Climatology",
  "Road with special function", "Accident severity", "Affected by fog",
  "Affected by terrain characteristics", "Affected by traffic",
  "Affected by weather", "Affected by wind", "Affected by lighting",
  "Affected by special traffic measures", "Affected by object on road",
  "Affected by grooves or ditches", "Affected by visibility", "Intersection",
  "Speed limit display", "Lighting conditions", "Right-of-way regulation",
  "Road direction(s)", "Accident subtype", "Section type", "Zone classification",
  "Road surface", "Road type", "Road ownership", "Altitude profile",
  "Wind conditions", "Workday or weekend", "Time of accident", "Time of day",
  "Accident type", "Day of week"
)
names(accidents) <- cols_eng

output_file <- "accidents_transformed.csv"

write.csv(accidents, file = output_file, row.names = FALSE)

cat("CSV-Datei gespeichert unter:", normalizePath(output_file), "\n")


############

accidents <- accidents |>
  mutate(
    # date to Date
    Date = dmy(Date),
    
    # 'SE' in via -> NA (sense especificar)
    Road = na_if(Road, "SE"),
    
    # speed limit 999 -> NA (code for unknown / invalid)
    `Speed limit` = if_else(`Speed limit` > 121,
                              NA_real_, `Speed limit`),
    
    # before/after toll removal
    toll_period = if_else(Date >= as.Date("2021-09-01"), "after", "before"),
    toll_period = factor(toll_period, levels = c("before", "after")),
    
    # roads where tolls were removed (AP-7, AP-2, C-32, C-33)
    toll_corridor = if_else(
      Road %in% c("AP-7", "AP-2", "C-32", "C-33"),
      "toll_removed",
      "other"
    ),
    toll_corridor = factor(toll_corridor,
                           levels = c("other", "toll_removed")),
    
    # simple road family (AP / A / N / C / other / missing)
    road_family = case_when(
      str_starts(Road, "AP-") ~ "AP",
      str_starts(Road, "A-")  ~ "A",
      str_starts(Road, "N-")  ~ "N",
      str_starts(Road, "C-")  ~ "C",
      is.na(Road)             ~ "missing",
      TRUE                   ~ "other"
    ),
    road_family = factor(road_family)
  )

#=========================================================
#---- 2) Create working dataset for EDA / modelling ------
#      - drop pure labels / not needed columns
#      - drop 'via' (773 levels) from model data
#=========================================================

accidents_work <- accidents |>
  select(
    -Year,
    -Municipality,
    -County,
    -`Accident location`,
    -`Kilometer marker`,
    -`Time of accident`,
    -Road
  )


#=========================================================
#---- 3) Convert text "Sense especificar" etc. to NA -----
#=========================================================

accidents_work <- accidents_work |>
  mutate(
    across(
      where(is.character),
      ~ .x |>
        na_if("Unspecified") |>
        na_if("NA") |>
        na_if("")
    )
  )

#=========================================================
#---- 4) Convert character variables to factors ----------
#=========================================================

accidents_work <- accidents_work |>
  mutate(
    across(
      where(is.character),
      as.factor
    )
  )

#=========================================================
#---- 5) Factor level summaries (console output) ---------
#      For each factor variable, print the count of
#      observations per level (sorted by frequency).
#=========================================================

factor_vars <- accidents_work |>
  select(where(is.factor)) |>
  names()

for (v in factor_vars) {
  cat("\n-----------", v, "-----------\n")
  print(
    accidents_work |>
      count(.data[[v]], sort = TRUE)
  )
}

#=========================================================
#---- 6) PLOTS -------------------------------------------
#=========================================================

#---------------------------------------------------------
# 6.1 Normalized cumulative accidents by road type
#---------------------------------------------------------

accidents_work |>
  filter(!is.na(toll_corridor)) |>
  arrange(Date) |>
  group_by(toll_corridor) |>
  mutate(
    acc_id   = row_number(),         
    acc_prop = acc_id / max(acc_id)  
  ) |>
  ungroup() |>
  ggplot(aes(x = Date, y = acc_prop, color = toll_corridor)) +
  geom_line(linewidth = 0.7) +
  geom_vline(
    xintercept = as.Date("2021-09-01"),
    linetype   = "dashed"
  ) +
  annotate(
    "text",
    x = as.Date("2021-09-01"),
    y = 0.88,
    label = "TOLL REMOVED",
    angle = 90,
    vjust = -0.5,
    hjust = 0
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Normalized cumulative accidents by road type",
    x = "Date",
    y = "Cumulative proportion of accidents",
    color = "Road type"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#---------------------------------------------------------
# 6.2 Cumulative within period (toll_removed only)
#---------------------------------------------------------

accidents_work |>
  filter(toll_corridor == "toll_removed") |>
  group_by(toll_period) |>
  arrange(Date, .by_group = TRUE) |>
  mutate(
    days_since_start = as.numeric(Date - first(Date)),
    acc_id          = row_number(),
    acc_prop        = acc_id / max(acc_id)
  ) |>
  ungroup() |>
  ggplot(aes(x = days_since_start, y = acc_prop, color = toll_period)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Normalized cumulative accidents on toll-removed corridors",
    subtitle = "Time reset at start of each period (before vs after toll removal)",
    x = "Days since period start",
    y = "Cumulative proportion of accidents",
    color = "Period"
  ) +
  theme_bw()

#---------------------------------------------------------
# 6.3 Severity mix before/after by road type
#---------------------------------------------------------

accidents_work |>
  count(toll_period, toll_corridor, `Accident severity` ) |>
  group_by(toll_period, toll_corridor) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  ggplot(aes(x = toll_period, y = prop, fill = `Accident severity`)) +
  geom_col(position = "fill") +
  facet_wrap(~ toll_corridor) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Severity mix before/after by road type",
    x = "Period",
    y = "Proportion within road type",
    fill = "Severity"
  ) +
  theme_bw()

