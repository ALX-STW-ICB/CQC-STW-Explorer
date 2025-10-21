# app.R — CQC Explorer + repo-first data discovery — v2025-10-21h
# ------------------------------------------------------------------------------
# What’s new in v2025-10-21h
# - Repo-first file discovery for BOTH CQC CSV and LSOA geometry.
# - Supports .gpkg / .geojson (prefer these in GitHub) + still accepts .shp.
# - Clear banner showing exactly which path was used (or if demo mode).
# - No behavioural change to filters, Overview, IMD plots, or map logic.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  .pkgs <- c(
    "shiny","bslib","readr","dplyr","tidyr","stringr","lubridate",
    "forcats","purrr","DT","ggplot2","leaflet","janitor","glue","scales",
    "jsonlite","sf"
  )
  miss <- .pkgs[! .pkgs %in% rownames(installed.packages())]
  if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org", quiet = TRUE)
  lapply(.pkgs, require, character.only = TRUE)
  suppressWarnings({ if (requireNamespace("plotly", quietly = TRUE)) library(plotly) })
})

set.seed(20251017)

# ------------------------------ CLI / Env config -------------------------------
.get_arg <- function(flag, default = NULL) {
  args <- commandArgs(trailingOnly = TRUE)
  i <- which(args == flag)
  if (length(i) == 1 && i < length(args)) return(args[i+1])
  default
}
`%||%` <- function(a,b) if (!is.null(a) && !is.na(a) && length(a)>0) a else b

# ---------- Repo-first discovery helpers ---------------------------------------
# We avoid assuming "data/". Instead we search several repo-friendly paths.
repo_candidates <- function(basenames, subdirs = c(".", "assets", "assets/cqc", "assets/geodata",
                                                   "cqc", "geodata", "extdata", "static")) {
  basenames <- unique(basenames)
  subdirs   <- unique(subdirs)
  as.character(unlist(lapply(subdirs, function(sd) file.path(sd, basenames))))
}
first_existing <- function(paths) {
  paths <- unique(normalizePath(paths[file.exists(paths)], winslash = "/", mustWork = FALSE))
  if (length(paths)) paths[1] else NA_character_
}
# Prefer single-file geospatial formats in GitHub
LSOA_BASENAMES <- c(
  "LSOA_2021_EW_BSC_V4.gpkg",
  "LSOA_2021_EW_BSC_V4.geojson",
  "LSOA_2021_EW_BSC_V4.shp"  # still supported if committed with all sidecars
)
CQC_BASENAMES  <- c(
  "locations_stw_filtered_merged.csv",
  "cqc_locations.csv"
)

resolve_cqc_path <- function() {
  # Priority: CLI -> ENV -> repo -> demo
  cli <- .get_arg("--input", NULL)
  if (!is.null(cli) && nzchar(cli) && file.exists(cli)) return(list(path = normalizePath(cli), source="CLI"))
  env <- Sys.getenv("CQC_CSV", unset = "")
  if (nzchar(env) && file.exists(env)) return(list(path = normalizePath(env), source="ENV CQC_CSV"))
  repo <- first_existing(repo_candidates(CQC_BASENAMES))
  if (!is.na(repo)) return(list(path = repo, source="repo"))
  list(path = NA_character_, source="demo")
}

resolve_lsoa_path <- function() {
  # Priority: CLI -> ENV -> repo -> NA (map polygons disabled)
  cli <- .get_arg("--lsoa_shp", NULL)
  if (!is.null(cli) && nzchar(cli) && file.exists(cli)) return(list(path = normalizePath(cli), source="CLI"))
  env <- Sys.getenv("LSOA_SHP", unset = "")
  if (nzchar(env) && file.exists(env)) return(list(path = normalizePath(env), source="ENV LSOA_SHP"))
  repo <- first_existing(repo_candidates(LSOA_BASENAMES))
  if (!is.na(repo)) return(list(path = repo, source="repo"))
  list(path = NA_character_, source="missing")
}

paths_cqc  <- resolve_cqc_path()
paths_lsoa <- resolve_lsoa_path()

# ------------------------------ Helpers: schema / utils ------------------------
canonical_map <- list(
  location_id          = c("locationid","location_id","locationId","LocationID","LocationId"),
  provider_id          = c("providerid","provider_id","providerId","ProviderID","ProviderId"),
  provider_name        = c("providername","provider_name","providerName","ProviderName"),
  location_name        = c("locationname","location_name","locationName","LocationName","name"),
  address1             = c("address1","address_1","line1","addressline1","address_line_1"),
  address2             = c("address2","address_2","line2","addressline2","address_line_2"),
  town_city            = c("towncity","town_city","town","city","TownCity"),
  county               = c("county","County"),
  postcode             = c("postcode","post_code","Postcode","postalCode","postal_code"),
  region               = c("region","Region"),
  ics_name             = c("icsname","ics_name","ICSName"),
  icb_name             = c("icbname","icb_name","ICBName"),
  local_authority      = c("localauthority","local_authority","LocalAuthority","lad_name","la_name",
                           "lad19nm","lad17nm","lad_name_2019"),
  latitude             = c("latitude","lat","Latitude"),
  longitude            = c("longitude","lon","long","Longitude"),
  overall_rating       = c("overallrating","overall_rating","OverallRating","overallRating",
                           "overall_rating_text","rating","ratingtext","rating_text"),
  overall_rating_value = c("overall_rating_value","overal_rating_value","overallratingvalue",
                           "ratingvalue","rating_value","overall_score"),
  rating_report_url    = c("ratingreporturl","rating_report_url","RatingReportUrl","report_url","url","ratingReportUrl"),
  last_inspection_date = c("lastinspectiondate","last_inspection_date","LastInspectionDate","inspection_date","lastRatedDate"),
  regulated_activity   = c("regulatedactivity","regulated_activity","RegulatedActivity","regulatedActivities","regulated_activities"),
  service_type         = c("servicetype","service_type","ServiceType","service","gacServiceType","gac_service_type"),
  gac_service_type_description = c("gacservicetypedescription","gac_service_type_description","gacServiceTypeDescription"),
  ownership_type       = c("ownershiptype","ownership_type","OwnershipType"),
  organisation_type    = c("organisationtype","organisation_type","OrganisationType","organization_type","organizationType"),
  domain               = c("domain","Domain","ratingDomain","rating_domain"),
  beds                 = c("beds","Beds","bed_count","BedCount"),
  on_site_services     = c("onSiteServices","on_site_services","onsiteservices","services_on_site"),
  phone                = c("phone","Phone","telephone","Telephone"),
  website              = c("website","Website","site","Site")
)

clean_and_canonicalise <- function(df) {
  df <- janitor::clean_names(df)
  src_names <- names(df); renames <- c()
  for (canon in names(canonical_map)) {
    variants <- tolower(canonical_map[[canon]])
    match <- intersect(variants, src_names)
    if (length(match) >= 1) renames[match[1]] <- canon
  }
  if (length(renames)) df <- dplyr::rename(df, !!!setNames(names(renames), renames))
  df
}

parse_date_safe <- function(x) {
  suppressWarnings({ out <- lubridate::parse_date_time(x, orders = c("Ymd","Y-m-d","d/m/Y","d/m/y","m/d/Y","Y-m")) })
  as.Date(out)
}

mask_text <- function(x) {
  x <- as.character(x)
  digits <- gsub("[^0-9]", "", x)
  ifelse(grepl("@", x),
         sub("^[^@]+", "***", x),
         ifelse(nchar(digits) >= 7, paste0("***", substr(digits, nchar(digits)-2, nchar(digits))), x))
}

schema_summary <- function(df) {
  tibble::tibble(
    name = names(df),
    type = vapply(df, function(col) paste(class(col), collapse=","), character(1)),
    missing_n = vapply(df, function(col) sum(is.na(col) | (is.character(col) & trimws(col)=="")), integer(1)),
    total_n = nrow(df)
  ) |>
    dplyr::mutate(missing_pct = round(100 * missing_n / pmax(total_n, 1), 1),
                  example = vapply(df, function(col) as.character(dplyr::first(na.omit(col))), character(1))) |>
    dplyr::select(name, type, missing_pct, example)
}

# ------------------------------ Ratings helpers --------------------------------
rating_levels  <- c("Outstanding","Good","Requires improvement","Inadequate","Not rated")
rating_palette <- c("Outstanding"="#2ca25f","Good"="#74c476","Requires improvement"="#fc8d59","Inadequate"="#e34a33","Not rated"="#bdbdbd")

normalize_text_to_levels <- function(x) {
  x <- stringr::str_to_sentence(trimws(as.character(x)))
  dplyr::case_when(
    is.na(x) | x=="" ~ "Not rated",
    x %in% "Outstanding" ~ "Outstanding",
    x %in% "Good" ~ "Good",
    x %in% c("Requires improvement","Requires Improvement","Requires-improvement","Ri","RI") ~ "Requires improvement",
    x %in% "Inadequate" ~ "Inadequate",
    TRUE ~ "Not rated"
  )
}

detect_text_rating_col <- function(df) {
  preferred <- intersect(c("overall_rating","overall_rating_text","rating"), names(df))
  if (length(preferred)) return(preferred[1])
  cand <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
  if (!length(cand)) return(NA_character_)
  pat <- "(outstanding|good|requires[ _-]*improvement|inadequate|^ri$)"
  score <- vapply(cand, function(nm) {
    x <- tolower(as.character(df[[nm]])); mean(grepl(pat, x, perl = TRUE), na.rm = TRUE)
  }, numeric(1))
  name_bias <- grepl("rating|overall", cand, ignore.case = TRUE)
  score <- score + ifelse(name_bias, 0.05, 0)
  if (max(score, na.rm = TRUE) < 0.01) return(NA_character_)
  cand[which.max(score)]
}

derive_overall_rating <- function(df) {
  used <- list(kind = "none", column = NA_character_, mapping = NA_character_)
  txt_col <- detect_text_rating_col(df)
  if (!is.na(txt_col)) {
    out <- normalize_text_to_levels(df[[txt_col]])
    used <- list(kind = "text", column = txt_col, mapping = "direct-text")
    return(list(values = factor(out, levels = rating_levels, ordered = TRUE), used = used))
  }
  num_cols_named <- names(df)[grepl("rating|overall", names(df), ignore.case = TRUE)]
  num_cand <- intersect(num_cols_named, names(df)[vapply(df, is.numeric, logical(1))])
  best <- NULL; best_cov <- -1
  maps <- list(
    "0NR_1I_2RI_3G_4O" = c(`0`="Not rated",`1`="Inadequate",`2`="Requires improvement",`3`="Good",`4`="Outstanding"),
    "1I_2RI_3G_4O"     = c(`1`="Inadequate",`2`="Requires improvement",`3`="Good",`4`="Outstanding")
  )
  for (nm in num_cand) {
    v <- as.character(suppressWarnings(as.integer(df[[nm]])))
    for (mname in names(maps)) {
      mapped <- unname(maps[[mname]][v]); cov <- mean(!is.na(mapped))
      if (is.finite(cov) && cov > best_cov) { best <- list(values = mapped, col = nm, map = mname); best_cov <- cov }
    }
  }
  if (!is.null(best) && best_cov > 0) {
    out <- ifelse(is.na(best$values), "Not rated", best$values)
    used <- list(kind = "numeric", column = best$col, mapping = best$map)
    return(list(values = factor(out, levels = rating_levels, ordered = TRUE), used = used))
  }
  used <- list(kind = "fallback", column = NA_character_, mapping = "all-not-rated")
  list(values = factor(rep("Not rated", nrow(df)), levels = rating_levels, ordered = TRUE), used = used)
}

# ------------------------------ Preprocess CQC ---------------------------------
preprocess_cqc <- function(df) {
  flags <- list(domain_present = "domain" %in% names(df), domain_kept = 0L, dedup_dropped = 0L, rating_source = NA_character_)
  if (flags$domain_present) {
    df <- df |>
      dplyr::mutate(.domain_l = tolower(as.character(domain))) |>
      dplyr::filter(grepl("^overall", .domain_l)) |>
      dplyr::select(-.domain_l)
  }
  flags$domain_kept <- nrow(df)
  if ("last_inspection_date" %in% names(df) && !inherits(df$last_inspection_date, "Date"))
    df$last_inspection_date <- parse_date_safe(df$last_inspection_date)
  df <- df |>
    dplyr::mutate(.date_ord = if ("last_inspection_date" %in% names(df)) as.numeric(last_inspection_date) else NA_real_) |>
    dplyr::arrange(dplyr::desc(.date_ord))
  n1 <- nrow(df)
  if ("location_id" %in% names(df)) df <- dplyr::distinct(df, location_id, .keep_all = TRUE)
  else if (all(c("location_name","postcode") %in% names(df))) df <- dplyr::distinct(df, location_name, postcode, .keep_all = TRUE)
  else if ("location_name" %in% names(df)) df <- dplyr::distinct(df, location_name, .keep_all = TRUE)
  df <- dplyr::select(df, -dplyr::any_of(".date_ord"))
  flags$dedup_dropped <- max(0L, n1 - nrow(df))
  drv <- derive_overall_rating(df); df$overall_rating <- drv$values
  flags$rating_source <- if (!is.null(drv$used$column)) glue::glue("{drv$used$kind} ({drv$used$column}; {drv$used$mapping})") else drv$used$kind
  list(data = df, flags = flags)
}

# ------------------------------ Load CQC ---------------------------------------
read_cqc_csv <- function(path) {
  if (!file.exists(path)) return(list(data=NULL, problems=NULL, used_demo=TRUE))
  df <- tryCatch(readr::read_csv(path, guess_max = 100000, locale = readr::locale(tz = "Europe/London"),
                                 show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
  if (is.null(df)) return(list(data=NULL, problems=NULL, used_demo=TRUE))
  problems <- readr::problems(df)
  df <- clean_and_canonicalise(df)
  if ("beds" %in% names(df)) df$beds <- suppressWarnings(as.numeric(df$beds))
  if ("latitude" %in% names(df)) df$latitude <- suppressWarnings(as.numeric(df$latitude))
  if ("longitude" %in% names(df)) df$longitude <- suppressWarnings(as.numeric(df$longitude))
  pp <- preprocess_cqc(df)
  list(data = pp$data, problems = problems, used_demo = FALSE, flags = pp$flags)
}

# ------------------------------ IMD (from input rows) --------------------------
detect_imd_cols_in_file <- function(nm) {
  list(
    decile = intersect(nm, c("imd_decile","imd2019_decile","imd_decile_2019","imddecile","imd_dec","decile")),
    rank   = intersect(nm, c("imd_rank","imd2019rank","imd_rank_2019","rank","imdrank")),
    score  = intersect(nm, c("imd_score","imd2019_score","imd_score_2019","score","imdscore"))
  )
}
attach_imd_from_input <- function(df) {
  nm <- names(df); cols <- detect_imd_cols_in_file(nm); co_num <- function(x) suppressWarnings(as.numeric(x))
  if (length(cols$decile) && !"imd_decile" %in% nm) df$imd_decile <- co_num(df[[cols$decile[1]]])
  if (length(cols$rank)   && !"imd_rank"   %in% nm) df$imd_rank   <- co_num(df[[cols$rank[1]]])
  if (length(cols$score)  && !"imd_score"  %in% nm) df$imd_score  <- co_num(df[[cols$score[1]]])
  if ("imd_decile" %in% names(df)) {
    df$imd_decile <- suppressWarnings(as.integer(round(df$imd_decile)))
    df$imd_decile[!(df$imd_decile %in% 1:10)] <- NA_integer_
  }
  matched <- sum(rowSums(cbind(
    if ("imd_decile" %in% names(df)) !is.na(df$imd_decile) else FALSE,
    if ("imd_rank"   %in% names(df)) !is.na(df$imd_rank)   else FALSE,
    if ("imd_score"  %in% names(df)) !is.na(df$imd_score)  else FALSE
  )) > 0)
  list(data = df, info = list(method = "in-file", matched = matched, available = nrow(df)))
}

# ------------------------------ Demo data --------------------------------------
make_demo <- function(n = 40) {
  la <- c("Shropshire","Telford and Wrekin","Herefordshire")
  ratings <- factor(sample(rating_levels, n, replace=TRUE, prob=c(0.12,0.55,0.22,0.06,0.05)), levels=rating_levels, ordered = TRUE)
  tibble::tibble(
    location_id = sprintf("1-%09d", sample(1e9, n)),
    provider_id = sprintf("1-%09d", sample(1e9, n)),
    provider_name = paste("Provider", sample(LETTERS, n, TRUE)),
    location_name = paste("Location", sample(LETTERS, n, TRUE)),
    address1 = paste(sample(1:250, n, TRUE), sample(c("High St","Station Rd","Market Sq"), n, TRUE)),
    town_city = sample(c("Shrewsbury","Telford","Ludlow","Oswestry"), n, TRUE),
    county = "Shropshire",
    postcode = paste0(sample(LETTERS, n, TRUE), sample(LETTERS, n, TRUE), sample(1:9, n, TRUE), " ",
                      sample(1:9, n, TRUE), sample(LETTERS, n, TRUE), sample(LETTERS, n, TRUE)),
    region = "West Midlands",
    icb_name = "Shropshire, Telford and Wrekin ICB",
    local_authority = sample(la, n, TRUE),
    latitude = 52.6 + rnorm(n, 0, 0.15),
    longitude = -2.6 + rnorm(n, 0, 0.15),
    overall_rating = ratings,
    rating_report_url = "https://www.cqc.org.uk/",
    last_inspection_date = as.Date("2024-01-01") + sample(0:600, n, TRUE),
    regulated_activity = sample(c("Treatment of disease","Diagnostic and screening","Surgical procedures"), n, TRUE),
    service_type = sample(c("Acute hospital","Ambulance service","Community services","Care home"), n, TRUE),
    gac_service_type_description = sample(c("Hospital services","Primary medical services","Community healthcare"), n, TRUE),
    ownership_type = sample(c("NHS Body","Independent","Local authority"), n, TRUE),
    organisation_type = sample(c("Provider","Location"), n, TRUE),
    beds = sample(c(NA, 10, 25, 50, 100, 150), n, TRUE),
    on_site_services = sample(c("Outpatients; Diagnostics","Maternity, A&E","Rehab; Imaging",""), n, TRUE),
    phone = sample(c("01743 123456","01952 654321",""), n, TRUE),
    website = sample(c("https://example.org",""), n, TRUE),
    domain = "Overall",
    imd_decile = sample(1:10, n, TRUE)
  )
}

# ------------------------------ Load data --------------------------------------
loaded <- {
  if (!is.na(paths_cqc$path)) read_cqc_csv(paths_cqc$path) else list(data=NULL, problems=NULL, used_demo=TRUE)
}
if (is.null(loaded$data)) {
  warn_banner <- TRUE
  cqc <- make_demo(sample(10:50, 1))
  problems_df <- NULL
  flags <- list(domain_present = TRUE, domain_kept = nrow(cqc), dedup_dropped = 0L, rating_source = "demo")
} else {
  warn_banner <- FALSE
  cqc <- loaded$data
  problems_df <- loaded$problems
  flags <- loaded$flags
}
imd_infile <- attach_imd_from_input(cqc); cqc <- imd_infile$data; imd_info <- imd_infile$info
has_col <- function(nm) nm %in% names(cqc)
service_col_priority <- c("regulated_activity","service_type","gac_service_type_description")
available_service_col <- { tmp <- intersect(service_col_priority, names(cqc)); if (length(tmp)) tmp[1] else NULL }

# ------------------------------ LSOA loader ------------------------------------
.detect_lsoa_id_name <- function(g) {
  nm <- names(g)
  idcand <- c("LSOA21CD","LSOA11CD","lsoa21cd","lsoa11cd","LSOA_CODE","lsoa_code","code")
  namecand <- c("LSOA21NM","LSOA11NM","lsoa21nm","lsoa11nm","LSOA_NAME","lsoa_name","name")
  id <- nm[match(intersect(tolower(nm), tolower(idcand))[1], tolower(nm))]
  nam <- nm[match(intersect(tolower(nm), tolower(namecand))[1], tolower(nm))]
  list(id = id %||% nm[1], name = nam %||% nm[1])
}
read_lsoa_any <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("gpkg","geojson","json")) {
    return(tryCatch(sf::st_read(path, quiet = TRUE), error = function(e) NULL))
  } else if (ext == "shp") {
    return(tryCatch(sf::st_read(path, quiet = TRUE), error = function(e) NULL))
  } else {
    tryCatch(sf::st_read(path, quiet = TRUE), error = function(e) NULL)
  }
}
load_lsoa <- function(path) {
  if (!nzchar(path) || is.na(path)) return(list(g=NULL, id_field=NA_character_, name_field=NA_character_, msg="LSOA file not found in repo"))
  g <- read_lsoa_any(path)
  if (is.null(g)) return(list(g=NULL, id_field=NA_character_, name_field=NA_character_, msg="Failed to read LSOA geometry"))
  suppressWarnings({ g <- sf::st_make_valid(g) })
  if (!is.na(sf::st_crs(g)$epsg) && sf::st_crs(g)$epsg != 4326) g <- sf::st_transform(g, 4326)
  g <- tryCatch(sf::st_simplify(g, dTolerance = 0.0005, preserveTopology = TRUE), error = function(e) g)
  f <- .detect_lsoa_id_name(g)
  list(g = g, id_field = f$id, name_field = f$name, msg="OK")
}
lsoa_loaded <- load_lsoa(paths_lsoa$path); lsoa <- lsoa_loaded$g
lsoa_id_field <- lsoa_loaded$id_field; lsoa_name_field <- lsoa_loaded$name_field

# ------------------------------ Banner UI --------------------------------------
alert_block <- function(text, severity = c("info","warning","danger","success")) {
  severity <- match.arg(severity)
  div(class = paste("alert", paste0("alert-", severity)), role = "alert", HTML(text))
}
banner_ui <- NULL
{
  msgs <- c()
  # File resolution diagnostics
  msgs <- c(msgs, glue::glue("CQC source: <em>{paths_cqc$source}</em> &nbsp;({htmltools::htmlEscape(paths_cqc$path %||% 'demo')})"))
  msgs <- c(msgs, glue::glue("LSOA source: <em>{paths_lsoa$source}</em> &nbsp;({htmltools::htmlEscape(paths_lsoa$path %||% 'not set')})"))
  if (!warn_banner && isTRUE(flags$domain_present)) msgs <- c(msgs, glue::glue("Filtered to domain = 'Overall' (kept {scales::comma(flags$domain_kept)} rows)"))
  dd <- as.integer(flags$dedup_dropped %||% 0L); if (!is.na(dd) && dd > 0L) msgs <- c(msgs, glue::glue("De-duplicated locations (dropped {scales::comma(dd)} rows)"))
  if (!is.null(flags$rating_source)) msgs <- c(msgs, glue::glue("Overall rating source: <em>{flags$rating_source}</em>."))
  msgs <- c(msgs, glue::glue("IMD (in-file for locations): matched {scales::comma(imd_info$matched)} of {scales::comma(imd_info$available)} rows."))
  msgs <- c(msgs, "Map highlights LSOAs by <em>derived</em> IMD decile (median of CQC locations within each LSOA). Provide official LSOA→IMD to switch to canonical deciles.")
  if (length(msgs)) banner_ui <- alert_block(paste(msgs, collapse = "<br/>"), severity = "info")
}

# ------------------------------ UI --------------------------------------------
ui <- page_navbar(
  title = "CQC Locations Explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly",
                   base_font = font_google("Inter"), heading_font = font_google("Inter")),
  navbar_options = navbar_options(collapsible = TRUE),
  sidebar = sidebar(
    width = 400, open = "open", title = "Filters",
    if (warn_banner || identical(paths_cqc$source, "demo")) alert_block("Demo mode: CQC CSV not found in repo/flags — synthetic data loaded.", severity = "warning"),
    if (is.null(lsoa)) alert_block("LSOA geometry not found in repo/flags — polygons disabled (map points still render).", severity = "warning"),
    banner_ui,
    textInput("q", label = "Search name / address / provider", placeholder = "Type to filter..."),
    uiOutput("rating_ui"), uiOutput("service_ui"), uiOutput("activity_ui"),
    uiOutput("ownership_ui"), uiOutput("orgtype_ui"), uiOutput("la_ui"),
    uiOutput("icb_ui"), uiOutput("ics_ui"), uiOutput("date_ui"), uiOutput("beds_ui"),
    uiOutput("imd_ui"),
    checkboxInput("coord_only", "Show only rows with coordinates", value = FALSE),
    sliderInput("lsoa_range", "LSOA IMD highlight (derived deciles)", min = 1, max = 10, value = c(1,3), step = 1),
    div(class = "d-flex gap-2",
        actionButton("reset", "Reset filters", class = "btn btn-outline-secondary"),
        bookmarkButton(label = "Copy shareable link")),
    hr(),
    downloadButton("dl_csv", "Download filtered CSV","btn-primary"),
    downloadButton("dl_log",  "Download session log (JSON)","btn-outline-primary")
  ),
  nav_panel("Overview",
            layout_columns(col_widths = c(4,4,4),
                           card(body_padding = "lg", card_header("Locations"), uiOutput("kpi_locations")),
                           card(body_padding = "lg", card_header("Unique providers"), uiOutput("kpi_providers")),
                           card(body_padding = "lg", card_header("Share by rating"), uiOutput("kpi_share_rating"))
            ),
            layout_columns(col_widths = c(6,6),
                           card(card_header("Locations by overall rating"), uiOutput("rating_plot_ui")),
                           card(card_header("Top services / activities"), uiOutput("top_service_plot_ui"))
            )
  ),
  nav_panel("Map", card(card_header("Locations map"), uiOutput("map_ui"))),
  nav_panel("Table", card(card_header("Filtered data table"), uiOutput("table_ui"))),
  nav_panel("IMD vs Rating",
            card(
              card_header(
                "Overall rating vs IMD decile",
                div(class = "d-flex gap-3 align-items-center",
                    selectInput("imd_chart_type", label = NULL, width = "260px",
                                choices = c("Heatmap (share)" = "heat",
                                            "Bubble (count)" = "bubble",
                                            "Stacked (share)" = "stack"),
                                selected = "heat"),
                    checkboxInput("imd_exclude_nr", "Exclude 'Not rated'", value = TRUE),
                    uiOutput("imd_interactive_ui")
                )
              ),
              uiOutput("imd_plot_container")
            )
  ),
  nav_panel("Details",
            layout_columns(col_widths = c(7,5),
                           card(card_header("Select a row in the Table to populate"), uiOutput("details_main")),
                           card(card_header("Context & Links"), uiOutput("details_context"))
            )
  ),
  nav_panel("Diagnostics",
            layout_columns(col_widths = c(6,6),
                           card(card_header("Schema summary"), uiOutput("schema_ui")),
                           card(card_header("Readr parsing problems"), uiOutput("problems_ui"))),
            card(card_header("Session info"), verbatimTextOutput("session_info"))
  ),
  footer = div(class = "text-muted small p-3",
               HTML("<strong>No PII:</strong> phone/email masked. State is bookmarkable; share the URL."))
)

enableBookmarking("url")

# ------------------------------ Server -----------------------------------------
server <- function(input, output, session) {
  # Distinct values for filters
  distinct_vals <- reactive({
    list(
      rating = if (has_col("overall_rating")) sort(na.omit(unique(as.character(cqc$overall_rating)))) else character(0),
      service = if (!is.null(available_service_col)) sort(na.omit(unique(as.character(cqc[[available_service_col]])))) else character(0),
      regulated_activity = if (has_col("regulated_activity")) sort(na.omit(unique(as.character(cqc$regulated_activity)))) else character(0),
      ownership_type = if (has_col("ownership_type")) sort(na.omit(unique(as.character(cqc$ownership_type)))) else character(0),
      organisation_type = if (has_col("organisation_type")) sort(na.omit(unique(as.character(cqc$organisation_type)))) else character(0),
      local_authority = if (has_col("local_authority")) sort(na.omit(unique(as.character(cqc$local_authority)))) else character(0),
      icb_name = if (has_col("icb_name")) sort(na.omit(unique(as.character(cqc$icb_name)))) else character(0),
      ics_name = if (has_col("ics_name")) sort(na.omit(unique(as.character(cqc$ics_name)))) else character(0),
      imd_decile = if ("imd_decile" %in% names(cqc)) sort(na.omit(unique(as.integer(round(cqc$imd_decile))))) else integer(0)
    )
  })
  output$rating_ui <- renderUI({ if (!has_col("overall_rating")) return(NULL); selectizeInput("rating", "Overall rating", distinct_vals()$rating, multiple = TRUE) })
  output$service_ui <- renderUI({ if (is.null(available_service_col)) return(NULL); selectizeInput("service", paste0("Service type (", available_service_col, ")"), distinct_vals()$service, multiple = TRUE) })
  output$activity_ui <- renderUI({ if (!has_col("regulated_activity")) return(NULL); selectizeInput("regulated_activity", "Regulated activity", distinct_vals()$regulated_activity, multiple = TRUE) })
  output$ownership_ui <- renderUI({ if (!has_col("ownership_type")) return(NULL); selectizeInput("ownership_type", "Ownership type", distinct_vals()$ownership_type, multiple = TRUE) })
  output$orgtype_ui <- renderUI({ if (!has_col("organisation_type")) return(NULL); selectizeInput("organisation_type", "Organisation type", distinct_vals()$organisation_type, multiple = TRUE) })
  output$la_ui <- renderUI({ if (!has_col("local_authority")) return(NULL); selectizeInput("local_authority", "Local authority", distinct_vals()$local_authority, multiple = TRUE) })
  output$icb_ui <- renderUI({ if (!has_col("icb_name")) return(NULL); selectizeInput("icb_name", "ICB name", distinct_vals()$icb_name, multiple = TRUE) })
  output$ics_ui <- renderUI({ if (!has_col("ics_name")) return(NULL); selectizeInput("ics_name", "ICS name", distinct_vals()$ics_name, multiple = TRUE) })
  output$date_ui <- renderUI({
    if (!has_col("last_inspection_date")) return(NULL)
    rng <- range(na.omit(as.Date(cqc$last_inspection_date))); if (!is.finite(rng[1])) return(NULL)
    dateRangeInput("date", "Last inspection date", start = rng[1], end = rng[2])
  })
  output$beds_ui <- renderUI({
    if (!has_col("beds")) return(NULL)
    rng <- suppressWarnings(range(na.omit(as.numeric(cqc$beds)))); if (!is.finite(rng[1])) return(NULL)
    sliderInput("beds", "Beds (min–max)", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
  })
  output$imd_ui <- renderUI({
    if (!"imd_decile" %in% names(cqc)) return(NULL)
    sliderInput("imd_decile", "IMD decile (1=most deprived, 10=least)", min = 1, max = 10, value = c(1,10), step = 1)
  })
  output$imd_interactive_ui <- renderUI({
    if (!"imd_decile" %in% names(cqc)) return(NULL)
    as.tags(checkboxInput("imd_interactive", "Interactive", value = TRUE))
  })
  output$imd_plot_container <- renderUI({
    if (!"imd_decile" %in% names(cqc)) return(HTML("<div class='text-muted p-3'>IMD not available in input file.</div>"))
    if (isTRUE(input$imd_interactive) && "plotly" %in% .packages()) plotlyOutput("imd_plotly", height = 460) else plotOutput("imd_plot", height = 460)
  })
  observeEvent(input$reset, {
    updateTextInput(session, "q", value = "")
    for (id in c("rating","service","regulated_activity","ownership_type","organisation_type","local_authority","icb_name","ics_name"))
      if (!is.null(input[[id]])) updateSelectizeInput(session, id, selected = character(0))
    if (!is.null(input$date)) updateDateRangeInput(session, "date")
    if (!is.null(input$beds)) updateSliderInput(session, "beds")
    if (!is.null(input$imd_decile)) updateSliderInput(session, "imd_decile", value = c(1,10))
    if (!is.null(input$lsoa_range)) updateSliderInput(session, "lsoa_range", value = c(1,3))
    updateCheckboxInput(session, "coord_only", value = FALSE)
  })
  
  filtered <- reactive({
    df <- cqc
    if (!is.null(input$q) && nzchar(trimws(input$q))) {
      pat <- regex(input$q, ignore_case = TRUE)
      fields <- intersect(c("location_name","provider_name","address1","address2","town_city","postcode"), names(df))
      if (length(fields)) {
        df <- df |>
          dplyr::mutate(.hit = do.call(pmax, c(lapply(fields, function(f) as.integer(grepl(pat, df[[f]]))), list(0L)))) |>
          dplyr::filter(.hit == 1) |>
          dplyr::select(-.hit)
      }
    }
    pick <- function(col) if (!is.null(input[[col]]) && length(input[[col]])>0 && col %in% names(df)) input[[col]] else NULL
    if (!is.null(pick("rating")) && "overall_rating" %in% names(df)) df <- df |> dplyr::filter(as.character(overall_rating) %in% input$rating)
    if (!is.null(pick("service")) && !is.null(available_service_col)) df <- df |> dplyr::filter(.data[[available_service_col]] %in% input$service)
    if (!is.null(pick("regulated_activity"))) df <- df |> dplyr::filter(regulated_activity %in% input$regulated_activity)
    if (!is.null(pick("ownership_type"))) df <- df |> dplyr::filter(ownership_type %in% input$ownership_type)
    if (!is.null(pick("organisation_type"))) df <- df |> dplyr::filter(organisation_type %in% input$organisation_type)
    if (!is.null(pick("local_authority"))) df <- df |> dplyr::filter(local_authority %in% input$local_authority)
    if (!is.null(pick("icb_name"))) df <- df |> dplyr::filter(icb_name %in% input$icb_name)
    if (!is.null(pick("ics_name"))) df <- df |> dplyr::filter(ics_name %in% input$ics_name)
    if (!is.null(input$date) && length(input$date)==2 && "last_inspection_date" %in% names(df))
      df <- df |> dplyr::filter(!is.na(last_inspection_date),
                                last_inspection_date >= input$date[1],
                                last_inspection_date <= input$date[2])
    if (!is.null(input$beds) && length(input$beds)==2 && "beds" %in% names(df))
      df <- df |> dplyr::filter(!is.na(beds), beds >= input$beds[1], beds <= input$beds[2])
    if (!is.null(input$imd_decile) && "imd_decile" %in% names(df)) {
      rng <- as.integer(input$imd_decile)
      df <- df |> dplyr::filter(is.na(imd_decile) | (imd_decile >= rng[1] & imd_decile <= rng[2]))
    }
    if (isTRUE(input$coord_only) && all(c("latitude","longitude") %in% names(df)))
      df <- df |> dplyr::filter(!is.na(latitude), !is.na(longitude))
    df
  })
  
  # ---------- Overview KPIs & plots --------------------------------------------
  output$kpi_locations <- renderUI({
    df <- filtered()
    div(class = "display-5 fw-bold", if (nrow(df)) format(nrow(df), big.mark=",") else "0")
  })
  output$kpi_providers <- renderUI({
    df <- filtered()
    name_col <- if (has_col("provider_name")) "provider_name" else if (has_col("provider_id")) "provider_id" else NULL
    val <- if (!is.null(name_col)) dplyr::n_distinct(df[[name_col]]) else NA_integer_
    div(class = "display-5 fw-bold", ifelse(is.na(val), "—", format(val, big.mark=",")))
  })
  output$kpi_share_rating <- renderUI({
    if (!has_col("overall_rating")) return(HTML("<span class='text-muted'>No rating column</span>"))
    df <- filtered(); tot <- nrow(df); if (tot==0) return(HTML("<span class='text-muted'>No rows</span>"))
    pct <- df |> dplyr::mutate(overall_rating = factor(normalize_text_to_levels(overall_rating), levels = rating_levels)) |>
      dplyr::count(overall_rating, name="n") |>
      dplyr::mutate(p = round(100*n/sum(n)))
    pct <- pct |> dplyr::arrange(match(as.character(overall_rating), rating_levels))
    spans <- paste0("<span class='me-2'><strong>", pct$p, "%</strong> ", pct$overall_rating, "</span>")
    HTML(paste(spans, collapse = " "))
  })
  output$rating_plot_ui <- renderUI({
    if (!has_col("overall_rating")) return(HTML("<div class='text-muted p-3'>Rating column not available.</div>"))
    plotOutput("rating_plot", height = 350)
  })
  output$top_service_plot_ui <- renderUI({
    if (is.null(available_service_col)) return(HTML("<div class='text-muted p-3'>No service/activity column available.</div>"))
    plotOutput("top_service_plot", height = 350)
  })
  output$rating_plot <- renderPlot({
    req(has_col("overall_rating"))
    df <- filtered(); if (!nrow(df)) return()
    df |>
      dplyr::mutate(overall_rating = factor(normalize_text_to_levels(overall_rating), levels = rating_levels)) |>
      dplyr::count(overall_rating) |>
      dplyr::mutate(p = n/sum(n)) |>
      ggplot2::ggplot(ggplot2::aes(x = overall_rating, y = n, fill = overall_rating)) +
      ggplot2::geom_col() +
      ggplot2::geom_text(ggplot2::aes(label = scales::percent(p, accuracy = 1)), vjust = -0.2) +
      ggplot2::scale_fill_manual(values = unname(rating_palette[rating_levels]), limits = rating_levels, drop = FALSE) +
      ggplot2::labs(x = NULL, y = "Locations", caption = "Ordered by CQC scale") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 1),
                     axis.text.x = ggplot2::element_text(angle=15, hjust=1),
                     legend.position = "none")
  })
  output$top_service_plot <- renderPlot({
    req(!is.null(available_service_col))
    df <- filtered(); if (!nrow(df)) return()
    df |>
      dplyr::filter(!is.na(.data[[available_service_col]]), trimws(.data[[available_service_col]])!="") |>
      dplyr::count(.data[[available_service_col]], name="n", sort=TRUE) |>
      dplyr::slice_head(n=12) |>
      ggplot2::ggplot(ggplot2::aes(x = reorder(.data[[available_service_col]], n), y = n)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Locations") +
      ggplot2::theme_minimal(base_size = 12)
  })
  
  # ---------- IMD vs Rating -----------------------------------------------------
  rating_to_num <- function(x) { rec <- c("Inadequate"=1, "Requires improvement"=2, "Good"=3, "Outstanding"=4); as.integer(unname(rec[as.character(x)])) }
  imd_chart_data <- reactive({
    df <- filtered()
    if (!nrow(df) || !"imd_decile" %in% names(df) || !"overall_rating" %in% names(df)) {
      return(list(cells = NULL, corr = list(tau=NA, rho=NA, n=0)))
    }
    df <- df |>
      dplyr::mutate(
        decile = as.integer(imd_decile),
        rating = factor(normalize_text_to_levels(overall_rating), levels = rating_levels, ordered = TRUE)
      ) |>
      dplyr::filter(!is.na(decile), decile >= 1, decile <= 10)
    if (isTRUE(input$imd_exclude_nr)) df <- dplyr::filter(df, rating != "Not rated")
    rn <- rating_to_num(df$rating)
    keep <- !is.na(rn) & !is.na(df$decile)
    tau <- suppressWarnings(stats::cor(df$decile[keep], rn[keep], method = "kendall"))
    rho <- suppressWarnings(stats::cor(df$decile[keep], rn[keep], method = "spearman"))
    cells <- df |>
      dplyr::count(decile, rating, name = "n") |>
      dplyr::group_by(decile) |>
      dplyr::mutate(p = n / sum(n)) |>
      dplyr::ungroup()
    list(cells = cells, corr = list(tau = unname(tau), rho = unname(rho), n = sum(keep)))
  })
  theme_imd <- function() {
    ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(linewidth = 0.4, colour = "#d9d9d9"),
        panel.grid.minor = ggplot2::element_line(linewidth = 0.2, colour = "#eeeeee"),
        plot.caption     = ggplot2::element_text(hjust = 1, colour = "#555"),
        axis.text.y      = ggplot2::element_text(size = 11),
        axis.text.x      = ggplot2::element_text(size = 11)
      )
  }
  build_bubble <- function(tbl, corr) {
    ggplot2::ggplot(tbl, ggplot2::aes(x = factor(decile), y = rating)) +
      ggplot2::geom_point(
        ggplot2::aes(size = n, fill = rating),
        shape = 21, colour = "black", stroke = 0.3, alpha = 0.9
      ) +
      ggplot2::scale_size_area(max_size = 16, guide = ggplot2::guide_legend(title = "Locations")) +
      ggplot2::scale_fill_manual(values = unname(rating_palette[levels(tbl$rating)]), drop = FALSE) +
      ggplot2::labs(
        x = "IMD decile (1 = most deprived, 10 = least)",
        y = "Overall rating",
        caption = glue::glue("Effect size on filtered data (excl. 'Not rated' if toggled): Kendall's τ = {scales::number(corr$tau, accuracy=0.01)}, Spearman's ρ = {scales::number(corr$rho, accuracy=0.01)}; N = {corr$n}.")
      ) +
      theme_imd() +
      ggplot2::guides(fill = "none")
  }
  build_heat <- function(tbl, corr) {
    ggplot2::ggplot(tbl, ggplot2::aes(x = factor(decile), y = rating, fill = p)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.4) +
      ggplot2::geom_text(ggplot2::aes(label = scales::percent(p, accuracy = 1)),
                         color = "black", size = 3) +
      ggplot2::scale_fill_gradient(
        name = "Share within decile",
        limits = c(0, 1), labels = scales::percent, low = "#f7fbff", high = "#08306b"
      ) +
      ggplot2::labs(
        x = "IMD decile (1 = most deprived, 10 = least)",
        y = "Overall rating",
        caption = glue::glue("Cell colour = share of rating within decile. Kendall's τ = {scales::number(corr$tau, accuracy=0.01)}, Spearman's ρ = {scales::number(corr$rho, accuracy=0.01)}; N = {corr$n}.")
      ) +
      theme_imd()
  }
  build_stack <- function(tbl, corr) {
    ggplot2::ggplot(tbl, ggplot2::aes(x = factor(decile), y = p, fill = rating)) +
      ggplot2::geom_col(width = 0.9) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::scale_fill_manual(values = unname(rating_palette[levels(tbl$rating)]), drop = FALSE) +
      ggplot2::labs(
        x = "IMD decile (1 = most deprived, 10 = least)",
        y = "Share within decile",
        fill = "Rating",
        caption = glue::glue("Each bar sums to 100%. Kendall's τ = {scales::number(corr$tau, accuracy=0.01)}, Spearman's ρ = {scales::number(corr$rho, accuracy=0.01)}; N = {corr$n}.")
      ) +
      theme_imd()
  }
  build_imd_plot <- reactive({
    stuff <- imd_chart_data()
    tbl <- stuff$cells; corr <- stuff$corr
    if (is.null(tbl) || !nrow(tbl)) {
      return(ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::labs(caption = "No data for current filters.") + theme_imd())
    }
    tbl$rating <- factor(as.character(tbl$rating), levels = rating_levels, ordered = TRUE)
    switch(input$imd_chart_type,
           "bubble" = build_bubble(tbl, corr),
           "stack"  = build_stack(tbl, corr),
           build_heat(tbl, corr))
  })
  output$imd_plot <- renderPlot({ build_imd_plot() })
  output$imd_plotly <- plotly::renderPlotly({
    gg <- build_imd_plot()
    suppressWarnings(plotly::ggplotly(gg, tooltip = c("x","y","fill","size","label")))
  })
  
  # ------------------------------ Map ------------------------------------------
  output$map_ui <- renderUI({
    if (!all(c("latitude","longitude") %in% names(cqc)))
      return(HTML("<div class='alert alert-info m-3'>No coordinates found. Include <code>latitude</code> and <code>longitude</code> columns to enable mapping.</div>"))
    leafletOutput("map", height = 600)
  })
  
  lsoa_subset <- reactive({
    req(!is.null(lsoa), all(c("latitude","longitude") %in% names(cqc)))
    df <- filtered() |> dplyr::filter(!is.na(latitude), !is.na(longitude))
    if (!nrow(df)) return(NULL)
    pts <- sf::st_as_sf(df, coords = c("longitude","latitude"), crs = 4326, remove = FALSE)
    keep_cols <- unique(c(lsoa_id_field, lsoa_name_field))
    poly_min  <- lsoa[, keep_cols, drop = FALSE]
    j <- suppressWarnings(sf::st_join(pts, poly_min, left = TRUE, join = sf::st_within))
    codes <- unique(na.omit(j[[lsoa_id_field]]))
    if (length(codes) == 0) return(NULL)
    sub <- lsoa[lsoa[[lsoa_id_field]] %in% codes, ]
    if ("imd_decile" %in% names(df)) {
      agg <- j |>
        sf::st_drop_geometry() |>
        dplyr::mutate(imd_decile = suppressWarnings(as.integer(round(as.numeric(imd_decile))))) |>
        dplyr::filter(!is.na(.data[[lsoa_id_field]]), !is.na(imd_decile)) |>
        dplyr::group_by(.data[[lsoa_id_field]]) |>
        dplyr::summarise(.derived_imd_decile = as.integer(round(stats::median(imd_decile))), .groups="drop")
      sub <- dplyr::left_join(sub, agg, by = setNames(lsoa_id_field, lsoa_id_field))
    }
    list(polys = sub, joined_pts = j)
  })
  
  output$map <- renderLeaflet({
    req(all(c("latitude","longitude") %in% names(cqc)))
    df <- filtered() |> dplyr::filter(!is.na(latitude), !is.na(longitude))
    df$rating_chr <- if ("overall_rating" %in% names(df)) {
      rc <- normalize_text_to_levels(as.character(df$overall_rating))
      rc[is.na(rc) | trimws(rc)==""] <- "Not rated"; rc
    } else rep("Not rated", nrow(df))
    df$rating_chr <- factor(df$rating_chr, levels = rating_levels, ordered = TRUE)
    
    pal <- leaflet::colorFactor(
      palette = unname(rating_palette[rating_levels]),
      domain  = factor(rating_levels, levels = rating_levels, ordered = TRUE),
      ordered = TRUE,
      na.color = unname(rating_palette["Not rated"])
    )
    
    m <- leaflet() |> addProviderTiles(providers$CartoDB.Positron)
    sub <- lsoa_subset()
    if (!is.null(sub)) {
      polys <- sub$polys
      labels_outline <- if (!is.null(polys) && nrow(polys) > 0 && lsoa_name_field %in% names(polys)) polys[[lsoa_name_field]] else NULL
      if (!is.null(polys) && nrow(polys) > 0) {
        m <- m |>
          addPolygons(data = polys, group = "LSOA outlines",
                      fill = FALSE, color = "#666666", weight = 0.5, opacity = 0.8,
                      smoothFactor = 0.5,
                      highlightOptions = highlightOptions(weight = 1.5, color = "#333333", bringToFront = TRUE),
                      label = labels_outline)
      }
      if (!is.null(polys) && nrow(polys) > 0 && ".derived_imd_decile" %in% names(polys) && !is.null(input$lsoa_range)) {
        rng <- as.integer(input$lsoa_range)
        hi <- tryCatch(polys[!is.na(polys$.derived_imd_decile) & polys$.derived_imd_decile >= rng[1] & polys$.derived_imd_decile <= rng[2], ], error = function(e) NULL)
        if (!is.null(hi) && nrow(hi) > 0) {
          labels_hi <- if (lsoa_name_field %in% names(hi)) paste0(hi[[lsoa_name_field]], " — derived IMD decile: ", hi$.derived_imd_decile) else paste0("LSOA — derived IMD decile: ", hi$.derived_imd_decile)
          m <- m |>
            addPolygons(data = hi, group = "LSOA IMD highlight (derived)",
                        fill = TRUE, fillColor = "#b2182b", fillOpacity = 0.5,
                        color = "#67000d", weight = 1.6, opacity = 0.95,
                        smoothFactor = 0.5,
                        label = labels_hi)
        }
      }
    }
    if (nrow(df) > 0) {
      pname <- if (has_col("provider_name")) df$provider_name else rep("", nrow(df))
      addr1 <- if (has_col("address1")) df$address1 else rep("", nrow(df))
      town  <- if (has_col("town_city")) df$town_city else rep("", nrow(df))
      pc    <- if (has_col("postcode")) df$postcode else rep("", nrow(df))
      link  <- if (has_col("rating_report_url"))
        ifelse(!is.na(df$rating_report_url) & df$rating_report_url!="", paste0("<a target=\"_blank\" href=\"", df$rating_report_url, "\">CQC report</a>"), "")
      else rep("", nrow(df))
      lname <- ifelse(!is.na(df$location_name), df$location_name, "Unknown")
      imd_badge <- if ("imd_decile" %in% names(df)) paste0("IMD decile: ", ifelse(is.na(df$imd_decile), "—", as.integer(df$imd_decile))) else "IMD decile: —"
      popup <- paste0("<strong>", lname, "</strong><br/>",
                      "Provider: ", pname, "<br/>",
                      "Rating: ", as.character(df$rating_chr), "<br/>",
                      imd_badge, "<br/>",
                      addr1, ", ", town, " ", pc, "<br/>", link)
      m <- m |>
        addCircleMarkers(data = df, lng = ~longitude, lat = ~latitude, group = "CQC locations",
                         radius = 6, weight = 1, opacity = 1, stroke = TRUE,
                         color = ~pal(rating_chr), fillColor = ~pal(rating_chr), fillOpacity = 0.9,
                         popup = popup, label = ~paste("Rating:", as.character(rating_chr)))
      m <- m |>
        fitBounds(lng1 = min(df$longitude), lat1 = min(df$latitude),
                  lng2 = max(df$longitude), lat2 = max(df$latitude))
    } else {
      m <- m |> setView(lng = -2.75, lat = 52.7, zoom = 8)
    }
    m |>
      addLegend("bottomright",
                pal = pal,
                values = factor(rating_levels, levels = rating_levels, ordered = TRUE),
                title = "Overall rating", opacity = 1) |>
      addLayersControl(
        overlayGroups = c("CQC locations",
                          if (!is.null(sub)) c("LSOA outlines", if (".derived_imd_decile" %in% names(sub$polys)) "LSOA IMD highlight (derived)") else NULL),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  # ---------------------------- /Map -------------------------------------------
  
  # Table & details
  output$table_ui <- renderUI({ DT::dataTableOutput("tbl") })
  output$tbl <- DT::renderDataTable({
    df <- filtered()
    if ("phone" %in% names(df)) df$phone <- mask_text(df$phone)
    if ("website" %in% names(df)) df$website <- as.character(df$website)
    if ("email" %in% names(df)) df$email <- mask_text(df$email)
    show_cols <- c("imd_decile","imd_rank","imd_score","imd_join_method"); if (!"imd_join_method" %in% names(df)) df$imd_join_method <- "in-file"
    DT::datatable(
      df, filter = "top",
      extensions = "Buttons",
      options = list(dom = 'Bfrtip',
                     buttons = list('copy','csv','excel', list(extend = 'colvis', text = 'Columns')),
                     pageLength = 25, scrollX = TRUE,
                     columnDefs = list(list(visible = TRUE, targets = which(names(df) %in% show_cols) - 1))
      ),
      selection = "single", rownames = FALSE
    )
  })
  selected_row <- reactive({ s <- input$tbl_rows_selected; if (length(s) != 1) return(NULL); filtered()[s, , drop = FALSE] })
  output$details_main <- renderUI({
    row <- selected_row(); if (is.null(row)) return(HTML("<div class='text-muted p-3'>Select a row in the Table tab.</div>"))
    addr <- paste(na.omit(c(row$address1, row$address2, row$town_city, row$county, row$postcode)), collapse = ", ")
    imd_line <- if ("imd_decile" %in% names(row)) paste0("IMD decile: ", ifelse(is.na(row$imd_decile), "—", as.integer(row$imd_decile))) else NULL
    tags$div(class = "p-3",
             tags$h4(ifelse(!is.na(row$location_name), row$location_name, "Location")),
             tags$p(tags$strong("Provider:"), ifelse(has_col("provider_name"), row$provider_name, "—")),
             if (has_col("overall_rating")) tags$p(tags$strong("Overall rating:"), as.character(row$overall_rating)) else NULL,
             if (!is.null(imd_line)) tags$p(tags$strong("Deprivation:"), imd_line),
             tags$p(tags$strong("Address:"), addr))
  })
  output$details_context <- renderUI({
    row <- selected_row(); if (is.null(row)) return(NULL)
    report_link <- if (has_col("rating_report_url") && !is.na(row$rating_report_url) && row$rating_report_url != "") {
      tags$a(href = row$rating_report_url, target = "_blank", "Open CQC report")
    } else NULL
    tags$div(class = "p-3",
             if (has_col("last_inspection_date") && !is.na(row$last_inspection_date))
               tags$p(tags$strong("Last inspection:"), format(row$last_inspection_date, "%d %b %Y")) else NULL,
             if ("imd_join_method" %in% names(row))
               tags$p(tags$strong("IMD source:"), ifelse(is.na(row$imd_join_method), "in-file", row$imd_join_method)) else NULL,
             report_link)
  })
  
  output$dl_csv <- downloadHandler(
    filename = function() paste0("cqc_filtered_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) { readr::write_csv(filtered(), file) }
  )
  
  session_log <- reactiveVal(list())
  snapshot_inputs <- function() {
    fields <- c("q","rating","service","regulated_activity","ownership_type",
                "organisation_type","local_authority","icb_name","ics_name","coord_only")
    vals <- lapply(fields, function(f) input[[f]]); names(vals) <- fields
    if (!is.null(input$date)) vals$date <- as.character(input$date)
    if (!is.null(input$imd_decile)) vals$imd_decile <- as.character(input$imd_decile)
    if (!is.null(input$lsoa_range)) vals$lsoa_range <- as.character(input$lsoa_range)
    vals$ts <- as.character(Sys.time()); vals$rows <- nrow(filtered()); vals
  }
  observeEvent(filtered(), { log <- session_log(); session_log(append(log, list(snapshot_inputs()))) }, ignoreInit = TRUE)
  output$dl_log <- downloadHandler(
    filename = function() paste0("cqc_session_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"),
    content = function(file) { jsonlite::write_json(session_log(), path = file, auto_unbox = TRUE, pretty = TRUE) }
  )
  output$schema_ui <- renderUI({ sm <- schema_summary(cqc); DT::datatable(sm, options = list(pageLength = 10, dom = 'tip'), rownames = FALSE) })
  output$problems_ui <- renderUI({
    if (is.null(problems_df) || nrow(problems_df) == 0) HTML("<div class='text-muted p-3'>No readr parsing problems.</div>")
    else DT::datatable(problems_df, options = list(pageLength = 5, scrollX = TRUE))
  })
  output$session_info <- renderText({ paste0(capture.output(sessionInfo()), collapse = "\n") })
}

# ------------------------------ Run app ----------------------------------------
shinyApp(ui, server)

  