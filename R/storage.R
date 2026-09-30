# Storage layer ---------------------------------------------------------------
# All league state lives in three tables: managers, picks, game_log.
# Two backends:
#   * CSV files in FHL_DATA_DIR (default "data/") -- fine for running locally or
#     on a server with a persistent disk (Posit Connect, a VM, Shiny Server).
#   * Google Sheets, when FHL_GSHEET_ID is set -- use this on shinyapps.io or
#     Posit Connect Cloud, whose disks are wiped whenever the app restarts.
#     Authenticate with a service account: FHL_GS_KEY is its key file's path or
#     the key JSON itself. Share the sheet with the service account's email as
#     an Editor. Step-by-step setup: DEPLOY.md.

DATA_DIR <- Sys.getenv("FHL_DATA_DIR", "data")
GS_ID    <- Sys.getenv("FHL_GSHEET_ID", "")
USE_GS   <- nzchar(GS_ID)

SCHEMAS <- list(
  managers = list(manager = "character", team_name = "character",
                  draft_order = "integer", partner = "character"),
  picks    = list(pick = "integer", manager = "character", player = "character",
                  is_new = "logical", league_team = "character",
                  picked_at = "character"),
  game_log = list(id = "character", game_date = "character",
                  player = "character", goals = "integer",
                  assists = "integer", pim = "integer",
                  entered_at = "character")
)

DEFAULT_MANAGERS <- data.frame(
  manager     = paste("Manager", 1:8),
  team_name   = paste("Team", 1:8),
  draft_order = 1:8,
  partner     = NA_character_
)

# Coerce a data frame to the schema: add missing columns, drop extras, fix types.
conform <- function(df, name) {
  sch <- SCHEMAS[[name]]
  if (is.null(df)) df <- data.frame()
  out <- lapply(names(sch), function(col) {
    v <- if (col %in% names(df)) df[[col]] else rep(NA, nrow(df))
    if (sch[[col]] == "logical") v <- toupper(as.character(v)) %in% c("TRUE", "T", "1")
    else v <- suppressWarnings(methods::as(v, sch[[col]]))
    v
  })
  names(out) <- names(sch)
  as.data.frame(out, stringsAsFactors = FALSE)
}

# ---- CSV backend -------------------------------------------------------------
csv_path <- function(name) file.path(DATA_DIR, paste0(name, ".csv"))

csv_read <- function(name) {
  p <- csv_path(name)
  if (!file.exists(p)) return(conform(NULL, name))
  conform(utils::read.csv(p, colClasses = "character", na.strings = ""), name)
}

csv_write <- function(name, df) {
  dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(csv_path(name), ".tmp")
  utils::write.csv(conform(df, name), tmp, row.names = FALSE, na = "")
  invisible(file.rename(tmp, csv_path(name)))   # atomic-ish replace
}

# ---- Google Sheets backend ---------------------------------------------------
gs_cache <- new.env()
GS_TTL <- 20  # seconds; keeps us well under the Sheets API quota

gs_init <- function() {
  if (!requireNamespace("googlesheets4", quietly = TRUE))
    stop("Package googlesheets4 is required when FHL_GSHEET_ID is set.")
  # FHL_GS_KEY is either a path to the service-account key file or the key's
  # JSON itself (for hosts like Posit Connect Cloud where it's a secret variable).
  key <- trimws(Sys.getenv("FHL_GS_KEY", ""))
  if (nzchar(key)) googlesheets4::gs4_auth(path = key)
  existing <- googlesheets4::sheet_names(GS_ID)
  for (nm in names(SCHEMAS)) if (!nm %in% existing)
    googlesheets4::sheet_write(conform(NULL, nm), GS_ID, sheet = nm)
}

gs_read <- function(name) {
  hit <- gs_cache[[name]]
  if (!is.null(hit) && difftime(Sys.time(), hit$at, units = "secs") < GS_TTL)
    return(hit$df)
  df <- conform(googlesheets4::read_sheet(GS_ID, sheet = name, col_types = "c"), name)
  gs_cache[[name]] <- list(df = df, at = Sys.time())
  df
}

gs_write <- function(name, df) {
  df <- conform(df, name)
  googlesheets4::sheet_write(df, GS_ID, sheet = name)
  gs_cache[[name]] <- list(df = df, at = Sys.time())
}

# ---- Public API --------------------------------------------------------------
store_init <- function() {
  if (USE_GS) gs_init()
  # First run against an empty store: start from the managers.csv shipped with
  # the app (names, draft order, partners), else placeholders.
  if (nrow(store_read("managers")) == 0) {
    seed <- if (file.exists("data/managers.csv"))
      conform(utils::read.csv("data/managers.csv", colClasses = "character", na.strings = ""), "managers")
    store_write("managers", if (NROW(seed)) seed else DEFAULT_MANAGERS)
  }
}

store_read  <- function(name) if (USE_GS) gs_read(name) else csv_read(name)
store_write <- function(name, df) if (USE_GS) gs_write(name, df) else csv_write(name, df)

# Cheap value that changes when the stored data may have changed; polled by
# every session so all managers see picks/stats appear without refreshing.
store_version <- function() {
  if (USE_GS) return(floor(as.numeric(Sys.time()) / GS_TTL))
  paste(file.mtime(vapply(names(SCHEMAS), csv_path, "")), collapse = "|")
}
