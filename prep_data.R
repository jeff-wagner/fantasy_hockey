# One-time helper: convert the "2025-2026 Stats" sheet of the original tracker
# spreadsheet into data/stats_2025_2026.csv, which the app reads at startup.
# Usage: Rscript prep_data.R path/to/local_fantasy_hockey_tracker.xlsx
library(readxl)

args <- commandArgs(trailingOnly = TRUE)
xlsx <- if (length(args)) args[1] else "local_fantasy_hockey_tracker.xlsx"

raw <- read_excel(xlsx, sheet = "2025-2026 Stats")
out <- data.frame(
  player      = trimws(raw$Name),
  team        = trimws(raw$Team),
  gp          = as.integer(raw$`Games Played`),
  goals       = as.integer(raw$Goals),
  assists     = as.integer(raw$Assists),
  pim         = as.integer(raw$`Penalty Minutes`),
  hat_tricks  = as.integer(ifelse(is.na(raw$`Hat Tricks`), 0, raw$`Hat Tricks`))
)
dir.create("data", showWarnings = FALSE)
write.csv(out, "data/stats_2025_2026.csv", row.names = FALSE)
cat("Wrote", nrow(out), "players to data/stats_2025_2026.csv\n")
