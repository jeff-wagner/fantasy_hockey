# FWHL Fantasy League (Shiny app)

A live web app for the Fairbanks women's hockey fantasy league: draft board,
standings, team rosters, and player stats. Managers only need the link, plus a PIN
if they want to make their own draft picks; the commissioner signs in to manage
the league and enter game stats.

## Rules built into the app

- 5 players per team, snake draft (the order reverses each round).
- Each team needs at least 1 **new** player: someone not in the 2025-26 stats.
  The app blocks a manager's last open spot from going to a returning player
  if they don't have a new player yet, and blocks entering a 2025-26 player as "new".
- Each team needs exactly 1 **partner pick**: another manager's partner (the `partner`
  column in `managers`). Managers can't draft their own partner. The app blocks a pick that
  would leave a manager without room for their partner pick, and blocks taking a partner if
  that would leave some other manager with no partner they're allowed to draft.
- A player can only be on one team.
- Scoring: goal = 2, assist = 1, penalty minute = 1.

These are constants at the top of `app.R` (`ROSTER_SIZE`, `MIN_NEW`, `PARTNER_PICKS`, `PTS_*`) if the league changes them.

## Tabs

| Tab | Who | What |
|---|---|---|
| Standings | everyone | rank, team totals, cumulative points chart |
| Teams | everyone | each roster with G / A / PIM / points, new players marked ★ |
| Draft | everyone (managers pick with their PIN, or the commissioner picks for them) | live draft board, on-the-clock banner, 2025-26 player pool with fantasy points |
| Player Stats | everyone | this season's totals and 2025-26 stats |
| Commissioner | password | enter game stats, fix the game log, rename teams / set draft order and PINs, download an Excel backup |

Pages refresh automatically every few seconds, so managers watching the draft see picks appear.

## Run locally

```r
install.packages(c("shiny", "bslib", "DT", "dplyr", "ggplot2", "openxlsx"))
Sys.setenv(FHL_ADMIN_PASSWORD = "pick-a-password")
shiny::runApp("fantasy_hockey")
```

The default commissioner password is `changeme`. Set `FHL_ADMIN_PASSWORD` before sharing the app.

## Commissioner workflow

1. **Before the draft:** Commissioner → *Teams & draft order*. Double-click to rename
   managers and teams and set the draft order (1 = first pick), or click *Randomize draft order*
   (only before the first pick). Add or remove managers as needed. Click *Generate missing PINs*
   and send each manager their PIN privately so they can make their own picks.
2. **Draft night:** managers sign in on the Draft tab with their name and PIN and pick when
   they're on the clock (they can only pick for themselves, in turn, and can't undo).
   To pick for someone, sign in as commissioner: the manager on the clock is preselected. Pick a
   returning player from the search box, or switch to *New player* and type a name
   ("Jane Doe" is stored as "Doe, Jane"). *Undo last pick* fixes mistakes.
3. **Each game night:** Commissioner → *Enter game stats*. Choose the date, double-click cells
   to enter G / A / PIM for drafted players, then *Save game stats*. Fix errors in *Game log*.
4. Use *Backup* now and then to download everything as an .xlsx.

## Data and hosting

League state lives in three tables: `managers`, `picks`, `game_log`.
`data/stats_2025_2026.csv` is last season (rebuild it with `Rscript prep_data.R tracker.xlsx`).

- **Default (CSV files in `data/`)**: fine on your own computer, a lab server, Shiny Server, or Posit Connect.
- **Posit Connect Cloud**: see [DEPLOY.md](DEPLOY.md) for step-by-step Google Sheets setup.
  There, `FHL_GS_KEY` holds the key's JSON itself (a secret variable) instead of a file path.
- **shinyapps.io**: the disk is wiped when the app restarts, so CSVs would be lost. Use Google Sheets for storage instead:
  1. Create an empty Google Sheet and copy its ID from the URL.
  2. Create a Google Cloud service account, download its JSON key, and share the sheet with the service account's email as an Editor.
  3. Put the key file in the app folder (for example `gs-key.json`) and add a `.Renviron` file next to `app.R`:
     ```
     FHL_ADMIN_PASSWORD=your-password
     FHL_GSHEET_ID=1AbC...your-sheet-id
     FHL_GS_KEY=gs-key.json
     ```
  4. Deploy with `rsconnect::deployApp("fantasy_hockey")`.

  The app creates the `managers`, `picks`, and `game_log` tabs on first run. You can also view or edit the sheet directly as a backup.
