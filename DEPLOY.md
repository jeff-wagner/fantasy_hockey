# Deploying to Posit Connect Cloud with Google Sheets storage

Posit Connect Cloud wipes the app's disk whenever it restarts or you republish,
so league data (managers, picks, game stats) is stored in a Google Sheet instead.
The app signs in to Google with a *service account*, a robot Google account
that only has access to the one sheet you share with it.

You'll set up four things, in this order:

1. A Google Sheet (the database)
2. A Google Cloud service account plus its key (the app's login)
3. A GitHub repository with the app code (Connect Cloud publishes from GitHub)
4. The app on Connect Cloud, with three secret variables

Plan on 30–45 minutes the first time.

---

## 1. Create the Google Sheet

1. Go to <https://sheets.new> (signed in with the Google account that should own the data).
2. Name it, e.g. **FWHL Fantasy Data**. Leave it empty; the app creates the
   `managers`, `picks`, and `game_log` tabs itself.
3. Copy the **sheet ID** from the address bar. It's the long part between `/d/` and `/edit`:

   ```
   https://docs.google.com/spreadsheets/d/1AbCdEfGh...XyZ/edit#gid=0
                                          └──── sheet ID ────┘
   ```

   Save it somewhere; this is `FHL_GSHEET_ID`.

## 2. Create a service account and key

A personal Google account works best here. Work or school Google accounts often
block creating service-account keys.

1. Go to <https://console.cloud.google.com/> and accept the terms if asked.
2. **Create a project:** project picker (top bar) → **New project** → name it
   `fwhl-fantasy` → **Create**, then make sure it's selected in the top bar.
3. **Turn on the Sheets API:** ☰ menu → **APIs & Services → Library** → search
   **Google Sheets API** → **Enable**.
4. **Create the service account:** ☰ menu → **IAM & Admin → Service Accounts** →
   **+ Create service account**.
   - Name: `fwhl-app` → **Create and continue**
   - Skip "Grant this service account access to project" and "Grant users access". Click **Done**.
5. **Create its key:** click the new service account → **Keys** tab →
   **Add key → Create new key → JSON → Create**. A `.json` file downloads.
   - **Treat this file like a password.** Anyone with it can edit your sheet.
   - Move it somewhere *outside* the app folder, e.g.
     `C:\Users\<you>\Documents\fwhl-key.json`, so it can't end up on GitHub.
6. Copy the service account's **email**, shown on the service account page. It looks like
   `fwhl-app@fwhl-fantasy.iam.gserviceaccount.com`.

## 3. Share the sheet with the service account

1. Open your sheet → **Share**.
2. Paste the service account email, set it to **Editor**, untick **Notify people**, and click **Share**.

   (Google may warn that the address isn't a regular account. That's expected.)

## 4. Test locally (recommended)

This confirms the Google setup before anything goes online.

1. In the app folder (`H:\R\fantasy_hockey`), create a file named `.Renviron`:

   ```
   FHL_GSHEET_ID=1AbCdEfGh...XyZ
   FHL_GS_KEY=C:/Users/<you>/Documents/fwhl-key.json
   FHL_ADMIN_PASSWORD=pick-a-password
   ```

   Use forward slashes in the path. `.Renviron` is listed in `.gitignore`, so it
   won't be committed.
2. Restart R (`.Renviron` is only read at startup), then run:

   ```r
   shiny::runApp("H:/R/fantasy_hockey")
   ```
3. Open the Google Sheet. You should see the tabs `managers` (with your 8 managers and
   their partners, copied from `data/managers.csv`), `picks`, and `game_log`.
4. In the app, sign in as commissioner, make one test pick, and check that it appears on the
   `picks` tab within a few seconds. Then use **Undo last pick**.

If something fails, see **Troubleshooting** at the bottom.

## 5. Prepare the key for Connect Cloud

Connect Cloud can't read a file from your computer, so you'll paste the key's
contents into a secret variable. This R command copies it to your clipboard as
one line:

```r
writeClipboard(as.character(jsonlite::minify(paste(readLines("C:/Users/<you>/Documents/fwhl-key.json"), collapse = "\n"))))
```

Don't paste it anywhere except the Connect Cloud variable in step 7.

## 6. Put the app on GitHub

Connect Cloud publishes from a GitHub repository (public or private).

1. Create a repository at <https://github.com/new>, e.g. `fwhl-fantasy`.
2. Add these files from `H:\R\fantasy_hockey`:

   ```
   app.R
   R/storage.R
   data/managers.csv
   data/picks.csv
   data/stats_2025_2026.csv
   manifest.json
   README.md
   DEPLOY.md
   prep_data.R
   .gitignore
   ```

   The easiest way is **GitHub Desktop** (File → Add local repository → the app folder →
   commit → Publish), which respects `.gitignore`. If you use the website's
   **Add file → Upload files** instead, it does *not* check `.gitignore`: upload only the
   files above, and never `.Renviron` or the key `.json`.
3. `manifest.json` tells Connect Cloud which R version and packages to install. If you
   later add packages or update R, regenerate it before pushing:

   ```r
   rsconnect::writeManifest("H:/R/fantasy_hockey",
     appFiles = c("app.R", "R/storage.R", "data/managers.csv",
                  "data/picks.csv", "data/stats_2025_2026.csv"))
   ```

## 7. Publish on Connect Cloud

1. Go to <https://connect.posit.cloud> and sign in with GitHub. Allow Connect Cloud to
   access the repository when asked; for a private repo, grant it access to that repo.
2. Click **Publish** → **Shiny**.
3. Choose the repository, branch `main`, and primary file **`app.R`**.
4. Open **Advanced settings → Configure variables** and add three variables:

   | Name | Value |
   |---|---|
   | `FHL_GSHEET_ID` | the sheet ID from step 1 |
   | `FHL_GS_KEY` | paste the one-line key from step 5 |
   | `FHL_ADMIN_PASSWORD` | your commissioner password |

   These are stored as secrets and aren't visible in the app or the repo.
5. Click **Publish** and wait for the build (a few minutes the first time).

## 8. Check that data survives

1. Open the app URL, sign in as commissioner, make a test pick, and confirm it shows on
   the sheet's `picks` tab.
2. In Connect Cloud, **republish** the app, or restart it from its settings.
3. Reload the app: the pick should still be there. Then **Undo last pick**.

Share the app URL with managers. They don't need to sign in to watch.

---

## Day-to-day notes

- **The sheet is the database.** You can view it any time, and fix values directly if
  needed (the app picks up changes within about 20 seconds). Don't rename the tabs or edit
  the header row.
- **`data/managers.csv` is only a starting point.** It's copied to the sheet the first
  time the `managers` tab is empty. After that, change managers, teams, draft order, and
  partners in the app (Commissioner → Teams & draft order) or on the sheet.
- **Updating the app:** edit the code, push to GitHub, and republish in Connect Cloud
  (or turn on automatic publishing on push). Data isn't affected.
- **Backups:** Commissioner → **Backup** downloads everything as Excel. You can also use
  File → Make a copy on the sheet.
- **Who can see it:** Connect Cloud apps can be public. Anyone with the link can view
  standings and the draft, but only someone with the commissioner password can change anything.

## Troubleshooting

To see error messages, open your app in Connect Cloud and look at its **Logs**.

| What you see | Likely cause / fix |
|---|---|
| "Can't get Google credentials" | `FHL_GS_KEY` is missing, cut off, or not the whole key. Re-copy it with the command in step 5 and paste it again. |
| `PERMISSION_DENIED` / HTTP 403 | The sheet isn't shared with the service account email as **Editor**, or the Google Sheets API isn't enabled in the project. |
| HTTP 404 / "not found" | Wrong `FHL_GSHEET_ID`: copy only the part between `/d/` and `/edit`. |
| Managers show as "Manager 1 … Manager 8" | `data/managers.csv` wasn't included in the GitHub repo. Add it, delete the `managers` tab's rows in the sheet (keep the header), and restart the app. |
| Build fails installing packages | Regenerate `manifest.json` (step 6.3) and push. If it complains about the R version, install a slightly older R locally, regenerate, and push. |
| Can't create a key ("key creation is disabled") | Your Google account's organization blocks it. Use a personal Google account for steps 1–3. |
