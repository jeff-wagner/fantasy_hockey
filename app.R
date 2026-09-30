# Fairbanks Women's Hockey -- Fantasy League tracker
# Files in R/ are sourced automatically by Shiny (storage layer lives there).

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(ggplot2)

# ---- League rules ------------------------------------------------------------
ROSTER_SIZE  <- 5
MIN_NEW      <- 1                 # players who did not play in 2025-26
PARTNER_PICKS <- 1                # exactly this many picks must be another manager's partner
PTS_GOAL     <- 2
PTS_ASSIST   <- 1
PTS_PIM      <- 1                 # per penalty minute
ADMIN_PW     <- Sys.getenv("FHL_ADMIN_PASSWORD", "changeme")

fpts <- function(g, a, pim) PTS_GOAL * g + PTS_ASSIST * a + PTS_PIM * pim

# ---- Static data: last season ------------------------------------------------
last_season <- read.csv("data/stats_2025_2026.csv", stringsAsFactors = FALSE) |>
  mutate(fpts = fpts(goals, assists, pim),
         fpts_gp = round(fpts / pmax(gp, 1), 2))
LEAGUE_TEAMS <- sort(unique(last_season$team))

# "hali morrow" -> "Morrow, Hali" so new names match last season's format.
normalize_name <- function(x) {
  x <- gsub("\\s+", " ", trimws(x))
  x <- gsub("(^|[ ,'-])([a-z])", "\\1\\U\\2", x, perl = TRUE)
  if (!nzchar(x) || grepl(",", x)) return(sub("\\s*,\\s*", ", ", x))
  parts <- strsplit(x, " ")[[1]]
  if (length(parts) < 2) return(x)
  paste0(parts[length(parts)], ", ", paste(parts[-length(parts)], collapse = " "))
}

# Snake draft: who is on the clock for the next pick?
on_the_clock <- function(managers, picks) {
  n <- nrow(managers); k <- nrow(picks)
  if (n == 0 || k >= n * ROSTER_SIZE) return(NULL)
  ord <- managers$manager[order(managers$draft_order)]
  rnd <- k %/% n + 1
  pos <- k %% n + 1
  if (rnd %% 2 == 0) pos <- n - pos + 1
  list(manager = ord[pos], round = rnd, pick = k + 1)
}

# ---- Partner rule ------------------------------------------------------------
# Each manager's partner is listed in the managers table. Every manager must draft
# exactly PARTNER_PICKS player(s) from that list, and can never draft their own partner.

# Manager whose partner this player is (NA if nobody's).
partner_of <- function(player, managers)
  managers$manager[match(tolower(player), tolower(managers$partner))]

# Is this player a partner pick for this manager (someone else's partner)?
is_partner_pick <- function(manager, player, managers) {
  o <- partner_of(player, managers)
  !is.na(o) & o != manager
}

# Undrafted partners this manager could use for their partner pick.
eligible_partners <- function(manager, managers, picks) {
  p <- managers$partner[managers$manager != manager & !is.na(managers$partner)]
  p[!tolower(p) %in% tolower(picks$player)]
}

partners_needed <- function(manager, managers, picks) {
  if (!any(!is.na(managers$partner) & managers$manager != manager)) return(0)
  mine <- picks$player[picks$manager == manager]
  max(0, PARTNER_PICKS - sum(is_partner_pick(manager, mine, managers)))
}

# Can every manager still needing a partner get a different undrafted one that
# isn't their own? (Bipartite matching, so the draft can't dead-end on the last pick.)
partners_feasible <- function(managers, picks) {
  slots <- unlist(lapply(managers$manager, function(m)
    rep(m, partners_needed(m, managers, picks))))
  opts <- lapply(slots, function(m) tolower(eligible_partners(m, managers, picks)))
  owner <- list()   # partner -> index of the slot that has claimed them
  augment <- function(i, seen) {
    for (p in opts[[i]]) {
      if (p %in% seen$v) next
      seen$v <- c(seen$v, p)
      if (is.null(owner[[p]]) || augment(owner[[p]], seen)) {
        owner[[p]] <<- i
        return(TRUE)
      }
    }
    FALSE
  }
  for (i in seq_along(slots)) if (!augment(i, new.env())) return(FALSE)
  TRUE
}

# Returns NULL if the pick is legal, otherwise an error message.
check_pick <- function(manager, player, is_new, picks, managers) {
  mine <- picks[picks$manager == manager, ]
  if (!nzchar(player)) return("Choose a player.")
  if (nrow(mine) >= ROSTER_SIZE) return(sprintf("%s already has %d players.", manager, ROSTER_SIZE))
  taken <- picks[tolower(picks$player) == tolower(player), ]
  if (nrow(taken)) return(sprintf("%s was already drafted by %s.", player, taken$manager[1]))
  if (is_new && tolower(player) %in% tolower(last_season$player))
    return(sprintf("%s played in 2025-26, so they are not a new player. Pick them from the returning list instead.", player))

  if (identical(partner_of(player, managers), manager))
    return(sprintf("%s is %s's own partner. Managers can't draft their own partner.", player, manager))
  partner_pick <- is_partner_pick(manager, player, managers)
  if (partner_pick && partners_needed(manager, managers, picks) == 0)
    return(sprintf("%s already has %d partner pick%s, the most allowed.", manager,
                   PARTNER_PICKS, if (PARTNER_PICKS == 1) "" else "s"))

  # After this pick, the remaining spots must still cover every requirement.
  after <- rbind(picks[c("manager", "player")], data.frame(manager = manager, player = player))
  slots_left <- ROSTER_SIZE - nrow(mine) - 1
  new_left <- max(0, MIN_NEW - sum(mine$is_new) - is_new)
  partner_left <- partners_needed(manager, managers, after)
  # A partner who is also a new player could cover both at once.
  both_at_once <- any(!tolower(eligible_partners(manager, managers, after)) %in% tolower(last_season$player))
  spots_needed <- if (both_at_once) max(new_left, partner_left) else new_left + partner_left
  wants <- c(if (!is_new && new_left > 0) "a new player",
             if (!partner_pick && partner_left > 0) "another manager's partner")
  if (spots_needed > slots_left && length(wants)) {
    return(sprintf("%s has %d spot%s left and still needs %s.",
                   manager, slots_left + 1, if (slots_left == 0) "" else "s",
                   paste(wants, collapse = " and ")))
  }
  if (!is.na(partner_of(player, managers)) && !partners_feasible(managers, after))
    return(sprintf("Taking %s would leave another manager with no partner they can draft.", player))
  NULL
}

store_init()
db_bump <- reactiveVal(0)   # shared across sessions in this R process

# ---- UI ----------------------------------------------------------------------
theme <- bs_theme(version = 5, preset = "flatly",
                  primary = "#0b3d91", secondary = "#6c757d",
                  base_font = font_google("Inter"))

ui <- page_navbar(
  title = "FWHL Fantasy",
  theme = theme,
  fillable = FALSE,
  header = tags$style(HTML("
    .draft-board td, .draft-board th { text-align:center; vertical-align:middle; font-size:.9rem; }
    .draft-board td.empty { color:#adb5bd; }
    .draft-board td.clock { outline:3px solid var(--bs-warning); outline-offset:-3px; }
    .new-badge { color:#d97706; font-weight:700; }
    .partner-badge { color:#db2777; font-weight:700; }
    .team-card .card-header { font-weight:600; }
  ")),

  nav_panel("Standings", icon = icon("trophy"),
    uiOutput("value_boxes"),
    layout_columns(col_widths = breakpoints(sm = 12, xl = c(5, 7)),
      card(card_header("League standings"), DTOutput("standings_tbl", fill = FALSE)),
      card(card_header("Points over the season"), plotOutput("points_plot", height = 360))
    )
  ),

  nav_panel("Teams", icon = icon("users"),
    uiOutput("team_cards")
  ),

  nav_panel("Draft", icon = icon("list-ol"),
    uiOutput("clock_banner"),
    layout_sidebar(
      sidebar = sidebar(width = 340, open = "desktop", uiOutput("draft_controls")),
      card(card_header("Draft board"), uiOutput("draft_board")),
      card(
        card_header("Player pool: 2025-26 stats",
                    class = "d-flex justify-content-between align-items-center",
                    checkboxInput("hide_drafted", "Hide drafted", TRUE)),
        DTOutput("pool_tbl", fill = FALSE)
      )
    )
  ),

  nav_panel("Player Stats", icon = icon("chart-bar"),
    navset_card_underline(
      nav_panel("This season", DTOutput("season_tbl", fill = FALSE)),
      nav_panel("2025-26", DTOutput("last_tbl", fill = FALSE))
    )
  ),

  nav_panel("Rules", icon = icon("book"),
    card(card_body(
      h4("How it works"),
      tags$ul(
        tags$li(sprintf("Each manager drafts %d players from the Fairbanks women's league.", ROSTER_SIZE)),
        tags$li(HTML(sprintf("At least %d pick must be a <b>new</b> player: someone who did not play in the league in 2025-26. New players are marked <span class='new-badge'>&#9733;</span>.", MIN_NEW))),
        tags$li(HTML(sprintf("Exactly %d pick must be <b>another manager's partner</b>. You can't draft your own partner. Partner picks are marked <span class='partner-badge'>&#9829;</span>.", PARTNER_PICKS))),
        tags$li("A player can only be on one fantasy team."),
        tags$li("The draft is a snake draft: order reverses every round."),
        tags$li("When you're on the clock, sign in on the Draft tab with your PIN to make your pick (or tell the commissioner).")
      ),
      h4("Scoring"),
      tags$table(class = "table table-sm w-auto",
        tags$tbody(
          tags$tr(tags$td("Goal"), tags$td(sprintf("%d points", PTS_GOAL))),
          tags$tr(tags$td("Assist"), tags$td(sprintf("%d point", PTS_ASSIST))),
          tags$tr(tags$td("Penalty minute"), tags$td(sprintf("%d point per minute", PTS_PIM)))
        )
      ),
      p(class = "text-muted", "2025-26 fantasy points in the Draft tab use the same formula, to help with drafting.")
    ))
  ),

  nav_spacer(),
  nav_panel("Commissioner", icon = icon("lock"),
    uiOutput("admin_ui")
  )
)

# ---- Server ------------------------------------------------------------------
server <- function(input, output, session) {

  is_admin <- reactiveVal(FALSE)

  # Live data: re-read when this process writes (db_bump) or the store changes.
  poll <- reactivePoll(4000, session, store_version, store_version)
  tables <- reactive({
    db_bump(); poll()
    list(managers = store_read("managers") |> arrange(draft_order),
         picks    = store_read("picks") |> arrange(pick),
         game_log = store_read("game_log"))
  })
  save <- function(name, df) {
    store_write(name, df)
    db_bump(db_bump() + 1)
  }

  season_stats <- reactive({
    tables()$game_log |>
      group_by(player) |>
      summarise(goals = sum(goals, na.rm = TRUE), assists = sum(assists, na.rm = TRUE),
                pim = sum(pim, na.rm = TRUE), .groups = "drop") |>
      mutate(fpts = fpts(goals, assists, pim))
  })

  rosters <- reactive({
    t <- tables()
    t$picks |>
      left_join(t$managers, by = "manager") |>
      left_join(season_stats(), by = "player") |>
      mutate(across(c(goals, assists, pim, fpts), ~ coalesce(.x, 0)),
             is_partner = is_partner_pick(manager, player, t$managers))
  })

  standings <- reactive({
    t <- tables()
    t$managers |>
      left_join(rosters() |> group_by(manager) |>
                  summarise(players = n(), goals = sum(goals), assists = sum(assists),
                            pim = sum(pim), fpts = sum(fpts), .groups = "drop"),
                by = "manager") |>
      mutate(across(c(players, goals, assists, pim, fpts), ~ coalesce(.x, 0))) |>
      arrange(desc(fpts), desc(goals)) |>
      mutate(rank = min_rank(desc(fpts)))
  })

  # ---- Standings tab ---------------------------------------------------------
  output$value_boxes <- renderUI({
    s <- standings(); r <- rosters(); gl <- tables()$game_log
    leader <- if (nrow(s) && s$fpts[1] > 0) s$team_name[1] else "-"
    top <- r |> arrange(desc(fpts)) |> head(1)
    layout_columns(fill = FALSE,
      value_box("League leader", leader, showcase = icon("trophy"), showcase_layout = "left center", max_height = "140px", theme = "primary",
                p(if (leader != "-") sprintf("%d pts - %s", s$fpts[1], s$manager[1]))),
      value_box("Top fantasy player",
                if (nrow(top) && top$fpts > 0) top$player else "-",
                showcase = icon("star"), showcase_layout = "left center", max_height = "140px", theme = "warning",
                p(if (nrow(top) && top$fpts > 0) sprintf("%d pts for %s", top$fpts, top$team_name))),
      value_box("Game nights recorded", length(unique(gl$game_date)),
                showcase = icon("calendar"), showcase_layout = "left center", max_height = "140px", theme = "secondary",
                p(if (nrow(gl)) paste("Latest:", format(max(as.Date(gl$game_date)), "%b %d, %Y"))))
    )
  })

  output$standings_tbl <- renderDT({
    s <- standings() |>
      transmute(Rank = rank, Team = team_name, Manager = manager,
                G = goals, A = assists, PIM = pim, Points = fpts)
    datatable(s, rownames = FALSE, fillContainer = FALSE, selection = "none",
              options = list(dom = "t", paging = FALSE, ordering = FALSE)) |>
      formatStyle("Points", fontWeight = "bold")
  })

  output$points_plot <- renderPlot({
    gl <- tables()$game_log
    validate(need(nrow(gl) > 0, "Points will appear here once game stats are entered."))
    r <- rosters()
    daily <- gl |>
      inner_join(r |> select(player, team_name), by = "player") |>
      mutate(game_date = as.Date(game_date), pts = fpts(goals, assists, pim)) |>
      group_by(team_name, game_date) |> summarise(pts = sum(pts), .groups = "drop")
    validate(need(nrow(daily) > 0, "No stats recorded yet for drafted players."))
    grid <- expand.grid(team_name = unique(r$team_name),
                        game_date = sort(unique(as.Date(gl$game_date))),
                        stringsAsFactors = FALSE)
    cum <- grid |> left_join(daily, by = c("team_name", "game_date")) |>
      mutate(pts = coalesce(pts, 0)) |>
      arrange(game_date) |> group_by(team_name) |> mutate(total = cumsum(pts)) |> ungroup()
    ggplot(cum, aes(game_date, total, colour = team_name)) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      labs(x = NULL, y = "Fantasy points", colour = NULL) +
      theme_minimal(base_size = 14) + theme(legend.position = "bottom")
  })

  # ---- Teams tab -------------------------------------------------------------
  output$team_cards <- renderUI({
    s <- standings(); r <- rosters()
    cards <- lapply(seq_len(nrow(s)), function(i) {
      m <- s[i, ]
      mine <- r |> filter(manager == m$manager) |> arrange(pick)
      rows <- if (nrow(mine)) lapply(seq_len(nrow(mine)), function(j) {
        p <- mine[j, ]
        tags$tr(
          tags$td(p$player, if (p$is_new) span(class = "new-badge", title = "New player", HTML(" &#9733;")),
                  if (p$is_partner) span(class = "partner-badge", title = "Partner pick", HTML(" &#9829;")),
                  br(), tags$small(class = "text-muted", p$league_team)),
          tags$td(p$goals), tags$td(p$assists), tags$td(p$pim), tags$td(tags$b(p$fpts))
        )
      })
      empty <- ROSTER_SIZE - nrow(mine)
      if (empty > 0) rows <- c(rows, lapply(seq_len(empty), function(j)
        tags$tr(tags$td(colspan = 5, class = "text-muted fst-italic", "Open roster spot"))))
      card(class = "team-card",
        card_header(class = "d-flex justify-content-between",
          span(sprintf("#%d  %s", m$rank, m$team_name)),
          span(class = "badge bg-primary fs-6", sprintf("%d pts", m$fpts))),
        card_body(
          tags$small(class = "text-muted", m$manager),
          tags$table(class = "table table-sm mb-0",
            tags$thead(tags$tr(tags$th("Player"), tags$th("G"), tags$th("A"),
                               tags$th("PIM"), tags$th("Pts"))),
            tags$tbody(rows))
        ))
    })
    layout_column_wrap(width = "320px", !!!cards)
  })

  # ---- Draft tab -------------------------------------------------------------
  clock <- reactive(on_the_clock(tables()$managers, tables()$picks))

  output$clock_banner <- renderUI({
    c <- clock()
    if (is.null(c))
      return(div(class = "alert alert-success", icon("check"), " The draft is complete."))
    div(class = "alert alert-warning d-flex justify-content-between",
        span(icon("clock"), sprintf(" Round %d, pick %d: ", c$round, c$pick),
             tags$b(c$manager), sprintf(" (%s) is on the clock",
               tables()$managers$team_name[tables()$managers$manager == c$manager])),
        span(sprintf("%d of %d picks made", nrow(tables()$picks),
                     nrow(tables()$managers) * ROSTER_SIZE)))
  })

  output$draft_board <- renderUI({
    t <- tables(); ck <- clock()
    head_row <- tags$tr(tags$th("Rd"), lapply(seq_len(nrow(t$managers)), function(i)
      tags$th(t$managers$team_name[i], br(), tags$small(class = "text-muted fw-normal", t$managers$manager[i]))))
    body <- lapply(seq_len(ROSTER_SIZE), function(rd) {
      tags$tr(tags$th(rd), lapply(t$managers$manager, function(m) {
        mine <- t$picks[t$picks$manager == m, ]
        if (nrow(mine) >= rd) {
          p <- mine[rd, ]
          tags$td(p$player, if (p$is_new) span(class = "new-badge", HTML(" &#9733;")),
                  if (is_partner_pick(m, p$player, t$managers)) span(class = "partner-badge", HTML(" &#9829;")),
                  br(), tags$small(class = "text-muted", sprintf("#%d", p$pick)))
        } else {
          on <- !is.null(ck) && ck$manager == m && nrow(mine) + 1 == rd
          tags$td(class = paste("empty", if (on) "clock"), if (on) "on the clock" else "-")
        }
      }))
    })
    div(class = "table-responsive",
        tags$table(class = "table table-bordered draft-board mb-0",
                   tags$thead(head_row), tags$tbody(body)),
        tags$small(class = "text-muted", HTML("&#9733; = new player (did not play in 2025-26) &nbsp; &#9829; = partner pick")))
  })

  pool <- reactive({
    p <- tables()$picks
    last_season |>
      left_join(p |> select(player, manager), by = "player") |>
      mutate(status = ifelse(is.na(manager), "Available", paste("Drafted:", manager)),
             partner_of = partner_of(player, tables()$managers))
  })

  output$pool_tbl <- renderDT({
    d <- pool()
    if (isTRUE(input$hide_drafted)) d <- d |> filter(status == "Available")
    d <- d |> arrange(desc(fpts)) |>
      transmute(Player = player, Team = team, GP = gp, G = goals, A = assists,
                PIM = pim, `Fantasy pts` = fpts, `Pts/GP` = fpts_gp,
                `Partner of` = coalesce(partner_of, ""), Status = status)
    datatable(d, rownames = FALSE, fillContainer = FALSE, selection = "none", filter = "top",
              options = list(pageLength = 15, order = list(list(6, "desc"))))
  })

  # Who this session picks for: the commissioner picks for anyone, a signed-in
  # manager only for themselves.
  picker <- reactive(if (is_admin()) input$pick_manager else me())

  output$draft_controls <- renderUI({
    if (!is_admin() && is.null(me())) return(tagList(
      h6("Manager sign-in"),
      selectInput("me_manager", NULL, c("Choose your name" = "", isolate(tables()$managers$manager))),
      passwordInput("me_pin", NULL, placeholder = "PIN"),
      actionButton("me_login", "Sign in to pick", class = "btn-primary w-100", icon = icon("right-to-bracket")),
      p(class = "text-muted small mt-2", "Get your PIN from the commissioner. You can make your own picks when you're on the clock."),
      hr(),
      p(class = "text-muted", "Use the player pool to scout: fantasy points use this year's scoring applied to last season.")))
    tagList(
      if (is_admin())
        selectInput("pick_manager", "Manager", isolate(tables()$managers$manager),
                    selected = isolate(clock()$manager))
      else div(class = "d-flex justify-content-between align-items-center mb-2",
               tags$b(me()),
               actionLink("me_logout", "Sign out", class = "small")),
      uiOutput("pick_needs"),
      radioButtons("pick_type", NULL, c("Returning player" = "returning", "New player" = "new"), inline = TRUE),
      conditionalPanel("input.pick_type == 'returning'",
        selectizeInput("pick_player", "Player", NULL,
                       options = list(placeholder = "Search last season's players"))),
      conditionalPanel("input.pick_type == 'new'",
        textInput("new_name", "Player name", placeholder = "First Last"),
        selectizeInput("new_team", "League team", c("", LEAGUE_TEAMS),
                       options = list(create = TRUE, placeholder = "Team (optional)"))),
      actionButton("make_pick", "Make pick", class = "btn-primary w-100", icon = icon("check")),
      if (is_admin()) tagList(hr(),
        actionButton("undo_pick", "Undo last pick", class = "btn-outline-danger btn-sm w-100", icon = icon("rotate-left")))
    )
  })

  # Manager sign-in (per session; PINs are set by the commissioner).
  me <- reactiveVal(NULL)
  pin_fails <- 0
  observeEvent(input$me_login, {
    if (pin_fails >= 5) return(showNotification("Too many wrong PINs. Reload the page to try again.", type = "error"))
    m <- store_read("managers")
    pin <- m$pin[m$manager == input$me_manager]
    if (length(pin) == 1 && !is.na(pin) && nzchar(pin) && identical(trimws(input$me_pin), pin)) {
      me(input$me_manager)
    } else {
      pin_fails <<- pin_fails + 1
      showNotification("That name and PIN don't match.", type = "error")
    }
  })
  observeEvent(input$me_logout, me(NULL))

  # Keep the pick form in sync without re-rendering it (so a half-made pick
  # isn't wiped when another update arrives).
  observe({
    req(is_admin())
    ck <- clock()
    updateSelectInput(session, "pick_manager", choices = tables()$managers$manager,
                      selected = if (!is.null(ck)) ck$manager else isolate(input$pick_manager))
  })
  observe({
    req(is_admin() || !is.null(me()))
    avail <- pool() |> filter(status == "Available") |> arrange(desc(fpts))
    choices <- setNames(avail$player, paste0(sprintf("%s (%s) - %d pts", avail$player, avail$team, avail$fpts),
                                             ifelse(is.na(avail$partner_of), "", paste0(" - partner of ", avail$partner_of))))
    keep <- isolate(input$pick_player)
    updateSelectizeInput(session, "pick_player", choices = c("", choices),
                         selected = if (isTRUE(keep %in% avail$player)) keep else "",
                         server = TRUE)
  })

  outputOptions(output, "draft_controls", suspendWhenHidden = FALSE)

  output$pick_needs <- renderUI({
    who <- picker(); req(who)
    t <- tables(); ck <- clock()
    mine <- t$picks |> filter(manager == who)
    need_new <- max(0, MIN_NEW - sum(mine$is_new))
    need_partner <- partners_needed(who, t$managers, t$picks)
    own <- t$managers$partner[t$managers$manager == who]
    tagList(
      if (!is_admin()) {
        if (is.null(ck)) div(class = "alert alert-success py-2 small", "The draft is complete.")
        else if (ck$manager == who) div(class = "alert alert-warning py-2 small", icon("clock"), tags$b(" You're on the clock!"))
        else div(class = "alert alert-light py-2 small", sprintf("Waiting: %s is on the clock.", ck$manager))
      },
      tags$p(class = "small",
      sprintf("%d of %d spots filled. ", nrow(mine), ROSTER_SIZE),
      if (need_new > 0) span(class = "new-badge", sprintf("Still needs %d new player.", need_new))
      else span(class = "text-success", "New-player requirement met."),
      br(),
      if (need_partner > 0) span(class = "partner-badge",
        sprintf("Still needs %d partner pick%s.", need_partner,
                if (length(own) && !is.na(own)) sprintf(" (not %s)", own) else ""))
      else span(class = "text-success", "Partner requirement met.")))
  })

  observeEvent(input$make_pick, {
    who <- picker()
    req(is_admin() || !is.null(me()), who)
    # Read fresh so two people clicking at once can't both take the same slot.
    managers <- store_read("managers"); picks <- store_read("picks") |> arrange(pick)
    if (!is_admin()) {
      ck <- on_the_clock(managers, picks)
      if (is.null(ck)) return(showNotification("The draft is complete.", type = "warning"))
      if (ck$manager != who)
        return(showNotification(sprintf("It's not your turn: %s is on the clock.", ck$manager), type = "error"))
    }
    is_new <- identical(input$pick_type, "new")
    player <- if (is_new) normalize_name(input$new_name) else input$pick_player
    err <- check_pick(who, player, is_new, picks, managers)
    if (!is.null(err)) return(showNotification(err, type = "error", duration = 8))
    team <- if (is_new) input$new_team else last_season$team[last_season$player == player][1]
    new_row <- data.frame(pick = nrow(picks) + 1, manager = who,
                          player = player, is_new = is_new, league_team = team %||% "",
                          picked_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    save("picks", bind_rows(picks, new_row))
    updateTextInput(session, "new_name", value = "")
    showNotification(sprintf("Pick %d: %s to %s", new_row$pick, player, who), type = "message")
  })

  observeEvent(input$undo_pick, {
    req(is_admin())
    p <- tables()$picks
    if (!nrow(p)) return()
    showModal(modalDialog(title = "Undo last pick?",
      sprintf("Remove pick %d: %s (%s)?", p$pick[nrow(p)], p$player[nrow(p)], p$manager[nrow(p)]),
      footer = tagList(modalButton("Cancel"), actionButton("undo_confirm", "Undo", class = "btn-danger"))))
  })
  observeEvent(input$undo_confirm, {
    req(is_admin())
    p <- tables()$picks
    save("picks", p[-nrow(p), ])
    removeModal()
  })

  # ---- Player stats tab ------------------------------------------------------
  output$season_tbl <- renderDT({
    d <- season_stats() |>
      left_join(tables()$picks |> select(player, manager), by = "player") |>
      left_join(tables()$managers |> select(manager, team_name), by = "manager") |>
      arrange(desc(fpts)) |>
      transmute(Player = player, `Fantasy team` = coalesce(team_name, "(undrafted)"),
                G = goals, A = assists, PIM = pim, `Fantasy pts` = fpts)
    datatable(d, rownames = FALSE, fillContainer = FALSE, selection = "none", filter = "top",
              options = list(pageLength = 25, language = list(emptyTable = "No stats entered yet this season.")))
  })

  output$last_tbl <- renderDT({
    d <- last_season |> arrange(desc(fpts)) |>
      transmute(Player = player, Team = team, GP = gp, G = goals, A = assists,
                PIM = pim, `Hat tricks` = hat_tricks, `Fantasy pts` = fpts, `Pts/GP` = fpts_gp)
    datatable(d, rownames = FALSE, fillContainer = FALSE, selection = "none", filter = "top",
              options = list(pageLength = 25))
  })

  # ---- Commissioner tab ------------------------------------------------------
  output$admin_ui <- renderUI({
    if (!is_admin()) return(
      card(max_height = 260, class = "mx-auto", style = "max-width:380px",
        card_header("Commissioner sign-in"),
        passwordInput("admin_pw", NULL, placeholder = "Password"),
        actionButton("admin_login", "Sign in", class = "btn-primary")))
    navset_card_underline(
      nav_panel("Enter game stats",
        layout_columns(col_widths = c(4, 8), fill = FALSE,
          dateInput("entry_date", "Game date", value = Sys.Date()),
          p(class = "text-muted mt-4", "Double-click a cell to edit. Only drafted players are listed; rows left at zero are skipped.")),
        DTOutput("entry_tbl", fill = FALSE),
        div(class = "mt-2",
          actionButton("save_entry", "Save game stats", class = "btn-primary", icon = icon("floppy-disk")),
          actionButton("reset_entry", "Clear", class = "btn-outline-secondary"))
      ),
      nav_panel("Game log",
        p(class = "text-muted", "Select rows to remove mistaken entries."),
        DTOutput("log_tbl", fill = FALSE),
        actionButton("delete_log", "Delete selected", class = "btn-outline-danger mt-2", icon = icon("trash"))
      ),
      nav_panel("Teams & draft order",
        p(class = "text-muted", "Double-click to edit manager names, team names, draft order (1 = first pick), partner (each manager's partner, \"Last, First\"), or PIN (lets a manager sign in on the Draft tab and make their own picks)."),
        DTOutput("mgr_tbl", fill = FALSE),
        div(class = "mt-2 d-flex gap-2",
          actionButton("add_mgr", "Add manager", icon = icon("plus"), class = "btn-outline-primary"),
          actionButton("del_mgr", "Remove selected", icon = icon("trash"), class = "btn-outline-danger"),
          actionButton("gen_pins", "Generate missing PINs", icon = icon("key"), class = "btn-outline-secondary ms-auto"),
          actionButton("shuffle_order", "Randomize draft order", icon = icon("shuffle"), class = "btn-outline-secondary"))
      ),
      nav_panel("Backup",
        p("Download every table (managers, picks, game log) as an Excel workbook."),
        downloadButton("backup", "Download backup", class = "btn-primary"))
    )
  })

  observeEvent(input$admin_login, {
    if (identical(input$admin_pw, ADMIN_PW)) is_admin(TRUE)
    else showNotification("Wrong password.", type = "error")
  })

  # Stats entry grid (drafted players only)
  entry <- reactiveVal(NULL)
  entry_blank <- reactive({
    input$reset_entry
    rosters() |> arrange(team_name, player) |>
      transmute(Player = player, Team = team_name, G = 0L, A = 0L, PIM = 0L)
  })
  observe(entry(entry_blank()))
  output$entry_tbl <- renderDT({
    datatable(entry_blank(), rownames = FALSE, fillContainer = FALSE, selection = "none",
              editable = list(target = "cell", disable = list(columns = c(0, 1))),
              options = list(dom = "ft", paging = FALSE, ordering = FALSE))
  })
  observeEvent(input$entry_tbl_cell_edit, {
    info <- input$entry_tbl_cell_edit
    if (!NROW(info)) return()
    info$value <- pmax(0L, suppressWarnings(as.integer(info$value)), na.rm = TRUE)
    entry(editData(entry(), info, proxy = "entry_tbl", rownames = FALSE))
  })
  observeEvent(input$save_entry, {
    req(is_admin(), input$entry_date)
    e <- entry() |> filter(G + A + PIM > 0)
    if (!nrow(e)) return(showNotification("Nothing to save: every row is zero.", type = "warning"))
    d <- format(input$entry_date)
    gl <- tables()$game_log
    dup <- intersect(e$Player, gl$player[gl$game_date == d])
    rows <- data.frame(
      id = paste0(format(Sys.time(), "%Y%m%d%H%M%OS3"), "-", seq_len(nrow(e))),
      game_date = d, player = e$Player, goals = e$G, assists = e$A, pim = e$PIM,
      entered_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    save("game_log", bind_rows(gl, rows))
    entry(entry_blank())
    DT::replaceData(dataTableProxy("entry_tbl"), entry_blank(), rownames = FALSE)
    msg <- sprintf("Saved %d player lines for %s.", nrow(rows), d)
    if (length(dup)) msg <- paste(msg, "Note: already had entries that date for", paste(dup, collapse = ", "))
    showNotification(msg, type = if (length(dup)) "warning" else "message", duration = 8)
  })

  # Game log
  log_view <- reactive(tables()$game_log |> arrange(desc(game_date), player))
  output$log_tbl <- renderDT({
    datatable(log_view() |> transmute(Date = game_date, Player = player, G = goals,
                                      A = assists, PIM = pim, Pts = fpts(goals, assists, pim)),
              rownames = FALSE, fillContainer = FALSE, options = list(pageLength = 25))
  })
  observeEvent(input$delete_log, {
    req(is_admin())
    sel <- input$log_tbl_rows_selected
    if (!length(sel)) return(showNotification("Select rows first.", type = "warning"))
    ids <- log_view()$id[sel]
    gl <- tables()$game_log
    save("game_log", gl[!gl$id %in% ids, ])
    showNotification(sprintf("Deleted %d entries.", length(ids)))
  })

  # Managers
  output$mgr_tbl <- renderDT({
    datatable(tables()$managers |> rename(Manager = manager, `Team name` = team_name,
                                          `Draft order` = draft_order, Partner = partner, PIN = pin),
              rownames = FALSE, fillContainer = FALSE, editable = "cell",
              options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })
  observeEvent(input$mgr_tbl_cell_edit, {
    req(is_admin())
    info <- input$mgr_tbl_cell_edit
    m <- tables()$managers; p <- tables()$picks
    col <- names(m)[info$col + 1]; row <- info$row
    if (col == "manager") {
      new <- trimws(info$value); old <- m$manager[row]
      if (!nzchar(new) || new %in% m$manager[-row])
        return(showNotification("Manager names must be unique and non-empty.", type = "error"))
      p$manager[p$manager == old] <- new
      m$manager[row] <- new
      save("picks", p)
    } else if (col == "draft_order") {
      m$draft_order[row] <- as.integer(info$value)
    } else if (col == "partner") {
      m$partner[row] <- if (nzchar(trimws(info$value))) normalize_name(info$value) else NA
    } else if (col == "pin") {
      m$pin[row] <- if (nzchar(trimws(info$value))) trimws(info$value) else NA
    } else m[[col]][row] <- info$value
    save("managers", m)
  })
  observeEvent(input$add_mgr, {
    req(is_admin())
    m <- tables()$managers
    i <- nrow(m) + 1
    while (paste("Manager", i) %in% m$manager) i <- i + 1
    save("managers", bind_rows(m, data.frame(manager = paste("Manager", i),
      team_name = paste("Team", i), draft_order = max(c(0, m$draft_order), na.rm = TRUE) + 1L)))
  })
  observeEvent(input$del_mgr, {
    req(is_admin())
    sel <- input$mgr_tbl_rows_selected
    m <- tables()$managers
    if (!length(sel)) return(showNotification("Select a manager first.", type = "warning"))
    if (any(m$manager[sel] %in% tables()$picks$manager))
      return(showNotification("Can't remove a manager who has drafted players. Undo their picks first.", type = "error"))
    save("managers", m[-sel, ])
  })
  observeEvent(input$gen_pins, {
    req(is_admin())
    m <- tables()$managers
    missing <- is.na(m$pin) | !nzchar(m$pin)
    if (!any(missing)) return(showNotification("Every manager already has a PIN.", type = "message"))
    m$pin[missing] <- sprintf("%04d", sample.int(10000, sum(missing)) - 1L)
    save("managers", m)
    showNotification(sprintf("Created PINs for %d manager%s. Send each manager their own PIN.",
                             sum(missing), if (sum(missing) == 1) "" else "s"), type = "message", duration = 8)
  })
  observeEvent(input$shuffle_order, {
    req(is_admin())
    if (nrow(store_read("picks")))
      return(showNotification("The draft has started. Undo every pick before changing the draft order.", type = "error"))
    showModal(modalDialog(title = "Randomize draft order?",
      "This replaces the current draft order with a random one. Everyone watching sees the new order right away.",
      footer = tagList(modalButton("Cancel"), actionButton("shuffle_confirm", "Randomize", class = "btn-primary"))))
  })
  observeEvent(input$shuffle_confirm, {
    req(is_admin())
    removeModal()
    m <- tables()$managers
    if (nrow(store_read("picks"))) return(showNotification("The draft has started. Undo every pick before changing the draft order.", type = "error"))
    m$draft_order <- sample.int(nrow(m))
    save("managers", m)
    m <- m[order(m$draft_order), ]
    showModal(modalDialog(title = "New draft order", easyClose = TRUE,
      tags$ol(lapply(seq_len(nrow(m)), function(i) tags$li(tags$b(m$manager[i]), " - ", m$team_name[i]))),
      p(class = "text-muted small", "Snake draft: the order reverses every round."),
      footer = modalButton("Done")))
  })

  output$backup <- downloadHandler(
    filename = function() sprintf("fantasy_hockey_backup_%s.xlsx", Sys.Date()),
    content = function(file) {
      t <- tables()
      openxlsx::write.xlsx(list(standings = standings(), managers = t$managers,
                                picks = t$picks, game_log = t$game_log), file)
    })
}

shinyApp(ui, server)
