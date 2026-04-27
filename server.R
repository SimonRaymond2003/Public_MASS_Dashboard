server <- function(input, output, session) {
  
  pal3 <- c(PINK, NAVY, GREEN)
  
  # ── FILTER BAR OBSERVERS ───────────────────────────────────────────────────
  # Top-level filters use real combos only (VALID_COMBOS)
  observeEvent(input$sel_cat, {
    if (!is.null(input$sel_cat) && input$sel_cat != "") {
      valid_discs <- VALID_COMBOS %>% filter(Category == input$sel_cat) %>% pull(Discipline) %>% sort()
      cur <- if (input$sel_disc %in% valid_discs) input$sel_disc else ""
      updateSelectInput(session, "sel_disc", choices = c("All" = "", valid_discs), selected = cur)
    } else {
      updateSelectInput(session, "sel_disc", choices = c("All" = "", DISCIPLINES), selected = input$sel_disc)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$sel_disc, {
    if (!is.null(input$sel_disc) && input$sel_disc != "") {
      valid_cats <- VALID_COMBOS %>% filter(Discipline == input$sel_disc) %>% pull(Category) %>% sort()
      cur <- if (input$sel_cat %in% valid_cats) input$sel_cat else ""
      updateSelectInput(session, "sel_cat", choices = c("All" = "", valid_cats), selected = cur)
    } else {
      updateSelectInput(session, "sel_cat", choices = c("All" = "", CATEGORIES), selected = input$sel_cat)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$clear, {
    updateSelectInput(session, "sel_year", selected = as.character(MAX_YEAR_CAPPED))
    updateSelectInput(session, "sel_prov", selected = "")
    updateSelectInput(session, "sel_cat",  choices = c("All" = "", CATEGORIES),  selected = "")
    updateSelectInput(session, "sel_disc", choices = c("All" = "", DISCIPLINES), selected = "")
  })
  
  # ── DATA SELECTORS ─────────────────────────────────────────────────────────
  # Summary stats use sum_mult_method and sum_base_metric
  get_org_data <- function() {
    mix <- !is.null(input$sum_mult_method) && input$sum_mult_method == "mixture"
    rev <- !is.null(input$sum_base_metric) && input$sum_base_metric == "rev"
    if (mix && rev)  return(org_data_mixture_rev)
    if (mix && !rev) return(org_data_mixture_exp)
    if (!mix && rev) return(org_data_single_rev)
    org_data_single_exp
  }
  
  fdata <- function() {
    df <- get_org_data()
    if (!is.null(input$sel_year) && input$sel_year != "") df <- df %>% filter(Year == as.numeric(input$sel_year))
    if (!is.null(input$sel_prov) && input$sel_prov != "") df <- df %>% filter(Province == input$sel_prov)
    if (!is.null(input$sel_cat)  && input$sel_cat  != "") df <- df %>% filter(Category == input$sel_cat)
    if (!is.null(input$sel_disc) && input$sel_disc != "") df <- df %>% filter(Discipline == input$sel_disc)
    df
  }
  
  fdata_allyears <- function() {
    df <- get_org_data()
    if (!is.null(input$sel_prov) && input$sel_prov != "") df <- df %>% filter(Province == input$sel_prov)
    if (!is.null(input$sel_cat)  && input$sel_cat  != "") df <- df %>% filter(Category == input$sel_cat)
    if (!is.null(input$sel_disc) && input$sel_disc != "") df <- df %>% filter(Discipline == input$sel_disc)
    df
  }
  
  # ── AGGREGATES ─────────────────────────────────────────────────────────────
  agg <- reactive({
    df <- fdata()
    n  <- n_distinct(df$`Business Number`)
    org_level <- df %>%
      group_by(`Business Number`) %>%
      summarise(gdp  = sum(imp_gdp_total,       na.rm = TRUE),
                jobs = sum(imp_jobs_total,       na.rm = TRUE),
                exp  = sum(`Total Expenditures`, na.rm = TRUE),
                rev  = sum(`Total Revenue`,      na.rm = TRUE),
                .groups = "drop")
    list(n_orgs       = n,
         gdp_total    = sum(df$imp_gdp_total,  na.rm = TRUE),
         gdp_med_org  = if (n > 0) median(org_level$gdp,  na.rm = TRUE) else 0,
         jobs_total   = sum(df$imp_jobs_total, na.rm = TRUE),
         jobs_med_org = if (n > 0) median(org_level$jobs, na.rm = TRUE) else 0,
         med_exp      = if (n > 0) median(org_level$exp,  na.rm = TRUE) else 0,
         med_rev      = if (n > 0) median(org_level$rev,  na.rm = TRUE) else 0,
         mean_exp     = if (n > 0) mean(org_level$exp,    na.rm = TRUE) else 0,
         mean_rev     = if (n > 0) mean(org_level$rev,    na.rm = TRUE) else 0)
  })
  
  output$fb_orgs <- renderText(comma(agg()$n_orgs))
  
  stat_row <- function(label, val_main, val_med) {
    div(style = "display:flex; justify-content:space-between; align-items:baseline; padding:6px 0; border-bottom:1px solid #f0ede8;",
        div(style = "font-size:0.72rem; font-weight:700; text-transform:uppercase; letter-spacing:0.4px; color:#888;", label),
        div(style = "display:flex; align-items:baseline; gap:8px;",
            div(style = sprintf("font-size:1.7rem; font-weight:800; color:%s; line-height:1;", NAVY), val_main),
            if (nchar(val_med) > 0) div(style = "font-size:0.68rem; color:#bbb; white-space:nowrap;", val_med)))
  }
  
  output$kpi_panel_impact <- renderUI({
    a <- agg()
    div(class = "chart-card", style = sprintf("border-top:3px solid %s; height:100%%;", NAVY),
        div(style = "font-size:0.7rem; font-weight:800; text-transform:uppercase; letter-spacing:0.6px; color:#aaa; margin-bottom:4px;",
            "Economic Impact",
            tags$span("ⓘ", id = "sum_gdp_mult_info_btn",
                      `data-bs-toggle` = "popover", `data-bs-placement` = "bottom",
                      `data-bs-content` = "Multipliers are applied assuming the broader economy is unchanged. Removing this subsector would not materially shift the multipliers themselves. Results are indicative of scale, not counterfactual impact.",
                      `data-bs-trigger` = "click",
                      style = "cursor:pointer; font-size:0.6rem; color:rgba(0,0,0,0.35); font-weight:400; text-transform:none; letter-spacing:0; margin-left:3px;")),
        stat_row("Total GDP Contribution", paste0("$", comma(round(a$gdp_total))),  paste0("med. $", comma(round(a$gdp_med_org)),  " / org")),
        stat_row("Total Jobs Supported",   comma(round(a$jobs_total)),              paste0("med. ", round(a$jobs_med_org, 1), " / org")))
  })
  
  output$kpi_panel_finance <- renderUI({
    a   <- agg()
    rev <- !is.null(input$sum_base_metric) && input$sum_base_metric == "rev"
    lbl      <- if (rev) "Revenue" else "Expenditures"
    col      <- if (rev) GREEN else PINK
    med_val  <- if (rev) paste0("$", comma(round(a$med_rev)))  else paste0("$", comma(round(a$med_exp)))
    mean_val <- if (rev) paste0("$", comma(round(a$mean_rev))) else paste0("$", comma(round(a$mean_exp)))
    div(class = "chart-card", style = sprintf("border-top:3px solid %s; height:100%%;", col),
        div(style = "font-size:0.7rem; font-weight:800; text-transform:uppercase; letter-spacing:0.6px; color:#aaa; margin-bottom:4px;",
            paste0("Org Financials — ", lbl, " basis")),
        stat_row(paste0("Median ", lbl, " per Org"),  med_val,  ""),
        stat_row(paste0("Mean ",   lbl, " per Org"),  mean_val, ""),
        div(style = "display:flex; justify-content:space-between; align-items:baseline; padding:6px 0; border-bottom:1px solid #f0ede8;",
            div(style = "font-size:0.72rem; font-weight:700; text-transform:uppercase; letter-spacing:0.4px; color:#888;", "Orgs in Selection"),
            div(style = sprintf("font-size:1.1rem; font-weight:800; color:%s; line-height:1;", NAVY), comma(a$n_orgs))))
  })
  
  # ── YEAR CHARTS ────────────────────────────────────────────────────────────
  make_year_chart <- function(df, prefix = "$") {
    df <- df %>% mutate(Type = factor(Type, levels = c("Induced","Indirect","Direct")))
    ht <- if (prefix == "$")
      "%{x}<br>%{fullData.name}: $%{y:,.0f}<extra></extra>"
    else
      "%{x}<br>%{fullData.name}: %{y:,.0f} jobs<extra></extra>"
    years_past <- unique(df$Year[df$Year > MAX_MULT_YEAR])
    p <- plot_ly(df, x = ~Year, y = ~Impact, color = ~Type, type = "bar",
                 colors = setNames(rev(pal3), c("Direct","Indirect","Induced")),
                 hovertemplate = ht, width = NULL) %>%
      layout(barmode = "stack", bargap = 0.45,
             xaxis  = list(title = "", dtick = 1, tickfont = list(size = 10)),
             yaxis  = list(title = "", tickprefix = if (prefix == "$") "$" else "",
                           separatethousands = TRUE, tickfont = list(size = 10)),
             legend = list(orientation = "h", y = -0.18, x = 0.5, xanchor = "center", font = list(size = 10)),
             margin = list(t = if (length(years_past) > 0) 30 else 5, r = 10),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(family = "Inter"))
    if (length(years_past) > 0)
      p <- p %>% layout(annotations = list(list(
        text    = paste0("* ", paste(years_past, collapse = ", "), " using ", MAX_MULT_YEAR, " multipliers"),
        xref    = "paper", yref = "paper", x = 1, y = 1.08, xanchor = "right", showarrow = FALSE,
        font    = list(size = 10, color = "#999", family = "Inter"))))
    p
  }
  
  output$plot_gdp_year <- renderPlotly({
    df <- fdata_allyears() %>%
      group_by(Year) %>%
      summarise(Direct   = sum(imp_gdp_direct,  na.rm = TRUE),
                Indirect = sum(imp_gdp_indirect, na.rm = TRUE),
                Induced  = sum(imp_gdp_induced,  na.rm = TRUE), .groups = "drop") %>%
      pivot_longer(c(Direct, Indirect, Induced), names_to = "Type", values_to = "Impact")
    make_year_chart(df, "$")
  })
  
  output$plot_jobs_year <- renderPlotly({
    df <- fdata_allyears() %>%
      group_by(Year) %>%
      summarise(Direct   = sum(imp_jobs_direct,  na.rm = TRUE),
                Indirect = sum(imp_jobs_indirect, na.rm = TRUE),
                Induced  = sum(imp_jobs_induced,  na.rm = TRUE), .groups = "drop") %>%
      pivot_longer(c(Direct, Indirect, Induced), names_to = "Type", values_to = "Impact")
    make_year_chart(df, "")
  })
  
  # ── PROVINCIAL MAP ─────────────────────────────────────────────────────────
  output$plot_map <- renderLeaflet({
    pc     <- isTRUE(input$per_capita)
    sel_yr <- if (!is.null(input$sel_year) && input$sel_year != "") as.numeric(input$sel_year) else MAX_YEAR_CAPPED
    
    prov_data <- fdata() %>%
      group_by(Province) %>%
      summarise(n          = n_distinct(`Business Number`),
                gdp_total  = sum(imp_gdp_total,    na.rm = TRUE),
                gdp_wp     = sum(imp_gdp_wp_total,  na.rm = TRUE),
                jobs_total = sum(imp_jobs_total,   na.rm = TRUE),
                jobs_wp    = sum(imp_jobs_wp_total, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(gdp_leaked  = gdp_total  - gdp_wp,
             jobs_leaked = jobs_total - jobs_wp,
             gdp_pct_wp  = ifelse(gdp_total  > 0, round(gdp_wp  / gdp_total  * 100, 1), 0),
             jobs_pct_wp = ifelse(jobs_total > 0, round(jobs_wp / jobs_total * 100, 1), 0))
    
    pop_yr <- pop_data %>% filter(Year == sel_yr)
    if (nrow(pop_yr) == 0) pop_yr <- pop_data %>% filter(Year == max(Year))
    prov_data <- prov_data %>%
      left_join(pop_yr %>% select(Province, population), by = "Province") %>%
      mutate(population = replace_na(population, 1))
    
    if (pc) {
      prov_data <- prov_data %>%
        mutate(gdp_display    = gdp_total  / population,
               gdp_wp_disp   = gdp_wp     / population,
               gdp_leak_disp = gdp_leaked / population,
               jobs_display   = jobs_total  / (population / 1000),
               jobs_wp_disp  = jobs_wp     / (population / 1000),
               jobs_leak_disp= jobs_leaked / (population / 1000))
    } else {
      prov_data <- prov_data %>%
        mutate(gdp_display    = gdp_total,  gdp_wp_disp   = gdp_wp,     gdp_leak_disp = gdp_leaked,
               jobs_display   = jobs_total, jobs_wp_disp  = jobs_wp,    jobs_leak_disp= jobs_leaked)
    }
    
    map_df <- prov_sf %>%
      left_join(prov_data, by = c("prov_abbr" = "Province")) %>%
      mutate(across(c(n, gdp_total, gdp_wp, gdp_leaked, jobs_total, jobs_wp, jobs_leaked,
                      gdp_pct_wp, jobs_pct_wp, population,
                      gdp_display, gdp_wp_disp, gdp_leak_disp,
                      jobs_display, jobs_wp_disp, jobs_leak_disp), ~replace_na(.x, 0))) %>%
      st_transform(4326)
    
    pal <- colorNumeric(palette = c("#d8d5cf","#6a5acd", NAVY), domain = map_df$gdp_display, na.color = "#e8e5e0")
    
    if (pc) {
      labels <- sprintf(
        "<div style='font-family:Inter,sans-serif; font-size:13px;'>
          <b style='font-size:15px;'>%s</b><br>Organizations: %s | Pop: %s<br><br>
          <b style='color:%s;'>GDP Impact per Capita</b><br>Total: $%s<br>Within Province: $%s (%s%%)<br>Leakage: $%s (%s%%)<br><br>
          <b style='color:%s;'>Jobs per 1,000 People</b><br>Total: %s<br>Within Province: %s (%s%%)<br>Leakage: %s (%s%%)</div>",
        map_df$prov_abbr, comma(map_df$n), comma(map_df$population), NAVY,
        comma(round(map_df$gdp_display)), comma(round(map_df$gdp_wp_disp)), map_df$gdp_pct_wp,
        comma(round(map_df$gdp_leak_disp)), round(100 - map_df$gdp_pct_wp, 1), PINK,
        round(map_df$jobs_display, 2), round(map_df$jobs_wp_disp, 2), map_df$jobs_pct_wp,
        round(map_df$jobs_leak_disp, 2), round(100 - map_df$jobs_pct_wp, 1)) %>% lapply(htmltools::HTML)
    } else {
      labels <- sprintf(
        "<div style='font-family:Inter,sans-serif; font-size:13px;'>
          <b style='font-size:15px;'>%s</b><br>Organizations: %s<br><br>
          <b style='color:%s;'>GDP Impact</b><br>Total: $%s<br>Within Province: $%s (%s%%)<br>Leakage: $%s (%s%%)<br><br>
          <b style='color:%s;'>Jobs Supported</b><br>Total: %s<br>Within Province: %s (%s%%)<br>Leakage: %s (%s%%)</div>",
        map_df$prov_abbr, comma(map_df$n), NAVY,
        comma(round(map_df$gdp_total)), comma(round(map_df$gdp_wp)), map_df$gdp_pct_wp,
        comma(round(map_df$gdp_leaked)), round(100 - map_df$gdp_pct_wp, 1), PINK,
        comma(round(map_df$jobs_total)), comma(round(map_df$jobs_wp)), map_df$jobs_pct_wp,
        comma(round(map_df$jobs_leaked)), round(100 - map_df$jobs_pct_wp, 1)) %>% lapply(htmltools::HTML)
    }
    
    leaflet(map_df, options = leafletOptions(
      minZoom = 3, maxZoom = 8,
      maxBounds = list(c(38, -145), c(75, -50)),
      maxBoundsViscosity = 1.0, scrollWheelZoom = FALSE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels,   options = providerTileOptions(opacity = 0.6)) %>%
      addProviderTiles(providers$CartoDB.PositronOnlyLabels, options = providerTileOptions(opacity = 0.4)) %>%
      setView(lng = -96, lat = 56, zoom = 4) %>%
      addPolygons(fillColor = ~pal(gdp_display), fillOpacity = 0.65,
                  weight = 1.5, color = "#ffffff", opacity = 0.9,
                  highlightOptions = highlightOptions(weight = 3, color = PINK, fillOpacity = 0.8, bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("padding" = "10px 14px", "border-radius" = "8px",
                                 "background-color" = "white", "box-shadow" = "0 2px 8px rgba(0,0,0,0.15)", "border" = "none"),
                    textsize = "13px", direction = "auto")) %>%
      addLegend(position = "bottomright", pal = pal, values = ~gdp_display,
                title = if (pc) "GDP / Capita" else "GDP Impact",
                labFormat = labelFormat(prefix = "$", big.mark = ","), opacity = 0.8)
  })
  
  
  # ── PORTFOLIO IMPACT ───────────────────────────────────────────────────────
  
  # Reactive store for portfolio orgs
  port_orgs <- reactiveVal(data.frame(
    id = character(), name = character(), count = numeric(),
    amount = numeric(),
    prov = character(), cat = character(), disc = character(),
    yr = numeric(), org_age = numeric(), base = character(),
    mult_lbl = character(),
    gdp_d = numeric(), gdp_i = numeric(), gdp_in = numeric(), gdp_t = numeric(),
    gdp_wp = numeric(),
    jobs_d = numeric(), jobs_i = numeric(), jobs_in = numeric(), jobs_t = numeric(),
    jobs_wp = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Store multiplier detail per org id (codes, weights, per-code mults, effective mults)
  port_mult_detail <- reactiveVal(list())
  
  # Portfolio category/discipline cascading
  observeEvent(input$port_cat, {
    valid_discs <- VALID_COMBOS_FULL %>% filter(Category == input$port_cat) %>% pull(Discipline) %>% sort()
    cur <- if (!is.null(input$port_disc) && input$port_disc %in% valid_discs) input$port_disc else valid_discs[1]
    updateSelectInput(session, "port_disc", choices = valid_discs, selected = cur)
  }, ignoreInit = FALSE)
  
  observeEvent(input$port_disc, {
    valid_cats <- VALID_COMBOS_FULL %>% filter(Discipline == input$port_disc) %>% pull(Category) %>% sort()
    cur <- if (!is.null(input$port_cat) && input$port_cat %in% valid_cats) input$port_cat else valid_cats[1]
    updateSelectInput(session, "port_cat", choices = valid_cats, selected = cur)
  }, ignoreInit = TRUE)
  
  # Bulk category/discipline cascading
  observeEvent(input$bulk_cat, {
    valid_discs <- VALID_COMBOS_FULL %>% filter(Category == input$bulk_cat) %>% pull(Discipline) %>% sort()
    cur <- if (!is.null(input$bulk_disc) && input$bulk_disc %in% valid_discs) input$bulk_disc else valid_discs[1]
    updateSelectInput(session, "bulk_disc", choices = valid_discs, selected = cur)
  }, ignoreInit = FALSE)

  observeEvent(input$bulk_disc, {
    valid_cats <- VALID_COMBOS_FULL %>% filter(Discipline == input$bulk_disc) %>% pull(Category) %>% sort()
    cur <- if (!is.null(input$bulk_cat) && input$bulk_cat %in% valid_cats) input$bulk_cat else valid_cats[1]
    updateSelectInput(session, "bulk_cat", choices = valid_cats, selected = cur)
  }, ignoreInit = TRUE)

  # Bulk add handler
  observeEvent(input$bulk_add, {
    n   <- as.integer(input$bulk_n %||% 1)
    amt <- as.numeric(input$bulk_amount %||% 0)
    if (length(n) == 0 || length(amt) == 0 || is.na(n) || is.na(amt) || n < 1 || amt <= 0) return()
    prov <- input$bulk_prov; cat <- input$bulk_cat; disc <- input$bulk_disc
    yr   <- as.numeric(input$bulk_year)
    use_rev    <- !is.null(input$bulk_base_metric) && input$bulk_base_metric == "rev"
    use_mix    <- !is.null(input$bulk_mult_method) && input$bulk_mult_method == "mixture"
    use_custom <- !is.null(input$bulk_mult_method) && input$bulk_mult_method == "custom"

    combo_row   <- combo_map_v2 %>% filter(Category == cat, Discipline == disc)
    ind_primary <- if (nrow(combo_row) > 0) combo_row$ind_primary[1] else "BS71A000"
    ind_all     <- if (nrow(combo_row) > 0) combo_row$ind_all[[1]]   else "BS71A000"

    weights <- NULL; method_lbl <- "Primary"
    if (use_custom) {
      custom_codes <- Filter(function(v) !is.null(v) && v != "" && v != "__placeholder__", lapply(1:5, function(k) input[[paste0("bulk_code_", k)]]))
      custom_codes <- unlist(custom_codes)
      if (is.null(custom_codes) || length(custom_codes) == 0) return()
      n_c <- length(custom_codes)
      if (n_c == 1) { weights <- 1; m <- get_mult_row(prov, yr, custom_codes) }
      else if (n_c == 2) {
        w1 <- if (!is.null(input$bulk_weight_slider)) input$bulk_weight_slider / 100 else 0.5
        weights <- c(w1, 1 - w1); m <- get_mult_row_weighted(prov, yr, custom_codes, weights)
      } else {
        raw_w <- sapply(seq_len(n_c), function(i) { v <- input[[paste0("bulk_cw_", i)]]; if (is.null(v) || is.na(as.numeric(v))) round(100 / n_c) else as.numeric(v) })
        weights <- raw_w / sum(raw_w); m <- get_mult_row_weighted(prov, yr, custom_codes, weights)
      }
      ind_all <- custom_codes; method_lbl <- "Custom"
    } else if (use_mix && length(ind_all) == 2) {
      w1 <- if (!is.null(input$bulk_weight_slider)) input$bulk_weight_slider / 100 else 0.5
      weights <- c(w1, 1 - w1); m <- get_mult_row_weighted(prov, yr, ind_all, weights); method_lbl <- "Mixture"
    } else if (use_mix && length(ind_all) > 1) {
      weights <- rep(1 / length(ind_all), length(ind_all)); m <- get_mult_row_mixture(prov, yr, ind_all); method_lbl <- "Mixture"
    } else {
      weights <- 1; m <- get_mult_row(prov, yr, ind_primary)
    }

    codes_used <- if (use_custom) ind_all else if (use_mix && length(ind_all) > 1) ind_all else ind_primary
    ind_rows   <- lapply(codes_used, function(ind) get_mult_row(prov, yr, ind))
    mult_lbl   <- if (length(codes_used) == 1) codes_used else paste0(codes_used, " (", round(weights * 100), "%)", collapse = " + ")

    total_amt <- amt * n
    amt_m     <- total_amt / 1e6
    org_name  <- paste0(n, " × ", disc, " (", prov, ")")
    org_id    <- paste0("p_", as.integer(Sys.time()), "_", sample(1000, 1))

    dl <- port_mult_detail()
    dl[[org_id]] <- list(codes = codes_used, weights = weights, ind_primary = ind_primary,
                         ind_rows = ind_rows, m_eff = m, method = method_lbl,
                         count = n, amt_per_org = amt)
    port_mult_detail(dl)

    new_row <- data.frame(
      id = org_id, name = org_name, count = n, amount = total_amt,
      prov = prov, cat = cat, disc = disc,
      yr = yr, org_age = NA_real_,
      base = if (use_rev) "Rev" else "Exp",
      mult_lbl = mult_lbl,
      gdp_d  = total_amt * m$gdp_direct[1],
      gdp_i  = total_amt * m$gdp_indirect[1],
      gdp_in = total_amt * m$gdp_induced[1],
      gdp_t  = total_amt * m$gdp_total[1],
      gdp_wp = total_amt * m$gdp_wp_total[1],
      jobs_d  = amt_m * m$jobs_direct[1],
      jobs_i  = amt_m * m$jobs_indirect[1],
      jobs_in = amt_m * m$jobs_induced[1],
      jobs_t  = amt_m * m$jobs_total[1],
      jobs_wp = amt_m * m$jobs_wp_total[1],
      stringsAsFactors = FALSE
    )
    port_orgs(rbind(port_orgs(), new_row))
  })

  # Amount input with dynamic label
  output$port_amount_lbl <- renderText({
    if (!is.null(input$port_base_metric) && input$port_base_metric == "rev") "Revenue ($)" else "Expenditures ($)"
  })

  output$bulk_amount_lbl <- renderText({
    if (!is.null(input$bulk_base_metric) && input$bulk_base_metric == "rev") "Revenue per Org ($)" else "Expenditures per Org ($)"
  })

  # Industry codes for current portfolio selection (for mixture slider)
  port_ind_codes <- reactive({
    cat <- input$port_cat; disc <- input$port_disc
    req(cat, disc)
    combo_row <- combo_map_v2 %>% filter(Category == cat, Discipline == disc)
    if (nrow(combo_row) > 0) combo_row$ind_all[[1]] else "BS71A000"
  })

  bulk_ind_codes <- reactive({
    cat <- input$bulk_cat; disc <- input$bulk_disc
    req(cat, disc)
    combo_row <- combo_map_v2 %>% filter(Category == cat, Discipline == disc)
    if (nrow(combo_row) > 0) combo_row$ind_all[[1]] else "BS71A000"
  })

  # Weight UI — mixture slider or custom code selects
  output$port_weight_ui <- renderUI({
    method <- input$port_mult_method %||% "single"

    if (method == "mixture") {
      codes <- port_ind_codes()
      n <- length(codes)
      if (n < 2) return(tags$div(style = "font-size:0.68rem; color:#aaa; font-style:italic; padding:2px 0;",
                                 paste0("Only one industry code applies (", codes[1], ") — no mixing needed.")))
      default_v <- round(100 / n)
      cards <- lapply(seq_len(n), function(i) {
        lbl <- names(CUSTOM_IND_CODES)[CUSTOM_IND_CODES == codes[i]][1] %||% codes[i]
        tags$div(style = "flex:1; min-width:110px; background:#f5f3ef; border-radius:8px; padding:8px 10px;",
          tags$div(style = "font-size:0.68rem; font-weight:700; color:#444; margin-bottom:1px; line-height:1.3;", lbl),
          tags$div(style = "font-size:0.55rem; color:#bbb; margin-bottom:6px;", codes[i]),
          tags$input(id = paste0("port_cw_", i), type = "range",
                     min = 0, max = 100, value = default_v, step = 1,
                     style = "width:100%; cursor:pointer; accent-color:#1B1464; margin-bottom:4px;",
                     onmousedown = sprintf("cwSnapshot('port',%d);", n),
                     ontouchstart = sprintf("cwSnapshot('port',%d);", n),
                     oninput = sprintf("cwSliderChanged('port',%d,parseFloat(this.value),%d);", i, n)),
          tags$div(style = "display:flex; align-items:center; gap:4px;",
            tags$input(id = paste0("port_cwn_", i), type = "number",
                       min = 0, max = 100, value = default_v, step = 1,
                       style = "width:44px; padding:2px 4px; border:1px solid #ddd; border-radius:4px; font-size:0.7rem; text-align:center; background:white;",
                       oninput = sprintf("cwNumChanged('port',%d,parseFloat(this.value),%d);", i, n)),
            tags$span(style = "font-size:0.65rem; color:#aaa;", "%"))
        )
      })
      return(tagList(
        tags$div(style = "display:flex; flex-wrap:wrap; gap:8px; margin-top:8px;", cards)
      ))
    }

    if (method == "custom") {
      filled <- 0
      for (k in 1:5) {
        v <- input[[paste0("port_code_", k)]]
        if (!is.null(v) && v != "" && v != "__placeholder__") filled <- k else break
      }
      n_rows <- min(filled + 1, 5)
      choices_with_blank <- c("Select a code" = "__placeholder__", CUSTOM_IND_CODES)
      rows <- lapply(seq_len(n_rows), function(i) {
        cur_val <- input[[paste0("port_code_", i)]] %||% "__placeholder__"
        tags$div(style = "display:flex; align-items:center; gap:8px; margin-bottom:5px;",
          tags$span(style = "font-size:0.65rem; font-weight:700; color:#aaa; width:16px; flex-shrink:0;", paste0(i, ".")),
          selectInput(paste0("port_code_", i), NULL, choices = choices_with_blank,
                      selected = cur_val, selectize = FALSE, width = "100%"),
          if (i <= filled)
            tags$button(type = "button",
              style = "background:none; border:none; color:#ccc; cursor:pointer; font-size:1rem; padding:0 4px; line-height:1;",
              onclick = sprintf("Shiny.setInputValue('port_code_%d','__placeholder__');Shiny.setInputValue('port_code_clear',%d);", i, i),
              HTML("&times;"))
        )
      })
      return(tagList(rows))
    }

    NULL
  })

  output$port_custom_weights_ui <- renderUI({
    sel <- Filter(function(v) !is.null(v) && v != "" && v != "__placeholder__", lapply(1:5, function(k) input[[paste0("port_code_", k)]]))
    sel <- unlist(sel)
    n <- length(sel)
    if (n < 2) return(NULL)
    default_v <- round(100 / n)
    cards <- lapply(seq_len(n), function(i) {
      lbl <- names(CUSTOM_IND_CODES)[CUSTOM_IND_CODES == sel[i]][1] %||% sel[i]
      tags$div(style = "flex:1; min-width:110px; max-width:180px; background:#f5f3ef; border-radius:8px; padding:8px 10px;",
        tags$div(style = "font-size:0.68rem; font-weight:700; color:#444; margin-bottom:1px; line-height:1.3;", lbl),
        tags$div(style = "font-size:0.55rem; color:#bbb; margin-bottom:6px;", sel[i]),
        tags$input(id = paste0("port_cw_", i), type = "range",
                   min = 0, max = 100, value = default_v, step = 1,
                   style = "width:100%; cursor:pointer; accent-color:#1B1464; margin-bottom:4px;",
                   onmousedown = sprintf("cwSnapshot('port',%d);", n),
                   ontouchstart = sprintf("cwSnapshot('port',%d);", n),
                   oninput = sprintf("cwSliderChanged('port',%d,parseFloat(this.value),%d);", i, n)),
        tags$div(style = "display:flex; align-items:center; gap:4px;",
          tags$input(id = paste0("port_cwn_", i), type = "number",
                     min = 0, max = 100, value = default_v, step = 1,
                     style = "width:44px; padding:2px 4px; border:1px solid #ddd; border-radius:4px; font-size:0.7rem; text-align:center; background:white;",
                     oninput = sprintf("cwNumChanged('port',%d,parseFloat(this.value),%d);", i, n)),
          tags$span(style = "font-size:0.65rem; color:#aaa;", "%"))
      )
    })
    tagList(tags$div(style = "display:flex; flex-wrap:wrap; gap:8px; margin-top:8px;", cards))
  })

  output$bulk_weight_ui <- renderUI({
    method <- input$bulk_mult_method %||% "single"

    if (method == "mixture") {
      codes <- bulk_ind_codes()
      n <- length(codes)
      if (n < 2) return(tags$div(style = "font-size:0.68rem; color:#aaa; font-style:italic; padding:2px 0;",
                                 paste0("Only one industry code applies (", codes[1], ") — no mixing needed.")))
      default_v <- round(100 / n)
      cards <- lapply(seq_len(n), function(i) {
        lbl <- names(CUSTOM_IND_CODES)[CUSTOM_IND_CODES == codes[i]][1] %||% codes[i]
        tags$div(style = "flex:1; min-width:110px; background:#f5f3ef; border-radius:8px; padding:8px 10px;",
          tags$div(style = "font-size:0.68rem; font-weight:700; color:#444; margin-bottom:1px; line-height:1.3;", lbl),
          tags$div(style = "font-size:0.55rem; color:#bbb; margin-bottom:6px;", codes[i]),
          tags$input(id = paste0("bulk_cw_", i), type = "range",
                     min = 0, max = 100, value = default_v, step = 1,
                     style = "width:100%; cursor:pointer; accent-color:#1B1464; margin-bottom:4px;",
                     onmousedown = sprintf("cwSnapshot('bulk',%d);", n),
                     ontouchstart = sprintf("cwSnapshot('bulk',%d);", n),
                     oninput = sprintf("cwSliderChanged('bulk',%d,parseFloat(this.value),%d);", i, n)),
          tags$div(style = "display:flex; align-items:center; gap:4px;",
            tags$input(id = paste0("bulk_cwn_", i), type = "number",
                       min = 0, max = 100, value = default_v, step = 1,
                       style = "width:44px; padding:2px 4px; border:1px solid #ddd; border-radius:4px; font-size:0.7rem; text-align:center; background:white;",
                       oninput = sprintf("cwNumChanged('bulk',%d,parseFloat(this.value),%d);", i, n)),
            tags$span(style = "font-size:0.65rem; color:#aaa;", "%"))
        )
      })
      return(tagList(tags$div(style = "display:flex; flex-wrap:wrap; gap:8px; margin-top:8px;", cards)))
    }

    if (method == "custom") {
      filled <- 0
      for (k in 1:5) {
        v <- input[[paste0("bulk_code_", k)]]
        if (!is.null(v) && v != "" && v != "__placeholder__") filled <- k else break
      }
      n_rows <- min(filled + 1, 5)
      choices_with_blank <- c("Select a code" = "__placeholder__", CUSTOM_IND_CODES)
      rows <- lapply(seq_len(n_rows), function(i) {
        cur_val <- input[[paste0("bulk_code_", i)]] %||% "__placeholder__"
        tags$div(style = "display:flex; align-items:center; gap:8px; margin-bottom:5px;",
          tags$span(style = "font-size:0.65rem; font-weight:700; color:#aaa; width:16px; flex-shrink:0;", paste0(i, ".")),
          selectInput(paste0("bulk_code_", i), NULL, choices = choices_with_blank,
                      selected = cur_val, selectize = FALSE, width = "100%"),
          if (i <= filled)
            tags$button(type = "button",
              style = "background:none; border:none; color:#ccc; cursor:pointer; font-size:1rem; padding:0 4px; line-height:1;",
              onclick = sprintf("Shiny.setInputValue('bulk_code_%d','__placeholder__');Shiny.setInputValue('bulk_code_clear',%d);", i, i),
              HTML("&times;"))
        )
      })
      return(tagList(rows))
    }

    NULL
  })

  output$bulk_custom_weights_ui <- renderUI({
    sel <- Filter(function(v) !is.null(v) && v != "" && v != "__placeholder__", lapply(1:5, function(k) input[[paste0("bulk_code_", k)]]))
    sel <- unlist(sel)
    n <- length(sel)
    if (n < 2) return(NULL)
    default_v <- round(100 / n)
    cards <- lapply(seq_len(n), function(i) {
      lbl <- names(CUSTOM_IND_CODES)[CUSTOM_IND_CODES == sel[i]][1] %||% sel[i]
      tags$div(style = "flex:1; min-width:110px; max-width:180px; background:#f5f3ef; border-radius:8px; padding:8px 10px;",
        tags$div(style = "font-size:0.68rem; font-weight:700; color:#444; margin-bottom:1px; line-height:1.3;", lbl),
        tags$div(style = "font-size:0.55rem; color:#bbb; margin-bottom:6px;", sel[i]),
        tags$input(id = paste0("bulk_cw_", i), type = "range",
                   min = 0, max = 100, value = default_v, step = 1,
                   style = "width:100%; cursor:pointer; accent-color:#1B1464; margin-bottom:4px;",
                   onmousedown = sprintf("cwSnapshot('bulk',%d);", n),
                   ontouchstart = sprintf("cwSnapshot('bulk',%d);", n),
                   oninput = sprintf("cwSliderChanged('bulk',%d,parseFloat(this.value),%d);", i, n)),
        tags$div(style = "display:flex; align-items:center; gap:4px;",
          tags$input(id = paste0("bulk_cwn_", i), type = "number",
                     min = 0, max = 100, value = default_v, step = 1,
                     style = "width:44px; padding:2px 4px; border:1px solid #ddd; border-radius:4px; font-size:0.7rem; text-align:center; background:white;",
                     oninput = sprintf("cwNumChanged('bulk',%d,parseFloat(this.value),%d);", i, n)),
          tags$span(style = "font-size:0.65rem; color:#aaa;", "%"))
      )
    })
    tagList(tags$div(style = "display:flex; flex-wrap:wrap; gap:8px; margin-top:8px;", cards))
  })
  
  # Add org to portfolio
  observeEvent(input$port_add, {
    amt <- as.numeric(input$port_amount %||% 0)
    if (length(amt) == 0 || is.na(amt) || amt <= 0) return()
    prov <- input$port_prov; cat <- input$port_cat; disc <- input$port_disc
    yr   <- as.numeric(input$port_year)
    org_age <- as.numeric(input$port_org_age) %||% 10
    use_rev    <- !is.null(input$port_base_metric) && input$port_base_metric == "rev"
    use_mix    <- !is.null(input$port_mult_method) && input$port_mult_method == "mixture"
    use_custom <- !is.null(input$port_mult_method) && input$port_mult_method == "custom"

    combo_row   <- combo_map_v2 %>% filter(Category == cat, Discipline == disc)
    ind_primary <- if (nrow(combo_row) > 0) combo_row$ind_primary[1] else "BS71A000"
    ind_all     <- if (nrow(combo_row) > 0) combo_row$ind_all[[1]]   else "BS71A000"

    weights <- NULL
    method_lbl <- "Primary"

    if (use_custom) {
      custom_codes <- Filter(function(v) !is.null(v) && v != "" && v != "__placeholder__", lapply(1:5, function(k) input[[paste0("port_code_", k)]]))
      custom_codes <- unlist(custom_codes)
      if (is.null(custom_codes) || length(custom_codes) == 0) return()
      n_c <- length(custom_codes)
      if (n_c == 1) {
        weights <- 1
        m <- get_mult_row(prov, yr, custom_codes)
      } else if (n_c == 2) {
        w1 <- if (!is.null(input$port_weight_slider)) input$port_weight_slider / 100 else 0.5
        weights <- c(w1, 1 - w1)
        m <- get_mult_row_weighted(prov, yr, custom_codes, weights)
      } else {
        raw_w <- sapply(seq_len(n_c), function(i) {
          v <- input[[paste0("port_cw_", i)]]
          if (is.null(v) || is.na(as.numeric(v))) round(100 / n_c) else as.numeric(v)
        })
        weights <- raw_w / sum(raw_w)
        m <- get_mult_row_weighted(prov, yr, custom_codes, weights)
      }

      ind_all <- custom_codes
      method_lbl <- "Custom"
    } else if (use_mix && length(ind_all) == 2) {
      w1 <- if (!is.null(input$port_weight_slider)) input$port_weight_slider / 100 else 0.5
      weights <- c(w1, 1 - w1)
      m <- get_mult_row_weighted(prov, yr, ind_all, weights)
      method_lbl <- "Mixture"
    } else if (use_mix && length(ind_all) > 1) {
      weights <- rep(1 / length(ind_all), length(ind_all))
      m <- get_mult_row_mixture(prov, yr, ind_all)
      method_lbl <- "Mixture"
    } else {
      weights <- 1
      m <- get_mult_row(prov, yr, ind_primary)
    }

    # Per-code multiplier rows for detail
    codes_used <- if (use_custom) ind_all else if (use_mix && length(ind_all) > 1) ind_all else ind_primary
    ind_rows <- lapply(codes_used, function(ind) get_mult_row(prov, yr, ind))
    mult_lbl <- if (length(codes_used) == 1) codes_used else paste0(codes_used, " (", round(weights * 100), "%)", collapse = " + ")

    amt_m <- amt / 1e6
    org_name <- if (!is.null(input$port_name) && nchar(trimws(input$port_name)) > 0) trimws(input$port_name) else paste0("Org ", nrow(port_orgs()) + 1)
    org_id <- paste0("p_", as.integer(Sys.time()), "_", sample(1000, 1))

    # Save multiplier detail for this org
    detail <- list(
      codes = codes_used, weights = weights, ind_primary = ind_primary,
      ind_rows = ind_rows,
      m_eff = m,
      method = method_lbl
    )
    dl <- port_mult_detail()
    dl[[org_id]] <- detail
    port_mult_detail(dl)

    new_row <- data.frame(
      id     = org_id,
      name   = org_name,
      count  = 1,
      amount = amt,
      prov   = prov, cat = cat, disc = disc,
      yr     = yr, org_age = org_age,
      base   = if (use_rev) "Rev" else "Exp",
      mult_lbl = mult_lbl,
      gdp_d  = amt   * m$gdp_direct[1],
      gdp_i  = amt   * m$gdp_indirect[1],
      gdp_in = amt   * m$gdp_induced[1],
      gdp_t  = amt   * m$gdp_total[1],
      gdp_wp = amt   * m$gdp_wp_total[1],
      jobs_d  = amt_m * m$jobs_direct[1],
      jobs_i  = amt_m * m$jobs_indirect[1],
      jobs_in = amt_m * m$jobs_induced[1],
      jobs_t  = amt_m * m$jobs_total[1],
      jobs_wp = amt_m * m$jobs_wp_total[1],
      stringsAsFactors = FALSE
    )
    
    port_orgs(rbind(port_orgs(), new_row))
    session$sendCustomMessage("resetPortName", TRUE)
  })
  
  # Clear all
  observeEvent(input$port_clear, {
    port_orgs(port_orgs()[0, , drop = FALSE])
    port_mult_detail(list())
  })
  
  # Conditional visibility
  output$port_has_orgs <- reactive(nrow(port_orgs()) > 0)
  outputOptions(output, "port_has_orgs",            suspendWhenHidden = FALSE)
  outputOptions(output, "port_weight_ui",            suspendWhenHidden = FALSE)
  outputOptions(output, "port_custom_weights_ui",    suspendWhenHidden = FALSE)
  outputOptions(output, "bulk_weight_ui",            suspendWhenHidden = FALSE)
  outputOptions(output, "bulk_custom_weights_ui",    suspendWhenHidden = FALSE)
  
  # Aggregate KPIs — current
  output$port_gdp_total <- renderText(paste0("$", comma(round(sum(port_orgs()$gdp_t, na.rm = TRUE)))))
  output$port_jobs_total <- renderText(round(sum(port_orgs()$jobs_t, na.rm = TRUE), 1))
  
  output$port_gdp_breakdown <- renderUI({
    df <- port_orgs()
    row_fn <- function(l, v, c, def_key = NULL) tags$div(
      style = "display:flex; justify-content:space-between; padding:2px 0;",
      tags$span(style = sprintf("font-size:0.75rem; color:%s; font-weight:700;", c), term(l, def_key)),
      tags$span(style = "font-size:0.75rem; color:#555;", paste0("$", comma(round(v)))))
    tags$div(
      row_fn("Direct",   sum(df$gdp_d,  na.rm = TRUE), PINK, "Direct Impact"),
      row_fn("Indirect", sum(df$gdp_i,  na.rm = TRUE), NAVY, "Indirect Impact"),
      row_fn("Induced",  sum(df$gdp_in, na.rm = TRUE), GREEN, "Induced Impact"))
  })
  
  output$port_jobs_breakdown <- renderUI({
    df <- port_orgs()
    row_fn <- function(l, v, c, def_key = NULL) tags$div(
      style = "display:flex; justify-content:space-between; padding:2px 0;",
      tags$span(style = sprintf("font-size:0.75rem; color:%s; font-weight:700;", c), term(l, def_key)),
      tags$span(style = "font-size:0.75rem; color:#555;", paste0(round(v, 1), " jobs")))
    tags$div(
      row_fn("Direct",   sum(df$jobs_d,  na.rm = TRUE), PINK, "Direct Impact"),
      row_fn("Indirect", sum(df$jobs_i,  na.rm = TRUE), NAVY, "Indirect Impact"),
      row_fn("Induced",  sum(df$jobs_in, na.rm = TRUE), GREEN, "Induced Impact"))
  })
  
  output$port_gdp_leakage <- renderUI({
    df <- port_orgs()
    gdp_t  <- sum(df$gdp_t,  na.rm = TRUE); gdp_wp <- sum(df$gdp_wp, na.rm = TRUE)
    wp_pct <- if (gdp_t > 0) round(gdp_wp / gdp_t * 100, 1) else 0
    tags$div(style = "font-size:0.72rem; color:#aaa; margin-top:4px;",
             paste0("Within-province: ", wp_pct, "% | Leakage: ", round(100 - wp_pct, 1), "%"))
  })
  
  output$port_jobs_leakage <- renderUI({
    df <- port_orgs()
    jobs_t  <- sum(df$jobs_t,  na.rm = TRUE); jobs_wp <- sum(df$jobs_wp, na.rm = TRUE)
    wp_pct <- if (jobs_t > 0) round(jobs_wp / jobs_t * 100, 1) else 0
    tags$div(style = "font-size:0.72rem; color:#aaa; margin-top:4px;",
             paste0("Within-province: ", wp_pct, "% | Leakage: ", round(100 - wp_pct, 1), "%"))
  })
  
  # Summary stats
  output$port_summary_stats <- renderUI({
    df <- port_orgs()
    n  <- nrow(df)
    total_amt <- sum(df$amount, na.rm = TRUE)
    n_prov    <- n_distinct(df$prov)
    n_disc    <- n_distinct(df$disc)

    row_fn <- function(l, v, col = NAVY) tags$div(
      style = "display:flex; justify-content:space-between; padding:5px 0; border-bottom:1px solid #f0ede8;",
      tags$span(style = "font-size:0.72rem; font-weight:700; color:#888; text-transform:uppercase;", l),
      tags$span(style = sprintf("font-size:0.95rem; font-weight:800; color:%s;", col), v))

    tagList(
      row_fn("Organizations", comma(n)),
      row_fn("Total Input",   paste0("$", comma(round(total_amt)))),
      row_fn("Provinces",     n_prov),
      row_fn("Disciplines",   n_disc))
  })
  
  # Portfolio table (includes forecast + expandable multiplier detail)
  output$port_table <- renderUI({
    df <- port_orgs()
    if (nrow(df) == 0) return(NULL)
    details <- port_mult_detail()
    
    rows <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      detail_id <- paste0("detail_", r$id)
      d <- details[[r$id]]
      
      # Build detail content
      detail_html <- if (!is.null(d)) {
        m_eff <- d$m_eff
        code_rows <- lapply(seq_along(d$codes), function(j) {
          mr <- d$ind_rows[[j]]
          w <- if (length(d$codes) > 1) paste0(round(d$weights[j] * 100, 1), "%") else "100%"
          star <- if (d$codes[j] == d$ind_primary) " \u2605" else ""
          tags$tr(style = "font-size:0.72rem; font-family:monospace;",
                  tags$td(style = "padding:2px 8px; font-weight:700;", paste0(d$codes[j], star)),
                  tags$td(style = "padding:2px 8px; text-align:right;", w),
                  tags$td(style = "padding:2px 8px; text-align:right;",
                          paste0(round(mr$gdp_direct[1], 4), " / ", round(mr$gdp_indirect[1], 4), " / ", round(mr$gdp_induced[1], 4))),
                  tags$td(style = "padding:2px 8px; text-align:right;",
                          paste0(round(mr$jobs_direct[1], 3), " / ", round(mr$jobs_indirect[1], 3), " / ", round(mr$jobs_induced[1], 3))))
        })
        
        base_lbl <- r$base
        tags$div(style = "padding:10px 16px; background:#f8f7f4; font-size:0.72rem; color:#555;",
                 tags$div(style = sprintf("font-weight:700; color:%s; margin-bottom:6px;", NAVY),
                          paste0(r$cat, " \u2014 ", r$disc, " | ", r$prov, " ", r$yr, " | Method: ", d$method)),
                 tags$table(style = "width:100%; border-collapse:collapse; margin-bottom:8px;",
                            tags$thead(tags$tr(style = "border-bottom:1px solid #ddd; font-size:0.65rem; color:#999;",
                                               tags$th(style = "padding:2px 8px; text-align:left;", "Code"),
                                               tags$th(style = "padding:2px 8px; text-align:right;", "Weight"),
                                               tags$th(style = "padding:2px 8px; text-align:right;", "GDP (D/I/Id)"),
                                               tags$th(style = "padding:2px 8px; text-align:right;", "Jobs (D/I/Id)"))),
                            tags$tbody(code_rows)),
                 tags$div(style = "font-family:monospace; line-height:1.7;",
                          tags$b("Effective multipliers: "),
                          paste0("GDP: ", round(m_eff$gdp_direct[1], 4), " (direct) + ",
                                 round(m_eff$gdp_indirect[1], 4), " (indirect) + ",
                                 round(m_eff$gdp_induced[1], 4), " (induced) = ",
                                 round(m_eff$gdp_total[1], 4), " total"),
                          tags$br(),
                          paste0("Jobs: ", round(m_eff$jobs_direct[1], 3), " + ",
                                 round(m_eff$jobs_indirect[1], 3), " + ",
                                 round(m_eff$jobs_induced[1], 3), " = ",
                                 round(m_eff$jobs_total[1], 3), " total"),
                          tags$br(), tags$br(),
                          if (!is.null(d$count) && d$count > 1) tagList(
                            tags$b(paste0(base_lbl, " per org: ")),
                            paste0("$", comma(round(d$amt_per_org)), " \u00d7 ", d$count, " orgs = $", comma(round(r$amount)), " total"),
                            tags$br()
                          ),
                          tags$b("GDP: "),
                          paste0("$", comma(round(r$amount)), " \u00d7 ", round(m_eff$gdp_direct[1], 4),
                                 " = $", comma(round(r$gdp_d)), " direct"),
                          tags$br(),
                          paste0("       $", comma(round(r$amount)), " \u00d7 ", round(m_eff$gdp_indirect[1], 4),
                                 " = $", comma(round(r$gdp_i)), " indirect"),
                          tags$br(),
                          paste0("       $", comma(round(r$amount)), " \u00d7 ", round(m_eff$gdp_induced[1], 4),
                                 " = $", comma(round(r$gdp_in)), " induced"),
                          tags$br(),
                          tags$b(paste0("       Total GDP = $", comma(round(r$gdp_t)))),
                          tags$br(),
                          tags$b("Jobs: "),
                          paste0("($", comma(round(r$amount)), " / 1M) \u00d7 ", round(m_eff$jobs_total[1], 3),
                                 " = ", round(r$jobs_t, 2), " jobs")
                 ))
      } else {
        tags$div(style = "padding:8px 16px; font-size:0.72rem; color:#bbb; font-style:italic;", "Detail not available")
      }
      
      tagList(
        tags$tr(style = "cursor:pointer;",
                onclick = sprintf("var d=document.getElementById('%s'); d.style.display = d.style.display==='none' ? 'table-row' : 'none';", detail_id),
                tags$td(style = "font-weight:600; color:#333;",
                        tags$span(style = "font-size:0.65rem; color:#bbb; margin-right:4px;", "\u25b6"), r$name),
                tags$td(paste0("$", comma(round(r$amount)))),
                tags$td(r$prov),
                tags$td(style = "max-width:120px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;", r$disc),
                tags$td(style = sprintf("font-weight:700; color:%s;", NAVY), paste0("$", comma(round(r$gdp_t)))),
                tags$td(style = sprintf("font-weight:700; color:%s;", PINK), round(r$jobs_t, 1)),
                tags$td(actionButton(paste0("port_rm_", r$id), "\u2715",
                                     class = "port-remove-btn",
                                     onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('port_rm_%s', (Math.random()), {priority: 'event'})", r$id)))),
        tags$tr(id = detail_id, style = "display:none;",
                tags$td(colspan = "7", style = "padding:0;", detail_html)))
    })
    
    tags$table(class = "port-table",
               tags$thead(tags$tr(
                 tags$th("Name"), tags$th("Amount"), tags$th("Prov"), tags$th("Discipline"),
                 tags$th("GDP"), tags$th("Jobs"),
                 tags$th(""))),
               tags$tbody(rows))
  })
  
  # Handle remove clicks
  observe({
    df <- port_orgs()
    if (nrow(df) == 0) return()
    for (id in df$id) {
      local({
        local_id <- id
        observeEvent(input[[paste0("port_rm_", local_id)]], {
          current <- port_orgs()
          port_orgs(current[current$id != local_id, , drop = FALSE])
          dl <- port_mult_detail()
          dl[[local_id]] <- NULL
          port_mult_detail(dl)
        }, ignoreInit = TRUE, once = TRUE)
      })
    }
  })
  
  
  # ── CITY EXPLORER ──────────────────────────────────────────────────────────
  city_fdata <- reactive({
    req(input$city_sel, input$city_sel != "")
    cduid <- CD_CHOICES[input$city_sel]

    # Filter business data to those in the selected Census Division
    cd_biz <- business_cd_lookup[business_cd_lookup$CDUID == cduid, ]

    df <- get_org_data()
    df <- df %>%
      semi_join(cd_biz, by = c("Business Number", "Year"))

    if (!is.null(input$sel_year) && input$sel_year != "") df <- df %>% filter(Year == as.numeric(input$sel_year))
    if (!is.null(input$sel_cat)  && input$sel_cat  != "") df <- df %>% filter(Category == input$sel_cat)
    if (!is.null(input$sel_disc) && input$sel_disc != "") df <- df %>% filter(Discipline == input$sel_disc)

    # Compute city-level impacts using 2021 Census CD employment shares
    cd_share_val <- get_city_share(cduid)
    df <- df %>%
      mutate(
        city_share          = cd_share_val,
        imp_gdp_city_total  = base_amt * gdp_wp_direct +
          base_amt * gdp_wp_indirect * cd_share_val +
          base_amt * gdp_wp_induced  * cd_share_val,
        imp_jobs_city_total = base_millions * jobs_wp_direct +
          base_millions * jobs_wp_indirect * cd_share_val +
          base_millions * jobs_wp_induced  * cd_share_val
      )
    df
  })
  
  output$city_kpi_panel <- renderUI({
    df    <- city_fdata()
    n         <- n_distinct(df$`Business Number`)
    gdp_wp    <- sum(df$imp_gdp_wp_total,    na.rm = TRUE)
    gdp_city  <- sum(df$imp_gdp_city_total,  na.rm = TRUE)
    jobs_wp   <- sum(df$imp_jobs_wp_total,   na.rm = TRUE)
    jobs_city <- sum(df$imp_jobs_city_total, na.rm = TRUE)
    city_gdp_pct <- if (gdp_wp > 0) round(gdp_city / gdp_wp * 100, 1) else 0
    
    kpi_row <- function(label_content, value, col = NAVY, label_col = "#888") {
      tags$div(style = "display:flex; justify-content:space-between; padding:4px 0; border-bottom:1px solid #f0ede8;",
               tags$span(style = sprintf("font-size:0.72rem; font-weight:700; color:%s;", label_col), label_content),
               tags$span(style = sprintf("font-size:1rem; font-weight:800; color:%s;", col), value))
    }
    
    div(style = sprintf("border-top:3px solid %s; padding:12px 0;", NAVY),
        div(style = "font-size:0.7rem; font-weight:800; text-transform:uppercase; letter-spacing:0.6px; color:#aaa; margin-bottom:8px;",
            paste0(input$city_sel, " Summary")),
        kpi_row("Organizations", comma(n)),
        kpi_row(tagList(term("GDP"), " (", term("Within Province", "Within-Province"), ")"), paste0("$", comma(round(gdp_wp)))),
        kpi_row(tagList(term("GDP"), " (", tags$span(class = "term-tip", `data-def` = paste0("Impact retained within ", input$city_sel, " — direct activity stays in the city; indirect/induced are scaled by the city's share of provincial employment."), paste0("Within ", input$city_sel)), ")"), paste0("$", comma(round(gdp_city))), PINK),
        kpi_row(tagList(term("Jobs"), " (", term("Within Province", "Within-Province"), ")"), comma(round(jobs_wp))),
        kpi_row(tagList(term("Jobs"), " (", tags$span(class = "term-tip", `data-def` = paste0("Jobs retained within ", input$city_sel, " — direct activity stays in the city; indirect/induced are scaled by the city's share of provincial employment."), paste0("Within ", input$city_sel)), ")"), comma(round(jobs_city)), PINK),
        kpi_row(term("City Retention"), paste0(city_gdp_pct, "%"), GREEN))
  })
  
  output$city_math_panel <- renderUI({
    df    <- city_fdata()
    gdp_wp   <- sum(df$imp_gdp_wp_total,    na.rm = TRUE)
    gdp_city <- sum(df$imp_gdp_city_total,  na.rm = TRUE)
    
    city_share_val <- if (nrow(df) > 0) unique(df$city_share)[1] else NA
    gdp_d_sum <- if (nrow(df) > 0) sum(df$base_amt * df$gdp_wp_direct, na.rm = TRUE) else 0
    gdp_i_sum <- if (nrow(df) > 0) sum(df$base_amt * df$gdp_wp_indirect, na.rm = TRUE) else 0
    gdp_id_sum <- if (nrow(df) > 0) sum(df$base_amt * df$gdp_wp_induced, na.rm = TRUE) else 0
    
    tags$div(
      tags$div(style = "font-size:0.72rem; font-weight:700; text-transform:uppercase; color:#aaa; margin-bottom:10px;", 
               "How City Impact is Calculated"),
      tags$div(style = "display:flex; gap:24px; flex-wrap:wrap;",
        # Step 1
        tags$div(style = "flex:1; min-width:200px;",
          tags$div(style = sprintf("font-weight:700; color:%s; font-size:0.75rem; margin-bottom:6px;", NAVY), 
                   "Step 1: Provincial Impact (Within-Province Multipliers)"),
          tags$div(style = "font-family:monospace; font-size:0.7rem; color:#555; line-height:1.6;",
            paste0("Direct:   $", comma(round(gdp_d_sum)), " (100% in city)"), tags$br(),
            paste0("Indirect: $", comma(round(gdp_i_sum))), tags$br(),
            paste0("Induced:  $", comma(round(gdp_id_sum))), tags$br(),
            tags$span(style = "font-weight:700;", paste0("Provincial Total: $", comma(round(gdp_wp)))))),
        # Step 2
        tags$div(style = "flex:1; min-width:180px;",
          tags$div(style = sprintf("font-weight:700; color:%s; font-size:0.75rem; margin-bottom:6px;", NAVY), 
                   "Step 2: City Employment Share"),
          tags$div(style = "font-size:0.7rem; color:#555; line-height:1.6;",
            paste0(input$city_sel, " has "),
            tags$b(style = sprintf("color:%s;", PINK), paste0(round(city_share_val * 100, 1), "%")),
            " of provincial employment (2021 Census).")),
        # Step 3
        tags$div(style = "flex:1.5; min-width:280px;",
          tags$div(style = sprintf("font-weight:700; color:%s; font-size:0.75rem; margin-bottom:6px;", NAVY), 
                   "Step 3: City-Level Impact"),
          tags$div(style = "font-family:monospace; font-size:0.7rem; color:#555; line-height:1.5; background:#f9f8f6; padding:8px 10px; border-radius:4px;",
            tags$span(style = sprintf("color:%s; font-weight:700;", PINK), "City GDP"), 
            " = Direct × 100% + (Indirect + Induced) × Share", tags$br(),
            paste0("       = $", comma(round(gdp_d_sum)), " + $", comma(round(gdp_i_sum + gdp_id_sum)), " × ", round(city_share_val, 2)), tags$br(),
            paste0("       = $", comma(round(gdp_d_sum)), " + $", comma(round((gdp_i_sum + gdp_id_sum) * city_share_val))), tags$br(),
            tags$b(paste0("       = $", comma(round(gdp_city))))))),
      tags$div(style = "margin-top:10px; font-size:0.65rem; color:#999; font-style:italic;",
        "Note: Direct impact stays in the city. Indirect and induced impacts are distributed based on city's share of provincial employment."))
  })
  
  # Aggregate all orgs by CD for the full-Canada map (respects global filters)
  all_cd_data <- reactive({
    df <- get_org_data()
    if (!is.null(input$sel_year) && input$sel_year != "") df <- df %>% filter(Year == as.numeric(input$sel_year))
    if (!is.null(input$sel_cat)  && input$sel_cat  != "") df <- df %>% filter(Category == input$sel_cat)
    if (!is.null(input$sel_disc) && input$sel_disc != "") df <- df %>% filter(Discipline == input$sel_disc)

    df %>%
      inner_join(business_cd_lookup[, c("Business Number", "Year", "CDUID")],
                 by = c("Business Number", "Year")) %>%
      group_by(CDUID) %>%
      summarise(
        n          = n_distinct(`Business Number`),
        gdp_total  = sum(imp_gdp_total,    na.rm = TRUE),
        jobs_total = sum(imp_jobs_total,   na.rm = TRUE),
        gdp_wp     = sum(imp_gdp_wp_total, na.rm = TRUE),
        jobs_wp    = sum(imp_jobs_wp_total, na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      mutate(gdp_pct_wp = ifelse(gdp_total > 0, round(gdp_wp / gdp_total * 100, 1), 0))
  })

  # Static map shell — tiles + initial view only; polygons added reactively below
  output$plot_city_map <- renderLeaflet({
    leaflet(options = leafletOptions(
      minZoom = 3, maxZoom = 12,
      maxBounds = list(c(38, -145), c(75, -50)),
      maxBoundsViscosity = 1.0, scrollWheelZoom = FALSE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels,   options = providerTileOptions(opacity = 0.6)) %>%
      addProviderTiles(providers$CartoDB.PositronOnlyLabels, options = providerTileOptions(opacity = 0.4)) %>%
      setView(lng = -96, lat = 60, zoom = 4)
  })

  # Redraw polygons + legend when data or metric toggle changes
  observe({
    agg    <- all_cd_data()
    metric <- if (isTRUE(input$cd_per_capita)) "gdp_per_capita" else "gdp_total"

    map_df <- cd_sf %>%
      left_join(agg, by = "CDUID") %>%
      mutate(across(c(n, gdp_total, jobs_total, gdp_wp, jobs_wp, gdp_pct_wp), ~replace_na(.x, 0)),
             gdp_per_capita = ifelse(!is.na(pop_cd) & pop_cd > 0, gdp_total / pop_cd, 0),
             map_val = .data[[metric]])

    val_max   <- max(map_df$map_val, 1, na.rm = TRUE)
    pal       <- colorNumeric(palette = c("#e8e5e0", "#6a5acd", NAVY),
                              domain = c(0, val_max), na.color = "#e8e5e0")

    is_pc     <- metric == "gdp_per_capita"
    fmt       <- if (is_pc) labelFormat(prefix = "$", suffix = "/capita", big.mark = ",")
                 else       labelFormat(prefix = "$", big.mark = ",")
    leg_title <- if (is_pc) "GDP per Capita" else "GDP Impact"

    labels <- sprintf(
      "<div style='font-family:Inter,sans-serif; font-size:13px;'>
        <b style='font-size:15px;'>%s</b><br>Population: %s<br>Organizations: %s<br><br>
        <b style='color:%s;'>GDP Impact</b><br>Total: $%s<br>Per Capita: $%s<br>Within Province: $%s (%s%%)<br>
        <b style='color:%s;'>Jobs</b><br>Total: %s</div>",
      map_df$CDNAME, comma(map_df$pop_cd), comma(map_df$n), NAVY,
      comma(round(map_df$gdp_total)), comma(round(map_df$gdp_per_capita, 2)),
      comma(round(map_df$gdp_wp)), map_df$gdp_pct_wp,
      PINK, comma(round(map_df$jobs_total))) %>% lapply(htmltools::HTML)

    leafletProxy("plot_city_map", data = map_df) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        layerId          = ~CDUID,
        fillColor        = ~pal(map_val), fillOpacity = 0.65,
        weight = 1, color = "#ffffff", opacity = 0.7,
        highlightOptions = highlightOptions(weight = 2.5, color = PINK, fillOpacity = 0.85, bringToFront = TRUE),
        label            = labels,
        labelOptions     = labelOptions(
          style    = list("padding" = "10px 14px", "border-radius" = "8px",
                          "background-color" = "white", "box-shadow" = "0 2px 8px rgba(0,0,0,0.15)", "border" = "none"),
          textsize = "13px", direction = "auto")) %>%
      addLegend(position = "bottomright", pal = pal, values = map_df$map_val,
                title = leg_title, labFormat = fmt, opacity = 0.8)
  })

  # Teleport to selected city without rebuilding the map
  observe({
    req(input$city_sel, input$city_sel != "")
    coords <- CITY_COORDS[[input$city_sel]]
    req(coords)
    leafletProxy("plot_city_map") %>%
      setView(lng = coords$lng, lat = coords$lat, zoom = coords$zoom)
  })
}