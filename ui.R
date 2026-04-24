ui <- page_fluid(
  theme = bs_theme(
    version = 5, bg = CREAM, fg = "#1a1a2e",
    base_font = font_google("Inter"), heading_font = font_google("Inter")
  ),
  
  tags$head(
    tags$style(HTML(paste0("
    #top-nav {
      position: sticky; top: 0; z-index: 1000;
      background: ", NAVY, "; display: flex; align-items: center;
      padding: 0 18px; gap: 4px; border-bottom: 1px solid rgba(255,255,255,0.12);
      min-height: 44px;
    }
    #top-nav .nav-btn {
      background: transparent; color: rgba(255,255,255,0.65);
      border: none; border-bottom: 3px solid transparent;
      padding: 10px 16px 7px; font-size: 0.72rem; font-weight: 700;
      text-transform: uppercase; letter-spacing: 0.5px;
      cursor: pointer; white-space: nowrap; transition: color .15s;
    }
    #top-nav .nav-btn:hover { color: ", WHITE, "; }
    #top-nav .nav-btn.active { color: ", PINK, "; border-bottom-color: ", PINK, "; }
    "))),
    tags$style(HTML(sprintf("
    body { background: %s; }
    #filter-bar-wrap {
      position: sticky; top: 44px; z-index: 999;
    }
    .filter-bar {
      background: %s; padding: 10px 18px;
      display: flex; align-items: center; gap: 10px;
      margin: 0 -1rem; flex-wrap: nowrap; border-bottom: none;
    }
    .org-count { flex-shrink:0; color:%s; min-width:70px;
      border-left:3px solid %s; padding-left:8px; }
    .org-count .num { font-size:1.4rem; font-weight:800; line-height:1; }
    .org-count .lab { font-size:0.62rem; font-weight:700; letter-spacing:0.6px;
      text-transform:uppercase; }
    .fgroup { flex:1; min-width:90px; }
    .fgroup .flabel { font-size:0.62rem; font-weight:700; text-transform:uppercase;
      letter-spacing:0.5px; color:rgba(255,255,255,0.7); margin-bottom:2px; }
    .fgroup select { background:%s !important; border:1px solid rgba(255,255,255,0.2) !important;
      border-radius:16px !important; padding:5px 12px !important;
      font-size:0.8rem; color:#333; box-shadow:none !important; height:auto !important; }
    .clear-btn { flex-shrink:0; background:transparent; color:%s;
      border:1.5px solid rgba(255,255,255,0.5); border-radius:6px; padding:5px 10px;
      font-size:0.65rem; font-weight:700; letter-spacing:0.3px;
      text-transform:uppercase; cursor:pointer; white-space:nowrap; }
    .clear-btn:hover { background:rgba(255,255,255,0.15); color:%s; }
    .sec-title { font-size:1.5rem; font-weight:800; margin:36px 0 6px; }
    .sec-title .highlight { color:%s; }
    .leaflet-top { z-index:400 !important; }
    .leaflet-bottom { z-index:400 !important; }
    .kpi-row { display:flex; align-items:baseline; gap:40px; flex-wrap:wrap; margin:16px 0 28px; }
    .kpi-item { display:flex; align-items:baseline; gap:12px; }
    .kpi-item .num { font-size:2.4rem; font-weight:800; color:%s; }
    .kpi-item .pipe { color:#d0cdc8; font-size:2rem; font-weight:200; }
    .kpi-item .lab { font-size:0.72rem; font-weight:700; text-transform:uppercase;
      letter-spacing:0.5px; color:#777; max-width:200px; line-height:1.3; }
    .chart-card { background:%s; border-radius:12px; padding:22px 24px; margin-bottom:22px; }
    .chart-card h4 { font-size:0.95rem; margin-bottom:14px; color:#1a1a2e; }
    .chart-card h4 strong { font-weight:800; }
    .dna-divider { border:none; border-top:1px solid #e8e5e0; margin:8px 0 0; }
    .mc-footer { text-align:center; padding:32px; color:#b5b0a8; font-size:0.75rem; margin-top:24px; }
    .fgroup .control-label { display:none !important; }
    .method-toggle-wrap { flex-shrink:0; display:flex; align-items:center; gap:5px; }
    .method-toggle-wrap .toggle-label { font-size:0.58rem; font-weight:700; text-transform:uppercase;
      letter-spacing:0.5px; color:rgba(255,255,255,0.7); }
    .toggle-pill { display:flex; border-radius:14px; overflow:hidden; border:1px solid rgba(255,255,255,0.4); }
    .toggle-pill button { padding:4px 10px !important; border:none !important; cursor:pointer !important;
      font-weight:700 !important; font-size:0.65rem !important; font-family:inherit !important;
      transition:background .2s, color .2s !important; white-space:nowrap; }
    .toggle-pill button.tog-active { background:#FF3EC9 !important; color:#ffffff !important; }
    .toggle-pill button.tog-inactive { background:rgba(255,255,255,0.12) !important; color:#ffffff !important; }
    .math-box { background:#f3f0ea; border-radius:8px; padding:12px 16px;
      font-size:0.78rem; color:#555; line-height:1.9; font-family:monospace; }
    .math-box .row-head { font-weight:700; color:%s; }
    .calc-row-equal > div[class^='col'] { display:flex; flex-direction:column; }
    .calc-row-equal > div[class^='col'] > div { flex:1; }
    .fc-btn-wrap { flex-shrink:0; }
    .port-input-bar { display:flex; align-items:flex-end; gap:12px; flex-wrap:wrap;
      padding:16px 20px; background:%s; border-radius:10px; margin-bottom:16px; }
    .port-input-bar .fc-field { flex:1; min-width:120px; }
    .port-input-bar .fc-field label { display:block; font-size:0.68rem; font-weight:700;
      text-transform:uppercase; letter-spacing:0.4px; color:#888; margin-bottom:4px; }
    .port-input-bar .fc-field select, .port-input-bar .fc-field input[type='number'] {
      width:100%%; padding:7px 10px; border:1px solid #ddd; border-radius:8px; font-size:0.82rem; background:white; }
    .port-input-bar .control-label { display:none !important; }
    .port-input-bar .form-group { margin-bottom:0 !important; }
    .port-table { width:100%%; border-collapse:collapse; font-size:0.78rem; }
    .port-table th { padding:8px 12px; text-align:left; font-size:0.65rem; font-weight:700;
      text-transform:uppercase; letter-spacing:0.4px; color:#999; border-bottom:2px solid #e8e5e0; }
    .port-table td { padding:8px 12px; border-bottom:1px solid #f0ede8; color:#555; }
    .port-table tr:hover { background:#f8f7f4; }
    .port-remove-btn { background:none; border:none; color:#ccc; cursor:pointer; font-size:1rem;
      padding:2px 6px; border-radius:4px; transition:all 0.15s; }
    .port-remove-btn:hover { color:#E24B4A; background:#fef2f2; }
    
    /* Section toggle styles (cream background) */
    .sec-toggle-wrap { display:flex; align-items:center; gap:5px; }
    .sec-toggle-wrap .toggle-label { font-size:0.65rem; font-weight:700; text-transform:uppercase;
      letter-spacing:0.4px; color:#888; }
    .sec-toggle-pill { display:flex; border-radius:12px; overflow:hidden; border:1px solid #ddd; }
    .sec-toggle-pill button { padding:4px 10px !important; border:none !important; cursor:pointer !important;
      font-weight:700 !important; font-size:0.65rem !important; font-family:inherit !important;
      transition:background .2s, color .2s !important; white-space:nowrap; background:#f5f3ef !important; color:#999 !important; }
    .sec-toggle-pill button.sec-active { background:%s !important; color:#fff !important; }
    
    /* ── TOOLTIP STYLES ─────────────────────────────────────────────────────── */
    .term-tip {
      position: relative;
      border-bottom: 1px dotted #888;
      cursor: help;
    }
    .term-tip::after {
      content: attr(data-def);
      position: absolute;
      top: 100%%;
      left: 0;
      margin-top: 6px;
      background: #ffffff;
      color: #333;
      padding: 10px 14px;
      border-radius: 8px;
      font-size: 0.75rem;
      font-weight: 400;
      line-height: 1.5;
      width: 240px;
      max-width: 90vw;
      text-align: left;
      box-shadow: 0 4px 20px rgba(0,0,0,0.18);
      border: 1px solid #e0e0e0;
      opacity: 0;
      visibility: hidden;
      transition: opacity 0.15s, visibility 0.15s;
      z-index: 10000;
      pointer-events: none;
      white-space: normal;
      text-transform: none;
      letter-spacing: normal;
      font-family: 'Inter', sans-serif;
    }
    .term-tip:hover::after {
      opacity: 1;
      visibility: visible;
    }
    .term-tip::before {
      content: '';
      position: absolute;
      top: 100%%;
      left: 12px;
      border: 6px solid transparent;
      border-bottom-color: #ffffff;
      margin-top: -6px;
      opacity: 0;
      visibility: hidden;
      transition: opacity 0.15s, visibility 0.15s;
      z-index: 10001;
      filter: drop-shadow(0 -1px 1px rgba(0,0,0,0.08));
    }
    .term-tip:hover::before {
      opacity: 1;
      visibility: visible;
    }
  ", CREAM, NAVY, WHITE, PINK, WHITE, WHITE, WHITE, PINK, NAVY, WHITE, NAVY, WHITE, NAVY)))),

  tags$style(HTML(".tab-btn-locked { opacity:0.45 !important; cursor:not-allowed !important; pointer-events:none !important; }")),

  tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      ['filter_info_btn', 'year_info_btn', 'port_gdp_mult_info_btn'].forEach(function(id) {
        var el = document.getElementById(id);
        if (el && bootstrap && bootstrap.Popover) {
          new bootstrap.Popover(el, { trigger: 'click' });
          document.addEventListener('click', function(e) {
            if (!el.contains(e.target)) {
              bootstrap.Popover.getInstance(el) && bootstrap.Popover.getInstance(el).hide();
            }
          });
        }
      });
      // sum_gdp_mult_info_btn lives inside renderUI so we use delegation
      document.addEventListener('click', function(e) {
        var tgt = e.target.closest('#sum_gdp_mult_info_btn');
        if (tgt && bootstrap && bootstrap.Popover) {
          var inst = bootstrap.Popover.getOrCreateInstance(tgt, { trigger: 'manual' });
          inst.toggle();
          e.stopPropagation();
        } else {
          var open = document.getElementById('sum_gdp_mult_info_btn');
          if (open) { var i = bootstrap.Popover.getInstance(open); if (i) i.hide(); }
        }
      });
    });
    Shiny.addCustomMessageHandler('resetPortName', function(msg) {
      var el = document.getElementById('port_name');
      if (el) { el.value = ''; Shiny.setInputValue('port_name', ''); }
    });
    function _setTabButtonsLocked(locked) {
      document.querySelectorAll('#model-content button').forEach(function(btn) {
        btn.disabled = locked;
        btn.classList.toggle('tab-btn-locked', locked);
      });
    }
    function switchTab(tab) {
      document.getElementById('main-content').style.display    = tab === 'main'  ? '' : 'none';
      document.getElementById('model-content').style.display   = tab === 'model' ? '' : 'none';
      document.getElementById('filter-bar-wrap').style.display = tab === 'main'  ? '' : 'none';
      ['main', 'model'].forEach(function(t) {
        var btn = document.getElementById('nav-btn-' + t);
        if (btn) btn.className = t === tab ? 'nav-btn active' : 'nav-btn';
      });
    }
    $(document).on('shiny:busy', function() { _setTabButtonsLocked(true); });
    $(document).on('shiny:idle', function() { _setTabButtonsLocked(false); });
  ")),

  # ── TOP NAV ───────────────────────────────────────
  div(id = "top-nav",
      tags$button(id = "nav-btn-main",  class = "nav-btn active",
                  onclick = "switchTab('main')",  "Economic Impact"),
      tags$button(id = "nav-btn-model", class = "nav-btn",
                  onclick = "switchTab('model')", "Model Your Impact"),
),

  # ── FILTER BAR ────────────────────────────────────
  div(id = "filter-bar-wrap",
    div(class = "filter-bar",
        div(class = "org-count",
            div(class = "num", textOutput("fb_orgs", inline = TRUE)),
            div(class = "lab", "Orgs ",
                tags$span("ⓘ", id = "filter_info_btn",
                          `data-bs-toggle` = "popover", `data-bs-placement` = "bottom",
                          `data-bs-content` = "Orgs with zero, negative, or missing financials are excluded from calculations.",
                          `data-bs-trigger` = "click",
                          style = "cursor:pointer; font-size:0.6rem; color:rgba(255,255,255,0.6); font-weight:400; text-transform:none; letter-spacing:0;"))),
        div(class = "fgroup", div(class = "flabel", "Year"),
            selectInput("sel_year", NULL,
                        choices = c("All" = "", YEARS_CAPPED),
                        selected = DEFAULT_YEAR,
                        selectize = FALSE)),
        div(class = "fgroup", div(class = "flabel", "Region"),
            selectInput("sel_prov", NULL, choices = c("Canada" = "", PROVINCES), selected = "", selectize = FALSE)),
        div(class = "fgroup", div(class = "flabel", "Org Type"),
            selectInput("sel_cat", NULL, choices = c("All" = "", CATEGORIES), selected = "", selectize = FALSE)),
        div(class = "fgroup", div(class = "flabel", "Discipline"),
            selectInput("sel_disc", NULL, choices = c("All" = "", DISCIPLINES), selected = "", selectize = FALSE)),
        tags$button(class = "clear-btn", id = "clear_btn", onclick = "Shiny.setInputValue('clear', Math.random())", "⟲ Clear Filters"))),
  
  div(id = "main-content",

  hr(class = "dna-divider"),

  # ── ECONOMIC IMPACT ─────────────────────────────────────────────────────────
  div(class = "sec-title", span(class = "highlight", "Economic"), " Impact"),
  tags$p(class = "sec-desc", style = "font-size:0.88rem; color:#555; margin:-8px 0 18px;",
    "This section shows the economic impact of ", tags$strong(style = "font-weight:700; color:#1a1a2e;", "non-profit"), " arts and culture organizations in the filtered selection. ",
    term("Total GDP", "Total GDP"), " is broken into ", term("Direct GDP", "Direct GDP"), ", ",
    term("Indirect GDP", "Indirect GDP"), ", and ", term("Induced GDP", "Induced GDP"),
    ". Together they capture the full value added across the economy. Jobs counts all positions supported across all impact layers. ",
    "Use the toggles to switch between ", term("Expenditures", "Expenditures"), " and ", term("Revenue", "Revenue"),
    " as the calculation base, or between a single industry ", term("Multiplier"), " (", term("Primary", "Primary"), ") and an equal-weight blend (", term("Mixture", "Mixture"), ")."),
  # Toggle row for summary stats
  div(style = "display:flex; gap:16px; margin-bottom:14px;",
      div(class = "sec-toggle-wrap",
          div(class = "toggle-label", term("Multiplier")),
          div(class = "sec-toggle-pill",
              tags$button(id = "sum_method_single_btn", class = "sec-active", type = "button",
                          onclick = "var cur=document.getElementById('sum_method_single_btn').className==='sec-active'; document.getElementById('sum_method_single_btn').className=cur?'':'sec-active'; document.getElementById('sum_method_mix_btn').className=cur?'sec-active':''; Shiny.setInputValue('sum_mult_method',cur?'mixture':'single');", "Primary"),
              tags$button(id = "sum_method_mix_btn", class = "", type = "button",
                          onclick = "var cur=document.getElementById('sum_method_mix_btn').className==='sec-active'; document.getElementById('sum_method_mix_btn').className=cur?'':'sec-active'; document.getElementById('sum_method_single_btn').className=cur?'sec-active':''; Shiny.setInputValue('sum_mult_method',cur?'single':'mixture');", "Mixture"))),
      div(class = "sec-toggle-wrap",
          div(class = "toggle-label", "Base"),
          div(class = "sec-toggle-pill",
              tags$button(id = "sum_base_exp_btn", class = "sec-active", type = "button",
                          onclick = "var cur=document.getElementById('sum_base_exp_btn').className==='sec-active'; document.getElementById('sum_base_exp_btn').className=cur?'':'sec-active'; document.getElementById('sum_base_rev_btn').className=cur?'sec-active':''; Shiny.setInputValue('sum_base_metric',cur?'rev':'exp');", "Exp"),
              tags$button(id = "sum_base_rev_btn", class = "", type = "button",
                          onclick = "var cur=document.getElementById('sum_base_rev_btn').className==='sec-active'; document.getElementById('sum_base_rev_btn').className=cur?'':'sec-active'; document.getElementById('sum_base_exp_btn').className=cur?'sec-active':''; Shiny.setInputValue('sum_base_metric',cur?'exp':'rev');", "Rev")))),
  fluidRow(style = "display:flex; align-items:stretch; margin-bottom:18px;",
           column(6, div(style = "height:100%;", uiOutput("kpi_panel_impact"))),
           column(6, div(style = "height:100%;", uiOutput("kpi_panel_finance")))),
  fluidRow(
    column(6, div(class = "chart-card", h4(strong(term("GDP"), " impact"), " over time (all years)"), plotlyOutput("plot_gdp_year", height = "320px"))),
    column(6, div(class = "chart-card", h4(strong("Jobs supported"), " over time (all years)"), plotlyOutput("plot_jobs_year", height = "320px")))),
  
  # ── PROVINCIAL IMPACT ───────────────────────────────────────────────────────
  div(class = "sec-title", span(class = "highlight", "Provincial"), " Impact & ", term("Leakage")),
  p(class = "sec-desc", style = "font-size:0.88rem; color:#555; margin:-8px 0 18px;",
    "The map breaks impact down by province, showing ", term("Total GDP", "Total GDP"), " which is the sum of ",
    term("Direct GDP", "Direct GDP"), ", ", term("Indirect GDP", "Indirect GDP"), ", and ", term("Induced GDP", "Induced GDP"),
    " within each province. The ", term("Within-Province", "Within-Province"), " share stays in the home province; the rest is ",
    term("Leakage", "Leakage"), " to other provinces via out-of-province supply chains. Indirect and induced layers drive most leakage."),
  fluidRow(column(12, div(class = "chart-card",
                          div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:14px;",
                              h4(style = "margin:0;", strong(term("GDP"), " & employment impact"), " by province — hover for details & ", term("leakage", "Leakage")),
                              div(style = "display:flex; align-items:center; gap:8px;",
                                  tags$label(style = "font-size:0.8rem; font-weight:600; color:#777; cursor:pointer;", `for` = "per_capita", term("Per capita", "Per Capita")),
                                  tags$input(type = "checkbox", id = "per_capita", style = "cursor:pointer; width:18px; height:18px;",
                                             onclick = "Shiny.setInputValue('per_capita', this.checked)"))),
                          leafletOutput("plot_map", height = "620px")))),
  
  # ── CITY EXPLORER ───────────────────────────────────────────────────────────
  div(class = "sec-title", span(class = "highlight", "City"), " Explorer"),
  p(class = "sec-desc", style = "font-size:0.88rem; color:#555; margin:-8px 0 18px;",
    "Each organization is assigned to the ", term("FSA"), " of its registered address, not necessarily where its activity takes place. ",
    "Within a selected city, ", term("Indirect GDP", "Indirect GDP"), " and ", term("Induced GDP", "Induced GDP"),
    " are distributed across postal codes in proportion to each ", term("FSA"), "'s share of the provincial labour force. ",
    term("Direct GDP", "Direct GDP"), " is assumed to stay within the city, as it reflects the organization's own operations."),
  fluidRow(style = "display:flex; align-items:stretch;",
    column(4, div(class = "chart-card", style = "height:100%; min-height:420px;",
                  h4(strong("Select"), " a city"),
                  selectInput("city_sel", NULL, choices = c("Choose a city" = "", levels(fsa_city_sf$city)), selected = ""),
                  conditionalPanel(condition = "input.city_sel !== ''",
                                   uiOutput("city_kpi_panel")))),
    column(8,
      conditionalPanel(condition = "input.city_sel === ''",
        div(class = "chart-card", style = "height:100%; min-height:420px; display:flex; flex-direction:column; align-items:center; justify-content:center; color:#ccc;",
            tags$div(style = "font-size:1rem; font-weight:700; color:#bbb; letter-spacing:0.3px;", "Select a city"),
            tags$div(style = "font-size:0.78rem; color:#ccc; margin-top:6px;", "The map will appear here"))),
      conditionalPanel(condition = "input.city_sel !== ''",
        div(class = "chart-card", style = "height:100%; min-height:420px;",
            h4(strong(term("FSA"), "-level impact"), " by postal code", style = "margin-bottom:10px;"),
            leafletOutput("plot_city_map", height = "360px"))))),
  conditionalPanel(condition = "input.city_sel !== ''",
                   fluidRow(
                     column(12, div(class = "chart-card", style = "margin-top:14px; padding:16px 20px;",
                                    uiOutput("city_math_panel"))))),
  
  div(class = "mc-footer", "Mass Culture — Economic Impact Explorer")

  ), # end main-content

  # ── MODEL YOUR IMPACT ───────────────────────────────────────────────────────
  div(id = "model-content", style = "display:none; padding:24px 8px 48px;",

    div(class = "sec-title", span(class = "highlight", "Model"), " Your Impact"),
    tags$p(class = "sec-desc", style = "font-size:0.88rem; color:#555; margin:-8px 0 20px;",
      "Estimate the economic footprint of one or more organizations using ", term("I-O Model", "I-O Model"), " ", term("multipliers", "Multiplier"), ". ",
      "The model multiplies each organization's ", term("Expenditures", "Expenditures"), " or ", term("Revenue", "Revenue"),
      " by industry-specific multipliers to estimate ", term("Direct GDP", "Direct GDP"), ", ",
      term("Indirect GDP", "Indirect GDP"), ", and ", term("Induced GDP", "Induced GDP"),
      ". Because the ", term("I-O Model", "I-O Model"),
      " is linear, multipliers can be blended across industries (", term("Mixture", "Mixture"), ") to reflect organizations that span multiple sectors."),

    tags$script(HTML("
      function setMultMethod(prefix, m) {
        var ids = {single: prefix+'_method_single_btn', mixture: prefix+'_method_mix_btn', custom: prefix+'_method_custom_btn'};
        Object.keys(ids).forEach(function(k){ document.getElementById(ids[k]).className = k===m ? 'sec-active' : ''; });
        Shiny.setInputValue(prefix+'_mult_method', m, {priority:'event'});
      }
      function resetMultMethod() { setMultMethod('port', 'single'); setMultMethod('bulk', 'single'); }
      var cwSnap = {};
      function cwSnapshot(prefix, n) {
        for (var j = 1; j <= n; j++) {
          var el = document.getElementById(prefix+'_cw_' + j);
          cwSnap[prefix+'_'+j] = el ? (parseFloat(el.value) || 0) : 0;
        }
      }
      function cwSync(prefix, changed, newVal, n) {
        newVal = Math.min(100, Math.max(0, Math.round(newVal)));
        var remaining = 100 - newVal;
        var otherSnapTotal = 0;
        for (var j = 1; j <= n; j++) { if (j !== changed) otherSnapTotal += (cwSnap[prefix+'_'+j] || 0); }
        for (var j = 1; j <= n; j++) {
          if (j === changed) continue;
          var snap = cwSnap[prefix+'_'+j] || 0;
          var v = otherSnapTotal > 0 ? Math.round((snap / otherSnapTotal) * remaining) : Math.round(remaining / (n - 1));
          v = Math.max(0, Math.min(100, v));
          document.getElementById(prefix+'_cw_' + j).value = v;
          document.getElementById(prefix+'_cwn_' + j).value = v;
          Shiny.setInputValue(prefix+'_cw_' + j, v);
        }
        document.getElementById(prefix+'_cw_' + changed).value = newVal;
        document.getElementById(prefix+'_cwn_' + changed).value = newVal;
        Shiny.setInputValue(prefix+'_cw_' + changed, newVal);
      }
      function cwSliderChanged(prefix, i, val, n) { cwSync(prefix, i, parseFloat(val), n); }
      function cwNumChanged(prefix, i, val, n) { cwSnapshot(prefix, n); cwSync(prefix, i, parseFloat(val), n); }
    ")),

    # ── SIDE-BY-SIDE INPUT CARDS (always visible, equal height) ──────────────
    div(style = "display:grid; grid-template-columns:1fr 1fr; gap:20px; align-items:stretch; margin-bottom:20px;",

        # ── LEFT: One at a Time ───────────────────────────────────────────────
        div(class = "chart-card", style = sprintf("border-top:3px solid %s; display:flex; flex-direction:column;", NAVY),
            div(style = sprintf("font-size:0.7rem; font-weight:800; text-transform:uppercase; letter-spacing:0.6px; color:%s; margin-bottom:10px;", NAVY), "One at a Time"),
            # toggles
            div(style = "display:flex; gap:10px; align-items:center; margin-bottom:10px; flex-wrap:wrap;",
                div(class = "sec-toggle-wrap",
                    div(class = "toggle-label", term("Multiplier")),
                    div(class = "sec-toggle-pill",
                        tags$button(id = "port_method_single_btn", class = "sec-active", type = "button",
                                    onclick = "setMultMethod('port','single');", "Primary"),
                        tags$button(id = "port_method_mix_btn", class = "", type = "button",
                                    onclick = "setMultMethod('port','mixture');", "Mixture"),
                        tags$button(id = "port_method_custom_btn", class = "", type = "button",
                                    onclick = "setMultMethod('port','custom');", "Custom"))),
                div(class = "sec-toggle-wrap",
                    div(class = "toggle-label", "Base"),
                    div(class = "sec-toggle-pill",
                        tags$button(id = "port_base_exp_btn", class = "sec-active", type = "button",
                                    onclick = "var cur=document.getElementById('port_base_exp_btn').className==='sec-active'; document.getElementById('port_base_exp_btn').className=cur?'':'sec-active'; document.getElementById('port_base_rev_btn').className=cur?'sec-active':''; Shiny.setInputValue('port_base_metric',cur?'rev':'exp');", "Exp"),
                        tags$button(id = "port_base_rev_btn", class = "", type = "button",
                                    onclick = "var cur=document.getElementById('port_base_rev_btn').className==='sec-active'; document.getElementById('port_base_rev_btn').className=cur?'':'sec-active'; document.getElementById('port_base_exp_btn').className=cur?'sec-active':''; Shiny.setInputValue('port_base_metric',cur?'exp':'rev');", "Rev")))),
            div(id = "port_weight_wrap", style = "margin:-4px 0 8px;",
                uiOutput("port_weight_ui"),
                uiOutput("port_custom_weights_ui")),
            div(style = "flex:1; display:flex; flex-direction:column; gap:10px;",
                div(tags$label(style = "font-size:0.68rem; font-weight:700; text-transform:uppercase; letter-spacing:0.4px; color:#888; display:block; margin-bottom:4px;", "Name (optional)"),
                    tags$input(id = "port_name", type = "text", value = "", placeholder = "e.g. My Theatre Co",
                               style = "width:100%; padding:7px 10px; border:1px solid #ddd; border-radius:8px; font-size:0.82rem; background:white; box-sizing:border-box;",
                               onchange = "Shiny.setInputValue('port_name', this.value)")),
                div(tags$label(style = "font-size:0.68rem; font-weight:700; text-transform:uppercase; letter-spacing:0.4px; color:#888; display:block; margin-bottom:4px;", "Organization Type"),
                    selectInput("port_cat", NULL, choices = CALC_CATEGORIES, selected = "Arts Organization", width = "100%")),
                div(tags$label(style = "font-size:0.68rem; font-weight:700; text-transform:uppercase; letter-spacing:0.4px; color:#888; display:block; margin-bottom:4px;", "Discipline"),
                    selectInput("port_disc", NULL,
                                choices = VALID_COMBOS_FULL %>% filter(Category == "Arts Organization") %>% pull(Discipline) %>% sort(),
                                selected = "Performing Arts", width = "100%")),
                div(tags$label(style = "font-size:0.68rem; font-weight:700; text-transform:uppercase; letter-spacing:0.4px; color:#888; display:block; margin-bottom:4px;", "Province"),
                    selectInput("port_prov", NULL, choices = PROVINCES, selected = "ON", width = "100%")),
                div(style = "display:grid; grid-template-columns:1fr 1fr; gap:10px;",
                    div(tags$label(style = "font-size:0.68rem; font-weight:700; text-transform:uppercase; letter-spacing:0.4px; color:#888; display:block; margin-bottom:4px;", "Year"),
                        selectInput("port_year", NULL, choices = YEARS_CAPPED, selected = as.character(MAX_MULT_YEAR), width = "100%")),
                    div(tags$label(style = "font-size:0.68rem; font-weight:700; text-transform:uppercase; letter-spacing:0.4px; color:#888; display:block; margin-bottom:4px;", "Org Age (yrs)"),
                        numericInput("port_org_age", NULL, value = 10, min = 0, max = 200, step = 1, width = "100%"))),
                div(tags$label(style = "font-size:0.68rem; font-weight:700; text-transform:uppercase; letter-spacing:0.4px; color:#888; display:block; margin-bottom:4px;",
                               textOutput("port_amount_lbl", inline = TRUE)),
                    numericInput("port_amount", NULL, value = 100000, min = 0, step = 1, width = "100%"))),
            div(style = "margin-top:16px;",
                actionButton("port_add", "+ Add to Portfolio",
                             style = sprintf("width:100%%; background:%s; color:%s; border:none; border-radius:8px; padding:11px 0; font-weight:700; font-size:0.85rem; letter-spacing:0.3px; cursor:pointer;", NAVY, WHITE),
                             onclick = "setTimeout(function(){ setMultMethod('port','single'); }, 300);"))),

        # ── RIGHT: Group Entry ────────────────────────────────────────────────
        div(class = "chart-card", style = sprintf("border-top:3px solid %s; display:flex; flex-direction:column;", PINK),
            div(style = sprintf("font-size:0.7rem; font-weight:800; text-transform:uppercase; letter-spacing:0.6px; color:%s; margin-bottom:10px;", PINK), "Group Entry"),
            # toggles
            div(style = "display:flex; gap:10px; align-items:center; margin-bottom:10px; flex-wrap:wrap;",
                div(class = "sec-toggle-wrap",
                    div(class = "toggle-label", term("Multiplier")),
                    div(class = "sec-toggle-pill",
                        tags$button(id = "bulk_method_single_btn", class = "sec-active", type = "button",
                                    onclick = "setMultMethod('bulk','single');", "Primary"),
                        tags$button(id = "bulk_method_mix_btn", class = "", type = "button",
                                    onclick = "setMultMethod('bulk','mixture');", "Mixture"),
                        tags$button(id = "bulk_method_custom_btn", class = "", type = "button",
                                    onclick = "setMultMethod('bulk','custom');", "Custom"))),
                div(class = "sec-toggle-wrap",
                    div(class = "toggle-label", "Base"),
                    div(class = "sec-toggle-pill",
                        tags$button(id = "bulk_base_exp_btn", class = "sec-active", type = "button",
                                    onclick = "var cur=document.getElementById('bulk_base_exp_btn').className==='sec-active'; document.getElementById('bulk_base_exp_btn').className=cur?'':'sec-active'; document.getElementById('bulk_base_rev_btn').className=cur?'sec-active':''; Shiny.setInputValue('bulk_base_metric',cur?'rev':'exp');", "Exp"),
                        tags$button(id = "bulk_base_rev_btn", class = "", type = "button",
                                    onclick = "var cur=document.getElementById('bulk_base_rev_btn').className==='sec-active'; document.getElementById('bulk_base_rev_btn').className=cur?'':'sec-active'; document.getElementById('bulk_base_exp_btn').className=cur?'sec-active':''; Shiny.setInputValue('bulk_base_metric',cur?'exp':'rev');", "Rev")))),
            div(id = "bulk_weight_wrap", style = "margin:-4px 0 8px;",
                uiOutput("bulk_weight_ui"),
                uiOutput("bulk_custom_weights_ui")),
            div(style = "flex:1; display:flex; flex-direction:column; gap:10px;",
                div(tags$label(style = "font-size:0.68rem; font-weight:700; text-transform:uppercase; letter-spacing:0.4px; color:#888; display:block; margin-bottom:4px;", "Organization Type"),
                    selectInput("bulk_cat", NULL, choices = CALC_CATEGORIES, selected = "Arts Organization", width = "100%")),
                div(tags$label(style = "font-size:0.68rem; font-weight:700; text-transform:uppercase; letter-spacing:0.4px; color:#888; display:block; margin-bottom:4px;", "Discipline"),
                    selectInput("bulk_disc", NULL,
                                choices = VALID_COMBOS_FULL %>% filter(Category == "Arts Organization") %>% pull(Discipline) %>% sort(),
                                selected = "Performing Arts", width = "100%")),
                div(tags$label(style = "font-size:0.68rem; font-weight:700; text-transform:uppercase; letter-spacing:0.4px; color:#888; display:block; margin-bottom:4px;", "Province"),
                    selectInput("bulk_prov", NULL, choices = PROVINCES, selected = "ON", width = "100%")),
                div(tags$label(style = "font-size:0.68rem; font-weight:700; text-transform:uppercase; letter-spacing:0.4px; color:#888; display:block; margin-bottom:4px;", "Year"),
                    selectInput("bulk_year", NULL, choices = YEARS_CAPPED, selected = as.character(MAX_MULT_YEAR), width = "100%")),
                div(tags$label(style = "font-size:0.68rem; font-weight:700; text-transform:uppercase; letter-spacing:0.4px; color:#888; display:block; margin-bottom:4px;", "Number of Orgs"),
                    numericInput("bulk_n", NULL, value = 10, min = 1, max = 10000, step = 1, width = "100%")),
                div(tags$label(style = "font-size:0.68rem; font-weight:700; text-transform:uppercase; letter-spacing:0.4px; color:#888; display:block; margin-bottom:4px;",
                               textOutput("bulk_amount_lbl", inline = TRUE)),
                    numericInput("bulk_amount", NULL, value = 100000, min = 0, step = 1, width = "100%"))),
            div(style = "margin-top:16px;",
                actionButton("bulk_add", "+ Add Group",
                             style = sprintf("width:100%%; background:%s; color:%s; border:none; border-radius:8px; padding:11px 0; font-weight:700; font-size:0.85rem; letter-spacing:0.3px; cursor:pointer;", PINK, WHITE),
                             onclick = "setTimeout(function(){ setMultMethod('bulk','single'); }, 300);"))))

    , # ── RESULTS ───────────────────────────────────────────────────────────
    conditionalPanel(condition = "output.port_has_orgs", div(style = "display:block; width:100%;",
                     div(style = "display:flex; width:100%; gap:18px; align-items:stretch; margin-bottom:18px;",
                         div(class = "chart-card", style = sprintf("border-top:3px solid %s; flex:1;", NAVY),
                             div(style = "font-size:0.68rem; font-weight:800; text-transform:uppercase; letter-spacing:0.5px; color:#aaa; margin-bottom:8px;", "Portfolio ", term("GDP"), " Impact",
                                 tags$span("ⓘ", id = "port_gdp_mult_info_btn",
                                           `data-bs-toggle` = "popover", `data-bs-placement` = "bottom",
                                           `data-bs-content` = "Multipliers are applied assuming the broader economy is unchanged. Removing this subsector would not materially shift the multipliers themselves. Results are indicative of scale, not counterfactual impact.",
                                           `data-bs-trigger` = "click",
                                           style = "cursor:pointer; font-size:0.6rem; color:rgba(0,0,0,0.35); font-weight:400; text-transform:none; letter-spacing:0; margin-left:3px;")),
                             div(style = sprintf("font-size:1.8rem; font-weight:800; color:%s; line-height:1;", NAVY), textOutput("port_gdp_total", inline = TRUE)),
                             div(style = "font-size:0.72rem; color:#aaa; margin-bottom:10px;", "total contribution"),
                             uiOutput("port_gdp_breakdown"),
                             hr(class = "dna-divider"),
                             uiOutput("port_gdp_leakage")),
                         div(class = "chart-card", style = sprintf("border-top:3px solid %s; flex:1;", PINK),
                             div(style = "font-size:0.68rem; font-weight:800; text-transform:uppercase; letter-spacing:0.5px; color:#aaa; margin-bottom:8px;", "Portfolio Jobs Supported"),
                             div(style = sprintf("font-size:1.8rem; font-weight:800; color:%s; line-height:1;", NAVY), textOutput("port_jobs_total", inline = TRUE)),
                             div(style = "font-size:0.72rem; color:#aaa; margin-bottom:10px;", "total jobs"),
                             uiOutput("port_jobs_breakdown"),
                             hr(class = "dna-divider"),
                             uiOutput("port_jobs_leakage")),
                         div(class = "chart-card", style = sprintf("border-top:3px solid %s; flex:1;", GREEN),
                             div(style = "font-size:0.68rem; font-weight:800; text-transform:uppercase; letter-spacing:0.5px; color:#aaa; margin-bottom:8px;", "Portfolio Summary"),
                             uiOutput("port_summary_stats"))),
                     div(class = "chart-card", style = "margin-bottom:18px;",
                         div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:12px;",
                             h4(style = "margin:0;", strong("Organizations"), " in portfolio"),
                             actionButton("port_clear", "Clear All",
                                          style = "background:transparent; color:#bbb; border:1px solid #ddd; border-radius:6px; padding:5px 14px; font-size:0.72rem; font-weight:700; cursor:pointer;")),
                         uiOutput("port_table"))))

  ) # end model-content
) # end page_fluid
