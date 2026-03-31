library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

base_metrics <- c("imputation_rmse", "imputation_mae", "distribution_ks", "distribution_wasserstein")
classification_metrics <- c("auc", "f1")
regression_metrics <- c("rmse_model", "r2")

imputer_colors <- c(
  mean_mode = "#000000",
  mice = "#0072B2",
  missForest = "#D55E00",
  torch_vae = "#009E73"
)

imputer_shapes <- c(
  mean_mode = 15,
  mice = 16,
  missForest = 17,
  torch_vae = 18
)

metric_direction <- function(metric_name) {
  lower_better <- c("imputation_rmse", "imputation_mae", "distribution_ks", "distribution_wasserstein", "rmse_model")
  if (metric_name %in% lower_better) "lower_better" else "higher_better"
}

pretty_metric <- function(metric_name) {
  labels <- c(
    imputation_rmse = "Imputation RMSE",
    imputation_mae = "Imputation MAE",
    distribution_ks = "KS Distance",
    distribution_wasserstein = "Wasserstein Distance",
    auc = "AUC",
    f1 = "F1 Score",
    rmse_model = "Model RMSE",
    r2 = "R-squared"
  )
  if (metric_name %in% names(labels)) labels[[metric_name]] else metric_name
}

results_file <- if (file.exists("experiments/results/benchmark_results.csv")) {
  "experiments/results/benchmark_results.csv"
} else {
  "../../experiments/results/benchmark_results.csv"
}

stats_file <- if (file.exists("experiments/results/statistical_tests.csv")) {
  "experiments/results/statistical_tests.csv"
} else {
  "../../experiments/results/statistical_tests.csv"
}

theme_dashboard <- bs_theme(
  version = 5,
  bg = "#f2f2f2",
  fg = "#121212",
  primary = "#0ea5e9",
  secondary = "#0b0b0b",
  success = "#16a34a",
  warning = "#d97706",
  danger = "#dc2626",
  base_font = font_collection("Segoe UI", "Arial"),
  heading_font = font_collection("Arial Black", "Segoe UI", "Arial"),
  code_font = font_collection("Consolas", "Courier New")
)

ui <- page_sidebar(
  title = div(
    class = "title-wrap",
    span(class = "title-main", "ImputeLens"),
    span(class = "title-sub", "Data-Centric Missing-Data Robustness Benchmark in R")
  ),
  theme = theme_dashboard,
  fillable = FALSE,
  sidebar = sidebar(
    width = 320,
    open = "open",
    tags$div(class = "panel-block",
      tags$div(class = "section-label", "Explore"),
      selectInput("dataset", "Dataset", choices = c("All"), selected = "All"),
      selectInput("metric", "Metric", choices = c("auc", "f1"), selected = "auc"),
      uiOutput("imputer_picker"),
      actionButton("refresh", "Reload Results", class = "btn btn-dark w-100"),
      tags$div(class = "hint", "Minimal mode: static plots, no plot toolbars, cleaner presentation.")
    )
  ),
  tags$head(
    tags$style(HTML(" 
      :root {
        --ink: #0f0f0f;
        --paper: #f2f2f2;
        --panel: #ffffff;
        --line: #161616;
        --accent: #0ea5e9;
      }
      body {
        background: var(--paper);
      }
      body::before {
        content: '';
        position: fixed;
        inset: 0;
        pointer-events: none;
        opacity: 0.16;
        background:
          repeating-linear-gradient(
            -45deg,
            rgba(14, 165, 233, 0.08) 0,
            rgba(14, 165, 233, 0.08) 12px,
            transparent 12px,
            transparent 42px
          );
        animation: drift 16s linear infinite;
      }
      @keyframes drift {
        0% { transform: translateX(0); }
        100% { transform: translateX(120px); }
      }

      .title-wrap { display:flex; flex-direction:column; gap:2px; }
      .title-main { font-size:1.3rem; font-weight:900; letter-spacing:0.2px; color:var(--ink); }
      .title-sub { font-size:0.78rem; color:#3a3a3a; }

      .bslib-sidebar-layout > .sidebar {
        background: #ffffff;
        border-right: 3px solid var(--line);
      }

      .panel-block { padding: 0.35rem 0.1rem; }
      .section-label {
        text-transform: uppercase;
        font-size: 0.76rem;
        letter-spacing: 1.3px;
        color: #1f1f1f;
        margin-bottom: 0.9rem;
        font-weight: 800;
      }
      .hint {
        margin-top: 0.9rem;
        font-size: 0.82rem;
        color: #3a3a3a;
      }

      .card {
        background: var(--panel);
        border: 3px solid var(--line);
        border-radius: 0;
        box-shadow: none;
      }
      .bslib-page-main,
      .bslib-sidebar-layout,
      .main,
      .html-fill-container,
      .html-fill-item {
        overflow: visible !important;
      }
      .main-content-stack {
        margin-top: 12px;
      }
      .snapshot-card {
        height: auto !important;
      }
      .snapshot-card .card-body {
        overflow: visible !important;
      }
      .card-header {
        background: #fff !important;
        border-bottom: 3px solid var(--line);
        color: var(--ink);
        font-weight: 800;
        text-transform: uppercase;
        letter-spacing: 0.7px;
      }

      .metric-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(170px, 1fr));
        gap: 10px;
      }
      .metric-tile {
        background: #fff;
        border: 3px solid var(--line);
        border-radius: 0;
        padding: 14px;
        min-height: 106px;
      }
      .metric-tile .k {
        color: #404040;
        font-size: 0.78rem;
        text-transform: uppercase;
        letter-spacing: 1px;
        font-weight: 700;
      }
      .metric-tile .v {
        margin-top: 4px;
        font-size: 1.45rem;
        color: var(--ink);
        font-weight: 900;
      }
      .metric-tile .s {
        margin-top: 3px;
        font-size: 0.8rem;
        color: #2f2f2f;
      }

      .table {
        color: #111;
        --bs-table-bg: transparent;
      }
      .table > :not(caption) > * > * {
        border-color: #1b1b1b;
      }

      @media (max-width: 1200px) {
        .metric-grid { grid-template-columns: repeat(2, minmax(170px, 1fr)); }
      }
      @media (max-width: 700px) {
        .metric-grid { grid-template-columns: 1fr; }
      }
    "))
  ),
  layout_columns(
    fill = FALSE,
    col_widths = c(12),
    card(class = "snapshot-card", full_screen = FALSE, fill = FALSE, card_header("Snapshot"), uiOutput("kpi_row"))
  ),
  div(
    class = "main-content-stack",
    navset_card_tab(
      nav_panel(
        "Dataset Drilldown",
        layout_columns(
          col_widths = c(8, 4),
          card(
            full_screen = TRUE,
            card_header("Metric vs Missing Rate"),
            plotOutput("trend_plot", height = "540px")
          ),
          card(
            card_header("Mechanism Contrast"),
            plotOutput("mechanism_plot", height = "260px")
          ),
          card(
            card_header("Summary (Mean +- SD by Imputer)"),
            tableOutput("summary_table")
          )
        )
      ),
      nav_panel(
        "Overall Rankings",
        layout_columns(
          col_widths = c(5, 7),
          card(card_header("Rank Table"), tableOutput("ranking_table")),
          card(card_header("Average Metric by Imputer"), plotOutput("rank_plot", height = "360px"))
        )
      ),
      nav_panel(
        "Significance Matrix",
        card(card_header("Pairwise Adjusted p-values (Holm)"), tableOutput("sig_matrix")),
        card(card_header("Global and Pairwise Tests"), tableOutput("stats_table"))
      ),
      nav_panel(
        "Error Analysis",
        card(card_header("Pipeline Error Summary"), tableOutput("error_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  scientific_p <- function(x) {
    ifelse(is.na(x), NA_character_, format(x, scientific = TRUE, digits = 3))
  }

  available_metrics <- function(df_subset) {
    if (nrow(df_subset) == 0) {
      return(character(0))
    }
    candidate <- c(base_metrics, classification_metrics, regression_metrics)
    candidate <- candidate[candidate %in% names(df_subset)]
    candidate[vapply(candidate, function(m) any(is.finite(df_subset[[m]])), logical(1))]
  }

  refresh_trigger <- reactiveVal(0L)
  observeEvent(input$refresh, {
    refresh_trigger(refresh_trigger() + 1L)
  }, ignoreInit = TRUE)

  results <- reactive({
    refresh_trigger()
    if (!file.exists(results_file)) {
      return(data.frame())
    }
    read.csv(results_file, stringsAsFactors = FALSE)
  })

  stats_report <- reactive({
    refresh_trigger()
    if (!file.exists(stats_file)) {
      return(data.frame())
    }
    read.csv(stats_file, stringsAsFactors = FALSE)
  })

  observe({
    df <- results()
    if (nrow(df) == 0) {
      updateSelectInput(session, "dataset", choices = c("All"), selected = "All")
      return()
    }

    ds <- sort(unique(df$dataset))
    selected <- if (input$dataset %in% c("All", ds)) input$dataset else "All"
    updateSelectInput(session, "dataset", choices = c("All", ds), selected = selected)
  })

  output$imputer_picker <- renderUI({
    df <- results()
    choices <- if (nrow(df) == 0) character(0) else sort(unique(df$imputer))
    selectInput("imputers", "Imputers", choices = choices, selected = choices, multiple = TRUE)
  })

  filtered_ok <- reactive({
    df <- results()
    if (nrow(df) == 0) {
      return(df)
    }

    df <- df %>% filter(status == "ok")

    if (input$dataset != "All") {
      df <- df %>% filter(dataset == input$dataset)
    }

    if (!is.null(input$imputers) && length(input$imputers) > 0) {
      df <- df %>% filter(imputer %in% input$imputers)
    }

    df
  })

  observe({
    df_ok <- filtered_ok()
    metric_choices <- available_metrics(df_ok)
    if (length(metric_choices) == 0) {
      metric_choices <- c("auc")
    }
    selected_metric <- if (input$metric %in% metric_choices) input$metric else metric_choices[1]
    updateSelectInput(session, "metric", choices = metric_choices, selected = selected_metric)
  })

  output$kpi_row <- renderUI({
    df <- filtered_ok()
    metric_name <- input$metric

    if (nrow(df) == 0 || !metric_name %in% names(df)) {
      return(div(class = "metric-grid", div(class = "metric-tile", div(class = "k", "No data"), div(class = "v", "-"), div(class = "s", "Run the pipeline first."))))
    }

    m <- df %>% filter(is.finite(.data[[metric_name]]))
    if (nrow(m) == 0) {
      return(div(class = "metric-grid", div(class = "metric-tile", div(class = "k", "No valid metric rows"), div(class = "v", "-"), div(class = "s", pretty_metric(metric_name)))))
    }

    avg_metric <- mean(m[[metric_name]], na.rm = TRUE)
    run_count <- nrow(m)
    imp_count <- dplyr::n_distinct(m$imputer)

    best_tbl <- m %>%
      group_by(imputer) %>%
      summarise(v = mean(.data[[metric_name]], na.rm = TRUE), .groups = "drop")

    if (metric_direction(metric_name) == "higher_better") {
      best_tbl <- best_tbl %>% arrange(desc(v))
    } else {
      best_tbl <- best_tbl %>% arrange(v)
    }

    best_name <- best_tbl$imputer[1]

    stats_df <- stats_report()
    sig_count <- NA_integer_
    if (nrow(stats_df) > 0) {
      st <- stats_df %>% filter(test_type == "friedman_global", metric == metric_name)
      if (input$dataset != "All") {
        st <- st %>% filter(dataset == input$dataset)
      }
      sig_count <- sum(st$p_value < 0.05, na.rm = TRUE)
    }

    div(
      class = "metric-grid",
      div(class = "metric-tile", div(class = "k", "Metric"), div(class = "v", pretty_metric(metric_name)), div(class = "s", paste("Direction:", metric_direction(metric_name)))) ,
      div(class = "metric-tile", div(class = "k", "Average Value"), div(class = "v", round(avg_metric, 4)), div(class = "s", paste(run_count, "rows used"))),
      div(class = "metric-tile", div(class = "k", "Best Imputer"), div(class = "v", best_name), div(class = "s", paste("Across", imp_count, "imputers"))),
      div(class = "metric-tile", div(class = "k", "Significant Global Tests"), div(class = "v", ifelse(is.na(sig_count), "N/A", as.character(sig_count))), div(class = "s", "From Friedman report"))
    )
  })

  output$trend_plot <- renderPlot({
    df <- filtered_ok()
    validate(need(nrow(df) > 0, "No results found. Run src/pipeline/run_experiments.R first."))

    metric_name <- input$metric
    plot_df <- df %>%
      filter(is.finite(.data[[metric_name]])) %>%
      group_by(mechanism, missing_rate, imputer) %>%
      summarise(value = mean(.data[[metric_name]], na.rm = TRUE), .groups = "drop")

    validate(need(nrow(plot_df) > 0, "No valid rows for this metric and filter combination."))

    ggplot(plot_df, aes(x = missing_rate, y = value, color = imputer, group = imputer)) +
      geom_line(linewidth = 1.1) +
      geom_point(aes(shape = imputer), size = 2.8, stroke = 0.5) +
      facet_wrap(~mechanism, nrow = 1) +
      scale_color_manual(values = imputer_colors) +
      scale_shape_manual(values = imputer_shapes) +
      labs(x = "Missing Rate", y = pretty_metric(metric_name), color = "Imputer") +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#d9d9d9", linewidth = 0.35),
        panel.background = element_rect(fill = "#ffffff", color = NA),
        plot.background = element_rect(fill = "#ffffff", color = NA),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(color = "#202020"),
        strip.text = element_text(color = "#111111", face = "bold"),
        legend.position = "bottom"
      )
  })

  output$mechanism_plot <- renderPlot({
    df <- filtered_ok()
    metric_name <- input$metric

    mech_df <- df %>%
      filter(is.finite(.data[[metric_name]])) %>%
      group_by(imputer, mechanism) %>%
      summarise(value = mean(.data[[metric_name]], na.rm = TRUE), .groups = "drop")

    validate(need(nrow(mech_df) > 0, "No mechanism summary available."))

    ggplot(mech_df, aes(x = imputer, y = value, fill = mechanism)) +
      geom_col(position = position_dodge(width = 0.66), width = 0.58) +
      scale_fill_manual(values = c(MCAR = "#1f2937", MAR = "#9ca3af")) +
      labs(x = NULL, y = pretty_metric(metric_name), fill = "Mechanism") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "#ffffff", color = NA),
        plot.background = element_rect(fill = "#ffffff", color = NA),
        axis.text = element_text(color = "#202020"),
        legend.position = "bottom"
      )
  })

  output$summary_table <- renderTable({
    df <- filtered_ok()
    if (nrow(df) == 0) {
      return(data.frame(message = "No results available"))
    }

    metric_name <- input$metric
    out <- df %>%
      filter(is.finite(.data[[metric_name]])) %>%
      group_by(imputer) %>%
      summarise(
        n = dplyr::n(),
        mean = round(mean(.data[[metric_name]], na.rm = TRUE), 4),
        sd = round(stats::sd(.data[[metric_name]], na.rm = TRUE), 4),
        .groups = "drop"
      )

    if (nrow(out) == 0) {
      return(data.frame(message = "No valid values for selected metric"))
    }

    out
  })

  output$ranking_table <- renderTable({
    df <- filtered_ok()
    if (nrow(df) == 0) {
      return(data.frame(message = "No results available"))
    }

    metric_name <- input$metric
    rank_df <- df %>%
      filter(is.finite(.data[[metric_name]])) %>%
      group_by(imputer) %>%
      summarise(mean_metric = mean(.data[[metric_name]], na.rm = TRUE), n = dplyr::n(), .groups = "drop")

    if (nrow(rank_df) == 0) {
      return(data.frame(message = "No valid ranking data"))
    }

    desc_order <- metric_direction(metric_name) == "higher_better"
    rank_df <- rank_df %>%
      arrange(if (desc_order) dplyr::desc(mean_metric) else mean_metric) %>%
      mutate(rank = row_number()) %>%
      select(rank, imputer, mean_metric, n)

    rank_df$mean_metric <- round(rank_df$mean_metric, 4)
    rank_df
  })

  output$rank_plot <- renderPlot({
    df <- filtered_ok()
    metric_name <- input$metric

    rank_df <- df %>%
      filter(is.finite(.data[[metric_name]])) %>%
      group_by(imputer) %>%
      summarise(mean_metric = mean(.data[[metric_name]], na.rm = TRUE), .groups = "drop")

    validate(need(nrow(rank_df) > 0, "No ranking chart data"))

    desc_order <- metric_direction(metric_name) == "higher_better"
    rank_df <- rank_df %>% arrange(if (desc_order) dplyr::desc(mean_metric) else mean_metric)

    ggplot(rank_df, aes(x = reorder(imputer, mean_metric), y = mean_metric, fill = imputer)) +
      geom_col(width = 0.62, show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = imputer_colors) +
      labs(x = NULL, y = pretty_metric(metric_name)) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#ffffff", color = NA),
        plot.background = element_rect(fill = "#ffffff", color = NA),
        axis.text = element_text(color = "#202020")
      )
  })

  output$stats_table <- renderTable({
    rep_df <- stats_report()
    metric_name <- input$metric

    if (nrow(rep_df) > 0) {
      if (input$dataset != "All") {
        rep_df <- rep_df %>% filter(dataset == input$dataset)
      }
      rep_df <- rep_df %>% filter(metric == metric_name)

      if (nrow(rep_df) > 0) {
        out <- rep_df %>%
          select(dataset, metric, test_type, comparison, p_value, adjusted_p, n_blocks, n_imputers) %>%
          mutate(
            p_value = scientific_p(p_value),
            adjusted_p = scientific_p(adjusted_p)
          )
        return(out)
      }
    }

    data.frame(message = "No statistical report available")
  })

  output$sig_matrix <- renderTable({
    df <- filtered_ok()
    metric_name <- input$metric

    stat_df <- df %>%
      filter(is.finite(.data[[metric_name]])) %>%
      mutate(metric_value = .data[[metric_name]])

    if (nrow(stat_df) < 3 || length(unique(stat_df$imputer)) < 2) {
      return(data.frame(message = "Not enough valid data"))
    }

    pw <- tryCatch(
      stats::pairwise.wilcox.test(stat_df$metric_value, stat_df$imputer, p.adjust.method = "holm", exact = FALSE)$p.value,
      error = function(e) NULL
    )

    if (is.null(pw)) {
      return(data.frame(message = "Could not compute significance matrix"))
    }

    pwt <- as.data.frame(as.table(pw), stringsAsFactors = FALSE) %>%
      filter(!is.na(Freq)) %>%
      mutate(p_value = scientific_p(Freq)) %>%
      select(imputer_1 = Var1, imputer_2 = Var2, p_value)

    if (nrow(pwt) == 0) {
      return(data.frame(message = "No pairwise values"))
    }

    pwt
  })

  output$error_table <- renderTable({
    df <- results()
    if (nrow(df) == 0) {
      return(data.frame(message = "No results file found"))
    }

    err <- df %>% filter(status != "ok")
    if (nrow(err) == 0) {
      return(data.frame(message = "No pipeline errors recorded"))
    }

    err %>%
      group_by(dataset, mechanism, imputer, error_message) %>%
      summarise(count = dplyr::n(), .groups = "drop") %>%
      arrange(desc(count))
  })
}

shinyApp(ui, server)
