library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

base_metrics <- c("imputation_rmse", "imputation_mae", "distribution_ks", "distribution_wasserstein")
classification_metrics <- c("auc", "f1")
regression_metrics <- c("rmse_model", "r2")

metric_direction <- function(metric_name) {
  lower_better <- c("imputation_rmse", "imputation_mae", "distribution_ks", "distribution_wasserstein", "rmse_model")
  if (metric_name %in% lower_better) "lower_better" else "higher_better"
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

ui <- page_sidebar(
  title = "Data-Centric AI Robustness Dashboard",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    selectInput("dataset", "Dataset", choices = c("All"), selected = "All"),
    selectInput("metric", "Metric", choices = c("auc", "f1", "rmse_model", "r2"), selected = "auc")
  ),
  navset_card_tab(
    nav_panel(
      "Dataset Drilldown",
      card(
        card_header("Metric vs Missing Rate"),
        plotOutput("metric_plot", height = "360px")
      ),
      card(
        card_header("Summary (Mean +- SD by Imputer)"),
        tableOutput("summary_table")
      )
    ),
    nav_panel(
      "Overall Rankings",
      card(
        card_header("Imputer Ranking (Across Selected Scope)"),
        tableOutput("ranking_table")
      )
    ),
    nav_panel(
      "Significance Matrix",
      card(
        card_header("Pairwise Adjusted p-values (Holm)"),
        tableOutput("sig_matrix")
      ),
      card(
        card_header("Global and Pairwise Tests"),
        tableOutput("stats_table")
      )
    ),
    nav_panel(
      "Error Analysis",
      card(
        card_header("Pipeline Error Summary"),
        tableOutput("error_table")
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

  results <- reactive({
    if (!file.exists(results_file)) {
      return(data.frame())
    }
    read.csv(results_file, stringsAsFactors = FALSE)
  })

  stats_report <- reactive({
    if (!file.exists(stats_file)) {
      return(data.frame())
    }
    read.csv(stats_file, stringsAsFactors = FALSE)
  })

  observe({
    df <- results()
    if (nrow(df) == 0) {
      updateSelectInput(session, "dataset", choices = c("All"), selected = "All")
    } else {
      ds <- sort(unique(df$dataset))
      updateSelectInput(session, "dataset", choices = c("All", ds), selected = "All")
    }
  })

  observe({
    df <- results()
    if (nrow(df) == 0) {
      return()
    }

    df_ok <- df %>% filter(status == "ok")
    if (input$dataset != "All") {
      df_ok <- df_ok %>% filter(dataset == input$dataset)
    }

    metric_choices <- available_metrics(df_ok)
    if (length(metric_choices) == 0) {
      metric_choices <- c("auc")
    }

    selected_metric <- if (input$metric %in% metric_choices) input$metric else metric_choices[1]
    updateSelectInput(session, "metric", choices = metric_choices, selected = selected_metric)
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
    df
  })

  output$metric_plot <- renderPlot({
    df <- filtered_ok()
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No results found. Run src/pipeline/run_experiments.R first.")
      return(invisible(NULL))
    }

    metric_name <- input$metric
    plot_df <- df %>%
      filter(is.finite(.data[[metric_name]])) %>%
      group_by(mechanism, missing_rate, imputer) %>%
      summarise(value = mean(.data[[metric_name]], na.rm = TRUE), .groups = "drop")

    if (nrow(plot_df) == 0) {
      plot.new()
      text(0.5, 0.5, "No valid data for this metric/dataset combination.")
      return(invisible(NULL))
    }

    ggplot(plot_df, aes(x = missing_rate, y = value, color = imputer)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      facet_wrap(~mechanism) +
      labs(x = "Missing Rate", y = metric_name, color = "Imputer") +
      theme_minimal(base_size = 13)
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

    df <- filtered_ok()
    if (nrow(df) == 0) {
      return(data.frame(message = "No results available"))
    }

    stat_df <- df %>%
      filter(is.finite(.data[[metric_name]])) %>%
      mutate(metric_value = .data[[metric_name]])

    if (nrow(stat_df) < 3 || length(unique(stat_df$imputer)) < 2) {
      return(data.frame(message = "Not enough valid data for statistical test"))
    }

    global_p <- tryCatch(
      stats::kruskal.test(metric_value ~ imputer, data = stat_df)$p.value,
      error = function(e) NA_real_
    )

    pairwise_matrix <- tryCatch(
      stats::pairwise.wilcox.test(
        stat_df$metric_value,
        stat_df$imputer,
        p.adjust.method = "holm",
        exact = FALSE
      )$p.value,
      error = function(e) NULL
    )

    pairwise_table <- if (is.null(pairwise_matrix)) {
      data.frame(comparison = NA_character_, p_value = NA_character_, stringsAsFactors = FALSE)
    } else {
      as.data.frame(as.table(pairwise_matrix), stringsAsFactors = FALSE) %>%
        filter(!is.na(Freq)) %>%
        transmute(comparison = paste(Var1, "vs", Var2), p_value = scientific_p(Freq))
    }

    rbind(
      data.frame(comparison = "Kruskal-Wallis (global)", p_value = scientific_p(global_p), stringsAsFactors = FALSE),
      pairwise_table
    )
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
