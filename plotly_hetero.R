library(plotly)

plotly_hetero <- function(selected, thr_p = 0.0001, groups = NULL) {

  colors <- list("EUR" = "#FFCCCC",
                 "AFR" = "#377EB8",
                 "AMR" = "#4DAF4A",
                 "ESA" = "#984EA3")

  if (!is.null(groups)) {
    selected <- selected[selected$dataset %in% groups, ]
  }

  title = ifelse(toupper(unique(selected$variable)) == "OR", "Odds Ratio", "Beta Coef")

  selected$hetero <- signif(selected$p_heter_adj, 2)
  selected$pval <- signif(selected$pval, 2)
  selected$maf <- signif(selected$maf, 2)
  selected$value <- signif(selected$value, 3)
  selected$group_info <- selected$group
  selected$y <- selected$value
  print(head(selected))
  if (toupper(unique(selected$variable)) == "OR") {
    selected$y <- selected$value - 1
  }

  if (grepl("^\\d.+", unique(selected$from))) {
    selected <- selected[order(selected$dataset, selected$to), ]
  } else {
    selected$chr <- as.numeric(gsub("(\\d+):.+", "\\1", selected$to, perl = TRUE))
    selected$pos <- as.numeric(gsub("\\d+:(\\d+):.+", "\\1", selected$to, perl = TRUE))
    selected <- selected[order(selected$chr, selected$pos), ]
  }

  p <- plotly::plot_ly(type="bar",
          data = selected, x = ~desc, y = ~y, color = ~ance,
          hovertext = ~paste(desc, " (", to, ")",
                        "<br>P-value: ", pval, '<br>', toupper(variable), ': ', value,
                        '<br>Heterogeneity adjusted P-value (vs EUR): ', hetero, '<br>MAF:', maf),
          hoverinfo = "text",
          colors = unlist(colors[sort(unique(selected$ance))])) %>%
    plotly::layout(yaxis = list(title = title),
           xaxis = list(title = list(text=NULL), tickangle = 45),
           hoverlabel = list(align = "left"),
           barmode = "group", hovermode = "x")
  if (toupper(unique(selected$variable)) == "OR") {

    a <- ceiling(abs(selected$y * 10)) * (selected$y / abs(selected$y)) / 10
    a[selected$y == 0] <- 0
    tickvals <- seq(min(a), max(a), 0.2)
    if (! 0 %in% tickvals) {
      tickvals <- c(min(tickvals) - 0.1, tickvals + 0.1)
    }

    p <- p %>% plotly::add_lines(showlegend = FALSE, hoverinfo = "none",
                                 y = 1,
                                 line = list(color = "red", dash = "dot")) %>%
               plotly::layout(yaxis = list(autorange = FALSE,
                                           rangemode = "tozero",
                                           range = c(-1, 0.5),
                                           fixedrange = TRUE,
                                           tickmode = "array",
                                           tickvals = tickvals,
                                           ticktext = tickvals + 1))
  }

  p

}
