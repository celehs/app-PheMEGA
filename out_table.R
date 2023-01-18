library(dplyr)

out_table <- function(df, dict, num_samples, sortby = "Variable", decreasing = FALSE) {

  df_table <- table(df$to)
  df <- df[df$to %in% names(df_table[df_table == length(unique(df$ance))]), ]

  if (sortby == "Variable") {
    df <- df[order(df$to, decreasing = decreasing), ]
  } else if (sortby == "P-value") {
    if (!decreasing) {
      df_min <- df %>%
        group_by(.data$to) %>%
        summarise(min_p = min(.data$pval))
      df <- left_join(df, df_min, by = "to")
      df <- df[order(df$min_p, df$to, df$pval), 1:(ncol(df) - 1)]
    } else {
      df_max <- df %>%
        group_by(.data$to) %>%
        summarise(max_p = max(.data$pval))
      df <- left_join(df, df_max, by = "to")
      df <- df[order(df$max_p, df$to, df$pval, decreasing = TRUE), 1:(ncol(df) - 1)]
    }
  } else if (sortby == "Heterogeneity Adjusted P-value (to EUR)") {

    compares <- function(x, fun) {
      if (length(na.omit(x)) > 0) {
        fun(x, na.rm = TRUE)
      } else {
        Inf
      }
    }
    if (!decreasing) {
      df_min <- df %>%
        group_by(.data$to) %>%
        summarise(min_p = compares(.data$p_heter_adj, min))
      df <- left_join(df, df_min, by = "to")
      df <- df[order(df$min_p, df$to, df$p_heter_adj), ]
    } else {
      df_max <- df %>%
        group_by(.data$to) %>%
        summarise(max_p = compares(.data$p_heter_adj, max))
      df <- left_join(df, df_max, by = "to")
      df <- df[order(df$max_p, df$to, df$pval, decreasing = TRUE), ]
    }
  }

  df_show <- df
  df_show$p_heter_adj <- sprint(df_show$p_heter_adj)
  df_show$p_heter_adj[df_show$ance %in% c("EUR", "META")] <- "-"

  df_show$pval <- sprint(df_show$pval)
  df_show$maf <- sprint(df_show$maf)
  df_show$value <- sprint(df_show$value)
  if (sum(!grepl("^\\d.+", df$from)) == 0) {
    df_show <- left_join(df_show, num_samples, by = c("to" = "trait", "ance"))
    df_show$num_samples <- paste0(df_show$num_samples, "(", df_show$num_cases, "/", df_show$num_controls, ")")
    df_show$num_samples <- gsub("(NA/NA)", "", df_show$num_samples, fixed = TRUE)
    df_show <- df_show[, c("ance", "to", "desc", "pval", "variable", "value", "p_heter_adj", "num_samples", "group")]
    df_show$variable[df_show$variable == "beta"] <- "continuous"
    df_show$variable[df_show$variable == "or"] <- "binary"
    colnames(df_show) <- c("Ance", "Variable", "Description", "P-value", "Trait Type", "Value", "Heterogeneity Adjusted P-value (to EUR)", "Num_samples(cases/controls)", "Group")
  } else {
    df_show$from <- dict$desc[match(df_show$from, dict$id)]
    df_show <- df_show[, c("ance", "to", "desc", "pval", "maf", "value", "p_heter_adj", "group")]
    colnames(df_show) <- c("Ance", "Variable", "Description chr:pos:alt:ref", "P-value", "Maf", toupper(unique(df$variable)), "Heterogeneity Adjusted P-value (to EUR)", "Gene")
  }

  df_show

}
