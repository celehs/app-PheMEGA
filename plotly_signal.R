library(dplyr)
library(plotly)

signal_plotly <- function(df, thr_pval = 5 * 10^-8, type = "column") {
  df$p <- df$pval
  df$p[df$p == 0] <- min(df$p[df$p != 0])/2
  df$p <- signif(-log10(df$p), 3)
  df$p[df$p < 1] <- 1

  colors <- list("EUR" = "#E41A1C",
                 "AFR" = "#377EB8",
                 "AMR" = "#4DAF4A",
                 "EAS" = "#984EA3",
                 "META" = "#FF7F00")

  df$pval <- signif(df$pval, 2)
  df$value = signif(df$value, 3)
  df$maf <- signif(df$maf, 2)
  df$hetero <- signif(as.numeric(df$p_heter_adj), 2)
  df$group_info <- df$group

  if(!grepl("^\\d.+", unique(df$from), perl = TRUE)){
    # snp ====
    df$chrom <- as.numeric(gsub("^(\\d+):.+", "\\1", df$to, perl = TRUE))
    df$pos <- as.numeric(gsub("^\\d+:(\\d+):.+", "\\1", df$to, perl = TRUE))
    df <- df[order(df$chrom, df$pos), ]
    groups <- table(df$chrom[df$ance == df$ance[1]])
    if(length(names(groups)) < 5){
      names(groups) <- paste0("chr", names(groups))
    } else {
      names(groups)[1] <- paste0("chr", names(groups)[1])
    }

  } else {
    # traits ====
    df <- df[order(df$dataset, df$desc), ]
    groups <- table(df$dataset[df$ance == df$ance[1]])
    print(groups)
  }

  df_groups <- data.frame(text = names(groups), 
                          x0 = cumsum(c(-0.5, groups[1:(length(groups) - 1)])),
                          x1 = cumsum(groups) - 0.5)
  df_groups$x = df$desc[(df_groups$x0 + 0.5 + (df_groups$x1-df_groups$x0)/2)*2]

  fig <- plot_ly(data = df, x = ~desc, y = ~p, color = ~ance,
                 hovertext = ~paste(desc, " (", to, ")",
                               "<br>P-value: ", pval, '<br>', toupper(variable), ': ', value,
                               # '<br>Heterogeneity adjusted P-value (vs EUR): ', hetero, 
                               '<br>MAF:', maf),
                 hoverinfo = "text",
                 type = 'scatter', mode = 'markers', 
                 colors = unlist(colors[sort(unique(df$ance))])) %>%
           layout(hovermode = "x", 
                  yaxis = list(title = '-log10(P-value)'), 
                  legend = list(title=list(text='<b>Population</b>')),
                  xaxis = list(title = list(text=NULL), tickangle = 45,
                               categoryorder = "array", 
                               categoryarray = unique(df$desc)))
  shapes <- list(list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = -log10(thr_pval),
    y1 = -log10(thr_pval),
    line = list(color = "black", dash="dot")
  ))
  for (i in 1:nrow(df_groups)) {
    color = ifelse(i %% 2 == 0, "grey", "white")
    shapes <- c(shapes, list(list(
                     type = "rect", opacity = ifelse(i %% 2 == 0, 0.2, 0), 
                     fillcolor = color, line = list(color = color), 
                     x0 = df_groups$x0[i], x1 = df_groups$x1[i],
                     y0 = min(df$p) - 1, y1 = max(df$p) + 1)))
    fig <- fig %>% add_text(showlegend = FALSE, hoverinfo="none",
                            textfont = list(color = c('#000000')),
                            text = df_groups$text[i],
                            x = df_groups$x[i], y = max(df$p) + 1)
  }
  fig %>% layout(shapes = shapes) %>% 
    add_text(showlegend = FALSE, hoverinfo="none",
             text = paste0("threshold:<br>", thr_pval),
             textfont = list(color = c('#000000')),
             x = df$desc[nrow(df)], y = -log10(thr_pval))
}
