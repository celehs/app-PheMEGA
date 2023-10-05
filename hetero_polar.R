

library(dplyr)
library(echarts4r)

## Generate interactive circular plot  ======================
hetero_polar <- function(selected, thr_p = 0.05, groups = NULL) {

  if (grepl("^\\d.+", unique(selected$from))) {
    selected <- selected[order(selected$dataset, selected$to), ]
  } else {
    selected$chr <- as.numeric(gsub("(\\d+):.+", "\\1", selected$to, perl = TRUE))
    selected$pos <- as.numeric(gsub("\\d+:(\\d+):.+", "\\1", selected$to, perl = TRUE))
    selected <- selected[order(selected$chr, selected$pos), ]
  }

  ances <- unique(selected$ance)

  colors <- list("EUR" = "#FFCCCC",
                 "AFR" = "#377EB8",
                 "AMR" = "#4DAF4A",
                 "EAS" = "#984EA3")


  if (!is.null(groups)) {
    selected <- selected[selected$dataset %in% groups, ]
  }
  
  isOR <- toupper(unique(selected$variable)) == "OR"

  title <- ifelse(isOR, "Odds Ratio", "Beta Coefficient")

  if (nrow(selected) > 50) {
    title <- paste0(title, " (Only show top 20)")
    selected_1 <- selected[!is.na(selected$p_heter_adj), ]
    selected_1 <- selected_1[order(selected_1$p_heter_adj), ]
    selected_1 <- selected_1[1:20, ]
    selected <- selected[selected$to %in% selected_1$to, ]
    rm(selected_1)
  }

  selected$hetero <- signif(selected$p_heter_adj, 2)
  selected$pval <- signif(selected$pval, 2)
  selected$maf <- signif(selected$maf, 2)
  selected$value <- signif(selected$value, 3)

  meta <- as.data.frame(t(selected[, c("pval", "maf", "hetero", "desc")]))
  meta_l <- as.list(meta)
  names(meta_l) <- paste0(selected$to, "_", selected$ance)
  meta_json <- jsonlite::toJSON(meta_l)
  
  if (isOR) {
    selected$value <- selected$value - 1
  }

  selected |>
    group_by(ance) |>
    e_charts(to, renderer = "svg") |>
    e_polar(radius = c("20%", "75%")) |>
    e_angle_axis(to, show = TRUE, splitLine = list(show = TRUE)) |>
    e_radius_axis(axisLabel = list(
      formatter = htmlwidgets::JS("function (value, index) {
        if (", tolower(isOR), ") {
          return (value + 1).toFixed(2);
        } else {
          return (value).toFixed(2);
        }
    }"))) |>
    e_bar(value, coord_system = "polar", itemStyle = list(opacity = 0.8)) |>
    e_color(color = as.vector(unlist(colors[sort(ances)]))) |>
    e_tooltip(trigger = "axis",
              formatter = htmlwidgets::JS(paste0("
      function(params){
        var meta = ", meta_json, "; 
        var blank = '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;';
        var s = '';
        var value;
        for(let i = 0; i < 2; i++) {
          value = params[i].value;
          if (", tolower(isOR), ") {
            value = value + 1;
          }
         var index = params[0].name + '_' + params[i].seriesName;
         if (params[i].seriesName == 'EUR') {
           s += '<tr><td><span style=\"color:' + params[i].color + '\">&#x25CF</span> ' + params[i].seriesName + '</td><td>'+blank+'</td><td>' + meta[index][0] + '</td><td>'+blank+'</td><td>' + value + '</td><td>'+blank+'</td><td align=\"right\">-</td><td>'+blank+'</td><td>'+ meta[index][1] +'</td></tr>';
         } else {
           s += '<tr><td><span style=\"color:' + params[i].color + '\">&#x25CF</span> ' + params[i].seriesName + '</td><td>'+blank+'</td><td>' + meta[index][0] + '</td><td>'+blank+'</td><td>' + value + '</td><td>'+blank+'</td><td align=\"right\">' + meta[index][2] + '</td><td>'+blank+'</td><td>'+ meta[index][1] +'</td></tr>';
         }
        }
        s = '<table><tr><th>Population</th><th>'+blank+'</th><th>P-value</th><th>'+blank+'</th><th>' + '", unique(toupper(selected$variable)), "' + '</th><th>'+blank+'</th><th>hetero (vs EUR)</th><th>'+blank+'</th><th>MAF</th></tr>' + s + '</table>';
        s = '<b>' + params[0].name + ':&nbsp;&nbsp;</b>' + meta[params[0].name + '_' + params[0].seriesName][3] + '<br>' + s;
        return s;
      }
    "))) |>
    e_title(title) |>
    e_toolbox_feature(feature = "saveAsImage") |>
    e_toolbox_feature(feature = "dataView")

}