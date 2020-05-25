require(data.table)
require(plotly)

source("Rscripts/top_categories.R")

# Este grafica un multiple chart con gráfico de barras y una linea mostrando el cambio porcentual
plot_change1 <- function(dt, num_, two_cat_, cat1_, showlegend = F, yaxis_showticklabels = F, title = NULL, color_palette = NULL, font = NULL, title_language = "es", sort = T, yaxis_title = "", metric = "roi", revert = F){
  # dt = d[logic & `Payment Category` %in% "Cash" & !`Método de pago` %in% "OTROS Efectivo"]
  dt$num_ <- dt[[num_]]
  dt$two_cat_ <- dt[[two_cat_]]
  dt$cat1_ <- dt[[cat1_]]
  levels_ <- as.character(unique(dt$two_cat_))
  dt <- dt[!is.na(cat1_)
           , .(num_ = sum(num_))
           , keyby = .(two_cat_, cat1_)
           ][order(num_, decreasing = T)]
  dt <- dcast.data.table(data = dt, formula = "cat1_ ~ two_cat_", value.var = "num_")
  setnames(x = dt, old = levels_[1], new = "level1_")
  setnames(x = dt, old = levels_[2], new = "level2_")
  
  if(revert){
    dt[, delta_ := level1_ / level2_]
  } else{
    dt[, delta_ := level2_ / level1_]
  }
  if(metric == "roi"){
    dt[, delta_ := delta_ - 1]
  } 
  if(is.null(color_palette)){
    color_palette <- RColorBrewer::brewer.pal(n = 8, name = "Set2")
  }
  ratio <- 2 / length(color_palette)
  color_palette <- rep(color_palette, ceiling(ratio))[1:2]
  if(is.null(title)){
    if(title_language == "en"){
      title = sprintf("%s Change of %s by %s (%s - %s)", "%", num_, cat1_, levels_[1], levels_[2])
    } else if(title_language == "es"){
      title = sprintf("Cambio (%s) de %s por %s (%s - %s)", "%", num_, cat1_, levels_[1], levels_[2])
    }
  }
  dt[delta_ < 0, delta_text := paste0(round(100*delta_, 1), "%")]
  dt[delta_ >= 0, delta_text := paste0("+", round(100*delta_, 1), "%")]
  if(sort){
    dt <- dt[order(level2_, decreasing = T)]
    categoryorder_ <- unique(dt$cat1_)
  } else {
    categoryorder_ <- unique(dt$cat1_)
    categoryorder_ <- categoryorder_[order(categoryorder_, decreasing = T)]
  }
  fig <- plot_ly(dt)
  fig <- fig %>% add_trace(x = ~cat1_, y = ~level1_, type = 'bar', name = levels_[1],
                           marker = list(color = color_palette[1]),
                           hoverinfo = "text",
                           text = ~paste(levels_[1], ": ", cat1_))
  fig <- fig %>% add_trace(x = ~cat1_, y = ~level2_, type = 'bar', name = levels_[2],
                           marker = list(color = color_palette[2]),
                           hoverinfo = "text",
                           text = ~paste0(levels_[2], ": ", cat1_))
  fig <- fig %>% add_trace(x = ~cat1_, y = ~delta_, type = 'scatter', mode = 'lines+text'
                           , name = '% cambio', yaxis = 'y2'
                           , line = list(color = color_palette[3])
                           , hoverinfo = "text"
                           # , hoverinfo = ~paste0(cat1_,' (', delta_text, ")")
                           , hovertemplate = ~paste0(cat1_,' (', delta_text, ")")
                           , text = ~delta_text
                           , textposition = "top")
  fig <- fig %>% layout(
    title = list('text' = title
                 , font = list(
                   'family' = 'Open Sans'
                   , 'font-size' = '17px'
                   , 'color' = '#363636'
                   , 'font-weight' = 'bold'
                 ))
    , margin = list(
      l = 0,
      r = 0,
      b = 50,
      t = 50,
      pad = 0
    )
    , font = font
    , showlegend = showlegend
    , xaxis = list(
      title = ""
      , showticklabels = T
      # No comentar la línea de abajo porque altera resultados de Geografía de Pagos
      , categoryorder = 'array'
      , categoryarray = categoryorder_
    )
    , yaxis = list(
      title = yaxis_title
      , showticklabels = yaxis_showticklabels
    )
    , yaxis2 = list(
      side = 'right'
      , overlaying = "y"
      , title = ''
      , showticklabels = F
      , showgrid = FALSE
      , zeroline = FALSE
      , showlegend = FALSE
    )
  )
  fig
}

# Este es un gráfico de barras horizontal resaltando la segunda categoría más importante
plot_change <- function(dt, num_, two_cat_, cat1_, cat2_ = NULL, showlegend = F, title_language = "en", title = NULL, color_palette = NULL, font = NULL, revert_levels = F, xaxis_fixedrange = F, yaxis_fixedrange = F){
  # dt = d[logic & `Payment Category` %in% "Cash" & !`Método de pago` %in% "OTROS Efectivo"]
  dt$num_ <- dt[[num_]]
  dt$two_cat_ <- dt[[two_cat_]]
  dt$cat1_ <- dt[[cat1_]]
  levels_ <- as.character(unique(dt$two_cat_))
  if(revert_levels){
    levels_ = rev(levels_)
  }
  data <- dt[!is.na(cat1_)
           , .(num_ = sum(num_))
           , keyby = .(two_cat_, cat1_)
           ][order(num_, decreasing = T)]
  data <- dcast.data.table(data = data, formula = "cat1_ ~ two_cat_", value.var = "num_")
  data$delta_ <- (data[[levels_[2]]] / data[[levels_[1]]]) - 1
  if(is.null(color_palette)){
    color_palette <- RColorBrewer::brewer.pal(n = 8, name = "Set2")
  }
  if(!is.null(cat2_)){
    dt$cat2_ <- dt[[cat2_]]
    top <- top_categories(dt[two_cat_ %in% levels_[2]], cat1_, cat2_, num_, top = 1)
    top$Top1_ <- top[[2]]
    top$Top_ <- sapply(strsplit(x = top[[2]], split = " \\("), function(x){x[1]})
    temp_ <- top[, .N, keyby = Top_][order(N, decreasing = T)]
    ratio <- nrow(temp_) / length(color_palette)
    temp_$color_ <- rep(color_palette, ceiling(ratio))[1:nrow(temp_)]
    top <- merge.data.table(x = top, y = temp_[,.(Top_, color_)], by = "Top_")
    data <- merge.data.table(x = data, y = top, by.x = "cat1_", by.y = cat1_)
    cat1__ <- cat1_
    data[, hovertemplate := paste0(cat1__, ": ", `cat1_`, ": ", round(100*`delta_`, 0), "%", "<br>", "Top ", cat2_, ": ", Top1_)]
  } else {
    data[delta_ >= 0, Top_ := "(+%)"]
    data[delta_ < 0, Top_ := "(-%)"]
    data[delta_ >= 0, color_ := color_palette[2]]
    data[delta_ < 0, color_ := color_palette[1]]
    cat1__ <- cat1_
    data[, hovertemplate := paste0(cat1__, ": ", `cat1_`, ": ", round(100*`delta_`, 0), "%")]
  }
  
  data <- data[order(delta_, decreasing = T)]
  data$cat1_ <- factor(data$cat1_, levels = rev(unique(data$cat1_)))
  p <- plot_ly(
    data = data
    , x = ~delta_
    , y = ~cat1_
    , type = "bar"
    , hovertemplate = ~hovertemplate
    , text = ~paste0(round(100*`delta_`, 0), "%")
    , textfont = list(color = "white")
    , textposition='inside'
    , color = ~Top_
    , colors =  ~color_
  )
  if(is.null(title)){
    if(title_language == "en"){
      title = sprintf("%s Change of %s by %s (%s - %s)", "%", num_, cat1_, levels_[1], levels_[2])
    } else if(title_language == "es"){
      title = sprintf("Cambio (%s) de %s por %s (%s - %s)", "%", num_, cat1_, levels_[1], levels_[2])
    }
  }
  p %>% layout(
    title = list('text' = title
                 , font = list(
                   'family' = 'Open Sans'
                   , 'font-size' = '17px'
                   , 'fill' = '#363636'
                   , 'font-weight' = 'bold'
                 ))
    , showlegend = showlegend
    , margin = list(
      l = 0,
      r = 0,
      b = 50,
      t = 50,
      pad = 0
    )
    , font = font
    , xaxis = list(
      showticklabels = T
      , title = ""
      , tickformat='%'
      , fixedrange=xaxis_fixedrange
      )
    , yaxis = list(
      title = ""
      , fixedrange=yaxis_fixedrange
      )
    # , autosize = F
    # , width = 500
    # , height = "200%"
    # , margin = list(
    #   l = 50,
    #   r = 50,
    #   b = 100,
    #   t = 100,
    #   pad = 4
    # )
  ) %>% config(displayModeBar = F) # Borrar la barra superior con instrucciones
}
