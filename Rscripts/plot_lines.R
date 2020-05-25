require(data.table)
require(plotly)

plot_lines <- function(dt, cat1_, cat2_, num_, title, color_palette = NULL, sort = T, font = NULL, showlegend = F, yaxis_title = "", symbol = "circle", categoryorder_ = NULL, xaxis_showticklabels = F, area_fill = 'none', yaxis_showticklabels = F, xaxis_label_first_last = F, xaxis_title = "", legend_orientation = "v", legend_yanchor = "center", legend_xanchor = "center", legend_x = 0, legend_y = 0, margin_bottom = 0, margin_left = 0, margin_right=0, margin_top=0, xaxis_fixedrange=F, yaxis_fixedrange=F, legend_title=""){
  # dt = d[logic & `Método de pago` %in% id_]
  dt$cat1_ <- as.character(dt[[cat1_]])
  dt$num_ <- as.numeric(dt[[num_]])
  dt$cat2_ <- as.character(dt[[cat2_]])
  dt <- dt[!is.na(cat1_) & !is.na(cat2_), .(
    num_ = sum(num_, na.rm = T)
  ), keyby = .(cat1_, cat2_)]
  cats_ <- unique(dt[, sum(num_), keyby = .(cat2_)][order(V1, decreasing = T)]$cat2_)
  temp_ <- dt[, .(num_ = sum(num_)), keyby = .(cat2_)][order(num_, decreasing = T)]
  if(is.null(color_palette)){
    color_palette <- RColorBrewer::brewer.pal(n = 8, name = "Set2")
  }
  if(length(symbol) == 1 | is.null(symbol)){
    symbol = rep(symbol, length(unique(dt$cat2_)))
  }
  ratio_color <- nrow(temp_) / length(color_palette)
  ratio_symbol <- nrow(temp_) / length(symbol)
  temp_$color_ <- rep(color_palette, ceiling(ratio_color))[1:nrow(temp_)]
  temp_$symbol_ <- rep(symbol, ceiling(ratio_symbol))[1:nrow(temp_)]
  dt <- merge.data.table(x = dt, y = temp_[,.(cat2_, color_, symbol_)], by = "cat2_")
  if(!is.null(categoryorder_)){
    categoryorder_ <- categoryorder_
    dt$cat1_ <- factor(dt$cat1_, levels = categoryorder_)
  } else if(sort){
    dt <- dt[order(num_, decreasing = T)]
    categoryorder_ <- unique(dt$cat1_)
  } else {
    categoryorder_ <- unique(dt$cat1_)
  }
  if(xaxis_label_first_last){
    dt$xaxis_label <- c(dt$cat1_[1], rep(NA, length(dt$cat1_)-2), dt$cat1_[length(dt$cat1_)])
  } else {
    dt$xaxis_label <- dt$cat1_
  }
  p <- plot_ly(
    data = dt
    , x = ~cat1_
    , y = ~num_
    , color = ~cat2_
    , colors = ~color_
    , mode = 'markers'
    , symbol = ~cat2_
    , symbols =  ~symbol_
    , hovertemplate = paste0(cat1_,': %{x}')
    , fill = area_fill
  ) %>% add_lines()
  if(!is.null(title)){
    if(title == ""){
      title = paste0(toupper(num_), ": ", cat1_, " and ", cat2_)
    }
  }
  p <- p %>%
    layout(
      title = list('text' = title
                   , font = list(
                     'family' = 'Open Sans'
                     , 'font-size' = '17px'
                     , 'color' = '#363636'
                     , 'font-weight' = 'bold'
                   ))
      , margin = list(
        l = margin_left,
        r = margin_right,
        b = margin_bottom,
        t = margin_top,
        pad = 0
      )
      , font= font
      , showlegend = showlegend
      , legend = list(
        title = list(
          text=legend_title
          )
        , orientation = legend_orientation
        , yanchor = legend_yanchor  # use center of legend as anchor
        , xanchor = legend_xanchor  # use center of legend as anchor
        , x = legend_x
        , y = legend_y
        # , traceorder = "reversed"
        , valign = "middle"
        )
      , xaxis = list(
        title = xaxis_title
        , showgrid = FALSE
        , showticklabels = xaxis_showticklabels
        , categoryorder = "array"
        , categoryarray = categoryorder_
        , ticktext = ~xaxis_label
        , fixedrange=xaxis_fixedrange
      )
      , yaxis = list(
        title = yaxis_title
        , showgrid = FALSE
        , showticklabels = yaxis_showticklabels
        , fixedrange=yaxis_fixedrange
      )
    )
  p %>% config(displayModeBar = F) # Borrar la barra superior con instrucciones
}
