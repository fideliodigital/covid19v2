plot_pie <- function(dt, cat_, num_, title, add_n = T, show_values = T, showlegend = T, color_palette = NULL, hole = 0, font = NULL){
  dt$cat_ <- dt[[cat_]]
  dt$num_ <- as.numeric(dt[[num_]])
  dt <- dt[!is.na(cat_), .(num_ = sum(num_, na.rm = T)), key = cat_]
  if(add_n){
    title = paste0(title, " (n = ", dt[,sum(.SD), .SDcols = num_], ")")
  } 
  if(is.null(color_palette)){
    color_palette <- RColorBrewer::brewer.pal(n = 8, name = "Set2")
  }
  ratio <- nrow(dt) / length(color_palette)
  dt <- dt[order(num_, decreasing = T)]
  dt$color_ <- rep(color_palette, ceiling(ratio))[1:nrow(dt)]
  cats_ <- unique(dt[, sum(num_), keyby = .(cat_)][order(V1, decreasing = T)]$cat_)
  if(length(cats_) > 10){
    temp <- head(cats_, 10)
    dt[!cat_ %in% temp, cat_ := "Otros"]
  }
  plot_ly(
    data = dt
    , labels = ~cat_
    , values = ~num_
    , type = 'pie'
    , name = ""
    , hole = hole
    , marker = list(colors = ~color_)
    , hovertemplate = {
      if (show_values){
        ~paste0(`cat_`, ": ", `num_`)
      } else {
        ~`cat_`
      }
    } 
    # , width = width
    # , height = height
    # , width = 890
    # , height = 501
  ) %>% layout(
    title = title
    , margin = list(
      l = 20,
      r = 0,
      b = 100,
      t = 50,
      pad = 0
    )
    , font = font
    , showlegend = showlegend
    , xaxis = list(
      showgrid = FALSE
      , automargin = F
      , zeroline = FALSE
      , showticklabels = FALSE)
    , yaxis = list(
      showgrid = FALSE
      , zeroline = FALSE
      , automargin = F
      , showticklabels = FALSE)
  ) %>% config(displayModeBar = F) # Borrar la barra superior con instrucciones
}
