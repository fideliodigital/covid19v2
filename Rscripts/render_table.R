require(data.table)
require(DT)

attach_link <- function(d, column, url_column){
  dt <- d[, c(column, url_column), with = F]
  dt$column_ <- d[[column]]
  dt$url_column_ <- d[[url_column]]
  
  dt[!startsWith(url_column_, "http"), url_column_ := paste0("http://", url_column_)]
  dt[startsWith(url_column_, "http") & !is.na(url_column_), column_ := paste0('<a href=\"', url_column_, '\" target=\"_blank\">', column_, '</a>')]
  dt$column_
  
}

render_table <- function(dt, number = NULL, currency = NULL, percentage = NULL, round = NULL, dom = 'ftirp', pageLength = 10, filter = "none"){
  # dom source: https://datatables.net/reference/option/dom
  if(!is.null(currency)){
    currency <- currency[currency %in% colnames(dt)]
  } else if(!is.null(percentage)){
    percentage <- percentage[percentage %in% colnames(dt)]
  } else if(!is.null(round)){
    round <- round[round %in% colnames(dt)]
  } else if(!is.null(number)){
    number <- number[number %in% colnames(dt)]
  }
  DT::datatable(dt
                , rownames = FALSE
                , escape = FALSE
                , filter = filter
                , options = list(
                  pageLength = pageLength
                  , dom = dom
                  , scrollX = TRUE
                ) # Reference for dom: https://datatables.net/reference/option/dom
  ) %>%
    formatCurrency(currency, digits = 0) %>%
    formatCurrency(number, digits = 0, currency = "") %>%
    formatPercentage(percentage, digits = 1) %>%
    formatRound(round, digits = 0)
}
