#' @description
#' Lay thong tin co ban ve cong ty gom san niem yet, ten cong ty, linh vuc hoat dong, ngay niem yet
#'
#' @param symbol Ma chung khoan, gom 3 ki tu
#' @return thong tin cong ty
#' @export
#' @example
#' VND_Info <- tq_company_info(c("VND","VPB"))


tq_company_info <-  function(symbols) {
    url <- paste0("https://finfo-api.vndirect.com.vn/stocks?status=all")
    resp <- GET(url) %>% content('text') %>% jsonlite::fromJSON()
    if(missing(symbols)){
      resp$data
    } else {
      resp$data %>% filter(symbol %in% symbols) 
    }
}



