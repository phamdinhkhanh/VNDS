#' @description
#' Lay thong tin co ban ve cong ty gom san niem yet, ten cong ty, linh vuc hoat dong, ngay niem yet
#'
#' @param symbol Ma chung khoan, gom 3 ki tu
#' @return thong tin cong ty
#' @export
#' @example
#' VND_Info <- getCompanyInfo("VND")


tq_company_info <-  function(symbol) {
  url <- paste0("https://finfo-api.vndirect.com.vn/stocks?symbol=",symbol)
  resp <- GET(url) %>% content()
  resp <- resp$data %>% unlist() %>% as.data.frame() %>% t()
  row.names(resp) <- 1
  return(resp)
}



