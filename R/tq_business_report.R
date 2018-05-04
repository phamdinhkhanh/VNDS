#' @description
#' Lay ket qua kinh doanh khoan vietNam
#' Ham so nay se lay ket qua kinh doanh chung khoan tu nam nam tai chinh ket thuc tro ve n nam truoc, co the lua chon qui hoac nam
#'
#' @param symbol Ma chung khoan, gom 3 ki tu
#' @param endYear nam ket thuc
#' @param n so chu ki tai chinh (toi da la 5)
#' @param period lua chon theo nam voi 'IN_YEAR' va theo qui voi 'Q1','Q2','Q3','Q4'
#' @return mot data frame cac chi so ket qua kinh doanh theo nam tai chinh
#' \item{tibble object} {business report data}
#' @export
#' @example
#' df <- tq_business_report("VND",2017,5,"Q1")


tq_business_report <- function (symbol, endYear, n, period){
  #Danh lua server save cookie
  url <- paste0("https://www.vndirect.com.vn/portal/bao-cao-ket-qua-kinh-doanh/", symbol, ".shtml")
  resp <- GET(url)

  #Get data ve
  url <- "https://www.vndirect.com.vn/portal/ajax/listed/SearchIncomeStatement.shtml"
  #Tao form-data request
  fd <- list(
    searchObject.fiscalQuarter = period,
    searchObject.fiscalYear = endYear,
    searchObject.numberTerm = n,
    searchObject.moneyRate = "1,000,000"
  )

  #Tao request post
  resp <- POST(url,body = fd,encode = "form")
  #Lay du lieu financeInfoList
  json <- content(resp)$model$financeInfoList

  df <- data.frame()
  for (i in 23:1) {
    row <- json[i] %>% unlist() %>% as.data.frame %>% t()
    df <- rbind(row,df)
  }


  timeColName <-  c((df[1,c(21:25)]))
  timeColName <- unlist(timeColName) %>% as.character()
  colnames(df) <- c(colnames((df[,c(1:25)])),timeColName,"usingPaging")
  row.names(df) <- 1:23
  #Chi lay cac cot quan trong
  df <- df[,c(8,3,26:30)]
  #Loai bo cac cot khong co gia tri
  df <- removeBlankCol(df)
  #ConvertNumber
  #load ham tu utils.R
  if(!exists("convertNumber", mode = "function")) {
    source("R/utils.R")
  }

  df[,-c(1:2)] <- lapply(df[,-c(1:2)],convertNumber)
  return(data.frame(symBols=symbol,df))
}


