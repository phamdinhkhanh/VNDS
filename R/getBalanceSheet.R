#' @description
#' Lay bao cao tai chinh
#' @param symbol Ma chung khoan, gom 3 ki tu
#' @param endYear nam ket thuc
#' @param n so chu ki tai chinh (toi da la 5)
#' @param period lua chon theo nam voi 'IN_YEAR' va theo qui voi 'Q1','Q2','Q3','Q4'
#' @return mot data frame cac chi so bao cao tai chinh theo nam tai chinh
#' \item{tibble object} {balanceSheet data}
#' @return mot data frame chua gia chung khoan va khoi luong
#' @export
#' @example
#' VND <- getBalanceSheet("VND",2017,5,"Q1")


getBalanceSheet <- function(symbol, endYear, n, period){
  url <- paste0("https://www.vndirect.com.vn/portal/bang-can-doi-ke-toan/",symbol,".shtml")

  fd <- list(
    searchObject.fiscalQuarter = period,
    searchObject.fiscalYear = endYear,
    searchObject.numberTerm = n,
    searchObject.moneyRate = "1,000,000"
  )

  resp <- resp <- POST(url,body = fd,encode = "form")

  content <- resp %>% read_html() %>% html_nodes(xpath='//*[@id="fBalanceSheet"]/div[4]/div/div[2]/table') %>%
    html_text() %>% str_remove_all("\t+") %>% str_replace_all("\n\\s+\n","\n") %>% str_split("\n+") %>% unlist()
  nCol <- n+1
  rs <- itemRowsBind(content,nCol)
  rs <- rbind(rs$df[-1,],itemRowsBind(rs$remainRow,nCol)$df[-1,]) %>% removeBlankCol()
  rs[,2:nCol] <- rs[,2:nCol] %>% lapply(convertNumber)
  return(data.frame(symBols=symbol,rs))
}



itemRowsBind <- function(content,nCol){
  nRow <- round(length(content)/nCol,0)
  options(stringsAsFactors = FALSE)
  df <- data.frame()
  for (i in 0:(nRow-1)){
    charDetect <- str_detect(content[(nCol*i+2):(nCol*(i+1))],"[0-9]")
    if(all(charDetect)){
      row_i <- content[(nCol*i+1):(nCol*(i+1))]
      df <- rbind(df,row_i)
    }
    else {
      #tim ra vi tri la chuoi ki tu
      k <- c(2:nCol)[!charDetect] %>% min()
      subContent <- c("item",content[c(2:nCol,(nCol*i+k):length(content))])
      break
    }
  }
  colnames(df) <- c("itemName",content[2:nCol])
  return(list(df = df,remainRow=subContent))
}

#itemRowsBind(content,nCol)
#itemRowsBind(itemRowsBind(content,nCol)$remainRow,nCol)

