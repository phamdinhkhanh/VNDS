#' Lay gia chung khoan vietNam
#'
#' Ham so nay se lay gia chung khoan tu ngay bat dau den ngay ket thuc
#'
#' @param symbol Ma chung khoan, gom 3 ki tu
#' @param from ngay bat dau dinh dang yyyy-mm-dd
#' @param to ngay ket thuc dinh dang yyyy-mm-dd
#' @return mot data frame chua gia chung khoan va khoi luong
#' @export 
#' @import rvest
#' @import xml2
#' @import httr
#' @import stringr
#' @import magrittr
#' @import dplyr
getSymbols <- function(symbol, from, to){
  url <- "https://www.vndirect.com.vn/portal/thong-ke-thi-truong-chung-khoan/lich-su-gia.shtml"
  #lay page cuoi cung
  lastPage <- getLastPage(url,symbol,from,to)
  #Khoi tao matrix
  cname <- c("DATE","CHANGE.PERCENT1","CHANGE.PERCENT2","OPEN","HIGH","LOW","CLOSE","AVERAGE","CLOSE.ADJUST","MATCH.VOLUME","RECONCILE.VOLUME")
  symbolData <- matrix(nrow = 0,ncol = 11,byrow = TRUE,dimnames = list(c(),cname))
  for (page in 1:lastPage) {
    #Tao form-data request
    fd <- list(
      searchMarketStatisticsView.symbol= symbol,
      strFromDate = dateChar(from),
      strToDate = dateChar(to),
      pagingInfo.indexPage = page
    )

    #Tao request post
    resp <- POST(url,body = fd,encode = "form")

    #doc html_node
    tmp <- resp %>% read_html() %>% html_nodes(xpath='//*[@id="tab-1"]/div[2]/ul') %>%
      html_children() %>%  html_text()

    noDays <- length(tmp)

    for(i in 2:noDays){
      row <- str_replace_all(tmp[i],"\t","") %>% str_replace_all("\n"," ") %>%
        gsub(' +',' ',.) %>% str_trim() %>%
        str_split(" ") %>% unlist() %>% str_split(" ") %>% unlist() %>% as.vector()
      symbolData <- rbind(symbolData,row)
    }
  }

  symbolData <- data.frame(symbolData,row.names = symbolData[,1])
  symbolData[,1]<- as.Date(symbolData[,1],"%Y-%m-%d")
  symbolData[,2:11] <- apply(symbolData[,2:11],2,as.numeric)
  myexport(symbolData)
  #doi ten cho bang
  assign(symbol,symbolData,envir = .GlobalEnv)
  rm(list = "symbolData",envir = .GlobalEnv)
  return(list(symbolData,
              call = match.call()))
}

getLastPage <- function (url,symbol,from,to){
  #Tao form-data request
  fd <- list(
    searchMarketStatisticsView.symbol= symbol,
    strFromDate = dateChar(from),
    strToDate = dateChar(to)
  )

  resp <- POST(url,body = fd,encode = "form")
  #Lay trang cuoi
  resp %>% read_html() %>% html_nodes(xpath='//*[@id="tab-1"]/div[1]') %>%
    html_text(.,trim=TRUE) %>%
    str_split("[^0-9]+") %>% unlist() -> df

  lastPage <- ifelse(is.na(df[3]),1,as.numeric(df[3]))
  #print(paste0("lastPage: ", lastPage))
  return(lastPage)
}


myexport <- function(...) {
  arg.list <- list(...)
  names <- all.names(match.call())[-1]
  for (i in seq_along(names)) assign(names[i],arg.list[[i]],.GlobalEnv)
}


dateChar <- function(dateTime){
  dateTime %<>%  as.Date() %>% format(.,format = "%d/%m/%Y")
  return(dateTime)
}



