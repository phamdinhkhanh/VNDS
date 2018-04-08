#' @description
#' Lay gia chung khoan vietNam
#' Ham so nay se lay gia chung khoan tu ngay bat dau den ngay ket thuc
#'
#' @param symbol Ma chung khoan, gom 3 ki tu
#' @param from ngay bat dau dinh dang yyyy-mm-dd
#' @param to ngay ket thuc dinh dang yyyy-mm-dd
#' @return mot data frame chua gia chung khoan va khoi luong
#' @export
#' @example
#' VND <- getSymbols("VND","2017-01-01","2018-01-01", src="VND")


# PRIMARY FUNCTION
getSymbols <- function(symbol,from,to,src="VND"){
  if(src=="VND"){
    invisible(getSymbolVND(symbol,from,to))
  } else if(src=="CP68"){
    invisible(getSymbolCP68(symbol,from,to))
  }
}


################################## get data VNDirect ###############################################
getSymbolVND <- function(symbol, from, to){
  url <- "https://www.vndirect.com.vn/portal/thong-ke-thi-truong-chung-khoan/lich-su-gia.shtml"
  #lay page cuoi cung
  lastPage <- getLastPage(url,symbol,from,to)
  #Khoi tao matrix
  cname <- c("DATE","CHANGE.PERCENT1","CHANGE.PERCENT2","OPEN","HIGH","LOW","CLOSE","AVERAGE","CLOSE.ADJUST","MATCH.VOLUME","RECONCILE.VOLUME")
  symbolData <- matrix(nrow = 0,ncol = 11,byrow = TRUE,dimnames = list(c(),cname))
  for (page in 1:lastPage) {
    #Tao form-data request
    #load ham tu utils.R
    if(!exists("dateChar", mode = "function")) {
      source("R/utils.R")
    }

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
  #myexport(symbolData)
  #doi ten cho bang
  #assign(symbol,symbolData,envir = .GlobalEnv)
  #rm(list = "symbolData",envir = .GlobalEnv)
  cat(paste0("#",symbol," from ",from, " to ",to," already cloned "))
  invisible(symbolData)
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



################################## get data CP68 ###############################################

getSymbolCP68 <- function (symbol,from,to){
  url<-"http://www.cophieu68.vn/historyprice.php"
  #lay cac trang cuoi cung
  fd<-list(
    id = symbol
  )

  resp <- POST(url,body=fd,endcode="form")
  resp %>% read_html() %>% html_nodes(xpath='//*[@id="navigator"]/li[7]/a') %>%
    html_attrs() -> tmp
  positionString <- str_locate(tmp[1],"[0-9]") %>% as.data.frame() %$% start
  lastPage <- as.numeric(str_sub(unlist(tmp[1]),positionString,positionString+1))

  #khoi tao dataframe
  symbolData <- data.frame(
    DATE = character(),
    REF.PRICE = numeric(),
    CHANGE.PERCENT1 = numeric(),
    CHANGE.PERCENT2 = numeric(),
    CLOSE = numeric(),
    MATCH.VOLUME = numeric(),
    OPEN = numeric(),
    HIGH = numeric(),
    LOW = numeric(),
    RECONCILE.VOLUME = numeric(),
    FOREIGN.BUY = numeric(),
    FOREIGN.SALE = numeric(),
    VALUE = numeric()
  )

  #tao vong lap lay du lieu
  for(i in 1:lastPage){
    #tao form-data
    fd<-list(
      currentPage = i,
      id = symbol
    )

    resp <- POST(url,body=fd,endcode="form")


    resp %>% read_html() %>% html_nodes(xpath='//*[@id="content"]/table') %>%
      html_table() %>% as.data.frame() -> tmp
    tmp <- tmp[-1,2:14]
    #load ham tu utils.R
    if(!exists("convertDate", mode = "function")) {
      source("R/utils.R")
    }
    tmp[,1] <- as.Date(convertDate(tmp[,1]),format = "%Y-%m-%d")
    #check dieu kien de continue
    if(min(tmp[,1]) > to){next}
    #check dieu kien de break
    if(max(tmp[,1]) < from){break}
    #load ham tu utils.R
    if(!exists("subComma", mode = "function")) {
      source("R/utils.R")
    }
    tmp[,c(6,10:12)] <- apply(tmp[,c(6,10:12)],2,subComma)
    #load ham tu utils.R
    if(!exists("convertPercent", mode = "function")) {
      source("R/utils.R")
    }
    tmp[,4] <- convertPercent(tmp[,4])
    tmp[,c(2:3,5:13)] <- apply(tmp[,c(2:3,5:13)],2,as.numeric)

    colnames(tmp)<-c("DATE","REF.PRICE","CHANGE.PERCENT1","CHANGE.PERCENT2","CLOSE","MATCH.VOLUME","OPEN","HIGH","LOW","RECONCILE.VOLUME","FOREIGN.BUY","FOREIGN.SALE","VALUE")
    symbolData <- rbind(symbolData,tmp)
  }

  symbolData <- data.frame(symbolData,row.names = symbolData[,1])
  colnames(symbolData)<-c("DATE","REF.PRICE","CHANGE.PERCENT1","CHANGE.PERCENT2","CLOSE","MATCH.VOLUME","OPEN","HIGH","LOW","RECONCILE.VOLUME","FOREIGN.BUY","FOREIGN.SALE","VALUE")
  symbolData <- symbolData[,c(1,3:4,7:9,5,2,6,10:13)]
  cat(paste0("#",symbol," from ",from, " to ",to," already cloned "))
  invisible(subset(symbolData,and(DATE >= from, DATE <= to)))
}


#ham convertDate
#convertDate <- function(dt){
#  paste0(str_sub(dt,7,10),"-",str_sub(dt,4,5),"-",str_sub(dt,1,2))
#}

#ham thay the dau comma
#subComma <- function(v) {
#  str_replace_all(v,",","")
#}

#chuyen percent dang text sang numeric
#convertPercent <- function(v){
#  str_replace_all(v,"%","") %>% as.numeric()*0.01
#}

################################## get data cafeF ###############################################


