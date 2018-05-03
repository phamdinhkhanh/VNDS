#' @description
#' Lay gia chung khoan vietNam
#' Ham so nay se lay gia chung khoan tu ngay bat dau den ngay ket thuc
#'
#' @param symbol Ma chung khoan, gom 3 ki tu
#' @param from ngay bat dau dinh dang yyyy-mm-dd
#' @param to ngay ket thuc dinh dang yyyy-mm-dd
#' @param src nguon du lieu de lay thong tin chung khoan
#' @param minimal co lay toan bo cac truong hay chi lay nhung truong chinh de phan tich
#' @return mot data frame chua gia chung khoan va khoi luong
#' @export
#' @example
#' VND <- tq_get("VND","2017-01-01","2018-01-01", src="CP68", minimal = FALSE)


# PRIMARY FUNCTION
tq_get <- function(symbol,from,to,src="VND", minimal = TRUE,...){
  if(minimal){
    colname<- c("date",
                "open",
                "high",
                "low",
                "close",
                "volume",
                "adjusted")

    if(src=="VND"){
      extractData <- tq_get_vnd(symbol,from,to)

      extractData <- extractData %>%
        mutate(volume = match.volume + reconcile.volume)

      extractData <- extractData[,colname]

      invisible(extractData)
    } else if(src=="CP68"){
      extractData <- tq_get_cp68(symbol,from,to)

      extractData <- extractData %>%
        mutate(volume = match.volume + reconcile.volume)

      extractData <- extractData %>%
        mutate(adjusted = (high + low)/2)

      extractData <- extractData[,colname]

      invisible(extractData)
    }
  } else {
    if(src=="VND"){
      invisible(tq_get_vnd(symbol,from,to))
    } else if(src=="CP68"){
      invisible(tq_get_cp68(symbol,from,to))
    }
  }
}


################################## get data VNDirect ###############################################
tq_get_vnd <- function(symbol, from, to,...){
  globalVariables(".")
  url <- "https://www.vndirect.com.vn/portal/thong-ke-thi-truong-chung-khoan/lich-su-gia.shtml"
  #lay page cuoi cung
  lastPage <- getLastPage(url,symbol,from,to)
  #Khoi tao matrix
  cname <- c("date",
             "change.percent1",
             "change.percent2",
             "open",
             "high",
             "low",
             "close",
             "average",
             "adjusted",
             "match.volume",
             "reconcile.volume")

  symbolData <- matrix(nrow = 0,
                       ncol = 11,
                       byrow = TRUE,
                       dimnames = list(c(),cname))




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
    resp <- POST(url,
                 body = fd,
                 encode = "form")

    #doc html_node
    tmp <- resp %>% read_html() %>%
      html_nodes(xpath='//*[@id="tab-1"]/div[2]/ul') %>%
      html_children() %>%
      html_text()

    noDays <- length(tmp)

    for(i in 2:noDays){
      row <- str_replace_all(tmp[i],"\t","") %>%
        str_replace_all("\n"," ") %>%
        gsub(' +',' ',.) %>%
        str_trim() %>%
        str_split(" ") %>%
        unlist() %>%
        str_split(" ") %>%
        unlist() %>%
        as.vector()
      symbolData <- rbind(symbolData,row)
    }
  }

  symbolData <- data.frame(symbolData,
                           row.names = symbolData[,1])

  symbolData[,1]<- as.Date(symbolData[,1],"%Y-%m-%d")

  symbolData[,2:11] <- suppressWarnings(apply(symbolData[,2:11],2,as.numeric))

  symbolData[is.na(symbolData)] <- 0
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

  resp <- POST(url,
               body = fd,
               encode = "form")

  #Lay trang cuoi
  resp %>% read_html() %>%
    html_nodes(xpath='//*[@id="tab-1"]/div[1]') %>%
    html_text(.,trim=TRUE) %>%
    str_split("[^0-9]+") %>%
    unlist() -> df

  lastPage <- ifelse(is.na(df[3]),
                     1,
                     as.numeric(df[3]))
  #print(paste0("lastPage: ", lastPage))
  return(lastPage)
}


myexport <- function(...) {
  arg.list <- list(...)
  names <- all.names(match.call())[-1]
  for (i in seq_along(names))
    assign(names[i],
           arg.list[[i]],
           .GlobalEnv)
}


dateChar <- function(dateTime){
  dateTime %<>%
    as.Date() %>%
    format(.,format = "%d/%m/%Y")
  return(dateTime)
}



################################## get data CP68 ###############################################

tq_get_cp68 <- function (symbol,from,to,...){
  globalVariables(".")
  url<-"http://www.cophieu68.vn/historyprice.php"

  #lay cac trang cuoi cung
  resp <- POST(url,
               body=list(id=symbol),
               endcode="form")

  resp %>% read_html() %>%
    html_nodes(xpath='//*[@id="navigator"]/li[7]/a') %>%
    html_attrs() -> tmp

  startString <- as.numeric(str_locate(tmp[1],"[0-9]")[1])
  endString <- as.numeric(str_locate(tmp[1],"[0-9]")[2])

  lastPage <- as.numeric(str_sub(unlist(tmp[1]),
                                 startString,
                                 endString))

  #khoi tao matrix
  cname <- c(
    "date",
    "ref.price",
    "change.percent1",
    "change.percent2",
    "close",
    "match.volume",
    "open",
    "high",
    "low",
    "reconcile.volume",
    "foreign.buy",
    "foreign.sale",
    "value"
  )

  symbolData <- matrix(nrow = 0,
                       ncol = 13,
                       byrow = TRUE,
                       dimnames = list(c(),cname))

  #tao vong lap lay du lieu
  for(i in 1:lastPage){

    resp <- POST(url,
                 body=list(currentPage = i, id = symbol),
                 endcode="form")


    resp %>% read_html() %>%
      html_nodes(xpath='//*[@id="content"]/table') %>%
      html_table() %>%
      as.data.frame() -> tmp

    tmp <- tmp[-1,2:14]

    #load ham tu utils.R
    if(!exists("convertDate", mode = "function")) {
      source("R/utils.R")
    }

    tmp[,1] <- as.Date(convertDate(tmp[,1]),
                       format = "%Y-%m-%d")

    #check dieu kien de continue
    if(min(tmp[,1]) > to){next}

    #check dieu kien de break
    if(max(tmp[,1]) < from){break}

    #load ham tu utils.R
    if(!exists("subComma", mode = "function")) {
      source("R/utils.R")
    }

    tmp[,c(6,10:12)] <- apply(tmp[,c(6,10:12)],2,
                              subComma)

    #load ham tu utils.R
    if(!exists("convertPercent", mode = "function")) {
      source("R/utils.R")
    }

    tmp[,4] <- convertPercent(tmp[,4])
    tmp[,c(2:3,5:13)] <- suppressWarnings(apply(tmp[,c(2:3,5:13)],
                                                2,as.numeric))

    colnames(tmp)<-cname
    symbolData <- rbind(symbolData,tmp)
  }

  symbolData <- symbolData[,c(1,3:4,7:9,5,2,6,10:13)]
  #symbolData <- data.frame(symbolData, row.names = symbolData[,1])
  #symbolData <- filter(symbolData,and(date >= from, date <= to))
  cat(paste0("#",symbol," from ",from, " to ",to," already cloned "))
  invisible(symbolData)
}

################################## get data cafeF ###############################################


