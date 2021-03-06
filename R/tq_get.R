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
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# Support query pretty much data -------------------------------------------------------------------

tq_bigquery <- function(symbols,
                        from,
                        to,
                        src="VND",
                        minimal = TRUE,
                        stack = FALSE,
                        return.type = 'tibble',n=5,timeout=2,...){
  ev <- new.env()
  loop <- floor(length(symbols)/n)
  odd <-  ifelse(length(symbols) - n*loop == 0, n,length(symbols) - n*loop)
  ls_total <- list()

  if(loop == 0){
    end <- odd
    start <- 1
    iterSymbols <- symbols[start:end]

    print(cat(start,'-',end,' : ',iterSymbols))

    assign('ls',
           tq_get(iterSymbols,from,to,src,minimal,stack,return.type),ev)
    ls_symbol <- get('ls',ev)

    for (j in start:end){
      k = ifelse(j %% n == 0, n, j %% n)
      ls_total[[j]] <- ls_symbol[[k]]
    }
  }

  if(loop != 0){
    #loop = 1, odd = 1
    for(i in 1:(loop+1)){
      if(i != (loop+1)){
        iterSymbols <- symbols[(n*(i-1)+1):(n*i)]
      } else {
        #i = loop+1
        if(odd == n){
          break
        } else {
          iterSymbols <- symbols[(n*(i-1)+1):(n*(i-1)+odd)]
        }
      }

      v_match <- match(iterSymbols, symbols)
      print(cat(v_match[1],'-',v_match[length(v_match)],' : ',iterSymbols))

      assign('ls',
             tq_get(iterSymbols,from,to,src,minimal,stack,return.type),ev)
      ls_symbol <- get('ls',ev)


      end <- ifelse(i == (loop+1), (n*(i-1)+odd), (n*i))
      start <- (n*(i-1)+1)
      for (j in start:end){
        k = ifelse(j %% n == 0, n, j %% n)
        ls_total[[j]] <- ls_symbol[[k]]
      }
      if(i != (loop+1) & !(odd == n & i == loop)){
        Sys.sleep(timeout)
      }
    }
  }

  names(ls_total) <- symbols
  ls_total
}


# Return multiple or single data -------------------------------------------------------------------
tq_get <-
  function(symbols,
           from,
           to,
           src="VND",
           minimal = TRUE,
           stack = FALSE,
           return.type='tibble',...){
  stopifnot(is.vector(symbols))
  if(length(symbols) == 1){
    tq_get_single(symbols,from,to,src,minimal,return.type)
  } else {

    ev <- new.env()
    for (symbol in symbols){
      assign(paste0(symbol),
             #data.frame(tq_get_single(symbol,from,to,src,minimal,return.type)),
             tq_get_single(symbol,from,to,src,minimal,return.type),
             envir = ev)
    }

    if(stack){
      stk_return <- data.frame()
      for(symbol in symbols){
        df <- data.frame(symbol = symbol, get(symbol, envir = ev))
        stk_return <- rbind(stk_return,df)
      }
      tibble::as_tibble(stk_return)
    } else {
      ls <- list()
      i <- 0
      for (symbol in symbols){
        i <- i + 1
        ls[[i]] <- get(symbol, envir = ev)
      }
      names(ls) <- symbols
      ls
    }
  }
}

#tq_get(c('VND','VPB'),'2018-01-01','2018-03-01') -> ls

#ls$VND %>% tibble::as_tibble() %>% tq_candlechart()
#ls$VND %>% as.matrix() %>% tibble::as_tibble()
#ls$VND %>% View()
# symbols = c('VND','VPB')
# from = '2018-01-01'
# to = '2018-02-01'
# src = 'VND'
# minimal = TRUE

# Export data as xts object ----------------------------------------------------------------
tq_get_xts <-
  function(symbols,
           from,
           to,
           src = 'VND',
           minimal = TRUE,...){
  stopifnot(is.vector(symbols))
  return.type <- 'xts'
  if(length(symbols) == 1){
    tq_get_single(symbols, from, to, src, minimal, return.type)
  } else {
    list <- list()
    i <- 0
    for(symbol in symbols){
      i <- i+1
      ls[[i]]  <- tq_get_single(symbol, from, to, src, minimal, return.type)
    }
    names(ls) <- symbols
    ls
  }
}


# Export data as quantmod::getSymbols-----------------------------------------------------
tq_getSymbols <-
  function(symbols,
           from,
           to,
           src="VND",
           minimal = TRUE,
           return.type = 'tibble',...){
  for (symbol in unique(symbols)){
    assign(paste0(symbol),
      lsSymbols <- tq_get(symbol,from,to,src,minimal, return.type),
      envir = .GlobalEnv)
  }
}


# Get 1 symbol -----------------------------------------------------------------------------
tq_get_single <-
  function(symbol,
           from,
           to,
           src="VND",
           minimal = TRUE,
           return.type = 'tibble',
           ...){
    switch(return.type,
      tibble = tq_get_single_tibble(symbol,from,to,src,minimal),
      xts = tq_get_single_tibble(symbol, from, to, src, minimal) %>%
        xts(order.by=.$date)
      )
  }

# Get 1 symbol df-----------------------------------------------------------------------------
tq_get_single_tibble <-
  function(symbol,
           from,
           to,
           src="VND",
           minimal = TRUE,
           ...){

  colname<- c("date",
              "open",
              "high",
              "low",
              "close",
              "volume",
              "adjusted")
  if (missing(to)){to <- as.character(Sys.Date()-1)}
  if (missing(from)){from <- as.character(Sys.Date()-366)}

  switch(src,
         VND = tq_get_vnd(symbol, from, to),
         CP68 = tq_get_cp68(symbol, from, to),
         CAFEF = tq_get_cafef(symbol, from, to)) -> extractData

  if(is.character(extractData))
    stop(cat(paste0('#There are not exist symbol ',symbol,'\n')))

  extractData <- extractData %>%
    mutate(volume = match.volume + reconcile.volume)

  if(src == 'CP68')
  extractData <- extractData %>%
      mutate(adjusted = (high + low)/2)

  switch(as.character(minimal),
         'TRUE' = extractData[,colname],
         'FALSE' = extractData) -> extractData

  invisible(extractData)
}


################################## get data VNDirect ###############################################
tq_get_vnd <- function(symbol, from, to,...){
  url <- "https://www.vndirect.com.vn/portal/thong-ke-thi-truong-chung-khoan/lich-su-gia.shtml"
  #lay page cuoi cung
  lastPage <- getLastPage(url,symbol,from,to)
  if(lastPage == 0){
    warning(paste0('There are not exist symbol ',symbol))
  } else {
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
    symbolData <- tibble::as_tibble(symbolData, validate = FALSE)
    cat(paste0("#",symbol," from ",from, " to ",to," already cloned \n"))
    invisible(symbolData)
  }
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

  lastPage <- if(df[1]=="" & length(df)==1){
    lastPage <- 0
  } else {
    lastPage <- ifelse(is.na(df[3]),1,as.numeric(df[3]))}
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
  url<-"http://www.cophieu68.vn/historyprice.php"

  #lay cac trang cuoi cung
  resp <- POST(url,
               body=list(id=symbol),
               endcode="form")

  resp %>% read_html() %>%
    html_nodes(xpath='//*[@id="navigator"]/li[7]/a') %>%
    html_attrs() -> tmp

  if(length(tmp) == 0){
    warning(paste0('There are not exist symbol \n',symbol))
  } else {
    lastPage <- as.numeric(str_extract(tmp[1],"[0-9]+"))

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

      tmp[,c(6,10:12)] <- lapply(tmp[,c(6,10:12)],
                                subComma)

      #load ham tu utils.R
      if(!exists("convertPercent", mode = "function")) {
        source("R/utils.R")
      }

      tmp[,4] <- convertPercent(tmp[,4])
      tmp[,c(2:3,5:13)] <- suppressWarnings(lapply(tmp[,c(2:3,5:13)],
                                                  as.numeric))

      colnames(tmp)<-cname
      symbolData <- rbind(symbolData,tmp)
    }

    symbolData <- symbolData[,c(1,3:4,7:9,5,2,6,10:13)]
    symbolData <- data.frame(symbolData, row.names = symbolData[,1])
    symbolData <- subset(symbolData,date >= from & date <= to)
    symbolData <- tibble::as_tibble(symbolData, validate = FALSE)
    cat(paste0("#",symbol," from ",from, " to ",to," already cloned \n"))
    invisible(symbolData)
  }
}

################################## get data cafeF ###############################################

#tq_get_cafef('VND','2018-01-01','2018-05-25') -> VND

tq_get_cafef <- function (symbol,from,to,...){
  url <- paste0 ("http://s.cafef.vn/Lich-su-giao-dich-", symbol, "-1.chn")

  #khoi tao matrix
  cname1 <- c(
    "date",
    "adjusted",
    "close",
    "change.percent",
    "match.volume",
    "match.value",
    "reconcile.volume",
    "reconcile.value",
    "open",
    "high",
    "low",
    "volume.round1",
    "volume.round2",
    "volume.round3"
  )

  cname2 <- c(
    "date",
    "adjusted",
    "close",
    "average",
    "change.percent",
    "match.volume",
    "match.value",
    "reconcile.volume",
    "reconcile.value",
    "reference",
    "open",
    "high",
    "low"
  )

  symbolData <- matrix(nrow = 0,
                       ncol = 14,
                       byrow = TRUE,
                       dimnames = list(c(),cname1))
  #Check table_id
  body <- list('ctl00$ContentPlaceHolder1$scriptmanager' = 'ctl00$ContentPlaceHolder1$ctl03$panelAjax|ctl00$ContentPlaceHolder1$ctl03$pager2',
               'ctl00$ContentPlaceHolder1$ctl03$txtKeyword' = symbol,
               'ctl00$ContentPlaceHolder1$ctl03$dpkTradeDate1$txtDatePicker' = dateChar(from),
               'ctl00$ContentPlaceHolder1$ctl03$dpkTradeDate2$txtDatePicker' = dateChar(to),
               '__EVENTTARGET' = 'ctl00$ContentPlaceHolder1$ctl03$pager2',
               '__EVENTARGUMENT' = 1,
               '__ASYNCPOST' = 'true')

  resp <- POST(url,
               user_agent('Mozilla'),
               body = body,
               endcode = "form")

  resp %>% read_html() %>%
    html_nodes(xpath='//*[@id="GirdTable2"]') %>%
    html_table(fill = TRUE) %>%
    as.data.frame() -> tmp

  if(nrow(tmp) != 0){
    table_id <- 'GirdTable2'
  } else {
    table_id <- 'GirdTable'
  }


  #tao vong lap lay du lieu
  for(i in 1:10000){
    body <- list('ctl00$ContentPlaceHolder1$scriptmanager' = 'ctl00$ContentPlaceHolder1$ctl03$panelAjax|ctl00$ContentPlaceHolder1$ctl03$pager2',
                 'ctl00$ContentPlaceHolder1$ctl03$txtKeyword' = symbol,
                 'ctl00$ContentPlaceHolder1$ctl03$dpkTradeDate1$txtDatePicker' = dateChar(from),
                 'ctl00$ContentPlaceHolder1$ctl03$dpkTradeDate2$txtDatePicker' = dateChar(to),
                 '__EVENTTARGET' = 'ctl00$ContentPlaceHolder1$ctl03$pager2',
                 '__EVENTARGUMENT' = i,
                 '__ASYNCPOST' = 'true')


    resp <- POST(url,
                 user_agent('Mozilla'),
                 body = body,
                 endcode = "form")

    resp %>% read_html() %>%
      html_nodes(xpath = glue::glue('//*[@id="{table_id}"]')) %>%
      html_table(fill = TRUE) %>%
      as.data.frame() -> tmp

    if(nrow(tmp) == 2) {break}

    if(table_id == 'GirdTable2') {
      if(ncol(tmp) == 14)
        tmp <- data.frame(X1 = tmp[-c(1:2), c(1)], reference = NA, tmp[-c(1:2), -c(1,4)])
      if(ncol(tmp) == 15)
        tmp <- tmp[-c(1:2), c(-5)]
    } else {
      if(ncol(tmp) == 13){
        tmp <- data.frame(tmp[-c(1:2), c(1:3)], average = NA, tmp[-c(1:2), c(4:13)])
      } else {
        tmp <- tmp[-c(1:2),]
      }
    }

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

    symbolData <- rbind(symbolData,tmp)
  }

  if(table_id == 'GirdTable2'){
    colnames(symbolData) <- cname1
    symbolData <- data.frame(symbolData, row.names = symbolData[,1])
    symbolData <- symbolData[,c(1,4,9:11,3,2,5,7,6,8,12:14)]
    symbolData[,3:14] <- lapply(symbolData[,3:14], subComma)
    symbolData[,3:14] <- lapply(symbolData[,3:14], as.numeric)
  } else {
    symbolData <- symbolData[,-6]
    colnames(symbolData) <- cname2
    symbolData <- data.frame(symbolData, row.names = symbolData[,1])
    symbolData[,c(6:9)] <- lapply(symbolData[,c(6:9)],subComma)
    symbolData[,c(2:4,6:13)] <- lapply(symbolData[,c(2:4,6:13)], as.numeric)
    symbolData <- symbolData[,c(1,5,11:13,3:4,2,10,6,8,7,9)]
  }

  symbolData <- subset(symbolData,date >= from & date <= to)
  symbolData <- tibble::as_tibble(symbolData, validate = FALSE)
  cat(paste0("#",symbol," from ",from, " to ",to," already cloned \n"))
  invisible(symbolData)
}

