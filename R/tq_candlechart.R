#' @description
#' Lay gia chung khoan vietNam
#' Ham so nay se lay gia chung khoan tu ngay bat dau den ngay ket thuc
#'
#' @param data du lieu dang data.frame theo kieu ohlc (tuc la gom open, high, low, close price)
#' @param width do rong cua mot nen (nam trong khoang tu 0 den 1)
#' @param colour vector dinh dang mau sac (gom 2 mau dai dien cho tang va giam)
#' @param date_breaks khoang cach giua cac diem tick marker tren truc x
#' @param date_labels dinh dang Date cua tick marker
#' @param title tieu de cua do thi
#' @param xlim gioi han cua truc x, co the la thoi gian c(start,end) hoac quan sat c(startPosition, endPosition)
#' @return mot data frame chua gia chung khoan va khoi luong
#' @export
#' @example
#   tq_candlechart(data=VND,
#            width=0.9,
#            colour=c('red','darkred'),
#            date_breaks = '2 week',
#            date_labels = '%Y-%m-%d',
#            title="VND",
#            xlim = c("2018-01-10","2018-04-20"))


tq_candlechart <- function(data, width, colour,
                        date_breaks = '1 month',
                        date_labels = '%Y-%m-%d',
                        title = "",
                        xlim = c(1,nrow(data)),
                        angle=-45,
                        ...){

  x <- index(data)
  vbreaks <- str_split(str_trim(date_breaks)," ") %>% unlist()

  npred <- vbreaks[str_detect(vbreaks,"^[0-9]")]
  pred <- vbreaks[!str_detect(vbreaks,"^[0-9]")]

  xlabels <- as.vector(to.period(x=xts(data$date,
                       order.by = data$date),
                       period = pred, k = npred)[,1])

  xlabels <- as.Date(xlabels)

  xbreaks <- which(data$date %in% xlabels)

  if(!class(xlim) == "numeric" && length(xlim) == 2){
    tryCatch(xlim <- as.Date(xlim),finally = "xlim must be a Numeric or Date vector")
    xlim <- which(data$date %in% xlim)
    xlim <- nrow(data)-c(max(xlim),min(xlim))
  }


  p <- ggplot(data, aes(x)) +
    geom_linerange(aes(ymin = low, ymax = high)) +
    geom_rect(aes(xmin = order(x) - 1/2*width,
                  xmax = order(x) + 1/2*width,
                  ymin = open,
                  ymax = close,
                  fill = ifelse(close >= open, "down","up"))) +
    guides(fill = FALSE,
           colour = FALSE) +

    scale_fill_manual(labels = c("up","down"),
                        values = colour)  +

    scale_x_continuous(breaks = xbreaks,
                       labels = format(xlabels,date_labels),
                       limits = xlim) +
    labs(
      x="Date",
      y="Price",
      title=title
    ) +

    theme(axis.text.x=element_text(angle=angle, hjust=0.5, vjust=0.5))


  if(any(data$open == data$close)) {
    xadjust <- which(data$open == data$close)
    p <- p + geom_segment(data = subset(data, open == close),
                          aes(x = order(xadjust) - 1/2*width,
                              xend = order(xadjust) + 1/2*width,
                              y = close,
                              yend = close))
  }

  p
}


