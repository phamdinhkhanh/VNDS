#' @description
#' Lay gia chung khoan vietNam
#' Ham so nay se ve bieu do tu ngay bat dau den ngay ket thuc
#'
#' @param data du lieu dang data.frame theo kieu ohlc (tuc la gom open, high, low, close price)
#' @param width do rong cua mot nen (nam trong khoang tu 0 den 1)
#' @param colour vector dinh dang mau sac (gom 2 mau dai dien cho tang va giam)
#' @param date_breaks khoang cach giua cac diem tick marker tren truc x
#' @param date_labels dinh dang Date cua tick marker
#' @param title tieu de cua do thi
#' @param xlim gioi han cua truc x, co the la thoi gian c(start,end) hoac quan sat c(startPosition, endPosition)
#' @return mot do thi gia chung khoan
#' @import ggplot2
#' @export
#' @example
# tq_candlechart_khanh_old(data=VND,
#          width=0.9,
#          colour=c('red','darkred'),
#          date_breaks = '2 week',
#          date_labels = '%Y-%m-%d',
#          title="VND",
#          xlim = c("2018-01-10","2018-04-20"))




# tq_candlechart_khanh(VND,colour = c('red','darkred'),
#                      show.volume = TRUE,
#                      title = 'VND Price', xbreak=5,
#                      xformat='%Y %b')


tq_candlechart_khanh_old <- function(data, width, colour,
                        date_breaks = '1 month',
                        date_labels = '%Y-%m-%d',
                        title = "",
                        xlim = c(1,nrow(data)),
                        angle=-45,
                        ...){

  x <- 1:nrow(data)
  vbreaks <- str_split(str_trim(date_breaks)," ") %>% unlist()

  npred <- vbreaks[str_detect(vbreaks,"^[0-9]")]
  pred <- vbreaks[!str_detect(vbreaks,"^[0-9]")]

  xlabels <- as.vector(to.period(x=xts(data$date,
                       order.by = data$date),
                       period = pred, k = npred)[,1])

  xlabels <- as.Date(xlabels)

  xbreaks <- which(data$date %in% xlabels)

  if(!class(xlim) == "numeric" && length(xlim) == 2){
    print(0)
    tryCatch(xlim <- as.Date(xlim),finally = "xlim must be a Numeric or Date vector")
    xlim <- which(data$date %in% xlim)
    if(is.na(xlim[1])){
      xlim[1] <- nrow(data)
    }
    if(is.na(xlim[2])){
      xlim[2] <- 1
    }
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


#tq_candlechart_khanh-------------------------------------------------------------

tq_candlechart_khanh <- function(data, colour = NA, show.volume = TRUE, title = NA, xbreak=10,
                                 xformat=NA,...){
  if(!quantmod::is.OHLC(data)){stop('Data must be a OHLC tible object')}
  if(is.na(colour)){colour <- c('#17BECF','#7F7F7F')}
  if(is.na(title)) {title <-  as.character(substitute(data))}
  
  print(1)
  p_price <- data %>% ggplot(aes(x=date, ymin=low, ymax=high, 
                               lower=pmin(open,close), upper=pmax(open,close), 
                               fill=open>close, group=date, 
                               middle=pmin(open,close))) + 
    
    geom_boxplot(stat='identity') +
    
    scale_fill_manual(labels = c('Increase','Decrease'), 
                      values = colour) +
    
    bdscale::scale_x_bd(business.dates=sort(data$date), 
               max.major.breaks=xbreak, 
               labels=scales::date_format(ifelse(is.na(xformat),"%b %y",xformat))) +
    
    labs(
      x="Date",
      y="Price",
      fill = NULL,
      title = paste(title,": ",min(data$date)," - ",max(data$date))
    ) + 
    
    theme_minimal() 
  
  if(show.volume){
    p_price <- p_price + 
      theme(legend.position='top', 
            legend.direction="horizontal",
            legend.margin = margin(5, 5, 5, 5),
            legend.box.margin=margin(-10,-10,-10,-10),
            plot.title=element_text(size=16, color="darkgreen", hjust = 0.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) 
  } else {
    p_price <- p_price + 
      theme(legend.position='top', 
            legend.direction="horizontal",
            legend.margin = margin(5, 5, 5, 5),
            legend.box.margin=margin(-10,-10,-10,-10),
            plot.title=element_text(size=16, color="darkgreen", hjust = 0.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) 
  }
      
    
  print(2)
  if(quantmod::is.OHLCV(data)) {
  p_volume <- data %>% ggplot(aes(x=date, y=volume, fill=open>close, group=date)) +
    geom_col(show.legend = FALSE) +
    bdscale::scale_x_bd(business.dates=sort(data$date),
               max.major.breaks=xbreak,
               labels=scales::date_format(ifelse(is.na(xformat),"%b %y",xformat)))   +
    scale_fill_manual(labels = c('Increase','Decrease'),
                      values = colour) +
    labs(
      x="Date",
      y="Volume"
    )
  }

  print(3)
  if(show.volume) {
    if(!quantmod::is.OHLCV(data)){ warning('Data miss volume colume')}

    suppressWarnings(gridExtra::grid.arrange
                     (p_price, p_volume, nrow = 2,heights = c(2, 0.7)) )
  } else {
    p_price
  }
}




