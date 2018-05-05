#' @description
#' Lay gia chung khoan vietNam
#' Ham so nay se ve bieu do tu ngay bat dau den ngay ket thuc
#'
#' @param symbol du lieu dang data.frame theo kieu ohlc (tuc la gom open, high, low, close price) hoac ten ma chung khoan
#' @param from thoi gian bat dau
#' @param to thoi gian ket thuc
#' @param colour mau sac cua than nen (gom mau tang va mau giam)
#' @return mot do thi gia chung khoan
#' @export
#' @example
# tq_candlechart('VND','2018-01-01','2018-05-01',colour = c('red','darkred'),show.volume = FALSE)

globalVariables(c(".","%<>%","c",".I",".N"))


#Ket hop cua tq_candlechart_ohlc() va tq_candlechart_symbol()---------------------------------
#Tu dong nhan biet tham so truyen vao la OHLC object hay symbol de visualize

# tq_candlechart <- function(symbol, from, to, 
#                            colour=c('#17BECF','#7F7F7F'), 
#                            show.volume = TRUE,...){
#   # create dataframe
#   if(quantmod::is.OHLC(symbol)){
#     df <- symbol
#     title <- as.character(substitute(symbol))
#   } else {
#     stopifnot(is.character(symbol))
#       df <- tq_get(symbol,from,to)
#       title <- symbol
#   }
#   
#   # create Bollinger Bands
#   bbands <- TTR::BBands(df[,c("high","low","close")])
#   
#   # join data
#   df <- cbind(df, data.frame(bbands[,1:3]))
#   print(1)
#   # colors column for increasing and decreasing
#   for (i in 1:length(df[,1])) {
#     if (df$close[i] >= df$open[i]) {
#       df$direction[i] = 'Increasing'
#     } else {
#       df$direction[i] = 'Decreasing'
#     }
#   }
#   
#   i <- list(line = list(color = colour[1]))
#   d <- list(line = list(color = colour[2]))
#   print(2)
#   # plot candlestick chart
#   p <- df %>%
#     plotly::plot_ly(x = ~date, type="candlestick",
#                     open = ~open, close = ~close,
#                     high = ~high, low = ~low, name = title,
#                     increasing = i, decreasing = d) %>%
#     plotly::add_lines(x = ~date, y = ~up , name = "B Bands",
#                       line = list(color = '#ccc', width = 0.5),
#                       legendgroup = "Bollinger Bands",
#                       hoverinfo = "none", inherit = F) %>%
#     plotly::add_lines(x = ~date, y = ~dn, name = "B Bands",
#                       line = list(color = '#ccc', width = 0.5),
#                       legendgroup = "Bollinger Bands", inherit = F,
#                       showlegend = FALSE, hoverinfo = "none") %>%
#     plotly::add_lines(x = ~date, y = ~mavg, name = "Mv Avg",
#                       line = list(color = '#E377C2', width = 0.5),
#                       hoverinfo = "none", inherit = F) %>%
#     plotly::layout(yaxis = list(title = "Price"))
#   print(3)
#   # create rangeselector buttons
#   rs <- list(visible = TRUE, x = 0.5, y = -0.055,
#              xanchor = 'center', yref = 'paper',
#              font = list(size = 9),
#              buttons = list(
#                list(count=1,
#                     label='RESET',
#                     step='all'),
#                list(count=1,
#                     label='1 YR',
#                     step='year',
#                     stepmode='backward'),
#                list(count=3,
#                     label='3 MO',
#                     step='month',
#                     stepmode='backward'),
#                list(count=1,
#                     label='1 MO',
#                     step='month',
#                     stepmode='backward')
#              ))
#   
#   show.volume <- ifelse(is.OHLCV(df),TRUE,FALSE)
#   if(show.volume){
#     # plot volume bar chart
#     print(4)
#     pp <- df %>%
#       plotly::plot_ly(x=~date, y=~volume, type='bar', name = paste0(title," Volume"),
#                       color = ~direction, colors = colour) %>%
#       plotly::layout(yaxis = list(title = "Volume"))
#     print(5)
#     # subplot with shared x axis
#     (p <- plotly::subplot(p, pp, heights = c(0.7,0.2), nrows=2,
#                           shareX = TRUE, titleY = TRUE) %>%
#         plotly::layout(title = paste(title,": ",min(df$date)," - ",max(df$date)),
#                        xaxis = list(rangeselector = rs),
#                        legend = list(orientation = 'h', x = 0.5, y = 1,
#                                      xanchor = 'center', yref = 'paper',
#                                      font = list(size = 10),
#                                      bgcolor = 'transparent'))) }
#   else {
#     (p <- p %>%
#        plotly::layout(title = paste(title,": ",min(df$date)," - ",max(df$date)),
#                       xaxis = list(rangeselector = rs),
#                       legend = list(orientation = 'h', x = 0.5, y = 1,
#                                     xanchor = 'center', yref = 'paper',
#                                     font = list(size = 10),
#                                     bgcolor = 'transparent')))}
# }

tq_candlechart <- function(symbol, from, to, 
                        colour=c('#17BECF','#7F7F7F'),
                        show.volume = TRUE,
                        title = "",...){
# create dataframe
  if(quantmod::is.OHLC(symbol)){
   df <- symbol
   title <- as.character(substitute(df))
  } else {
   stopifnot(is.character(symbol))
     df <- tq_get(symbol,from,to)
     title <- symbol
  }
  tq_candlechart_ohlc(df,colour,show.volume,title)
}


#Ve bieu do tu nguon internet thong qua ma chung khoan truyen vao ----------------------------

tq_candlechart_symbol <- function(symbol, from, to, 
                           colour=c('#17BECF','#7F7F7F'), 
                           show.volume = TRUE,
                           title = "",...){
  # create dataframe
  df <- tq_get(symbol,from,to)
  title <- symbol
  tq_candlechart_ohlc(df,colour,show.volume, title)
}



#Ve bieu do tu mot OHLC object--------------------------------------------------------------

tq_candlechart_ohlc <- function(df, 
                              colour=c('#17BECF','#7F7F7F'), 
                              show.volume = TRUE,
                              title = "",...){
  stopifnot(quantmod::is.OHLC(df))
  # Symbol
  title <- as.character(substitute(df))
  # create Bollinger Bands
  bbands <- TTR::BBands(df[,c("high","low","close")])
  
  # join data
  df <- cbind(df, data.frame(bbands[,1:3]))
  print(1)
  # colors column for increasing and decreasing
  for (i in 1:length(df[,1])) {
    if (df$close[i] >= df$open[i]) {
      df$direction[i] = 'Increasing'
    } else {
      df$direction[i] = 'Decreasing'
    }
  }
  
  i <- list(line = list(color = colour[1]))
  d <- list(line = list(color = colour[2]))
  print(2)
  # plot candlestick chart
  p <- df %>%
    plotly::plot_ly(x = ~date, type="candlestick",
                    open = ~open, close = ~close,
                    high = ~high, low = ~low, name = title,
                    increasing = i, decreasing = d) %>%
    plotly::add_lines(x = ~date, y = ~up , name = "B Bands",
                      line = list(color = '#ccc', width = 0.5),
                      legendgroup = "Bollinger Bands",
                      hoverinfo = "none", inherit = F) %>%
    plotly::add_lines(x = ~date, y = ~dn, name = "B Bands",
                      line = list(color = '#ccc', width = 0.5),
                      legendgroup = "Bollinger Bands", inherit = F,
                      showlegend = FALSE, hoverinfo = "none") %>%
    plotly::add_lines(x = ~date, y = ~mavg, name = "Mv Avg",
                      line = list(color = '#E377C2', width = 0.5),
                      hoverinfo = "none", inherit = F) %>%
    plotly::layout(yaxis = list(title = "Price"))
  print(3)
  # create rangeselector buttons
  rs <- list(visible = TRUE, x = 0.5, y = -0.055,
             xanchor = 'center', yref = 'paper',
             font = list(size = 9),
             buttons = list(
               list(count=1,
                    label='RESET',
                    step='all'),
               list(count=1,
                    label='1 YR',
                    step='year',
                    stepmode='backward'),
               list(count=3,
                    label='3 MO',
                    step='month',
                    stepmode='backward'),
               list(count=1,
                    label='1 MO',
                    step='month',
                    stepmode='backward')
             ))
  
  show.volume <- ifelse(is.OHLCV(df),TRUE,FALSE)
  if(show.volume){
    # plot volume bar chart
    print(4)
    pp <- df %>%
      plotly::plot_ly(x=~date, y=~volume, type='bar', name = paste0(title," Volume"),
                      color = ~direction, colors = colour) %>%
      plotly::layout(yaxis = list(title = "Volume"))
    print(5)
    # subplot with shared x axis
    (p <- plotly::subplot(p, pp, heights = c(0.7,0.2), nrows=2,
                          shareX = TRUE, titleY = TRUE) %>%
        plotly::layout(title = paste(title,": ",min(df$date)," - ",max(df$date)),
                       xaxis = list(rangeselector = rs),
                       legend = list(orientation = 'h', x = 0.5, y = 1,
                                     xanchor = 'center', yref = 'paper',
                                     font = list(size = 10),
                                     bgcolor = 'transparent'))) }
  else {
    (p <- p %>%
       plotly::layout(title = paste(title,": ",min(df$date)," - ",max(df$date)),
                      xaxis = list(rangeselector = rs),
                      legend = list(orientation = 'h', x = 0.5, y = 1,
                                    xanchor = 'center', yref = 'paper',
                                    font = list(size = 10),
                                    bgcolor = 'transparent')))}
}

