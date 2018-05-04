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

tq_candlechart <- function(symbol, from, to, colour, show.volume = TRUE,...){
  # create dataframe
  if(quantmod::is.OHLC(symbol)){
    df <- symbol
    title <- base::substitute(symbol)
  } else {
    df <- VNDS::tq_get(symbol,from,to)
  }
  # create Bollinger Bands
  bbands <- TTR::BBands(df[,c("high","low","close")])

  # join data
  df <- cbind(df, data.frame(bbands[,1:3]))

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

  # plot candlestick chart
  p <- df %>%
    plot_ly(x = ~date, type="candlestick",
            open = ~open, close = ~close,
            high = ~high, low = ~low, name = title,
            increasing = i, decreasing = d) %>%
    add_lines(x = ~date, y = ~up , name = "B Bands",
              line = list(color = '#ccc', width = 0.5),
              legendgroup = "Bollinger Bands",
              hoverinfo = "none", inherit = F) %>%
    add_lines(x = ~date, y = ~dn, name = "B Bands",
              line = list(color = '#ccc', width = 0.5),
              legendgroup = "Bollinger Bands", inherit = F,
              showlegend = FALSE, hoverinfo = "none") %>%
    add_lines(x = ~date, y = ~mavg, name = "Mv Avg",
              line = list(color = '#E377C2', width = 0.5),
              hoverinfo = "none", inherit = F) %>%
    layout(yaxis = list(title = "Price"))

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
    pp <- df %>%
      plot_ly(x=~date, y=~volume, type='bar', name = paste0(title," Volume"),
              color = ~direction, colors = colour) %>%
      layout(yaxis = list(title = "Volume"))

    # subplot with shared x axis
    (p <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
                  shareX = TRUE, titleY = TRUE) %>%
        layout(title = paste(title,": ",min(df$date)," - ",max(df$date)),
               xaxis = list(rangeselector = rs),
               legend = list(orientation = 'h', x = 0.5, y = 1,
                             xanchor = 'center', yref = 'paper',
                             font = list(size = 10),
                             bgcolor = 'transparent'))) }
  else {
    (p <- p %>%
       layout(title = paste(title,": ",min(df$date)," - ",max(df$date)),
              xaxis = list(rangeselector = rs),
              legend = list(orientation = 'h', x = 0.5, y = 1,
                            xanchor = 'center', yref = 'paper',
                            font = list(size = 10),
                            bgcolor = 'transparent')))}
}

