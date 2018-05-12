#' Bieu dien Financial Charts trong ggplot2
#'
#'
#' @inheritParams geom_ma
#' @inheritParams ggplot2::geom_linerange
#' @param color_up,color_down Select colors to be applied based on price movement
#' from open to close. If close >= open, `color_up` is used. Otherwise,
#' `color_down` is used. The default is "darkblue" and "red", respectively.
#' @param fill_up,fill_down Select fills to be applied based on price movement
#' from open to close. If close >= open, `fill_up` is used. Otherwise,
#' `fill_down` is used. The default is "darkblue" and "red", respectively.
#' Only affects `geom_candlestick`.
#'
#' @section Aesthetics:
#' Cac thanh phan truc duoc giai thich nhu sau:
#' \itemize{
#'    \item \strong{`x`}, truc ngay
#'    \item \strong{`open`}, open price (khong the thieu)
#'    \item \strong{`high`}, high price (khong the thieu)
#'    \item \strong{`low`}, low price (khong the thieu)
#'    \item \strong{`close`}, close price (khong the thieu)
#'    \item `alpha`
#'    \item `group`
#'    \item `linetype`
#'    \item `size`
#' }
#'
#' @seealso do thi duoc ve ra tu cac ham tinh chuoi:
#' \itemize{
#'    \item [geom_ma()] adding moving averages vao ggplots
#'    \item [geom_bbands()] adding Bollinger Bands vao ggplots
#'    \item [coord_x_date()] lay du lieu trong mot khoang thoi gian nao do cua do thi
#' }
#'
#' @name geom_chart
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(VNDS)
#'
#' VND <- tq_get("VND")
#'
#' # Bar Chart
#' VND %>%
#'     ggplot(aes(x = date, y = close)) +
#'     geom_barchart(aes(open = open, high = high, low = low, close = close)) +
#'     geom_ma(color = "darkgreen") +
#'     coord_x_date(xlim = c(today() - weeks(6), today()),
#'                  ylim = c(100, 130))
#'
#' # Candlestick Chart
#' VND %>%
#'     ggplot(aes(x = date, y = close)) +
#'     geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
#'     geom_ma(color = "darkgreen") +
#'     coord_x_date(xlim = c(today() - weeks(6), today()),
#'                  ylim = c(100, 130))
#'                  
#Bar chart-------------------------------------------------------------------------------
#' @rdname geom_chart
#' @export

geom_barchart <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = TRUE, show.legend = NA,
                          inherit.aes = TRUE,
                          color_up = "darkblue", color_down = "red",
                          fill_up = "darkblue", fill_down = "red",
                          ...) {
  
  
  
  linerange <- ggplot2::layer(
    stat = StatLinerangeBC, geom = GeomLinerangeBC, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fill_up = fill_up, fill_down = fill_down,
                  color_up = color_up, color_down = color_down, ...)
  )
  
  segment_left <- ggplot2::layer(
    stat = StatSegmentLeftBC, geom = GeomSegmentBC, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fill_up = fill_up, fill_down = fill_down,
                  color_up = color_up, color_down = color_down, ...)
  )
  
  segment_right <- ggplot2::layer(
    stat = StatSegmentRightBC, geom = GeomSegmentBC, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fill_up = fill_up, fill_down = fill_down,
                  color_up = color_up, color_down = color_down, ...)
  )
  
  list(linerange, segment_left, segment_right)
}

StatLinerangeBC <- ggplot2::ggproto("StatLinerangeBC", Stat,
                                    required_aes = c("x", "open", "high", "low", "close"),
                                    
                                    compute_group = function(data, scales, params,
                                                             fill_up, fill_down,
                                                             color_up, color_down) {
                                      
                                      data <-  data %>%
                                        dplyr::mutate(color = ifelse(open < close, color_up, color_down))
                                      
                                      tibble::tibble(x = data$x,
                                                     ymin = data$low,
                                                     ymax = data$high,
                                                     colour = data$color)
                                    }
)

#Tao mot ggproto ke thua tu Stat
StatSegmentLeftBC <- ggplot2::ggproto("StatSegmentLeftBC", Stat,
                                      required_aes = c("x", "open", "high", "low", "close"),
                                      
                                      compute_group = function(data, scales, params,
                                                               fill_up, fill_down,
                                                               color_up, color_down) {
                                        
                                        data <-  data %>%
                                          dplyr::mutate(color = ifelse(open < close, color_up, color_down))
                                        
                                        tibble::tibble(x    = data$x,
                                                       xend = data$x - 0.5,
                                                       y    = data$open,
                                                       yend = data$open,
                                                       colour = data$color)
                                      }
)


StatSegmentRightBC <- ggplot2::ggproto("StatSegmentRightBC", Stat,
                                       required_aes = c("x", "open", "high", "low", "close"),
                                       
                                       compute_group = function(data, scales, params,
                                                                fill_up, fill_down,
                                                                color_up, color_down) {
                                         
                                         data <-  data %>%
                                           dplyr::mutate(color = ifelse(open < close, color_up, color_down))
                                         
                                         tibble::tibble(x    = data$x,
                                                        xend = data$x + 0.5,
                                                        y    = data$close,
                                                        yend = data$close,
                                                        colour = data$color)
                                       }
)

#Tao mot ggproto GeomLinerangeBC ke thua tu GeomLinerange.
GeomLinerangeBC <- ggproto("GeomLinerangeBC", GeomLinerange,
                           default_aes = aes(size = 0.5,
                                             linetype = 1,
                                             alpha = NA)
)

#Tao mot ggproto GeomSegmentBC ke thua tu GeomSegment.
GeomSegmentBC <- ggproto("GeomSegmentBC", GeomSegment,
                         default_aes = aes(size = 0.5,
                                           linetype = 1,
                                           alpha = NA)
)


# Candlestick Chart ---------------------------------------------------------------------------------
#' @rdname geom_chart
#' @export

geom_candlestick <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", na.rm = TRUE, show.legend = NA,
                             inherit.aes = TRUE,
                             color_up = "darkblue", color_down = "red",
                             fill_up = "darkblue", fill_down = "red",
                             ...) {
  #Tao mot layer co ggproto = StatLinerangeBC
  #ggproto la object co chuc nang render du lieu vao do thi
  linerange <- ggplot2::layer(
    stat = StatLinerangeBC, geom = GeomLinerangeBC, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fill_up = fill_up, fill_down = fill_down,
                  color_up = color_up, color_down = color_down, ...)
  )
  
  rect <- ggplot2::layer(
    stat = StatRectCS, geom = GeomRectCS, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fill_up = fill_up, fill_down = fill_down,
                  color_up = color_up, color_down = color_down, ...)
  )
  
  
  
  list(linerange, rect)
}

StatRectCS <- ggplot2::ggproto("StatRectCS", Stat,
                               required_aes = c("x", "open", "high", "low", "close"),
                               
                               compute_group = function(data, scales, params,
                                                        fill_up, fill_down,
                                                        color_up, color_down) {
                                 
                                 data <-  data %>%
                                   dplyr::mutate(fill = ifelse(open < close, fill_up, fill_down),
                                                 ymin = ifelse(open < close, open, close),
                                                 ymax = ifelse(open < close, close, open))
                                 
                                 tibble::tibble(xmin = data$x - 0.45,
                                                xmax = data$x + 0.45,
                                                ymin = data$ymin,
                                                ymax = data$ymax,
                                                fill = data$fill)
                               }
)





GeomRectCS <- ggproto("GeomRectCS", GeomRect,
                      default_aes = aes(colour = NA,
                                        size = 0.5,
                                        linetype = 1,
                                        alpha = NA)
)

