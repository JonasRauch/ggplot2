#' waterfall chart
#'
#' @section Aesthetics: 
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "waterfall")}
#'
#' @seealso \code{\link{geom_rect}}: rectangles;
#'   \code{\link{stat_waterfall}}: waterfall chart statistics
#' @export
#' @inheritParams geom_rect
#' @param add_labels Logical indicating wether text labels should be placed
#' @param format function to format text labels
#' @examples
#'   X <- data.frame(transaction = c("Starting Cash","Sales", "Refunds", "Payouts", "Court Losses","Court Wins", "Contracts"), 
#'              amount = c(3400, -1100, -100, -6600, 3800, 1400, 2800))
#'   X <- transform(X, transaction=factor(transaction, levels=unique(transaction)))
#'   ggplot(X, aes(x=transaction, y=amount)) + stat_waterfall()
geom_waterfall <- function (mapping = NULL, data = NULL, stat = "waterfall", position = "identity", add_labels = FALSE, format = identity, ...) { 
  GeomWaterfall$new(mapping = mapping, data = data, stat = stat, position = position, add_labels = add_labels, format = format, ...)
}

GeomWaterfall <- proto(Geom, {
  objname <- "waterfall"
  
  default_stat <- function(.) StatWaterfall
  default_aes <- function(.) aes(width = 0.9, colour=NA, size=0.5, linetype=1, alpha = NA, 
                                 text_colour="black", text_size=5 , angle=0, hjust=0.5,
                                 vjust=0, family="", fontface=1, lineheight=1.2, segment_colour="black", net_fontface = "bold")

  guide_geom <- function(.) "polygon"
  required_aes <- c("x", "ymin", "ymax")
  
  draw <- function(., data, scales, coordinates, add_labels = FALSE, format = identity, ...) {
    dataR <- transform(data, xmin = x - width / 2, xmax = x + width / 2)
    gR <- GeomRect$draw(dataR, scales, coordinates)
    dataS1 <- transform(dataR, x = xmin, xend = xmax + !lastgroup, yend = y, colour = ifelse(lastgroup, NA, segment_colour))
    dataS2 <- transform(dataR, x = xmin, xend = xmax, y = ymin, yend = ymin, colour = ifelse(firstgroup, NA, segment_colour))
    gS <- GeomSegment$draw(rbind(dataS1, dataS2), scales, coordinates)
    dataT <- transform(data, label=format(ifelse(is.na(deltay), y, deltay)), colour=text_colour, size=text_size, fontface=ifelse(is.na(deltay), net_fontface, fontface), vjust = ifelse(type=="out" &! is.na(type), 1.2 + vjust, - vjust - 0.3))
    gT <- GeomText$draw(dataT, scales, coordinates)
    gList(gR, gS, gT)
  }
})
