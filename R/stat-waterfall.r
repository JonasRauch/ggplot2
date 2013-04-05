#' waterfall statistic
#' 
#' @inheritParams stat_identity
#' @return a data.frame with additional columns:
#'   \item{x}{\code{x} in data}
#'   \item{y}{cumulative values of \code{y}}
#'   \item{ymax}{cumulative values of \code{y}}
#'   \item{ymin}{cumulative value of previous \code{y}}
#'   \item{type}{\code{NA} if \code{is.na(y)}, 'in' if \code{y > 0}, 'out' else}
#'   \item{deltay}{original \code{y}}
#' @export
#' @examples
#'   X <- data.frame(transaction = c("Starting Cash","Sales", "Refunds", "Payouts", "Court Losses","Court Wins", "Contracts"), 
#'              amount = c(3400, -1100, -100, -6600, 3800, 1400, 2800))
#'   X <- transform(X, transaction=factor(transaction, levels=unique(transaction)))
#'   ggplot(X, aes(x=transaction, y=amount)) + stat_waterfall()
stat_waterfall <- function (mapping = NULL, data = NULL, geom = "linerange", position = "identity", ...) {
  StatWaterfall$new(mapping = mapping, data = data, geom = geom, position = position, ...)
}

StatWaterfall <- proto(Stat, {
  objname <- "waterfall"
  
  calculate_groups <- function(., data, ...) {
    if(empty(data)) return(data.frame())
    data <- data[order(data$x),]
    I <- is.na(data$y)
    data$y[I] <- 0
    data <- transform(data, 
      type = factor(ifelse(I, NA_character_, ifelse(y > 0, "in", "out")), levels=c("in", "out")),
      deltay = y,
      ymax = cumsum(y),
      y = cumsum(y),
      ymin = c(0, head(cumsum(y), -1)),
      lastgroup = c(rep(FALSE, nrow(data) - 1), TRUE),
      firstgroup = c(TRUE, rep(FALSE, nrow(data) - 1))
    )
    data$deltay[I] <- NA_real_
    data
  }
  
  default_aes <- function(.) aes(ymin = ..ymin.., ymax=..ymax.., fill=..type..)
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomWaterfall
})
