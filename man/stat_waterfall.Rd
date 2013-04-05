\name{stat_waterfall}
\alias{stat_waterfall}
\title{waterfall statistic}
\usage{
  stat_waterfall(mapping = NULL, data = NULL,
    geom = "linerange", position = "identity", ...)
}
\arguments{
  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link{aes}} or \code{\link{aes_string}}. Only
  needs to be set at the layer level if you are overriding
  the plot defaults.}

  \item{data}{A layer specific dataset - only needed if you
  want to override the plot defaults.}

  \item{geom}{The geometric object to use display the data}

  \item{position}{The position adjustment to use for
  overlappling points on this layer}

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}
}
\value{
  a data.frame with additional columns: \item{x}{\code{x}
  in data} \item{y}{cumulative values of \code{y}}
  \item{ymax}{cumulative values of \code{y}}
  \item{ymin}{cumulative value of previous \code{y}}
  \item{type}{\code{NA} if \code{is.na(y)}, 'in' if \code{y
  > 0}, 'out' else} \item{deltay}{original \code{y}}
}
\description{
  waterfall statistic
}
\examples{
X <- data.frame(transaction = c("Starting Cash","Sales", "Refunds", "Payouts", "Court Losses","Court Wins", "Contracts"),
             amount = c(3400, -1100, -100, -6600, 3800, 1400, 2800))
  X <- transform(X, transaction=factor(transaction, levels=unique(transaction)))
  ggplot(X, aes(x=transaction, y=amount)) + stat_waterfall()
}

