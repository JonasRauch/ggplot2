\name{geom_waterfall}
\alias{geom_waterfall}
\title{waterfall chart}
\usage{
  geom_waterfall(mapping = NULL, data = NULL,
    stat = "waterfall", position = "identity",
    add_labels = FALSE, format = identity, ...)
}
\arguments{
  \item{add_labels}{Logical indicating wether text labels
  should be placed}

  \item{format}{function to format text labels}

  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link{aes}} or \code{\link{aes_string}}. Only
  needs to be set at the layer level if you are overriding
  the plot defaults.}

  \item{data}{A layer specific dataset - only needed if you
  want to override the plot defaults.}

  \item{stat}{The statistical transformation to use on the
  data for this layer.}

  \item{position}{The position adjustment to use for
  overlappling points on this layer}

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}
}
\description{
  waterfall chart
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom",
  "waterfall")}
}
\examples{
X <- data.frame(transaction = c("Starting Cash","Sales", "Refunds", "Payouts", "Court Losses","Court Wins", "Contracts"),
             amount = c(3400, -1100, -100, -6600, 3800, 1400, 2800))
  X <- transform(X, transaction=factor(transaction, levels=unique(transaction)))
  ggplot(X, aes(x=transaction, y=amount)) + stat_waterfall()
}
\seealso{
  \code{\link{geom_rect}}: rectangles;
  \code{\link{stat_waterfall}}: waterfall chart statistics
}

