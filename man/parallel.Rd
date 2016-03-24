\name{parallel}
\alias{parallel}
\alias{print.parallel}
\alias{plot.parallel}
\title{Parallel Analysis}
\description{
to do
}
\usage{
parallel(x, iter = 1000, ordinal = FALSE, method = "perm", alpha = 0.05,
         standard = FALSE, plot = TRUE, fn = eigen, ...)
}
\arguments{
    \item{x}{xxx}
    \item{iter}{xxx}
    \item{ordinal}{xxx}
    \item{method}{xxx}
    \item{alpha}{xxx}
    \item{standard}{xxx}
    \item{plot}{xxx}
    \item{fn}{xxx}
    \item{...}{xxx}
}
\details{
to do
}
\author{
Davide Massidda \email{davide.massidda@gmail.com}
}
\seealso{
\code{\link[raschTools]{reliability}}
}
\examples{
data(test1)
fit <- RM(test1)
ppar <- person.parameter(fit)
res <- residuals(ppar)
(pa <- parallel(res))
}
