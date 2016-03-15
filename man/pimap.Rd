\name{pimap}
\alias{pimap}
\title{Person-Item Map}
\description{
to do
}
\usage{
pimap(data.pr, data.it, logit.pr, logit.it, item, thr, rel_widths = c(1,2),
      logit.range = NULL, binwidth = 0.25, xlab.pr = "", xlab.it = "Item",
      ylab.pr = "Count", ylab.it = "logit", title.pr = "Person\'s abilities",
      title.it = "Item thresholds")
}
\arguments{
    \item{data.pr}{to do}
    \item{data.it}{to do}
    \item{logit.pr}{to do}
    \item{logit.it}{to do}
    \item{item}{to do}
    \item{thr}{to do}
    \item{rel_widths}{to do}
    \item{logit.range}{to do}
    \item{binwidth}{to do}
    \item{xlab.pr}{to do}
    \item{xlab.it}{to do}
    \item{ylab.pr}{to do}
    \item{ylab.it}{to do}
    \item{title.pr}{to do}
    \item{title.it}{to do}
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
\examples{\dontrun{
data(test2)
fit <- PCM(test2)
persPar <- person.parameter(fit)
persPar <- data.frame(logit=persPar$thetapar[[1]])
itemPar <- thresholds(fit)
itemPar <- data.frame(itemPar$threshtable[[1]][,-1])
itemPar <- reshape(itemPar, varying=1:3, v.names="logit", timevar="step", idvar="item", direction="long")
pimap(persPar, itemPar, logit.pr="logit", logit.it="logit", item="item", thr="step")
}}
