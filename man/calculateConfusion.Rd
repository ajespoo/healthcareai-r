% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/common-kmeans.R
\name{calculateConfusion}
\alias{calculateConfusion}
\title{Generate confusion matrix of percentages}
\usage{
calculateConfusion(labels, clusters)
}
\arguments{
\item{labels}{A vector with countable unique items, usually a factor variable 
in a data frame, labeling each observation.}

\item{clusters}{A vector with countable unique items, usually is the clustering 
results returned by kmeans(), No NA's.}
}
\value{
A confusion matrix of percentages.
}
\description{
Generate confusion matrix and convert from raw counts to percentage
of each label
}
\examples{
data(iris)
head(iris)
kmeans.fit <- kmeans(iris[,1:4],3)
labs <- iris[,5]
cls <- kmeans.fit[["cluster"]]
calculateConfusion(labels = labs, clusters = cls)

}
\references{
\url{http://healthcare.ai}
}
\seealso{
\code{\link{healthcareai}}
}
