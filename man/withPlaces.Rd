% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getData.R
\name{withPlaces}
\alias{withPlaces}
\title{Add enclosed Geo-Information to Tweet-Dataframe}
\usage{
withPlaces(res, tweets)
}
\arguments{
\item{res}{A resource, retrieved by \code{twitter_fas()}.}

\item{tweets}{The tweets-dataframe, as returned by \code{getTweets()}}
}
\value{
A data frame, actually with the length of the tweets-data, with media-information added.
}
\description{
Add enclosed Geo-Information to Tweet-Dataframe
}
\examples{
tweets <- withPlaces(cducsuTweets, getTweets(cducsuTweets))
head(tweets)
}
