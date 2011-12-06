\name{zestimate}
\alias{zestimate}
\title{Get the Zillow estimate for a property}
\description{
 This returns the Zillow estimate for a particular property
 for a house identified by street and ZIP code, or street
 and city with state.
}
\usage{
zestimate(address, citystatezip, zillowId, ...)
}
\arguments{
  \item{address}{the house number and street}
  \item{citystatezip}{either  a ZIP code or a city and state separated by a ','}
  \item{zillowId}{the Zillow Web Services ID (ZWSID) that allows you to make
     calls to the Zillow API. See \url{https://www.zillow.com/webservice/Registration.htm}.}
  \item{\dots}{additional parameters passed on to the \code{\link[RCurl]{getForm}} call.   
    These allow the caller to customize/parameterize the Web request, e.g. reusing an existing
     connection via a curl handle, changing the HTTP headers, etc.}
}
\value{
 A data frame with a single row.
 The row name is the Zillow property id (ZPID)  for the house.
 The  columns are  amount, low, high, valueChange30Day
 which are all numeric values.
}
\references{ 
 \url{http://www.zillow.com/howto/api/APIOverview.htm}
}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{getComps}}
}
\examples{
\dontrun{
      # You need a ZWSID to run these examples.
   zestimate("1280 Monterey Avenue", "94707", zillowId)
   zestimate("1600 Pennsylvania Avenue", "Washington, DC", zillowId)
}
}
\keyword{IO}


