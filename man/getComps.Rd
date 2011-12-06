\name{getComps}
\alias{getComps}
\title{Get Zillow estimates and information for comparable properties}
\description{
  This retrieves information about properties that Zillow
  deems comparable to the specified property of interest.
}
\usage{
getComps(id, zillowId, count = 30, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{the property identifier. This can be a the Zillow property identifier,
  (i.e. a number something like 24842790 which is obtained from a previous call to the Zillow API)
  or it can be a character vector of length 2 giving the house number and street in
   the first element, and the zip code or city and state in the second element.}
  \item{zillowId}{the Zillow Web Services ID (ZWSID) that allows you to make
     calls to the Zillow API. See \url{https://www.zillow.com/webservice/Registration.htm}.}
  \item{count}{the number of comparables to return in the result. This should be
   a positive number and the current limit allowed by the Zillow API appears to be 30.}
  \item{\dots}{additional parameters passed on to the \code{\link[RCurl]{getForm}} call.   
    These allow the caller to customize/parameterize the Web request, e.g. reusing an existing
     connection via a curl handle, changing the HTTP headers, etc.}
}

\value{
  A data frame. The first row gives the details for the
  specified house, and the remaining rows provide the same
  details for each of the comparables.
  The variables are
\item{ amount }{}
\item{ low }{}
\item{ high }{}
\item{ valueChange30Day }{}
\item{ street }{}
\item{ zip }{}
\item{ latitude }{}
\item{ longitude }{}
\item{ taxAssessmentYear }{}
\item{ taxAssessment }{}
\item{ yearBuilt }{}
\item{ lotSizeSqFt }{}
\item{ finishedSqFt }{}
\item{ bathrooms }{}
\item{ bedrooms }{}
\item{ lastSoldPrice }{}
\item{ lastSold }{}
\item{ score }{}
}
\references{ 
\url{http://www.zillow.com/howto/api/GetDeepComps.htm}
}

\seealso{
 \code{\link{zestimate}}
}
\examples{
\dontrun{
      # You need a ZWSID to run these examples.
   getComps(c("1280 Monterey Avenue", "94707"), zillowId)
   getComps(c("1280 Monterey Avenue", "94707"), zillowId, count = 5)

      # Using a previously retrieved Zillow property identifier.
   getComps(24842790, zillowId)
}
}
\keyword{IO}



