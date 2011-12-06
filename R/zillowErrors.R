zillowErrorTable =
  matrix(c("1" ,	"ServiceError", "there was a server-side error while processing the request 	Check to see if your url is properly formed: delimiters, character cases, etc.",
"2", 	"ZWSIDError", "The specified ZWSID parameter was invalid or not specified in the request 	Check if you have provided a ZWSID in your API call. If yes, check if the ZWSID is keyed in correctly. If it still doesn't work, contact Zillow to get help on fixing your ZWSID.",
"3", 	"ServiceUnavailable", "Web services are currently unavailable 	The Zillow Web Service is currently not available. Please come back later and try again.",
"4", 	"APICallUnavailable", "The API call is currently unavailable 	The Zillow Web Service is currently not available. Please come back later and try again.",
"500", 	"InvalidZPID", "The specified zpid parameter was not specified or invalid 	The zpid parameter you have provided is not valid. Please check if your zpid is correct and try again.",
"501", 	"InvalidCount", "The specified count parameter was not specified or invalid 	The count parameter which tells the api how many comparable homes to return is mandatory. Please check if you have specified a valid integer greater than or equal to 1.",
"502", "UnrecognizedZPID", "There is no record in our database that corresponds to the specified zpid parameter 	The zpid you provided didn't match any of the property zpids in our database. Either the property is not available in our database or there might be a typo in the zpid.",
"503", "UnavailableZestimate", "The property identified by the specified zpid does not have a Zestimate 	We found the property, but we do not have a Zestimate for this property. See our Zestimate coverage table.",
"504", "NoResults", "No results 	There were no comparables for the property you specified."), , 3, byrow = TRUE)
