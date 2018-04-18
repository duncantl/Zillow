# Zillow

This package provides an R interface to the Zillow Web Service API. It allows one to 

1. get the Zillow estimate for the price of a particular property specified by street address and ZIP code (or city and state)
2. find information (e.g. size of property and lot, number of bedrooms and bathrooms, year built.) about a given property
3. get comparable properties.

One needs a (free) login for Zillow to access the Web service.
The following gets the estimate, where zillowId is a string 
giving the Zillow key you obtained when you registered to use the Zillow Web Service.

    est = zestimate("1280 Monterey Avenue", "94707", zillowId)

We can get the comparables with

    comps = getComps(c("1280 Monterey Avenue", "94707"), zillowId)
    
or

    comps = getComps(rownames(est), zillowId)
    
Then we can plot these with
 
    plot(comps)
