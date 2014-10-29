zestimate =
  #
  # v = zestimate("1292 Monterey Avenue", "94707", zillowId)
  #
  #
function(address, citystatezip, zillowId = getOption("ZillowId", stop("need zillow id")), ...)  
{
  reply = getForm("http://www.zillow.com/webservice/GetSearchResults.htm",
                  'zws-id' = zillowId, 
                  address = address,
                  citystatezip = citystatezip,
                  ...)

  doc = xmlParse(reply, asText = TRUE)

  checkStatus(doc)
  
  zpid = xmlValue(doc[["//result/zpid"]])
  est = doc[["//result/zestimate"]]  
  data.frame(amount = as.numeric(xmlValue(est[["amount"]])),
             low = as.numeric(xmlValue(est[["valuationRange"]][["low"]])),
             high = as.numeric(xmlValue(est[["valuationRange"]][["high"]])),
             valueChange30Day = as.numeric(xmlValue(est[["valueChange"]])),             
             row.names = zpid )
}

checkStatus =
function(doc)
{
  msg = doc[["//message"]]
  code = xmlValue(msg[["code"]])

  if(!is.null(msg[["limit-warning"]]))
    warning("reaching the call limit of your Zillow Id")
  
  if(code == "0")
    return(TRUE)

  i = match(code, zillowErrorTable[, 1])
  e = simpleError(zillowErrorTable[i, 3])
  class(e) = c(zillowErrorTable[i, 2], class(e))
  stop(e)
}

getComps =
function(id, zillowId = getOption("ZillowId", stop("need zillow id")), count = 30, ...)
{
     # if the caller gives us an address rather than a zpid, find the zpid.
  if(!is.numeric(id) && length(grep("[^[:digit:]]", id))) {
    id = rownames(zestimate(id[1], id[2], zillowId))[1]
  }

  txt = getForm("http://www.zillow.com/webservice/GetDeepComps.htm",
                 zpid = id, 'zws-id' = zillowId,  count = count, ...)

  doc = xmlParse(txt, asText = TRUE)

  checkStatus(doc)
  
  comps = doc["//response//comparables[.//links]"][[1]]

  principal = doc[["//response/properties/principal"]]
  ans = compToDataFrame(principal)  
  ans = rbind(ans, do.call("rbind",  c(xmlApply(comps, compToDataFrame))))
  rownames(ans) = c(xmlValue(principal[["zpid"]]), xmlSApply(comps, function(x) xmlValue(x[["zpid"]])))

  class(ans$lastSold) = c("POSIXt", "POSIXct")
  class(ans) = c("ZillowComparables", class(ans))
  
  ans
}

compToDataFrame =
function(node, deep = TRUE)
{
  est = node[["zestimate"]]
  add = xmlToList(node[["address"]])
  if(length(add$latitude) == 0)
    add$latitude = NA
  if(length(add$longitude) == 0)
    add$longitude = NA  
     
  ans =
   data.frame(amount = as.numeric(xmlValue(est[["amount"]])),
             low = as.numeric(xmlValue(est[["valuationRange"]][["low"]])),
             high = as.numeric(xmlValue(est[["valuationRange"]][["high"]])),
             valueChange30Day = as.numeric(xmlValue(est[["valueChange"]])),
             street = add[["street"]],
             zip = add$zipcode,
             latitude = as.numeric(add$latitude),
             longitude = as.numeric(add$longitude),             
             row.names = xmlValue(node[["zpid"]] ))
  if(deep) {
    sapply(c("taxAssessmentYear", "taxAssessment", "yearBuilt", "lotSizeSqFt", "finishedSqFt",
              "bathrooms", "bedrooms", "lastSoldPrice"),
            function(id) {
                   ans[1, id] <<- as.numeric(if(!is.null(node[[id]])) xmlValue(node[[id]]) else NA)
            })
    
    ans[1, "lastSold"] = if(!is.null(node[["lastSoldDate"]]))
                            as.POSIXct(strptime(xmlValue(node[["lastSoldDate"]]), "%m/%d/%Y"))
                         else
                            NA
  }
  ans[1,"score"] =  xmlGetAttr(node, "score", NA, as.numeric)

  ans
}
          


plot.ZillowComparables =
function(x, threshold = NA, ...)
{
  opts = par(no.readonly = TRUE)
#  i = match(c("cin", "cra", "csi", "cxy", "din"), names(opts))
  on.exit(par(opts))
#  par(mfrow = c(1, 1))
  par(mfrow = c(1, 2))

#  boxplot(v$amount, horizontal = TRUE)
#  points(v$amount[1], 1, col = "red")

  bedrooms = ordered(x$bedrooms)
  bathrooms = ordered(x$bathrooms)  
  plot(x$finishedSqFt, x$amount, col = rainbow(length(levels(bathrooms)))[bathrooms],
        ylim = c(min(x$low), max(x$high)),
        xlab = "Sq Ft.", ylab = "Price")
  text(x$finishedSqFt, x$amount, as.character(bedrooms))
  x$score[1] = 1.0
  score = cut(x$score, 6)
  cols = rainbow(length(levels(score)))
  sapply(seq(length = nrow(x)),
          function(i) lines(rep(x$finishedSqFt[i], 2), c(x$low[i], x$high[i]),
                            col = if(i == 1) "blue" else cols[score[i]],
                            lwd = if(i == 1) 2 else 1))


  if(!is.na(threshold))
    abline(h = threshold, col = "yellow", lty = 2)

  plot(amount/finishedSqFt ~ lotSizeSqFt, x,
            col = c("red", rep("blue", nrow(x) - 1)))
  
  invisible(x)
}

# Recursive comps (2 levels) ---
zillowRecursiveComps <- function(address, citystatezip, zillowId=""){
  require(RCurl)
  require(XML)
  require(data.table)
  require(reshape2)
    
  u <- zestimate(address, citystatezip, zillowId)
  
  v <- getComps(row.names(u), zillowId,count=25)
  w <- lapply(row.names(v),getComps, zillowId, count=25)     
  zpidList <- list()
  
  for(i in 1:length(w)){
    zpidList[[i]] <- rownames(w[[i]])
  }  
  zlist <- do.call("rbind",zpidList)
  blah <- as.character(unique(melt(zlist)$value))
  
  x <- lapply(blah, getComps, zillowId, count=25)
  y <- rbindlist(x)
  z <- y[!duplicated(y$street)]
  z$subject <- paste0(address," ",citystatezip)
  return(z)
}


# Mapping --
mapComps <- function(data, zoomLevel=11,subject=NA){
  require(ggmap)
  
  
  xMap <- median(data$longitude)
  yMap <- median(data$latitude)
  coord <- paste0(yMap, ", ", xMap)
  compsMap <- qmap(coord, zoom = zoomLevel, color="bw")
  
  viewer <- compsMap + geom_point(aes(x = data$longitude, y = data$latitude),data = data, alpha=1/6,colour="red")
  if(is.na(subject)==T){out <- viewer; out} else {
    subj <- geocode(subject)
    out <- viewer + 
      geom_point(aes(data=subj$lon, y=subj$lat),data = subj,size=2,shape=9) + 
      geom_point(aes(data=subj$lon, y=subj$lat),data = subj,size=2,alpha=1/4,colour="green")
    out
  }
}
