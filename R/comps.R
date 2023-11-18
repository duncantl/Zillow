getCompComps =
function(id, zillowId = getOption("ZillowId"), count = 25, verbose = FALSE, info = getComps(id, zillowId, count))
{
   if(is.data.frame(id))
     info = id
   
   info$zid = rownames(info)
   
   tmp = lapply(unique(rownames(info)[-1]),
                 function(x) {
                     if(verbose)
                       cat(x, "\n")
                     try(getComps(x, zillowId, count))
                 })
   tmp = tmp[ sapply(tmp, inherits, "data.frame") ]

   tmp = lapply(tmp, function(x) { x$zid = rownames(x) ; x})
   other = do.call("rbind", tmp)
   ans = rbind(info, other)
   structure(ans[!duplicated(ans$zid), ], class = c("ExtendedZillowComparables", "ZillowComparables", class(ans)))
}



if(FALSE) 
followComps  =
function(id, zillowId, count = 30, verbose = FALSE, prevId = integer())
{
    lapply(id, getComps, zillowId)
}
