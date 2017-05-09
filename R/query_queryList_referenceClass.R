#' dataQueries reference class
#'
#' A reference class for dataqueries to prevent the need to return the data back to global.
#'
#' @field q data.frame containing these columns id, identifier, queryRun, patid, CRF, repeatLine, firstQuery, STATScomments,comments, resolved, STATSresolved
#' @field queryRun A number denoting the query run. Useful for seeing what queries are still active.

# use a reference class so that we don't need to return the results. This works since there is only one copy and everything else is a reference to this copy.
#' @exportClass dataQueries
setRefClass("dataQueries", fields = c("q"="data.frame", queryRun = "numeric"))



#' queryList
#'
#' A generator function for the \code{\link{dataQueries-class}}. Takes an optional dataframe and queryRun number and creates a reference object. If the data.frame is not provided then an empty one is generated.
#'
#' @param q query data.frame with the columns id, identifier, queryRun, patid, CRF, repeatLine, firstQuery, STATScomments,comments, resolved, STATSresolved
#' @param queryRun A number denoting the query run. Useful for seeing what queries are still active.
#' @export queryList
queryList = function( q = NULL, queryRun = 1){
  if(is.null(q)){
    coln = c("id","identifier","queryRun","patid","CRF","repeatLine1","repeatLine2","firstQuery","STATScomments","comments","resolved","STATSresolved")
    q = data.frame(matrix("",nrow=0,ncol=length(coln)), stringsAsFactors = FALSE)
    names(q) = coln
  } else {
    q$STATSresolved = q$resolved
    message("The STATSresolved column is set to the resolved column and will be updated as necessary")
  }
  new("dataQueries", q = q, queryRun = queryRun)
}
