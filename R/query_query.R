

#' query
#'
#' A function for testing queries on some data. This function uses the gmp package to match data queries which were asked previously.
#'
#' @param q An object of reference class dataQueries
#' @param data The database to check is valid
#' @param validation a quote object to test with the data
#' @param CRF The CRF name (string)
#' @param mess The message to return if any data fails the validation
#' @param parameters a vector of column names to append to the message (string)
#' @param patid Name for the main identifier column. Repeat line can take a second. Any others should be added to parameters.
#' @param repeatLine1,repeatLine2 Name of a line reference column when data is in long format with respect to patid
#' @param reject Logical should the validation be TRUE or FALSE to report the query?
#' @param prnt Logical should the number of failed queries be returned as a message?
#'
#' @details
#'
#' \code{queryQ()} is a shorthand form which means the parameters do not need to be passed to the function. Instead they must be correctly named in the current namespace. This is currently only tested in the global environment and may not work in other environments such as creating .Rmd files.
#'
#' @example examples/query_example.R
#' @import gmp
#' @importFrom gmp as.bigz
#' @importFrom gmp %*%
#' @importFrom gmp numerator
#' @import openxlsx
#' @export query
query = function(q, data, validation, CRF, mess, parameters = NULL, patid = "patid", repeatLine1 = NULL, repeatLine2 = NULL, reject = TRUE, prnt = TRUE){
  mod = as.bigz("900000000000000046043660025881") # nextprime(10^30 - 10^29)
  nme = names(data)

  if(dim(data)[1] > 0){
    ev = if(reject){
      sapply(with(data,eval(validation)), function(x) isTRUE(x))
    } else {
      sapply(with(data,!eval(validation)), function(x) isTRUE(x))
    }
    dsub = data[ev,]

    dma = dim(dsub)
    counter = c(0,0)
    if(dma[1] > 0){
      for(i in 1:dma[1]){
        a = 0
        thisMessage = mess
        if(!is.null(parameters)){
          thisMessage = paste(thisMessage,"(")
          for(p in 1:length(parameters)) {
            thisMessage = paste0(thisMessage, parameters[p]," = ", dsub[i,parameters[p]],ifelse(p == length(parameters),"",", "))
          }
          thisMessage = paste0(thisMessage,")")
        }
        text = paste0(dsub[i,patid], CRF, dsub[i,repeatLine1], dsub[i,repeatLine2], thisMessage, paste0(validation,collapse = ","))
        code = .encode(text)
        dm = dim(q$q)

        if(dm[1] > 0){
          if(code %in% q$q$identifier){
            a = 1
            counter[2] = counter[2] + 1
            lineNumber = which(code == q$q$identifier)

            if(q$q$queryRun[lineNumber] != q$queryRun) {
              q$q$STATSresolved[lineNumber] = "No"
              q$q$firstQuery[lineNumber] = "No"

            }
            q$q$queryRun[lineNumber] = q$queryRun
          }
        }


        if(a == 0){
          counter[1] = counter[1] + 1
          q$q[dm[1]+1,] <- c(dm[1]+1, code, q$queryRun, dsub[i,patid], CRF, ifelse(is.null(repeatLine1),"1",dsub[i,repeatLine1]), ifelse(is.null(repeatLine2),"1",dsub[i,repeatLine2]), "Yes", thisMessage, "", "","No")
        }
      }
      if(prnt){
        message("New queries: ", counter[1], " Unresolved queries: ", counter[2])
      }
    }
  }
}

#' @describeIn query A short hand version which will find the parameters from the global environment
#' @export queryQ
queryQ = function(){

  if(!exists("patid", mode = "any")){
    patid = "patid"
  }
  if(!exists("repeatLine1")){
    repeatLine = NA
  }
  if(!exists("repeatLine2")){
    repeatLine = NA
  }
  if(!exists("parameters")){
    parameters = NULL
  }
  if(!exists("reject")){
    reject = TRUE
  }
  if(!exists("prnt")){
    prnt = TRUE
  }
  query(q, data, validation, CRF, mess, parameters, patid, repeatLine1, repeatLine2, reject, prnt)
}

.hexNumber = function(text){
  paste0(sapply(as.character(utf8ToInt(text)), function(x) ifelse(nchar(x) < 3, paste0(rep("0",3-length(x)),x),x)),collapse = "")
}

.encode = function(text){
  num = .hexNumber(text)
  # print(num)
  big = as.bigz(paste0(1,num))
  modulus(big) = as.bigz("900000000000000046043660025881")
  print(big)
  # big = as.bigz(bn , mod = "900000000000000046043660025881") # mod = nextprime(10^30 - 10^29)
  return(as.character(numerator(big)))
}




#' Load and Save the queries from an excel workbook.
#'
#' @param file fileName to load from or save to
#' @param queryRun The number of the current
#' @param wb The workbook object created by queryLoad
#' @param q The query object with all queries checked and updated
#'
#' @details
#' As a minimum the file to load should contain an "Instructions" and an "Initials" worksheet.
#' The database has 12 columns: "id","identifier","queryRun","patid","CRF","repeatLine1","repeatLine2","firstQuery","STATScomments","comments","resolved","STATSresolved" these are checked by queryLoad to ensure there will be no errors later.
#'
#' @export queryLoad
queryLoad = function(file, queryRun = 1) {

  wb <- createWorkbook("DataQueries")
  sheet1<-addWorksheet(wb,"Instructions")
  sheet2<-addWorksheet(wb,"Initials")
  sheet3<-addWorksheet(wb, "Previous")
  sheet4<-addWorksheet(wb, "Current",tabColour = "red")

  writeData(wb=wb,x=readWorkbook(xlsxFile = file, sheet = "Instructions"),sheet=1)
  writeData(wb=wb,x=readWorkbook(xlsxFile = file, sheet = "Initials"),sheet=2)

  previous = NULL
  current = NULL

  try({
    previous = readWorkbook(xlsxFile = file, sheet = "Previous")
  }, silent = TRUE)
  try({
    current = readWorkbook(xlsxFile = file, sheet = "Current")
  }, silent = TRUE)


  coln = c("id","identifier","queryRun","patid","CRF","repeatLine1","repeatLine2","firstQuery","STATScomments","comments","resolved","STATSresolved")
  if(is.null(previous)){
    previous = data.frame(matrix("",nrow=0,ncol=length(coln)), stringsAsFactors = FALSE)
    names(previous) = coln
  }

  if(is.null(current)){
    current = data.frame(matrix("",nrow=0,ncol=length(coln)), stringsAsFactors = FALSE)
    names(current) = coln
  }

  if(dim(current)[2] != 12){
    warning("Current spreadsheet is not the correct 12 columns long")
  }
  if(sum(names(previous) == coln) != 12) {
    warning("Not all column names match in Current spreadsheet")
  }
  if(dim(previous)[2] != 12){
    warning("Previous spreadsheet is not the correct 12 columns long")
  }
  if(sum(names(previous) == coln) != 12) {
    warning("Not all column names match in Previous spreadsheet")
  }

  message("Rows in Previous queries spreadsheet ",dim(previous)[1])
  message("Rows in Current queries spreadsheet ",dim(current)[1])

  dta = rbind(previous, current)

  if(dim(dta)[1] == 0){
    q = queryList()
  } else {
    q <- queryList(dta, queryRun = queryRun)
  }
  return(list(q=q, wb = wb))

}

#' @describeIn queryLoad Save the query object to excel
#' @export querySave
querySave = function(wb, q, file) {

  resolved = q$q[q$q$STATSresolved != "No",]

  message("Rows of resolved queries ",dim(resolved)[1])

  unresolved = q$q[q$q$STATSresolved == "No",]

  p1 = which(!unresolved$resolved %in% c("Yes","No",NA))
  p2 = which(is.na(unresolved$resolved))
  p3 = which(!unresolved$resolved %in% c("No"))
  p4 = which(!unresolved$resolved %in% c("Yes"))

  unresolved = unresolved[c(p1,p2,p3,p4),]

  message("Rows of unresolved queries ",dim(unresolved)[1])

  # Add No, Yes, Unresolved to STATSresolved column -- ADD TO CODE
  # writeData(wb=wb,x=instr,sheet=1)
  # writeData(wb=wb,x=initials,sheet=2)
  writeData(wb=wb,x=resolved, sheet=3)
  writeData(wb=wb,x=unresolved, sheet=4)
  saveWorkbook(wb,file, overwrite = TRUE)

}

