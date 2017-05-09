
source('C:/Users/pdutton/Dropbox/R/dataCleaning/functions.R')




# Create empty queryList
q = queryList()
# add the loaded data.frame if there is an existing list here, it will take the data.frame as an argument!
# if adding an existing query data.frame then STATSresolved is set to resolved and is updated as required by the query function.


# read in the data
data = read.csv("I:/Data/MSG Support/projects/FOXFIRE/Data Management/Data/Data as Received/DataLock19 16Mar2016/FOXFIRE_APR_DataLock_16Mar2016_READ_ONLY/CRF - Adverse Event Form_16_Mar_2016.txt", stringsAsFactors = FALSE)



# Validation (code, quote(expr))
# true/false (whether to report on TRUE or FALSE)

# message (message to print in STATS)
# parameters (parameters to print in STATS)


data$OverallStopDate


data =data
patid = "TrialNumber"

validation = quote(OverallStopDate == "" & OverallOngoing == "Yes")
validation = quote(OverallStopDate == "")
reject = TRUE
CRF = "Adverse Event Form"
mess = "missing OverallStopDate"
parameters = c("OverallStopDate", "OverallDiagnosis")


query(q, data, validation, CRF, mess, parameters)

# if we repeat a query it doesn't add them but will alter queryRun and STATSresolved
# This is handled by using the identifier value which is
query(q, data, validation = quote(OverallStopDate == ""), CRF = "Adverse Event Form", mess = "missing OverallStopDate", parameters = c("OverallStopDate", "OverallDiagnosis"))






queryRun
## Save the data queries by writing the only slot in the queries to a csv file.
# write.csv(q$q, file = "")
## can alternatively save to word using an R markdown file instead.

