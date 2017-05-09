# Create a new queryList
q = queryList()

# use the mtcars data for the example
data = mtcars
data$patid = rownames(data)

CRF = "cars"
reject = TRUE

# first validation
validation = quote(disp > 400)
mess = "Disp too high"
parameters = c("disp")

query(q, data, validation, CRF, mess, parameters)
q$q

# running a query again will not add them again:
query(q, data, validation, CRF, mess, parameters)

# a more complex validation
validation = quote(grepl("Merc",patid) & cyl == 4)
mess = "Merc's cannot have 4 cylinders"
parameters = c("cyl")

# test that the validation is written correctly:
with(data, eval(validation))

query(q, data, validation, CRF, mess, parameters)

# short hand form.
queryQ()
q$q
