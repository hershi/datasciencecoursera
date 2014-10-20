# Find the n-th ranked hospital in a given state for a given "outcome".
#
# state - only hospitals from this state will be considered
# outcome - the medical condition for which we want to evaluate the hospitals.
#   Can be one of: "heart attack", "heart failure", and "pneumonia"
#
# rank - "best", "worst", or a number indicating the rank we're interested
# in.
#
# Hospitals with no data for the particular outcome will not be considered
# In case of a tie, the result will be based on lexicographic order of the
#   hospital names (first hospital will be returned)
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    hospitalData<-read.csv("outcome-of-care-measures.csv", colClasses="character")

    # Initialize some constants for column numbers
    hospitalStateCol<-7
    hospitalNameCol<-2

    # Valid outcomes, and the column number where the 30 day mortality rate 
    # is stored
    validOutcomes <- rbind("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)

    # Extract the list of states from the loaded data. If the 'state' provided
    # in the argument is not in that list, then it's invalid. Abort with the
    # appropriate message
    if (!(state %in% unique(as.character(hospitalData[,hospitalStateCol]))))
    {
        stop("invalid state")
    }

    # If the outcome is not one of the allowed ones, abort with the appropriate
    # message
    if (!(outcome %in% rownames(validOutcomes)))
    {
        stop("invalid outcome")
    }

    # Extract the column number for the desired outcome
    colNum <- validOutcomes[outcome,1]

    # Extract the lines for hospitals in the desired state, since we only
    # want to evaluate those. 
    # Also, drop any lines where the desired outcome is "Not Available". While
    # this can be done using is.na after coercing the vector to numeric, it
    # yields an annoying warning message when converting "Not Available" to NA,
    # which we want to avoid.
    #
    # For these hospitals, extract the hospital name and the 30-day mortality
    # rate for the outcome we're evaluating (we don't need the rest of the
    # data)
    data<-hospitalData[hospitalData[,hospitalStateCol] == state & hospitalData[colNum] != "Not Available" ,c(hospitalNameCol,colNum)]

    if (num == "best")
    {
        num <- 1
    }
    else if (num == "worst")
    {
        num <- nrow(data)
    }
    else
    {
        num <- as.numeric(num)
    }

    if (is.na(num) | num > nrow(data))
    {
        return(NA)
    }


    # Coerce the mortality rate to numeric, since it was read as "character"
    data[,2]<-as.numeric(data[,2])

    # Sort the hospitals based on 30-day mortality rate, with name as a
    # secondary sort (for breaking ties), then return the top of the list
    sortOrder<-order(data[,2],data[,1])
    return(as.character(data[sortOrder[num],1]))
}

