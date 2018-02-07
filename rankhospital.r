rankhospital <- function(stateabbr,outcome,num) {
        
                ## The rankhospital function accepts a state abbreviation, an outcome (heart attack, heart failure, pneumonia)
                ## and a desired hospital ranking.  The goal is to find the hospital, within the desired state, that 
                ## is ranked at the desired position
        
                ## Initialize variables
        
        best<-NULL
        outcome_list<-c("heart attack","heart failure","pneumonia")
        hospitals<-NULL
        ordered_list<-NULL
        order_index<-NULL
        
                ## Read in the input dataset.
        
        best<-read.csv("outcome-of-care-measures.csv", colClasses="character")
        
                ## Validate the input arguments.  If the state abbreviation provided is not a valid state, 
                ## or the outcome is not a valid outcome (heart attack, heart failure, or pneumonia), then stop
                ## the processing and issue a message
                
        
        if (!stateabbr %in% c(state.abb)) {
                stop ("invalid state")
        }
        if (!outcome %in% outcome_list) {
                stop ("invalid outcome")
        }
        
        ## If the arguments are valid, subset the original dataset to extract only the state that
        ## was input. The new dataset is called 'hospitals'.
        
        
        if (outcome %in% c("heart attack","heart failure","pneumonia")){
                        
                hospitals<-subset(best,subset=(stateabbr==best$State))
                
        }
        
                ## Each outcome has its own column of values that need to be analyzed.  Below, based on the outcome
                ## input, the corresponding column is set to numeric so that sorting will work appropriately.  Then
                ## the subset is ordered from low to high based on the outcome column. Lastly, any hospitals with missing
                ## information in the outcome column, are removed.  The resulting list is 'ordered_list'.
        
        if (outcome == "heart attack") {
                                
                hospitals[,11]<-suppressWarnings(as.numeric(as.character(hospitals[,11])))
                
                ordered_list<-hospitals[order(hospitals[,11],hospitals[,2]),]
                
                ordered_list<-subset(ordered_list,!is.na(ordered_list[,11]))
                
                
        } else if (outcome == "heart failure") {
                
                hospitals[,17]<-suppressWarnings(as.numeric(as.character(hospitals[,17]))) 
                
                ordered_list<-hospitals[order(hospitals[,17],hospitals[,2]),]
                
                ordered_list<-subset(ordered_list,!is.na(ordered_list[,17]))
               
        } else if (outcome == "pneumonia") {
                                
                hospitals[,23]<-suppressWarnings(as.numeric(as.character(hospitals[,23])))
                
                ordered_list<-hospitals[order(hospitals[,23],hospitals[,2]),]
                 
                ordered_list<-subset(ordered_list,!is.na(ordered_list[,23]))     
        }
        
                ## If 'best' or 'worse' were entered as the rank (in the argument, num), then reset 'num' to equal
                ## one for 'best', or for worse, calculate the number of rows/hospitals in the state, and enter the
                ## total number of rows into 'num'.  This will represent the last row of hospital data.
        
        if (num=="best") {
                num<-1
        } 
        
        
        if (num=="worst") {
                num<-nrow(ordered_list)

        }
        
                ## If the rank input is greater than the number of hospitals with data, for that state, issue an error.
        
        if (num>nrow(hospitals)) {
                opt <- options(show.error.messages = FALSE)
                on.exit(options(opt))
                print("NA")
                stop ()
        }
        
                ## Grab the row that matches the desired rank ('num'), and the hospital name (located in column 2). Save
                ## this to 'rankedhosp'.
        
        rankedhosp <- ordered_list[num,2]
                
        return (rankedhosp)
}