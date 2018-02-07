best <- function(stateabbr,outcome) {
                ## The best function will accept a state abbreviation, and an outcome (heart attack, heart failure or pneumonia).
                ## The goal is to find the hospital, within that state, that has the best outcome (lowest value for mortality).
                ## 
        
                ## Initialize variables
        best<-NULL
        outcome_list<-c("heart attack","heart failure","pneumonia")
        hospitals<-NULL
        ordered_list<-NULL
        order_index<-NULL
                
                ## Read the input dataset into a dataframe called 'best'.
        
        best<-read.csv("outcome-of-care-measures.csv", colClasses="character")
        
                ## Validate the input arguments.  If the state abbreviation provided is not a valid state, 
                ## or the outcome is not a valid outcome (heart attack, heart failure, or pneumonia), then stop
                ## the processing and issue a message
                ## If the arguments are valid, subset the original dataset to extract only the state that
                ## was input. The new dataset is called 'hospitals'.
        
        if (!stateabbr %in% c(state.abb)) {
                stop ("invalid state")
        }
        if (!outcome %in% outcome_list) {
                stop ("invalid outcome")
        }
        if (outcome %in% c("heart attack","heart failure","pneumonia")){
                        
                hospitals<-subset(best,subset=(stateabbr==best$State))
                
        }
                
                ## Each outcome has its own column of values that need to be analyzed.  Below, based on the outcome
                ## input, the corresponding column is set to numeric so that sorting will work appropriately.  Then
                ## the subset is ordered from low to high based on the outcome column.
        
        if (outcome == "heart attack") {
                                
                hospitals[,11]<-suppressWarnings(as.numeric(as.character(hospitals[,11])))
                
                ordered_list<-hospitals[order(hospitals[,11],hospitals[,2]),]
                
                
        } else if (outcome == "heart failure") {
                
                
                hospitals[,17]<-suppressWarnings(as.numeric(as.character(hospitals[,17]))) 
                
                ordered_list<-hospitals[order(hospitals[,17],hospitals[,2]),]
                
               
               
        } else if (outcome == "pneumonia") {
                                
                hospitals[,23]<-suppressWarnings(as.numeric(as.character(hospitals[,23])))
                
                ordered_list<-hospitals[order(hospitals[,23],hospitals[,2]),]
                      
        }
        
                ## Only the first row, representing the best hospital for that state, and only the hospital name
                ## in column 2, is required for the output. 
        
        besthosp <- ordered_list[1,2]
                
        ##
         return (besthosp)
}