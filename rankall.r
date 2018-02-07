rankall <- function(outcome,num) {
                ## This function accepts an outcome (heart attack, heart failure, or pneumonia), and a desired rank.  The dataset 
                ## used is a listing of hospitals, with corresponding values for each outcome, by state.  Depending on the outcome
                ## selected, the hospitals are sorted according to the outcome chosen, and then the hospital that matches the
                ## rank provided is produced as output, along with the state it represents.
        
                ## Initialize all variables
        
        hospitals<-NULL
        statehosps<-NULL
        outcome_list<-c("heart attack","heart failure","pneumonia")
        ordered_list<-NULL
        ranked_hosp<-NULL
        
                ## Read in the input data
        
        hospitals<-read.csv("outcome-of-care-measures.csv", colClasses="character")
        
                ## If an incorrect outcome (other than: heart attack, heart failure or pneumonia)is entered, stop the code
                ## and reply with "invalid outcome".  Else if the outcome is valid, capture the column in the dataset that the 
                ## outcome refers to...
        
        if (!outcome %in% outcome_list) {
                stop ("invalid outcome")
        } else if (outcome=="heart attack") {
                col<-11
        } else if (outcome=="heart failure") {
                        col<-17
                } else {col<-23}
        
                ## Update the column of values, to a numeric in order to perform accurate sorting and subsetting.
        hospitals[,col]<-suppressWarnings(as.numeric(as.character(hospitals[,col])))
        
                ## Eliminate any rows where there is a "NA" for the outcome chosen
        ordered_list<-subset(hospitals,!is.na(hospitals[,col]))
        
                ## Order the full list first by State, then by outcome, then by hospital name
        ordered_list<-ordered_list[order(ordered_list[,7],ordered_list[,11],ordered_list[,2]),]
        
                ## split the full list into each state's list of hospitals, select only columns for state, hospital name and outcome
        statehosps<-split(ordered_list[,c(2,7,col)],ordered_list$State) 

        ## If the rank is 'worst', select the last hospital listed for each state, by using the number of rows to determine
        ## the last row. 
        ## If the rank is 'best', replace the 'num' value with 1 (for first).  Select the correct row from each state, based on 
        ## the value in 'num'.
        ## The lapply function will select and extract the correct row from the statehosps list.  The state abbreviations are 
        ## automatically used from the split function above, to label each row in ranked_hosp.  The result is that each state 
        ## will have one dataframe, with the selected row (matching the rank desired), including state abbreviation and hospital.
        ## These rows are then combined into one file using the 'do.call' and 'rbind' functions.
        
        if (num=="worst") {
                ranked_hosp<-lapply(statehosps,function(x) {x[nrow(statehosps),1:2]})
        }
        else {
                if (num=="best") {
                num<-1
                }
                ranked_hosp<-lapply(statehosps,function(y) {y[num,1:2]})
        }    
        do.call(rbind,ranked_hosp)
        }