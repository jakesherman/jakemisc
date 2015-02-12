







seeNAs <- function(data, type = "NAs") {
    
    if (type == "NAs") {
        return(sapply(data, function(f) sum(is.na(f))))
        
    } else if (type == "")
    
}