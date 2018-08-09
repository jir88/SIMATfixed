## function to perform unit test for getTargetTable

test_getTargetTable <- function() {
    
    checkException(getTargetTable(), "A target table file is required!")  
}