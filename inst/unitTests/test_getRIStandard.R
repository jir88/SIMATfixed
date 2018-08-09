## function to perform unit test for getRIStandard

test_getRIStandard <- function() {
    
    checkException(getRIStandard(), "A file name should be provided!")
}