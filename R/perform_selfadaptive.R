perform_selfadaptive <- function(L) {
  
  if(isTRUE(L$adapars$use)) {
    
    # ========== Error catching and default value definitions
    assertthat::assert_that("name" %in% names(L$adapars))
    # ========== 
    
    #remember change this
    L$adapars <- do.call(adapars$name,
                         args = list(L,
                                     adapars = L$adapars))
  } 
  
  return(L)
  
}