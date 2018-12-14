perform_selfadaptive <- function(L) {
  adapars <- L$adapars
  
  if(isTRUE(adapars$use)) {
    
    # ========== Error catching and default value definitions
    assertthat::assert_that("name" %in% names(L$adapars))
    # ========== 
    
    #remember change this
    L$adapars <- do.call(adapars$name,
                         args = list(L,
                                     adapars = adapars))
  } 
  
  return(L)
  
}