perform_selfadaptive <- function(L) {
  # ========== Error catching and default value definitions
  
  assertthat::assert_that("adapars" %in% names(L), 
                          "name"    %in% names(L$adapars))
  
  # ========== 
  adapars <- L$adapars
  
  if(isTRUE(adapars$use)) {
    
    #remember change this
    L$adapars <- do.call(adapars$name,
                         args = list(L,
                                     adapars = adapars))
  } 
  
  return(L)
  
}