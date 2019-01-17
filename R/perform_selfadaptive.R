#' @export
perform_selfadaptive <- function(L) {
  
  if(isTRUE(L$adapars$use)) {
    
    # ========== Error catching and default value definitions
    assertthat::assert_that("name" %in% names(L$adapars))
    # ========== 
    
    L <- do.call(L$adapars$name,
                args = list(L))
    } 
  
  return(L)
  
}