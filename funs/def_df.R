

#' Define workflow
#'
#' Simple wrapper to save typing around `workflow()`
#'
#' @return workflow
#' @export
#'
#' @examples
#' wf <- def_wf()
def_wf <- function(){
  
  wf <-
    workflow() %>% 
    add_model(mod) %>% 
    add_recipe(rec)
}
