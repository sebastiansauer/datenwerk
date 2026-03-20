

#' Define workflow
#'
#' Simple wrapper to save typing around `workflow()` (from tidymodels)
#' The function exepcts a model/learner object by name of `mod` 
#' and a recipe by the name of `rec`
#' Then, the workflow is initiated and model and recipe are added.
#' This workflow is returned.
#' Note that no fitting/tuning is defined or performed.
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
