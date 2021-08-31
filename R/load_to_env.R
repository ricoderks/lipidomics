#' @title Load to environment
#'
#' @description Load objects into a new environment
#'
#' @param RData the .Rdata file.
#' @param env an environment to load the data into
#'
#' @details This function was found in a blog on R-bloggers. For more information
#' see the blog.
#'
#' @return Returns an environment containing all the saved objects.
#'
#' @author Rico Derks
#'
#' @references https://www.r-bloggers.com/2016/04/safe-loading-of-rdata-files/
#'
load_to_env <- function(RData, env = new.env()){
  # load the data file into a new environment
  load(RData, env)

  # return the environment
  return(env)
}
