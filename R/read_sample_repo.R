#' Read the sample data
#'
#'  @param none
#'  @return a git repo object
#'  @description Show the sample data for giraffe
#'  @import git2r
#'  @export
#'  @examples
#'  repo<-read_sample_repo()
#'  summary(repo)

read_sample_repo <- function() {
  repository(paste0(path.package("giraffe"), "/extdata/repo/"))
}
