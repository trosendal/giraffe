#' Get blobs from the git database
#'
#'  @param repo A git2R repo object
#'  @param path A relative path to the table that you are interested in
#'  @description None
#'

get_blobs <- function(repo = repo, path = "test.csv"){
    ##    stopifnot(in_repository(path))
    ##    repo <- repository(discover_repository(path))
    blobs <- odb_blobs(repo)
    files <- paste0(ifelse(nchar(blobs$path) > 0, paste0(blobs$path, "/"), ""), blobs$name)
    i <- grep(path, files)
    lapply(blobs[i, "sha"], function(sha) lookup(repo, sha))
}

#' render diffs of an object in the git database
#'
#'  @param repo A git2R repo object
#'  @param path A relative path to the table that you are interested in
#'  @description None
#'  @import daff
#'  @export
#'  @examples
#'  repo <- read_sample_repo()
#'  make_tables(repo = repo, path = "test.csv")


make_tables <- function(repo = repo, path = "test.csv"){

    dfs <- lapply(get_blobs(repo = repo, path = path), function(x) read.csv2(textConnection(content(x))))
    lapply(seq_along(dfs)[1:(length(seq_along(dfs))-1)], function(x) {
        render_diff(diff_data(dfs[[x]], dfs[[x+1]]), view = TRUE)
    })
}

