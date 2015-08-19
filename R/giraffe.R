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
    file_seq <- seq_along(dfs)[1:(length(seq_along(dfs))-1)]
    file_names <- unlist(lapply(file_seq,function(dummy){tempfile(fileext = ".html")}))
    pages <- unlist(lapply(file_seq, function(x) {
        render_diff(diff_data(dfs[[x]], dfs[[x+1]]), view = FALSE, fragment = TRUE,file = file_names[x])
    }))
    page1 <- print(xtable(dfs[[1]]), type = "html", print.results = FALSE)
    pagelast <- print(xtable(dfs[[length(dfs)]]), type = "html", print.results = FALSE)
    pages <- c(page1, pages, pagelast)
    return(pages)
}

#' Make a webpage of the tables
#'
#'  @param repo A git2R repo object
#'  @param path A relative path to the table that you are interested in
#'  @description None
#'  @import xtable
#'  @export
#'  @examples
#'  repo <- read_sample_repo()
#'  render_tables(repo = repo, path = "test.csv")

render_tables <- function(repo, path){

    tabs <- make_tables(repo, path)

    l1h <- '<div class="tab-content">'
    l2h <- '<div id="initial" class="tab-pane fade in active">'
    l3h <- '<h3>DATE OF INITIAL COMMIT</h3>'
    l5h <- '</div>'

    l2l <- '<div id="final" class="tab-pane fade">'
    l3l <- '<h3>DATE OF FINAL COMMIT</h3>'
    l5l <- '</div></div></div></body></html>'

    l2d <- '<div id="ID_FIELD" class="tab-pane fade">'
    l3d <- '<h3>DATE OF COMMIT</h3>'
    l5d <- '</div>'

    for(i in seq_along(tabs)){
        if(i == 1){
        tabs[i] <- paste0(l1h, l2h, l3h, tabs[i], l5h)}
        if(i == length(tabs)){
        tabs[i] <- paste0(l2l, l3l, tabs[i], l5l)}
        if(i>1 & i<length(tabs)){
        tabs[i] <- paste0(gsub("ID_FIELD", paste0("tab", i), l2d), l3d, tabs[i], l5d)}
    }

    tl1 <- '<div class="container">'
    tl2 <- '<h2>NAME OF .CSV FILE AND REPOSITORY</h2>'
    tl3 <- '<ul class="nav nav-tabs">'
    tlfirsttab <- '<li class="active"><a data-toggle="tab" href="#ID_FIELD">TITLE_FIELD</a></li>'
    tlmiddle <- '<li><a data-toggle="tab" href="#ID_FIELD">TITLE_FIELD</a></li>'
    tl6 <- '</ul>'
    tab_definitions <- paste0(tl1, tl2, tl3, gsub("TITLE_FIELD","Original Data",gsub("ID_FIELD", "tab1",tlfirsttab)))

    for(i in seq_len(length(tabs)-2)+1){
        tab_definitions <- c(tab_definitions,gsub("TITLE_FIELD",paste0("Change", i-1),gsub("ID_FIELD",paste0("tab",i),tlmiddle)))
    }
        tab_definitions <- c(tab_definitions, gsub("TITLE_FIELD","Final Data",gsub("ID_FIELD",paste0("tab",length(tabs)),tlmiddle)))

    data(header)
    result <- c(header, tab_definitions, tabs)

return(result)

}
