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
    result <- lapply(blobs[i, "sha"], function(sha){
        lookup(repo, sha)
    })
    dates <- blobs[i,'when']
    return(list(result, dates))
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

    blobs <- get_blobs(repo = repo, path = path)

    dfs <- lapply(blobs[[1]], function(x){
        read.csv2(textConnection(content(x)))
    })

    file_seq <- seq_along(dfs)[1:(length(seq_along(dfs))-1)]
    file_names <- unlist(lapply(file_seq,function(dummy){
        tempfile(fileext = ".html")
    }))

    diff_pages <- unlist(lapply(file_seq, function(x) {
        render_diff(diff_data(dfs[[x]], dfs[[x+1]]), view = FALSE, fragment = TRUE,file = file_names[x])
    }))

    full_table_pages <- unlist(lapply(seq_along(dfs), function(x){
        print(xtable(dfs[[x]]), type = 'html', print.results = FALSE)
    }))

## build into order for tabs on page

    output <- full_table_pages[1]

    for(i in file_seq + 1){
        output <- c(output,diff_pages[i-1],full_table_pages[i])
    }

    dates <- blobs[[2]]
    return(list(output, dates))
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

render_tables <- function(repo, path, view = TRUE){

    page_content <- make_tables(repo, path)
    tabs <- page_content[[1]]

    l1h <- '<div class="tab-content">'
    l2h <- '<div id="tab1" class="tab-pane fade in active">'
    l3h <- '<h3></h3>'
    l4h <- "<div class='highlighter'>"
    l5h <- '</div><!--Close device of highlighter class-->'
    l6h <- '</div><!--Close device of the current tab-->'

    l2l <- '<div id="ID_FIELD" class="tab-pane fade">'
    l3l <- '<h3></h3>'
    l4l <- "<div class='highlighter'>"
    l5l <- c('<script>','$(document).ready(function(){','    $(".nav-tabs a").click(function(){',
        "$(this).tab('show');",'    });','});','</script>)')
    l6l <- c('</div>','</div>','</div>','</div>')
    l7l <- c('</body>','</html>')

    l2d <- '<div id="ID_FIELD" class="tab-pane fade">'
    l3d <- '<h3></h3>'
    l4d <- "<div class='highlighter'>"
    l5d <- '</div><!--Close device of highlighter class-->'
    l6d <- '</div><!--Close device of the current tab-->'

    for(i in seq_along(tabs)){
        if(i %% 2 == 1){
            if(i == length(tabs)){
                tab_content <- c(tab_content, c(gsub("ID_FIELD", paste0("tab", i),l2l), l3l, l4l, rep("",2), tabs[i], rep("",2), l6l, l5l, l7l))
            }else{
                tab_content <- c(l1h, l2h, l3h, l4h, rep("",2),tabs[i], rep("",2), l5h, l6h, rep("",5))
            }
        }
        if(i %% 2 == 0){
        tab_content <- c(tab_content, c(gsub("ID_FIELD", paste0("tab", i), l2d), l3d, l4d, rep("",2), tabs[i], rep("",2), l5d, l6d), rep("",5))}
    }

    tl1 <- '<div class="container">'
    tl2 <- '<h2>Analysis of changes to csv file</h2>'
    tl3 <- '<ul class="nav nav-tabs">'
    tlfirsttab <- '<li class="active"><a href="#ID_FIELD">TITLE_FIELD</a></li>'
    tlmiddle <- '<li><a href="#ID_FIELD">TITLE_FIELD</a></li>'
    tl6 <- '</ul>'
    tab_definitions <- c(tl1, tl2, tl3, gsub("TITLE_FIELD","Original Data",gsub("ID_FIELD", "tab1",tlfirsttab)))

    for(i in seq_len(length(tabs)-2)+1){
        tab_definitions <- c(tab_definitions,gsub("TITLE_FIELD",paste0("Change", i-1),gsub("ID_FIELD",paste0("tab",i),tlmiddle)))
    }
        tab_definitions <- c(tab_definitions, gsub("TITLE_FIELD","Final Data",gsub("ID_FIELD",paste0("tab",length(tabs)),tlmiddle)), tl6)

    data(header)
    result <- c(header, tab_definitions, rep("", 5), tab_content)

    if(view) {
        filename <- tempfile(fileext = ".html")
        writeLines(result, filename)
        utils::browseURL(filename)
    }
    cat("Resultant html is invisible; if you want it assign it to an object\n")
    invisible(result)

}
