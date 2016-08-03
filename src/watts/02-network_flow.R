library(igraph)
library(stringr)

rm(list = ls())

load('output/params.RData')
load('output/edge_lists.RData')

params[1, ]

for (i in 1:nrow(params)) {
    curr_edge_list_df <- edge_lists[[i]]
    curr_params <- params[i, ]
    for (j in 1:length(names(params))) {
        new_col_name <- names(params)[j]
        curr_edge_list_df[, new_col_name] <- params[i, j]
    }
    # print(table(curr_edge_list_mat$agent.threshold, useNA = 'always'))
    ##sim_summary[[i]] <- curr_df
    print(head(curr_edge_list_df))
    get_p <- sapply(str_split(curr_edge_list_df$graph.generator,
                              'p='), '[[', 2)
    curr_edge_list_df$graph_p <- str_replace(string = get_p,
                                  pattern = '\\)',
                                  replacement = '')
    curr_edge_list_df <- curr_edge_list_df[, ! names(curr_edge_list_df) %in% c('graph.generator'),
                                           drop = FALSE]
    ## print(head(curr_edge_list_df))
    el_mat <- apply(as.matrix(curr_edge_list_df), 2, as.numeric)
    ## print(head(el_mat))
    edge_lists[[i]] <- el_mat
    ## break
}

t <- edge_lists[[1]][, c(1:2)]

g = graph.edgelist(edge_lists[[1]][, c(1:2)] + 1, directed = TRUE)
