library(igraph)
library(stringr)

rm(list = ls())

load('output/params.RData')
load('output/edge_lists.RData')

params[1, ]

el_mat <- lapply(X = edge_lists[[1]], as.matrix)

el_mat2 <- lapply(X = edge_lists[[1]], '+', 1)

g_list <- lapply(x = edge_lists[[1]], FUN = function(x){print(x)})


for (thing in edge_lists[[1]]){
    g = graph.edgelist(as.matrix(thing) + 1, directed = TRUE)
    print(summary(degree(g)))
}

g = graph.edgelist(as.matrix(el_mat2[[1]]), directed = TRUE)

## add param info to edge list
## for each row
for (i in 1:nrow(params)) {
    curr_edge_list_df <- edge_lists[[i]]
    curr_params <- params[i, ]

    ## for each column
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
    curr_edge_list_df <- curr_edge_list_df[, !names(curr_edge_list_df) %in% c('graph.generator'),
                                           drop = FALSE]
    ## print(head(curr_edge_list_df))
    el_mat <- as.matrix(curr_edge_list_df)#apply(as.matrix(curr_edge_list_df), 2, as.numeric)
    ## print(head(el_mat))
    edge_lists[[i]] <- el_mat
    ## break
}

t <- apply(edge_lists[[1]][, c(1:2)], 2, as.numeric) + 1

g = graph.edgelist(as.matrix(t), directed = TRUE)

data <- degree(g)
hist(data, breaks = 20)
curve(dnorm(x, mean=mean(data), sd=sd(data)),
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

