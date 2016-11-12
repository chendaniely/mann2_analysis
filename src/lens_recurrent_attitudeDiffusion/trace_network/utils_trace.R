color_node <- function(g, nodes, color) {
    # nodes <- seeded_agent
    # color <- 'red'
    #  V(g)$name
    selected_nodes <- V(g)$name %in% as.character(nodes)
    V(g)[selected_nodes]$color <- color
    return(g)
}

plot_graph <- function(g, title_t, save=save_plots, show=show_plots, seed=g_plot_seed, g_edge.arrow.size=g_arrow_size) {
    title_f <- sprintf('Time: %03d', title_t)

    if (save) {png(sprintf('output/trace_network/plot_%03d.png', title_t))}

    set.seed(seed)
    plot(g, edge.arrow.size = g_edge.arrow.size)
    title(main = title_f)

    if (save) {dev.off()}

    if (show) {
        set.seed(seed)
        plot(g, edge.arrow.size = g_edge.arrow.size)
        title(main = title_f)
    }
}

calculate_prototype_cosine_similarity <- function(path_dist, first_state_data, prototype) {
    # print(path_dist)
    s <- path_dist['source']
    # print(s)
    l <- path_dist['leaf']

    # source_state <- first_state_data %>% filter(aid == s) %>% select(pos_0:neg_9)
    leaf_state <- first_state_data %>% filter(aid == l) %>% select(pos_0:neg_9)

    # print(prototype)
    # print(leaf_state)

    dist <- cosine(x = prototype, y = as.numeric(leaf_state))

    return(dist)
    # return(as.numeric(s) + as.numeric(l))
}
