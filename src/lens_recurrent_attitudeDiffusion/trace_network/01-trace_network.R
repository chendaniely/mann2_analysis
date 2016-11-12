library(igraph)
library(readr)
library(dplyr)
library(stringr)

rm(list = ls())

source('src/lens_recurrent_attitudeDiffusion/trace_network/utils_trace.R')

sim_data_file <- '../mann2_simulations/src/simulations/lens_recurrent_attitudeDiffusion/output_sim_ticks.csv'
sim_edgelist_file <- '../mann2_simulations/src/simulations/lens_recurrent_attitudeDiffusion/edge_list_nx.gz'

g_arrow_size <- .5

# TRUE FALSE both
save_plots <- TRUE
show_plots <- TRUE
g_plot_seed <- 42

sim_df <- read_csv(sim_data_file)
sim_df$agent_update <- as.numeric(sim_df$agent_update)

el <- read.table(gzfile(sim_edgelist_file), header = T)
el_m <- as.matrix(el)[, 1:2]

g <- graph.edgelist(el = el_m, directed = TRUE)
V(g)$color = 'white'
V(g)$name <- str_trim(V(g)$name )

plot_graph(g, title_t = -2)

seeded_agent <- as.character(sim_df[sim_df$time == -1, 'aid'])

igraph::degree(g, mode = 'in')
igraph::degree(g, mode = 'out')

paths <- vector('list', length(unique(sim_df$time)))

paths[[1]] <- seeded_agent
head(paths)
# names(paths) <- sort(unique(sim_df$time))
# paths <- list('-1' = seeded_agent)

g <- color_node(g, seeded_agent, 'red')
plot_graph(g, -1)

for (t in sort(unique(sim_df$time))[-1]) {
    print(sprintf('looking at t: %s', t))
    i <- t + 2
    # i

    time <- sim_df[sim_df$time == t, ]

    pt <- i - 1
    previous <- paths[[pt]]
    # previous

    ## If the previous time does not have any agents, keep looking back
    ## until an agent is found
    if (length(previous) == 0) {
        for (pi in pt:1) {
            previous <- paths[[pi]]
            # print(sprintf('looking back to %s', pi))
            if (length(previous) > 0) {break}
        }
    }

    path <- time[time$agent_update %in% previous, 'aid'] %>% collect %>% .[['aid']]
    path
    paths[[i]] <- path

    paths

    set.seed(42)
    g <- color_node(g, path, 'red')
    plot_graph(g, t)

    # V(g)$color[path + 1] <- 'red'
    # plot.igraph(g, edge.arrow.size = g_edge.arrow.size)

    # if (t == 2) break
}

# gif the images
if (save_plots) {system('convert -delay 100 -loop -1 output/trace_network/plot_*.png out.gif')}

save(g, paths, seeded_agent, file = 'output/trace_network/graph_and_paths.RData')
