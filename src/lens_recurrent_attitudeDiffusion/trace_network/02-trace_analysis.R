library(igraph)
library(readr)
library(lsa)
library(dplyr)
library(ggplot2)

rm(list = ls())

source('src/lens_recurrent_attitudeDiffusion/trace_network/utils_trace.R')

load('output/trace_network/graph_and_paths.RData')

sv <- as.character(unlist(paths))

sg <- induced.subgraph(graph = g,vids = sv)

set.seed(42)
plot(sg)

leaves <- which(degree(sg, v = V(sg), mode = "out") == 0, useNames = TRUE)

spaths <- all_simple_paths(graph = sg, from = as.character(seeded_agent), to = names(leaves))

sim_data_file <- '../mann2_simulations/src/simulations/lens_recurrent_attitudeDiffusion/output_sim_ticks.csv'
sim_df <- read_csv(sim_data_file)

first_states_df <- as.data.frame(matrix(nrow = length(unique(names(unlist(spaths)))), ncol = length(names(sim_df))))
names(first_states_df) <- names(sim_df)

# used to assign row in first_states_df
agent_num <- 1

for (i in 1:length(paths)) {
    print(i)
    t <- i - 2
    print(t)

    agents <- paths[[i]]

    for (agent_id in agents) {
        print(agent_id)
        agent_first_values <- sim_df[sim_df$time == t & sim_df$aid == agent_id, ]
        # dat[[agent_num]] <- agent_first_values
        first_states_df[agent_num, ] <- agent_first_values
        agent_num <- agent_num + 1
    }

    # if (i == 10) break
    # if (length(t) == 0) {next}

}
first_states_df

# dat

path_dist_df <- as.data.frame(matrix(nrow = length(spaths), ncol = 3))
names(path_dist_df) <- c('source', 'leaf', 'distance')
path_dist_df

# get distances
for (i in 1:length(spaths)) {
    p <- spaths[[i]]
    # print(p)
    # print(length(p))

    s <- p[1]
    s <- names(s)
    # s

    t <- p[length(p)]
    t <- names(t)
    # t

    d <- length(p)
    # d

    path_dist_df[i, ] <- c('source' = s,
                          'target' = t,
                          'distance' = d)
}

path_dist_df

path_dist_df$first_update_cosine <- apply(X = path_dist_df, MARGIN = 1,
                                          FUN = calculate_prototype_cosine_similarity, first_state_data = first_states_df,
                                          prototype = c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                                                        0, 0, 0, 1, 1, 0, 1, 0, 0, 0))

path_dist_df <- path_dist_df %>%
    group_by(source, leaf) %>%
    summarize(max_distance = max(distance),
              first_update_cosine = unique(first_update_cosine))

ggplot(data = path_dist_df) +
    geom_point(aes(x = max_distance, y = first_update_cosine))

ggsave(filename = 'output/trace_network/final_state_cosine_similarity.png')
