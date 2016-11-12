library(readr)
library(yaml)
library(igraph)

sim_folder_name <- '2016-07-12_093205_batch_watts'
# sim_dir_name <- sprintf('../mann2_output/%s', sim_folder_name)
sim_dir_name <- sprintf('/run/media/dchen/Untitled/mann2_output/%s',
                        sim_folder_name)

sim_dirs <- list.dirs(path = sim_dir_name,
                      full.names = TRUE,
                      recursive = TRUE)[-1][101:200]

read_sim_edge_list <- function(sim_dir,
                               sim_ticks_file_name = 'edge_list_nx.gz',
                               sim_yaml = 'config_model_watts.yaml') {
    # sim_dir <- sim_dirs[20]
    # sim_ticks_file_name = 'edge_list_nx.gz'
    # sim_yaml = 'config_model_watts.yaml'
    fname <- sprintf('%s/%s', sim_dir, sim_ticks_file_name)
    sim_tick_out_df <- read_delim(fname, delim = ' ', col_names = FALSE)
    config <- yaml.load_file(sprintf('%s/%s', sim_dir, sim_yaml))
    sim_tick_out_df$threshold <- config$agent$threshold
    sim_tick_out_df$run <- config$sim_generated_configs$run_number
    return(sim_tick_out_df)
}


#
g <- igraph::graph.data.frame(sim_tick_out_df)
degree(g)
plot(degree_distribution(g))
