library(readr)
library(yaml)
library(dplyr)

sim_folder_name <- '2016-07-12_093205_batch_watts'
# sim_dir_name <- sprintf('../mann2_output/%s', sim_folder_name)
sim_dir_name <- sprintf('/run/media/dchen/Untitled/mann2_output/%s',
                        sim_folder_name)

sim_dirs <- list.dirs(path = sim_dir_name,
                      full.names = TRUE,
                      recursive = TRUE)[-1][101:200]

read_sim_ticks_out <- function(sim_dir,
                               sim_ticks_file_name = 'output_sim_ticks.csv',
                               sim_yaml = 'config_model_watts.yaml') {
    # sim_dir <- sim_dirs[20]
    # sim_ticks_file_name = 'output_sim_ticks.csv'
    # sim_yaml = 'config_model_watts.yaml'
    sim_tick_out_df <- read_csv(sprintf('%s/%s', sim_dir, sim_ticks_file_name))
    config <- yaml.load_file(sprintf('%s/%s', sim_dir, sim_yaml))
    sim_tick_out_df$threshold <- config$agent$threshold
    sim_tick_out_df$run <- config$sim_generated_configs$run_number
    return(sim_tick_out_df)
}

list_dfs <- lapply(X = sim_dirs, FUN = read_sim_ticks_out)

# plyr::ldply(list_dfs, data.frame)
stacked_df <- plyr::rbind.fill(list_dfs)
rm(list_dfs)
gc()
# stacked_df <- do.call(what = 'rbind', args = list_dfs)

t24 <- stacked_df %>%
    filter(time == 999) %>%
    summarize(n_flipped = sum(state), p_flipped = n_flipped / n(), n = n())

t12
t24
