## library(plyr)
library(dplyr)
library(yaml)
print(3)

get_single_sim_df <- function(path,
                              sim_data_name='output_sim_ticks.csv',
                              config_name='config_model_watts.yaml'){
    data_path <- file.path(path, sim_data_name)
    config_path <- file.path(path, config_name)
    sim_config <- yaml.load_file(config_path)
    run <- sim_config$sim_generated_configs$run
    df <- read.csv(data_path, stringsAsFactor=FALSE)
    df$run <- run
    return(df)
}

all_param_sims <- function(paths){
    dfs <- lapply(paths, get_single_sim_df)
    df <- bind_rows(dfs)
    return(df)
}

summarize_sim <- function(all_param_sims_df){
    avg_final_state <- all_param_sims_df %>%
        filter(time = max(time)) %>%
        summarize(mean = mean(state))
}
