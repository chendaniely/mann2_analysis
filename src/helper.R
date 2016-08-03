library(readr)
library(dplyr)
library(yaml)
## library(foreach)
## library(doParallel)

get_sim_run_number <- function(path,
                               config_name='config_model_watts.yaml') {
    config_path <- file.path(path, config_name)
    sim_config <- yaml.load_file(config_path)
    run <- sim_config$sim_generated_configs$run
    return(run)
}

get_single_sim_df <- function(path,
                              sim_data_name='output_sim_ticks.csv',
                              config_name='config_model_watts.yaml') {
    data_path <- file.path(path, sim_data_name)
    run <- get_sim_run_number(path, config_name)
    df <- read_csv(data_path)
    df$run <- run
    return(df)
}

all_param_sims <- function(paths) {
    ## cores <- detectCores() - 1
    ## cl <- makeCluster(cores)
    ## clusterExport(cl, varlist = ls())
    ## registerDoParallel(cl)
    dfs <- lapply(paths, get_single_sim_df)
    ## stopCluster(cl)
    ## registerDoSEQ()
    df <- bind_rows(dfs)
    return(df)
}

summarize_sim <- function(all_param_sims_df) {
    avg_final_state <- all_param_sims_df %>%
        filter(time = max(time)) %>%
        summarize(mean = mean(state))
}

get_single_sim_edge_list <- function(path,
                                     sim_edge_list_name='edge_list_nx.gz',
                                     config_name='config_model_watts.yaml') {
    edge_list_path <- file.path(path, sim_edge_list_name)
    tryCatch({
        el_df <- read_delim(edge_list_path, delim = ' ',
                            col_names = c('source', 'target', 'na'),
                            na = c("{}"))
        el_df <- el_df[, ! names(el_df) %in% c('na'), drop = FALSE]
        return(el_df)
    }, error = function(err){
        d <- data.frame('source'=NA, 'target'=NA)
        return(d)
    })
}

get_param_networks <- function(paths) {
    edge_lists_df <- lapply(paths, get_single_sim_edge_list)
}
