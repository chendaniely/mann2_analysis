library(yaml)
library(dplyr)

rm(list = ls())

source('./src/helper.R')

##
## find and load all config yaml files into a dataframe
##
configs <- list.files(path = file.path('..', 'mann2_output'),
                      pattern = 'config_model_watts.yaml',
                      full.names = TRUE,
                      recursive = TRUE)
## configs

# 600 configs is about 4.8 mb in memory
sim_configs <- lapply(configs, yaml.load_file)

unlisted_configs <- sapply(sim_configs, unlist)
unlisted_configs <- as.data.frame(t(unlisted_configs),
                                  stringsAsFactors = FALSE)

## lapply(unlisted_configs, table)

## var_interest <- c('agent.threshold', 'graph.generator')

## sapply(var_interest, function(x){table(unlisted_configs[x])})

## cell_counts <- table(unlisted_configs$agent.threshold,
##                      unlisted_configs$graph.generator)

## params <- as.data.frame(cell_counts, stringsAsFactor = FALSE)
## names(params) <- c(var_interest, 'Freq')
## params <- params[params$Freq > 0, ]
## print(dim(params))

drop_runs <- unlisted_configs[, !names(unlisted_configs) %in% c("sim_generated_configs.run",
                                                               "sim_generated_configs.run_number")]
params <- drop_runs[!duplicated(drop_runs), ]

save(params, file='output/params.RData')

sim_summary <- list()
edge_lists <- list()

for (p in 1:nrow(params)) {
    ## setup calculations
    df_param <- params[p, ]
    sims <- unlisted_configs[
        unlisted_configs$agent.threshold == df_param$agent.threshold &
      unlisted_configs$graph.generator == df_param$graph.generator, ]
    sim_num <- row.names(sims)
    param_configs <- configs[as.numeric(sim_num)]
    sim_dirs <- dirname(param_configs)

    ## get sim summary
    df <- all_param_sims(sim_dirs)
    sim_analysis <- df %>%
        filter(time == max(time)) %>%
        group_by(run) %>%
        summarize(mean = mean(state),
                  p_flipped = sum(state) / n())
    sim_summary[p] <- list(sim_analysis)

    ## get edge lists
    edge_lists[p] <- list(get_param_networks(sim_dirs))
}
## print(sim_summary)

save(sim_summary, file = 'output/sim_summary.RData')
save(edge_lists, file = 'output/edge_lists.RData')
