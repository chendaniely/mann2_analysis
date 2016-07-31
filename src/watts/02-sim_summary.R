library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

rm(list = ls())

load('output/sim_summary.RData')
load('output/params.RData')

for (i in 1:nrow(params)) {

    curr_df <- sim_summary[[i]]
    curr_params <- params[i, ]
    for (j in 1:length(names(params))) {
        new_col_name <- names(params)[j]
        curr_df[, new_col_name] <- params[i, j]
    }
    print(table(curr_df$agent.threshold, useNA = 'always'))
    sim_summary[[i]] <- curr_df
}

all_df <- bind_rows(sim_summary)

get_p <- sapply(str_split(all_df$graph.generator, 'p='), '[[', 2)
all_df$graph_p <- str_replace(string = get_p,
                              pattern = '\\)',
                              replacement = '')

all_df$e_degree <- as.numeric(all_df$graph_p) * 1000
all_df$e_degree <- factor(all_df$e_degree,
                          levels = c(seq(0, 15, 1), 500))

all_df$graph_p <- factor(all_df$graph_p)

all_df$agent.threshold <- factor(all_df$agent.threshold,
                                 levels = seq(0.10, 0.25, 0.01))



ggplot(data = all_df, aes(x = p_flipped)) +
    theme_minimal() +
    geom_histogram(binwidth = 0.1) +
    #geom_density() +
    scale_x_continuous(breaks = c(0, 0.50, 1.00)) +
    scale_y_continuous(breaks = c(0, 50, 100)) +
    facet_grid(e_degree ~ agent.threshold,
               drop = FALSE,
               scales = 'fixed', as.table = FALSE) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5))
