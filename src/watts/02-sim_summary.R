##
## Uses output from `batch.R`
##
##
##
##
##
##
##
##

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

grouped_means <- all_df %>%
    group_by(e_degree, agent.threshold) %>%
    summarize(mean = mean(p_flipped))

g <- ggplot(data = all_df, aes(x = p_flipped)) +
    theme_minimal() +
    geom_histogram(binwidth = 0.1) +
    geom_vline(data = grouped_means, aes(xintercept = mean), color = 'red', size = .5) +
    geom_text(data = grouped_means, aes(x = 0.50, y = 50, label = round(mean, 3)),size = 2.5) +
    scale_x_continuous(breaks = c(0, 0.50, 1.00)) +
    scale_y_continuous(breaks = c(0, 50, 100)) +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5, size = 7),
          axis.text.y  = element_text(vjust = 0.5, size = 5),
          strip.text.x = element_text(size = 7),
          strip.text.y = element_text(size = 6),
          axis.ticks = element_line(size = 2)) +
    ggtitle('Proportion and average Number of Flipped Agents by threshold and expected average degree centrality') +
    xlab('Proportion of Agents with State 1 in last time tick') +
    ylab('Counts over 100 realizations of the network')

g + facet_grid(e_degree ~ agent.threshold,
               drop = FALSE,
               scales = 'fixed', as.table = FALSE)
ggsave(filename = 'output/p_flipped_all.png', dpi = 300, width = 10, height = 8)

g + facet_grid(e_degree ~ agent.threshold,
               scales = 'fixed', as.table = FALSE)
ggsave(filename = 'output/p_flipped_dropped.png', dpi = 300, width = 10, height = 8)
