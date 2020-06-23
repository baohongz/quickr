# Create a "balloon plot" as alternative to a heatmap with ggplot2
# 
# January 2017
# Author: Markus Konrad <markus.konrad@wzb.eu>, WZB Berlin Social Science Center

library(dplyr)
library(tidyr)
library(ggplot2)

# define the variables that will be displayed in the columns
vars <- c('awake', 'sleep_total', 'sleep_rem')

# prepare the data: we use the "msleep" dataset which comes with ggplot2
df <- msleep[!is.na(msleep$vore), c('name', 'vore', vars)] %>%  # only select the columns we need from the msleep dataset
  group_by(vore) %>% sample_n(5) %>% ungroup() %>%              # select 5 random rows from each "vore" group as subset
  gather(key = variable, value = value, -name, -vore) %>%       # make a long table format: gather columns in rows
  filter(!is.na(value)) %>%                                     # remove rows with NA-values -> those will be empty spots in the plot
  arrange(vore, name)                                           # order by vore and name

# add a "row" column which will be the y position in the plot: group by vore and name, then set "row" as group index
df <- df %>% mutate(row = group_indices_(df, .dots=c('vore', 'name')))
# add a "col" column which will be the x position in the plot: group by variable, then set "col" as group index
df <- df %>% mutate(col = group_indices_(df, .dots=c('variable')))

# get character vector of variable names for the x axis. the order is important, hence arrange(col)!
vars_x_axis <- c(df %>% arrange(col) %>% select(variable) %>% distinct())$variable
# get character vector of observation names for the y axis. again, the order is important but "df" is already ordered
names_y_axis <- c(df %>% group_by(row) %>% distinct(name) %>% ungroup() %>% select(name))$name

# now plot
# make color dependent on vore, size and alpha dependent on value
# x and y must be set as factor() otherwise scale_x/y_discrete() won't work
fig = ggplot(df, aes(x=factor(col), y=factor(row), color=vore, size=value, alpha=value)) +
  geom_point() +    # plot as points
  geom_text(aes(label=value, x=col + 0.25), alpha=1.0, size=3, show_guide=F) +   # display the value next to the "balloons"
  scale_alpha_continuous(range=c(0.3, 0.7)) +
  scale_size_area(max_size = 5) +
  scale_x_discrete(breaks=1:length(vars_x_axis), labels=vars_x_axis, position='top') +   # set the labels on the X axis
  scale_y_discrete(breaks=1:length(names_y_axis), labels=names_y_axis) +                 # set the labels on the Y axis
  theme_bw() +
  theme(axis.line = element_blank(),            # disable axis lines
        axis.title = element_blank(),           # disable axis titles
        panel.border = element_blank(),         # disable panel border
        panel.grid.major.x = element_blank(),   # disable lines in grid on X-axis
        panel.grid.minor.x = element_blank())   # disable lines in grid on X-axis


png("balloon.png", width=600, height=600, res=120)
print(fig)
dev.off()

pdf("balloon.pdf", width = 8, height = 8, useDingbats=F)
print(fig)
dev.off()

