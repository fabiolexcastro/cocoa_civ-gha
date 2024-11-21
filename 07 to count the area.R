

# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, fs, colourpicker, pROC, pvclust, RColorBrewer, colourpicker, dendextend, randomForest, ggspatial, ggdendro, usdm, sf, glue, rgeos, gtools, tidyverse, rnaturalearthdata, rnaturalearth, dismo, raster)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

# Random forest results
fles <- dir_ls('rf/output/run_1/results/process', regexp = '.tif$')
clst <- grep('unc', fles, value = T) %>% terra::rast()

# To count the area -------------------------------------------------------
names(clst) <- glue('aez_bnsl')

frqn <- freq(clst) %>% mutate(class = c('Unsuit', 'Unsuit', 'Moderate - Very dry', 'Hot - Dry', 'Hot - Wet', 'Cold - Wet', 'Very hot - Very wet', 'Limitations', 'Mixed'))
frqn <- dplyr::select(frqn, class, count)
frqn <- mutate(frqn, porc = count / sum(count) * 100, porc = round(porc, digits = 0))
sum(frqn$porc)

frqn <- frqn %>% group_by(class) %>% summarise(count = sum(count)) %>% ungroup() %>% mutate(porc = count / sum(count) * 100)
frqn <- frqn %>% mutate(porc = round(porc, 1))
frqn <- frqn %>% arrange(desc(count))
frqn <- mutate(frqn, class = factor(class, levels = frqn$class))
frqn <- mutate(frqn, class = factor(class, levels = c('Unsuit', 'Moderate - Very dry', 'Hot - Dry', 'Hot - Wet', 'Cold - Wet', 'Very hot - Very wet', 'Limitations', 'Mixed')))

write.csv(frqn, './tbl/models/frqn_aez_bsl2.csv', row.names = FALSE)

# To make the graph -------------------------------------------------------
gfrq <- ggplot(data = frqn, aes(x = class, y = porc, fill = class)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  geom_text(aes(label = porc), position = position_dodge(width = 0.9), vjust = -0.25) +
  scale_fill_manual(values = c('#F5F0F0', '#7A772A', '#DE8E16', '#3C8548', '#14AFC7', '#D929D0', '#878787', '#D1C98A')) +
  ggtitle(label = 'Count area in percentage for agroclimatic zones in CIV - GHA') +
  labs(x = 'Agroclimatic zone', y = 'Percentage (%)', fill = 'AEZ', caption = 'Random forest model') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        plot.title = element_text(face = 'bold', hjust = 0.5),
        text = element_text(family = 'Gill Sans MT'))
gfrq
ggsave(plot = gfrq, filename = './png/graphs/bar_area_aes-bsl_run1.png', units = 'in', width = 9, height = 7, dpi = 300)
