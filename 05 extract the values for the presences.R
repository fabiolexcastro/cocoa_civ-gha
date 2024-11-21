

# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, fs, colourpicker, pROC, pvclust, RColorBrewer, dendextend, randomForest, ggdendro, usdm, sf, glue, gtools, tidyverse, rnaturalearthdata, rnaturalearth, dismo, raster)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

# Random forest results
fles <- dir_ls('rf/output/run_1/results/raw', regexp = '.asc$')
prob <- terra::rast(grep('Prob', fles, value = T))
clst <- terra::rast(grep('Clust', fles, value = T))
uncr <- terra::rast(grep('Unc', fles, value = T))

# Presence data
load(file = 'rData/run_1/clustereddata.rData')
prsc <- clusteredpresdata

# Climate data 
clma <- terra::rast('tif/climate/westafrica_baseline/bioc_allv.tif')
vars <- readRDS(file = 'rds/run_1/vars_vif.rds')
clma <- clma[[grep(paste0(paste0(vars, '$'), collapse = '|'), names(clma), value = T)]]

# To make the graph  ------------------------------------------------------
prsc <- mutate(prsc, cluster = factor(cluster, levels = as.character(1:5)))
dfrm <- prsc %>% 
  gather(var, value, -x, -y, -cluster, -pb) %>% 
  mutate(var = factor(var, levels = vars), 
         cluster = factor(cluster, levels = as.character(1:5))) 
dfrm <- inner_join(dfrm, dfrm %>% distinct(var) %>% mutate(letters = letters[1:13]), by = 'var')

g1 <- ggplot(data = dfrm, aes(x = cluster, y = value, fill = cluster)) + 
  facet_wrap(.~letters,  scales = 'free_y') +
  geom_boxplot() + 
  scale_fill_manual(values = c('#DFC27E', '#EADCB9', '#98CDC4', '#BBEBF9', '#48CEB6')) +
  labs(x = 'Climatic type', y = 'Value') +
  ggtitle(label = 'Bioclimatic values for each cluster') + 
  theme_classic() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        legend.position = 'none',
        strip.text = element_text(face = 'bold'),
        plot.title = element_text(face = 'bold', hjust = 0.5))

ggsave(plot = g1, filename = 'png/graphs/boxplot_run1_v2.png', units = 'in', width = 9, height = 9, dpi = 300)

# Another way to make the graph  ------------------------------------------
nmes <- read_csv('./labels_bios.csv')
nmes <- nmes %>% setNames(c('rmv', 'vari', 'meaning'))
nmes[13,3] <- 'Mean temp in wettest semester (°C)'
dfrm <- inner_join(dfrm, nmes, by = c('var' = 'vari'))
dfrm <- dfrm %>% mutate(var = factor(var, levels = c(nmes$vari)))


# Function to use ---------------------------------------------------------
varis <- unique(dfrm$var)
varis <- as.character(varis)

make.graph <- function(vari){
  
  dfr <- dfrm %>% filter(var == vari)
  ltt <- dfr %>% pull(letters) %>% unique()
  mnn <- dfr %>% pull(meaning) %>% unique()
  
  ggp <- ggplot(data = dfr, aes(x = cluster, y = value, fill = cluster)) + 
    geom_boxplot() + 
    scale_fill_manual(values = c('#DFC27E', '#EADCB9', '#98CDC4', '#BBEBF9', '#48CEB6')) +
    labs(x = 'Climatic type', y = mnn) +
    ggtitle(label = paste0(ltt, ')')) + 
    theme_classic() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          legend.position = 'none',
          strip.text = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold'))
  
  return(ggp)
  
  
}

# To apply the function ---------------------------------------------------
ggpl <- map(unique(dfrm$var), make.graph)
library(ggpubr); library(cowplot)

ggal <- ggarrange(ggpl[[1]], ggpl[[2]], ggpl[[3]], ggpl[[4]], ggpl[[5]], ggpl[[6]],
                  ggpl[[7]], ggpl[[8]], ggpl[[9]], ggpl[[10]], ggpl[[11]], ggpl[[12]],
                  ggpl[[13]], ncol = 4, nrow = 4)

ggsave(plot = ggal, filename = './graph_box_v5.jpg', units = 'in', width = 11, height = 11, dpi = 300)



