library(stringr)
library(tsne)
library(ggplot2)

cantometrics = read.csv('processed_data/cantometrics_wunusualness_std.csv')

line_idx = str_detect(colnames(cantometrics), "line_[1-9]{1,2}_new")

cantometrics_dist = dist(cantometrics[,line_idx])

## Including quasi-redundant variables
mds_out = cmdscale(cantometrics_dist)
tsne_out = tsne(cantometrics_dist)

plot_df = data.frame(
  x_mds = mds_out[,1],
  y_mds = mds_out[,2],
  x_tsne = 
  y_tsne = 
  )

plot_df = cbind(plot_df, cantometrics)

p1 = ggplot(data = plot_df,
       aes(
         x = x,
         y = y,
         col = unusualness_langaugefamily
       )) + 
  geom_point(alpha = 0.5) + 
  scale_color_viridis_c(option = "magma") + 
  theme_linedraw() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank())

ggsave(plot = p1,
       filename = "figures/mds_allcantovariables.jpg")

### Without Quasi-redundant variables

rm_vr = c(1, 4, 5, 6, 11, 22, 2, 3, 7, 8, 9, 13, 14, 27)
remove_vars = paste0("line_", rm_vr, "_new")

line_idx = str_detect(colnames(cantometrics), "line_[1-9]{1,2}_new")
use_cols = colnames(cantometrics)[line_idx]
use_cols = use_cols[!use_cols %in% remove_vars]

cantometrics_dist_worv = dist(cantometrics[,use_cols])

mds_out_woqr = cmdscale(cantometrics_dist_worv)
tsne_out_woqr = tsne(cantometrics_dist_worv)

plot_dfworv = data.frame(
  x_mds = mds_out[,1],
  y_mds = mds_out[,2],
  x_tsne = 
  y_tsne = 
)

plot_dfworv = cbind(plot_dfworv, cantometrics)

p2 = ggplot(data = plot_dfworv,
           aes(
             x = x,
             y = y,
             col = unusualness_langaugefamily
           )) + 
  geom_point(alpha = 0.5) + 
  scale_color_viridis_c(option = "magma") + 
  theme_linedraw() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank())

ggsave(plot = p2,
       filename = "figures/mds_worcantovariables.jpg")

#### All Cantometric variables coloured by Line 2 (Relationship of orchestra to vocal parts)

p3 = ggplot(data = plot_df,
            aes(
              x = x,
              y = y,
              col = factor(line_3)
            )) + 
  geom_point(alpha = 0.5) + 
  theme_linedraw() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank())

# line 16
# Line 18
ggplot(data = plot_dfworv,
            aes(
              x = x,
              y = y,
              col = factor(line_16)
            )) + 
  geom_point(alpha = 0.5) + 
  theme_linedraw() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank())
