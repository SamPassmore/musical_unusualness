# Nice Network of Cantometric variables
suppressMessages({
  library(ggraph)
  library(igraph) 
  library(tidyverse)
  library(aricode)
  library(RCy3)
  library(patchwork)
})

make_network = function(data, min_cor = 0.3){
  correlation_matrix = cor(data)
  diag(correlation_matrix) = 0
  
  # save network object 1
  full_network = graph_from_adjacency_matrix(correlation_matrix,
                                                mode = "undirected",
                                                weighted = TRUE)
  
  ## Clusters
  clusters = cluster_spinglass(full_network, weights = E(full_network)$weight)
  clusters_df = data.frame(node = clusters$names,
                           cluster = clusters$membership)
  
  adj_correlationmatrix = correlation_matrix 
  adj_correlationmatrix[abs(adj_correlationmatrix) < min_cor] = 0
  
  ## If there are no correlations with other variables remove them
  adj_correlationmatrix = 
    adj_correlationmatrix[rowSums(adj_correlationmatrix) != 0,
                       colSums(adj_correlationmatrix) != 0]
  
  ig = graph_from_adjacency_matrix(adj_correlationmatrix,
                                   mode = "undirected",
                                   weighted = TRUE)
  # Graph layout
  layout <- layout.auto(ig) 
  
  # Colour nodes
  nodes = data.frame(name = V(ig)$name)
  nodes = left_join(nodes, cantometric_variables, c("name" = "id"))
  V(ig)$category = nodes$category
  V(ig)$cluster = clusters_df$cluster[clusters_df$node %in% V(ig)$name]
  V(ig)$shortname = nodes$shorttitle
  # E(ig)$weight = abs(E(ig)$weight)
  
  # Plot with same aesthetic adjustments as previous
  p = ggraph(ig, layout = "stress") +
    geom_edge_link(aes(width = weight, col = weight, alpha = weight)) + 
    scale_edge_width(range = c(0.25, 3)) + 
    scale_edge_color_continuous(low = "red", high = "blue", guide = "none") +
    geom_node_label(aes(label = shortname, fill = category), repel = FALSE) +
    theme_void() +
    theme(legend.position = "right",
          legend.title = element_blank()) +
    guides(color = "none", edge_width = "none", edge_alpha = "none",
           override.aes = aes(node_label = ""))
  list(full_network = full_network, adj_network = ig,
       plot_network = p, 
       correlation_matrix = correlation_matrix, clusters = clusters_df)
}



cantometrics = read.csv('processed_data/cleaned_cantometrics.csv')
lines_idx = grepl("line_[0-9]+", colnames(cantometrics))
cantometric_lines = cantometrics[,lines_idx]

## Remove dependent lines
dependent_lines = 
  paste0("line_", c(1,5,6,11,12,22,2,3,8,9,13,14,27))

cantometric_lines = cantometric_lines[!cantometric_lines %in% dependent_lines]

cantometric_variables = read.csv("data/cantometric_variables.csv")

## Network of everything
full_network = make_network(cantometric_lines)
## Correlations in the entire sample
correlation_allsongs = full_network$correlation_matrix 
summary(correlation_allsongs[lower.tri(correlation_allsongs)])
cor_threshold = 0.2
cat("There are",
    sum(
      correlation_allsongs[lower.tri(correlation_allsongs)] >
        cor_threshold
    ), 
    "pairwise correlations between musical features in the full",
    "observed sample that have an absolute correlation higher than",
    cor_threshold,
    "\n"
    )


write_graph(simplify(full_network$adj_network),  
            "full_network.gml", format = "gml")

## Network excluding solo singer songs
cantometric_solo = cantometric_lines %>%
  dplyr::filter(!line_1 %in% 2:3)
no_solo = make_network(cantometric_solo)

write_graph(simplify(no_solo$adj_network),  
            "no_solo.gml", format = "gml")

## Network excluding solo instrumentalists
cantometric_soloinst = cantometric_lines %>% 
  dplyr::filter(line_2 != 1)
no_instruments = make_network(cantometric_soloinst)

write_graph(simplify(no_solo$adj_network),  
            "no_solo.gml", format = "gml")

## Network of non-solo songs
cantometric_nosolo = cantometric_lines %>%
  dplyr::filter(line_2 != 1 & !line_1 %in% 2:3)
no_solosinginst = make_network(cantometric_nosolo)

write_graph(simplify(no_solosinginst$adj_network),  
            "no_solo_inst.gml", format = "gml")


#### Language family ####
#### indo1319: 1336 songs
european = cantometrics$Region == "Europe"
european_lines = cantometric_lines %>%
  dplyr::filter(european)
european_network = make_network(european_lines)

write_graph(simplify(european_network$adj_network),  
            "Europe_network.gml", format = "gml")

#### aust1307: 1123 songs
oceania = cantometrics$Region == "Oceania"
oceania_lines = cantometric_lines %>%
  dplyr::filter(oceania)
oceania_network = make_network(oceania_lines)

write_graph(simplify(oceania_network$adj_network),  
            "Oceania_network.gml", format = "gml")

#### atla1278: 497 songs
africa = cantometrics$Region == "Africa"
africa_lines = cantometric_lines %>%
  dplyr::filter(africa)
africa_network = make_network(africa_lines)

write_graph(simplify(africa_network$adj_network),  
            "africa_network.gml", format = "gml")

#### afro1255: 238 songs
seasia = cantometrics$Region == "Southeast Asia"
seasia_lines = cantometric_lines %>%
  dplyr::filter(seasia)
seasia_network = make_network(seasia_lines)

write_graph(simplify(seasia_network$adj_network),  
            "seasia_network.gml", format = "gml")


#### Society Networks ####
### Networks of the largest societies 
# cantometrics %>%
#   group_by(society_id) %>%
#   summarise(n_songs = n()) %>%
#   filter(n_songs >= 5) %>%
#   View()

# Grenadian 13223 (74 songs)
grenadian = cantometrics$society_id == 13223
grenadian_nosolo = cantometric_lines %>%
  dplyr::filter(grenadian)
grenadian_network = make_network(grenadian_nosolo)

# Ami 10506 (72 songs)
ami = cantometrics$society_id == 10506
ami_nosolo = cantometric_lines %>%
  dplyr::filter(ami)
ami_network = make_network(ami_nosolo)

# Trinidadian 27457 (57 songs) 
trinidadian = cantometrics$society_id == 27457
trinidadian_nosolo = cantometric_lines %>%
  dplyr::filter(trinidadian)
trinidadian_network = make_network(trinidadian_nosolo)

# Peinan 30034 (41 songs) (Taiwan)
peinan = cantometrics$society_id == 30034
peinan_nosolo = cantometric_lines %>%
  dplyr::filter(peinan)
peinan_network = make_network(peinan_nosolo)

#### Cluster comparison
### Network cluster comparison using Jackson et al. (2020)
all_networks = list(
  full_network,
  european_network,
  oceania_network,
  seasia_network,
  africa_network,
  grenadian_network, # West Indies
  trinidadian_network, # West Indies 
  ami_network, # Taiwan
  peinan_network # Taiwan
)
names(all_networks) = c(
  "global",
  "european",
  "oceania",
  "seasia",
  "africa",
  "grenadian",
  "trinidadian",
  "ami",
  "peinan")

### Add community detection 
all_networks = 
  lapply(all_networks,
         function(x){
           x$clusters = cluster_spinglass(x$full_network, 
                                          weights = E(x$full_network)$weight)
           x
         })

all_pairs = t(combn(names(all_networks), 2))

## Compare all Networks to all others
ARI_comparisons = 
  apply(all_pairs, 1,
        function(x){
          ARI(
            all_networks[[x[1]]]$clusters$membership,
            all_networks[[x[2]]]$clusters$membership
          )
        })

output = data.frame(all_pairs, ARI = round(ARI_comparisons, 2))

ggsave(all_networks$global$plot_network + ggtitle("Global"),
       file = 'figures/global_relationships.jpg')
ggsave(all_networks$oceania$plot_network + ggtitle("oceania"),
       file = 'figures/oceania_relationships.jpg')
ggsave(all_networks$european$plot_network + ggtitle("Indo-European"),
       file = 'figures/european_relationships.jpg')
ggsave(all_networks$grenadian$plot_network + ggtitle("Grenadian"),
       file = 'figures/grenadian_relationships.jpg')
ggsave(all_networks$ami$plot_network + ggtitle("Ami"),
       file = 'figures/ami_relationships.jpg')

