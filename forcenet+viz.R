
edgelist= topics_n %>%
  filter(weight >= .01) %>%
  arrange(target)

sources <- edgelist$source
targets <- edgelist$target
node_names <- factor(sort(unique(c(as.character(sources), 
                                   as.character(targets)))))



groups = edgelist %>% group_by(target) %>% top_n(1, weight)
groups = groups$source
nodes <- data.frame(name = node_names, group = c(1:num_clusters, groups), size = 8)
links <- data.frame(source = match(sources, node_names) - 1, 
                    target = match(targets, node_names) - 1, 
                    value = edgelist$weight)
forceNetwork(Links = links, Nodes = nodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.9, zoom = T, legend = T)
