library(dplyr)
library(egor)
library(ggplot2)
library(ggsci)
library(tidyr)
library(viridis)
library(scales)
library(igraph)
library(RColorBrewer)

#set the current working directory
setwd("/Users/.../Egonet_Compon")

#load the RData file saved from EgoNet_analysis.R
load("./output/EgoNet_analysis.RData")

# ————————— 0. nomination counts —————————
#### make a circled bar chat for the number of nominations for each sector
infl_count_data <- infl_alter_count[, c('alter', 'infl_n', 'sector')]
#this is the function to plot the circled bar chart, grouped by a variable
plot_circled_bar <-  function (data, n_var, group_var){
  #rename the column name of the target variable as "group"
  colnames(data)[colnames(data) == group_var] ="group"
  colnames(data)[colnames(data) == n_var] ="value"

  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 3
  to_add <- data.frame( matrix(NA, empty_bar*length(unique(data$group)), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$group <- rep(unique(data$group), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(group, value)
  data$id <- seq(1, nrow(data))

  # Get the name and the y position of each label
  label_data <- data
  number_of_bar <- nrow(label_data)
  angle <- 90 - (360 * (label_data$id-0.5) /number_of_bar )    # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse(angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)

  # prepare a data frame for base lines
  base_data <- data %>%
    group_by(group) %>%
    summarize(start=min(id), end=max(id) - empty_bar) %>%
    rowwise() %>%
    mutate(title=mean(c(start, end)))

  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]

  # Make the plot

  p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
    scale_fill_npg() +

    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 5, xend = start, yend = 5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 3, xend = start, yend = 3), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +

    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = rep(max(data$id),4), y = c(1, 3, 5, 10), label = c("1", "3", "5", "10") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +

    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
    ylim(-6,21) + #make sure ylim the upper value is larger than the largest value in the data, otherwise long bars will be cut off
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(c(-3,-5,-3,-5), "cm")
    ) +
    coord_polar() +
    geom_text(data=label_data, aes(x=id, y=value+0.5, label=alter, hjust=hjust), color="black",
              alpha=0.5, size=2, angle= label_data$angle, inherit.aes = FALSE ) +

    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -0.5, xend = end, yend = -0.5), colour = "black", alpha=0.6, size=0.5 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -1.5, label=group), colour = "black", alpha=0.6, size=3, fontface="bold", inherit.aes = FALSE)
  p
}

png("./output/circled_bar.png",
      width = 2900, height = 2400, units = 'px', res = 300)
p <- plot_circled_bar(infl_count_data, 'infl_n', 'sector')
print(p)
dev.off()

# ———————— 1. stacked bar chart based on composistion table ————————

### let's deal with sector composistion first
# this is the function to plot sector composition stacked bar chart
plot_sector_percent_stacked_bar <- function(composition_table, title_text){
  compos_long <- pivot_longer(composition_table, cols = -org_code, names_to = "sector", values_to = "percentage")
  #fill na with 0
  compos_long$percentage[is.na(compos_long$percentage)] <- 0
  ggplot(compos_long, aes(x = as.factor(org_code), y = percentage, fill = factor(sector))) +
    geom_bar(stat = "identity", position = "fill") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    labs(x = "Policy actor", y = "Percentage", fill = "sector") +
    theme(legend.position = "right") +
    scale_fill_manual(values=c("UNI" = "#D53E4F",
                               "RES" ="#F46D43",
                               "POL" = "#FDAE61",
                               "NGO" ="#FEE08B",
                               "LAB" = "#FFFFBF",
                               "INT"="#E6F598",
                               "GOV" = "#ABDDA4",
                               "ENG" = "#66C2A5",
                               "BUS" = "#3288BD")) +
    ggtitle(title_text) +
    theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

#remove the first column '.egoID' before plot
#save as a png file
png("./output/influ_sector_compos.png",
    width = 1600, height = 1600, units = "px", res = 300)
plot_sector_percent_stacked_bar(Influ_Sector_compos[, -1],  "Influence network sector composition")
dev.off()


### then turn to scale composistion, if needed
plot_scale_percent_stacked_bar <- function(composition_table, title_text){
  compos_long <- pivot_longer(composition_table, cols = -org_code, names_to = "scale", values_to = "percentage")
  #fill na with 0
  compos_long$percentage[is.na(compos_long$percentage)] <- 0
  ggplot(compos_long, aes(x = as.factor(org_code), y = percentage,
                          fill = factor(scale,
                                        levels = c('local',
                                                   'provincial',
                                                   'national',
                                                   'international')))) +
    geom_bar(stat = "identity", position = "fill") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    labs(x = "Policy actor", y = "Percentage", fill = "scale") +
    theme(legend.position = "right") +
    scale_fill_manual(values=c("local"="#E6F598",
                               "provincial" = "#ABDDA4",
                               "national" = "#66C2A5",
                               "international" = "#3288BD")) +
    ggtitle(title_text) +
    theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

png("./output/influ_scale_compos.png",
    width = 1600, height = 1600, units = "px", res = 300)
plot_scale_percent_stacked_bar(Influ_Scale_compos[, -1],  "Influence network scale composition")
dev.off()


# ———————— 2. plot each individual ego-centric network ————————

plot_egonet_i <- function(ego_net, directed){
  #recode the scale levels
  scale_code <- data.frame(scale = c("local", "provincial", "national", "international"),scale_code = c(1, 2, 3, 4))
  ego_net$ego$scale <- factor(ego_net$ego$scale, levels = scale_code$scale, labels = scale_code$scale_code)
  ego_net$alter$scale <- factor(ego_net$alter$scale, levels = scale_code$scale, labels = scale_code$scale_code)

  igraph_obj <- as_igraph(ego_net, directed = directed)

    #create a color palette for scale
  pal <- brewer.pal(length(unique(nodelist$scale)), "Greens")

  par(mar = c(0,0,0,0), mfrow = c(4, 3))

  #loop through each ego network, get the egoID and egoCode, and plot the ego network
  for (i in 1:length(igraph_obj)){
    egoID <- ego_net$ego$.egoID[i]
    egoCode <- ego_net$ego$org_code[i]
    egoName <- ego_net$ego$org_name[i]
    g <- igraph_obj[[i]]
    plot(g, edge.arrow.size=0.2, vertex.label.cex = .75,
         frame=T, frame.color='grey',
         margin=0.1,
         vertex.color = pal[as.numeric(vertex_attr(g, "scale"))],
         #vertex.shape = shapes[as.numeric(vertex_attr(g, "sector"))],
         #vertex.size = vertex_attr(g, size_var)*2,
         vertex.label.color="grey10", vertex.label.dist=2.5, vertex.label.family="Helvetica",
         vertex.label=V(g)$org_code)

    mtext(paste('ego', egoID, ": ", egoCode, sep = ""),
          side = 3, line = -2, adj = 0.02, cex=1)
  }
}

# suggest to save figure directly from Rstudio so figure size can be adjusted
png("./output/ego_i.png",
    width = 1600, height = 1600, units = "px", res = 300)
plot_egonet_i(infl_net, directed = T)
dev.off()


# ———————— 3. plot egograms ————————

#plot egogram
#loop through each subset, plot the egograms
plot_egograms_by_sector <- function(ego_net, net_name){
  #recode the scale levels
  scale_code <- data.frame(scale = c("local", "provincial", "national", "international"),scale_code = c('4.local', '3.provincial', '2.national', '1.international'))
  ego_net$ego$scale <- factor(ego_net$ego$scale, levels = scale_code$scale, labels = scale_code$scale_code)
  ego_net$alter$scale <- factor(ego_net$alter$scale, levels = scale_code$scale, labels = scale_code$scale_code)

  #break the ego_net based on the sector
  subsets <- split(ego_net$ego$.egoID, ego_net$ego$sector)
  #loop through each subset, plot the egograms
  for (i in 1:length(subsets)){
    subsets_egos <- subsets[[i]]
    sector_name <- names(subsets[i])
    #check if the length of the subset is smaller than or equal to 4
    if (length(subsets_egos) <= 4){
      ego_labels <- ego_net$ego[ego_net$ego$.egoID %in% subsets_egos, "org_code"]$org_code
      png(paste("./output/egogram/", net_name, i,  ".png", sep = ""), width = 1500, height = 1150, units = "px", res = 150)
      par(oma=c(0,0,3,0))
      plot(ego_net[subsets_egos], venn_var = "scale", pie_var = "sector", type = "egogram",
             #vertex_size_var = "n_document_scaled",
             #vertex_label_var = "org_code",
             vertex_zoom = 0.2, edge_zoom = 0.8)
      mtext(paste(net_name, ' egogram(s) for ', sector_name, " actors: \n", paste(ego_labels, collapse = ", "), sep = ""),
            side=3, line=1, adj = 0.01, outer=TRUE, cex=1)
      dev.off()
      }else {
      #break the subset into smaller groups of 4
        subsets_egos_groups <- split(subsets_egos, ceiling(seq_along(subsets_egos) / 4))
        for (j in 1:length(subsets_egos_groups)){
          png(paste("./analysis/Survey_Media_analysis/EgoNet_output/egogram/", net_name, i, '_', j,  ".png", sep = ""), width = 1500, height = 1150, units = "px", res = 150)
          par(oma=c(0,0,3,0))
          ego_labels <- ego_net$ego[ego_net$ego$.egoID %in% subsets_egos_groups[[j]], "org_code"]$org_code
          plot(ego_net[subsets_egos_groups[[j]]], venn_var = "scale", pie_var = "sector", type = "egogram",
               vertex_size_var = "n_document_scaled",
               #vertex_label_var = "org_code",
               vertex_zoom = 0.2, edge_zoom = 0.8)
          mtext(paste(net_name, ' egogram(s) for ', sector_name, " actors: \n", paste(ego_labels, collapse = ", "), sep = ""),
            side=3, line=1, adj = 0.01, outer=TRUE, cex=1)
          dev.off()
        }
    }

  }
}

plot_egograms_by_sector(infl_net, "influence")
