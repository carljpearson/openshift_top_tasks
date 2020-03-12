#rank analysis
library(rstatix)
library(ggpubr)


#friedman test
res.fried <- df.cli.rank %>%
  friedman_test(Rank ~ Task | ResponseId)
res.fried

#effect size
df.cli.rank %>%
  friedman_effsize(Rank ~ Task | ResponseId)

mod.pairwise <- df.cli.rank %>%
  arrange(ResponseId) %>% #order needs to be by ID
  mutate(Rank=as.numeric(Rank)) %>%
  pairwise_wilcox_test(Rank ~ Task, p.adjust.method = "holm")
mod.pairwise

mod.pairwise <- mod.pairwise %>% 
  add_xy_position(x = "name_s") 
ggboxplot(df_long, x = "name_s", y = "value", add = "point") +
  stat_pvalue_manual(mod.pairwise, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(mod.pairwise)
  )


#cluster

library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)

#get dataset
df_kmeans <- df.sum.ui %>% 
  ungroup() %>% 
  select(prop,rank=rank_avg) %>% #select input variables, proportion and avg rank
  na.omit() %>%
  mutate(prop=scale(prop)) #scale proportion due to high skew

rownames(df_kmeans) <- df.sum.ui$ui_topt


#Make screen plot based on # of k
# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(df_kmeans, centers = i, nstart=20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

#2 seems to be the winner
k2 <- kmeans(df_kmeans, centers = 2,  nstart = 25)
#but lets explore other options as well, 3-5 k
k3 <- kmeans(df_kmeans, centers = 3, nstart = 25)
k4 <- kmeans(df_kmeans, centers = 4, nstart = 25)
k5 <- kmeans(df_kmeans, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df_kmeans) + labs(title="k = 2", x="Response proportion (standardized)",y="Rank Average")
p2 <- fviz_cluster(k3, geom = "point",  data = df_kmeans) + labs(title="k = 2", x="Response proportion (standardized)",y="Rank Average")
p3 <- fviz_cluster(k4, geom = "point",  data = df_kmeans) + labs(title="k = 2", x="Response proportion (standardized)",y="Rank Average")
p4 <- fviz_cluster(k5, geom = "point",  data = df_kmeans) +  labs(title="k = 2", x="Response proportion (standardized)",y="Rank Average")

#tie them together
grid.arrange(p1, p2, p3, p4, nrow = 2)
#save
#ggsave("/Users/carlpearson/Documents/r_github/top_tasks_rhel/k2-5plots.png",device="png",
#       width=9,height=9,bg="transparent")

#final plot with border
#Make data so Tasks are included but only for top tasks
df_kmeans.plot <- df_kmeans %>% 
  mutate(
    cluster = k2$cluster,
    task=df.sum.ui$ui_topt,
    task = case_when(cluster==1~task,
                     cluster==2~"XX"),
    task=na_if(task,"XX")
  ) 

#plot with labels
fviz_cluster(k2, data = df_kmeans,
             geom="point",
             show.clust.cent = F
)+ 
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  ggrepel::geom_label_repel(aes(label=df_kmeans.plot$task)) + 
  labs(
    title="Clustering via k-means",
    y="Rank Average",
    x="Response proportion (standardized)"
  ) +
  scale_color_manual(values=c("#8481DD","#73C5C5")) + scale_fill_manual(values=c("#8481DD","#73C5C5"))
#save
ggsave("/Users/carlpearson/Documents/r_github/top_tasks_rhel/k2plot.png",device="png",
       width=6,height=6,bg="white")



#agnes

ag.mod <- agnes(df_kmeans,method = "complete")

ag.mod$ac
# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(m, ac) #compare methods

ag.mod.ward <- agnes(df_kmeans, method = "ward") #ward agness model


pltree(ag.mod.ward, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 
rect.hclust(ag.mod.ward, k = 4, border = 2:5)

#diana
di.mod <- diana(df_kmeans)

#divise coeff
di.mod$dc

#plot
pltree(di.mod,cex=.6,hang=-1,main = "Dendogram of diana")
rect.hclust(di.mod, k = 3, border = 2:5)

#compare
library(dendextend)

dend1 <- as.dendrogram (ag.mod.ward)
dend2 <- as.dendrogram (di.mod)

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           #common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

#determine eblow
fviz_nbclust(df_kmeans, FUN = hcut, method = "wss")
#determine silhouette
fviz_nbclust(df_kmeans, FUN = hcut, method = "silhouette")
#gap statistic
gap_stat <- clusGap(df_kmeans, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
