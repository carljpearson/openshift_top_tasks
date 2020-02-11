#level 1 plots: descriptives

#Experience
df.wide %>%
  ggplot(aes(experience,fill=experience)) +
  geom_bar() +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.4),size=7) + 
  ggthemes::theme_tufte(base_family="sans", base_size = 15) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme(
    axis.text.x= element_text(size=12),
    axis.text.y=element_blank(),
    axis.title.x =element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "none") +
  labs(y="Count")

ggsave("/Users/carlpearson/Documents/r_github/openshift_top_tasks/plots/experience.png",bg="transparent",width = 8,height = 6)


#Create Top Task plots

#static values
total.responses <- length(unique(df$ResponseId))
zval=1.64

#proportions dataframe
#cli
df.sum.cli <- df.long %>%
  mutate(total=total.responses,
         expertise.num = case_when(
           experience == "Expert" ~ 4,
           experience == "Advanced" ~ 3,
           experience == "Intermediate" ~ 2,
           experience == "Beginner" ~ 1,
           experience == "None" ~ 0
         )
  ) %>%
  group_by(cli_topt,total) %>%
  summarize(count=n(),
            rank_avg=mean(as.numeric(cli_rank),na.rm = T),
            difficulty_avg=mean(cli_diff.n,na.rm = T),
            difficulty_sd = sd(cli_diff.n, na.rm=T ),
            difficulty_marg = ((difficulty_sd/sqrt(count))*zval),
            expertise_avg = mean(expertise.num,na.rm = T)          
  ) %>%
  mutate(prop = count / total, #get cis
         n=total, #rename
         prop = count / n, #exact proportion from succesess/trials
         laplace = (count + 1) / (n + 2), #laplace point estimate
         p_adj = (n * prop + (zval * zval) / 2) / (n + (zval * zval)), #adjust p for wald calculation
         n_adj = n + (zval * zval), #adjust n for wald calculation
         marg =  zval * sqrt(p_adj * (1 - p_adj) / n_adj), #wald margin value
         lowerci = p_adj - marg, #lower wald ci
         lowerci = ifelse(lowerci <= 0, 0, lowerci), #keep lower ci above 0
         upperci = p_adj + marg, #upper wald ci
         upperci = ifelse(upperci >= 1, 1, upperci)) #keep upper ci below 1

#ui

#get summarized dataset
df.sum.ui <- df.long %>%
  mutate(total=total.responses) %>%
  group_by(ui_topt,total) %>%
  summarize(count=n(),
            rank_avg=mean(as.numeric(ui_rank),na.rm = T),
            difficulty_avg=mean(ui_diff.n,na.rm = T),
            difficulty_sd = sd(ui_diff.n, na.rm=T ),
            difficulty_marg = ((difficulty_sd/sqrt(count))*zval),
            expertise_avg = mean(expertise.num,na.rm = T)          
  ) %>%
  mutate(prop = count / total, #get cis
         n=total, #rename
         prop = count / n, #exact proportion from succesess/trials
         laplace = (count + 1) / (n + 2), #laplace point estimate
         p_adj = (n * prop + (zval * zval) / 2) / (n + (zval * zval)), #adjust p for wald calculation
         n_adj = n + (zval * zval), #adjust n for wald calculation
         marg =  zval * sqrt(p_adj * (1 - p_adj) / n_adj), #wald margin value
         lowerci = p_adj - marg, #lower wald ci
         lowerci = ifelse(lowerci <= 0, 0, lowerci), #keep lower ci above 0
         upperci = p_adj + marg, #upper wald ci
         upperci = ifelse(upperci >= 1, 1, upperci)) #keep upper ci below 1

#join
df.sum 
full_join(df.sum.cli,df.sum.ui,suffix=c("cli","ui"),by=c("cli_topt","ui_topt"))




df.sum.cli %>% 
  #filter(prop>.07) %>%
  ggplot(aes(y=prop,x=reorder(cli_topt,prop))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",width=.5) +
  scale_y_continuous(labels = scales::percent) +#Make scale 0-1 and %
  scale_fill_manual(values=c("#4CB140","#0066CC")) +
  coord_flip() +
  #theme
  #theme(axis.text.x = element_text(angle = -45,hjust=.7,vjust=1))
  ggthemes::theme_tufte(base_family="sans") +
  labs(
    title="OpenShift CLI Top Tasks",
    subtitle = "Confidence internals at 90%, Adjusted Wald method",
    x="Tasks",
    y="Percentage"
  )

df.sum.ui %>% 
  #filter(prop>.07) %>%
  ggplot(aes(y=prop,x=reorder(ui_topt,prop))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",width=.5) +
  scale_y_continuous(labels = scales::percent) +#Make scale 0-1 and %
  scale_fill_manual(values=c("#4CB140","#0066CC")) +
  coord_flip() +
  #theme
  #theme(axis.text.x = element_text(angle = -45,hjust=.7,vjust=1))
  ggthemes::theme_tufte(base_family="sans") +
  labs(
    title="OpenShift UI Top Tasks",
    subtitle = "Confidence internals at 90%, Adjusted Wald method",
    x="Tasks",
    y="Percentage"
  )

#difficulty side by side

#cli
p1 <- df.sum.cli %>% 
  filter(prop>.02) %>%
  ggplot(aes(y=prop,x=reorder(cli_topt,prop))) +
  geom_bar(stat = "identity") +
  # geom_point(aes(y=difficulty_avg)) +
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",width=.5) +
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",width=.5) +
  scale_y_reverse(labels = scales::percent) +
  theme_minimal() +
  coord_flip() +
  #theme
  #theme(axis.text.x = element_text(angle = -45,hjust=.7,vjust=1))
  # ggthemes::theme_tufte(base_family="sans") +
  labs(
    title="RHEL Top 20 Tasks - Proprotion Chosen",
    subtitle = "Confidence internals at 90%, Adjusted Wald method",
    x="Tasks",
    y="Percentage"
  )

p2 <- df.sum.cli %>% 
  filter(prop>.02) %>%
  ggplot(aes(y=difficulty_avg,x=reorder(cli_topt,prop))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=difficulty_avg-difficulty_marg,ymax=difficulty_avg+difficulty_marg),color="gray",width=.5) +

  coord_flip() +
  theme_minimal() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") +
  # ggthemes::theme_tufte(base_family="sans") +
  labs(
    title="RHEL Top 20 Tasks - Average Difficulty",
    subtitle = "Confidence internals at 90%, standard method",
    x="Tasks",
    y="Dfficulty rating"
  )


gridExtra::grid.arrange(p1, p2, nrow = 1) 


#save
#ggsave("/Users/carlpearson/Documents/r_github/top_tasks_rhel/difficulty.png",device="png",  width=8,height=6,bg="white")


#ui
p1 <- df.sum.ui %>% 
  filter(prop>.02) %>%
  ggplot(aes(y=prop,x=reorder(ui_topt,prop))) +
  geom_bar(stat = "identity") +
  # geom_point(aes(y=difficulty_avg)) +
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",width=.5) +
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",width=.5) +
  scale_y_reverse(labels = scales::percent) +
  theme_minimal() +
  coord_flip() +
  #theme
  #theme(axis.text.x = element_text(angle = -45,hjust=.7,vjust=1))
  # ggthemes::theme_tufte(base_family="sans") +
  labs(
    title="RHEL Top 20 Tasks - Proprotion Chosen",
    subtitle = "Confidence internals at 90%, Adjusted Wald method",
    x="Tasks",
    y="Percentage"
  )

p2 <- df.sum.ui %>% 
  filter(prop>.02) %>%
  ggplot(aes(y=difficulty_avg,x=reorder(ui_topt,prop))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=difficulty_avg-difficulty_marg,ymax=difficulty_avg+difficulty_marg),color="gray",width=.5) +
  
  coord_flip() +
  theme_minimal() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") +
  # ggthemes::theme_tufte(base_family="sans") +
  labs(
    title="",
    subtitle = "",
    x="Tasks",
    y="Dfficulty rating"
  )


gridExtra::grid.arrange(p1, p2, nrow = 1) 


#save
#ggsave("/Users/carlpearson/Documents/r_github/top_tasks_rhel/difficulty.png",device="png",  width=8,height=6,bg="white")
