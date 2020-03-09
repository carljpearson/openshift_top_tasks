library(RColorBrewer)

#level 1 plots: descriptives

#source
df.wide %>%
  ggplot(aes(channel_source,fill=channel_source)) +
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

#ggsave("/Users/carlpearson/Documents/r_github/openshift_top_tasks/plots/experience.png",bg="transparent",width = 8,height = 6)

#ui vs cli bar
df.wide %>%
  select(ResponseId,use_oc,use_odo,use_ui) %>%
  pivot_longer(-ResponseId,names_to = "Interface",values_to = "Weekly_Use") %>%
  mutate(Interface=gsub("use_","",Interface)) %>%
  ggplot(aes(Interface,fill=Weekly_Use)) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.4),size=5,position = position_dodge(width = .9)) + 
  ggthemes::theme_tufte(base_family="sans", base_size = 15) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme(
    axis.text.x= element_text(size=12),
    axis.text.y=element_blank(),
    axis.title.x =element_blank(),
    axis.ticks.y=element_blank()) +
  labs(y="Count")

#ui vs oc heat

df.wide %>%
  drop_na(use_oc,use_ui) %>%
  group_by(use_oc,use_ui) %>%
  count() %>%
  rename(count=n) %>%
  mutate(oc_fac=as.numeric(use_oc),
         ui_fac=as.numeric(use_ui)) %>%
  filter(ui_fac != 1,
         oc_fac != 1) %>%
  mutate(gradient.group = case_when(
    ui_fac == oc_fac ~ "Even",
    ui_fac > oc_fac ~ "Less oc use",
    ui_fac < oc_fac ~ "More oc use"
  )) %>%
  ggplot(aes(y=use_oc,x=use_ui,fill=count,color=gradient.group)) +
  geom_tile(stat="identity",size=2) +
  geom_text(aes(label=count),color="white") +
  scale_color_manual(values=c("gray","green","orange")) +
  ggthemes::theme_tufte(base_family = "sans") +
  labs(y="Weekly oc use",x="Weekly UI use",
       title = "Weekly use of oc vs. UI")


#Create Top Task plots

#static values
total.responses <- nrow(df.wide)
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
            rank_sum=sum(as.numeric(cli_rank),na.rm = T),
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
            rank_sd = sd(ui_rank, na.rm=T ),
            rank_marg = ((rank_sd/sqrt(count))*zval),
            rank_sum=sum(as.numeric(ui_rank)),
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
df.sum <- full_join(df.sum.cli,df.sum.ui,suffix=c(".cli",".ui"),by=c("cli_topt" = "ui_topt"))




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

#difficulty side by side with top tasks

#cli
p1.cli <- df.sum.cli %>% 
  ungroup() %>%
  arrange(desc(prop)) %>% slice(1:20) %>%
  ggplot(aes(y=prop,x=reorder(cli_topt,prop))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=count,y=0),color="white",hjust=1.4) +
  # geom_point(aes(y=difficulty_avg)) +
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",width=.5) +
  scale_y_reverse(labels = scales::percent) +
  ggthemes::theme_tufte(base_family = "sans") +
  coord_flip() +
  #theme
  #theme(axis.text.x = element_text(angle = -45,hjust=.7,vjust=1))
  # ggthemes::theme_tufte(base_family="sans") +
  labs(
    title="OpenShift CLI Top Tasks & Average Difficulty",
    subtitle = "Confidence internals at 90%, Numbers are raw counts",
    x="Tasks",
    y="Percentage"
  )

p2.cli <- df.sum.cli %>% 
  ungroup() %>%
  arrange(desc(prop)) %>% slice(1:20) %>%
  ggplot(aes(y=difficulty_avg,x=reorder(cli_topt,prop),fill=difficulty_avg)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low="#580000",mid="darkgray",high="blue",midpoint=4) +
  geom_errorbar(aes(ymin=difficulty_avg-difficulty_marg,ymax=difficulty_avg+difficulty_marg),color="gray",width=.5) +
  coord_flip(ylim=c(1,5)) +
  ggthemes::theme_tufte(base_family = "sans") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") +
  # ggthemes::theme_tufte(base_family="sans") +
  labs(
    title="",
    subtitle = "Redder (smaller number) is more difficult",
    x="Tasks",
    y="Dfficulty rating"
  )


gridExtra::grid.arrange(p1.cli, p2.cli, 
                        widths = c(2,1),
                        nrow = 1) 



#save
#ggsave("/Users/carlpearson/Documents/r_github/top_tasks_rhel/difficulty.png",device="png",  width=8,height=6,bg="white")


#ui
p1.ui <- df.sum.ui %>% 
  ungroup() %>%
  arrange(desc(prop)) %>% slice(1:20) %>%
  ggplot(aes(y=prop,x=reorder(ui_topt,prop))) +
  geom_bar(stat = "identity") +
  # geom_point(aes(y=difficulty_avg)) +
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",width=.5) +
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",width=.5) +
  geom_text(aes(label=count,y=0),color="white",hjust=1.4) +
  scale_y_reverse(labels = scales::percent) +
  ggthemes::theme_tufte(base_family = "sans") +
  coord_flip() +
  #theme
  #theme(axis.text.x = element_text(angle = -45,hjust=.7,vjust=1))
  # ggthemes::theme_tufte(base_family="sans") +
  labs(
    title="OpenShift UI Top Tasks & Average Difficulty",
    subtitle = "Confidence internals at 90%, Numbers are raw counts",
    x="Tasks",
    y="Percentage"
  )

p2.ui <- df.sum.ui %>% 
  ungroup() %>%
  arrange(desc(prop)) %>% slice(1:20) %>%
  ggplot(aes(y=difficulty_avg,x=reorder(ui_topt,prop),fill=difficulty_avg)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low="#580000",mid="darkgray",high="blue",midpoint=4) +
  geom_errorbar(aes(ymin=difficulty_avg-difficulty_marg,ymax=difficulty_avg+difficulty_marg),color="gray",width=.5) +
  coord_flip(ylim=c(1,5)) +
  ggthemes::theme_tufte(base_family = "sans") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") +
  # ggthemes::theme_tufte(base_family="sans") +
  labs(
    title="",
    subtitle = "Redder (smaller number) is more difficult",
    x="Tasks",
    y="Dfficulty rating"
  )


gridExtra::grid.arrange(p1.ui, p2.ui, 
                        widths = c(10,6),
                        nrow = 1) 


#save
#ggsave("/Users/carlpearson/Documents/r_github/top_tasks_rhel/difficulty.png",device="png",  width=8,height=6,bg="white")


#difficulty comparison
df.sum %>%
  filter(count.cli > 10,
         count.ui > 10) %>%
  select(cli_topt, 
         contains("difficulty") 
         ) %>%
  pivot_longer(-cli_topt) %>%
  separate(name,into = c("name","Interface"),sep = "\\.") %>%
  pivot_wider() %>%
  rename(Tasks = cli_topt) %>%
  arrange(desc(difficulty_avg)) %>%
  ggplot(aes(x=Tasks,y=difficulty_avg,fill=Interface,color=Interface)) +
  geom_errorbar(aes(ymin=difficulty_avg-difficulty_marg,ymax=difficulty_avg+difficulty_marg),position = position_dodge(width = .5)) +
  geom_point(position=position_dodge(width = .5)) +
  ggthemes::theme_tufte(base_family = "sans") +
  coord_flip()


#rank sum

df.sum.cli %>%
  ungroup() %>%
  arrange(desc(rank_sum)) %>% 
  slice(1:20) %>%
  ggplot(aes(x=cli_topt,y=rank_sum))+
  geom_bar(stat="identity")+
  geom_point(aes(y=prop*700)) +
  ggthemes::theme_tufte(base_family = "sans") +
  coord_flip()
  


