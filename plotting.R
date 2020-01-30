#Creat Top Task plots

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
    title="RHEL Top 20 Tasks",
    subtitle = "Confidence internals at 80%, Adjusted Wald method",
    x="Tasks",
    y="Percentage"
  )
