library(tidyverse)

data <- read_csv("/Users/carlpearson/Documents/r_github/openshift_top_tasks/do_not_upload/seven_weeks.csv",col_names = T)
task_group_key <- read_csv("/Users/carlpearson/Documents/r_github/openshift_top_tasks/do_not_upload/task_group_key.csv",col_names = T) %>% 
  mutate(Task = as.character(Task),
         Group = as.character(Group)) %>%
  select(Task,Group)

task_group_key <- task_group_key[2:nrow(task_group_key),]

df <- data[3:nrow(data),]

#get wide df for level 1 vars
df.wide <-  df %>% 
  filter(Status != "Survey Preview",#remove previews
         RH =="No, I'm not a Red Hat employee.",
         Finished=="True") %>%  #remove internals
  select(ResponseId, #id variable
         rh=RH, #internal/external variablke
         experience=Experience, #xperience level
         contains("role"), #role
         #UI/CLI
         use_oc=use_interface_1,
         use_odo=use_interface_2,
         use_ui=use_interface_3,
         umux.eff = Q7_1,
         umux.eas = Q7_2,
         contains("cli_topt"),
         contains("cli_rank"),
         contains("cli_diff"),
         contains("ui_topt"),
         contains("ui_rank"),
         contains("ui_diff"),
         progress=Progress,
         duration=`Duration (in seconds)`,
         data=RecordedDate,
         channel=DistributionChannel,
         lat=LocationLatitude,
         long=LocationLongitude,
         version,
         research,
         Status,
         channel_source='src',
         dist=DistributionChannel) %>%
  select(-contains("DO_")) %>% #remove randomization data
  unite(col="role",contains("role"),na.rm=T,sep=",") %>% #combine role cols
  mutate(role=gsub(",,","",role)) %>% #remove excess characters in role
  mutate( channel_source = ifelse(is.na(channel_source),dist,channel_source),
          channel_source = case_when(
              channel_source == "bl" ~ "Blog",
              channel_source == "dv" ~ "Developer outreach",
              channel_source == "er" ~ "Email referral via CEE",
              channel_source == "ef" ~ "Email referral via BI users",
              channel_source == "bf" ~ "Email referral via BI buyers",
              channel_source == "mj" ~ "Mojo link",
              channel_source == "ml" ~ "memo-list link",
              channel_source == "rd" ~ "Referral",
              channel_source == "email" ~ "Email"
          ),
          channel_source = replace_na(channel_source,"Unknown")
          ) %>% #get channel data united 
  pivot_longer(contains("i_"),names_to = "variable",values_to = "response" ) %>% #elongate top task measurement variables
  drop_na(response) %>% #remove na, unpicked choices
  separate(variable,into=c("interface","measure","number"),sep="_") %>% #split out variable col into three pieces of info
  unite("variable",c("interface","measure"),sep="_") %>%
  select(-number) %>% #remove number variable, unneeded
  pivot_wider(names_from = "variable",values_from = "response") %>% #widen data across ui/cli measure
  separate(experience, into = c('experience','drop_me'),sep=" – ") %>% select(-drop_me) %>%
  mutate( experience = factor(experience, levels = c("None","Basic","Intermediate","Advanced","Expert")),
    use_oc = factor(use_oc,levels=c("I'm not sure","Never","Less than once","1-2 times", "3-5 times", "6-10 times","11+ times")),
    use_odo = factor(use_odo,levels=c("I'm not sure","Never","Less than once","1-2 times", "3-5 times", "6-10 times","11+ times")),
    use_ui = factor(use_ui,levels=c("I'm not sure","Never","Less than once","1-2 times", "3-5 times", "6-10 times","11+ times")),
    expertise.num = case_when(
      experience == "Expert" ~ 4,
      experience == "Advanced" ~ 3,
      experience == "Intermediate" ~ 2,
      experience == "Beginner" ~ 1,
      experience == "None" ~ 0
    ),
    umux.eff.n = case_when(
      umux.eff == "Strongly agree" ~ 5,
      umux.eff == "Somewhat agree" ~ 4,
      umux.eff == "Neither agree nor disagree" ~ 3,
      umux.eff == "Somewhat disagree" ~ 2,
      umux.eff == "Strongly disagree" ~ 1
    ),
    umux.eas.n = case_when(
      umux.eas == "Strongly agree" ~ 5,
      umux.eas == "Somewhat agree" ~ 4,
      umux.eas == "Neither agree nor disagree" ~ 3,
      umux.eas == "Somewhat disagree" ~ 2,
      umux.eas == "Strongly disagree" ~ 1
    )) 

#%>%
  #  left_join(task_group_key, by=c("cli_topt"="Task"))


#get long df for level 2 vars
df.long <- df.wide %>% 
  unnest() %>%
  mutate( #numeric rank and difficulty
    cli_diff.n = case_when(cli_diff == "Very Easy" ~ 5,
                           cli_diff == "Easy" ~ 4,
                           cli_diff == "Neutral" ~ 3,
                           cli_diff == "Difficult" ~ 2,
                           cli_diff == "Very Difficult" ~ 1
                            ),
    ui_diff.n = case_when(ui_diff == "Very Easy" ~ 5,
                           ui_diff == "Easy" ~ 4,
                           ui_diff == "Neutral" ~ 3,
                           ui_diff == "Difficult" ~ 2,
                           ui_diff == "Very Difficult" ~ 1
    )
  )

#rank data

df.cli.rank <- df.long %>% 
  select(ResponseId,cli_topt,cli_rank) %>% 
  pivot_wider(names_from = cli_topt,values_from = cli_rank) %>%
  pivot_longer(-ResponseId,names_to = "Task",values_to = "Rank") %>%
  mutate(Rank= replace_na(Rank,0))

nrow(df.wide)

#get summarized data

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

