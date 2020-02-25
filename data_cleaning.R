library(tidyverse)

data <- read_csv("/Users/carlpearson/Documents/r_github/openshift_top_tasks/do_not_upload/five_weeks.csv",col_names = T)

df <- data[3:nrow(data),]

#get wide df for level 1 vars
df.wide <-  df %>% 
  filter(Status != "Survey Preview",#remove previews
         RH =="No, I'm not a Red Hat employee.") %>%  #remove internals
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
              channel_source == "er" ~ "Email referral via CEE",
              channel_source == "ef" ~ "Email referral via BI users",
              channel_source == "bf" ~ "Email eferral via BI buyers",
              channel_source == "email" ~ "Email"
          )) %>% #get channel data united 
  pivot_longer(contains("i_",),names_to = "variable",values_to = "response" ) %>% #elongate top task measurement variables
  na.omit() %>% #remove na, unpicked choices
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

