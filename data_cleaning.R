library(tidyverse)

data <- read_csv("/Users/carlpearson/Documents/r_github/openshift_top_tasks/do_not_upload/initial_eight.csv",col_names = T)

df <- data[3:nrow(data),]

df.long <- df %>% 
  filter(Status != "Survey Preview") %>% #remove previews
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
         ) %>%
  select(-contains("DO_")) %>% #remove randomization data
  unite(col="role",contains("role"),na.rm=T,sep=",") %>% #combine role cols
  mutate(role=gsub(",,","",role)) %>%
  pivot_longer(contains("i_",),names_to = "variable",values_to = "response" ) %>% #elongate top task measurement variables
  na.omit() %>% #remove na, unpicked choices
  separate(variable,into=c("interface","measure","number"),sep="_") %>% #split out variable col into three pieces of info
  unite("variable",c("interface","measure"),sep="_") %>%
  select(-number) %>% #remove number variable, unneeded
  pivot_wider(names_from = "variable",values_from = "response") %>% #widen data across ui/cli measure
  unnest() %>%
  separate(experience, into = c('experience','drop_me'),sep=" – ") %>% select(-drop_me) %>%
  mutate(
    experience = factor(experience, levels = c("None","Basic","Intermediate","Advanced","Expert")),
    use_oc = factor(use_oc,levels=c("I'm not sure","Never","Less than once","1-2 times", "3-5 times", "6-10 times","11+ times")),
    use_odo = factor(use_odo,levels=c("I'm not sure","Never","Less than once","1-2 times", "3-5 times", "6-10 times","11+ times")),
    use_ui = factor(use_ui,levels=c("I'm not sure","Never","Less than once","1-2 times", "3-5 times", "6-10 times","11+ times")),
    cli_diff.n = case_when(cli_diff == "Very Easy" ~ 5,
                           cli_diff == "Easy" ~ 4,
                           cli_diff == "Neutral" ~ 3,
                           cli_diff == "Difficult" ~ 2,
                           cli_diff == "Very Difficult" ~ 1
                            )
  )
