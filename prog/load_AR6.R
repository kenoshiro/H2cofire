AR6_data_file <- 'data/AR6/1668008030411-AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1.csv/AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1.csv'
AR6_meta_file <- 'data/AR6/1668008030411-AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1.csv/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx'

df$load_AR6_global_meta <- read_xlsx(path=AR6_meta_file,sheet='meta_Ch3vetted_withclimate') %>% 
    select(Model,Scenario,Category,IMP_marker)
df$load_AR6_global <- read_csv(file=AR6_data_file) %>% 
    pivot_longer(cols=-c(Model,Scenario,Region,Variable,Unit),names_to='Year',values_to='Value') %>% 
    filter(!is.na(Value)) %>% 
    mutate(Year=as.numeric(Year)) %>% 
    inner_join(df$var_config,by='Variable') %>% select(-Variable) %>% rename(Variable='Variable2')
df$load_AR6_global %<>% 
    bind_rows(fcalc_sum(df_in=df$load_AR6_global,vars=c('Cap_Ele_Gas','Cap_Ele_Coa'),name_new='Cap_Ele_Coa_and_Gas')) %>% 
    bind_rows(fcalc_sum(df_in=df$load_AR6_global,vars=c('Cap_Ele_Gas','Cap_Ele_Coa','Cap_Ele_Oil'),name_new='Cap_Ele_Fos')) %>% 
    inner_join(df$load_AR6_global_meta,by=c('Model','Scenario'))
