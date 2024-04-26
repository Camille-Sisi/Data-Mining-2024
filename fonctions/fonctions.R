
tableau <- function(data, ..., var_quali, pond=IPONDI, nom_var_quali){
  
  tab <- data %>% 
    filter(...) %>% 
    count({{ var_quali }}, wt={{ pond }}) %>% 
    mutate(Pourcentage=prop.table(n)*100, Pourcentage=round(Pourcentage, 1)) %>% 
    adorn_totals("row") %>% 
    rename(Effectif=n, {{nom_var_quali}}:={{ var_quali }}) 
  
  return(tab)
  
}



libelles_var <- function(data, cod_var, new_var){
  
  levels_var <- meta[meta$COD_VAR=={{ cod_var }}, ]$COD_MOD
  labels_var <- meta[meta$COD_VAR=={{ cod_var }}, ]$LIB_MOD
  data %>% mutate({{ new_var }} := factor(eval(parse(text={{ cod_var }})), 
                                          levels = levels_var, labels = labels_var))
  
}


somme <- function(data, var_gpe, nom_var){
  
  som <- data %>% 
    group_by({{var_gpe}}) %>% 
    count({{nom_var}}, wt=IPONDI) %>% 
    mutate(n=round(n)) %>% 
    pivot_wider(names_from = {{nom_var}}, values_from = n)
  
  return(som)
  
}


tab_n <- function(data, ..., nom_var, var, prefix_var) {
  data %>% 
    group_by(...) %>%
    summarise({{ nom_var }} := round(sum(IPONDI))) %>% 
    pivot_wider(names_from = {{ var }}, values_from = {{ nom_var }},
                values_fill = 0, names_prefix = prefix_var)
  
  return(tab_n)
}


Tabcr_pctcol <- function(data, var1, var2, pond=IPONDI, nom_var1="classe"){
  tabcroisefin <- data %>% group_by({{var2}})  %>% 
    count({{var1}}, wt={{pond}}) %>% pivot_wider(names_from = {{var1}}, values_from = "n", names_prefix = paste0(nom_var1,"_")) %>% 
    adorn_totals(c("row",'col')) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits=2) 
  
  return(tabcroisefin)
}
  
  