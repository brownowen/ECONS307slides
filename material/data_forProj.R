
# Cleaning dataphones dataset
df <- read.csv("C:/Users/user/PyCharmMiscProject/dataphones.csv")


list_var <- c('price',
              "Qualité de la caméra","Capacité totale de stockage",
              "Année d'introduction","Produit",
              "Marque","Garantie","Poids","Couleur","Mémoire vive (RAM)",
              "Processeur","Nombre total de mégapixels","Densité de pixels"
)
phones <- df %>% select(-X) %>% 
  filter(Variable %in% list_var) %>% 
  group_by(model) %>% distinct() %>% 
  pivot_wider(names_from = "Variable",values_from = "Value",id_cols = "model") %>% 
  ungroup %>% 
  select(-model) %>% 
  relocate(Produit,.before=price) %>% 
  tibble

phonesdf <- phones %>% 
  mutate(price=str_remove_all(price, "[€\\-\\,\\.]")) %>% 
  mutate(price=as.numeric(price)) %>% 
  mutate(Poids=str_remove_all(Poids, "g"),Poids=as.numeric(Poids)) %>% 
  mutate(RAM=str_remove_all(`Mémoire vive (RAM)`, "Go"),RAM=as.numeric(RAM)) %>% select(-`Mémoire vive (RAM)`) %>% 
  mutate(stockage=str_replace_all(`Capacité totale de stockage`,"1 To","1000 Go"),
         stockage=str_remove_all(stockage, "Go"),
         stockage=as.numeric(stockage)) %>% select(-`Capacité totale de stockage`) %>% 
  mutate(megapixels=str_remove_all(`Nombre total de mégapixels`, "Mpx"),megapixels=as.numeric(megapixels)) %>%
  select(-`Nombre total de mégapixels`) %>% 
  mutate(age=2026-as.numeric(`Année d'introduction`)) %>% 
  mutate(Marque=factor(Marque),Marque=relevel(Marque,"Apple"),
         `Qualité de la caméra`=factor(`Qualité de la caméra`),
         `Qualité de la caméra`=relevel(`Qualité de la caméra`,"Bon")) %>% 
  filter(Marque!="Refurbished")