library(xlsx)
library('egor')

#set the current working directory
#Note! Please change the directory to your own path to the Egonet_Compon folder!
setwd("/Users/.../Egonet_Compon")

#load in the org_codebook with attribute data
#in the example data file, the attribute is the "scale" column for political scales
nodelist <- read.xlsx("./input/org_codebook_attr.xlsx",1)
nodelist <- nodelist[,-1] #drop the first column
#extract the first three characters of org_code as a new column named "sector"(
# this is the sector code))"
nodelist$sector <- substr(nodelist$org_code, 1, 3)


#let's use influence network as the example to demonstrate

#read in the network data
infl_data <- read.xlsx("./output/influence_edgelist.xlsx", 1)

#some general statistics
#based on the full roster, count the number of organizations in each sector
sector_count <- nodelist %>%
  group_by(sector) %>%
  summarise(n = n())
#add a new column to sector_count, which is the percentage of organizations in each sector
sector_count$percent <- sector_count$n / sum(sector_count$n) * 100
#repeat the process for scale
scale_count <- nodelist %>%
  group_by(scale) %>%
  summarise(n = n())
scale_count$percent <- scale_count$n / sum(scale_count$n) * 100

#count the unique org_code in the alter column in the influence data
infl_alter_count <- infl_data %>%
  group_by(alter) %>%
  summarise(infl_n = n())
#match the org_code in the infl_alter_count to org_code in nodelist
infl_alter_count <- merge(infl_alter_count, nodelist[, c('org_name', 'scale', 'sector', 'org_code')],
                          by.x = "alter", by.y = "org_code", all.x = TRUE)
#merge back to the full nodelist
fulllist <- nodelist[,c('org_name', 'scale', 'sector', 'org_code')]
fulllist <- merge(fulllist, infl_alter_count[, c('alter', 'infl_n')], by.x = "org_code", by.y = "alter", all.x = TRUE)


#now, let's move on the egor part

#the following function prepares the data for egor
EgoNet_data_Prepare <- function(net_data){
  #get the unique org_code in net_data's "ego" column
  ego_data <- unique(net_data$ego)
  #select the rows in nodelist that have org_code in ego_data
  ego_data <- nodelist[nodelist$org_code %in% ego_data,]
  #order the rows in ego_data by org_code
  ego_data <- ego_data[order(ego_data$org_code),]
  #add a new id column to ego_data, starting from 1
  ego_data$egoID <- 1:nrow(ego_data)


  #prepare the alter datafile
  #first, copy the ego and alter columns from net_data to alter_data
  alter_data <- net_data[, c("ego", "alter")]
  #second, match the org_code in alter_data$alter to the org_code in nodelist
  #and get the corresponding sector code and org_name
  alter_data <- merge(alter_data, nodelist, by.x = "alter", by.y = "org_code")
  #copy the alter column to a new column named "org_code"
  alter_data$org_code <- alter_data$alter
  #replace ego with egoID as shown in ego_data, matching by org_code
  alter_data <- merge(alter_data, ego_data[, c("egoID", "org_code")], by.x = "ego", by.y = "org_code")
  #make a copy of the df for later use
  record <- alter_data
  #delete the ego column and move egoID to the first column
  alter_data$ego <- NULL
  alter_data <- alter_data[, c("egoID", "alter", "org_code",
                               "sector", "scale", "org_name")]
  #write a loop, for each unique egoID, recode its alters to 1, 2, 3, etc.
  for (i in 1:nrow(ego_data)){
    alter_data[alter_data$egoID == i, "alter"] <- 1:sum(alter_data$egoID == i)
  }
  #change the name of column "alter" to "alterID"
  names(alter_data)[names(alter_data) == "alter"] <- "alterID"
  #copy the alterID to record
  record$alterID <- alter_data$alterID

  #prepare the alter-alter datafile
  #lopp through each unique egoID in the record df
  #create an empty df named alter_alter_tie_data
  alter_alter_tie_data <- data.frame(egoID = factor(), egoCode = factor(),
                                     sourceID = factor(), sourceCode = factor(),
                                     targetID = factor(), targetCode = factor())
  for (i in 1:nrow(ego_data)){
    egoID <- i
    ego <- record[record$egoID == egoID, "ego"][1]
    #select the rows in record that have egoID == i
    ego_i_df <- record[record$egoID == egoID,]
    #loop through each row in ego_i_df
      for (j in 1:nrow(ego_i_df)){
        #get the alter of the jth row, this is the source node in the alter-alter tie data
          source <- ego_i_df[j, "alter"]
          sourceID <- ego_i_df[j, "alterID"]
        #select the rows in record that have ego == source
          source_df <- record[record$ego == source,]
        #check if source_df is empty, if not, find any value in the alter column that appear in ego_i_df$alter
          if (nrow(source_df) != 0){
            #if there is a match, get the alter the match, these are the target nodes in the alter-alter tie data
              targets <- source_df[source_df$alter %in% ego_i_df$alter, "alter"]
            ##check if targets is empty, if not, proceed with the next step
            if (length(targets) != 0){#for each target in targets, get its alterID in ego_i_df
              for (k in 1:length(targets)){
                target <- targets[k]
                targetID <- ego_i_df[ego_i_df$alter == target, "alterID"]
                #create a df with three columns: egoID, sourceID, and targetID
                AAT_ST <- data.frame(egoID = egoID, egoCode = ego,
                                     sourceID = sourceID, sourceCode = source,
                                     targetID = targetID, targetCode = target)
                alter_alter_tie_data <- rbind(alter_alter_tie_data, AAT_ST)
              }
            }
      }
  }
  }


  #create an egor object
  ego_net <- egor(egos = ego_data, alters = alter_data, aaties = alter_alter_tie_data,
                  ID.vars = list(ego = "egoID",
                                 alter = "alterID",
                                 source = "sourceID", target = "targetID"))
  #return the egor object
  return(ego_net)
}

#feed in the network data for each type of relationship
infl_net <- EgoNet_data_Prepare(infl_data)


#some descriptive analysis for ego-centric networks#

# ———————— 1.the general summary ————————— #
summary(infl_net)

# ——————— 2.network density for each ego ——————— #
density_table <- ego_density(infl_net)
write.xlsx(density_table, "./output/egonet_descriptives.xlsx",
           sheetName = "density_table", showNA=FALSE)

# ———————— 3.ego net composition by group_var ———————— #
#this is a function to get net composition by a group attribute variable
ego_composition_table <- function(net, group_var){
  compos <- composition(net, group_var)
  #multiply by 100, and keep only two digits after the decimal point
  compos[, -1] <- round(compos[, -1] * 100, 2)
  #attach the 'org_code' column by matching by 'ego' in the nodelist
  compos <- merge(compos, net$ego[, c("org_code", ".egoID")], by.x = ".egoID", by.y = ".egoID")
  return(compos)
}
#call the function, get the composition table by "sector"
Influ_Sector_compos <- ego_composition_table(infl_net, "sector")
#save a copy for later use
write.xlsx(Influ_Sector_compos, "./output/egonet_descriptives.xlsx",
           sheetName = "Influ_Sector_compos", append = T, showNA=FALSE)

#repeat the same process for scale
Influ_Scale_compos <- ego_composition_table(infl_net, "scale")
write.xlsx(Influ_Scale_compos, "./output/egonet_descriptives.xlsx",
           sheetName = "Influ_Secale_compos", append = T, showNA=FALSE)


# —————————— 4.Ego-Alter Homophily (EI-Index) ——————————— #
EI_infl_sector <- comp_ei(infl_net, "sector", "sector")
#merge the EI table with the attribute data
EI_infl_sector <- rename(EI_infl_sector, c("egoID" = ".egoID", "Influ_EI" = "ei"))
EI_table_sector <- merge(EI_infl_sector, infl_net$ego[, c("org_code", ".egoID", 'sector')], by.x = "egoID", by.y = ".egoID")
#get the average EI for each sector
EI_avg_sector <- EI_table_sector %>% group_by(sector) %>% summarise(Influ_EI = mean(Influ_EI))
write.xlsx(EI_avg_sector, "./output/egonet_descriptives.xlsx",
           sheetName = "EI_avg_sector", append = T, showNA=FALSE)
write.xlsx(EI_table_sector, "./output/egonet_descriptives.xlsx",
           sheetName = "EI_table_sector", append = T, showNA=FALSE)

#repeat the process for scale
EI_infl_scale <- comp_ei(infl_net, "scale", "scale")
EI_infl_scale <- rename(EI_infl_scale, c("egoID" = ".egoID", "Influ_EI" = "ei"))
EI_table_scale <- merge(EI_infl_scale, infl_net$ego[, c("org_code", ".egoID", 'scale')], by.x = "egoID", by.y = ".egoID")
EI_avg_scale <- EI_table_scale %>% group_by(scale) %>% summarise(Influ_EI = mean(Influ_EI))
write.xlsx(EI_avg_scale, "./output/egonet_descriptives.xlsx",
           sheetName = "EI_avg_scale", append = T, showNA=FALSE)
write.xlsx(EI_table_scale, "./output/egonet_descriptives.xlsx",
           sheetName = "EI_table_scale", append = T, showNA=FALSE)

#save the R workspace
save.image("./output/EgoNet_analysis.RData")
