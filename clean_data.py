####
#this script extract edgelist data from policy network survey data (nomination relationships)

#input:
# 1. survey response data spreadsheet (.xlsx file)
#this is the file from the survey platform, which includes the survey questions and responses
# 2. organization codebook (.xlsx file)
#this is the file that includes the org code (e.g., GOV001, EDU003) and org name for each organization in the survey

#output:
# one edgelist (.xlsx file) for each type of relationship
# (1=influence, 2=scientific information, 3=support)

#before we start, make sure:
# you have added new columns for orgs participants specified in the open question (e.g., "Other (please specify)")
# you have assigned a org code (eg., GOV001, EDU003) to each org listed in the survey questions, and this is the first row of the data file
# you have added a column to indicate the org code for each participant's orgnization (in the example data file, the column "actor_code")

import pandas as pd

#read in the survey data
survey_data = pd.read_excel('./input/survey_data.xlsx')

# ——————————— Task 1: pre-process the raw survey response data ——————————— #
# first we noticed there are some long texts in the survey question row that we don't need
#i.e., "Below we have listed organizations that... - Government of Alberta"
# we only need the org name after the dash, so let's remove the redundant text before the dash

Q_indx = survey_data.loc[0,:]
#replace Q_string with the name of orgs
for i in range(3, len(Q_indx)):
    q_string = Q_indx[i]
    org_i = q_string[q_string.rfind("-")+1:].lstrip()
    Q_indx[i] = org_i
survey_data.loc[0,:] = Q_indx

#save a copy of the codebook for later use
codebook = Q_indx.iloc[4:]
#reset the index of the codebook and rename the columns
codebook = codebook.reset_index()
codebook.columns = ['org_code', 'org_name']
codebook.to_excel('./output/org_codebook.xlsx')

#subset the data to only include the relationship nomination responses
net_data = survey_data.iloc[1:, 3:] #in example data, this starts from the second row and the fourth column#in example data, this starts from the second row and the fourth column
net_data = net_data.set_index('actor_code')

# ——————————— Task 2: extract edgelist for each type of networks ——————————— #
#Network type code
# 1=influence network
# 2=scientific information network
# 3=support network

ego_ls = net_data.index.tolist()
net_data = net_data.astype(str)

influence_edgelist = []
for ego in ego_ls:
    ego_row = net_data.loc[ego]
    alter_ls = ego_row.loc[ego_row.str.contains('1')].index.tolist()
    for alter in alter_ls:
        i = ego
        j = alter
        k = '1'
        influence_edgelist.append([i, j, k])
influence_edgelist_df = pd.DataFrame(influence_edgelist, columns=['ego', 'alter', 'net_type'])
influence_edgelist_df.to_excel('./output/influence_edgelist.xlsx')

sciinfo_edgelist = []
for ego in ego_ls:
    ego_row = net_data.loc[ego]
    alter_ls = ego_row.loc[ego_row.str.contains('2')].index.tolist()
    for alter in alter_ls:
        i = ego
        j = alter
        k = '2'
        sciinfo_edgelist.append([i, j, k])
sciinfo_edgelist_df = pd.DataFrame(sciinfo_edgelist, columns=['ego', 'alter', 'net_type'])
sciinfo_edgelist_df.to_excel('./output/sciinfo_edgelist.xlsx')

support_edgelist = []
for ego in ego_ls:
    ego_row = net_data.loc[ego]
    alter_ls = ego_row.loc[ego_row.str.contains('3')].index.tolist()
    for alter in alter_ls:
        i = ego
        j = alter
        k = '3'
        support_edgelist.append([i, j, k])
support_edgelist_df = pd.DataFrame(support_edgelist, columns=['ego', 'alter', 'net_type'])
support_edgelist_df.to_excel('./output/support_edgelist.xlsx')