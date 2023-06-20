# EgoNet-COMPON
Scripts for data cleaning, analysis, and visualization using ego network analysis in policy network analysis

## clean_data.py
Converts policy network nomination survey data into edgelist.

example **survey data**
|      |GOV19|POL006|GOV007|
|------|-----|------|------|
|NGO001|1,2,3|1     |2,3   |
|NGO006|     |      |1     |		

example **dgelist** (for relationship type 1)
| ego | alter | relationship type|
| ----| ----- |------- |
|NGO001|GOV019|1|
|NGO001|POL006|1|
|NGO006|GOV007|1|


## EgoNet_analysis.R
Performs descriptive Ego-centric network analysis, primarily using the *egor* library:

Density
| egoID | density |
| -------- | ------- |
| 1  | 0.4|
| 2 | 0 |
| 3 | 0.1666|

Composition
| egoID| ENG |GOV|INT|NGO|POL|RES|UNI
|-----|----|---|---|---|---|---|---|
|1|83.33|	 |	|16.67|   |   |   |
|2||	25 |	||   |  50 |  25 |
|3||	 |69.23	|| 15.38  |   | 15.38  |

Ego–Alter homophily
| egoID | EI index |
| -------- | ------- |
|1	|-0.666667|
|2	|1.00000|
|3	|0.2365|


## EgoNet_visualization.R
Produces visualizations:

circled bar chat for nomination counts (by group, e.g., sector/political scale)
![support_n](https://github.com/yixiyang21/EgoNet-COMPON/assets/52981994/318e2a6d-18e6-459c-800f-cfc3fda08de4)

stacked bar chart for network composition (by group, e.g., sector/political scale)
![composition](https://github.com/yixiyang21/EgoNet-COMPON/assets/52981994/1508c3f8-8ff8-47a1-85e7-fd7cbd57c2de)

individual participant's ego-centric network

![sci2](https://github.com/yixiyang21/EgoNet-COMPON/assets/52981994/08802839-eddd-4819-9074-bc927634554f)

egograms

![sci_info3_2](https://github.com/yixiyang21/EgoNet-COMPON/assets/52981994/10dde301-17e0-4556-9b45-d7e79f673e6f)





