---
title: Create a pivot table in postgresql
author: Yen Chun Chen
date: '2024-01-27'
slug: Create-a-pivot-table-in-postgresql
categories:
  - Postgresql
tags:
  - R Markdown
  - Postgresql
subtitle: ''
summary: 'Illustrate how to use pivot table to report data more clearly.'
authors: []
lastmod: '2024-01-27T14:52:35+08:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
output:
  blogdown::html_page:
    toc: true
---

### Setup database table

We will be using a dataset with 5000 New York City parking violation records stored in the `parking_violation` table.


```r
# Creating a new database
# Connect to the default postgres database
library(DBI)
con <- dbConnect(RSQLite::SQLite(), "")
knitr::opts_chunk$set(connection = "con")
```






```r
# Glimpse dataset
dplyr::glimpse(parking_violation)
```

```
## Rows: 5,000
## Columns: 43
## $ summons_number                    <dbl> 1447152396, 1447152402, 1447152554, …
## $ plate_id                          <chr> "JET2661", "JCV6523", "GMK6954", "JG…
## $ registration_state                <chr> "NY", "NY", "NY", "NY", "NY", "NY", …
## $ plate_type                        <chr> "PAS", "PAS", "PAS", "PAS", "COM", "…
## $ issue_date                        <chr> "06/28/2019", "06/28/2019", "06/16/2…
## $ violation_code                    <int> 21, 20, 19, 19, 48, 46, 40, 46, 40, …
## $ vehicle_body_type                 <chr> "SDN", "SDN", "SUBN", "SDN", NA, "SU…
## $ vehicle_make                      <chr> "BMW", "TOYOT", "BMW", "AUDI", NA, "…
## $ issuing_agency                    <chr> "P", "P", "P", "P", "P", "P", "P", "…
## $ street_code1                      <int> 27390, 36290, 36270, 36270, 31190, 3…
## $ street_code2                      <int> 36290, 27390, 11710, 11710, 36310, 1…
## $ street_code3                      <int> 36350, 13113, 27390, 27390, 36330, 2…
## $ vehicle_expiration_date           <int> 20210306, 20210109, 20190720, 202104…
## $ violation_location                <int> 26, 26, 26, 26, 26, 26, 26, 26, 26, …
## $ violation_precinct                <int> 26, 26, 26, 26, 26, 26, 26, 26, 26, …
## $ issuer_precinct                   <int> 26, 26, 26, 26, 26, 26, 26, 26, 26, …
## $ issuer_code                       <int> 964055, 964055, 927590, 927590, 9634…
## $ issuer_command                    <chr> "0026", "0026", "0026", "0026", "002…
## $ issuer_squad                      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ violation_time                    <chr> "1000A", "1011A", "0107A", "0300A", …
## $ time_first_observed               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ violation_county                  <chr> "NY", "NY", NA, NA, "NY", "NY", "NY"…
## $ violation_in_front_of_or_opposite <chr> "F", "F", "F", "F", "F", "F", "O", "…
## $ house_number                      <chr> "21", "545", "509", "501", "341", "5…
## $ street_name                       <chr> "OLD BROADWAY", "W 126 STREET", "W 1…
## $ intersecting_street               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ date_first_observed               <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ law_section                       <int> 408, 408, 408, 408, 408, 408, 408, 4…
## $ sub_division                      <chr> "D1", "D1", "F1", "F2", "F1", "C", "…
## $ violation_legal_code              <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ days_parking_in_effect            <chr> "BYBBYBB", "BBBBBBB", "BBBBBBB", "BB…
## $ from_hours_in_effect              <chr> "0900A", "ALL", "ALL", "ALL", "ALL",…
## $ to_hours_in_effect                <chr> "1030A", "ALL", "ALL", "ALL", "ALL",…
## $ vehicle_color                     <chr> "GRY", "GY", "BLK", "BLK", NA, "GY",…
## $ unregistered_vehicle_             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ vehicle_year                      <int> 0, 0, 2019, 2015, 0, 0, 2005, 2003, …
## $ meter_number                      <chr> "-", "-", "-", "-", "-", "-", "-", "…
## $ feet_from_curb                    <int> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, …
## $ violation_post_code               <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ violation_description             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ no_standing_or_stopping_violation <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ hydrant_violation                 <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ double_parking_violation          <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, …
```


```r
# Create database tables
dbWriteTable(con, "parking_violation", parking_violation)
dbListTables(con)
```

```
## [1] "parking_violation"
```


```sql
-- Glimpse table
SELECT *
FROM parking_violation
LIMIT 5
```


<div class="knitsql-table">


Table: <span id="tab:unnamed-chunk-5"></span>Table 1: 5 records

| summons_number|plate_id |registration_state |plate_type |issue_date | violation_code|vehicle_body_type |vehicle_make |issuing_agency | street_code1| street_code2| street_code3| vehicle_expiration_date| violation_location| violation_precinct| issuer_precinct| issuer_code|issuer_command | issuer_squad|violation_time |time_first_observed |violation_county |violation_in_front_of_or_opposite |house_number |street_name        |intersecting_street | date_first_observed| law_section|sub_division | violation_legal_code|days_parking_in_effect |from_hours_in_effect |to_hours_in_effect |vehicle_color | unregistered_vehicle_| vehicle_year|meter_number | feet_from_curb| violation_post_code|violation_description | no_standing_or_stopping_violation| hydrant_violation| double_parking_violation|
|--------------:|:--------|:------------------|:----------|:----------|--------------:|:-----------------|:------------|:--------------|------------:|------------:|------------:|-----------------------:|------------------:|------------------:|---------------:|-----------:|:--------------|------------:|:--------------|:-------------------|:----------------|:---------------------------------|:------------|:------------------|:-------------------|-------------------:|-----------:|:------------|--------------------:|:----------------------|:--------------------|:------------------|:-------------|---------------------:|------------:|:------------|--------------:|-------------------:|:---------------------|---------------------------------:|-----------------:|------------------------:|
|     1447152396|JET2661  |NY                 |PAS        |06/28/2019 |             21|SDN               |BMW          |P              |        27390|        36290|        36350|                20210306|                 26|                 26|              26|      964055|0026           |            0|1000A          |NA                  |NY               |F                                 |21           |OLD BROADWAY       |NA                  |                   0|         408|D1           |                   NA|BYBBYBB                |0900A                |1030A              |GRY           |                     0|            0|-            |              0|                  NA|NA                    |                                NA|                NA|                       NA|
|     1447152402|JCV6523  |NY                 |PAS        |06/28/2019 |             20|SDN               |TOYOT        |P              |        36290|        27390|        13113|                20210109|                 26|                 26|              26|      964055|0026           |            0|1011A          |NA                  |NY               |F                                 |545          |W 126 STREET       |NA                  |                   0|         408|D1           |                   NA|BBBBBBB                |ALL                  |ALL                |GY            |                     0|            0|-            |              0|                  NA|NA                    |                                NA|                NA|                       NA|
|     1447152554|GMK6954  |NY                 |PAS        |06/16/2019 |             19|SUBN              |BMW          |P              |        36270|        11710|        27390|                20190720|                 26|                 26|              26|      927590|0026           |            0|0107A          |NA                  |NA               |F                                 |509          |W 125 ST           |NA                  |                   0|         408|F1           |                   NA|BBBBBBB                |ALL                  |ALL                |BLK           |                     0|         2019|-            |              0|                  NA|NA                    |                                NA|                NA|                       NA|
|     1447152580|JGX1641  |NY                 |PAS        |06/24/2019 |             19|SDN               |AUDI         |P              |        36270|        11710|        27390|                20210408|                 26|                 26|              26|      927590|0026           |            0|0300A          |NA                  |NA               |F                                 |501          |W 125 ST           |NA                  |                   0|         408|F2           |                   NA|BBBBBBB                |ALL                  |ALL                |BLK           |                     0|         2015|-            |              0|                  NA|NA                    |                                NA|                NA|                       NA|
|     1447152724|GDM8069  |NY                 |COM        |07/06/2019 |             48|NA                |NA           |P              |        31190|        36310|        36330|                20210109|                 26|                 26|              26|      963447|0026           |            0|0653A          |NA                  |NY               |F                                 |341          |ST NICHOLAS AVENUE |NA                  |                   0|         408|F1           |                   NA|BBBBBBB                |ALL                  |ALL                |NA            |                     0|            0|-            |              0|                  NA|NA                    |                                NA|                NA|                       NA|

</div>

### Selecting data

In an effort to get a better understanding of which agencies are responsible for different types of parking violations, you have been tasked with creating a report providing these details.

This report will focus on four issuing agencies:

-   `Police Department` (`P`)

-   `Department of Sanitation` (`S`)

-   `Parks Department` (`K`)

-   `Department of Transportation` (`V`)

An `INTEGER` `violation_code` and `CHAR` `issuing_agency` is recorded for every `parking_violation`.

Here we will write a `SELECT` query that provides the underlying data for the report: the parking violation code, the issuing agency code, and the total number of records with each pair of values.


```sql
SELECT 
	-- Include the violation code in results
	violation_code, 
    -- Include the issuing agency in results
    issuing_agency, 
    -- Number of records with violation code/issuing agency
    COUNT(*) 
FROM 
	parking_violation 
WHERE 
	-- Restrict the results to the agencies of interest
	issuing_agency IN ('P', 'S', 'K', 'V') 
GROUP BY 
	-- Define GROUP BY columns to ensure correct pair count
	violation_code, issuing_agency
ORDER BY 
	violation_code, issuing_agency;
```


<div class="knitsql-table">


Table: <span id="tab:unnamed-chunk-6"></span>Table 2: Displaying records 1 - 10

| violation_code|issuing_agency | COUNT(*)|
|--------------:|:--------------|--------:|
|              4|P              |        2|
|              5|V              |       15|
|              6|P              |        3|
|              9|K              |        1|
|              9|P              |        4|
|              9|S              |        1|
|             10|P              |        3|
|             11|P              |        1|
|             13|K              |        1|
|             14|K              |        4|

</div>

However, while this representation provides all of the information that we want, *the `violation_code` and `issuing_agency` pairs are repeated throughout the results.*

### Using FILTER to create pivot table

Using the `FILTER` clause to produce results in a pivot table format. This improved presentation of the data can more easily be used in the report for parking violations issued by each of the four agencies of interest.


```sql
SELECT 
	violation_code, 
    -- Define the "Police" column
	COUNT(issuing_agency) FILTER (WHERE issuing_agency = 'P') AS "Police",
    -- Define the "Sanitation" column
	COUNT(issuing_agency) FILTER (WHERE issuing_agency = 'S') AS "Sanitation",
    -- Define the "Parks" column
	COUNT(issuing_agency) FILTER (WHERE issuing_agency = 'K') AS "Parks",
    -- Define the "Transportation" column
	COUNT(issuing_agency) FILTER (WHERE issuing_agency = 'V') AS "Transportation"
FROM 
	parking_violation 
GROUP BY 
	violation_code
ORDER BY 
	violation_code
```


<div class="knitsql-table">


Table: <span id="tab:unnamed-chunk-7"></span>Table 3: Displaying records 1 - 10

| violation_code| Police| Sanitation| Parks| Transportation|
|--------------:|------:|----------:|-----:|--------------:|
|              4|      2|          0|     0|              0|
|              5|      0|          0|     0|             15|
|              6|      3|          0|     0|              0|
|              9|      4|          1|     1|              0|
|             10|      3|          0|     0|              0|
|             11|      1|          0|     0|              0|
|             13|      0|          0|     1|              0|
|             14|    485|          4|     4|              0|
|             16|     10|          0|     0|              0|
|             17|     69|          8|     0|              0|

</div>

Notice how much easier the results are to read when presenting the results as a pivot table. The number of violations of each type can be compared across agencies by simply visually scanning the rows.


```r
# Disconnect database
dbDisconnect(con)
```

*Notice: this content was edited from Datacamp's exercise.*
