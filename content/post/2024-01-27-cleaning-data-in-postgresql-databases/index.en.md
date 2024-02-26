---
title: Create a pivot table in PostgreSQL
author: Yen Chun Chen
date: '2024-01-27'
slug: Create-a-pivot-table-in-PostgreSQL
categories:
  - Postgresql
tags:
  - R Markdown
  - Postgresql
subtitle: ''
summary: 'Illustrate how to use a pivot table to report data more clearly.'
authors: []
lastmod: '2024-01-27T14:52:35+08:00'
featured: no
image:
  caption: 'Friends scenes from Reddit'
  focal_point: 'smart'
  preview_only: no
projects: []
output:
  blogdown::html_page:
    toc: true
---

Pivots don't always make people crazy! It could sometimes give us a clear view of the mess data, let's see how it works!

### Setup database table

We will use a dataset with New York City parking violation records.


```r
# Creating a temporal database, only RSQLite has this feature so far
# Connect to the default database
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


### Selecting data

Suppose we want to get a better understanding of which agencies (`issuing_agency`) are responsible for different types of parking violations (`violation_code`).


```sql
-- How many type of agency: 11
SELECT issuing_agency, COUNT(*) AS N
FROM parking_violation
GROUP BY issuing_agency
ORDER BY N DESC
```


<div class="knitsql-table">


Table: <span id="tab:unnamed-chunk-5"></span>Table 1: Displaying records 1 - 10

|issuing_agency |    N|
|:--------------|----:|
|P              | 4068|
|X              |  495|
|S              |  330|
|K              |   59|
|V              |   15|
|C              |   15|
|F              |   11|
|H              |    3|
|D              |    2|
|T              |    1|

</div>

These four agencies will be our main focus:

-   `Police Department` (`P`)

-   `Department of Sanitation` (`S`)

-   `Parks Department` (`K`)

-   `Department of Transportation` (`V`)

### Without a pivot table

Here we will write a `SELECT` query that contains three columns:

- parking violation code

- issuing agency code

- total number of records with each combination



```sql
SELECT 
	violation_code, 
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

The result shows the `violation_code` and `issuing_agency` pairs are repeated throughout the outcomes, ending up with 93 rows!

### With a pivot table

To generate the results in a pivot table format, we will use the `FILTER` clause. It is now easier to present the parking violation by agency for all four with this modified data format.


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

When the results are shown in a pivot table, by just looking at the rows, it is possible to compare the number of violations for each agency.


```r
# Disconnect database
dbDisconnect(con)
```

*Notice: This content was modified from an exercise on DataCamp.*
