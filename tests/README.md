Tests and Coverage
================
16 May, 2024 20:10:23

- [Coverage](#coverage)
- [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                          | Coverage (%) |
|:------------------------------------------------|:------------:|
| pannotator                                      |    48.21     |
| [R/run_app.R](../R/run_app.R)                   |     0.00     |
| [R/fct_helpers.R](../R/fct_helpers.R)           |     5.56     |
| [R/mod_leaflet_map.R](../R/mod_leaflet_map.R)   |    53.47     |
| [R/mod_360_image.R](../R/mod_360_image.R)       |    62.18     |
| [R/mod_control_form.R](../R/mod_control_form.R) |    66.73     |
| [R/app_config.R](../R/app_config.R)             |    100.00    |
| [R/app_server.R](../R/app_server.R)             |    100.00    |
| [R/app_ui.R](../R/app_ui.R)                     |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file                                                          |   n | time | error | failed | skipped | warning |
|:--------------------------------------------------------------|----:|-----:|------:|-------:|--------:|--------:|
| [test-app.R](testthat/test-app.R)                             |   1 | 0.05 |     0 |      0 |       0 |       0 |
| [test-fct_helpers.R](testthat/test-fct_helpers.R)             |   1 | 0.00 |     0 |      0 |       0 |       0 |
| [test-golem-recommended.R](testthat/test-golem-recommended.R) |  10 | 5.46 |     0 |      0 |       0 |       0 |
| [test-mod_360_image.R](testthat/test-mod_360_image.R)         |   2 | 0.01 |     0 |      0 |       0 |       0 |
| [test-mod_control_form.R](testthat/test-mod_control_form.R)   |   2 | 0.22 |     0 |      0 |       0 |       0 |
| [test-mod_leaflet_map.R](testthat/test-mod_leaflet_map.R)     |   2 | 0.03 |     0 |      0 |       0 |       0 |

<details closed>
<summary>
Show Detailed Test Results
</summary>

| file                                                                  | context           | test                 | status |   n | time |
|:----------------------------------------------------------------------|:------------------|:---------------------|:-------|----:|-----:|
| [test-app.R](testthat/test-app.R#L2)                                  | app               | multiplication works | PASS   |   1 | 0.05 |
| [test-fct_helpers.R](testthat/test-fct_helpers.R#L2)                  | fct_helpers       | multiplication works | PASS   |   1 | 0.00 |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L3)      | golem-recommended | app ui               | PASS   |   2 | 0.26 |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L13)     | golem-recommended | app server           | PASS   |   4 | 0.04 |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L24_L26) | golem-recommended | app_sys works        | PASS   |   1 | 0.01 |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L36_L42) | golem-recommended | golem-config works   | PASS   |   2 | 0.02 |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L72)     | golem-recommended | app launches         | PASS   |   1 | 5.13 |
| [test-mod_360_image.R](testthat/test-mod_360_image.R#L31)             | mod_360_image     | module ui works      | PASS   |   2 | 0.01 |
| [test-mod_control_form.R](testthat/test-mod_control_form.R#L31)       | mod_control_form  | module ui works      | PASS   |   2 | 0.22 |
| [test-mod_leaflet_map.R](testthat/test-mod_leaflet_map.R#L31)         | mod_leaflet_map   | module ui works      | PASS   |   2 | 0.03 |

</details>
<details>
<summary>
Session Info
</summary>

| Field    | Value                             |
|:---------|:----------------------------------|
| Version  | R version 4.3.1 (2023-06-16 ucrt) |
| Platform | x86_64-w64-mingw32/x64 (64-bit)   |
| Running  | Windows 10 x64 (build 19045)      |
| Language | English_Australia                 |
| Timezone | Australia/Hobart                  |

| Package  | Version |
|:---------|:--------|
| testthat | 3.2.1.1 |
| covr     | 3.6.4   |
| covrpage | 0.2     |

</details>
<!--- Final Status : pass --->
