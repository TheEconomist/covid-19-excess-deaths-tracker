# *The Economist's* tracker for covid-19 excess deaths 

This repository contains the data behind *The Economist’s* [tracker for covid-19 excess deaths](https://www.economist.com/graphic-detail/2020/04/16/tracking-covid-19-excess-deaths-across-countries) (which is free to read), and the code that we have used to clean, analyse and present the numbers.

## Source data

Our tracker uses data from a number of statistical bureaus, health ministries and government departments. For each country, you can find the relevant source documents in the [`/source-data/`](source-data) folder, including some old versions in each country's `/archive/` folder. Some of the data are automatically downloaded from official websites in [`cleaning_script.R`](scripts/cleaning_script.R), an R file that formats the data consistently across countries.

We have also collated a full list of sources and links in a file called [`list_of_sources.csv`](source-data/list_of_sources.csv). In general we have tried to use the most expansive official estimate of covid-19 deaths available in each country. Belgium, Britain and Sweden all publish restrospectively adjusted estimates of when deaths occurred or were registered, rather than when they were reported. For most other countries, we have used the figures [maintained by the ECDC and Our World In Data](https://ourworldindata.org/coronavirus-source-data). We have subtracted one day from the ECDC's time series (since it uses 10am CET as its cut-off point).

Below is a summary of our sources for each country. 

### Austria

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Statistik Austria                 | Last analysed to April 5th                                        |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | Statistik Austria                 | Weekly average, based on 2016-19                                  |

### Belgium

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Sciensano                         | Last analysed to April 26th                                       |
| `covid_deaths`    | Sciensano                         | Retrospectively adjusted, to use day that death occurred          |
| `expected_deaths` | Sciensano                         | Weekly average, based on 2015-19                                  |

### Britain

We have combined English and Welsh data from the Office for National Statistics with data from the National Records of Scotland and the Northern Ireland Statistics and Research Agency. Scotland reports data two days after everywhere else. We have used the ONS and NISRA dates as our weekly ending point.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | ONS / NRS / NISRA                 | Last analysed to May 1st                                          |
| `covid_deaths`    | ONS / NRS / NISRA                 | Retrospectively adjusted, to use day that death was registered    |
| `expected_deaths` | *The Economist*                   | Weekly average, based on 2015-19                                  |

### Denmark

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Danmarks Statistik                | Last updated to May 5th                                           |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | *The Economist*                   | Weekly average, based on 2015-19                                  |

### Ecuador

We are importing Ecuador's national covid-19 death toll from the ECDC. Ecuador's Ministerio de Salud Pública is producing [daily PDF bulletins of official covid-19 deaths in each province](https://www.salud.gob.ec/boletin-epidemiologico-por-provincia/), but we have not yet found this data in a machine-readable format, and so have left province-level covid-19 observations blank.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Registro Civil                    | Last analysed to April 30th                                       |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | *The Economist*                   | Monthly average, based on 2018-19                                 |

### France

France's national covid-19 toll, which we are importing from the ECDC, includes deaths in nursing homes. But the regional covid-19 tolls, which we are importing from Santé Publique France, only include deaths in hospitals. Santé Publique France's hospital data begin on March 18th. Before then, we have trained a statistical model to predict each region's share of national hospital deaths.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Insee                             | Last analysed to April 28th                                       |
| `covid_deaths`    | Santé Publique France / ECDC      |                                                                   |
| `expected_deaths` | *The Economist*                   | Weekly average, based on 2015-19                                  |

### Germany

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Destatis                          | Last analysed to April 14th                                       |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | *The Economist*                   | Weekly average, based on 2016-19                                  |

### Indonesia

The only available data about deaths from all causes is [a monthly tally of burials in Jakarta](https://pertamananpemakaman.jakarta.go.id/v140/t15), which comes from the city's Department of Parks and Cemeteries.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Provinsi DKI Jakarta              | Last analysed to April 30th                                       |
| `covid_deaths`    | Provinsi DKI Jakarta              |                                                                   |
| `expected_deaths` | *The Economist*                   | Monthly average, based on 2018-19                                 |

### Italy

Italy's national statistical bureau, ISTAT, has published data about deaths from all causes up to March 31st, covering 6,866 of the country's 7,904 comunes. We have used the covid-19 death toll for the entire country, imported from the Dipartimento della Protezione Civile.

| Variable          | Source                                    | Notes                                                             |
| ------------------| ------------------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | ISTAT                                     | Last analysed to March 31st                                       |
| `covid_deaths`    | Dipartimento della Protezione Civile      |                                                                   |
| `expected_deaths` | *The Economist*                           | Weekly average, based on 2015-19                                  |

### Netherlands

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Centraal Bureau voor de Statistiek | Last analysed to May 10th                                         |
| `covid_deaths`    | ECDC / Our World In Data           |                                                                   |
| `expected_deaths` | *The Economist*                    | Weekly average, based on 2015-19                                  |

### Norway

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Statistisk Sentralbyra             | Last analysed to May 5th                                          |
| `covid_deaths`    | ECDC / Our World In Data           |                                                                   |
| `expected_deaths` | *The Economist*                    | Weekly average, based on 2015-19                                  |

### Portugal

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Ministerio da Saúde                | Last analysed to May 12th                                         |
| `covid_deaths`    | ECDC / Our World In Data           |                                                                   |
| `expected_deaths` | *The Economist*                    | Weekly average, based on 2015-19                                  |

### Spain

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Instituto de Salud Carlos III      | Last analysed to May 5th                                          |
| `covid_deaths`    | Ministerio de Sanidad / Datadista  |                                                                   |
| `expected_deaths` | Instituto de Salud Carlos III      | Modelled MoMo baseline                                            |

### Sweden

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Statistiska Centralbyran           | Last analysed to April 28th                                       |
| `covid_deaths`    | Folkhalsomyndigheten               | Retrospectively adjusted, to use day that death occurred          |
| `expected_deaths` | *The Economist*                    | Weekly average, based on 2015-19                                  |

### Switzerland

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Federal Statistical Office         | Last analysed to May 3rd                                          |
| `covid_deaths`    | ECDC / Our World In Data           |                                                                   |
| `expected_deaths` | *The Economist*                    | Weekly average, based on 2015-19                                  |

### Turkey

The only available data about deaths from all causes is [a daily tally of burials in Istanbul](https://turkiye.gov.tr/istanbul-buyuksehir-belediyesi-vefat-sorgulama), which comes from the city's Metropolitan Municipality (İstanbul Büyükşehir Belediyesi). We are grateful to Oğuz Işık for collecting this data by hand and sending it to us.

The Turkish government has not released a regional breakdown of covid-19 data [since April 1st](https://www.aa.com.tr/tr/koronavirus/turkiyenin-il-il-kovid-19-vaka-haritasi/1788776), at which point the city had roughly 40% of the nation’s official deaths and 55% of confirmed cases. For our tracker, we have assumed that Istanbul has had 50% of Turkey's official covid-19 death toll, which we are importing from the ECDC.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | İstanbul Büyükşehir Belediyesi    | Last analysed to May 12th                                         |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | *The Economist*                   | Weekly average, based on 2017-19                                  |

### United States

The CDC is publishing weekly data about deaths from all causes in each state and New York City. We are importing data on covid-19 deaths from USA Facts, which collates it from the CDC. For New York City, we are using covid-19 death data from the city's Department of Health and Mental Hygiene, which includes confirmed and probable fatalities. For the time being, we are not reporting a nationwide death toll from all causes, because the quality of data varies from state to state.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | CDC                               | Last analysed to April 25th                                       |
| `covid_deaths`    | CDC / USA Facts / NYC Health      | NYC toll includes "probable" deaths from covid-19                 |
| `expected_deaths` | *The Economist*                   | Weekly average, based on 2015-19                                  |

## Scripts and output data

Our tracker uses two different R scripts to calculate excess deaths in each country:

* [`cleaning_script.R`](scripts/cleaning_script.R): this imports raw data from various sources, and exports a weekly or monthly historical time series of `total_deaths` and `covid_deaths` going back to 2015 (or the earliest available year). If the source for total deaths has also specified a value for `expected_deaths`, that is included too. We remove any weeks or months in which the data might be incomplete. The files are exported to [`/output-data/historical-deaths/`](output-data/historical-deaths).

* [`excess_deaths_script.R`](scripts/excess_deaths_script.R): this imports the time series files from [`/output-data/historical-deaths/`](output-data/historical-deaths), and calculates the weekly or monthly `expected_deaths` for each country. We then use that baseline to calculate `excess_deaths`, and export the files to [`/output-data/excess-deaths/`](output-data/excess-deaths).

## Licence

This software is published by [*The Economist*](https://www.economist.com) under the [MIT licence](https://opensource.org/licenses/MIT). The data generated by *The Economist* are available under the [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/).

The data and files that we have generated from official sources are freely available for public use, as long as *The Economist* is cited as a source.

## Authors

This data has been collected, cleaned and analysed by [James Tozer](https://twitter.com/J_CD_T) and [Martín González](https://twitter.com/martgnz). We are grateful to Oğuz Işık for collecting and providing data from Turkey. 

If you use the data, or have any suggestions, please email [jamestozer@economist.com](mailto:jamestozer@economist.com).
