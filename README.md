# *The Economist's* tracker for covid-19 excess deaths 

This repository contains the data behind *The Economist’s* [tracker for covid-19 excess deaths](https://www.economist.com/graphic-detail/2020/04/16/tracking-covid-19-excess-deaths-across-countries) (which is free to read), and the code that we have used to clean, analyse and present the numbers.

## Scripts and output data

Our tracker uses two different R scripts to calculate excess deaths in each country:

* [`cleaning_script.R`](scripts/cleaning_script.R): this imports raw data from various sources, and exports a weekly or monthly historical time series of `total_deaths` and `covid_deaths` going back to 2015 (or the earliest available year). If the source for total deaths has also specified a value for `expected_deaths`, that is included too. We remove any weeks or months in which the data might be incomplete. The files are exported to [`/output-data/historical-deaths/`](output-data/historical-deaths).

* [`excess_deaths_script.R`](scripts/excess_deaths_script.R): this imports the time series files from [`/output-data/historical-deaths/`](output-data/historical-deaths), and calculates the weekly or monthly `expected_deaths` for each country. We then use that baseline to calculate `excess_deaths`, and export the files to [`/output-data/excess-deaths/`](output-data/excess-deaths).

There's also an additional script that summarises the data for some graphics in the article:

* [`interactive_script.R`](scripts/interactive_script.R): this imports the files for excess deaths for a list of countries and creates the data for the small multiple chart and the table featured in the article. The files are exported to [`/output-data/interactive/`](output-data/interactive).

## Source data

Our tracker uses data from a number of statistical bureaus, health ministries and government departments. For each country, you can find the relevant source documents in the [`/source-data/`](source-data) folder, including some old versions in each country's `/archive/` folder. Some of the data are automatically downloaded from official websites in [`cleaning_script.R`](scripts/cleaning_script.R), an R file that formats the data consistently across countries.

We have also collated a full list of sources and links in a file called [`list_of_sources.csv`](source-data/list_of_sources.csv). In general we have tried to use the most expansive official estimate of covid-19 deaths available in each country. Belgium, Britain and Sweden all publish restrospectively adjusted estimates of when deaths occurred or were registered, rather than when they were reported. For most other countries, we have used the figures [maintained by the ECDC and Our World In Data](https://ourworldindata.org/coronavirus-source-data). We have subtracted one day from the ECDC's time series (since it uses 10am CET as its cut-off point).

Below is a summary of our sources for each country. 

### Austria

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Statistik Austria                 | Last analysed to July 19th                                        |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | Statistik Austria                 | Weekly average, based on 2016-19                                  |

### Belgium

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Statbel                           | Last analysed to June 7th                                         |
| `covid_deaths`    | Sciensano                         | Retrospectively adjusted, to use day that death occurred          |
| `expected_deaths` | *The Economist*                   | Weekly average, based on 2017-19                                  |

### Brazil

We have used the Registro Civil, with data scraping by Brasil.IO, to import monthly data on deaths from all causes and covid-19 during 2019-20 for five Brazilian cities: São Paulo, Rio de Janeiro, Fortaleza, Manaus and Recife. We have also imported historical data for 2016-18 from Brazil's DataSUS. 

| Variable          | Source                               | Notes                                                             |
| ------------------| -------------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Registro Civil / DataSUS / Brasil.IO | Last analysed to July 31st                                        |
| `covid_deaths`    | Registro Civil / DataSUS / Brasil.IO |                                                                   |
| `expected_deaths` | *The Economist*                      | Monthly average, based on 2016-19                                 |

### Britain

We have combined English and Welsh data from the Office for National Statistics with data from the National Records of Scotland and the Northern Ireland Statistics and Research Agency. Scotland reports data two days after everywhere else. We have used the ONS and NISRA dates as our weekly ending point.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | ONS / NRS / NISRA                 | Last analysed to July 24th                                        |
| `covid_deaths`    | ONS / NRS / NISRA                 | Retrospectively adjusted, to use day that death was registered    |
| `expected_deaths` | *The Economist*                   | Weekly average, based on 2015-19                                  |

### Chile

To follow the latest census in 2017, from which we are importing population estimates, we have grouped the regions of Ñuble and Biobio together. We have imported regional covid-19 death tolls from [a GitHub repository maintained by Data Science Research Peru](https://github.com/DataScienceResearchPeru/covid-19_latinoamerica). Note: Chile's deaths have been higher than the 2015-19 average for most of 2020, possibly because of population growth. We are looking into ways to adjust for this.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Registro Civil                    | Last analysed to July 28th                                        |
| `covid_deaths`    | Ministerio de Salud / DSRP        |                                                                   |
| `expected_deaths` | *The Economist*                   | Weekly average, based on 2015-19                                  |

### Denmark

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Danmarks Statistik                | Last analysed to July 21st                                        |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | *The Economist*                   | Weekly average, based on 2015-19                                  |

### Ecuador

We are importing Ecuador's national covid-19 death toll from the ECDC. Ecuador's Ministerio de Salud Pública is producing [daily PDF bulletins of official covid-19 deaths in each province](https://www.salud.gob.ec/boletin-epidemiologico-por-provincia/), but we have not yet found this data in a machine-readable format, and so have left province-level covid-19 observations blank.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Registro Civil                    | Last analysed to July 31st                                        |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | *The Economist*                   | Monthly average, based on 2018-19                                 |

### France

France's national covid-19 toll, which we are importing from the ECDC, includes deaths in nursing homes. But the regional covid-19 tolls, which we are importing from Santé Publique France, only include deaths in hospitals. Santé Publique France's hospital data begin on March 18th. Before then, we have trained a statistical model to predict each region's share of national hospital deaths.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Insee                             | Last analysed to July 14th                                        |
| `covid_deaths`    | Santé Publique France / ECDC      |                                                                   |
| `expected_deaths` | *The Economist*                   | Weekly average, based on 2015-19                                  |

### Germany

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Destatis                          | Last analysed to June 30th                                        |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | *The Economist*                   | Weekly average, based on 2016-19                                  |

### Indonesia

The only available data about deaths from all causes is [a monthly tally of burials in Jakarta](https://pertamananpemakaman.jakarta.go.id/v140/t15), which comes from the city's Department of Parks and Cemeteries.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Provinsi DKI Jakarta              | Last analysed to May 31st                                         |
| `covid_deaths`    | Provinsi DKI Jakarta              |                                                                   |
| `expected_deaths` | *The Economist*                   | Monthly average, based on 2018-19                                 |

### Italy

| Variable          | Source                                    | Notes                                                             |
| ------------------| ------------------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | ISTAT                                     | Last analysed to May 26th                                         |
| `covid_deaths`    | Dipartimento della Protezione Civile      |                                                                   |
| `expected_deaths` | *The Economist*                           | Weekly average, based on 2015-19                                  |

### Mexico

The only available data about deaths from all causes comes from [Mexico City's Civil Register](http://www.rcivil.cdmx.gob.mx/solicitudactas/busqueda/registrales/clase_acta/DEFUNCION), which has been [analysed by Laurianne Despeghel and Mario Romero Zavala](https://github.com/mariorz/folio-deceso). You can read a full description of their methodology in [this article published by Nexos](https://datos.nexos.com.mx/?p=1388), a Mexican magazine.

| Variable          | Source                               | Notes                                                             |
| ------------------| -------------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Direccion General del Registro Civil | Last analysed to July 26th                                        |
| `covid_deaths`    | ECDC / Our World In Data             |                                                                   |
| `expected_deaths` | *The Economist*                      | Monthly average, based on 2018-19                                 |

### Netherlands

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Centraal Bureau voor de Statistiek | Last analysed to July 19th                                        |
| `covid_deaths`    | ECDC / Our World In Data           |                                                                   |
| `expected_deaths` | *The Economist*                    | Weekly average, based on 2015-19                                  |

### Norway

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Statistisk Sentralbyra             | Last analysed to July 21st                                        |
| `covid_deaths`    | ECDC / Our World In Data           |                                                                   |
| `expected_deaths` | *The Economist*                    | Weekly average, based on 2015-19                                  |

### Peru

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Ministerio de Salud               | Last analysed to July 31st                                        |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | *The Economist*                   | Monthly average, based on 2017-19                                 |

### Portugal

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Ministerio da Saúde                | Last analysed to July 28th                                        |
| `covid_deaths`    | ECDC / Our World In Data           |                                                                   |
| `expected_deaths` | *The Economist*                    | Weekly average, based on 2015-19                                  |

### Russia

Russia's Federal State Statistics Service has only published [regional data about deaths from all causes](https://gks.ru/free_doc/2020/demo/edn03-20.htm) up to March 31st. Instead, we have imported data from the Moscow's Government Open Data Portal, which has released monthly figures up to April 30th.

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Moscow Government Open Data Portal | Last analysed to June 30th                                        |
| `covid_deaths`    | Ministry of Health of Russia       |                                                                   |
| `expected_deaths` | *The Economist*                    | Monthly average, based on 2015-19                                 |

### South Africa

| Variable          | Source                                 | Notes                                                             |
| ------------------| ---------------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | South African Medical Research Council | Last analysed to July 28th                                        |
| `covid_deaths`    | ECDC / Our World In Data               |                                                                   |
| `expected_deaths` | South African Medical Research Council | Baseline modelled by SAMRC                                        |

### Spain

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Instituto de Salud Carlos III      | Last analysed to July 28th                                        |
| `covid_deaths`    | Ministerio de Sanidad / Datadista  |                                                                   |
| `expected_deaths` | Instituto de Salud Carlos III      | Baseline modelled by MoMo                                         |

### Sweden

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Statistiska Centralbyran           | Last analysed to July 21st                                        |
| `covid_deaths`    | Folkhalsomyndigheten               | Retrospectively adjusted, to use day that death occurred          |
| `expected_deaths` | *The Economist*                    | Weekly average, based on 2015-19                                  |

### Switzerland

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Federal Statistical Office         | Last analysed to July 26th                                        |
| `covid_deaths`    | ECDC / Our World In Data           |                                                                   |
| `expected_deaths` | *The Economist*                    | Weekly average, based on 2015-19                                  |

### Turkey

The only available data about deaths from all causes is [a daily tally of burials in Istanbul](https://turkiye.gov.tr/istanbul-buyuksehir-belediyesi-vefat-sorgulama), which comes from the city's Metropolitan Municipality (İstanbul Büyükşehir Belediyesi). We are grateful to Oğuz Işık for collecting this data by hand and sending it to us.

The Turkish government has not released a regional breakdown of covid-19 data [since April 1st](https://www.aa.com.tr/tr/koronavirus/turkiyenin-il-il-kovid-19-vaka-haritasi/1788776), at which point the city had roughly 40% of the nation’s official deaths and 55% of confirmed cases. For our tracker, we have assumed that Istanbul has had 50% of Turkey's official covid-19 death toll, which we are importing from the ECDC.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | İstanbul Büyükşehir Belediyesi    | Last analysed to July 28th                                        |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | *The Economist*                   | Weekly average, based on 2017-19                                  |

### United States

The CDC is publishing weekly data about deaths from all causes in each state and New York City. We are importing data on covid-19 deaths from USA Facts, which collates it from the CDC. For New York City, we are using covid-19 death data from the city's Department of Health and Mental Hygiene, which includes confirmed and probable fatalities. For the time being, we are not reporting a nationwide death toll from all causes, because the quality of data varies from state to state.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | CDC                               | Last analysed to July 11th                                        |
| `covid_deaths`    | CDC / USA Facts / NYC Health      | NYC toll includes "probable" deaths from covid-19                 |
| `expected_deaths` | *The Economist*                   | Weekly average, based on 2015-19                                  |

## Licence

This software is published by [*The Economist*](https://www.economist.com) under the [MIT licence](https://opensource.org/licenses/MIT). The data generated by *The Economist* are available under the [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/).

The data and files that we have generated from official sources are freely available for public use, as long as *The Economist* is cited as a source.

## Authors

This data has been collected, cleaned and analysed by [James Tozer](https://twitter.com/J_CD_T) and [Martín González](https://twitter.com/martgnz). We are grateful to Oğuz Işık for providing data from Turkey; to Laurianne Despeghel and Mario Romero Zavalato for providing data from Mexico; and to Thais Carrança, Helio Gurovitz and Diogo Melo for providing data from Brazil. 

If you use the data, or have any suggestions, please email [jamestozer@economist.com](mailto:jamestozer@economist.com).
