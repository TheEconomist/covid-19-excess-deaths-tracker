# *The Economist's* tracker for covid-19 excess deaths 

This repository contains the data behind *The Economist’s* [tracker for covid-19 excess deaths](https://www.economist.com/graphic-detail/2020/04/16/tracking-covid-19-excess-deaths-across-countries) (which is free to read), and the code that we have used to clean, analyse and present the numbers.

## Scripts and output data

Our tracker uses two different R scripts to calculate excess deaths in each country:

* [`cleaning_script.R`](scripts/cleaning_script.R): this imports raw data from various sources, and exports a weekly or monthly historical time series of `total_deaths` and `covid_deaths` going back to 2015 (or the earliest available year). If the source that we use for total deaths has also modelled and provided a baseline for `expected_deaths`, we include that too. We remove any weeks or months in which the data might be incomplete. The files are exported to [`/output-data/historical-deaths/`](output-data/historical-deaths).

* [`excess_deaths_script.R`](scripts/excess_deaths_script.R): this imports the time series files from [`/output-data/historical-deaths/`](output-data/historical-deaths), and calculates the weekly or monthly `expected_deaths` for each country (unless a modelled baseline has already been provided). Our own modelled baselines fit a linear trend for year, to account for long-term increases or decreases in mortality, and a fixed effect for each week or month up to February 2020. We have exported these models to [`/output-data/expected-deaths-models/`](output-data/expected-deaths-models). We then use these baselines to calculate `excess_deaths`, and export the files to [`/output-data/excess-deaths/`](output-data/excess-deaths).

There's also an additional script that summarises the data for some graphics in the article:

* [`interactive_script.R`](scripts/interactive_script.R): this imports the files for excess deaths for a list of countries and creates the data for the small multiple chart and the table featured in the article. The files are exported to [`/output-data/interactive/`](output-data/interactive).

## Source data

Our tracker uses data from a number of statistical bureaus, health ministries and government departments. For each country, you can find the relevant source documents in the [`/source-data/`](source-data) folder, including some old versions in each country's `/archive/` folder. Some of the data are automatically downloaded from official websites in [`cleaning_script.R`](scripts/cleaning_script.R), an R file that formats the data consistently across countries.

We have also collated a full list of sources and links in a file called [`list_of_sources.csv`](source-data/list_of_sources.csv). In general we have tried to use the most expansive official estimate of covid-19 deaths available in each country. Belgium, Britain and Sweden all publish restrospectively adjusted estimates of when deaths occurred or were registered, rather than when they were reported. For most other countries, we have used the figures [maintained by the ECDC and Our World In Data](https://ourworldindata.org/coronavirus-source-data). We have subtracted one day from the ECDC's time series (since it uses 10am CET as its cut-off point).

Below is a summary of our sources for each country. 

### Austria

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Statistik Austria                 | Last analysed to September 6th                                    |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | Statistik Austria                 | Weekly modelled baseline, trained on 2016-19                      |

### Belgium

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Statbel                           | Last analysed to August 4th                                       |
| `covid_deaths`    | Sciensano                         | Retrospectively adjusted, to use day that death occurred          |
| `expected_deaths` | *The Economist*                   | Weekly modelled baseline, trained on 2016-19                      |

### Brazil

We have used [estimates from Vital Strategies](https://www.vitalstrategies.org/resources/excess-mortality-in-brazil-a-detailed-description-of-trends-in-mortality-during-the-covid-19-pandemic/), a non-profit, which has compared `total_deaths` from the Registro Civil for 2019-20 to those from the Sistema de Informações sobre Mortalidade (SIM) for 2015-19. Vital Strategies' estimates account for possible under-reporting in each region by analysing the ratio of `total_deaths` in 2019 between the Registro Civil and SIM, and adjusting the 2020 Registro Civil data accordingly.

| Variable          | Source                                  | Notes                                                             |
| ------------------| ----------------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Vital Strategies / Registro Civil / SIM | Last analysed to August 15th                                      |
| `covid_deaths`    | ECDC / Our World In Data                |                                                                   |
| `expected_deaths` | Vital Strategies                        | Weekly modelled baseline, trained by Vital Strategies             |

### Britain

We have combined English and Welsh data from the Office for National Statistics with data from the National Records of Scotland and the Northern Ireland Statistics and Research Agency. Scotland reports data two days after everywhere else. We have used the ONS and NISRA dates as our weekly ending point.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | ONS / NRS / NISRA                 | Last analysed to September 4th                                    |
| `covid_deaths`    | ONS / NRS / NISRA                 | Retrospectively adjusted, to use day that death was registered    |
| `expected_deaths` | *The Economist*                   | Weekly modelled baseline, trained on 2015-19                      |

### Chile

To follow the latest census in 2017, from which we are importing population estimates, we have grouped the regions of Ñuble and Biobio together. We have imported regional `covid_deaths` from [a GitHub repository maintained by Data Science Research Peru](https://github.com/DataScienceResearchPeru/covid-19_latinoamerica).

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Registro Civil                    | Last analysed to September 15th                                   |
| `covid_deaths`    | Ministerio de Salud / DSRP        |                                                                   |
| `expected_deaths` | *The Economist*                   | Weekly modelled baseline, trained on 2015-19                      |

### Denmark

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Danmarks Statistik                | Last analysed to September 8th                                    |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | *The Economist*                   | Weekly modelled baseline, trained on 2015-19                      |

### Ecuador

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Registro Civil                    | Last analysed to August 31st                                      |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | *The Economist*                   | Monthly modelled baseline, trained on 2018-19                     |

### France

France's national `covid_deaths`, which we are importing from the ECDC, includes deaths in nursing homes. But the regional `covid_deaths`, which we are importing from Santé Publique France, only include deaths in hospitals. Santé Publique France's hospital data begin on March 18th. Before then, we have trained a statistical model to predict each region's share of national hospital deaths.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Insee                             | Last analysed to September 1st                                    |
| `covid_deaths`    | Santé Publique France / ECDC      |                                                                   |
| `expected_deaths` | *The Economist*                   | Weekly modelled baseline, trained on 2015-19                      |

### Germany

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Destatis                          | Last analysed to August 18th                                      |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | *The Economist*                   | Weekly modelled baseline, trained on 2016-19                      |

### Indonesia

The only available data about deaths from all causes is [a monthly tally of burials in Jakarta](https://pertamananpemakaman.jakarta.go.id/v140/t15), which comes from the city's Department of Parks and Cemeteries.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Provinsi DKI Jakarta              | Last analysed to May 31st                                         |
| `covid_deaths`    | Provinsi DKI Jakarta              |                                                                   |
| `expected_deaths` | *The Economist*                   | Monthly modelled baseline, trained on 2018-19                     |

### Italy

| Variable          | Source                                    | Notes                                                             |
| ------------------| ------------------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | ISTAT                                     | Last analysed to June 30th                                        |
| `covid_deaths`    | Dipartimento della Protezione Civile      |                                                                   |
| `expected_deaths` | *The Economist*                           | Weekly modelled baseline, trained on 2015-19                      |

### Mexico

We have traced data on `total_deaths` and `expected_deaths` from [a report](https://www.gob.mx/cms/uploads/attachment/file/576411/CP_Salud_CTD_coronavirus_COVID-19__05sep20.pdf#page=14) by the Secretaría de Salud on September 5th. These data, and the modelled baseline, cover 24 of Mexico's 32 federal states, including 85.5% of the population. We have imported data for `covid_deaths` from the ECDC, which cover all 32 states.


| Variable          | Source                               | Notes                                                             |
| ------------------| -------------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Secretaría de Salud                  | Last analysed to August 1st                                       |
| `covid_deaths`    | ECDC / Our World In Data             |                                                                   |
| `expected_deaths` | *The Economist*                      | Monthly average, based on 2018-19                                 |

### Netherlands

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Centraal Bureau voor de Statistiek | Last analysed to September 6th                                    |
| `covid_deaths`    | ECDC / Our World In Data           |                                                                   |
| `expected_deaths` | *The Economist*                    | Weekly modelled baseline, trained on 2015-19                      |

### Norway

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Statistisk Sentralbyra             | Last analysed to August 25th                                      |
| `covid_deaths`    | ECDC / Our World In Data           |                                                                   |
| `expected_deaths` | *The Economist*                    | Weekly modelled baseline, trained on 2015-19                      |

### Peru

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Ministerio de Salud               | Last analysed to August 31st                                      |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | *The Economist*                   | Monthly modelled baseline, trained on 2017-19                     |

### Portugal

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Ministerio da Saúde                | Last analysed to September 15th                                   |
| `covid_deaths`    | ECDC / Our World In Data           |                                                                   |
| `expected_deaths` | *The Economist*                    | Weekly modelled baseline, trained on 2015-19                      |

### Russia

*The Moscow Times* has collected [monthly data](https://www.themoscowtimes.com/2020/09/04/russia-records-30k-excess-deaths-in-july-most-month-fatalities-in-decade-a71354) for `total_deaths` in Russia from Rosstat, Russia's national statistical bureau. We have modelled our own baseline for `expected_deaths`, which predicts fewer deaths in 2020 than a simple five-year average does. This is because the number of deaths that Russia recorded each year fell steadily from 2015-19.

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Rosstat / *The Moscow Times*       | Last analysed to July 31st                                        |
| `covid_deaths`    | ECDC / Our World In Data           |                                                                   |
| `expected_deaths` | *The Economist*                    | Monthly modelled baseline, trained on 2015-19                     |

### South Africa

| Variable          | Source                                 | Notes                                                             |
| ------------------| ---------------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | South African Medical Research Council | Last analysed to September 8th                                    |
| `covid_deaths`    | ECDC / Our World In Data               |                                                                   |
| `expected_deaths` | South African Medical Research Council | Weekly modelled baseline, trained by SAMRC                        |

### Spain

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Instituto de Salud Carlos III      | Last analysed to September 15th                                   |
| `covid_deaths`    | Ministerio de Sanidad / Datadista  |                                                                   |
| `expected_deaths` | Instituto de Salud Carlos III      | Daily modelled baseline, trained by MoMo                          |

### Sweden

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Statistiska Centralbyran           | Last analysed to September 8th                                    |
| `covid_deaths`    | Folkhalsomyndigheten               | Retrospectively adjusted, to use day that death occurred          |
| `expected_deaths` | *The Economist*                    | Monthly modelled baseline, trained on 2015-19                     |

### Switzerland

| Variable          | Source                             | Notes                                                             |
| ------------------| -----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | Federal Statistical Office         | Last analysed to September 6th                                    |
| `covid_deaths`    | ECDC / Our World In Data           |                                                                   |
| `expected_deaths` | *The Economist*                    | Monthly modelled baseline, trained on 2015-19                     |

### Turkey

The only available data about `total_deaths` is [a daily tally of burials in Istanbul](https://turkiye.gov.tr/istanbul-buyuksehir-belediyesi-vefat-sorgulama), which comes from the city's Metropolitan Municipality (İstanbul Büyükşehir Belediyesi). We are grateful to Oğuz Işık for collecting this data by hand and sending it to us.

The Turkish government has not released a regional breakdown of covid-19 data [since April 1st](https://www.aa.com.tr/tr/koronavirus/turkiyenin-il-il-kovid-19-vaka-haritasi/1788776), at which point the city had roughly 40% of the nation’s official deaths and 55% of confirmed cases. For our tracker, we have assumed that Istanbul has had 50% of Turkey's official `covid_deaths`, which we are importing from the ECDC.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | İstanbul Büyükşehir Belediyesi    | Last analysed to August 25th                                      |
| `covid_deaths`    | ECDC / Our World In Data          |                                                                   |
| `expected_deaths` | *The Economist*                   | Monthly modelled baseline, trained on 2017-19                     |

### United States

The CDC is publishing weekly data about `total_deaths` in each state and New York City, which it is also correcting for under-reporting in recent weeks. We are importing data on `covid_deaths` from USA Facts, which are collated from the CDC. For New York City, we are using `covid_deaths` from the city's Department of Health and Mental Hygiene, which includes confirmed and probable fatalities.

| Variable          | Source                            | Notes                                                             |
| ------------------| ----------------------------------|-------------------------------------------------------------------|
| `total_deaths`    | CDC                               | Last analysed to August 22nd                                      |
| `covid_deaths`    | CDC / USA Facts / NYC Health      | NYC toll includes "probable" deaths from covid-19                 |
| `expected_deaths` | *The Economist*                   | Monthly modelled baseline, trained on 2015-19                     |

## Licence

This software is published by [*The Economist*](https://www.economist.com) under the [MIT licence](https://opensource.org/licenses/MIT). The data generated by *The Economist* are available under the [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/).

The data and files that we have generated from official sources are freely available for public use, as long as *The Economist* is cited as a source.

## Authors

This data has been collected, cleaned and analysed by [James Tozer](https://twitter.com/J_CD_T) and [Martín González](https://twitter.com/martgnz). We are grateful to Oğuz Işık for providing data from Istanbul; to Laurianne Despeghel and Mario Romero Zavalato for providing data from Mexico City; to Thais Carrança, Helio Gurovitz and Diogo Melo for providing data from Brazilian cities; and to Vital Strategies for providing data from the whole of Brazil. 

If you use the data, or have any suggestions, please email [jamestozer@economist.com](mailto:jamestozer@economist.com).
