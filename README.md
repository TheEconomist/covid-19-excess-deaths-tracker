# *The Economist's* tracker for covid-19 excess deaths 

This repository contains the data behind *The Economist’s* [tracker for covid-19 excess deaths](https://www.economist.com/graphic-detail/coronavirus-excess-deaths-tracker) and the code that we have used to clean, analyse and present the numbers.

## Scripts and output data

Our tracker uses two R scripts to calculate excess deaths in each country:

* [`cleaning_script.R`](scripts/cleaning_script.R): this imports raw data from various sources, and exports a weekly or monthly historical time series of `total_deaths` and official `covid_deaths` going back to 2015 (or the earliest available year). If the source that we use for `total deaths` has also modelled and provided a baseline for `expected_deaths`, we include that too. We remove any weeks or months in which the data might be incomplete. The files are exported to [`/output-data/historical-deaths/`](output-data/historical-deaths).

* [`excess_deaths_script.R`](scripts/excess_deaths_script.R): this imports the time series files from [`/output-data/historical-deaths/`](output-data/historical-deaths), and calculates the weekly or monthly `expected_deaths` for each country (unless a modelled baseline has already been provided). Our own modelled baselines fit a linear trend for year, to account for long-term increases or decreases in mortality, and a fixed effect for each week or month up to February 2020. We have exported these models to [`/output-data/expected-deaths-models/`](output-data/expected-deaths-models). We then use these baselines to calculate `excess_deaths`, and export the files to [`/output-data/excess-deaths/`](output-data/excess-deaths).

There is a third R script that summarises the data for some graphics in the article:

* [`interactive_script.R`](scripts/interactive_script.R): this imports the files for `excess deaths` for a list of countries and creates the data for the small multiple chart and the table featured in the article. The files are exported to [`/output-data/interactive/`](output-data/interactive).

## Source data

Our tracker uses data from a number of statistical bureaus, government departments and academic projects. For many of the countries, we have imported `total_deaths` from the [Human Mortality Database](https://www.mortality.org/), which collates detailed weekly breakdowns from official sources around the world. For other countries, you can find a full list of sources and links in a file called [`list_of_sources.csv`](source-data/list_of_sources.csv), as well as spreadsheets in the [`/source-data/`](source-data) folder.

For most countries, we have imported national figures on official `covid deaths` from a time series [maintained by Johns Hopkins University and Our World In Data](https://ourworldindata.org/coronavirus-source-data). For some countries, we have provided a regional breakdown of mortality. In these cases, we have imported regional `covid_deaths` from a variety of sources, including a Latin American time series maintained by [Data Science Research Peru](https://github.com/DataScienceResearchPeru/covid-19_latinoamerica).

Below is a summary of our sources. 

| Country        | Sources                                                        | Notes                                                                             |
| ---------------| ---------------------------------------------------------------|-----------------------------------------------------------------------------------|
| Australia      | HMD, OWID, JHU                                                 | The data only include deaths certified by doctors (about 85% of the total).       |
| Austria        | HMD, OWID, JHU                                                 |                                                                                   |
| Belgium        | HMD, OWID, JHU                                                 |                                                                                   |
| Bolivia        | Registro Civil, Data Science Research Peru                     | We excluded data for April 2020, when the Registro Civil was shut.                |
| Brazil         | Vital Strategies, Registro Civil, SIM, OWID, JHU               | We used Vital Strategies's modelled baseline for `expected deaths`.               |
| Britain        | ONS / NRS / NISRA                                              | We used weekly registered `covid deaths` as the official toll.                    | 
| Bulgaria       | HMD, OWID, JHU                                                 |                                                                                   |
| Canada         | HMD, OWID, JHU                                                 |                                                                                   |
| Chile          | Registro Civil, Data Science Research Peru                     | We grouped the regions of Ñuble and Biobio together.                              |
| Colombia       | DANE, OWID, JHU                                                |                                                                                   |
| Croatia        | HMD, OWID, JHU                                                 |                                                                                   |
| Czech Republic | HMD, OWID, JHU                                                 |                                                                                   |
| Denmark        | HMD, OWID, JHU                                                 |                                                                                   |
| Ecuador        | Registro Civil, OWID, JHU                                      |                                                                                   |
| Estonia        | HMD, OWID, JHU                                                 |                                                                                   |
| Finland        | HMD, OWID, JHU                                                 |                                                                                   |
| France         | INSEE, Santé Publique France, OWID, JHU                        |                                                                                   |
| Germany        | HMD, OWID, JHU                                                 |                                                                                   |
| Greece         | HMD, OWID, JHU                                                 |                                                                                   |
| Hungary        | HMD, OWID, JHU                                                 |                                                                                   |
| Iceland        | HMD, OWID, JHU                                                 |                                                                                   |
| Indonesia      | Provinsi DKI Jakarta                                           | We analysed monthly burials in Jakarta's parks and cemeteries.                    |
| Israel         | HMD, OWID, JHU                                                 |                                                                                   |
| Italy          | ISTAT, Dipartimento della Protezione Civile                    |                                                                                   |
| Latvia         | HMD, OWID, JHU                                                 |                                                                                   |
| Lithuania      | HMD, OWID, JHU                                                 |                                                                                   |
| Luxembourg     | HMD, OWID, JHU                                                 |                                                                                   |
| Mexico         | Secretaría de Salud, OWID, JHU                                 |                                                                                   |
| Netherlands    | HMD, OWID, JHU                                                 |                                                                                   |
| New Zealand    | HMD, OWID, JHU                                                 |                                                                                   |
| Norway         | HMD, OWID, JHU                                                 |                                                                                   |
| Peru           | Ministerio de Salud, OWID, JHU                                 |                                                                                   |
| Poland         | HMD, OWID, JHU                                                 |                                                                                   |
| Portugal       | HMD, OWID, JHU                                                 |                                                                                   |
| Russia         | Rosstat, OWID, JHU                                             |                                                                                   |
| Slovakia       | HMD, OWID, JHU                                                 |                                                                                   |
| Slovenia       | HMD, OWID, JHU                                                 |                                                                                   |
| South Africa   | South African Medical Research Council, OWID, JHU              | We used SAMRC's modelled baseline for `expected deaths`.                          |
| South Korea    | HMD, OWID, JHU                                                 |                                                                                   |
| Spain          | Instituto de Salud Carlos III, Datadista                       | We used ISC's modelled baseline for `expected deaths`.                            |
| Sweden         | HMD, OWID, JHU                                                 |                                                                                   |
| Switzerland    | HMD, OWID, JHU                                                 |                                                                                   |
| Taiwan         | HMD, OWID, JHU                                                 |                                                                                   |
| Turkey         | İstanbul Büyükşehir Belediyesi, OWID, JHU                      | We analysed burials in Istanbul and assigned it half of Turkey's `covid deaths`.  |
| United States  | CDC / USA Facts / NYC Health                                   | For NYC we used `covid_deaths` from the Department of Health.                     |

## Licence

This software is published by [*The Economist*](https://www.economist.com) under the [MIT licence](https://opensource.org/licenses/MIT). The data generated by *The Economist* are available under the [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/).

The data and files that we have generated from official sources are freely available for public use, as long as *The Economist* is cited as a source.

## Authors

This data has been collected, cleaned and analysed by [James Tozer](https://twitter.com/J_CD_T) and [Martín González](https://twitter.com/martgnz). We are grateful to Lasse Skafte Vestergaard for providing data from [EuroMOMO](https://www.euromomo.eu/graphs-and-maps); to Oğuz Işık for providing data from Istanbul; to René van der Veer for providing code for the Netherlands; to Laurianne Despeghel and Mario Romero Zavalato for providing data from Mexico City; to Karen Gil for providing data from Bolivia; to Thais Carrança, Helio Gurovitz and Diogo Melo for providing data from Brazilian cities; and to Renato Teixeira and Vital Strategies for providing data from the whole of Brazil. 

The [Financial Times](https://github.com/Financial-Times/coronavirus-excess-mortality-data) and the [New York Times](https://github.com/nytimes/covid-19-data/tree/master/excess-deaths) have both published similar analyses of excess mortality.

If you use the data, or have any suggestions, please email [jamestozer@economist.com](mailto:jamestozer@economist.com).
