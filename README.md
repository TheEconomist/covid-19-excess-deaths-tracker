# *The Economist's* tracker for covid-19 excess deaths 

[![update](https://github.com/TheEconomist/covid-19-excess-deaths-tracker/actions/workflows/update.yaml/badge.svg)](https://github.com/TheEconomist/covid-19-excess-deaths-tracker/actions/workflows/update.yaml)

This repository contains the data behind *The Economist’s* [tracker for covid-19 excess deaths](https://www.economist.com/graphic-detail/coronavirus-excess-deaths-tracker) and the code that we have used to clean, analyse and present the numbers.

## Scripts, sources and output data

Our tracker uses two R scripts to calculate excess deaths in each country:

* [`cleaning_script.R`](scripts/cleaning_script.R): this imports raw data about all-cause mortality and official covid-19 death tolls. We take most of our all-cause mortality figures from the [World Mortality Dataset](https://github.com/akarlinsky/world_mortality)(WMD), which is maintained by Ariel Karlinsky and Dmitry Kobak. The WMD draws on various national registry offices and statistical bureaus, as well as the [Human Mortality Database](https://www.mortality.org/), a similar project. We take official covid-19 death tolls for each country from [Our World In Data](https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv), which in turn sources them from Johns Hopkins University. Finally, we take data for US states from the [CDC](https://www.cdc.gov/nchs/nvss/vsrr/covid19/excess_deaths.htm) and [USA Facts](https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/). From these sources, we export a weekly or monthly historical time series of `total_deaths` and official `covid_deaths` going back to 2015 (or the earliest available year). If the source that we use for `total deaths` has also modelled and provided a baseline for `expected_deaths`, we include that too. The files are exported to [`/output-data/historical-deaths/`](output-data/historical-deaths).

* [`excess_deaths_script.R`](scripts/excess_deaths_script.R): this imports the time series files from [`/output-data/historical-deaths/`](output-data/historical-deaths), and calculates the weekly or monthly `expected_deaths` for each country (unless a modelled baseline has already been provided). Our own modelled baselines fit a linear trend for year, to account for long-term increases or decreases in mortality, and a fixed effect for each week or month up to February 2020. We have exported these models to [`/output-data/expected-deaths-models/`](output-data/expected-deaths-models). We then use these baselines to calculate `excess_deaths`, and export the files to [`/output-data/excess-deaths/`](output-data/excess-deaths).

There is a third R script that summarises the data for some graphics in the article:

* [`interactive_script.R`](scripts/interactive_script.R): this imports the files for `excess deaths` for a list of countries and creates the data for the small multiple chart and the table featured in the article. The files are exported to [`/output-data/interactive/`](output-data/interactive).

## Historical versions and sources

This tracker has gone through several iterations since it was launched in April 2020, at which point it was the first database to compare excess mortality with official covid death tolls. As of July 2021 we are now largely relying on all-cause mortality data collected by Ariel Karlinsky and Dmitry Kobak for the WMD. But throughout the development of the tracker we have drawn on many other sources, including:

Registro Civil (Bolivia); Vital Strategies; Office for National Statistics; Northern Ireland Statistics and Research Agency; National Records of Scotland; Registro Civil (Chile); Registro Civil (Ecuador); Institut National de la Statistique et des Études Économiques; Santé Publique France; Provinsi DKI Jakarta; Istituto Nazionale di Statistica; Dipartimento della Protezione Civile; Secretaría de Salud (Mexico); Ministerio de Salud (Peru); Data Science Research Peru; Departamento Administrativo Nacional de Estadística (Colombia); South African Medical Research Council; Instituto de Salud Carlos III; Ministerio de Sanidad (Spain); Datadista; Istanbul Buyuksehir Belediyesi; New York City Health. 

## Authors

This data has been collected, cleaned and analysed by [James Tozer](https://twitter.com/J_CD_T) and [Martín González](https://twitter.com/martgnz). We are grateful to Lasse Skafte Vestergaard for providing data from [EuroMOMO](https://www.euromomo.eu/graphs-and-maps); to Oğuz Işık for providing data from Istanbul; to René van der Veer for providing code for the Netherlands; to Laurianne Despeghel and Mario Romero Zavalato for providing data from Mexico City; to Karen Gil for providing data from Bolivia; to Thais Carrança, Helio Gurovitz and Diogo Melo for providing data from Brazilian cities; and to Renato Teixeira and Vital Strategies for providing data from the whole of Brazil. 

The [Financial Times](https://github.com/Financial-Times/coronavirus-excess-mortality-data) and the [New York Times](https://github.com/nytimes/covid-19-data/tree/master/excess-deaths) have both published similar analyses of excess mortality.

If you use the data, or have any suggestions, please email [jamestozer@economist.com](mailto:jamestozer@economist.com).

## Licence

This software is published by [*The Economist*](https://www.economist.com) under the [MIT licence](https://opensource.org/licenses/MIT). The data generated by *The Economist* are available under the [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/).

The data and files that we have generated from official sources are freely available for public use, as long as *The Economist* is cited as a source.
