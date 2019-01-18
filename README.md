# E-HEALTH

## Table of contents
1. [Scope of the project](#Scope-of-the-project)
1. [Brief introduction to methods](#Brief-introduction-to-methods)
2. [Project outline](#Project-outline)
 1. [Identification and data gathering](#Identification-and-data-gathering)
 2. [Characterization and classification of health apps](#Characterization-and-classification-of-health-apps)

## Scope of the project
Given the huge number of health-related Apps available online and given their increasing relevance for
healthcare purposes, there is the necessity to understand which and how many Apps are nowadays
useful for the user, and to understand how they can improve the quality of life of the patients.
## Brief introduction to methods
The project was carried out entirely using <b>["R"](https://www.r-project.org/)</b>, chosen because of its [numerous and versatile packages](https://blog.revolutionanalytics.com/2017/01/cran-10000.html).

For this project, a combination of web-scraping, text mining and machine learning techniques have been used.

## Project outline
The project outline is divided into two parts:
### 1. Identification and data gathering
The goal was to identify all the Apps related
to the “Medical” and “Health and Fitness” categories and to extract information from the Apps
webpages in an automated way, collecting them into a database.

For this part we used the **R web-scraping oriented package ["Rvest"](https://blog.rstudio.com/2014/11/24/rvest-easy-web-scraping-with-r/)** that makes it easy to scrape (or harvest) data from html web pages.

### 2. Characterization and classification of health apps
The second part's aim was to characterize all the Apps retrieved, classifying them into the specific medical
categories they belong to, and then, to analyze the Apps features by mean of the newest and most
relevant methods available from the recent literature.

For this part we used a combination of **text mining and machine learning**, developing a **[naive bayes classifier](https://towardsdatascience.com/introduction-to-naive-bayes-classification-4cffabb1ae54)** that was used to separate medical from not-medical apps. This separation allowed us to have a lighter database of apps that was successively analyzed using **[MetaMap](https://metamap.nlm.nih.gov/)**, a highly configurable program to map biomedical text to the UMLS Metathesaurus or, equivalently, to discover Metathesaurus concepts referred to in text.

## Full pdf report
You can read more about this project in the full pdf report available [here](R script/report/report_ehealth.pdf)
