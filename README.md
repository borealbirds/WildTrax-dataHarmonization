# The Boreal Avian Modelling Project's WildTrax Integration Instructions for Processing Avian Count Data
**** The documentation is in progress. Please feel free to add and edit text and sections. 

## Citation
When referencing any information from this repository or using data that was processed using this protocol, please cite this document.

Citation: Mélina Houle, Maggie MacPherson, Ana Raymundo, Teegan Docherty. *The Boreal Avian Modelling Project's WildTrax Inegration Instructions for Processing Avian Count Data*. Version Number Scheme.

IMPORTANT NOTE: If any authors are asked to give additional assistance, they should be invited to be listed co-authors on the product.

## Version Number Scheme
The current version of these instructions are version 1.0 (Updated 08/02/2022).
Minor improvements will increase the version number by a fraction of 0.1, while major updates trigger a whole number increase in version number.

## Directory Structure

The Directory folder of the working directory is presented below. Sub-folder "project" and "out" aren't shared because they are often too large.  

<pre>
./                                      Sample files for configuring and running scripts

./archived                              Old versions of scripts 

./lookupTables                          Project specific lookup tables created to allow translation from source data to WildTrax format

./out                                   Location of the resulting output (location, visit, survey, extended)  (Not shared)

./project                               Master folder where all source data are found within thei respective subfolder (Not shared)

./script                                R scripts to summarize WT output for validation checks

./template                              Example of output table structure, communication, ...
</pre>

## Table of Contents

<a href="#Introduction">Introduction</a>

<a href="#Identification">Identifying avian count data</a>

<a href="#Communication">Communicating with the data contributor</a>

<a href="#Acquisition">Acquiring a data sharing agreement</a>

<a href="#Dataaquisition">Acquiring source data and related documentation that facilitates reformatting (step 4, below)</a>

<a href="#Reformatting">Reformatting the data for WildTrax</a>

<a href="#Uploading">Uploading the data to WildTrax</a>


<a name=Introduction></a>
## Introduction
The motivation for creating this repository was to share scripts, lookup tables and templates that are needed for translating point count data into the WildTrax upload format. This document serves as a step-by-step guide for how to bring data into BAM's network of datasets that are uploaded to WildTrax. In other words, a cradle-to-WildTrax story of avian count data.

The avian count data intake process involves six main steps (numbered 0-5). Here, we introduce each step with text. When naming of projects, datasets, or files must occur, you will see *file naming protocol:* within each step. Please follow these file naming protocols precisely so that BAM can easily track and find all information easily and as needed. 


<a name=Identification></a>
## Identifying avian count data.
The first main objective of the Boreal Avian Modelling Project (hereafter, BAM), is to assemble, harmonize, and archive standardized bird survey data. See more [here](https://borealbirds.ualberta.ca/about-us/vision-mission-values/). 

**What kind of data does BAM assemble, harmonize, and archive?**

The BAM project gratefully accepts avian point-count data from studies conducted within the boreal and hemiboreal regions of North America. BAM [harmonizes](https://borealbirds.ualberta.ca/data-harmonization/) avian count data to produce models of [bird densities & population estimates](https://borealbirds.ualberta.ca/bird-densities/), that are applied in a variety of [collaborative projects](https://borealbirds.ualberta.ca/projects/).

The BAM database was created by collating and harmonizing avian data from the Breeding Bird Survey, Breeding Bird Atlases, and individual research, monitoring, and inventory efforts conducted across the Canadian and US boreal and hemi-boreal region. Over the last 20 years, BAM team members worked with partners and collaborators to discover and solicit point count data. BAM also reaches out to potential data contributors when they become aware of projects (e.g., via scientific and grey literature or data portals) that may have count data from boreal and hemiboreal regions of North America.

If you have count data that you might want to contribute to BAM's projects, please [contact](mailto:bamp@ualberta.ca?subject=[GitHub]%20Count%20Data%20Contribution) BAM to discuss possible data contributions. To discuss contributing data from autonomous recording units (ARUs), please [contact](http://bioacoustic.abmi.ca/job-opportunities/contact/) the Bioacoustic Unit.

<a name=Communication></a>
## Communication with the data contributor

The majority of our communication  with data partners is by email. Although, many collaborations begin with in-person conversations at meetings. 

When a discussion with a potential data contributor takes place, there should be a record of all commmunication with a data partner saved in [BAM.SharedDrive](https://drive.google.com/drive/folders/1IC92Vg1Nrrw6astOXPZPoDUeRfHOy4IX). All communications are stored in the same Google folder. This is so that any BAM staff and scientists can follow up with potential contributors so that data can be processed as efficiently as possible. 

For any spoken conversations, please keep a GoogleDoc record of the details, with the most recent conversation at the top of the document. 

All communication saved on the BAM.SharedDrive should follow the *File naming protocol*:

- Label the communication with the name of the organization, the word 'communication', and the date in MMDDYYYY format (e.g., United_States_Geological_Survey_communication_08012022).
- For GoogleDocs with multiple communications, use the MMDDYYYY format for the first date and then add a hyphen with the most recent communication date also in MMDDYYYY format (e.g., United_States_Geological_Survey_communication_08012022-08022022).
- Do not use short-forms because this creates room for error when any team members aren't familiar with a certain short form.
- When the data is coming from an individual, the individual's full name should be used instead in the First_Last order (e.g., Elly Knight should be 'Elly_Knight').
- Upload every conversation on the day that it occurred, or as soon therafter as possible so that no information is lost.

When reaching out to potentail data contributors, the following email templates can be used:

- [GENERAL COLD CALL EMAIL](https://docs.google.com/document/d/1jmX8El6lT4MDDdXJQ__hjmn1jP5SSgcvhb1hf4VDWng)
- [COLD CALL EMAIL FOR A PARTICULAR PROJECT](https://docs.google.com/document/d/1FE1UM77VAwtK3o7NJrjvebT2YVWt9Cby42RdFA_Cx54)



<a name=Acquisition></a>
## Acquiring a data sharing agreement. 

A data sharing agreement must be signed prior to working with a data partner to upload their data to WildTrax. You can contact the data partner to share the data sharing agreement document using [this email template](https://docs.google.com/document/d/1wYuIfD_QzuIyFa36PDBem7sq_Y6t1v7PyQf1GmCLWnw) and attach the data sharing agreement the data partner need to fill and sign using this [template](https://docs.google.com/document/d/1wxYQldMbqgGW8wqHueOrnHFQ361n3TI9/edit#heading=h.gjdgxs)

Once we have a signed data sharing agreement with a data partner, this should be saved as a PDF at BAM.SharedDrive/DataStuff/Avian.Data/Contributions.Sharing using the following *file naming protocol*.


*File naming protocol* for data sharing agreements:

Data sharing agreements should be saved using the full name of the data partner, the words 'DataSharingAgreement', and the date it was signed or received in MMDDYYYY format (e.g., 'United_States_Geological_Survey_DataSharingAgreement_08042022'). In addition, a copy should be emailed to bamp@ualberta.ca as a backup with the name of the file in the subject line.


In WildTrax, sharing can be either:

1. **Published - Private**  - The project data will only be available to project members. Users will need to request access to the project in order to view any details such as species or locations.
2. **Published - Map Only** - The project data will be accessible through Data Discover but the media and report are not accessible to users who are not project members. If you're not a project or organization member, the location buffering and visibility settings will apply.
3. **Published - Map + Report Only** - (not for point counts) The project data become available to all WildTrax users through Data Downloads and Data Discover, however, the media is not accessible. Use this setting if you want to make your data publicly available but there are privacy concerns with your media. If you're not a project or organization member, the location buffering and visibility settings will apply.
4. **Published - Public** - The project data become available to any WildTrax user as well as the details of the project in Data Downloads and Data Discover. If you're not a project or organization member, the location buffering and visibility settings will apply.

(Note that you can also use **Active** status for projects that are currently being processed, designed or are in the preliminary stages of data uploading. Use this status if the project is actively being worked on. This is the default project status when it is first created.)

Any detailed communication about the data sharing agreement should clearly reflect these options.

<a name=Dataacquisition></a>
## Source data and related documentation that facilitates reformatting (step 4, below).

Historically for BAM, once a signed data sharing agreement has been acquired the data acquisition process can take place.

All data acquired should be logged in the [template](https://github.com/MelinaHoule/WT-Integration/blob/345282009ddcbd465f07789eca1cc0b8ba78e13a/project_Integration.xlsx) as soon as it is received. 

* Make sure the Organization exists. If not, create an entry.
* Create a new project entry. Marked it as NS (Not Started). 
* Create a new PartnerContactInfo entry for the project

If you don't intend to process the data straight away, make sure you overview the data to document some attributes, have all tables that define attributes and values used. Some information will be answered by emails. Make sure you keep a copy by converting the email as a PDF and make it available in the respective project folder under "communication". 

Overviewing the data includes verifying that:
* Observations have XY coordinates.
* Reference system of the coordinates is known.
* Protocol is documented (can be a report, email exchange, or found in the db itself).
* Species code definition is present. We cannot assume the species code used is the same as the one used by WildTrax. We need to check.
* Date and time is present.
* Abundance is present.

Once verification has taken place, the WT-status of the data can be changed from 'NS' (for 'not started') to IP, for 'in progress' in the [template](https://github.com/MelinaHoule/WT-Integration/blob/345282009ddcbd465f07789eca1cc0b8ba78e13a/project_Integration.xlsx).

<a name=Reformatting></a>
## Reformatting the data for WildTrax.
Reformating will be unique per project. Some projects will use rules that are similar. All scripts are available to allow reusing of code. 

For data to be uploaded to WildTrax, three hierarchical files are needed.

Reformatting data for WildTrax can be challenging when data files are missing required fields, the fields are not filled in properly or are incomplete (according to WildTrax requirements), or the data was collected using a different documentation scheme. If data is being reformatted by BAM, team members communicate with the original data partner to acquire project metadata (e.g., documentation that can clarify how data was collected). 

NOTE: WildTrax does not yet provide space for the storage of project metadata. Here we use the term **metadata** could include important information found in literature published using data from a project, explicit conditions for permission of use (e.g., what the data can be used for or not used for), or instructions on how to cite the data.

Below, we describe what WildTrax is expecting in each of the three required files, the constraints that can cause upload errors and how to resolve them. 

### 1. LOCATION TABLE
The location table is the highest level in the hierarchy at the organization level. The location file comes first because it allows the organization to use the location for multiple projects without duplication. Each line in the location file will be the unique, and precise location for each point count station in TEXT format.

The **LOCATION** attributes identify the geographic extent of the site. 

The **location** field:
| Field   | Format   | Description   | Required     |
| :------- | :-------------- | :-------------- | :------------------|
| location     | Text | Name of the physical place on the landscape where the data was collected. If not present in source data, create it using the concatenation of  [datasetCode]:[site]:[station], unless otherwise specified | YES |

Common **location** field errors:
* A location might not be accepted because it includes characters that are not allowed (e.g., "*****", or "%").



The **latitude** and **longitude** fields:
| Field   | Format   | Description   | Required     |
| :------- | :-------------- | :-------------- | :---------------- |
| latitude     | Decimal degrees | NAD83, convert if otherwise | YES |
| longitude     | Decimal degrees | NAD83, convert if otherwise | YES |

Common **coordinate** fields errors:
* This will not load if the fields are empty or NULL. Do not load any locations with missing coordinates.



Location table unrequired fields:
| Field   | Format   | Description   | Required     |
| :------- | :-------------- | :-------------- | :---------------- |
| elevationMeters     | Numeric | Elevation in meters. NULL if not collected. The upload will fill it  | NO |
| bufferRadiusMeters     | Numeric | Radius of the buffer around the location (in meters). Use only if points need to be masked. NULL otherwise | NO |
| isHidden     | Logical | t if points need to be masked  | NO |
| trueCoordinates     | Logical | t if coordinates are not buffered | NO |
| comments     | Text | Any comments related to locations. As needed | NO |
| internal_wildtrax_id     | Numeric | Generated during the upload. Leave it blank | NO |
| internal_update_ts     | Text | Generated during the upload. Leave it blank | NO |

Common Location Table Errors for Unrequired Fields"







### 2. VISIT TABLE
This is the second level in the hierarchy at the project level. Visits occur at the date scale (YYYY-MM-DD). The location file has to come before the Visit file so that the visit can occur at the location. You cannot load to a location that has not previously been loaded to WildTrax. Each line in the visit file will have the location, written exactly as it appears in the location file, and the date in YYYY-MM-DD format.

The **VISIT** attributes identify the date the survey was performed.
The **location** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| location     | Text | The physical place on the landscape where the data was collected. Created using the concatenation of  [datasetCode]:[site]:[station], unless otherwise specified | YES |

Common **location** field errors:
* The location does not match any previously loaded locations. In this case, check that the location file was loaded first. If it was, check that the spelling of the location is correct in the visit table.



The **visitDate** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| visitDate     | Text | The date of the survey (YYYY-MM-DD) | YES |

Common **visitDate** field errors:
* When there is no year, day or month listed, change these to January 1st 1900 (e.g., 1900-01-01).
* When date doesn't exist (2014-06-31), change these to January 1st 1900 (e.g., 1900-01-01).



Visit table unrequired fields:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| snowDepthMeters     | Numeric | Generated during the upload. Leave it blank | NO |
| waterDepthMeters     | Numeric | Generated during the upload. Leave it blank  | NO |
| crew     | Text | Leave blank. ARUs field | NO |
| bait     | Text | Use "None" for point count data  | NO |
| accessMethod     | Text | Leave blank. ARUs field | NO |
| landFeatures     | Text | Leave blank. ARUs field | NO |
| comments     | Text | Any comments related to visit. As needed | NO |
| wildtrax_internal_update_ts     | Text | Generated during the upload. Leave it blank | NO |
| wildtrax_internal_lv_id     | Text | Generated during the upload. Leave it blank | NO |

Common Visit Table Errors for Unrequired Fields:







### 3. SURVEY TABLE
This is the third file that includes the point count data. No fly-over data should be included in the data upload. 

The **SURVEY** attributes identify protocols, species, abundance, and time of the observations.  

The **location** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| location     | Text | The physical place on the landscape where the data was collected. Created using the concatenation of  [datasetCode]:[site]:[station], unless otherwise specified | YES |

Common **location** field errors:
* The **location** is not written exactly as it appears in the location file.



The **surveyDateTime** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| surveyDateTime     | Text | YYYY-MM-DD HH:MM:SS, Concatenation of  visitDate  and time of survey; separated by space | YES |

Common **surveyDateTime** field errors:
* When there is no time listed, fill time with 00:00:01.



The **durationMethod** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| durationMethod     | Text | The duration method used the count-remove species from the survey. Refer to [duration_method_codes table](https://github.com/borealbirds/WT-Integration/blob/main/lookupTables/duration_method_codes.csv)  | YES |

Common **durationMethod** field errors:
* NOTE: You can request to add a new duration method if the one that was used for the project is not already in WildTrax. Locations within a project can't use different **durationMethod** for the same surveyDateTime. For example, a specie observed on a specific location/surveyDateTime need to have the same **durationMethod** across all species observed at that location/surveyDateTime. 



The **distanceMethod** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| distanceMethod     | Text | The distance band separation method used. Refer to [distance_method_codes table](https://github.com/borealbirds/WT-Integration/blob/main/lookupTables/distance_method_codes.csv)  | YES |

Common **distanceMethod** field errors:
* NOTE: You can request to add a new distance method if the one that was used for the project is not already in WildTrax. Locations within a project can't use different **distanceMethod** for the same surveyDateTime. For example, a specie observed on a specific location/surveyDateTime need to have the same **distanceMethod** across all species observed at that location/surveyDateTime.



The **observer** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| observer     | Text | The observer code who conducted the survey. When observer name are provided in the source data, we create a lookup table where observer name get a serial number assigned using this format:  [Dataset Code]_[serial number] | YES |

Common **observer** field errors:
* Can't be NULL. Must me of type TEXT. Default value is NA if information is not provided in the source data.
* To anonymize the identities of individuals, BAM writes the name of the project and an integer for observer (e.g., [Dataset Code]_[serial number]).


The **species** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| species     | Text | AOU code used by WildTrax. See [species_codes table](https://github.com/borealbirds/WT-Integration/blob/main/lookupTables/species_codes.csv)   | YES |

Common **species** field errors:
* NOTE: Individuals from the same species observed at the same location, same day, same distance band and duration interval MUST HAVE THEIR ABUNDANCE SUMMED in the same line. Duplicate entries based on location, species, surveyDateTime, distanceband and durationinterval are not allowed. 
*  A visit without observation should have an entry created in the survey table with species = NONE, abundance = 0, durationinterval = UNKNOWN, distanceband = UNKNOWN.


The **distanceband** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| distanceband     | Text | The distance band the species was detected in. Refer to [distance_band_codes table](https://github.com/borealbirds/WT-Integration/blob/main/lookupTables/duration_interval_codes.csv)   | YES |

Common **distanceband** field errors:
* NOTE: **distanceband** must follow the protocol given in **distanceMethod**. For example, if **distanceMethod** = 0m-50m-100m-INF can't have **distanceband** of 0m-100m. It needs to be either 0m-50m, 50m-100m, 100m-INF or UNKNOWN otherwise. 



The **durationinterval** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| durationinterval     | Text | The duration interval the species was detected in. Refer to duration_interval_codes table  | YES |

Common **durationinterval** field errors:
* NOTE: **durationinterval** must follow the protocol given in **durationMethod**. For example, if **durationMethod** = 0-3-5-10min can't have **durationinterval** of 0-5min. It needs to be either 0-3min, 0-5min, 5-10min or UNKNOWN otherwise. 



The **abundance** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| abundance     | Numeric | Number of individual of a species with the same date, time, observer, isHeard, isSeen, distanceband and durationinterval information | YES |

Common **abundance** field errors:
* This must be an number. When the cell reads "too many to count" or UNKNOWN, change it to 999.
* You can't have duplicates of species at the same date, time, distanceband, durationinterval and location because there must be a single line for every species. If data is organized as every individual that was seen/heard per line, these must be summed (e.g., the total number of Black-capped Chickadees during that point count).



The **isHeard** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| isHeard     | Text | Was / were the bird(s) detected using a visual method (Yes, No or DNC). If no behaviour data, fill in as DNC except for NONE = null | YES |

Common **isHeard** field errors:




The **isSeen** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| isSeen     | Text | Was / were the bird(s) detected using an auditory method (Yes, No or DNC). If no behaviour data, fill in as DNC except for NONE = null | YES |

Common **isSeen** field errors:




Survey table unrequired fields:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| comments     | Text | Any comments related to survey. As needed | NO |

Common Survey Table Errors for Unrequired Fields:





Templates for each file can be found under [template](https://github.com/MelinaHoule/WT-Integration/tree/main/template).
Examples for each file can be found under [examples](https://github.com/MelinaHoule/WT-Integration/tree/main/examples).

<a name=Uploading></a>
## Uploading the data to WildTrax.
Order of operations: Several ordered steps are required prior to uploading formatted avian count data to WildTrax. Tutorials for these steps can be found on the WildTrax website [here](https://wildtrax.ca/home/resources/tutorials).
1. Create an Account - This is the first step and is necessary prior to any other steps.
2. Create an Organization - You must have a user account to add yourself to Organizations. You must be an administrator of at least one organization in order to create a project (below). If you are adding data from a new Organization to WildTrax, follow the guidance found on the WildTrax website [here](https://www.wildtrax.ca/home/resources/guide/organizations/organization-management.html).
3. Create a Project - Projects can only belong to a single Organization, and you must be an administrator of an Organization to be able to create a Project. With the project in "active" mode, you can set the project to "Private" or "Public", and give access to specific individuals as either 'Administrators' or 'Read Only Access'.

Once your Project is created, you can upload the three required .csv files detailed in Section 4 (above).

As described in Section 4 on reformatting data for WildTrax, WildTrax is expecting 3 files: 1) the Location Table, 2) the Visit Table, and 3) the Survey Table. 

Click on the 'Manage' button within your Project and select 'Upload Surveys' to begin uploading the 3 required .csv files. 

Once the data is uploaded, a 'QA Project Data' button will turn green. You must click this button for WildTrax to check that the data you're uploading fits the WildTrax standards. When you run into errors, see Section 4 (above) for a list of common issues that cause files to not meet the standards that WildTrax is expecting. 

