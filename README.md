# The Boreal Avian Modelling Project's WildTrax Inegration Instructions for Processing Avian Count Data
**** The documentation is in progress. Please feel free to add and edit text and sections. 

# Citation
When referencing any information from this repository or using data that was processed using this protocol, please cite this document.

Citation: Mélina Houle, Maggie MacPherson, Ana Raymundo, Teegan Docherty. *The Boreal Avian Modelling Project's WildTrax Inegration Instructions for Processing Avian Count Data*. Version Number Scheme.

IMPORTANT NOTE: If any authors are asked to give additional assistance, they should be invited to be listed co-authors on the product.

# Version Number Scheme
The current version of these instructions are version 1.0 (Updated 08/02/2022).
Minor improvements will increase the version number by a fraction of 0.1, while major updates trigger a whole number increase in version number.

# Directory Structure

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

# Introduction
The motivation for creating this repository was to share scripts, lookup tables and templates that are needed for translating point count data into the WildTrax upload format. This document serves as a step-by-step guide for how to bring data into BAM's network of datasets that are uploaded to WildTrax. In other words, a cradle-to-WildTrax story of avian count data.

The avian count data intake process involves six main steps (numbered 0-5). Here, we introduce each step with text. When naming of projects, datasets, or files must occur, you will see *file naming protocol:* within each step. Please follow these file naming protocols precisely so that BAM can easily track and find all information easily and as needed. 

0. Identifying avian count data.

1. Communicating with the data contributor.

2. Acquiring a data sharing agreement.

3. Acquiring source data and related documentation that facilitates revormatting (step 4, below).

4. Reformatting the data for WildTrax.

5. Uploading the data to WildTrax.


# 0. Identifying avian count data.
The first main objective of the Boreal Avian Modelling Project (hereafter, BAM), is to assemble, harmonize, and archive standardized bird survey data. See more [here](https://borealbirds.ualberta.ca/about-us/vision-mission-values/). 

**What kind of data does BAM assemble, harmonize, and archive?**

The BAM project gratefully accepts avian point-count data from studies conducted within the boreal and hemiboreal regions of North America. BAM [harmonizes](https://borealbirds.ualberta.ca/data-harmonization/) avian count data to produce models of [bird densities & population estimates](https://borealbirds.ualberta.ca/bird-densities/), that are applied in a variety of [collaborative projects](https://borealbirds.ualberta.ca/projects/).

The BAM database was created by collating and harmonizing avian data from the Breeding Bird Survey, Breeding Bird Atlases, and individual research, monitoring, and inventory efforts conducted across the Canadian and US boreal and hemi-boreal region. Over the last 20 years, BAM team members worked with partners and collaborators to discover and solicit point count data. BAM also reaches out to potential data contributors when they become aware of projects (e.g., via scientific and grey literature or data portals) that may have count data from boreal and hemiboreal regions of North America.

If you have count data that you might want to contribute to BAM's projects, please [contact](mailto:bamp@ualberta.ca?subject=[GitHub]%20Count%20Data%20Contribution) BAM to discuss possible data contributions. To discuss contributing data from autonomous recording units (ARUs), please [contact](http://bioacoustic.abmi.ca/job-opportunities/contact/) the Bioacoustic Unit.

# 1. Communicating with the data contributor

The majority of our communication  with data partners is by email. Although, many collaborations begin with in-person conversations at meetings. 

When a discussion with a potential data contributor takes place, there should be a record of all commmunication with a data partner saved in BAM.SharedDrive/DataStuff/Avian.Data/WildTraxIntegration/Communications. All communications are stored in the same Google folder. This is so that any BAM staff and scientists can follow up with potential contributors so that data can be processed as efficiently as possible. 

For any spoken conversations, please keep a GoogleDoc record of the details, with the most recent conversation at the top of the document. 



*File naming protocol* for documenting communication:

The *file naming protocol* for these communications is to label them with the name of the organization, the word 'communication', and the date in MMDDYYYY format (e.g., United_States_Geological_Survey_communication_08012022). For GoogleDocs with multiple communications, use the MMDDYYYY format for the first date and then add a hyphen with the most recent communication date also in MMDDYYYY format (e.g., United_States_Geological_Survey_communication_08012022-08022022). Do not use short-forms because this creates room for error when any team members aren't familiar with a certain short form. When the data is coming from an individual, then the individual's full name should be used instead in the First_Last order (e.g., Elly Knight should be 'Elly_Knight'). Please take the time to upload every conversation on the day that it occrred, or as soon therafter as possible so that no information is lost.

When reaching out to potentail data contributors, the following email templates can be used.



GENERAL COLD CALL EMAIL

Below is an example template that can be used when reaching out to a potential data contributor in the 'cold call' fashion. This is done, for example, when we're requesting data that we found out about from a third party, like by reading a journal article.

From : [your institutional email address]
Cc: bamp@ualberta.ca; houle.melina@gmail.com; tdochert@ualberta.ca; 

Subject: Adding your avian count data to the Boreal Avian Modelling Project's platform on WildTrax

Dear [data partner’s name], 

I am contacting you to inquire about sharing your avian count data with the Boreal Avian Modelling (BAM) Project. We are an international scientific collaboration that develops and disseminates reliable, data-driven and model-based science and products to support migratory bird management and conservation across the boreal region of North America (read more [here](https://borealbirds.ca/about-us/)). My name is [your name], and I am a representative of BAM. I discovered your data [briefly, in one sentence, describe how data was discovered]. Data like yours has contributed to many important data products and research such as BAM's [large-scale bird density models](https://borealbirds.github.io/). **Would you be willing to share your avian count data with BAM?**

BAM has recently developed a new data platform on [WildTrax](https://www.wildtrax.ca/home) to host avian point count data. WildTrax allows you, the data owner, to manage and access your data in real time. It also gives data owners tools to make the most of their data and integrate it with other data sets and data types (e.g., ARU data). WildTrax supports advanced data sharing options and you can make your data as private or public as you like. We can help you upload your data yourself or we can upload it for you, whatever is your preference.

While BAM encourages open access, we understand the constraints of data sharing and realize that this is not always possible. BAM is part of a new initiative called [*CanAvian: the Canadian Network for Open Avian Data*](http://canavian.ca/), which is a collaboration among BAM, Environment and Climate Change Canada, Birds Canada, the Alberta Biodiversity Monitoring Institute (ABMI), and the Bioacoustic Unit. We are working to network Avian Data Portals across North America to support data access and sharing. Learn more about BAM’s open data initiative [here](TBD). 

What is the next step? If you are willing to share your avian count data with BAM, we will make sure that it is stored and shared appropriately using a data sharing agreement. As we strive to efficiently encorporate count data, we would appreciate your response by [give the date 1 week from when email is sent] and we will send you our data sharing agreement to review before advancing with adding your data to WildTrax. 

Sincerely, and on behalf of BAM,

[your name]


COLD CALL EMAIL FOR A PARTICULAR PROJECT

Below is an example template that can be used when reaching out to a potential data contributor for a specific project in the 'cold call' fashion. 

From : [your institutional email address]
Cc: bamp@ualberta.ca; houle.melina@gmail.com; tdochert@ualberta.ca; 

Subject: Inquiry about Bird Point Count Data

Dear [data partner’s name], 

I am writing to you from the Boreal Avian Modelling (BAM) Project. BAM is a collaborative research project that has spent years assembling a large avian point count database to support the development of avian population estimates, research and conservation in North America (read more [here](https://borealbirds.ca/about-us/)). However, we still have some important data gaps in our harmonized database. **We are currently looking for data from [the region their data is from] to support projects related to the [name of specific project focus; e.g., Eastern Habitat Joint Venture].** 

BAM has a data mission to advance avian conservation and knowledge by enhancing data sharing, centralization and collaboration. We have recently moved our database to the [WildTrax](https://www.wildtrax.ca/home) data platform where it is now hosted on the “[point count sensor](https://www.wildtrax.ca/home/projects/data-discover.html)”. This platform provides enhanced data access, security, and management. WildTrax also allows for the processing and hosting of acoustic data (e.g., ARU data). This allows for the integration of ARU and point count data for analyses. BAM is working with data owners like yourself to get their data onto WildTrax. You can share your data as *publicly* or as *privately* as you wish. Private options range from fully private (i.e., only you can access it), to sharing with select individuals, to hiding records of species at risk. BAM can manage your data for you on WildTrax or you can manage it yourself, which gives you the ability to update your sharing preferences in real time. Let us know if you would like to know more about WildTrax or visit [WildTrax](https://www.wildtrax.ca/home). 

**If you have data that you would be interested in sharing with BAM and/or placing on WildTrax then please let us know.** We have a team who can standardize the data so that it is in the right format to upload to WildTrax. We can accept data in most formats and from any year; however, it must be point count data and must have spatial coordinates. 

Sincerely, and on behalf of BAM,

[your name]



DATA SHARING AGREEMENT EMAIL

Below is an example template that can be used when responding to the potential data contributor with the data sharing agreement. This is done, when the data partner requests to see the agreement or has elected to share their data. 

From : [your institutional email address]
Cc: bamp@ualberta.ca; houle.melina@gmail.com; tdochert@ualberta.ca; 

Subject: BAM's data sharing agreement

Dear [data partner’s name], 

Thank you for your interest in sharing your avian count data with BAM! Please find attached the data sharing agreement [attach DataSharingAgreementTemplate.docx found in BAM.SharedDrive/DataStuff/Avian.Data/WildTraxIntegration/Communications]. 

Once the signed data sharing agreement is returned, we can move forward with helping you to add your avian count data to WildTrax. :)

Sincerely, and on behalf of BAM,

[your name]




# 2. Acquiring a data sharing agreement. 

*****A template of Sharing agreement should be found in Template. 

A data sharing agreement must be signed prior to working with a data partner to upload their data to WildTrax. 

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


# 3. Source data and related documentation that facilitates reformatting (step 4, below).

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

# 4. Reformatting the data for WildTrax.
Reformating will be unique per project. Some projects will use rules that are similar. All scripts are available to allow reusing of code. 

For data to be uploaded to WildTrax, three hierarchical files are needed.

Reformatting data for WildTrax can be challenging when data files are missing required fields, the fields are not filled in properly or are incomplete (according to WildTrax requirements), or the data was collected using a different documentation scheme. If data is being reformatted by BAM, team members communicate with the original data partner to acquire project metadata (e.g., documentation that can clarify how data was collected). 

NOTE: WildTrax does not yet provide space for the storage of project metadata. Here we use the term **metadata** could include important information found in literature published using data from a project, explicit conditions for permission of use (e.g., what the data can be used for or not used for), or instructions on how to cite the data.

Below, we describe what WildTrax is expecting in each of the three required files, the constraints that can cause upload errors and how to resolve them. 

## 1. LOCATION TABLE
The location table is the highest level in the hierarchy at the organization level. The location file comes first because it allows the organization to use the location for multiple projects without duplication. Each line in the location file will be the unique, and precise location for each point count station in TEXT format.

The **LOCATION** attributes identify the geographic extent of the site. 

The **location** field:
| Field   | Format   | Description   | Requred     |
| :------- | :-------------- | :-------------- | :------------------|
| location     | Text | The physical place on the landscape where the data was collected. Created using the concatenation of  [datasetCode]:[site]:[station], unless otherwise specified | YES |

Common **location** field errors:
* A location might not be accepted because it includes characters that are not allowed (e.g., *"*", or "%").



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







## 2. VISIT TABLE
This is the second level in the hierarchy at the project level. Visits occur at the date scale (YYYY-MM-DD). The location file has to come before the Visit file so that the visit can occur at the location. You cannot load to a location that has not previously been loaded to WildTrax. Each line in the visit file will have the location, written exactly as it appears in the location file, and the date in YYYY-MM-DD format.

The **VISIT** attributes identify the date the survey was performed.
The **location** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| location     | Text | The physical place on the landscape where the data was collected. Created using the concatenation of  [datasetCode]:[site]:[station], unless otherwise specified | YES |

Common **location** field errors:




The **visitDate** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| visitDate     | Text | The date of the survey (YYYY-MM-DD) | YES |

Common **visitDate** field errors:




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







## 3. SURVEY TABLE
This is the third file that includes the point count data. 

The **SURVEY** attributes identify protocols, species, abundance, and time of the observations.

The **location** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| location     | Text | The physical place on the landscape where the data was collected. Created using the concatenation of  [datasetCode]:[site]:[station], unless otherwise specified | YES |

Common **location** field errors:
* The location is not written exactly as it appears in the location file.



The **surveyDateTime** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| surveyDateTime     | Text | YYYY-MM-DD HH:MM:SS, Concatenation of  visitDate  survey_time; separated by space | YES |

Common **surveyDateTime** field errors:
* when time is missing, fill time with 00:00:01.



The **durationMethod** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| durationMethod     | Text | The duration method used the count-remove species from the survey. Refer to duration_method_codes table  | YES |

Common **durationMethod** field errors:
* NOTE: You can request to add a new duration method if the one that was used for the project is not already in WildTrax.



The **distanceMethod** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| distanceMethod     | Text | The distance band separation method used. Refer to distance_method_codes table   | YES |

Common **distanceMethod** field errors:
* NOTE: You can request to add a new distance method if the one that was used for the project is not already in WildTrax.



The **observer** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| observer     | Text | The observer code who conducted the survey. When observer name are provided in the source data, we create a lookup table where observer name get a serial number assigned using this format:  [Dataset Code]_[serial number] | YES |

Common **observer** field errors:
* Can't be NULL. Must me of type TEXT. Default value is NA if information is not provided in the source data.
* To anonymize the identities of individuals, BAM writes the name of the project and an integer for observer (e.g., [PCODE]-[Integer]).


The **species** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| species     | Text | AOU code used by WildTrax. See species codes table  | YES |

Common **species** field errors:
* NOTE: each line if for one species, not for each individual counted during a point count.



The **distanceband** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| distanceband     | Text | The distance band the species was detected in. Refer to distance_band_codes table   | YES |

Common **distanceband** field errors:




The **durationinterval** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| durationinterval     | Text | The duration interval the species was detected in. Refer to duration_interval_codes table  | YES |

Common **durationinterval** field errors:




The **abundance** field:
| Field   | Format   | Description   | Required |
| :------- | :-------------- | :-------------- | :---------------- |
| abundance     | Numeric | Number of individual of a species with the same date, time, observer, isHeard, isSeen, distanceband and durationinterval information | YES |

Common **abundance** field errors:




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





All surveys include the:
* location, written exactly as it appears in the location file, 
* date and time in YYYY-MM-DD hh:mm:ss format, 
* observer, written as the project code and an integer [PCODE]-[Integer] to anonymize the identities of individuals, 
* distance method, see (website) for list of accepted distance methods, NOTE: You can request to add a new distance method if the one that was used for the project is not already in WildTrax,
* duration method, same as above for distance method,
* species, the 4 letter code specified for each species in WildTrax, NOTE: each line is for one species,
* is heard, DEFINE
* is seen, DEFINE
* abundance, an integer
* distance band, DEFINE, and
* duration interval, DEFINE.

Templates for each file can be found under [template](https://github.com/MelinaHoule/WT-Integration/tree/main/template).
Examples for each file can be found under [examples].

## Solutions to Common Missing Data Problems


Table 1. Solutions to Common Missing Data Problems

| Location of Problem   | Field Affected   | Description   | Solution     |
| :------- | :-------------- | :-------------- | :----------------  |
| **LOCATION** file     | location | The location is not accepted because it includes characters that are not allowed (e.g., *"*"* or "%") | Reformat location names to not include banned characters |
| **LOCATION** file    | latitude or longitude | will not load if null | Do not load any locations with missing coordinates |
| **VISIT** file     | location | location does not match any previously loaded locations | Check that the location file was loaded first. If it was, check that the spelling of the location is correct in the visit file |
| **VISIT** file    | visitDate | There is no day or month listed.  | Change these to January 1st (e.g., YYYY-01-01). |
| **SURVEY** file     | surveyDateTime | There is no time listed. | Change these to 12:01 a.m. (e.g., 00:00:01). |
| **SURVEY** file     | abundance | Is not an integer (e.g., "too many to count").  | Change these to 999. |
| **SURVEY** file     | distanceMethod or durationMethod | Does not match a methodology in WildTrax. | Request to add if does not exist. WildTrax will have to update their server before the change is reflected and then you can upload. |
| **SURVEY** file     | distancebnad or distanceinterval | Does not match distanceMethod or durationMethod. | These must be resolved in communication with the data partner. |
| **SURVEY**   | abuncance | There is a single line for every individual bird seen/heard. | You can't have duplicates of species at the same dateTime and location. These must be summed. |



# 5. Uploading the data to WildTrax.
