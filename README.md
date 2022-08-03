# The Boreal Avian Modelling Project's WildTrax Inegration Instructions for Processing Avian Count Data
**** The documentation is in progress. Please feel free to add and edit text and sections. 

# Citation
When referencing any information from this repository or using data that was processed using this protocol, please cite this document.

Citation: Mélina Houle, Maggie MacPherson, Ana Raymundo, Teegan Dougherty. *The Boreal Avian Modelling Project's WildTrax Inegration Instructions for Processing Avian Count Data*. Version Number Scheme.

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

To find original count datasets, BAM... (maybe TD can offer a general statement of how most of the data we use has been collected?). BAM also reaches out to potential data contributors when they become aware of projects (e.g., through reading scientific and grey literature) that may have count data from boreal and hemiboreal regions of North America.

If you have count data that you might want to contribute to BAM's projects, please [contact](mailto:borealavianmodellingproject@ualberta.ca?subject=[GitHub]%20Count%20Data%20Contribution) BAM to discuss possible data contributions. To discuss contributing data from autonomous recording units (ARUs), please [contact](http://bioacoustic.abmi.ca/job-opportunities/contact/) the Bioacoustic Unit.

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

Dear [data partner’s name], 

I am writing to you from the Boreal Avian Modelling (BAM) Project. BAM is a collaborative research project that has spent years assembling a large avian point count database to support the development of avian population estimates, research and conservation in North America (read more [here](https://borealbirds.ca/about-us/)). However, we still have some important data gaps in our harmonized database. **We are currently looking for data from [the region their data is from] to support projects related to the [name of specific project focus; e.g., Eastern Habitat Joint Venture]. 

BAM has a data mission to advance avian conservation and knowledge by enhancing data sharing, centralization and collaboration. We have recently moved our database to the [WildTrax](https://www.wildtrax.ca/home) data platform where it is now hosted on the “[point count sensor](https://www.wildtrax.ca/home/projects/data-discover.html)”. This platform provides enhanced data access, security, and management. WildTrax also allows for the processing and hosting of acoustic data (e.g., ARU data). This allows for the integration of ARU and point count data for analyses. BAM is working with data owners like yourself to get their data onto WildTrax. You can share your data as *publicly* or as *privately* as you wish. Private options range from fully private (i.e., only you can access it), to sharing with select individuals, to hiding records of species at risk. BAM can manage your data for you on WildTrax or you can manage it yourself, which gives you the ability to update your sharing preferences in real time. Let us know if you would like to know more about WildTrax or visit [WildTrax](https://www.wildtrax.ca/home). 

**If you have data that you would be interested in sharing with BAM and/or placing on WildTrax then please let us know.** We have a team who can standardize the data so that it is in the right format to upload to WildTrax. We can accept data in most formats and from any year; however, it must be point count data and must have spatial coordinates. 

Sincerely, and on behalf of BAM,
[your name]


DATA SHARING AGREEMENT EMAIL

Below is an example template that can be used when responding to the potential data contributor with the data sharing agreement. This is done, when the data partner requests to see the agreement or has elected to share their data. 

From : bamp@ualberta.ca
Cc: bayne@ualberta.ca; lankau@ualberta.ca; tdochert@ualberta.ca; 

Subject: Adding your avian count data to the Boreal Avian Modelling Project's platform on WildTrax

Dear [data partner’s name], 

Thank you for your interest in sharing your avian count data with BAM! Please find attached the data sharing agreement [attach ]




# 2. Acquiring a data sharing agreement. 

A template of Sharing agreement should be found in Template. In WildTrax, sharing can be either:

1. **Published - Private**  - The project data will only be available to project members. Users will need to request access to the project in order to view any details such as species or locations.
2. **Published - Map Only** - The project data will be accessible through Data Discover but the media and report are not accessible to users who are not project members. If you're not a project or organization member, the location buffering and visibility settings will apply.
3. **Published - Map + Report Only** - (not for point counts) The project data become available to all WildTrax users through Data Downloads and Data Discover, however, the media is not accessible. Use this setting if you want to make your data publicly available but there are privacy concerns with your media. If you're not a project or organization member, the location buffering and visibility settings will apply.
4. **Published - Public** - The project data become available to any WildTrax user as well as the details of the project in Data Downloads and Data Discover. If you're not a project or organization member, the location buffering and visibility settings will apply.

(Note that you can also use **Active** status for projects that are currently being processed, designed or are in the preliminary stages of data uploading. Use this status if the project is actively being worked on. This is the default project status when it is first created.)

You need to make sure the communication reflects these options.


# 3. Acquiring source data and related documentation that facilitates revormatting (step 4, below).

All data acquired should be logged in the [template](https://github.com/MelinaHoule/WT-Integration/blob/345282009ddcbd465f07789eca1cc0b8ba78e13a/project_Integration.xlsx) as soon as you receive it. 

* Make sure the Organization exists. If not, create an entry.
* Create a new project entry. Marked it as NS (Not Started). 
* Create a new PartnerContactInfo entry for the project

If you don't intend to process the data straight away, make sure you overview the data to document some attributes, have all tables that define attributes and values used. Some information will be answered by emails. Make sure you keep a copy by converting the email as a PDF and make it available in the respective project folder under "communication". 

Overviewing the data includes:
* Observations have XY coordinates.
* Reference system of the coordinates is known.
* Protocol is documented (can be a report, email exchange, or found in the db itself).
* Species code definition is present. We cannot assume the species code used is the same as the one used by WildTrax. We need to check.
* If behaviour data are present, make sure we have the meaning of the values.
* Date and time is present.
* Abundance is present.

# 4. Reformatting the data for WildTrax.
Reformating will be unique per project. Some projects will use rules that are similar. All scripts are available to allow reusing chunk of codes. 

An example of the expected upload format for the point count and definition of attributes are found under [template](https://github.com/MelinaHoule/WT-Integration/tree/main/template).


# 5. Uploading the data to WildTrax.
