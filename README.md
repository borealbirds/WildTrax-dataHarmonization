# WT-Integration
**** The documentation is in progress. Feel free to add sections. 

This repo aimed to share scripts, lookup tables and templates needed to translate point counts data into WT upload format. The Directory folder of the working directory is presented below. Sub-folder "project" and "out" aren't shared because they are often too large.  

# Directory Structure
<pre>
./                                      Sample files for configuring and running scripts

./WT_Point_Count_Codes                  Distance, duration and species code used by WT database

./archived                              Old versions of scripts 

./lookupTables                          Project specific lookup tables created to allow translation from source data to WildTrax format

./out                                   Location of the resulting output (location, visit, survey, extended)  (Not shared)

./project                               Master folder where all source data are found within thei respective subfolder (Not shared)

./script                                R scripts to summarize WT output for validation checks

./template                              Example of output table structure, communication, ...
</pre>

# Introduction

The Data intake process involve five main steps:

* Communication with data contributor.
* Acquire sharing agreement
* Acquire source data and related documentation to allow reformatting.
* Reformat the data.
* Upload data to WildTrax.


# Communication with data contributor

Communication  with data partners are generally by email. When relevant, save email communication as a pdf and store it into the respective data folder under communication. Email template should be added in Template directory 


# Sharing Agreement

A template of Sharing agreement should be found in Template. In WT, sharing can be either:

1. **Published - Private**  - The project data will only be available to project members. Users will need to request access to the project in order to view any details such as species or locations.
2. **Published - Map Only** - The project data will be accessible through Data Discover but the media and report are not accessible to users who are not project members. If you're not a project or organization member, the location buffering and visibility settings will apply.
3. **Published - Map + Report Only** - (not for point counts) The project data become available to all WildTrax users through Data Downloads and Data Discover, however, the media is not accessible. Use this setting if you want to make your data publicly available but there are privacy concerns with your media. If you're not a project or organization member, the location buffering and visibility settings will apply.
4. **Published - Public** - The project data become available to any WildTrax user as well as the details of the project in Data Downloads and Data Discover. If you're not a project or organization member, the location buffering and visibility settings will apply.

(Note that you can also use **Active** status for projects that are currently being processed, designed or are in the preliminary stages of data uploading. Use this status if the project is actively being worked on. This is the default project status when it is first created.)

You need to make sure the communication reflect these options.


# Source data and related documentation to allow reformatting

All data acquired should be logged in the [template](https://github.com/MelinaHoule/WT-Integration/blob/345282009ddcbd465f07789eca1cc0b8ba78e13a/project_Integration.xlsx) as soon as you receive it. 

* Make sure the Organization exists. If not, create an entry.
* Create a new project entry. Marked it as NS (Not Started). 
* Create a new PartnerContactInfo entry for the project

If you don't intend to process the data straight away, make sure you overview the data to document some attributes, have all tables that define attributes and values used. Some informations will be answered by emails. Make sure you keep a copy by converting the email as a PDF and make it available in the respective project folder. 

Overviewing the data includes:
* Observations have XY coordinates.
* Reference system of the coordinates is known.
* Protocol is documented (can be a report, email exchange, or found in the db itself)
* Species code definition is present. We cannot assume the species code used is the same as the one used by WildTrax. We need to check.
* If behaviour data are present, make sure we have the meaning of the values.
* Date and time is present.
* Abundance is present

# Reformatting
Reformating will be unique per project. Some projects will use rules that are similar. scripts are all available to allow reusing chunk of codes. 

Example of the expected upload format for the Point Count and definition of attributes are found under [template](https://github.com/MelinaHoule/WT-Integration/tree/main/template).


# Upload to WildTrax

# Citation

# Version Number Scheme
