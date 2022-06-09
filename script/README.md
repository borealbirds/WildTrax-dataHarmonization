Scripts should all have a header that mention the PCODE, Project name, authors of the script, dates, specifications on the source data and details on translation that needs attention.

Pending tasks should be put on top with github issues number stated until resolved. 

Example  
--------------------------------------------------------------------------------------------------------------------------------------  
#--PENDING: Duplicates are found in source data at the sruvey level: Bird Data. We delete them for now. Waiting t hear back from our partner (issue #1)  
#-    
#- PCODE: NEFBMP2012-19  
#- Title: "Translate New England Forest Bird Monitoring Program 2012-2019"   
#- Source dataset is an Excel spreadsheet using 2 sheet: Site Data, Bird Data  
#- Author: "Melina Houle"  
#- Date: "March 11, 2022"  
#- Note on translation:  
#-- Download manually source data locally prior to process  
#-- Species code used are not equivalent to WildTrax species_code. Species codes need to be derived by using species common name  
#-- 3 stations use concatenation of transect name + number instead of a numeric Point_Count (ADAMSCAMP, BAKERBUSH, CRAFTBURYOU). Fix is hardcoded.   
#---  

