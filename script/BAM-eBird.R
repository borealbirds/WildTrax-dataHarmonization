#Hi Melina,
#To start with, I installed a program called cygwin64 on my computer. It's a program for reading text in eBird records since eBird stores records differently from a regular CSV or text file.

#I then installed an R package called "auk".

#I then went to the eBird website and made a request to download all data for a specific region. Once a request has been processed, eBird sends me a link to a folder that I download. So far, I've downloaded eBird data for Alberta, British Columbia, and Nova Scotia. While it's possible to download a country's worth of records, I wouldn't recommend it over processing one province or state at a time. Both BC and Alberta have so much data that it took a few hours for me to take each of those eBird datasets and convert them each to an enormous data frame that I saved as a large RData file.

#Once you have the RData file saved, then things go a lot faster. I filtered observations to those whose methods were described as stationary counts that lasted 10 minutes or less and created the columns needed for calculating offsets in QPAD based on Peter's "recurring" folder. 

#It will take me a bit of time to organize it properly: all I've done so far was copy the file I created for Nova Scotia stationary counts to my version of the National Model offsets R project on my computer, where I calculated offsets. If you give me a couple days I can reorganize everything and give you counts and offsets for Nova Scotia, and perhaps BC and Alberta too. Then we can meet and discuss any other refinements to the script that need to be made.

#In the meantime, here is a link you can use: https://drive.google.com/drive/folders/1krVLWYk88EMZmuZLQVd1FjEz9825zb6q?usp=sharing