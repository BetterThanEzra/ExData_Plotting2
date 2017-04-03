##
##
## COursera: Exploratory Data Analysis, Week 4
## Peer-graded Assignment: Course Project 2
## 
## 
## 
## 
## The overall goal of this assignment is to explore the National Emissions Inventory database 
## and see what it say about fine particulate matter pollution in the United states over the 10-year 
## period 1999-2008. You may use any R package you want to support your analysis.
## 
## 
## 
## 
## Questions
## You must address the following questions and tasks in your exploratory analysis. 
## For each question/task you will need to make a single plot. Unless specified, you 
## can use any plotting system in R to make your plot.
##
##
##
## 1.) Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
##      Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
##
## 2.) Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
##      Use the base plotting system to make a plot answering this question.
## 
## 3.) Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
##      Which have seen increases in emissions from 1999-2008? 
##      Use the ggplot2 plotting system to make a plot answer this question.
## 
## 4.) Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?
## 
## 5.) How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
## 
## 6.) Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
##      Which city has seen greater changes over time in motor vehicle emissions?
##
##




##---------------- LOAD DATA --------------------


        #Download data and unzip
                #fileUrl1 <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
                #download.file(url=fileUrl1, destfile="pm25.zip")
                #unzip("pm25.zip")

        
        #Load Data
        ## "This first line will likely take a few seconds. Be patient!"
                NEI <- readRDS("summarySCC_PM25.rds")
                SCC <- readRDS("Source_Classification_Code.rds")
                
        
                #Note:
                #count_of_emssions_equal_to_zero <- length(NEI$Emissions[NEI$Emissions==0])
                #count_of_total_emissions <- dim(NEI)[1]
                #percent_of_0_emission_records = count_of_emssions_equal_to_zero / count_of_total_emissions
                #0.05055581
        
              
                
                
##--------------- PLOT FIVE ----------------------    

        ## Q:   How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
 

                        
                
        ##~~~~~~~~~~~ETL~~~~~~~~~~~~~~~~~~~~~
        
                
        #Start by subsetting data of Baltimore
                balt <- subset(NEI, fips == "24510")     
                
                
                
        #Extract SCC codes of "motor vehicle sources"
                
                #vehicles is a boolean vector, true for each row containing "vehicle" in the EI.Sector column
                vehicles <- grepl("Vehicles", SCC$EI.Sector, ignore.case = TRUE)
                
                #now get values of SCC column for rows where vehicles is true
                cc_scc_codes <- data.frame(SCC = SCC$"SCC"[vehicles]) 
                
                #note: there is one code that is vehicular, but not onroad
                #dim(cc_scc_codes)[1] - dim(subset(SCC, Data.Category=="Onroad"))[1] #[1] 1
                
                
        #subset balt for all observations from sites listed in cc_scc_codes
        #since the data set is small, merge {base} can handle it.
                
                balt_vehicle_em <- merge(balt, cc_scc_codes, by = "SCC" )
                
                
        #finally, total the values of each year        
                balt_vehicle_em_sum_annual <- aggregate(Emissions ~ year, balt_vehicle_em, sum)
 
                
                
                               
                
        ##~~~~~~~~~~~PLOT~~~~~~~~~~~~~~~~~~~~~               
                
                

        #plot data with lables, omitting x-axis labels
                barplot(
                        balt_vehicle_em_sum_annual$Emissions, 
                        ylim = c(0,400), 
                        ylab = "Total PM2.5 Emissions (tons)", 
                        xlab = "Year", 
                        main ="Total PM2.5 Emissions in Tons from Motor Vehicle Sources, \nBaltimore City, MD - 1999, 2002, 2005, 2008", 
                        xaxt="n"
                )
                
                
        #specify x-axis lables and spacing
                axis(
                        1, 
                        labels = balt_vehicle_em_sum_annual$year, 
                        at = c(1,2,3,4)
                )
                
                
        #create png device and copy plot, then close out device
                dev.copy(png, "plot5.png", 480, 480)
                dev.off()
                
                

                
  