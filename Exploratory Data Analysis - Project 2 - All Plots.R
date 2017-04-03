##
##
## Coursera: Exploratory Data Analysis, Week 4
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
        
##--------------- PLOT ONE ----------------------


        ##Q:    Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?  
        
        ##      Using the base plotting, make a plot showing the total PM2.5 emission from 
        ##      all sources for each of the years 1999, 2002, 2005, and 2008.      
        
        
        ##~~~~~~~~~~~ETL~~~~~~~~~~~~~~~~~~~~~   
                     
                years <- unique(NEI$year)
        
        
        #create an empty dataframe and name the rows
                annual_totals <- data.frame(totals = rep(NA,4))
                row.names(annual_totals) <-years
        
        
        #total each year's emmisions
                annual_totals[1,] <- sum(NEI$Emissions[NEI$year == years[1]])
                annual_totals[2,] <- sum(NEI$Emissions[NEI$year == years[2]])
                annual_totals[3,] <- sum(NEI$Emissions[NEI$year == years[3]])
                annual_totals[4,] <- sum(NEI$Emissions[NEI$year == years[4]])
                print(annual_totals)
        
                
                
                
        ##~~~~~~~~~~~PLOT~~~~~~~~~~~~~~~~~~~~~       
        
                
        
        #to make y scale more redable, lets avoid scietific notation  
        
                #first store the old value of "scipen" to be restored later
                old_scipen_value <- getOption("sciepen") 
                
                #now set scipen VERY HIGH
                options(scipen = 999)
                
              
        
        #plot data with lables, omitting x-axis labels
        

                barplot(
                        annual_totals[,1], 
                        ylim = c(0,8e+06), 
                        ylab = "Total PM2.5 Emissions (tons)", 
                        xlab = "Year", 
                        main ="Total PM2.5 Emissions in tons from All Sources \nUS 1999, 2002, 2005", 
                        xaxt="n"
                )
        
        
        #specify x-axis lables and spacing
                axis(
                        1, 
                        labels = years, 
                        at = c(1,2,3,4)
                )
        
        #create png device and copy plot, then close out device
                dev.copy(png, "plot1.png", 480, 480)
                dev.off()
                
                
                
        #restore "scipen" value back to defults
                options(scipen = old_scipen_value)
                
                
                
##--------------- PLOT TWO ----------------------

        ##Q:    Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 

        ##      Use the base plotting system to make a plot answering this question.
        
 
                
        ##~~~~~~~~~~~ETL~~~~~~~~~~~~~~~~~~~~~

        #Start by subsetting data
                balt <- subset(NEI, fips == "24510")        
                years <- unique(NEI$year)
                
                
        #create an empty dataframe and name the rows
                balt_totals <- data.frame(totals = rep(NA,4))
                row.names(balt_totals) <-years
                
                
        #total each year's emmisions
                balt_totals[1,] <- sum(balt$Emissions[balt$year == years[1]])
                balt_totals[2,] <- sum(balt$Emissions[balt$year == years[2]])
                balt_totals[3,] <- sum(balt$Emissions[balt$year == years[3]])
                balt_totals[4,] <- sum(balt$Emissions[balt$year == years[4]])
                
                
                
        ##~~~~~~~~~~~PLOT~~~~~~~~~~~~~~~~~~~~~
                
        #plot data with lables, omitting x-axis labels

                barplot(
                        balt_totals[,1], 
                        ylim = c(0,3500), 
                        ylab = "Total PM2.5 Emissions (tons), Baltimore City, Maryland", 
                        xlab = "Year", 
                        main ="Total PM2.5 Emissions in Tons from All Sources, \nBaltimore City, MD. - 1999, 2002, 2005, 2008", 
                        xaxt="n"
                )
                
                
         #specify x-axis lables and spacing
                axis(
                        1, 
                        labels = years, 
                        at = c(1,2,3,4)
                )
          
                
          #create png device and copy plot, then close out device
                dev.copy(png, "plot2.png", 480, 480)
                dev.off()
                
                
                
                
##--------------- PLOT THREE ----------------------
                
                
        ## Q:   Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
        ##      which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
        ##      Which have seen increases in emissions from 1999-2008? 
                
        ##    Use the ggplot2 plotting system to make a plot answer this question.

                
            require("ggplot2")
                
            
                    
        ##~~~~~~~~~~~ETL~~~~~~~~~~~~~~~~~~~~~
        
                
        #Start by subsetting data
                balt <- subset(NEI, fips == "24510")        
                
                
        #now aggregate the Emissions data over year and type, applying sum to the aggregate
                balt_agg <- aggregate(Emissions ~ (year + type), balt, sum )
                                
                
                
                
        ##~~~~~~~~~~~PLOT~~~~~~~~~~~~~~~~~~~~~
        
                
        #create ggplot "core" wth data and aesthetic layer ("the data")
                gp <- ggplot(balt_agg, aes(x=year, y=Emissions)) 
                
        #add gemetic layer ("the visuals")
                gp <- gp + geom_col() 
                
        
        #add additional properties...
        
                
        #(facets break up data into separate graphs, over a particular factor)
                gp <- gp + facet_grid(. ~ type) 
                
        #here we tweak the x-axis tick marks         
                gp <- gp + scale_x_continuous(breaks=balt_agg$year[1:4])
                
        #fianlly, call ggplot object to display it...
                gp
                
                
        #of course, all of this could have been called in a single, less easily read line, as well
                #ggplot(balt_agg, aes(x=year, y=Emissions)) + geom_col() + facet_grid(. ~ type) + scale_x_continuous(breaks=balt_agg$year[1:4])
                
        
        #create png device and copy plot, then close out device
                dev.copy(png, "plot3.png", 480, 480)
                dev.off()
                
                
                
                
                
                
                
                
                
                
##--------------- PLOT FOUR ----------------------  


        ## Q:   Across the United States, how have emissions from coal combustion-related 
        ##      sources changed from 1999-2008?     
                
        
                
                require(data.table)     
                
                
        ##~~~~~~~~~~~ETL~~~~~~~~~~~~~~~~~~~~~
                
                
        #Extract SCC codes of "coal combustion-related"
                
                #coal is a boolean vector, true for each row containing "coal" in the EI.Sector column
                coal <- grepl("Coal", SCC$EI.Sector, ignore.case = TRUE)
                #comb is a boolean vector, true for each row containing "comb" in the EI.Sector column
                comb <- grepl("Comb", SCC$EI.Sector, ignore.case = TRUE)
                
                #now get values of SCC column where both coal and combustion (comb) are true
                cc_scc_codes <- SCC$"SCC"[coal & comb] 
                
                
        
        #subset NEI for all observations from sites listed in cc_scc_codes
        
                #convert to data.table for fast INNER JOIN
                dt_NCI <- data.table(NEI)
                dt_SCC <- data.table("SCC" = cc_scc_codes)
                
                #perform INNER JOIN (comparable to merge, but faster and better on memory)
                dt_NCI_coalcombust <- dt_NCI[dt_SCC, on = "SCC", nomatch = 0]
                
                
        #finally, total the values of each year
                NCI_coal_sum_by_year <- aggregate(Emissions ~ year, dt_NCI_coalcombust, sum)
                
                
                
                
                
                
        ##~~~~~~~~~~~PLOT~~~~~~~~~~~~~~~~~~~~~               
                
                
                
        #set scipen very high, so that scientic numbers are not used
        
                #first store the old value of "scipen" to be restored later
                old_scipen_value <- getOption("sciepen") 
                
                #now set scipen VERY HIGH
                options(scipen = 999)
                
        #plot data with lables, omitting x-axis labels
                barplot(
                        NCI_coal_sum_by_year$Emissions, 
                        ylim = c(0,600000), 
                        ylab = "Total PM2.5 Emissions (tons)", 
                        xlab = "Year", 
                        main ="Total PM2.5 Emissions in Tons from Coal Combustion-related Sources, \nUSA - 1999, 2002, 2005, 2008", 
                        xaxt="n"
                )
                
                
        #specify x-axis lables and spacing
                axis(
                        1, 
                        labels = NCI_coal_sum_by_year$year, 
                        at = c(1,2,3,4)
                )
                
                
        #create png device and copy plot, then close out device
                dev.copy(png, "plot4.png", 480, 480)
                dev.off()
                
                
         #restor scipen defult 
                options(scipen = old_scipen_value)      
                
                
                
                
                
                
                
                
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
                
                

                
                
                
##--------------- PLOT SIX ----------------------     

                
        ##Q:    Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
        ##      Which city has seen greater changes over time in motor vehicle emissions?
        
                
                require(ggplot2)
                
        
        ##~~~~~~~~~~~ETL~~~~~~~~~~~~~~~~~~~~~
        

                
        #Start by subsetting data of Baltimore and Los Angeles County
                balt <- subset(NEI, fips == "24510")  
                la <- subset(NEI, fips == "06037")  
                
                
                
        #Extract SCC codes of "motor vehicle sources"
                
                #vehicles is a boolean vector, true for each row containing "vehicle"  in the EI.Sector column
                vehicles <- grepl("Vehicles", SCC$EI.Sector, ignore.case = TRUE)
                
                #now get values of SCC column for rows where vehicles is true
                cc_scc_codes <- data.frame(SCC = SCC$"SCC"[vehicles]) 
                
                #note: there is one code that is vehicular, but not onroad
                #dim(cc_scc_codes)[1] - dim(subset(SCC, Data.Category=="Onroad"))[1] #[1] 1
                
                
        #subset cities for all observations from sites listed in cc_scc_codes
                #since the data set is small, merge {base} can handle it.
                
                balt_vehicle_em <- merge(balt, cc_scc_codes, by = "SCC" )
                la_vehicle_em <- merge(la, cc_scc_codes, by = "SCC" )
                
                
        #finally, total the values of each year, attatch city name, and combine into single dataframe       
                comparative <-rbind(
                                        cbind(aggregate(Emissions ~ year, balt_vehicle_em, sum), location = "Baltimore City, MD" ), 
                                        cbind(aggregate(Emissions ~ year, la_vehicle_em, sum), location = "Los Angeles County, California" )  
                )
                
                
        
                
        ##~~~~~~~~~~~PLOT~~~~~~~~~~~~~~~~~~~~~
        
                
                
                #create ggplot "core" wth data and aesthetic layer ("the data")
                gp <- ggplot(comparative, aes(x=year, y=Emissions)) 
                
                #add gemetic layer ("the visuals")
                gp <- gp + geom_col() 
                
                
                #add additional properties...
                
                
                #(facets break up data into separate graphs, over a particular factor)
                gp <- gp + facet_grid(. ~ location) 
                
                #here we tweak the x-axis tick marks         
                gp <- gp + scale_x_continuous(breaks=comparative$year[1:4])
                
                #fianlly, call ggplot object to display it...
                gp
                
                
                #of course, all of this could have been called in a single, less easily read line, as well
                #ggplot(comparative, aes(x=year, y=Emissions)) + geom_col() + facet_grid(. ~ location) + scale_x_continuous(breaks=comparative$year[1:4])

                
                #create png device and copy plot, then close out device
                dev.copy(png, "plot6.png", 480, 480)
                dev.off()
                
                
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                
                

        
        
