
##================================================================##
##                                                                ##
##   Network Visualization of Multi-data Sources using R          ##
##                                                                ##
##   uRos2021                                                     ##
##                                                                ##
##   Based on the work developed under                            ##
##   Essnet2018] ESSnet on Big Data 2018 -2020                    ##
##   Eurostat grant ESTAT-PA11-2018-8                             ##
##   Multipurpose statistics and efficiency gains in production   ##
##   ESSnet Big Data II WPJ - Task 1C                             ##
##                                                                ##
##   Rui Alves                                                    ##
##   Statistics Portugal                                          ##
##   Email: rui.alves@ine.pt                                      ##
##                                                                ##
##================================================================##


#1. Presentation -------------------------------------------------------------
#
# This R-script is intended to produce a network visualisation based 
# on visNetwork package. 

# Network visualization is a valuable alternative to present a dense and complex set of heavily interconnected data, particularly when it offers interactivity. Under the ESSnet on Big Data 2018 -2020 - Eurostat grant ESTAT-PA11-2018-8 Multipurpose statistics and efficiency gains in production, one of the goals was to provide an interactive graphical representation of connections between data sources (multi-purpose data sources, survey data and web scraped data), variables and domains, countries and experimental results.


# The input data (nodes and edges) is based the graphic WPJ Flow Models provided by the eight countries envolved in Essnet2018 grant: Bulgaria, Germany- Hesse, Greece, Italy, the Netherlands, Poland, Portugal and Slovakia. 
# Adaptations had to be made for compatibility reasons and original content was updated. 
# The visNetwork displays networks consisting of nodes and edges. 
# This visualization is easy to use and supports custom shapes, styles, colours, sizes, images and more.
# The visNetwork visualization works on any modern browser. 
# To handle a larger amount of nodes, visNetwork has clustering support if needed.


#2. About this script -------------------------------------------------------------

# The source for the function required_packages is the "Elegant way to check for missing packages and install them?" post from stackoverflow (https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them/44660688#44660688 .) It checks if the required packages are installed and if they don't it asks the user if he/she wants to install.


# The function set_wd() sets the working directory to the folder you are now working from. So we don’t need the setwd("paste here the absolute path"). Package rstudioapi is required.
# For more details: https://eranraviv.com/r-tips-and-tricks-working-directory/



#3. Load Packages -----------------------------------------------------------
#~~3.1 Create required_packages function ---------------------------------------------------------

required_packages <- function(...) {
        libs <- unlist(list(...))
        req <- unlist(lapply(libs,require,character.only = TRUE))
        need <- libs[req == FALSE]
        n <- length(need)
        if(n > 0){
                libsmsg <- if(n > 2) paste(paste(need[1:(n-1)],collapse=", "),",",sep="") else need[1]
                print(libsmsg)
                if(n > 1){
                        libsmsg <- paste(libsmsg," and ", need[n],sep="")
                }
                libsmsg <- paste("The following packages could not be found: ",libsmsg,"\n\r\n\rInstall missing packages?",collapse = "")
                if(winDialog(type = c("yesno"), libsmsg) == "YES"){       
                        install.packages(need)
                        lapply(need,require,character.only = TRUE)
                }
        }
}

required_packages("dplyr", "visNetwork", "rstudioapi")


#4. Set working directory --------------------------------------------------
#~~4.1 Create set_wd function --------------------------------------------------
set_wd <- function() {
        # library(rstudioapi) # make sure you have it installed
        current_path <- getActiveDocumentContext()$path 
        setwd(dirname(current_path ))
        print( getwd() )
}

set_wd()

#5.  Load Data ---------------------------------------------------------------
# Run the code bellow in order to recreate the data as dataframes in your current session


# ~~5.1 Run Code to create Dataframes ---------------------------------------------------------------


# ~~5.1.1 Create Nodes ---------------------------------------------------------------

# dput(nodes)

nodes <- structure(list(id = c(-1348489570L, -1571167369L, 1970009842L, 
                               -560658481L, -1607614960L, 117368370L, 2015487688L, -818898040L, 
                               101902232L, 1057131455L, 2012453948L, 1232757658L, -2071905485L, 
                               382194493L, 520931725L, -412414323L, 1035842796L, -1142430416L, 
                               -1730926905L, -923249231L, 896763173L, 392114958L, -2025409250L, 
                               -397720592L, 1984026328L, 763696093L, -502749944L, -1732064462L, 
                               459896560L, 623760513L, -1122338699L, -343200140L, 136949862L, 
                               -934483136L, -326741965L, -384623412L, 492002219L, 265804694L, 
                               975882099L, 205446448L, 1229429034L, 1817881360L, 2121201565L, 
                               -1481182525L, -218836211L, 1500219763L, 2017665772L, 260209331L, 
                               -333796411L, 1382076146L, -1908834875L, -366601983L, -397756546L, 
                               -2017069376L, -1114520580L, -428468662L, -1655853647L, 1049804537L, 
                               -914515536L, -338109497L, 1606071174L, 154293575L, 1937297096L, 
                               -417893648L, 1963275057L, -880689581L, 1003246733L, -1015690683L, 
                               -4289225L, -1083288549L, -2003605932L, 1255616799L, 2035259298L, 
                               284547738L, 1969570735L, 211985999L, -1381630046L, -1636572429L, 
                               -40831304L, -1848286899L, -478428115L, 1381473985L), label = c("365tickets", 
                                                                                              "365tickets.com", "Airbnb", "Airbnb.com", "Airport Data", "BAG Viewer", 
                                                                                              "booking", "booking.com", "Border Traffic Survey", "Border traffic survey (BI)", 
                                                                                              "BTB data", "City Tourism", "Credit & Debit Card transactions", 
                                                                                              "Eurostat Experimental Statistics", "Event related tourism", 
                                                                                              "Financial transaction data", "Flash estimates", "holydaycheck", 
                                                                                              "holydaycheck.de", "hotels", "hotels.com", "hrs", "hrs.com", 
                                                                                              "Improve quality data on trips", "Improve quality of satellite accounts", 
                                                                                              "Improve quality of tourists expenses data", "Improve quality of tourists transport data", 
                                                                                              "Improve tourist accommodation base", "Land Border Traffic", 
                                                                                              "Mobile phone data", "Monthly bed tax data inc. overnights", 
                                                                                              "Multi-Purpose Data", "NBTC Dashboard", "NBTC-NIPO", "Netherlands Chamber of Commerce", 
                                                                                              "NRT Local Accommodation", "NTR Tourist Establishments", "Other sources", 
                                                                                              "Other webpages (booking accommodation)", "Parking Meters Data", 
                                                                                              "pincamp", "pincamp.de", "pochivka", "pochivka.bg", "Railway, airport and port data", 
                                                                                              "Register of accommodation establishments", "Register of Addresses and Buildings", 
                                                                                              "Register of non-categorised accommodation", "Register of Properties offered for short-term lease", 
                                                                                              "Regulatory reporting FIN1-12", "seatguru", "seatguru.com", "Skyscanner", 
                                                                                              "Skyscanner.net", "Smart City", "Spatial disaggregation of accommodation", 
                                                                                              "Survey Data", "Survey data on tourist trips", "Survey on museums and similar", 
                                                                                              "Survey on participation of residents in trips", "Survey on rural tourism accommodations", 
                                                                                              "Survey on the visits by foreigners", "Survey on tourist accommodation base", 
                                                                                              "Survey on trips made by foreigners", "Tax Authority Register of Short-Term Lease Properties", 
                                                                                              "taxi2airport", "taxi2airport.com", "Tourism attractiveness", 
                                                                                              "Tourism potential", "travelBI by Turismo de Portugal", "Tripadvisor", 
                                                                                              "Tripadvisor.com", "Turismo de Portugal Local Accommodation Open Data", 
                                                                                              "Turismo de Portugal Tourist Establishments Open Data", "Update register of accommodation establishments", 
                                                                                              "Waste production (ISPRA)", "Water Consumption", "Water Demand", 
                                                                                              "Web Data", "WPJ Milestones and Deliverables", "nocowanie", "nocowanie.pl"
                               ), title = c("365tickets.com <br> [PL] <br> <br> id:-1348489570", 
                                            "https://www.365tickets.com <br> [PL] <br> <br> id:-1571167369", 
                                            "airbnb.com <br> [EL] [NL] <br> <br> id:1970009842", "https://www.airbnb.com <br> [EL] [NL] <br> <br> id:-560658481", 
                                            "Airport Data <br> and  <br> Other sources  <br>  (Locatus, Airports register) <br>[BG] [NL] <br> <br> id:-1607614960", 
                                            "bagviewer.kadaster.nl <br> [NL] <br> <br> id:117368370", "booking.com <br> [DE] [BG] [IT] [NL] [PL] [SK] <br> <br> id:2015487688", 
                                            "https://www.booking.com <br> [BG] [DE] [IT] [NL] [PL] [SK] <br> <br> id:-818898040", 
                                            "Border <br> Traffic Survey <br> [PL] <br> <br> id:101902232", 
                                            "Border traffic survey <br> (Bank of Italy) <br> [IT] <br> <br> id:1057131455", 
                                            "BTB data (unique wifi connection, etc.) <br> [SK] <br> <br> id:2012453948", 
                                            "City Tourism <br>  [DE] [IT] [PL] [SK] <br> <br> id:1232757658", 
                                            "Credit & Debit <br> card transactions  <br> [PT] <br> <br> id:-2071905485", 
                                            "https://ec.europa.eu/eurostat/web/experimental-statistics <br> <br> id:382194493", 
                                            "Event related tourism <br> [DE] [IT] [PL] <br> <br> id:520931725", 
                                            "Financial transaction data <br> [SK] <br> <br> id:-412414323", 
                                            "Flash estimates <br> of the use of  <br> tourist accommodation base <br> [BG] [DE] [EL] [IT] [PL] [SK] <br> <br> id:1035842796", 
                                            "holidaycheck.de <br> [DE] <br> <br> id:-1142430416", "https://www.holidaycheck.de/ <br> [DE] <br> <br> id:-1730926905", 
                                            "hotels.com <br> [DE] [BG] [EL] [IT] [NL] [PL] [PT] [SK] <br> <br> id:-923249231", 
                                            "https://www.hotels.com <br> [BG] [DE] [EL] [IT] [NL] [PL] [PT] [SK] <br> <br> id:896763173", 
                                            "hrs.de <br> [DE] <br> <br> id:392114958", "https://www.hrs.de/hotel/ <br> [DE] <br> <br> id:-2025409250", 
                                            "Improve quality data <br> on trips <br> [BG] [DE] [IT] [PL] [SK] <br> <br> id:-397720592", 
                                            "Improve quality <br> of satellite <br> accounts <br> [BG] [DE]  [IT] [NL] [PL] [PT] [SK] <br> <br> id:1984026328", 
                                            "Improve quality <br>  of tourists expenses <br> data <br>  [DE] [IT] [NL] [PT] [PL] [SK] <br> <br> id:763696093", 
                                            "Improve quality of <br> tourists transport data <br> [SK] <br> <br> id:-502749944", 
                                            "Improve tourist <br> accommodation <br> base of reference <br> [BG] [DE] [EL] [IT] [NL] [PL] [PT] [SK] <br> <br> id:-1732064462", 
                                            "Land Border Traffic <br> Traffic Sensors, <br> Border Guard Data <br> Automatic Number Plate Recognition System <br> [PL] <br> <br> id:459896560", 
                                            "Anonymized mobile <br> phone data (pilot): <br> no of guests (overnight), <br> daytime tourists <br> [DE] <br> <br> id:623760513", 
                                            "Monthly bed tax data including the overnights data in the cities Bratislava and Košice <br> [SK] <br> <br> id:-1122338699", 
                                            "Multi-Purpose  <br> Data <br> <br> id:-343200140", "https://dashboard.nbtc.nl/jive/jivereportcontents.ashx?report=home_new <br> [NL] <br> <br> id:136949862", 
                                            "NBTC-NIPO <br> Research company <br> on holiday, <br> recreation and <br> business travel <br> [NL] <br> <br> id:-934483136", 
                                            "kvk.nl <br> [NL] <br> <br> id:-326741965", "National Tourist Registration <br> Local Accommodation <br> [PT] <br> <br> id:-384623412", 
                                            "National Tourist Registration <br>Tourist Establishments <br> [PT] <br> <br> id:492002219", 
                                            "Other sources <br> (Terrace Research, <br> Museum and recreation statistics)<br> [DE] [NL] [PL] <br> <br> id:265804694", 
                                            "Other national webpages <br> for booking of accommodation <br> [SK] <br> <br> id:975882099", 
                                            "Parking Meters Data <br> [PL] <br> <br> id:205446448", "pincamp.de <br> [DE] <br> <br> id:1229429034", 
                                            "https://www.pincamp.de/ <br> [DE] <br> <br> id:1817881360", 
                                            "pochivka.bg <br> [BG] <br> <br> id:2121201565", "pochivka.bg <br> [BG] <br> <br> id:-1481182525", 
                                            "Railway, airport <br> and port data <br> [DE] [IT] <br> <br> id:-218836211", 
                                            "Register of  <br> accommodation establishments <br> [SK] <br> <br> id:1500219763", 
                                            "Register of <br> Addresses and <br> Buildings (BAG 2019)<br> [NL] <br> <br> id:2017665772", 
                                            "Register of <br> non-categorised tourist <br> accommodation establishments <br> (CoC*) <br> [NL] [PL] <br> <br> id:260209331", 
                                            "Register of Properties <br> offered for  <br> short-term lease <br>  through digital platforms <br> [EL] <br> <br> id:-333796411", 
                                            "Regulatory reporting FIN1-12 <br> [SK] <br> <br> id:1382076146", 
                                            "seatguru.com <br> [PL] <br> <br> id:-1908834875", "https://www.seatguru.com <br> [PL] <br> <br> id:-366601983", 
                                            "skyscanner.net <br> [NL] [PL] [SK] <br> <br> id:-397756546", 
                                            "https://www.skyscanner.net <br> [NL] [PL] [SK] <br> <br> id:-2017069376", 
                                            "Smart City <br> [PL] <br> <br> id:-1114520580", "Spatial disaggregation <br> of data on tourist <br> accommodation base  <br>  [DE] [EL] [IT] [NL] [PL] [SK] <br> <br> id:-428468662", 
                                            "Survey <br> Data <br> <br> id:-1655853647", "Survey data on tourist trips <br> of Bulgarian residents <br> in the country and\r\n\r\nabroad <br> and the expenditure <br> [BG] <br> <br> id:1049804537", 
                                            "Survey on museums <br> and similar institutions <br> [DE] [IT] <br> <br> id:-914515536", 
                                            "Survey on <br> participation of <br> residents in trips <br> [BG] [IT:HBS] [NL: CVO] [PL] [PT: IDR] [SK] <br> <br> id:-338109497", 
                                            "Survey on <br> rural tourism accommodations <br> [IT] <br> <br> id:1606071174", 
                                            "Survey data on the visits by foreigners in Bulgaria <br> [BG] <br> <br> id:154293575", 
                                            "Survey on <br> tourist <br> accommodation base <br> [BG] [DE] [EL] [IT] [PL] [SK] <br> <br> id:1937297096", 
                                            "Survey on <br> trips made <br> by foreigners <br> [NL] [PL] [SK] <br> <br> id:-417893648", 
                                            "Tax Authority Register of Short-Term Lease Properties  <br> [EL] <br> <br> id:1963275057", 
                                            "Price of taxis <br> from / to <br> the airport <br> taxi2airport.com <br> [NL] <br> <br> id:-880689581", 
                                            " https://www.taxi2airport.com <br> [NL] <br> <br> id:1003246733", 
                                            "Tourism attractiveness <br>  [DE] [IT] [PL] [SK] <br> <br> id:-1015690683", 
                                            "Tourism potential <br>  [DE] [IT] [PL] [SK] <br> <br> id:-4289225", 
                                            "Turismo de Portugal: <br> TravelBI <br> https://travelbi.turismodeportugal.pt/en-us/Pages/Home.aspx <br> [PT] <br> <br> id:-1083288549", 
                                            "tripadvisor.com <br> [DE] [IT] [NL] <br> <br> id:-2003605932", 
                                            "https://www.tripadvisor.com <br> [DE] [IT] [NL] <br> <br> id:1255616799", 
                                            "Turismo de Portugal Open Data: <br>Local Accommodation <br> https://dadosabertos.turismodeportugal.pt/datasets/estabelecimentos-de-al <br> [PT] <br> <br> id:2035259298", 
                                            "Turismo de Portugal Open Data: <br> Tourist Establishment <br> https://dadosabertos.turismodeportugal.pt/datasets/empreendimentos-turisticos-existentes?geometry=-91.485%2C33.243%2C75.771%2C54.949 <br> [PT] <br> <br> id:284547738", 
                                            "Update register of <br> accommodation establishments <br> [SK] <br> <br> id:1969570735", 
                                            "Waste production <br> (ISPRA: National Institute <br> for Environmental Protection) <br> [DE] [IT] <br> <br> id:211985999", 
                                            "Water Consumption <br> [DE] [IT] [PL] <br> <br> id:-1381630046", 
                                            "Water Demand <br> [DE] [PL] <br> <br> id:-1636572429", "Web <br> Scraped <br> Data <br> <br> id:-40831304", 
                                            "https://webgate.ec.europa.eu/fpfis/mwikis/essnetbigdata/index.php/WPJ_Milestones_and_deliverables. <br> <br> id:-1848286899", 
                                            "nocowanie.pl <br> [PL] <br> <br> id:-478428115", "id:7000194<br> <br>  https://www.nocowanie.pl <br> [PL] <br> <br> id:1381473985"
                               ), group = c("Country: PL, Tourism Domain: Demand, Data Source: Web Scraped Data, Tourism Domain: Expenses", 
                                            "Country: PL, Tourism Domain: Demand, Data Source: Web Scraped Data, Tourism Domain: Expenses, External Links", 
                                            "Country: NL, Country: EL, Tourism Domain: Supply, Data Source: Web Scraped Data, Tourism Domain: Accommodation Base", 
                                            "Country: NL, Country: EL, Tourism Domain: Supply, Data Source: Web Scraped Data, Tourism Domain: Accommodation Base, External Links", 
                                            "Country: BG, Country: NL, Tourism Domain: Demand, Data Source: Multi-Purpose Data, Tourism Domain: Trips", 
                                            "Country: NL, Tourism Domain: Supply, Data Source: Multi-Purpose Data, External Links", 
                                            "Country: DE, Country: BG, Country: IT, Country: PL, Country: NL, Country: SK, Tourism Domain: Supply, Data Source: Web Scraped Data, Tourism Domain: Accommodation Base, Tourism Domain: Expenses, Tourism Domain: Trips", 
                                            "Country: BG, Country: DE, Country: IT, Country: PL, Country: NL, Country: SK, Tourism Domain: Supply, Data Source: Web Scraped Data, Tourism Domain: Accommodation Base, Tourism Domain: Expenses, Tourism Domain: Trips, External Links", 
                                            "Country: PL, Tourism Domain: Demand, Data Source: Survey Data", 
                                            "Country: IT, Tourism Domain: Demand, Data Source: Multi-Purpose Data", 
                                            "Country: SK, Data Source: Multi-Purpose Data", "Country: DE, Country: IT, Country: PL, Country: SK, Tourism Domain: Demand, Tourism Domain: Supply, Experimental Results", 
                                            "Country: PT, Tourism Domain: Demand, Data Source: Multi-Purpose Data", 
                                            "Country: EL, Tourism Domain: Supply, Tourism Domain: Demand,  External Links", 
                                            "Country: DE, Country: IT, Country: PL, Tourism Domain: Demand, Tourism Domain: Supply, Experimental Results", 
                                            "Country: SK, Data Source: Multi-Purpose Data, Tourism Domain: Demand", 
                                            "Country: DE, Country: IT, Country: BG, Country: EL, Country: PL, Country: SK, Tourism Domain: Supply, Tourism Domain: Demand, Experimental Results", 
                                            "Country: DE, Tourism Domain: Supply, Data Source: Web Scraped Data, Tourism Domain: Accommodation Base", 
                                            "Country: DE, Tourism Domain: Supply, Data Source: Web Scraped Data, Tourism Domain: Accommodation Base, External Links", 
                                            "Country: DE, Country: BG, Country: IT, Country: PL, Country: PT, Country: NL, Country: EL, Country: SK, Tourism Domain: Supply, Data Source: Web Scraped Data, Tourism Domain: Accommodation Base, Tourism Domain: Expenses", 
                                            "Country: BG, Country: DE, Country: IT, Country: PL, Country: PT, Country: NL, Country: EL, Country: SK, Tourism Domain: Supply, Data Source: Web Scraped Data, Tourism Domain: Accommodation Base, Tourism Domain: Expenses, External Links", 
                                            "Country: DE, Tourism Domain: Supply, Data Source: Web Scraped Data, Tourism Domain: Accommodation Base", 
                                            "Country: DE, Tourism Domain: Supply, Data Source: Web Scraped Data, Tourism Domain: Accommodation Base, External Links", 
                                            "Country: DE, Country: IT, Country: BG, Country: PL, Country: EL, Country: SK, Tourism Domain: Demand, Experimental Results", 
                                            "Country: DE, Country: IT, Country: BG, Country: NL, Country: PL, Country: PT, Country: SK, Tourism Domain: Demand, Experimental Results", 
                                            "Country: DE, Country: IT, Country: PT, Country: NL, Country: PL, Country: SK, Tourism Domain: Demand, Experimental Results", 
                                            "Country: SK, Experimental Results", "Country: DE, Country: IT, Country: BG, Country: EL, Country: NL, Country: PL, Country: PT, Country: SK, Tourism Domain: Supply, Experimental Results", 
                                            "Country: PL, Tourism Domain: Demand, Data Source: Multi-Purpose Data", 
                                            "Country: DE, Tourism Domain: Demand, Data Source: Multi-Purpose Data", 
                                            "Country: SK, Data Source: Multi-Purpose Data, Tourism Domain: Accommodation Base, Tourism Domain: Supply", 
                                            "Country: DE, Country: BG, Country: IT, Country: PT, Country: NL, Country: PL, Country: EL, Country: SK, Data Source, Data Source: Multi-Purpose Data", 
                                            "Country: NL, Tourism Domain: Demand, Data Source: Multi-Purpose Data, External Links", 
                                            "Country: NL, Tourism Domain: Demand, Data Source: Multi-Purpose Data", 
                                            "Country: NL, Tourism Domain: Supply, Data Source: Multi-Purpose Data, External Links", 
                                            "Country: PT, Tourism Domain: Demand, Data Source: Multi-Purpose Data", 
                                            "Country: PT, Tourism Domain: Demand, Data Source: Multi-Purpose Data", 
                                            "Country: NL, Country: NL, Country: PL, Tourism Domain: Demand, Data Source: Multi-Purpose Data", 
                                            "Country: SK, Data Source: Web Scraped Data, Tourism Domain: Expenses, Tourism Domain: Accommodation Base", 
                                            "Country: PL, Tourism Domain: Demand, Data Source: Multi-Purpose Data", 
                                            "Country: DE, Tourism Domain: Supply, Data Source: Web Scraped Data, Tourism Domain: Accommodation Base", 
                                            "Country: DE, Tourism Domain: Supply, Data Source: Web Scraped Data, Tourism Domain: Accommodation Base, External Links", 
                                            "Country: BG, Tourism Domain: Demand, Data Source: Multi-Purpose Data", 
                                            "Country: BG, Tourism Domain: Demand, Data Source: Multi-Purpose Data, External Links", 
                                            "Country: DE, Country: IT, Data Source: Multi-Purpose Data, Tourism Domain: Demand", 
                                            "Country: PL, Country: SK, Data Source: Multi-Purpose Data, Tourism Domain: Supply", 
                                            "Country: NL, Tourism Domain: Supply, Data Source: Multi-Purpose Data", 
                                            "Country: NL, Country: PL, Tourism Domain: Supply, Data Source: Multi-Purpose Data", 
                                            "Country: EL, Tourism Domain: Supply, Data Source: Multi-Purpose Data", 
                                            "Country: SK, Data Source: Multi-Purpose Data, Tourism Domain: Supply", 
                                            "Country: PL, Tourism Domain: Demand, Data Source: Web Scraped Data, Tourism Domain: Trips", 
                                            "Country: PL, Tourism Domain: Demand, Data Source: Web Scraped Data, Tourism Domain: Trips, External Links", 
                                            "Country: PL, Country: NL, Country: SK, Tourism Domain: Demand, Data Source: Web Scraped Data, Tourism Domain: Trips, Tourism Domain: Expenses", 
                                            "Country: PL, Country: NL, Country: SK, Tourism Domain: Demand, Data Source: Web Scraped Data, Tourism Domain: Trips, Tourism Domain: Expenses, External Links", 
                                            "Country: PL, Tourism Domain: Demand, Data Source: Multi-Purpose Data", 
                                            "Country: DE, Country: IT, Country: NL, Country: EL,  Country: PL, Country: SK, Tourism Domain: Supply, Experimental Results", 
                                            "Country: DE, Country: BG, Country: IT, Country: PT, Country: NL, Country: PL, Country: EL, Country: SK, Data Source, Data Source: Survey Data", 
                                            "Country: BG, Tourism Domain: Demand, Data Source: Survey Data, Tourism Domain: Accommodation Base", 
                                            "Country: DE, Country: IT, Data Source: Survey Data, Tourism Domain: Demand", 
                                            "Country: BG, Country: IT, Country: NL, Country: PL, Country: PT, Country: SK,Tourism Domain: Demand, Data Source: Survey Data", 
                                            "Country: IT, Data Source: Survey Data, Tourism Domain: Supply", 
                                            "Country: BG, Tourism Domain: Demand, Data Source: Survey Data", 
                                            "Country: DE, Country: IT, Country: BG, Country: EL, Country: PL, Country: SK, Tourism Domain: Supply, Data Source: Survey Data", 
                                            "Country: NL, Country: PL, Country: SK, Tourism Domain: Demand, Data Source: Survey Data", 
                                            "Country: EL, Tourism Domain: Supply, Data Source: Multi-Purpose Data", 
                                            "Country: NL, Tourism Domain: Demand, Data Source: Web Scraped Data, Tourism Domain: Trips", 
                                            "Country: NL, Tourism Domain: Demand, Data Source: Web Scraped Data, Tourism Domain: Trips, External Links", 
                                            "Country: DE, Country: IT, Country: PL, Country: SK, Tourism Domain: Demand, Tourism Domain: Supply, Experimental Results", 
                                            "Country: DE, Country: IT, Country: PL, Country: SK, Tourism Domain: Demand, Tourism Domain: Supply, Experimental Results", 
                                            "Country: PT, Tourism Domain: Demand, Data Source: Multi-Purpose Data, External Links", 
                                            "Country: DE, Country: IT, Country: NL, Tourism Domain: Demand, Data Source: Web Scraped Data, Tourism Domain: Expenses, Tourism Domain: Supply, Tourism Domain: Accommodation Base", 
                                            "Country: DE, Country: IT, Country: NL, Tourism Domain: Demand, Data Source: Web Scraped Data, Tourism Domain: Expenses, External Links", 
                                            "Country: PT, Tourism Domain: Demand, Data Source: Multi-Purpose Data, External Links", 
                                            "Country: PT, Tourism Domain: Demand, Data Source: Multi-Purpose Data, External Links", 
                                            "Country: SK, Experimental Results", "Country: DE, Country: IT, Tourism Domain: Demand, Data Source: Multi-Purpose Data", 
                                            "Country: DE, Country: IT, Country: PL, Tourism Domain: Demand, Tourism Domain: Supply, Data Source: Multi-Purpose Data", 
                                            "Country: DE, Country: PL, Tourism Domain: Demand, Data Source: Multi-Purpose Data", 
                                            "Country: DE, Country: BG, Country: IT, Country: PT, Country: NL, Country: PL, Country: EL, Country: SK, Data Source, Data Source: Web Scraped Data", 
                                            "Country: PT, Country: NL, Country: EL, Tourism Domain: Supply, Tourism Domain: Demand, External Links", 
                                            "Country: PL, Tourism Domain: Supply, Data Source: Web Scraped Data, Tourism Domain: Accommodation Base, Tourism Domain: Expenses", 
                                            "Country: PL, Tourism Domain: Supply, Data Source: Web Scraped Data, Tourism Domain: Accommodation Base, Tourism Domain: Expenses, External Links"
                               ), value = c(2, 2, 4, 4, 4, 2, 12, 12, 2, 2, 2, 8, 2, 16, 6, 
                                            2, 12, 2, 2, 16, 16, 2, 2, 10, 14, 12, 2, 16, 2, 2, 2, 16, 2, 
                                            2, 2, 2, 2, 6, 2, 2, 2, 2, 2, 2, 4, 4, 2, 4, 2, 2, 2, 2, 6, 6, 
                                            2, 12, 16, 2, 4, 12, 2, 2, 12, 6, 2, 2, 2, 8, 8, 2, 6, 6, 2, 
                                            2, 2, 4, 6, 4, 16, 16, 2, 2), shape = c("dot", "triangle", "dot", 
                                                                                    "triangle", "dot", "triangle", "dot", "triangle", "dot", "dot", 
                                                                                    "dot", "square", "dot", "triangle", "square", "dot", "square", 
                                                                                    "dot", "triangle", "dot", "triangle", "dot", "triangle", "square", 
                                                                                    "square", "square", "square", "square", "dot", "dot", "dot", 
                                                                                    "dot", "triangle", "dot", "triangle", "dot", "dot", "dot", "dot", 
                                                                                    "dot", "dot", "triangle", "dot", "triangle", "dot", "dot", "dot", 
                                                                                    "dot", "dot", "dot", "dot", "triangle", "dot", "triangle", "dot", 
                                                                                    "square", "dot", "dot", "dot", "dot", "dot", "dot", "dot", "dot", 
                                                                                    "dot", "dot", "triangle", "square", "square", "triangle", "dot", 
                                                                                    "triangle", "triangle", "triangle", "square", "dot", "dot", "dot", 
                                                                                    "dot", "triangle", "dot", "triangle"), color = c("orange", "lightblue", 
                                                                                                                                     "orange", "lightblue", "orange", "lightblue", "orange", "lightblue", 
                                                                                                                                     "orange", "orange", "orange", "purple", "orange", "lightblue", 
                                                                                                                                     "purple", "orange", "purple", "orange", "lightblue", "orange", 
                                                                                                                                     "lightblue", "orange", "lightblue", "purple", "purple", "purple", 
                                                                                                                                     "purple", "purple", "orange", "orange", "orange", "teal", "lightblue", 
                                                                                                                                     "orange", "lightblue", "orange", "orange", "orange", "orange", 
                                                                                                                                     "orange", "orange", "lightblue", "orange", "lightblue", "orange", 
                                                                                                                                     "orange", "orange", "orange", "orange", "orange", "orange", "lightblue", 
                                                                                                                                     "orange", "lightblue", "orange", "purple", "teal", "orange", 
                                                                                                                                     "orange", "orange", "orange", "orange", "orange", "orange", "orange", 
                                                                                                                                     "orange", "lightblue", "purple", "purple", "lightblue", "orange", 
                                                                                                                                     "lightblue", "lightblue", "lightblue", "purple", "orange", "orange", 
                                                                                                                                     "orange", "teal", "lightblue", "orange", "lightblue"), idcat7 = c(1000170, 
                                                                                                                                                                                                       7000170, 1000130, 7000130, 2000240, 2000281, 1000120, 7000120, 
                                                                                                                                                                                                       3000340, 2000297, 2000304, 6000671, 2000230, 6000611, 6000672, 
                                                                                                                                                                                                       2000302, 6000650, 1000203, 7000192, 1000110, 7000110, 1000202, 
                                                                                                                                                                                                       7000191, 6000660, 6000630, 6000640, 6000702, 6000610, 2000292, 
                                                                                                                                                                                                       2002001, 2000301, 2e+06, 2000291, 2000290, 2000271, 2000220, 
                                                                                                                                                                                                       2000210, 2000299, 1000201, 2000294, 1000204, 7000193, 1000190, 
                                                                                                                                                                                                       7000190, 3000391, 2000305, 2000280, 2000270, 2000260, 2000303, 
                                                                                                                                                                                                       1000180, 7000180, 1000150, 7000150, 2000293, 6000620, 3e+06, 
                                                                                                                                                                                                       3000360, 3000392, 3000310, 3000380, 3000350, 3000330, 3000320, 
                                                                                                                                                                                                       2000250, 1000160, 7000160, 6000674, 6000673, 2000223, 1000140, 
                                                                                                                                                                                                       7000140, 2000222, 2000211, 6000701, 2000298, 3000370, 2000295, 
                                                                                                                                                                                                       1e+06, 6000612, 1000191, 7000194)), row.names = c(NA, 82L), class = "data.frame")

# ~~5.1.2 Create Edges ---------------------------------------------------------------

# dput(edges)

edges <- structure(list(from = c(-40831304L, -40831304L, -40831304L, -40831304L, 
                                 -40831304L, -40831304L, -40831304L, -40831304L, -40831304L, -40831304L, 
                                 -40831304L, -40831304L, -40831304L, -923249231L, -923249231L, 
                                 -923249231L, -923249231L, -923249231L, 2015487688L, 2015487688L, 
                                 2015487688L, 2015487688L, 1970009842L, 1970009842L, -2003605932L, 
                                 -2003605932L, -2003605932L, -397756546L, -397756546L, -397756546L, 
                                 -397756546L, -880689581L, -880689581L, -880689581L, -1348489570L, 
                                 -1908834875L, 2121201565L, 975882099L, 392114958L, -1142430416L, 
                                 1229429034L, -343200140L, -343200140L, -343200140L, -343200140L, 
                                 -343200140L, -343200140L, -343200140L, -343200140L, -343200140L, 
                                 -343200140L, -343200140L, -343200140L, -343200140L, -343200140L, 
                                 -343200140L, -343200140L, -343200140L, -343200140L, -343200140L, 
                                 -343200140L, -343200140L, -343200140L, 492002219L, 492002219L, 
                                 492002219L, -384623412L, -384623412L, -384623412L, -2071905485L, 
                                 -2071905485L, -1607614960L, -1607614960L, 1963275057L, -333796411L, 
                                 260209331L, 260209331L, 2017665772L, 2017665772L, -934483136L, 
                                 -934483136L, -934483136L, 265804694L, 265804694L, -1122338699L, 
                                 -412414323L, -412414323L, 1382076146L, 2012453948L, 2012453948L, 
                                 2012453948L, 1500219763L, -1655853647L, -1655853647L, -1655853647L, 
                                 -1655853647L, -1655853647L, -1655853647L, -1655853647L, -1655853647L, 
                                 -1655853647L, -1655853647L, -1655853647L, -338109497L, -338109497L, 
                                 -417893648L, -417893648L, 1937297096L, 1937297096L, 1937297096L, 
                                 1937297096L, 1937297096L, 154293575L, 1049804537L, -1732064462L, 
                                 -1732064462L, -428468662L, -428468662L, -428468662L, 1984026328L, 
                                 1984026328L, 763696093L, 763696093L, 763696093L, 1035842796L, 
                                 1035842796L, -397720592L, 1969570735L, -502749944L, 2121201565L, 
                                 2121201565L, -923249231L, -923249231L, 2015487688L, 2015487688L, 
                                 2015487688L, -2003605932L, -2003605932L, -2003605932L, 1057131455L, 
                                 1057131455L, 1057131455L, 1057131455L, 1057131455L, 211985999L, 
                                 211985999L, 211985999L, 211985999L, 211985999L, -1381630046L, 
                                 -1381630046L, -1381630046L, -1381630046L, -1381630046L, -338109497L, 
                                 -338109497L, -338109497L, 1937297096L, 1606071174L, 1606071174L, 
                                 -218836211L, -218836211L, -218836211L, -914515536L, -914515536L, 
                                 -914515536L, 1057131455L, 1970009842L, -2003605932L, 975882099L, 
                                 392114958L, -1142430416L, -1142430416L, 1229429034L, 1229429034L, 
                                 623760513L, 623760513L, 623760513L, 623760513L, -1636572429L, 
                                 -1636572429L, -1636572429L, -1636572429L, 392114958L, -1114520580L, 
                                 -1114520580L, -1114520580L, 459896560L, 205446448L, 205446448L, 
                                 205446448L, 101902232L, 101902232L, -1348489570L, -1348489570L, 
                                 -1348489570L, -478428115L, -478428115L, -478428115L, -478428115L, 
                                 -478428115L, -478428115L, -478428115L, 459896560L, -1908834875L
), from_label = c("Web Data", "Web Data", "Web Data", "Web Data", 
                  "Web Data", "Web Data", "Web Data", "Web Data", "Web Data", "Web Data", 
                  "Web Data", "Web Data", "Web Data", "hotels", "hotels", "hotels", 
                  "hotels", "hotels", "booking", "booking", "booking", "booking", 
                  "Airbnb", "Airbnb", "Tripadvisor", "Tripadvisor", "Tripadvisor", 
                  "Skyscanner", "Skyscanner", "Skyscanner", "Skyscanner", "taxi2airport", 
                  "taxi2airport", "taxi2airport", "365tickets", "seatguru", "pochivka", 
                  "Other webpages (booking accommodation)", "hrs", "holydaycheck", 
                  "pincamp", "Multi-Purpose Data", "Multi-Purpose Data", "Multi-Purpose Data", 
                  "Multi-Purpose Data", "Multi-Purpose Data", "Multi-Purpose Data", 
                  "Multi-Purpose Data", "Multi-Purpose Data", "Multi-Purpose Data", 
                  "Multi-Purpose Data", "Multi-Purpose Data", "Multi-Purpose Data", 
                  "Multi-Purpose Data", "Multi-Purpose Data", "Multi-Purpose Data", 
                  "Multi-Purpose Data", "Multi-Purpose Data", "Multi-Purpose Data", 
                  "Multi-Purpose Data", "Multi-Purpose Data", "Multi-Purpose Data", 
                  "Multi-Purpose Data", "NTR Tourist Establishments", "NTR Tourist Establishments", 
                  "NTR Tourist Establishments", "NRT Local Accommodation", "NRT Local Accommodation", 
                  "NRT Local Accommodation", "Credit & Debit Card transactions", 
                  "Credit & Debit Card transactions", "Airport Data", "Airport Data", 
                  "Tax Authority Register of Short-Term Lease Properties", "Register of Properties offered for short-term lease", 
                  "Register of non-categorised accommodation", "Register of non-categorised accommodation", 
                  "Register of Addresses and Buildings", "Register of Addresses and Buildings", 
                  "NBTC-NIPO", "NBTC-NIPO", "NBTC-NIPO", "Other sources", "Other sources", 
                  "Monthly bed tax data inc. overnights", "Financial transaction data", 
                  "Financial transaction data", "Regulatory reporting FIN1-12", 
                  "BTB data", "BTB data", "BTB data", "Register of accommodation establishments", 
                  "Survey Data", "Survey Data", "Survey Data", "Survey Data", "Survey Data", 
                  "Survey Data", "Survey Data", "Survey Data", "Survey Data", "Survey Data", 
                  "Survey Data", "Survey on participation of residents in trips", 
                  "Survey on participation of residents in trips", "Survey on trips made by foreigners", 
                  "Survey on trips made by foreigners", "Survey on tourist accommodation base", 
                  "Survey on tourist accommodation base", "Survey on tourist accommodation base", 
                  "Survey on tourist accommodation base", "Survey on tourist accommodation base", 
                  "Survey on the visits by foreigners", "Survey data on tourist trips", 
                  "Improve tourist accommodation base", "Improve tourist accommodation base", 
                  "Spatial disaggregation of accommodation", "Spatial disaggregation of accommodation", 
                  "Spatial disaggregation of accommodation", "Improve quality of satellite accounts", 
                  "Improve quality of satellite accounts", "Improve quality of tourists expenses data", 
                  "Improve quality of tourists expenses data", "Improve quality of tourists expenses data", 
                  "Flash estimates", "Flash estimates", "Improve quality data on trips", 
                  "Update register of accommodation establishments", "Improve quality of tourists transport data", 
                  "pochivka", "pochivka", "hotels", "hotels", "booking", "booking", 
                  "booking", "Tripadvisor", "Tripadvisor", "Tripadvisor", "Border traffic survey (BI)", 
                  "Border traffic survey (BI)", "Border traffic survey (BI)", "Border traffic survey (BI)", 
                  "Border traffic survey (BI)", "Waste production (ISPRA)", "Waste production (ISPRA)", 
                  "Waste production (ISPRA)", "Waste production (ISPRA)", "Waste production (ISPRA)", 
                  "Water Consumption", "Water Consumption", "Water Consumption", 
                  "Water Consumption", "Water Consumption", "Survey on participation of residents in trips", 
                  "Survey on participation of residents in trips", "Survey on participation of residents in trips", 
                  "Survey on tourist accommodation base", "Survey on rural tourism accommodations", 
                  "Survey on rural tourism accommodations", "Railway, airport and port data", 
                  "Railway, airport and port data", "Railway, airport and port data", 
                  "Survey on museums and similar", "Survey on museums and similar", 
                  "Survey on museums and similar", "Border traffic survey (BI)", 
                  "Airbnb", "Tripadvisor", "Other webpages (booking accommodation)", 
                  "hrs", "holydaycheck", "holydaycheck", "pincamp", "pincamp", 
                  "Mobile phone data", "Mobile phone data", "Mobile phone data", 
                  "Mobile phone data", "Water Demand", "Water Demand", "Water Demand", 
                  "Water Demand", "hrs", "Smart City", "Smart City", "Smart City", 
                  "Land Border Traffic", "Parking Meters Data", "Parking Meters Data", 
                  "Parking Meters Data", "Border Traffic Survey", "Border Traffic Survey", 
                  "365tickets", "365tickets", "365tickets", "nocowanie", "nocowanie", 
                  "nocowanie", "nocowanie", "nocowanie", "nocowanie", "nocowanie", 
                  "Land Border Traffic", "seatguru"), to = c(-923249231L, 2015487688L, 
                                                             1970009842L, -2003605932L, -397756546L, -880689581L, -1348489570L, 
                                                             -1908834875L, 2121201565L, 975882099L, 392114958L, -1142430416L, 
                                                             1229429034L, 1500219763L, -1732064462L, -428468662L, 1035842796L, 
                                                             896763173L, 1500219763L, -1732064462L, -428468662L, -818898040L, 
                                                             -1732064462L, -560658481L, 1984026328L, 763696093L, 1255616799L, 
                                                             -338109497L, -417893648L, 1984026328L, -2017069376L, 1984026328L, 
                                                             763696093L, 1003246733L, -1571167369L, -366601983L, -1481182525L, 
                                                             1500219763L, -2025409250L, -1730926905L, 1817881360L, 492002219L, 
                                                             -384623412L, -2071905485L, -1607614960L, 1963275057L, -333796411L, 
                                                             260209331L, 2017665772L, -934483136L, 459896560L, -1114520580L, 
                                                             205446448L, -1636572429L, 1057131455L, 211985999L, 265804694L, 
                                                             -1122338699L, -412414323L, 1382076146L, 2012453948L, 1500219763L, 
                                                             623760513L, 284547738L, -1083288549L, -1732064462L, 2035259298L, 
                                                             -1083288549L, -1732064462L, 1984026328L, 763696093L, 1984026328L, 
                                                             763696093L, -1732064462L, -1732064462L, -326741965L, -1732064462L, 
                                                             117368370L, -1732064462L, 136949862L, 1984026328L, 763696093L, 
                                                             1984026328L, 763696093L, 1937297096L, 763696093L, -502749944L, 
                                                             1937297096L, -338109497L, -4289225L, -1015690683L, 1969570735L, 
                                                             -218836211L, -914515536L, -338109497L, -417893648L, 1937297096L, 
                                                             101902232L, 154293575L, 1049804537L, -1381630046L, 1606071174L, 
                                                             -914515536L, 1984026328L, 763696093L, 1984026328L, 763696093L, 
                                                             -1732064462L, -428468662L, 763696093L, 1035842796L, 1969570735L, 
                                                             -397720592L, -397720592L, 382194493L, -1848286899L, 382194493L, 
                                                             -1848286899L, 1984026328L, 382194493L, -1848286899L, 382194493L, 
                                                             -1848286899L, -397720592L, 382194493L, 1984026328L, 1984026328L, 
                                                             1984026328L, -397720592L, -1732064462L, 1035842796L, 1984026328L, 
                                                             763696093L, 1984026328L, 763696093L, 1035842796L, -1732064462L, 
                                                             -428468662L, 1035842796L, 1984026328L, 763696093L, 520931725L, 
                                                             1232757658L, -397720592L, -397720592L, -1015690683L, 1232757658L, 
                                                             520931725L, -4289225L, -397720592L, -1015690683L, 1232757658L, 
                                                             520931725L, -4289225L, -397720592L, 1232757658L, -1015690683L, 
                                                             1984026328L, -1732064462L, -428468662L, -397720592L, 520931725L, 
                                                             -1015690683L, -1015690683L, 1232757658L, -4289225L, -1015690683L, 
                                                             1500219763L, 1500219763L, -1732064462L, -1732064462L, 1500219763L, 
                                                             -1732064462L, 1500219763L, -1732064462L, -1015690683L, 1232757658L, 
                                                             -4289225L, 520931725L, -1015690683L, 1232757658L, 520931725L, 
                                                             -4289225L, 1500219763L, 1232757658L, 520931725L, -4289225L, -397720592L, 
                                                             1232757658L, 520931725L, -4289225L, 763696093L, -397720592L, 
                                                             1232757658L, 520931725L, -4289225L, -1732064462L, -428468662L, 
                                                             1500219763L, 1035842796L, 763696093L, 1984026328L, 1381473985L, 
                                                             763696093L, -397720592L), to_label = c("hotels", "booking", "Airbnb", 
                                                                                                    "Tripadvisor", "Skyscanner", "taxi2airport", "365tickets", "seatguru", 
                                                                                                    "pochivka", "Other webpages (booking accommodation)", "hrs", 
                                                                                                    "holydaycheck", "pincamp", "Register of accommodation establishments", 
                                                                                                    "Improve tourist accommodation base", "Spatial disaggregation of accommodation", 
                                                                                                    "Flash estimates", "hotels.com", "Register of accommodation establishments", 
                                                                                                    "Improve tourist accommodation base", "Spatial disaggregation of accommodation", 
                                                                                                    "booking.com", "Improve tourist accommodation base", "Airbnb.com", 
                                                                                                    "Improve quality of satellite accounts", "Improve quality of tourists expenses data", 
                                                                                                    "Tripadvisor.com", "Survey on participation of residents in trips", 
                                                                                                    "Survey on trips made by foreigners", "Improve quality of satellite accounts", 
                                                                                                    "Skyscanner.net", "Improve quality of satellite accounts", "Improve quality of tourists expenses data", 
                                                                                                    "taxi2airport.com", "365tickets.com", "seatguru.com", "pochivka.bg", 
                                                                                                    "Register of accommodation establishments", "hrs.com", "holydaycheck.de", 
                                                                                                    "pincamp.de", "NTR Tourist Establishments", "NRT Local Accommodation", 
                                                                                                    "Credit & Debit Card transactions", "Airport Data", "Tax Authority Register of Short-Term Lease Properties", 
                                                                                                    "Register of Properties offered for short-term lease", "Register of non-categorised accommodation", 
                                                                                                    "Register of Addresses and Buildings", "NBTC-NIPO", "Land Border Traffic", 
                                                                                                    "Smart City", "Parking Meters Data", "Water Demand", "Border traffic survey (BI)", 
                                                                                                    "Waste production (ISPRA)", "Other sources", "Monthly bed tax data inc. overnights", 
                                                                                                    "Financial transaction data", "Regulatory reporting FIN1-12", 
                                                                                                    "BTB data", "Register of accommodation establishments", "Mobile phone data", 
                                                                                                    "Turismo de Portugal Tourist Establishments Open Data", "travelBI by Turismo de Portugal", 
                                                                                                    "Improve tourist accommodation base", "Turismo de Portugal Local Accommodation Open Data", 
                                                                                                    "travelBI by Turismo de Portugal", "Improve tourist accommodation base", 
                                                                                                    "Improve quality of satellite accounts", "Improve quality of tourists expenses data", 
                                                                                                    "Improve quality of satellite accounts", "Improve quality of tourists expenses data", 
                                                                                                    "Improve tourist accommodation base", "Improve tourist accommodation base", 
                                                                                                    "Netherlands Chamber of Commerce", "Improve tourist accommodation base", 
                                                                                                    "BAG Viewer", "Improve tourist accommodation base", "NBTC Dashboard", 
                                                                                                    "Improve quality of satellite accounts", "Improve quality of tourists expenses data", 
                                                                                                    "Improve quality of satellite accounts", "Improve quality of tourists expenses data", 
                                                                                                    "Survey on tourist accommodation base", "Improve quality of tourists expenses data", 
                                                                                                    "Improve quality of tourists transport data", "Survey on tourist accommodation base", 
                                                                                                    "Survey on participation of residents in trips", "Tourism potential", 
                                                                                                    "Tourism attractiveness", "Update register of accommodation establishments", 
                                                                                                    "Railway, airport and port data", "Survey on museums and similar", 
                                                                                                    "Survey on participation of residents in trips", "Survey on trips made by foreigners", 
                                                                                                    "Survey on tourist accommodation base", "Border Traffic Survey", 
                                                                                                    "Survey on the visits by foreigners", "Survey data on tourist trips", 
                                                                                                    "Water Consumption", "Survey on rural tourism accommodations", 
                                                                                                    "Survey on museums and similar", "Improve quality of satellite accounts", 
                                                                                                    "Improve quality of tourists expenses data", "Improve quality of satellite accounts", 
                                                                                                    "Improve quality of tourists expenses data", "Improve tourist accommodation base", 
                                                                                                    "Spatial disaggregation of accommodation", "Improve quality of tourists expenses data", 
                                                                                                    "Flash estimates", "Update register of accommodation establishments", 
                                                                                                    "Improve quality data on trips", "Improve quality data on trips", 
                                                                                                    "Eurostat Experimental Statistics", "WPJ Milestones and Deliverables", 
                                                                                                    "Eurostat Experimental Statistics", "WPJ Milestones and Deliverables", 
                                                                                                    "Improve quality of satellite accounts", "Eurostat Experimental Statistics", 
                                                                                                    "WPJ Milestones and Deliverables", "Eurostat Experimental Statistics", 
                                                                                                    "WPJ Milestones and Deliverables", "Improve quality data on trips", 
                                                                                                    "Eurostat Experimental Statistics", "Improve quality of satellite accounts", 
                                                                                                    "Improve quality of satellite accounts", "Improve quality of satellite accounts", 
                                                                                                    "Improve quality data on trips", "Improve tourist accommodation base", 
                                                                                                    "Flash estimates", "Improve quality of satellite accounts", "Improve quality of tourists expenses data", 
                                                                                                    "Improve quality of satellite accounts", "Improve quality of tourists expenses data", 
                                                                                                    "Flash estimates", "Improve tourist accommodation base", "Spatial disaggregation of accommodation", 
                                                                                                    "Flash estimates", "Improve quality of satellite accounts", "Improve quality of tourists expenses data", 
                                                                                                    "Event related tourism", "City Tourism", "Improve quality data on trips", 
                                                                                                    "Improve quality data on trips", "Tourism attractiveness", "City Tourism", 
                                                                                                    "Event related tourism", "Tourism potential", "Improve quality data on trips", 
                                                                                                    "Tourism attractiveness", "City Tourism", "Event related tourism", 
                                                                                                    "Tourism potential", "Improve quality data on trips", "City Tourism", 
                                                                                                    "Tourism attractiveness", "Improve quality of satellite accounts", 
                                                                                                    "Improve tourist accommodation base", "Spatial disaggregation of accommodation", 
                                                                                                    "Improve quality data on trips", "Event related tourism", "Tourism attractiveness", 
                                                                                                    "Tourism attractiveness", "City Tourism", "Tourism potential", 
                                                                                                    "Tourism attractiveness", "Register of accommodation establishments", 
                                                                                                    "Register of accommodation establishments", "Improve tourist accommodation base", 
                                                                                                    "Improve tourist accommodation base", "Register of accommodation establishments", 
                                                                                                    "Improve tourist accommodation base", "Register of accommodation establishments", 
                                                                                                    "Improve tourist accommodation base", "Tourism attractiveness", 
                                                                                                    "City Tourism", "Tourism potential", "Event related tourism", 
                                                                                                    "Tourism attractiveness", "City Tourism", "Event related tourism", 
                                                                                                    "Tourism potential", "Register of accommodation establishments", 
                                                                                                    "City Tourism", "Event related tourism", "Tourism potential", 
                                                                                                    "Improve quality data on trips", "City Tourism", "Event related tourism", 
                                                                                                    "Tourism potential", "Improve quality of tourists expenses data", 
                                                                                                    "Improve quality data on trips", "City Tourism", "Event related tourism", 
                                                                                                    "Tourism potential", "Improve tourist accommodation base", "Spatial disaggregation of accommodation", 
                                                                                                    "Register of accommodation establishments", "Flash estimates", 
                                                                                                    "Improve quality of tourists expenses data", "Improve quality of satellite accounts", 
                                                                                                    "nocowanie.pl", "Improve quality of tourists expenses data", 
                                                                                                    "Improve quality data on trips"), length = c(250, 250, 250, 250, 
                                                                                                                                                 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                 5, 250, 250, 250, 5, 250, 5, 250, 250, 5, 250, 250, 250, 5, 250, 
                                                                                                                                                 250, 5, 5, 5, 5, 250, 5, 5, 5, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                 250, 250, 250, 5, 5, 250, 5, 5, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                 250, 5, 250, 5, 250, 5, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                 5, 5, 5, 5, 250, 5, 5, 5, 5, 250, 5, 250, 250, 250, 250, 250, 
                                                                                                                                                 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                 250, 250, 250, 250, 250, 250, 250, 5, 250, 250), width = c(1, 
                                                                                                                                                                                                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), label = c(NA, NA, 
                                                                                                                                                                                                                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Linking data on tourist  accommodations to frame for statistical survey", 
                                                                                                                                                                                                                                                                    "Removing duplicates and checking plausability of estimates using QGIS [NL]", 
                                                                                                                                                                                                                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Exception: [IT]", 
                                                                                                                                                                                                                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), 
arrows = c("to", "to", "to", "to", "to", "to", "to", "to", 
           "to", "to", "to", "to", "to", "to", "middle;to", "middle;to", 
           "to", "FALSE", "to", "to", "to", "FALSE", "to", "FALSE", 
           "to", "to", "FALSE", "to", "to", "to", "FALSE", "to", "to", 
           "FALSE", "FALSE", "FALSE", "FALSE", "to", "FALSE", "FALSE", 
           "FALSE", "to", "to", "to", "to", "to", "to", "to", "to", 
           "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
           "to", "to", "to", "to", "FALSE", "FALSE", "to", "FALSE", 
           "FALSE", "to", "to", "to", "to", "to", "to", "to", "FALSE", 
           "to", "FALSE", "to", "FALSE", "to", "to", "to", "to", "to", 
           "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
           "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
           "to", "to", "to", "to", "to", "to", "to", "to", "to", "FALSE", 
           "FALSE", "FALSE", "FALSE", "to", "FALSE", "FALSE", "FALSE", 
           "FALSE", "to", "FALSE", "to", "to", "to", "to", "to", "to", 
           "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
           "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
           "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
           "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
           "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
           "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
           "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
           "to", "FALSE", "to", "to"), from_idcat7 = c(1e+06, 1e+06, 
                                                       1e+06, 1e+06, 1e+06, 1e+06, 1e+06, 1e+06, 1e+06, 1e+06, 1e+06, 
                                                       1e+06, 1e+06, 1000110, 1000110, 1000110, 1000110, 1000110, 
                                                       1000120, 1000120, 1000120, 1000120, 1000130, 1000130, 1000140, 
                                                       1000140, 1000140, 1000150, 1000150, 1000150, 1000150, 1000160, 
                                                       1000160, 1000160, 1000170, 1000180, 1000190, 1000201, 1000202, 
                                                       1000203, 1000204, 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 
                                                       2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 
                                                       2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2000210, 
                                                       2000210, 2000210, 2000220, 2000220, 2000220, 2000230, 2000230, 
                                                       2000240, 2000240, 2000250, 2000260, 2000270, 2000270, 2000280, 
                                                       2000280, 2000290, 2000290, 2000290, 2000299, 2000299, 2000301, 
                                                       2000302, 2000302, 2000303, 2000304, 2000304, 2000304, 2000305, 
                                                       3e+06, 3e+06, 3e+06, 3e+06, 3e+06, 3e+06, 3e+06, 3e+06, 3e+06, 
                                                       3e+06, 3e+06, 3000310, 3000310, 3000320, 3000320, 3000330, 
                                                       3000330, 3000330, 3000330, 3000330, 3000350, 3000360, 6000610, 
                                                       6000610, 6000620, 6000620, 6000620, 6000630, 6000630, 6000640, 
                                                       6000640, 6000640, 6000650, 6000650, 6000660, 6000701, 6000702, 
                                                       1000190, 1000190, 1000110, 1000110, 1000120, 1000120, 1000120, 
                                                       1000140, 1000140, 1000140, 2000297, 2000297, 2000297, 2000297, 
                                                       2000297, 2000298, 2000298, 2000298, 2000298, 2000298, 3000370, 
                                                       3000370, 3000370, 3000370, 3000370, 3000310, 3000310, 3000310, 
                                                       3000330, 3000380, 3000380, 3000391, 3000391, 3000391, 3000392, 
                                                       3000392, 3000392, 2000297, 1000130, 1000140, 1000201, 1000202, 
                                                       1000203, 1000203, 1000204, 1000204, 2002001, 2002001, 2002001, 
                                                       2002001, 2000295, 2000295, 2000295, 2000295, 1000202, 2000293, 
                                                       2000293, 2000293, 2000292, 2000294, 2000294, 2000294, 3000340, 
                                                       3000340, 1000170, 1000170, 1000170, 1000191, 1000191, 1000191, 
                                                       1000191, 1000191, 1000191, 1000191, 2000292, 1000180), to_idcat7 = c(1000110, 
                                                                                                                            1000120, 1000130, 1000140, 1000150, 1000160, 1000170, 1000180, 
                                                                                                                            1000190, 1000201, 1000202, 1000203, 1000204, 2000305, 6000610, 
                                                                                                                            6000620, 6000650, 7000110, 2000305, 6000610, 6000620, 7000120, 
                                                                                                                            6000610, 7000130, 6000630, 6000640, 7000140, 3000310, 3000320, 
                                                                                                                            6000630, 7000150, 6000630, 6000640, 7000160, 7000170, 7000180, 
                                                                                                                            7000190, 2000305, 7000191, 7000192, 7000193, 2000210, 2000220, 
                                                                                                                            2000230, 2000240, 2000250, 2000260, 2000270, 2000280, 2000290, 
                                                                                                                            2000292, 2000293, 2000294, 2000295, 2000297, 2000298, 2000299, 
                                                                                                                            2000301, 2000302, 2000303, 2000304, 2000305, 2002001, 2000211, 
                                                                                                                            2000223, 6000610, 2000222, 2000223, 6000610, 6000630, 6000640, 
                                                                                                                            6000630, 6000640, 6000610, 6000610, 2000271, 6000610, 2000281, 
                                                                                                                            6000610, 2000291, 6000630, 6000640, 6000630, 6000640, 3000330, 
                                                                                                                            6000640, 6000702, 3000330, 3000310, 6000673, 6000674, 6000701, 
                                                                                                                            3000391, 3000392, 3000310, 3000320, 3000330, 3000340, 3000350, 
                                                                                                                            3000360, 3000370, 3000380, 3000392, 6000630, 6000640, 6000630, 
                                                                                                                            6000640, 6000610, 6000620, 6000640, 6000650, 6000701, 6000660, 
                                                                                                                            6000660, 6000611, 6000612, 6000611, 6000612, 6000630, 6000611, 
                                                                                                                            6000612, 6000611, 6000612, 6000660, 6000611, 6000630, 6000630, 
                                                                                                                            6000630, 6000660, 6000610, 6000650, 6000630, 6000640, 6000630, 
                                                                                                                            6000640, 6000650, 6000610, 6000620, 6000650, 6000630, 6000640, 
                                                                                                                            6000672, 6000671, 6000660, 6000660, 6000674, 6000671, 6000672, 
                                                                                                                            6000673, 6000660, 6000674, 6000671, 6000672, 6000673, 6000660, 
                                                                                                                            6000671, 6000674, 6000630, 6000610, 6000620, 6000660, 6000672, 
                                                                                                                            6000674, 6000674, 6000671, 6000673, 6000674, 2000305, 2000305, 
                                                                                                                            6000610, 6000610, 2000305, 6000610, 2000305, 6000610, 6000674, 
                                                                                                                            6000671, 6000673, 6000672, 6000674, 6000671, 6000672, 6000673, 
                                                                                                                            2000305, 6000671, 6000672, 6000673, 6000660, 6000671, 6000672, 
                                                                                                                            6000673, 6000640, 6000660, 6000671, 6000672, 6000673, 6000610, 
                                                                                                                            6000620, 2000305, 6000650, 6000640, 6000630, 7000194, 6000640, 
                                                                                                                            6000660)), row.names = c(NA, -205L), class = "data.frame")



#6. Create VisNetwork object ------------------------------------------------
# Create VisNetwork object 
# "height" and "width" are optional and the default is automatic. 
# "height" can be customized in pixels or as a percentage 
# according your display resolution.
# "main" and "submain" define the title and subtitle.

visNet_Workflow_uRos2021 <- visNetwork(nodes, edges,
                                       height = "800px",
                                       width = "90%",
                                       main="Overview of the Inputs and Outputs of the Pilot Project on Innovative Tourism Statistics", 
                                       submain="[BG] [DE] [EL] [IT] [NL] [PL] [PT] [SK]") %>% 
        
# ~~6.1 Create Legend  ------------------------------------------------
# Create a user defined VisNetwork Legend 


visLegend(useGroups = FALSE, 
          width = 0.05,
          position = "right",
          zoom = F,
          addNodes = data.frame(
                  label = c("Data Source\n Type", "Data Sources", "Experimental\n Results", "External links"),
                  shape = c("dot", "dot", "square", "triangle"),
                  color = c("teal", "orange", "purple", "lightblue"))) %>% 
        
# ~~6.2 Interaction ----------------------------------------------
# Added functionalities for user interaction: navigations buttons and multi selection.
# Green navigation buttons are placed on the bottom left and bottom right of the network canvas.
# These will help to navigate, zoom in and out and re-center.
# Multi-selection: a long click as well as a control-click will add to the selection. 

visInteraction(navigationButtons = TRUE, multiselect = T) %>% 
        
# ~~6.3 Export Current Network Canvas to PNG -------------------------------------------
# A small button with "Export to PNG" text will be placed on the bottom right corner 
# of the network canvas. On click, a PNG file (the default) will be exported to downloads folder. 
# Can be configured to "jpeg" or "pdf"

visExport(type = "png") %>%                                                                 
        

# ~~6.4 Default Format for Nodes -----------------------------------------
# Nodes will get a dot shape by default if none is provided in 
# nodes$shape.  

visNodes(shape = "dot",
         color = list(
                 highlight = "#FF8000"),
         shadow = list(enabled = TRUE, size = 10),
         labelHighlightBold = T) %>%
        
# ~~6.5 Default Format for Edges -----------------------------------------

visEdges(shadow = FALSE,
         color = list(color = "#0085AF", highlight = "#C62F4B"), 
         # smooth = list(enabled = F), # to generate straight lines
         arrows = 'to') %>%
        
# ~~6.6 General Options (combo box)----------------------------------------------------------
# Creates two combo boxes for selection of nodes. One based on nodes id label (nodes$label)
# and another based on  multiple groups per nodes using a comma as it is the case 
# of nodes$group

visOptions(highlightNearest = list(enabled = T, degree = 10, hover = T, algorithm = "hierarchical"),
           manipulation = TRUE,
           
           # First combo box "Select by id"
           # Defined by nodesIdSelection can be "values : Optional. Vector of possible values (node's id), 
           # and so order is preserved. Default to all id in nodes data.frame.        
           
           nodesIdSelection = list(enabled = T, main = "Select by node"),

           # Network will automatically detect when its container is resized, and redraw itself accordingly
           autoResize = TRUE,
           
           # Second combo box "Select by group"
           # Defined by selectedBy, uses multiple "categories" separated by a comma in nodes$group 
           
           selectedBy = list(variable = "group", multiple = TRUE)) %>%

        
# ~~6.7 Layout Options ----------------------------------------------------------
# When not using hierarchical layout, providing random seed manually
# will produce the same layout every time.

visLayout(randomSeed = 11, improvedLayout = T, hierarchical = F) %>%            


#~~6.8 Physics Options-------------------------------------------------- 
# Configuration of the physics system governing the simulation 
# of the nodes and edges. 
# BarnesHut is the recommended solver for non-hierarchical layout. 
# The remaining parameters were fine tuned for this particular network.


visPhysics(maxVelocity = 5,
           solver = "barnesHut",
           barnesHut = list(avoidOverlap = 0.15,
                            gravitationalConstant = -1500,
                            springConstant = 0.01),
           repulsion = list(centralGravity = 1.5))


#7. Show result on RStudio viewer-----------------------------------------------------------
visNet_Workflow_uRos2021

#8.Save visNetwork to HTML File -----------------------------------------------------------
# Save the visNetwork object to a single self-contained HTML file for sharing.
visSave(visNet_Workflow_uRos2021, file = "visNet_Workflow_uRos2021.html")

# ================ |-------------| ================

