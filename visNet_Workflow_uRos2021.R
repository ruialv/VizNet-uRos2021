
##========================================================##
##                                                        ##
##   Network Visualization with R                         ##
##   ESSnet Big Data II WPJ - Task 1C                     ##
##                                                        ##
##   Rui Alves                                            ##
##   Statistics Portugal                                  ##
##   Email: rui.alves@ine.pt                              ##
##                                                        ##
##========================================================##


# Introduction -------------------------------------------------------------
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


# About Input Data-------------------------------------------------------------
# (...)

# About the Code-------------------------------------------------------------
# The function set_wd() sets the working directory to the folder you are now working from. So we don’t need the setwd("paste here the absolute path"). Package rstudioapi is required.
# For more details: https://eranraviv.com/r-tips-and-tricks-working-directory/
# Added an option in "Default Format  for Edges" section (presently commented) to use # straight lines instead of smooth ones: # smooth = list(enabled = F), # to generate straight lines
# On "Create VisNetwork object " section, height was changed from 800px to 1000px 
# in order to accommodate a larger visNetwork
# Legend corrected to match data:  Expected Results > Experimental Results
# hoovering over a node now highlightNearest up to a degree = 2
# Other minor changes

# Load Packages -----------------------------------------------------------


# Create required_packages function ---------------------------------------------------------
# source: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them/44660688#44660688


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



# Create set_wd function --------------------------------------------------
set_wd <- function() {
        # library(rstudioapi) # make sure you have it installed
        current_path <- getActiveDocumentContext()$path 
        setwd(dirname(current_path ))
        print( getwd() )
}

set_wd()

# Load Data ---------------------------------------------------------------
# Run the code bellow in order to recreate the data as dataframes in your current session

# ~~1. Load CSV Files ---------------------------------------------------------------
# (...)

# ~~2. Run Code to create Dataframes ---------------------------------------------------------------

# ~~2.1 Create Nodes ---------------------------------------------------------------
nodes <- structure(list(id = c(1000170, 7000170, 1000130, 7000130, 2000240, 
                               2000281, 1000120, 7000120, 3000340, 2000297, 2000304, 6000671, 
                               2000230, 6000611, 6000672, 2000302, 6000650, 1000203, 7000192, 
                               1000110, 7000110, 1000202, 7000191, 6000660, 6000630, 6000640, 
                               6000702, 6000610, 2000292, 2002001, 2000301, 2e+06, 2000291, 
                               2000290, 2000271, 2000220, 2000210, 2000299, 1000201, 2000294, 
                               1000204, 7000193, 1000190, 7000190, 3000391, 2000305, 2000280, 
                               2000270, 2000260, 2000303, 1000180, 7000180, 1000150, 7000150, 
                               2000293, 6000620, 3e+06, 3000360, 3000392, 3000310, 3000380, 
                               3000350, 3000330, 3000320, 2000250, 1000160, 7000160, 6000674, 
                               6000673, 2000223, 1000140, 7000140, 2000222, 2000211, 6000701, 
                               2000298, 3000370, 2000295, 1, 6000612, 1000191, 7000194), label = c("365tickets", 
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
                               ), title = c("id:1000170 <br> <br>  365tickets.com <br> [PL]", 
                                            "id:7000170 <br> <br>  https://www.365tickets.com <br> [PL]", 
                                            "id:1000130 <br> <br>  airbnb.com <br> [EL] [NL]", "id:7000130 <br> <br>  https://www.airbnb.com <br> [EL] [NL]", 
                                            "id:2000240 <br> <br>  Airport Data <br> and  <br> Other sources  <br>  (Locatus, Airports register) <br>[BG] [NL]", 
                                            "id:2000281 <br> <br>  bagviewer.kadaster.nl <br> [NL]", "id:1000120 <br> <br>  booking.com <br> [DE] [BG] [IT] [NL] [PL] [SK]", 
                                            "id:7000120 <br> <br>  https://www.booking.com <br> [BG] [DE] [IT] [NL] [PL] [SK]", 
                                            "id:3000340 <br> <br>  Border <br> Traffic Survey <br> [PL]", 
                                            "id:2000297 <br> <br>  Border traffic survey <br> (Bank of Italy) <br> [IT]", 
                                            "id:2000304 <br> <br>  BTB data (unique wifi connection, etc.) <br> [SK]", 
                                            "id:6000671 <br> <br>  City Tourism <br>  [DE] [IT] [PL] [SK]", 
                                            "id:2000230 <br> <br>  Credit & Debit <br> card transactions  <br> [PT]", 
                                            "id:6000611 <br> <br>  https://ec.europa.eu/eurostat/web/experimental-statistics", 
                                            "id:6000672 <br> <br>  Event related tourism <br> [DE] [IT] [PL]", 
                                            "id:2000302 <br> <br>  Financial transaction data <br> [SK]", 
                                            "id:6000650 <br> <br>  Flash estimates <br> of the use of  <br> tourist accommodation base <br> [BG] [DE] [EL] [IT] [PL] [SK]", 
                                            "id:1000203 <br> <br>  holidaycheck.de <br> [DE]", "id:7000192 <br> <br>  https://www.holidaycheck.de/ <br> [DE]", 
                                            "id:1000110 <br> <br>  hotels.com <br> [DE] [BG] [EL] [IT] [NL] [PL] [PT] [SK]", 
                                            "id:7000110 <br> <br>  https://www.hotels.com <br> [BG] [DE] [EL] [IT] [NL] [PL] [PT] [SK]", 
                                            "id:1000202 <br> <br>  hrs.de <br> [DE]", "id:7000191 <br> <br>  https://www.hrs.de/hotel/ <br> [DE]", 
                                            "id:6000660 <br> <br>  Improve quality data <br> on trips <br> [BG] [DE] [IT] [PL] [SK]", 
                                            "id:6000630 <br> <br>  Improve quality <br> of satellite <br> accounts <br> [BG] [DE]  [IT] [NL] [PL] [PT] [SK]", 
                                            "id:6000640 <br> <br>  Improve quality <br>  of tourists expenses <br> data <br>  [DE] [IT] [NL] [PT] [PL] [SK]", 
                                            "id:6000702 <br> <br>  Improve quality of <br> tourists transport data <br> [SK]", 
                                            "id:6000610 <br> <br>  Improve tourist <br> accommodation <br> base of reference <br> [BG] [DE] [EL] [IT] [NL] [PL] [PT] [SK]", 
                                            "id:2000292 <br> <br>  Land Border Traffic <br> Traffic Sensors, <br> Border Guard Data <br> Automatic Number Plate Recognition System <br> [PL]", 
                                            "id:2002001 <br> <br>  Anonymized mobile <br> phone data (pilot): <br> no of guests (overnight), <br> daytime tourists <br> [DE]", 
                                            "id:2000301 <br> <br>  Monthly bed tax data including the overnights data in the cities Bratislava and Košice <br> [SK]", 
                                            "id:2000000 <br> <br>  Multi-Purpose  <br> Data", "id:2000291 <br> <br>  https://dashboard.nbtc.nl/jive/jivereportcontents.ashx?report=home_new <br> [NL]", 
                                            "id:2000290 <br> <br>  NBTC-NIPO <br> Research company <br> on holiday, <br> recreation and <br> business travel <br> [NL]", 
                                            "id:2000271 <br> <br>  kvk.nl <br> [NL]", "id:2000220 <br> <br>  National Tourist Registration <br> Local Accommodation <br> [PT]", 
                                            "id:2000210 <br> <br>  National Tourist Registration <br>Tourist Establishments <br> [PT]", 
                                            "id:2000299 <br> <br>  Other sources <br> (Terrace Research, <br> Museum and recreation statistics)<br> [DE] [NL] [PL]", 
                                            "id:1000201 <br> <br>  Other national webpages <br> for booking of accommodation <br> [SK]", 
                                            "id:2000294 <br> <br>  Parking Meters Data <br> [PL]", "id:1000204 <br> <br>  pincamp.de <br> [DE]", 
                                            "id:7000193 <br> <br>  https://www.pincamp.de/ <br> [DE]", "id:1000190 <br> <br>  pochivka.bg <br> [BG]", 
                                            "id:7000190 <br> <br>  pochivka.bg <br> [BG]", "id:3000391 <br> <br>  Railway, airport <br> and port data <br> [DE] [IT]", 
                                            "id:2000305 <br> <br>  Register of  <br> accommodation establishments <br> [SK]", 
                                            "id:2000280 <br> <br>  Register of <br> Addresses and <br> Buildings (BAG 2019)<br> [NL]", 
                                            "id:2000270 <br> <br>  Register of <br> non-categorised tourist <br> accommodation establishments <br> (CoC*) <br> [NL] [PL]", 
                                            "id:2000260 <br> <br>  Register of Properties <br> offered for  <br> short-term lease <br>  through digital platforms <br> [EL]", 
                                            "id:2000303 <br> <br>  Regulatory reporting FIN1-12 <br> [SK]", 
                                            "id:1000180 <br> <br>  seatguru.com <br> [PL]", "id:7000180 <br> <br>  https://www.seatguru.com <br> [PL]", 
                                            "id:1000150 <br> <br>  skyscanner.net <br> [NL] [PL] [SK]", "id:7000150 <br> <br>  https://www.skyscanner.net <br> [NL] [PL] [SK]", 
                                            "id:2000293 <br> <br>  Smart City <br> [PL]", "id:6000620 <br> <br>  Spatial disaggregation <br> of data on tourist <br> accommodation base  <br>  [DE] [EL] [IT] [NL] [PL] [SK]", 
                                            "id:3000000 <br> <br>  Survey <br> Data", "id:3000360 <br> <br>  Survey data on tourist trips <br> of Bulgarian residents <br> in the country and\r\n\r\nabroad <br> and the expenditure <br> [BG]", 
                                            "id:3000392 <br> <br>  Survey on museums <br> and similar institutions <br> [DE] [IT]", 
                                            "id:3000310 <br> <br>  Survey on <br> participation of <br> residents in trips <br> [BG] [IT:HBS] [NL: CVO] [PL] [PT: IDR] [SK]", 
                                            "id:3000380 <br> <br>  Survey on <br> rural tourism accommodations <br> [IT]", 
                                            "id:3000350 <br> <br>  Survey data on the visits by foreigners in Bulgaria <br> [BG]", 
                                            "id:3000330 <br> <br>  Survey on <br> tourist <br> accommodation base <br> [BG] [DE] [EL] [IT] [PL] [SK]", 
                                            "id:3000320 <br> <br>  Survey on <br> trips made <br> by foreigners <br> [NL] [PL] [SK]", 
                                            "id:2000250 <br> <br>  Tax Authority Register of Short-Term Lease Properties  <br> [EL]", 
                                            "id:1000160 <br> <br>  Price of taxis <br> from / to <br> the airport <br> taxi2airport.com <br> [NL]", 
                                            "id:7000160 <br> <br>   https://www.taxi2airport.com <br> [NL]", 
                                            "id:6000674 <br> <br>  Tourism attractiveness <br>  [DE] [IT] [PL] [SK]", 
                                            "id:6000673 <br> <br>  Tourism potential <br>  [DE] [IT] [PL] [SK]", 
                                            "id:2000223 <br> <br>  Turismo de Portugal: <br> TravelBI <br> https://travelbi.turismodeportugal.pt/en-us/Pages/Home.aspx <br> [PT]", 
                                            "id:1000140 <br> <br>  tripadvisor.com <br> [DE] [IT] [NL]", 
                                            "id:7000140 <br> <br>  https://www.tripadvisor.com <br> [DE] [IT] [NL]", 
                                            "id:2000222 <br> <br>  Turismo de Portugal Open Data: <br>Local Accommodation <br> https://dadosabertos.turismodeportugal.pt/datasets/estabelecimentos-de-al <br> [PT]", 
                                            "id:2000211 <br> <br>  Turismo de Portugal Open Data: <br> Tourist Establishment <br> https://dadosabertos.turismodeportugal.pt/datasets/empreendimentos-turisticos-existentes?geometry=-91.485%2C33.243%2C75.771%2C54.949 <br> [PT]", 
                                            "id:6000701 <br> <br>  Update register of <br> accommodation establishments <br> [SK]", 
                                            "id:2000298 <br> <br>  Waste production <br> (ISPRA: National Institute <br> for Environmental Protection) <br> [DE] [IT]", 
                                            "id:3000370 <br> <br>  Water Consumption <br> [DE] [IT] [PL]", 
                                            "id:2000295 <br> <br>  Water Demand <br> [DE] [PL]", "id:1 <br> <br>  Web <br> Scraped <br> Data", 
                                            "id:6000612 <br> <br>  https://webgate.ec.europa.eu/fpfis/mwikis/essnetbigdata/index.php/WPJ_Milestones_and_deliverables.", 
                                            "id:1000191 <br> <br>  nocowanie.pl <br> [PL]", "id:7000194<br> <br>  https://www.nocowanie.pl <br> [PL]"
                               ), title_old = c("365tickets.com <br> [PL]", "https://www.365tickets.com <br> [PL]", 
                                                "airbnb.com <br> [EL] [NL]", "https://www.airbnb.com <br> [EL] [NL]", 
                                                "Airport Data <br> and  <br> Other sources  <br>  (Locatus, Airports register) <br>[BG] [NL]", 
                                                "bagviewer.kadaster.nl <br> [NL]", "booking.com <br> [DE] [BG] [IT] [NL] [PL] [SK]", 
                                                "https://www.booking.com <br> [BG] [DE] [IT] [NL] [PL] [SK]", 
                                                "Border <br> Traffic Survey <br> [PL]", "Border traffic survey <br> (Bank of Italy) <br> [IT]", 
                                                "BTB data (unique wifi connection, etc.) <br> [SK]", "City Tourism <br>  [DE] [IT] [PL] [SK]", 
                                                "Credit & Debit <br> card transactions  <br> [PT]", "https://ec.europa.eu/eurostat/web/experimental-statistics", 
                                                "Event related tourism <br> [DE] [IT] [PL]", "Financial transaction data <br> [SK]", 
                                                "Flash estimates <br> of the use of  <br> tourist accommodation base <br> [BG] [DE] [EL] [IT] [PL] [SK]", 
                                                "holidaycheck.de <br> [DE]", "https://www.holidaycheck.de/ <br> [DE]", 
                                                "hotels.com <br> [DE] [BG] [EL] [IT] [NL] [PL] [PT] [SK]", "https://www.hotels.com <br> [BG] [DE] [EL] [IT] [NL] [PL] [PT] [SK]", 
                                                "hrs.de <br> [DE]", "https://www.hrs.de/hotel/ <br> [DE]", "Improve quality data <br> on trips <br> [BG] [DE] [IT] [PL] [SK]", 
                                                "Improve quality <br> of satellite <br> accounts <br> [BG] [DE]  [IT] [NL] [PL] [PT] [SK]", 
                                                "Improve quality <br>  of tourists expenses <br> data <br>  [DE] [IT] [NL] [PT] [PL] [SK]", 
                                                "Improve quality of <br> tourists transport data <br> [SK]", 
                                                "Improve tourist <br> accommodation <br> base of reference <br> [BG] [DE] [EL] [IT] [NL] [PL] [PT] [SK]", 
                                                "Land Border Traffic <br> Traffic Sensors, <br> Border Guard Data <br> Automatic Number Plate Recognition System <br> [PL]", 
                                                "Anonymized mobile <br> phone data (pilot): <br> no of guests (overnight), <br> daytime tourists <br> [DE]", 
                                                "Monthly bed tax data including the overnights data in the cities Bratislava and Košice <br> [SK]", 
                                                "Multi-Purpose  <br> Data", "https://dashboard.nbtc.nl/jive/jivereportcontents.ashx?report=home_new <br> [NL]", 
                                                "NBTC-NIPO <br> Research company <br> on holiday, <br> recreation and <br> business travel <br> [NL]", 
                                                "kvk.nl <br> [NL]", "National Tourist Registration <br> Local Accommodation <br> [PT]", 
                                                "National Tourist Registration <br>Tourist Establishments <br> [PT]", 
                                                "Other sources <br> (Terrace Research, <br> Museum and recreation statistics)<br> [DE] [NL] [PL]", 
                                                "Other national webpages <br> for booking of accommodation <br> [SK]", 
                                                "Parking Meters Data <br> [PL]", "pincamp.de <br> [DE]", "https://www.pincamp.de/ <br> [DE]", 
                                                "pochivka.bg <br> [BG]", "pochivka.bg <br> [BG]", "Railway, airport <br> and port data <br> [DE] [IT]", 
                                                "Register of  <br> accommodation establishments <br> [PL] [SK]", 
                                                "Register of <br> Addresses and <br> Buildings (BAG 2019)<br> [NL]", 
                                                "Register of <br> non-categorised tourist <br> accommodation establishments <br> (CoC*) <br> [DE] [NL] [PL]", 
                                                "Register of Properties <br> offered for  <br> short-term lease <br>  through digital platforms <br> [EL]", 
                                                "Regulatory reporting FIN1-12 <br> [SK]", "seatguru.com <br> [PL]", 
                                                "https://www.seatguru.com <br> [PL]", "skyscanner.net <br> [NL] [PL] [SK]", 
                                                "https://www.skyscanner.net <br> [NL] [PL] [SK]", "Smart City <br> [PL]", 
                                                "Spatial disaggregation <br> of data on tourist <br> accommodation base  <br>  [DE] [EL] [IT] [NL] [PL] [SK]", 
                                                "Survey <br> Data", "Survey data on tourist trips <br> of Bulgarian residents <br> in the country and\r\n\r\nabroad <br> and the expenditure <br> [BG]", 
                                                "Survey on museums <br> and similar institutions <br> [IT]", 
                                                "Survey on <br> participation of <br> residents in trips <br> [BG] [IT:HBS] [NL: CVO] [PL] [PT: IDR] [SK]", 
                                                "Survey on <br> rural tourism accommodations <br> [IT]", "Survey data on the visits by foreigners in Bulgaria <br> [BG]", 
                                                "Survey on <br> tourist <br> accommodation base <br> [BG] [DE] [EL] [IT] [PL] [SK]", 
                                                "Survey on <br> trips made <br> by foreigners <br> [NL] [PL] [SK]", 
                                                "Tax Authority Register of Short-Term Lease Properties  <br> [EL]", 
                                                "Price of taxis <br> from / to <br> the airport <br> taxi2airport.com <br> [NL]", 
                                                "https://www.taxi2airport.com <br> [NL]", "Tourism attractiveness <br>  [DE] [IT] [PL] [SK]", 
                                                "Tourism potential <br>  [DE] [IT] [PL] [SK]", "Turismo de Portugal: <br> TravelBI <br> https://travelbi.turismodeportugal.pt/en-us/Pages/Home.aspx <br> [PT]", 
                                                "tripadvisor.com <br> [DE] [IT] [NL]", "https://www.tripadvisor.com <br> [DE] [IT] [NL]", 
                                                "Turismo de Portugal Open Data: <br>Local Accommodation <br> https://dadosabertos.turismodeportugal.pt/datasets/estabelecimentos-de-al <br> [PT]", 
                                                "Turismo de Portugal Open Data: <br> Tourist Establishment <br> https://dadosabertos.turismodeportugal.pt/datasets/empreendimentos-turisticos-existentes?geometry=-91.485%2C33.243%2C75.771%2C54.949 <br> [PT]", 
                                                "Update register of <br> accommodation establishments <br> [SK]", 
                                                "Waste production <br> (ISPRA: National Institute <br> for Environmental Protection) <br> [IT]", 
                                                "Water Consumption <br> [DE] [IT] [PL]", "Water Demand <br> [DE] [PL]", 
                                                "Web <br> Scraped <br> Data", "https://webgate.ec.europa.eu/fpfis/mwikis/essnetbigdata/index.php/WPJ_Milestones_and_deliverables.", 
                                                "https://nocowanie.pl <br> [PL]", "https://www.nocowanie.pl <br> [PL]"
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
                                                                                                                                     "orange", "teal", "lightblue", "orange", "lightblue"), url = c(NA, 
                                                                                                                                                                                                    "https://www.365tickets.com", NA, "https://www.airbnb.com/", 
                                                                                                                                                                                                    NA, "https://bagviewer.kadaster.nl/lvbag/bag-viewer/index.html#?geometry.x=160000&geometry.y=455000&zoomlevel=0", 
                                                                                                                                                                                                    NA, "https://www.booking.com/index.pt-pt.html?label=gen173nr-1BCAEoggI46AdIM1gEaLsBiAEBmAEfuAEHyAEN2AEB6AEBiAIBqAIDuALMu9j2BcACAdICJGIxMWVhOTlmLWNlYjgtNDZiYy1iNTg3LTk3ZWQxZmUzYjJiM9gCBeACAQ;sid=fd47e17373884136ae275ce04ce4a350;keep_landing=1&sb_price_type=total&", 
                                                                                                                                                                                                    NA, NA, NA, NA, NA, "https://ec.europa.eu/eurostat/web/experimental-statistics", 
                                                                                                                                                                                                    NA, NA, NA, NA, "https://www.holidaycheck.de/", NA, "https://www.hotels.com", 
                                                                                                                                                                                                    NA, "https://www.hrs.de/hotel/", NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                    NA, NA, "https://dashboard.nbtc.nl/jive/jivereportcontents.ashx?report=home_new", 
                                                                                                                                                                                                    NA, "https://www.kvk.nl", NA, NA, NA, NA, NA, NA, "https://www.pincamp.de/", 
                                                                                                                                                                                                    NA, "https://pochivka.bg", NA, NA, NA, NA, NA, NA, NA, "https://www.seatguru.com", 
                                                                                                                                                                                                    NA, "https://www.skyscanner.net", NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                    NA, NA, NA, NA, NA, "https://www.taxi2airport.com", NA, NA, "https://travelbi.turismodeportugal.pt/en-us/Pages/Home.aspx", 
                                                                                                                                                                                                    NA, "https://www.tripadvisor.com/", "https://dadosabertos.turismodeportugal.pt/datasets/estabelecimentos-de-al", 
                                                                                                                                                                                                    "https://dadosabertos.turismodeportugal.pt/datasets/empreendimentos-turisticos-existentes?geometry=-91.485%2C33.243%2C75.771%2C54.949", 
                                                                                                                                                                                                    NA, NA, NA, NA, NA, "https://webgate.ec.europa.eu/fpfis/mwikis/essnetbigdata/index.php/WPJ_Milestones_and_deliverables.", 
                                                                                                                                                                                                    NA, "https://www.nocowanie.pl")), row.names = c(NA, 82L), class = "data.frame")


#
# ~~2.2 Create Edges ---------------------------------------------------------------
edges <- structure(list(from = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                 1000110, 1000110, 1000110, 1000110, 1000110, 1000120, 1000120, 
                                 1000120, 1000120, 1000130, 1000130, 1000140, 1000140, 1000140, 
                                 1000150, 1000150, 1000150, 1000150, 1000160, 1000160, 1000160, 
                                 1000170, 1000180, 1000190, 1000201, 1000202, 1000203, 1000204, 
                                 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 
                                 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 
                                 2e+06, 2e+06, 2e+06, 2e+06, 2e+06, 2000210, 2000210, 2000210, 
                                 2000220, 2000220, 2000220, 2000230, 2000230, 2000240, 2000240, 
                                 2000250, 2000260, 2000270, 2000270, 2000280, 2000280, 2000290, 
                                 2000290, 2000290, 2000299, 2000299, 2000301, 2000302, 2000302, 
                                 2000303, 2000304, 2000304, 2000304, 2000305, 3e+06, 3e+06, 3e+06, 
                                 3e+06, 3e+06, 3e+06, 3e+06, 3e+06, 3e+06, 3e+06, 3e+06, 3e+06, 
                                 3000310, 3000310, 3000320, 3000320, 3000330, 3000330, 3000330, 
                                 3000330, 3000330, 3000350, 3000360, 6000610, 6000610, 6000620, 
                                 6000620, 6000620, 6000630, 6000630, 6000640, 6000640, 6000640, 
                                 6000650, 6000650, 6000660, 6000701, 6000702, 1000190, 1000190, 
                                 1000110, 1000110, 1000120, 1000120, 1000120, 1000140, 1000140, 
                                 1000140, 2000297, 2000297, 2000297, 2000297, 2000297, 2000298, 
                                 2000298, 2000298, 2000298, 2000298, 3000370, 3000370, 3000370, 
                                 3000370, 3000370, 3000310, 3000310, 3000310, 3000330, 3000380, 
                                 3000380, 3000391, 3000391, 3000391, 3000392, 3000392, 3000392, 
                                 2000297, 1000130, 1000140, 1000201, 1000202, 1000203, 1000203, 
                                 1000204, 1000204, 2002001, 2002001, 2002001, 2002001, 2000295, 
                                 2000295, 2000295, 2000295, 1000202, 2000293, 2000293, 2000293, 
                                 2000292, 2000294, 2000294, 2000294, 3000340, 3000340, 1000170, 
                                 1000170, 1000170, 1000191, 1000191, 1000191, 1000191, 1000191, 
                                 1000191, 1000191, 2000292, 1000180, 3000310), from_label = c("Web Data", 
                                                                                              "Web Data", "Web Data", "Web Data", "Web Data", "Web Data", "Web Data", 
                                                                                              "Web Data", "Web Data", "Web Data", "Web Data", "Web Data", "Web Data", 
                                                                                              "hotels.com", "hotels.com", "hotels.com", "hotels.com", "hotels.com", 
                                                                                              "booking.com", "booking.com", "booking.com", "booking.com", "Airbnb.com", 
                                                                                              "Airbnb.com", "Tripadvisor.com", "Tripadvisor.com", "Tripadvisor.com", 
                                                                                              "Skyscanner.net", "Skyscanner.net", "Skyscanner.net", "Skyscanner.net", 
                                                                                              "taxi2airport.com", "taxi2airport.com", "taxi2airport.com", "365tickets.com", 
                                                                                              "seatguru.com", "pochivka.bg", "Other webpages (booking accommodation)", 
                                                                                              "hrs.com", "holydaycheck", "pincamp", "Multi-Purpose Data", "Multi-Purpose Data", 
                                                                                              "Multi-Purpose Data", "Multi-Purpose Data", "Multi-Purpose Data", 
                                                                                              "Multi-Purpose Data", "Multi-Purpose Data", "Multi-Purpose Data", 
                                                                                              "Multi-Purpose Data", "Multi-Purpose Data", "Multi-Purpose Data", 
                                                                                              "Multi-Purpose Data", "Multi-Purpose Data", "Multi-Purpose Data", 
                                                                                              "Multi-Purpose Data", "Multi-Purpose Data", "Multi-Purpose Data", 
                                                                                              "Multi-Purpose Data", "Multi-Purpose Data", "Multi-Purpose Data", 
                                                                                              "Multi-Purpose Data", "Multi-Purpose Data", "Multi-Purpose Data", 
                                                                                              "NTR Tourist Establishments", "NTR Tourist Establishments", "NTR Tourist Establishments", 
                                                                                              "NRT Local Accommodation", "NRT Local Accommodation", "NRT Local Accommodation", 
                                                                                              "Credit & Debit Card transactions", "Credit & Debit Card transactions", 
                                                                                              "Airport Data", "Airport Data", "Tax Authority Register of Short-Term Lease Properties", 
                                                                                              "Register of Properties offered for short-term lease through digital platforms", 
                                                                                              "Register of non-categorised accommodation", "Register of non-categorised accommodation", 
                                                                                              "Register of Addresses and Buildings", "Register of Addresses and Buildings", 
                                                                                              "NBTC-NIPO", "NBTC-NIPO", "NBTC-NIPO", "Other sources", "Other sources", 
                                                                                              "Monthly bed tax data inc. overnights", "Financial transaction data", 
                                                                                              "Financial transaction data", "Regulatory reporting FIN1-12", 
                                                                                              "BTB data", "BTB data", "BTB data", "Register of accommodation establishments", 
                                                                                              "Survey Data", "Survey Data", "Survey Data", "Survey Data", "Survey Data", 
                                                                                              "Survey Data", "Survey Data", "Survey Data", "Survey Data", "Survey Data", 
                                                                                              "Survey Data", "Survey Data", "Survey on participation of residents in trips", 
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
                                                                                              "pochivka.bg", "pochivka.bg", "hotels.com", "hotels.com", "booking.com", 
                                                                                              "booking.com", "booking.com", "Tripadvisor.com", "Tripadvisor.com", 
                                                                                              "Tripadvisor.com", "Border traffic survey (BI)", "Border traffic survey (BI)", 
                                                                                              "Border traffic survey (BI)", "Border traffic survey (BI)", "Border traffic survey (BI)", 
                                                                                              "Waste production (ISPRA)", "Waste production (ISPRA)", "Waste production (ISPRA)", 
                                                                                              "Waste production (ISPRA)", "Waste production (ISPRA)", "Water Consumption", 
                                                                                              "Water Consumption", "Water Consumption", "Water Consumption", 
                                                                                              "Water Consumption", "Survey on participation of residents in trips", 
                                                                                              "Survey on participation of residents in trips", "Survey on participation of residents in trips", 
                                                                                              "Survey on tourist accommodation base", "Survey on rural tourism accommodations", 
                                                                                              "Survey on rural tourism accommodations", "Railway, airport and port data", 
                                                                                              "Railway, airport and port data", "Railway, airport and port data", 
                                                                                              "Survey on museums and similar", "Survey on museums and similar", 
                                                                                              "Survey on museums and similar", "Border traffic survey (BI)", 
                                                                                              "Airbnb.com", "Tripadvisor.com", "Other webpages (booking accommodation)", 
                                                                                              "hrs.com", "holydaycheck", "holydaycheck", "pincamp", "pincamp", 
                                                                                              "Mobile phone data", "Mobile phone data", "Mobile phone data", 
                                                                                              "Mobile phone data", "Water Demand", "Water Demand", "Water Demand", 
                                                                                              "Water Demand", "hrs.com", "Smart City", "Smart City", "Smart City", 
                                                                                              "Land Border Traffic", "Parking Meters Data", "Parking Meters Data", 
                                                                                              "Parking Meters Data", "Border Traffic Survey", "Border Traffic Survey", 
                                                                                              "365tickets", "365tickets", "365tickets", "nocowanie", "nocowanie", 
                                                                                              "nocowanie", "nocowanie", "nocowanie", "nocowanie", "nocowanie", 
                                                                                              "Land Border Traffic", "seatguru.com", "Survey on participation of residents in trips"
                                 ), to = c(1000110, 1000120, 1000130, 1000140, 1000150, 1000160, 
                                           1000170, 1000180, 1000190, 1000201, 1000202, 1000203, 1000204, 
                                           2000305, 6000610, 6000620, 6000650, 7000110, 2000305, 6000610, 
                                           6000620, 7000120, 6000610, 7000130, 6000630, 6000640, 7000140, 
                                           3000310, 3000320, 6000630, 7000150, 6000630, 6000640, 7000160, 
                                           7000170, 7000180, 7000190, 2000305, 7000191, 7000192, 7000193, 
                                           2000210, 2000220, 2000230, 2000240, 2000250, 2000260, 2000270, 
                                           2000280, 2000290, 2000292, 2000293, 2000294, 2000295, 2000296, 
                                           2000297, 2000298, 2000299, 2000301, 2000302, 2000303, 2000304, 
                                           2000305, 2002001, 2000211, 2000223, 6000610, 2000222, 2000223, 
                                           6000610, 6000630, 6000640, 6000630, 6000640, 6000610, 6000610, 
                                           2000271, 6000610, 2000281, 6000610, 2000291, 6000630, 6000640, 
                                           6000630, 6000640, 3000330, 6000640, 6000702, 3000330, 3000310, 
                                           6000673, 6000674, 6000701, 2002991, 2002992, 3000310, 3000320, 
                                           3000330, 3000340, 3000350, 3000360, 3000370, 3000380, 3000391, 
                                           3000392, 6000630, 6000640, 6000630, 6000640, 6000610, 6000620, 
                                           6000640, 6000650, 6000701, 6000660, 6000660, 6000611, 6000612, 
                                           6000611, 6000612, 6000630, 6000611, 6000612, 6000611, 6000612, 
                                           6000660, 6000611, 6000630, 6000630, 6000630, 6000660, 6000610, 
                                           6000650, 6000630, 6000640, 6000630, 6000640, 6000650, 6000610, 
                                           6000620, 6000650, 6000630, 6000640, 6000672, 6000671, 6000660, 
                                           6000660, 6000674, 6000671, 6000672, 6000673, 6000660, 6000674, 
                                           6000671, 6000672, 6000673, 6000660, 6000671, 6000674, 6000630, 
                                           6000610, 6000620, 6000660, 6000672, 6000674, 6000674, 6000671, 
                                           6000673, 6000674, 2000305, 2000305, 6000610, 6000610, 2000305, 
                                           6000610, 2000305, 6000610, 6000674, 6000671, 6000673, 6000672, 
                                           6000674, 6000671, 6000672, 6000673, 2000305, 6000671, 6000672, 
                                           6000673, 6000660, 6000671, 6000672, 6000673, 6000640, 6000660, 
                                           6000671, 6000672, 6000673, 6000610, 6000620, 2000305, 6000650, 
                                           6000640, 6000630, 7000194, 6000640, 6000660, 6000671), to_label = c("hotels.com", 
                                                                                                               "booking.com", "Airbnb.com", "Tripadvisor.com", "Skyscanner.net", 
                                                                                                               "taxi2airport.com", "365tickets.com", "seatguru.com", "pochivka.bg", 
                                                                                                               "Other webpages (booking accommodation)", "hrs.com", "holydaycheck", 
                                                                                                               "pincamp", "Register of accommodation establishments", "Improve tourist accommodation base", 
                                                                                                               "Spatial disaggregation of data on tourist accommodation base", 
                                                                                                               "Flash estimates of the use of tourist accommodation base", "hotels.com", 
                                                                                                               "Register of accommodation establishments", "Improve tourist accommodation base", 
                                                                                                               "Spatial disaggregation of data on tourist accommodation base", 
                                                                                                               "booking.com", "Improve tourist accommodation base", "Airbnb.com", 
                                                                                                               "Improve quality of satellite accounts", "Improve quality of tourists expenses data", 
                                                                                                               "Tripadvisor.com", "Survey on participation of residents in trips", 
                                                                                                               "Survey on trips made by foreigners", "Improve quality of satellite accounts", 
                                                                                                               "Skyscanner.net", "Improve quality of satellite accounts", "Improve quality of tourists expenses data", 
                                                                                                               "taxi2airport.com", "365tickets.com", "seatguru.com", "pochivka.bg", 
                                                                                                               "Register of accommodation establishments", "hrs.com", "holydaycheck", 
                                                                                                               "pincamp", "NTR Tourist Establishments", "NRT Local Accommodation", 
                                                                                                               "Credit & Debit Card transactions", "Airport Data", "Register of non-categorised tourist accommodation establishments", 
                                                                                                               "Tax Authority Register of Short-Term Lease Properties", "Register of Properties offered for short-term lease through digital platforms", 
                                                                                                               "Register of Addresses and Buildings", "NBTC-NIPO", "Land Border Traffic", 
                                                                                                               "Smart City", "Parking Meters Data", "Water Demand", "Ministry of Interior", 
                                                                                                               "Border traffic survey (BI)", "Waste production (ISPRA)", "Other sources", 
                                                                                                               "Monthly bed tax data inc. overnights", "Financial transaction data", 
                                                                                                               "Regulatory reporting FIN1-12", "BTB data", "Register of accommodation establishments", 
                                                                                                               "Mobile phone data", "Turismo de Portugal Open Data", "travelBI by Turismo de Portugal", 
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
                                                                                                               "Railway, airport and port data", "Survey on museums and similar institutions", 
                                                                                                               "Survey on participation of residents in trips", "Survey on trips made by foreigners", 
                                                                                                               "Survey on tourist accommodation base", "Border Traffic Survey", 
                                                                                                               "Survey on the visits by foreigners", "Survey data on tourist trips", 
                                                                                                               "Water Consumption", "Survey on rural tourism accommodations", 
                                                                                                               "Railway, airport and port data", "Survey on museums and similar", 
                                                                                                               "Improve quality of satellite accounts", "Improve quality of tourists expenses data", 
                                                                                                               "Improve quality of satellite accounts", "Improve quality of tourists expenses data", 
                                                                                                               "Improve tourist accommodation base", "Spatial disaggregation of accommodation", 
                                                                                                               "Improve quality of tourists expenses data", "Flash estimates", 
                                                                                                               "Update register of accommodation establishments", "Improve quality data on trips", 
                                                                                                               "Improve quality data on trips", "Eurostat Experimental Statistics", 
                                                                                                               "WPJ Milestones and Deliverables", "Eurostat Experimental Statistics", 
                                                                                                               "WPJ Milestones and Deliverables", "Improve quality of satellite accounts", 
                                                                                                               "Eurostat Experimental Statistics", "WPJ Milestones and Deliverables", 
                                                                                                               "Eurostat Experimental Statistics", "WPJ Milestones and Deliverables", 
                                                                                                               "Improve quality data on trips", "Eurostat Experimental Statistics", 
                                                                                                               "Improve quality of satellite accounts", "Improve quality of satellite accounts", 
                                                                                                               "Improve quality of satellite accounts", "Improve quality data on trips", 
                                                                                                               "Improve tourist accommodation base", "Flash estimates", "Improve quality of satellite accounts", 
                                                                                                               "Improve quality of tourists expenses data", "Improve quality of satellite accounts", 
                                                                                                               "Improve quality of tourists expenses data", "Flash estimates", 
                                                                                                               "Improve tourist accommodation base", "Spatial disaggregation of accommodation", 
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
                                                                                                               "Improve quality data on trips", "City Tourism"), length = c(250, 
                                                                                                                                                                            250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                                            250, 250, 250, 5, 250, 250, 250, 5, 250, 5, 250, 250, 5, 250, 
                                                                                                                                                                            250, 250, 5, 250, 250, 5, 5, 5, 5, 250, 5, 5, 5, 250, 250, 250, 
                                                                                                                                                                            250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                                            250, 250, 250, 250, 250, 250, 250, 5, 5, 250, 5, 5, 250, 250, 
                                                                                                                                                                            250, 250, 250, 250, 250, 5, 250, 5, 250, 5, 250, 250, 250, 250, 
                                                                                                                                                                            250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                                            250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                                            250, 250, 250, 250, 250, 5, 5, 5, 5, 250, 5, 5, 5, 5, 250, 5, 
                                                                                                                                                                            250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                                            250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                                            250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                                            250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                                            250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                                                                                                                                                                            250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 5, 
                                                                                                                                                                            250, 250, 250), width = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                      1, 1, 1, 1, 1, 1, 1), label = c(NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                      NA, NA, NA, NA, NA, NA, "Linking data on tourist  accommodations to frame for statistical survey", 
                                                                                                                                                                                                                                      "Removing duplicates and checking plausability of estimates using QGIS [NL]", 
                                                                                                                                                                                                                                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Exception: [IT]", 
                                                                                                                                                                                                                                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                                                                                                                                                                                      ), arrows = c("to", "to", "to", "to", "to", "to", "to", "to", 
                                                                                                                                                                                                                    "to", "to", "to", "to", "to", "to", "middle;to", "middle;to", 
                                                                                                                                                                                                                    "to", "FALSE", "to", "to", "to", "FALSE", "to", "FALSE", "to", 
                                                                                                                                                                                                                    "to", "FALSE", "to", "to", "to", "FALSE", "to", "to", "FALSE", 
                                                                                                                                                                                                                    "FALSE", "FALSE", "FALSE", "to", "FALSE", "FALSE", "FALSE", "to", 
                                                                                                                                                                                                                    "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
                                                                                                                                                                                                                    "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
                                                                                                                                                                                                                    "FALSE", "FALSE", "to", "FALSE", "FALSE", "to", "to", "to", "to", 
                                                                                                                                                                                                                    "to", "to", "to", "FALSE", "to", "FALSE", "to", "FALSE", "to", 
                                                                                                                                                                                                                    "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
                                                                                                                                                                                                                    "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
                                                                                                                                                                                                                    "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
                                                                                                                                                                                                                    "to", "FALSE", "FALSE", "FALSE", "FALSE", "to", "FALSE", "FALSE", 
                                                                                                                                                                                                                    "FALSE", "FALSE", "to", "FALSE", "to", "to", "to", "to", "to", 
                                                                                                                                                                                                                    "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
                                                                                                                                                                                                                    "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
                                                                                                                                                                                                                    "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
                                                                                                                                                                                                                    "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
                                                                                                                                                                                                                    "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
                                                                                                                                                                                                                    "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", "to", 
                                                                                                                                                                                                                    "to", "to", "to", "to", "to", "to", "FALSE", "to", "to", "to"
                                                                                                                                                                                                      )), row.names = c(NA, 208L), class = "data.frame")

# Create VisNetwork object ------------------------------------------------
# Create VisNetwork object 
# "height" and "width" are optional and the default is automatic. 
# "height" can be customized in pixels or as a percentage 
# according your display resolution.
# "main" and "submain" define the title and subtitle.

visNet_Workflow_uRos2021 <- visNetwork(nodes, edges,
                                 height = "800px",
                                 # height = "1000px",
                                 # width = "100%",
                                 width = "90%",
                                 main="Overview of the Inputs and Outputs of the Pilot Project on Innovative Tourism Statistics", 
                                 submain="[BG] [DE] [EL] [IT] [NL] [PL] [PT] [SK]") %>% 
        
        # ~~Create Legend  ------------------------------------------------
# Create a user defined VisNetwork Legend 


visLegend(useGroups = FALSE, 
          # width = 0.3,
          width = 0.17,
          position = "right",
          zoom = F,
          addNodes = data.frame(
                  label = c("Data Sources Type", "Data Sources", "Experimental Results", "External links"),
                  shape = c("dot", "dot", "square", "triangle"),
                  color = c("teal", "orange", "purple", "lightblue"))) %>% 
        
        # ~~Interaction ----------------------------------------------
# Added functionalities for user interaction: navigations buttons and multi selection.
# Green navigation buttons are placed on the bottom left and bottom right of the network canvas.
# These will help to navigate, zoom in and out and re-center.
# Multi-selection: a long click as well as a control-click will add to the selection. 

visInteraction(navigationButtons = TRUE, multiselect = T ) %>% 
        
        # ~~Export Current Network Canvas to PNG -------------------------------------------
# A small button with "Export to PNG" text will be placed on the bottom right corner 
# of the network canvas. On click, a PNG file (the default) will be exported to downloads folder. 
# Can be configured to "jpeg" or "pdf"

visExport(type = "png") %>%                                                                 
        
        # ~~Use igraph Layout -------------------------------------------   
# Using a igraph layout allows to compute coordinates and fast rendering. 
# The network will be rendered faster and with no stabilization. 
# This is an option but will over run default settings for VisNetwork

#  visIgraphLayout(type = "full") %>%

# ~~Default Format for Nodes -----------------------------------------
# Nodes will get a dot shape by default if none is provided in 
# nodes$shape.  

visNodes(shape = "dot",
         color = list(
                 highlight = "#FF8000"),
         shadow = list(enabled = TRUE, size = 10),
         labelHighlightBold = T) %>%
        
        # ~~Default Format for Edges -----------------------------------------

visEdges(shadow = FALSE,
         color = list(color = "#0085AF", highlight = "#C62F4B"), 
         # smooth = list(enabled = F), # to generate straight lines
         arrows = 'to') %>%
        
        # ~~General Options (combo box)----------------------------------------------------------
# Creates two combo boxes for selection of nodes. One based on nodes id label (nodes$label)
# and another based on  multiple groups per nodes using a comma as it is the case 
# of nodes$group

# visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T), 
           manipulation = TRUE,
           
           # First combo box "Select by id"
           # Defined by nodesIdSelection can be "values : Optional. Vector of possible values (node's id), 
           # and so order is preserve. Default to all id in nodes data.frame.        
           
           nodesIdSelection = list(enabled = T, selected = "1"),
           
           # Second combo box "Select by group"
           # Defined by selectedBy, uses multiple "categories" separated by a comma in nodes$group 
           
           selectedBy = list(variable = "group", multiple = TRUE)) %>%
        
        # ~~Layout Options ----------------------------------------------------------
# When not using hierarchical layout, providing random seed manually
# will produce the same layout every time.

visLayout(randomSeed = 11, improvedLayout = T, hierarchical = F) %>%            
        
        # ~~Open URL in New Window--------------------------------------------------
# This fires an event when a node is selected and double clicked on. 
# In this case the event is open url as defined in nodes$url in a new
# browser window.

# visEvents(selectNode = 
visEvents(doubleClick = 
                  "function(params) {
    var nodeID = params.nodes[0];
    var url = this.body.nodes[nodeID].options.url;
    window.open(url, '_blank');
   }") %>%
        
        # ~~Clustering Options OFF-------------------------------------------------- 
# By default, clustering is performed based on nodes$group.
# Nevertheless in this case that variable has multiple groups per node,
# so we can define on which nodes should the clustering be done.
# This helps to keep the initial network declutered. 
# Clusters can be expanded by double click and can be reinitialized by
# clicking the "Reinitialize clustering" in the lower left corner of
# the canvas

# visClusteringByConnection(nodes = c(1,2,3)) %>%

# ~~Physics Options-------------------------------------------------- 
# Configuration of the physics system governing the simulation 
# of the nodes and edges. 
# BarnesHut is the recommended solver for non-hierarchical layout. 
# The remaining parameters were fine tuned for this particular network.

# visPhysics(maxVelocity = 5,
#            solver = "barnesHut", 
#            barnesHut = list(avoidOverlap = 0.15,
#                             gravitationalConstant = -1500,
#                             springConstant = 0.01),
#            repulsion = list(centralGravity = 1.5))

visPhysics(maxVelocity = 5,
           solver = "barnesHut",
           barnesHut = list(avoidOverlap = 0.15,
                            # centralGravity = 1,
                            gravitationalConstant = -1500,
                            springConstant = 0.01),
           repulsion = list(centralGravity = 1.5))


# Show result on RStudio viewer-----------------------------------------------------------
visNet_Workflow_uRos2021

# Save visNetwork to HTML File -----------------------------------------------------------
# Save the visNetwork object to a single self-contained HTML file for sharing.
visSave(visNet_Workflow_uRos2021, file = "visNet_Workflow_uRos2021.html")

# ================ |-------------| ================

