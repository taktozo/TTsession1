snippet Header_Script "R Header Template" b

#' ------------------------------------------------
#' Project:  ${1:TT R session 1}
#' Script:   ${2:TT R session 1}
#' Author:   ${3:Takesure Tozooneyi }
#' Date:     ${4:`r paste(date())`}
#' ------------------------------------------------

snippet Folders "Create project folders" b
Folder_names <- c("Raw Data", "Data", "Figures", "Tables", "Analysis", "Literature", "Paper", "Templates", "Slides", "Website")
ifelse(!dir.exists(Folder_names), sapply(Folder_names, dir.create), "Folder Exists")


# Installing R packages ---------------------------------------------------

install.packages('gtsummary')
install.packages('tidyverse')
install.packages('readxl')
install.packages('janitor')
install.packages('openxlsx')
install.packages('gt')
install.packages('arsenal')
install.packages('questionr')


# Uploading the libraries -------------------------------------------------

library(gtsummary)
library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)
library(gt)
library(arsenal)
library(questionr)


# Loading Data ------------------------------------------------------------

sam_data_001 <- read_excel("sam_data_001.xlsx") %>%
  clean_names()


# Labeling variables -----------------------------------------------------

labels(sam_data_001) = c(
  age = "Age of respondent, yrs",
  agehousehold = "HH Age, yrs",
  size = "Size of household",
  female = "No. of females",
  male = "No. of males",
  members_under5 = "No. of childern under 5 yrs",
  members5to17 = "No. of hh members 5-17 yrs",
  members18to65 = "No. of hh members 18-65 yrs",
  members66 = "No. of hh members 66+ yrs",
  genderhousehold = "Gender of HH",
  marital_status = " Marital status of HH",
  sec4b1 = "Religion")

pt <- sam_data_001 %>%
  select(genderhousehold, index)


# Demographic Summary -----------------------------------------------------

dem <- sam_data_001 %>%
  select(7:18, sec4b1)


# Treating missing values -------------------------------------------------

dem <- dem %>%
  replace_na(
    list(
      agehousehold = 0,
      size = 0,
      female = 0,
      male = 0,
      members_under5 = 0,
      members5to17 = 0,
      members18to65 = 0,
      members66 = 0
    )
  )


# Household Roster --------------------------------------------------------

sam_data_002 <- read_excel("sam_data_001.xlsx", sheet = 2) %>%
  clean_names()

names(sam_data_002) <- gsub("section1_", "", names(sam_data_002))

roster <- sam_data_002 %>%
  select(2:6, 8) %>%
  replace_na(list (sec1q3 = 0))


# Household Food Security -------------------------------------------------

fs <- sam_data_001 %>%
  select(genderhousehold, contains("sec2"))

fs <- fs %>%
  select(-3, -11)

names(fs) <- gsub("sec2q2_", "", names(fs))

labels(fs) <-
  c(sec2q1 = "In the past 5 years, where there years you did not have enough food to meet your family needs ",
    sec2q3 = "How many meals excluding snacks do you normally have in a day",
    sec2q4 = "Compared to 5 years ago, your households is: ")


# Major shocks and risks --------------------------------------------------

sam_data_003 <- read_excel("sam_data_001.xlsx", sheet = 3) %>%
  clean_names()

#Join table 3 with the primary table (to link table with gender of household)
shocks <-
  left_join(pt, sam_data_003, by = c("index" = "parent_index")) %>%
  select(genderhousehold, sec3a_sec3a1)

labels(shocks) <-
  c(genderhousehold = "Gender of the head of household",
    sec3a_sec3a1 = "Which shocks did you experience")


# Shocks and social networks ----------------------------------------------

sam_data_004 <- read_excel("sam_data_001.xlsx", sheet = 4)
names(sam_data_004) <- gsub("sec3b/", "", names(sam_data_004))
adaptation <- sam_data_004 %>%
  select(sec3b1, 5:18)

# Shocks and local knowledge systems --------------------------------------

sam_data_005 <- read_excel("sam_data_001.xlsx", sheet = 5) %>%
  clean_names()
names(sam_data_005) <- gsub("sec4a_", "", names(sam_data_005))
iks <- sam_data_005 %>%
  select(1:2)
labels(iks) <- c(sec4a1 = "Changes in weather pattern",
                 sec4a2 = "Are the ways that local people predict or know about weather other than through radio")
iks2 <- sam_data_001 %>%
  select(genderhousehold, sec4a4, sec4a6)
labels(iks2) <- c(genderhousehold = "Gender of household head",
                  sec4a4 = "In your view, are the local weather prediction systems useful?",
                  sec4a6 = "Have you used this informatoon to plan your agricutural activities?")

# Sacred sites ------------------------------------------------------------

sacred_sites <- sam_data_001 %>%
  select(sec4b1, sec4b2)

# Sacred animals, birds, and trees ----------------------------------------

sam_data_006 <- read_excel("sam_data_001.xlsx", sheet = 6) %>%
  clean_names()

# Recoding sam_data_006$section4c_sec4c1b into sam_data_006$sectio --------

sam_data_006$section4c_sec4c1b_rec <-
  fct_recode(
    sam_data_006$section4c_sec4c1b,
    "Mariti" = "Manti",
    "Mishumbi, Mukwakwa" = "Mishumbi ne Mukwakwa",
    "Jackal" = "Gava",
    "Mariti, Mbira" = "Mariti Mbira",
    "Mbira" = "mbira",
    "Mariti, Kowiro" = "Mariti, kowiro",
    "Baboon, Jackal, Mbira" = "Mbira,  Gava,  Gudo",
    "Baboon,  Chikovo" = "Baboon,  chikovo",
    "Mariti, Dendera, Kowiro" = "Riti/Dendera,kowiro",
    "Jackal" = "Gava/Jackle",
    "Jichidza" = "Majijidza",
    "Mariti, Kowiro" = "Kowiri, Mariti",
    "Mariti, Kowiro" = "Mariti , kowiro",
    "Jackal, Fox" = "Gava/Fox",
    "Dendera" = "Matendera",
    "Mariti" = "Riti",
    "Kowiro, Owls, Masongano" = "Kowiro, Owls Masongano",
    "Owl" = "Owls",
    "Hyena" = "Hyenas",
    "Baboon" = "Baboons",
    "Mukamba, Mukwa, Mutuwa trees" = "Mukamba tree(Mukwa tree),Mutuwa",
    "Mutuwa, Mubvumira, Mukamba trees" = "Mutuwa/mubvumira/ mukamba trees"
  )

abt <- sam_data_006 %>%
  select(1:2)
labels(abt) <-
  c(section4c_sec4c1a = "Is there a sacred bird, tree, animal?",
    section4c_sec4c1b = "Indicate the name")


#  Access to services and programs --------------------------------------

sam_data_007 <- read_excel("sam_data_001.xlsx", sheet = 7) %>%
  clean_names()

sam_data_007 <-
  left_join(pt, sam_data_007, by = c("index" = "parent_index")) %>%
  select(
    genderhousehold,
    section5_sec5q1,
    section5_sec5q2,
    section5_sec5q3,
    section5_sec5q4,
    section5_sec5q5,
    section5_sec5q6,
    section5_sec5q7,
    section5_sec5q8,
    section5_sec5q9
  )

sam_data_007$section5_sec5q2 <-
  fct_recode(
    sam_data_007$section5_sec5q2,
    "Social Welfare" = "Social walfare",
    "Government" = "Gvt",
    "BEAM" = "Beam",
    "Social Welfare" = "Social welfare",
    "Agritex" = "Agritex Officers",
    "Vet Services" = "Veterinary Services",
    "Vet Services" = "Vetinary services",
    "Agritex" = "Agritex Officer",
    "Vet Services" = "Veterinary Officers",
    "Vet Services" = "Veterinary",
    "Donors" = "USAID",
    "Agritex" = "Agritex officers",
    "Donors" = "NAC",
    "Agritex" = "Arex",
    "Donors" = "NGO",
    "Agritex" = "Agritex offices",
    "Vet Services" = "Verterinary services",
    "Donors" = "Usaid",
    "Donors" = "WfP",
    "Donors" = "Donor",
    "Government" = "GVT",
    "Donors" = "Dor",
    "Vet Services" = "Vertinary"
  )

sam_data_007 <- sam_data_007 %>%
  rename(
    `Programmes` = section5_sec5q1,
    `Source` = section5_sec5q2,
    `Did_HH_Travel` = section5_sec5q3,
    `HH_Member_Travelled` = section5_sec5q4,
    `Where_travelled` = section5_sec5q5,
    `Form_of_Transport` = section5_sec5q6,
    `Distance_in_km` = section5_sec5q7,
    `Duration_in_minutes` = section5_sec5q8,
    `Cost_of_Transport` = section5_sec5q9
  )

labels(sam_data_007) = c(
  Programmes = "Services and programmes",
  Source = "Who provided support?",
  Did_HH_Travel = "Did hh member travelled to get suppot?",
  HH_Member_Travelled = "Who travelled?",
  Where_travelled = "Where did he/she travelled?",
  Form_of_Transport = "Form of transport used",
  Distance_in_km = "Distance in kilometres",
  Duration_in_minutes = "Time taken in minutes",
  Cost_of_Transport = "Cost of transport"
  
)

# Land --------------------------------------------------------------------

sam_data_008 <- read_excel("sam_data_001.xlsx", sheet = 8) %>%
  clean_names()
sam_data_008 <- sam_data_008 %>%
  select(c(-3, -11:-17, -20:-39))

labels(sam_data_008) = c(
  plot_questions_plot_id = "Plot Number",
  plot_questions_sec6a2	= "Type of Land",
  plot_questions_sec6a4	= "Irrigated",
  plot_questions_sec6a5	= "Tenure",
  plot_questions_sec6a6	= "Land Acquired Through",
  plot_questions_sec6a7 = "Principal Use",
  plot_questions_sec6a8 = "Crops Rotated",
  plot_questions_sec6a9a = "Degradation",
  plot_questions_sec6a9b = "Form of Degradation",
  plot_questions_sec6a10 = "Extent of Degradation",
  plot_questions_sec6a11 = "Manager"
)

sam_data_008 <- sam_data_008 %>%
  rename(
    `Plot_Number` = plot_questions_plot_id,
    `Type_of_Land` = plot_questions_sec6a2,
    `Irrigated` = plot_questions_sec6a4,
    `Tenure` = plot_questions_sec6a5,
    `Land_Acquired_Through` = plot_questions_sec6a6,
    `Principal_Use` = plot_questions_sec6a7,
    `Crops_Rotated` = plot_questions_sec6a8,
    `Degradation` = plot_questions_sec6a9a,
    `Form_of_Degradation` = plot_questions_sec6a9b,
    `Extent_of_Degradation` = plot_questions_sec6a10,
    `Who_Manage_the_field` = plot_questions_sec6a11
  )


# Crop Production - Last Rainy Season (2018/2019) -------------------------

sam_data_009 <- read_excel("sam_data_001.xlsx", sheet = 9) %>%
  clean_names() %>% select(
    sec6b_sec6b2,
    sec6b_sec6b3,
    sec6b_sec6b4,
    sec6b_sec6b5,
    sec6b_sec6b6,
    sec6b_sec6b7,
    sec6b_sec6b8,
    sec6b_sec6b8a,
    sec6b_sec6b8b,
    sec6b_sec6b9,
    sec6b_sec6b9a,
    sec6b_sec6b9b
  )

sam_data_009 <- sam_data_009 %>%
  rename(
    `Plot_Id` = sec6b_sec6b2,
    `Crop` = sec6b_sec6b3,
    `Type_of_Crop_Stand` = sec6b_sec6b4,
    `Entire_Plot` = sec6b_sec6b5,
    `Area_Under_Crop` = sec6b_sec6b6,
    `Seed_Variety` = sec6b_sec6b7
  )


# Recoding sam_data_009$sec6b_sec6b8 into sam_data_009$Total_Harve --------

sam_data_009$Total_Harvest_in_kgs <-
  fct_recode(
    sam_data_009$sec6b_sec6b8,
    "90" = "90Kg Bag",
    "25" = "25 Kg Bag",
    "1" = "Kilogram",
    "50" = "50 Kg Bag",
    "75" = "75Kg Bag"
  )

sam_data_009$Expected_Harvest_in_kgs <-
  fct_recode(
    sam_data_009$sec6b_sec6b9,
    "90" = "90Kg Bag",
    "25" = "25 Kg Bag",
    "1" = "Kilogram",
    "50" = "50 Kg Bag",
    "75" = "75Kg Bag")

## Recoding sam_data_009$Total_Harvest_in_kgs into sam_data_009$Total_Harvest_in_kgs_rec

sam_data_009$Total_Harvest_in_kgs_rec <- fct_recode(sam_data_009$Total_Harvest_in_kgs,
                                                    "25.0" = "25",
                                                    "50.0" = "50",
                                                    "75.0" = "75",
                                                    "90.0" = "90",
                                                    "1.0" = "1"
)

sam_data_009$Total_Harvest_in_kgs_rec <- fct_explicit_na(sam_data_009$Total_Harvest_in_kgs_rec, "0")
sam_data_009$Total_Harvest_in_kgs_rec <- as.numeric(as.character(sam_data_009$Total_Harvest_in_kgs_rec))

## Recoding sam_data_009$sec6b_sec6b8a into sam_data_009$sec6b_sec6b8a_rec

sam_data_009$sec6b_sec6b8a_rec <- as.character(sam_data_009$sec6b_sec6b8a)
sam_data_009$sec6b_sec6b8a_rec <- fct_recode(sam_data_009$sec6b_sec6b8a_rec,
                                             "1.0" = "1",
                                             "2.0" = "2",
                                             "0.0" = "0",
                                             "3.0" = "3",
                                             "4.0" = "4",
                                             "6.0" = "6",
                                             "7.0" = "7",
                                             "500.0" = "500",
                                             "350.0" = "350",
                                             "260.0" = "260",
                                             "200.0" = "200",
                                             "100.0" = "100",
                                             "50.0" = "50",
                                             "9.0" = "9",
                                             "5.0" = "5",
                                             "20.0" = "20",
                                             "15.0" = "15",
                                             "12.0" = "12"
)

sam_data_009$sec6b_sec6b8a_rec <- fct_explicit_na(sam_data_009$sec6b_sec6b8a_rec, "0.0")
sam_data_009$sec6b_sec6b8a_rec <- as.numeric(as.character(sam_data_009$sec6b_sec6b8a_rec))

sam_data_009$Total_Harvest_in_kgs_ <- (sam_data_009$Total_Harvest_in_kgs_rec * sam_data_009$sec6b_sec6b8a_rec)


# Crop production ---------------------------------------------------------

sam_data_010 <- read_excel("sam_data_001.xlsx", sheet = 10) %>%
  clean_names()
names(sam_data_010) <- gsub("sec6c_", "", names(sam_data_010))

crops <- sam_data_010 %>%  select(1:3, 29:30)
labels(crops) <- c(
  sec6c1 = "Name of crop grown",
  sec6c2 = "Trend in proportion under this croop",
  sec6c3 =  "Explain the change in land size",
  sec6c4 = "Production levels in 2009",
  sec6c5 = "Explain the change in production"
)


# Crop production - changing preferences ----------------------------------

sam_data_011 <- read_excel("sam_data_001.xlsx", sheet = 11) %>%
  clean_names()
crops_1980  <- sam_data_011 %>%
  select(1:2)
labels(crops_1980) <- c(sec6d_sec6d1 = "Crop dropped since 1980",
                        sec6d_sec6d2 = "Why was the crop dropped")


# Crop production and changing preferences adopted ------------------------

sam_data_012 <- read_excel("sam_data_001.xlsx", sheet = 12) %>%
  clean_names()
crops_1980a  <- sam_data_012 %>%
  select(1:2)
labels(crops_1980a) <- c(sec6dd_sec6d3 = "Crop dropped since 1980",
                         sec6dd_sec6d4 = "Why was the crop dropped")


# Watershed condition and management --------------------------------------

sam_data_013 <- read_excel("sam_data_001.xlsx", sheet = 13) %>%
  clean_names() %>% select(sec7a_sec7a1,
                           sec7a_sec7a2,
                           sec7a_sec7a3,
                           sec7a_sec7a4)

sam_data_013 <- sam_data_013 %>%
  rename(
    `Type_of_change` = sec7a_sec7a1,
    `Observed_in_comm` = sec7a_sec7a2,
    `Observed_in_own_field` = sec7a_sec7a3,
    `Prevention_measure` = sec7a_sec7a4
  )

labels(sam_data_013) = c(
  Type_of_change = "Type of change",
  Observed_in_comm = "Change observed in community",
  Observed_in_own_field = "Change observed in own field",
  Prevention_measures = "Prevention measure"
)
