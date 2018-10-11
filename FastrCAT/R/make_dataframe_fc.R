#' @title Create a Dataframe from .up files
#'
#' @description This function writes a single data frame in .csv format to file
#' containing oceanographic data collected by the FastCat during a cruise.
#' The format and column naming conventions are specific to the needs of EcoDAAT.
#' This is the primary function of the FastrCAT package and must be run prior to
#' all other functions. All other functions depend on the data frame generated.
#' Other outputs of the function are two text files. This first is a cruise
#' summary.The summary contains basic information about the data and some summary
#' statistics.The second is a warnings file, which tells users if data or
#' information is missing incase reprocessing is necessary.
#' @param current_path The path to directory where all .up files
#' are located for a cruise.
#' @param GE A logical value, Returns the dataframe to the global R environment.
#' By defulaut it is set to FALSE. Set to TRUE if you would like the data
#' available in the global environment.
#' @return .csv file of all .up files. A .txt file of warnings and one
#' of summary statistics.



make_dataframe_fc <- function(current_path,GE = FALSE){
  #This function will take post processed FASTCAT files, BON.up, and grab the data and headers then
  #export a .csv files of a single cruises data.The function requires the path to the folder where the
  #data are located. This data being pressure, temperature,conductivity,salinity, Sigma T, and whether
  #it is flagged. It also grabs the header information which is included in the .up file from COD.This
  #information is cruise,station,haul,latitude,longitude, and UTC date and time. If a data isn't in
  #the BON.up file the header information will be included but the fields will have NA's. This will
  #generate a warning in the console and in a text file after all the BON.up files have been
  #processed. If the header infromation is missing then it will move to the next file and not include
  #it in the final .csv file. This will generate a warning in the console and in a text file when the
  #function is done. When this happens the warning suggest that reprocessing of these files is needed
  #in order to grab the header information from COD.Use this function with the base R function lapply.
  #Just give it a list of the folder directories and get all the post field season FASTCAt data at #once.Enjoy!

  # current_path is the path to the folder where the .up files are located
  # GE is if you want the dataframe pushed into the R global environment, then set this
  # to TRUE. Otherwise it is set to FALSE and will not be pushed to the Global Environment.
  # Only use this if you are doing a single folder and not a batch of folders otherwise it will
  # be overwritten each time. Otherwise read in the .csv files.


  #FOR TESTING
  #current_path <- test_path
  #GE <- TRUE
  #Temp grabs the list of file names from the path set within the funtion. What ever folder
  #you have a the working directory it will pull the file names from it.
  temp <- list.files(path = current_path, pattern = "\\.up$", ignore.case = TRUE,
                     include.dirs = TRUE, full.names = TRUE)

  #List object for the for the BON.up data. Each dataframe generated from each BON file
  #will be appended to the list called CruiseData.
  cruise_data <- list()

  #List objects for the no data and no header warnings.
  no_data_files <- list()
  no_head_files <- list()

  for(i in 1:length(temp)){
    #Reads in the the BON.up file as a character string. This is for grep to find which line
    #particular data are located and to provide an integer to skip in read.table.
    full_table <- read_lines(temp[i])

    #Statement to skip files which are completley empty
    if(length(file.size(full_table)) == 0){

      no_data_files[[i]] <- paste("This file", temp[i] ,"is empty.", sep = " ")

      warning(paste("This file", temp[i], "is empty.", sep = " "))
      next()
    }

    #Conditional statemnt which reads in the portion the file which has Latitude,Longitude, Cruise,
    #Station and haul. If header information is missing it will kick out of the loop and go to the
    #next BON.up file. A warning will be generated.
    if(length(grep("@ ", full_table,ignore.case = TRUE)) == 0){

      no_head_files[[i]] <- paste("This file", temp[i],
                                 "has no header info needs to be reprocesed.", sep = " ")

      warning(paste("This file", temp[i], "has no header info, needs to be reprocesed.", sep = " "))

      next() #passes out of the loop to the next BON file. Will not be read.

    }else{
      #This block is looking for the header information which is placed into the .up
      #file by the PERL script, these located near the base of all the header informaiton
      #prior to the columns of oceanographic data. This information is designated by an
      # @ symbol and then the name.

      #LAT
      lat_id <- if(is.na(str_trim(unlist(strsplit(
        full_table[grep("@ Latitude?", full_table,ignore.case = TRUE)], ":")))[2])){
        lat_id <- as.double(NA) #If Latitude is missing it will sub in an NA.
      }else{
        lat_id <-str_trim(unlist(strsplit(
        full_table[grep("@ Latitude?", full_table,ignore.case = TRUE)], ":"))[2],
        side = "both")} #Pulls out Latitdue and trims off white space.
      #LON
      long_id <- if(is.na(str_trim(unlist(strsplit(
        full_table[grep("@ Longitude?", full_table,ignore.case = TRUE)], ":")))[2])){
        long_id <- as.double(NA) #If Longitude is missing it will sub ab NA
      }else{
        long_id <- str_trim(unlist(strsplit(
        full_table[grep("@ Longitude?", full_table,ignore.case = TRUE)], ":"))[2],
        side = "both") } #Pulls out Longitude and trims off white space.
      #DATE
      date_id <- if(is.na(str_trim(unlist(strsplit(
        full_table[grep("@ Date?", full_table,ignore.case = TRUE)], ":")))[2])){
        date_id <- as.character(NA) #If the date is missing subs an NA.
      }else{
        date_id <- str_trim(unlist(strsplit(
        full_table[grep("@ Date?", full_table,ignore.case = TRUE)],":"))[2],
        side = "both")} #Pulls out date and trims white space.
      #TIME
      time_id <-  if(is.na(str_trim(unlist(strsplit(
        full_table[grep("@ Cast?", full_table,ignore.case = TRUE)], ":")))[2])){
        time_id <- as.character(NA) #If time is missing subs an NA.
      }else{
        time_id <- strftime(as.POSIXct(paste(date_id,str_pad(str_trim(unlist(strsplit(
        full_table[grep("@ Cast?", full_table,ignore.case = TRUE)],":"))[2],
        side = "both"),width = 4, side = "left", pad = "0"), sep = "-"),
        format = "%d%b%y-%H%M", tz = "UTC"), format = "%H:%M:%S", tz = "UTC")}
        #Pulls out the cast time, trims white space, and adds a leading zero if needed.
        #Then converts it into a POSIxct date-time formate in UTC time. This assumes that
        #the time entered into MasterCod is in UTC and not another time zone.
      #CRUISE
      cruise_id <- if(is.na(str_trim(unlist(strsplit(
        full_table[grep("@ Cruise?", full_table,ignore.case = TRUE)], ":")))[2])){
        cruise_id <- as.character(NA)#If Cruise is missing subs an NA
      }else{
        cruise_id <- toupper(str_trim(unlist(strsplit(
        full_table[grep("@ Cruise?", full_table,ignore.case = TRUE)], ":"))[2],
        side = "both"))} #Pulls out the Cruise name, trims the white space, and makes all
        #letters uppercase.
      #STATION_NAME
      station_id_p <- if(length(full_table[grep("@ Station?", full_table,ignore.case = TRUE)])!=0){
        station_id_p <- str_trim(unlist(strsplit(
        full_table[grep("@ Station?", full_table,ignore.case = TRUE)], ":"))[2], side = "both")
        #Pulls out station name and trims white space.
        }else{
          station_id_p <- as.integer(NA)}#If station name is missing subs an NA
      #HAUL_NAME
      haul_id_p <- if(length(full_table[grep("@ Haul?", full_table,ignore.case = TRUE)]) != 0 ){
        haul_id <- str_trim(unlist(strsplit(
        full_table[grep("@ Haul?", full_table,ignore.case = TRUE)], ":"))[2], side = "both")
        #Pulls out haul name and trims white space.
        }else{
        haul_id <- as.integer(NA) } #If haul name is missing subs in an NA

      #GRID
      grid_id <- if(length(full_table[grep("@ Grid?", full_table,ignore.case = TRUE)]) == 0){
        grid_id <- as.character(NA) #If Grid name is missing subs in an NA
      }else if (is.na(str_trim(unlist(strsplit(
        full_table[grep("@ Grid?", full_table,ignore.case = TRUE)], ":")))[2])){
        grid_id <- as.character(NA)
        #if the field after : is blank then subs in an NA
      }else if(str_detect(str_trim(unlist(strsplit(
        full_table[grep("@ Grid?", full_table,ignore.case = TRUE)], ":"))[2], side = "both"),
        "BON\\") == TRUE){
        grid_id <- as.character(NA)
        #Handles if the BON# was entered into FOCI_GRID
      }else {
        grid_id <- toupper(str_trim(unlist(strsplit(
          full_table[grep("@ Grid?", full_table,ignore.case = TRUE)], ":"))[2], side = "both"))}
      #Pulls out Grid name,trims the white space, and make the letters uppercase.

      #Sometimes the Station Id is actually the FOCI_GRID designation, this will push a station_name
      #which isn't a number into the grid id.

      station_id <- if(is.na(suppressWarnings(as.numeric(station_id_p))) == TRUE){
        station_id <- as.integer(NA)
      }else if(str_detect(station_id_p,"[.]") == TRUE){
        station_id <- unlist(strsplit(station_id_p,"[.]"))[1]
      }else{
        station_id <- station_id_p
      }
      #If Grid name is empty and the station name is actually the Foci grid, this will toggle the
      #Station name into the Grid column.
      grid_id <- if(is.na(suppressWarnings(as.numeric(station_id))) == TRUE &
                   is.na(grid_id) == TRUE){
        grid_id <- station_id_p
      }else{
        grid_id <- grid_id
      }

      #Sometimes Station is St.H this will pull out haul number if there isn't a haul number.
      haul_id <- if(is.na(haul_id_p) == TRUE &
                   !is.na(station_id_p) &
                   str_detect(station_id_p, "[.]") == TRUE){
        haul_id <- unlist(strsplit(station_id_p, "[.]"))[2]
      }else if(is.na(haul_id_p) == TRUE){
        haul_id <- as.integer(NA)
      }else{
        haul_id <- haul_id_p
      }

      #Depth
      depth_id <- if(length(full_table[grep("@ Water?", full_table,ignore.case = TRUE)]) != 0 ){
        depth_id <- str_trim(unlist(strsplit(
          full_table[grep("@ Water?", full_table,ignore.case = TRUE)], ":"))[2], side = "both")
        #Pulls out depth and trims the white space
      }else{
        depth_id <- as.integer(NA) } #if depth is missing will put in an NA

      #Instrument type
      instrument_id <- if(length(full_table[grep("@ Instrument?", full_table, ignore.case = TRUE)]) != 0){
        instrument_id <- str_trim(unlist(strsplit(
          full_table[grep("@ Instrument??", full_table,ignore.case = TRUE)], ":"))[2], side = "both")
        #Pulls out the type of instrument is used
      }else{
        instrument_id <- as.character(NA)#If instrument is missing subs an NA
      }
      #Get LAT and LON into format so it can be converted to decimal degrees.
      lat_id <- paste(unlist(strsplit(lat_id, " "))[1], unlist(strsplit(lat_id, " "))[2], sep = " ")

      long_id <- paste(unlist(strsplit(long_id, " "))[1], unlist(strsplit(long_id, " "))[2], sep = " ")
    }
    #Conditional statement for physical data located at the end of the BON.up file. If the data is
    #present it will be added to final dataframe. If it isn't then NA's will show up for that
    #station/haul, it will generate a warning that no data was present.

    if (grep("END", full_table) >= length(full_table)){

      bongo_data <- data.frame(PRESSURE = as.double(NA),
                             TEMPERATURE1 = as.double(NA),
                             CONDUCTIVITY1 = as.double(NA),
                             SALINITY1 = as.double(NA),
                             SIGMA_T = as.double(NA),
                             FLAG = as.character(NA))

      no_data_files[[i]] <- paste("This file", temp[i],"has no data associated with it.",
                                 sep = " ")

      warning(paste("This file", temp[i],"has no data associated with it.", sep = " "))

    }else{
      #In the older data more types of data were selected, which means it will look for the
      #correct header and then convert to prefered column name. Using match if order of vars
      #is not always the same.
      bongo_table <- read.table(temp[i], skip = grep("END", full_table), sep = "", header = FALSE)

      col_length <- length(names(bongo_table))

      col_nums <- as.character(seq(0,(col_length -1), by = 1))

      # These are all the different ways over since 1996 that the seasoft software
      #designated the names to the columns of data.
      pref_col_names<- c("timeS: time [s]",
                         "timeS: Time, Elapsed [seconds]",
                         "timeJ: Julian Days",
                         "timeH: Time, Elapsed [hours]",
                         "timeS: Time, Elapsed [seconds]",
                         "timeJ: time [julian days]",
                         "pr: pressure [db]",
                         "prSM: Pressure, Strain Guage [db]",
                         "prSM: Pressure, Strain Gauge [db]",
                         "prdM: Pressure, Strain Gauge [db]",
                         "prDM: Pressure, Strain Gauge [db]",
                         "t068: temperature, IPTS-68 [deg C]",
                         "t090: temperature, ITS-90 [deg C]",
                         "t090C: temperature, ITS-90 [deg C]",
                         "t090C: Temperature [ITS-90, deg C]",
                         "t4990C: Temperature [ITS-90, deg C]",
                         "tv290C: Temperature [ITS-90, deg C]",
                         "c0mS/cm: conductivity [mS/cm]",
                         "c0mS/cm: Conductivity [mS/cm]",
                         "c0S/m: Conductivity [S/m]",
                         "sal00: salinity, PSS-78 [PSU]",
                         "sal00: Salinity, Practical [PSU]",
                         "sal00: Salinity [PSU]",
                         "density00: Density [density, Kg/m^3]",
                         "sigma-t00: density, sigma-t [kg/m^3]",
                         "sigma-t00: Density [sigma-t, Kg/m^3 ]",
                         "sigma-t00: Density [sigma-t, kg/m^3 ]",
                         "flag:  0.000e+00",
                         "flag: flag",
                         "nbin: number of scans per bin",
                         "depSM: Depth [salt water, m]",
                         "depS: depth, salt water [m]",
                         "svCM: Sound Velocity [Chen-Millero, m/s]",
                         "scan: scan number",
                         "dm: dynamic meters")
      #This names them with the standardized columns name.
      names(pref_col_names) <-  c("Time1","Time2","Time3","Time4","Time5","Time6",
                                 "PRESSURE","PRESSURE","PRESSURE","PRESSURE","PRESSURE",
                                 "TEMPERATURE1","TEMPERATURE1","TEMPERATURE1","TEMPERATURE1",
                                 "TEMPERATURE1",
                                 "TEMPERATURE1","CONDUCTIVITY1","CONDUCTIVITY1",
                                 "CONDUCTIVITY1","SALINITY1","SALINITY1","SALINITY1",
                                 "SIGMA_T","SIGMA_T","SIGMA_T","SIGMA_T",
                                 "FLAG","FLAG","Bins","depth","depth",
                                 "sound","scan","dm")

            colum_names <- vector()
      #Over the course of the years there have been as few as four coulmns and and many as
      #nine columns of data after *END* in the up files. This grabs all the column names
      # and gives them the standardized names,ones we don't use or where in consistant use
      #are dropped later.
            for(n in 1:col_length){

            name0 <- unlist(strsplit(
                            full_table[grep(paste("# name",col_nums[n], sep = " "),
                            full_table, ignore.case = TRUE)], "= "))[2]

            colum_names[n] <- names(pref_col_names[pmatch(name0, pref_col_names)])}


      colnames(bongo_table) <- colum_names

      #Sometimes depth was calculated by the seasoft software,these logical statements are for
      #determing if the depth column needs to be calculated from pressure or if it can be used as
      #is.Also incase there is no pressure column, but only a depth column.
      dep_and_press <- sum(colum_names %in% c("depth", "PRESSURE")) == 2 #TRUE
      dep_only <- sum(colum_names %in% c("depth")) == 1 #TRUE
      press_only <- sum(colum_names %in% c("PRESSURE")) == 1 #TRUE

      bongo_data <- if(nrow(bongo_table) != 0 & press_only == TRUE){

        bongo_data <- bongo_table %>%
          mutate(PRESSURE = as.double(PRESSURE))%>%
          mutate(TEMPERATURE1 = as.double(TEMPERATURE1))%>%
          mutate(CONDUCTIVITY1 = as.double(CONDUCTIVITY1))%>%
          mutate(SALINITY1 = as.double(SALINITY1))%>%
          mutate(SIGMA_T = as.double(SIGMA_T))%>%
          mutate(FLAG = as.character(FLAG))
      }else if (nrow(bongo_table) != 0 & dep_only == TRUE &
                press_only == FALSE){

        bongo_data <- bongo_table %>%
          mutate(PRESSURE = as.double(depth))%>%
          mutate(TEMPERATURE1 = as.double(TEMPERATURE1))%>%
          mutate(CONDUCTIVITY1 = as.double(CONDUCTIVITY1))%>%
          mutate(SALINITY1 = as.double(SALINITY1))%>%
          mutate(SIGMA_T = as.double(SIGMA_T))%>%
          mutate(FLAG = as.character(FLAG))

      }else{
        #if the table is empty this gives as dataframe of NA's bc the table has zero rows,
        #will throw error of data frame is empty and bomb the pressure calculations and
        #rowbind
        bongo_data <- data.frame(
          PRESSURE = as.double(NA),
          TEMPERATURE1 = as.double(NA),
          CONDUCTIVITY1 = as.double(NA),
          SALINITY1 = as.double(NA),
          SIGMA_T = as.double(NA),
          FLAG = as.character(NA))
      }



    }



    bongo_data <- bongo_data %>%
      mutate(LAT = lat_id)%>%
      mutate(LAT = as.numeric(conv_unit(LAT,
                                             from = 'deg_dec_min', to = 'dec_deg')))%>%
      mutate(LON = long_id)%>%
      mutate(LON = -1*(as.numeric(conv_unit(LON,
                                             from = 'deg_dec_min', to = 'dec_deg'))))%>%
      mutate(DATE = as.POSIXct(date_id,format = "%d%b%y", tz = "UTC"))%>%
      mutate(TIME = time_id)%>%
      mutate(CRUISE = cruise_id)%>%
      mutate(STATION_NAME = as.character(station_id))%>% #Station needs is character not always consitstent
      mutate(HAUL_NAME = as.integer(haul_id))%>%
      mutate(DIRECTORY =  as.character(temp[i]))%>%
      mutate(FOCI_GRID = as.character(grid_id))%>%
      mutate(DEPTH_BOTTOM = suppressWarnings(as.integer(depth_id)))%>%
      mutate(INSTRUMENT = as.character(instrument_id))



    bongo_data <- if(nrow(bongo_table) > 1 &
                   dep_only == TRUE &
                   press_only == FALSE){

      bongo_data <- bongo_data %>% mutate(DEPTH = as.integer(PRESSURE))

    }else if (nrow(bongo_table) > 1 &
              dep_only == FALSE &
              press_only == TRUE){
    #Depth calaculated using AN69:Conversion of Pressure to Depth, requires Decimal degrees.
    #http://www.seabird.com/document/an69-conversion-pressure-depth
      bongo_data <- bongo_data %>% mutate(DEPTH = as.integer((((((-1.82e-15  * PRESSURE + 2.279e-10 )*
                                      PRESSURE - 2.2512e-5 ) * PRESSURE + 9.72659) *PRESSURE)/
                                   (9.780318 * ( 1.0 + ( 5.2788e-3  + 2.36e-5  *
                                                           (sin(LAT/57.29578)^2)) *
                                                   sin(LAT/57.29578)^2)) + 1.092e-6 * PRESSURE)))
    }else{
      bongo_data <- bongo_data %>% mutate(DEPTH = as.integer(NA))
    }



    #Rearranges the columns so they are in the correct format for import into the Oracle database.
    bongo_data <- bongo_data %>% dplyr::select(CRUISE, STATION_NAME, HAUL_NAME, FOCI_GRID, DATE,
                                           TIME, LAT, LON, DEPTH_BOTTOM, DEPTH,PRESSURE,
                                           TEMPERATURE1, SALINITY1,  CONDUCTIVITY1,
                                           SIGMA_T, INSTRUMENT, DIRECTORY)%>%
      filter(TEMPERATURE1 >= -3 & TEMPERATURE1 <= 20)%>% #filters out bad temps
      filter(SALINITY1 >= 0.05  & SALINITY1 <= 38) #filters out bad salinities/conductivities


    #Appends the data frame to the Cruise data list
    cruise_data[[i]] <- bongo_data
    #print(unique(bongo_data[,"DIRECTORY"]))#These are for trouble shooting
    #print(i)These are for trouble shooting
  }

  #Genterates a text file of problem BON.up files
  no_data_files <- unlist(no_data_files)
  No_head_files <- unlist(no_head_files)

  problem_files <- append(no_data_files, no_head_files)


  #Outside the loop this takes the list of dataframes and binds them together into a
  #single cruise. The extra column names are requested for future input into EcoDATT.
  cruise_data_all <- as.data.frame(dplyr::bind_rows(cruise_data))%>%
    mutate(CONDUCTIVITY2 = as.double(NA))%>%
    mutate(FLOUROMETER = as.double(NA))%>%
    mutate(OXYGEN1 = as.double(NA))%>%
    mutate(OXYGEN2 = as.double(NA))%>%
    mutate(PAR = as.double(NA))%>%
    mutate(PH = as.double(NA))%>%
    mutate(POTENTIAL_DENSITY = as.double(NA))%>%
    mutate(SALINITY2 = as.double(NA))%>%
    mutate(TEMPERATURE2 = as.double(NA))%>%
    mutate(TRANS = as.double(NA))%>%
    mutate(COMMENTS_SEACAT_CTD = as.character(NA))%>%
    mutate(GEAR_NAME = as.character("CAT"))%>%
    mutate(NET = as.integer(0))


  #Make a summary of the FastCat data and write a .txt
  summary_fc <- summary(cruise_data_all %>%
                                select(LAT, LON, DEPTH_BOTTOM, DEPTH, PRESSURE, TEMPERATURE1,
                                       SALINITY1,  CONDUCTIVITY1, SIGMA_T))


  how_many_tows <- unique(paste(cruise_data_all$STATION_NAME,
                                cruise_data_all$HAUL_NAME, sep = "."))

  tows <- if(length(how_many_tows) < 2){
      tows <- length(unique(cruise_data_all$FOCI_GRID))
    } else {
      tows <- length(how_many_tows)
    }

  cruise_name_check <- unique(cruise_data_all$CRUISE)
  station_name_num <- length(unique(cruise_data_all$STATION_NAME))
  foci_grid_name_check <- unique(cruise_data_all$FOCI_GRID)
  haul_name_check <- unique(cruise_data_all$HAUL_NAME)
  time_range_check <- range(cruise_data_all$DATE, na.rm = TRUE)
  temp_range_check <- round(range(cruise_data_all$TEMPERATURE1, na.rm = TRUE), digits = 2)
  temp_mean <- round(mean(cruise_data_all$TEMPERATURE1, na.rm = TRUE), digits = 2)
  sal_range_check <- round(range(cruise_data_all$SALINITY1, na.rm = TRUE), digits = 2)
  sal_mean <- round(mean(cruise_data_all$SALINITY1, na.rm = TRUE), digits = 2)
  depth_range_check <- round(max(cruise_data_all$DEPTH, na.rm = TRUE), digits = 2)
  lat_range_check <- round(range(cruise_data_all$LAT, na.rm = TRUE), digits = 3)
  lon_range_check <- round(range(cruise_data_all$LON, na.rm = TRUE), digits = 3)

  cruise_summary <- paste("Quick Cruise FastCat Summary.\n\n", "During the cruise, ",
                          cruise_name_check[1], ", there were ", tows,
        " tows with depth, temperature, and salinity collected.",
        " Tows were taken during the time period from ",
        time_range_check[1], " to ",
        time_range_check[2], ". ", "Temperature ranged from ", temp_range_check[1], " to ",
        temp_range_check[2], " with a mean of ", temp_mean, "C. ",
        "Salinity ranged from ", sal_range_check[1], " to ", sal_range_check[2],
        " with a mean of ", sal_mean, " PSU. ", "The deepest tow was ", depth_range_check,
        " meters.", " The Southern most bongo tow was ", lat_range_check[1], " and the furthest North was ",
        lat_range_check[2], " Latitude. The Western most bongo tow was ", lon_range_check[1],
        " and the Eastern most was ", lon_range_check[2], " Longitude.",
        "\n\nIf any of this looks suspect,",
        "check for the values in the .csv file and then correct in MasterCOD.",
        " After this is done, re-run the Perl script and then re-run FastrCAT.\n\n",
        "SUMMARY\n\n", "CRUISE_NAMES\n", str_c(cruise_name_check, collapse = ", "),
        "\n\nSTATION_HAUL_NAMES\n",
        str_c(str_replace_na(how_many_tows), collapse = ", "), "\n\nFOCI_GRID_NAMES\n",
        str_c(str_replace_na(unique(cruise_data_all$FOCI_GRID)), collapse = ", "), "\n\nBOTTOM_DEPTHS\n",
        str_c(str_replace_na(summary_fc[-c(2,5),3]), collapse = "\n"), "\n\nTOW_DEPTHS\n",
        str_c(str_replace_na(summary_fc[-c(2,5),4]), collapse = "\n"), "\n\nPRESSURE\n",
        str_c(str_replace_na(summary_fc[-c(2,5),5]), collapse = "\n"), "\n\nTEMPERATURE\n",
        str_c(str_replace_na(summary_fc[-c(2,5),6]), collapse = "\n"), "\n\nSALINITY\n",
        str_c(str_replace_na(summary_fc[-c(2,5),7]), collapse = "\n"), "\n\nCONDUCTIVITY\n",
        str_c(str_replace_na(summary_fc[-c(2,5),8]), collapse = "\n"), "\n\nSIGMA_T\n",
        str_c(str_replace_na(summary_fc[-c(2,5),9]), collapse = "\n"),
        sep = "")

  #The DATE needs to be converted to a character otherwise excel tries to convert it to a
  #Date_time rather than leaving it a date.
  cruise_data_all$DATE %<>% as.character()

  #Pushes the CruiseData into the Global Environment
  if(GE == TRUE){
    fastcat_data <<- as.data.frame(dplyr::bind_rows(cruise_data))
  }

  #Creates the final file name
  file_name <- paste(cruise_id, "_forEcoDATT", sep = "", ".csv")
  #Writes file to folder


  #test <- "G:/SeaCatData/Processed/Nissa Test Folder"
  write_csv(cruise_data_all, file.path(current_path, file_name))
  write.table(problem_files, file = paste(current_path, paste(cruise_id, "warnings.txt", sep = "_")
                                         , sep = "/"), eol = "\n\n")
  write.table(cruise_summary, file = paste(current_path, paste(cruise_id, "cruise_summary.txt",
                                                               sep = "_"), sep = "/"),
              eol = "\n\n")
}

