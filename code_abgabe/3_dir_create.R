#***************************** dir_create *************************************#
#* STEP 3 
#* !!! IMPORTANT:  RUN this script only ONCE during the first run
#* !! Treat this carefully if run on  FAT/FAT32 or network-mounted file system 
#* This file set up the necessary directories needed for the output
#* 
#******************************************************************************#
#*
#***************************** Parameters *************************************#
# Set the name for main output folder used 
main_subfolder <- "output"

#******************************************************************************#
# Creates the output directory, to be used as a main directory for the following
dir.create(main_subfolder)

# Creation of sub-folders: 
dir.create(paste0(main_subfolder,"/models"))
dir.create(paste0(main_subfolder,"/visuals"))
dir.create(paste0(main_subfolder,"/visuals/figures"))
dir.create(paste0(main_subfolder,"/visuals/cifs_hazard"))
dir.create(paste0(main_subfolder,"/visuals/kaplan_meier_plots"))
dir.create(paste0(main_subfolder,"/visuals/confounder"))
dir.create(paste0(main_subfolder,"/visuals/lag_lead_window"))


# potential: Data with the data saved used for the specific analysis there,
# allowing for easier overview 
# dir.create(Data)