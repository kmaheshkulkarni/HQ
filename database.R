############### SQL Server Global Conenction ##########

sql_data_con<-dbConnect(odbc(),
                        Driver="SQL Server",
                        Server ="fdxx90sqlvcl1.jdnet.deere.com",
                        Database = "HX_AssemblyDataPool",
                        port ="1433",
                        UID = "Jdaat_reporter",
                        PWD = "Jdaat2014"
)

###### selected_dept ######

# qr<-paste0("Select distinct deptid,Name from departments where name not like '%Archive' and 
#            name not like '%Test%' and archived_timestamp is null order by deptid")

all_symbols<- "[][!#$%()*,.:\"/;<=>@^_-`|~.{}_+?']"
Department <- read.csv("data/departments.csv")
# 
Stations<- dbGetQuery(sql_data_con, "SELECT DeptID, StationID, Name as StationName from stations")
SPGroup<- dbGetQuery(sql_data_con, "SELECT StationID, GroupID as SPGroupID, Name as SPGroupName from STATION_PARAMETER_GROUPS")
Param_Name_DB<- dbGetQuery(sql_data_con, "Select distinct ASSEMBLY_PARAMETERS.ParameterID, ASSEMBLY_PARAMETERS.Name as ParameterName,
                                          ASSEMBLY_PARAMETERS.GroupID as SPGroupID from ASSEMBLY_PARAMETERS inner join
                                           ASSEMBLY_RESULTS_DATA with(NoLOCK)
                                           on
                                           ASSEMBLY_PARAMETERS.ParameterID=ASSEMBLY_RESULTS_DATA.ParameterID
                                           where ASSEMBLY_PARAMETERS.GroupID
                                           in
                                           (Select groupid from dbo.station_parameter_groups)
                                           and ASSEMBLY_PARAMETERS.Disabled_Timestamp is null
                                           and ASSEMBLY_PARAMETERS.DataType = 7
			                                     and ASSEMBLY_PARAMETERS.EnableParameter = 1
                                           and ASSEMBLY_RESULTS_DATA.MaxValue<>''
			                                     and ASSEMBLY_RESULTS_DATA.ParameterChildName = 'Torque'")
# 
# parameter_Name$Name<- as.character(parameter_Name$Name)
# STATION_PARAMETER_GROUPS$Name <- as.character(STATION_PARAMETER_GROUPS$Name)

StationDept <- inner_join(Department, Stations, by= "DeptID")

StationDB<- inner_join(StationDept, SPGroup, by= "StationID")

ParameterDB<- inner_join(StationDB, Param_Name_DB, by= "SPGroupID")
# 
# ParameterDB$DeptName <- gsub(all_symbols, "", ParameterDB$DeptName, perl=TRUE)
# ParameterDB$DeptName <- gsub('-', '', ParameterDB$DeptName, perl=TRUE)
# 
# ParameterDB$StationName<- gsub(all_symbols, "", ParameterDB$StationName, perl=TRUE)
# ParameterDB$StationName<- gsub('-', '', ParameterDB$StationName, perl=TRUE)
# 
# ParameterDB$SPGroupName<- gsub(all_symbols, "", ParameterDB$SPGroupName, perl=TRUE)
# ParameterDB$SPGroupName<- gsub('-', '', ParameterDB$SPGroupName, perl=TRUE)
# 
# ParameterDB$ParameterName<- gsub(all_symbols, "", ParameterDB$ParameterName, perl=TRUE)
# ParameterDB$ParameterName<- gsub('-', '', ParameterDB$ParameterName, perl=TRUE)
# 
FinalDB<- ParameterDB
# 
# FinalDB<- write.csv(FinalDB, "data/FinalDB.csv")

# FinalDB<- read.csv("data/FinalDB.csv")
# stations$Name<- iconv(stations$Name, sub="")
# stations_dept$StationName<- iconv(stations_dept$StationName, sub="")
# parameter_Name$Name<- iconv(parameter_Name$Name, sub="")

Dept_Choice <- as.character(Department$DeptName)
# ST_Choice<- unique(FinalDB$StationName[FinalDB$DeptName == input$selected_dept])
# SPG_Choice<- unique(FinalDB$SPGroupName[FinalDB$StationName == input$selected_station])
# ParameterName_choice<- unique(FinalDB$ParameterName[FinalDB$SPGroupName == input$selected_SPGroup])
ST_Choice<- setNames(as.character(FinalDB$StationName),FinalDB$StationID)
SPG_Choice<- setNames(as.character(FinalDB$SPGroupName),FinalDB$SPGroupName)
ParameterName_choice<- setNames(as.character(FinalDB$ParameterName),FinalDB$ParameterID)
