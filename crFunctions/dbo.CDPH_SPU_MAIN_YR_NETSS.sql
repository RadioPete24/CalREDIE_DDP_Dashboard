SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON
GO

--
-- Attempting to preserve unknown grants.   Don't forget to document grants!
--
IF OBJECT_ID(N'[dbo].[CDPH_SPU_MAIN_YR_NETSS]',N'P') IS NULL              -- Check SP existence.
    EXEC('CREATE PROC [dbo].[CDPH_SPU_MAIN_YR_NETSS] AS SET NOCOUNT ON;') -- Create empty stub for first run.
GO
-- ============================================================================================
-- Description:  CalREDIE DDP Stored Procedure [dbo].[CDPH_SPU_MAIN_YR_NETSS]
-- Author:       Unknown
-- Create Date:  Unknown
-- ============================================================================================
-- Author  Modify Date      Notes
--
-- VTH     6/10/2016        Removed Pertussis from this table.    Available in Extended only.
-- TMA	   8/22/2016		Added 'probable' casestatus to Hep C acute
-- TMA	   11/28/2016		Remove 'suspect' casestatus from Lyme disease (Mantis 11075)
-- TMA	   01/12/2017		Remove 'Acute Flaccid Myelitis' disease (Mantis 11126)
-- TMa	   1/20/2017		Mantis 11137 Update SiteCode
-- TMa	   4/5/2017			Mantis 11170 To remove event code 11700 and 10520
-- TMa	   12/21/2017		Mantis 12098 Update MMWR week to match CDC 2018 calendar
-- TMa 	   12/28/2017		Mantis 12122 Comment out "Delete code 11370 Rubella Cong" disease
-- TMa 	   1/2/2018			Mantis 12102 Update NETSS code 50237 Paratyphoid and 50242 Salmonellosis(excluding paratyphoid...)
-- ============================================================================================
ALTER Proc [dbo].[CDPH_SPU_MAIN_YR_NETSS]
AS

Delete CDPH_TBL_NETSS_Yr
DBCC CHECKIDENT(CDPH_TBL_NETSS_Yr,Reseed,0)

-- Set Date Range 
--Declare @beginYear datetime, @EndYear datetime
--Set @beginYear = DateAdd(yy, -1, GetDate()-1)
--Set @EndYear = GetDate() + 1

Declare @beginYear date, @EndYear date
Set @beginYear = DateAdd(MM, -18, GetDate()-1)
Set @EndYear = GetDate() + 1


Declare @RecordType char, @Pad char, @State char(2), @SiteCode char(3), @Count varchar(10), @AgeType int,
@DateType int, @Future int
set @RecordType = 'M'
Set @Pad = '9'
Set @State = '06'
Set @SiteCode = 'S01'
Set @Count = '00001'
Set @AgeType = '0'
Set @DateType = '1'
Set @Future = '99999'

Insert into CDPH_TBL_NETSS_Yr

Select
IncidentID,ID,@RecordType as 'RecordType', @Pad as 'Pad',@State as 'State',(Select substring(convert(varchar, year(DtEpisode)), 3, 2)) as 'Year',
IncidentID, 'S' + substring(RIGHT('000000'+CAST(IncidentID AS VARCHAR(7)),7), 1, 1) + '1' as 'SiteCode', DATEPART(wk,DtEpisode) as 'Week',
Disease,Disease,@Count as 'Count',LHJ as 'County',Substring(DOB,7,4) + Substring(DOB,1,2) + Substring(DOB,4,2) as 'DOB',Age as 'Age',
@AgeType AS 'AgeType',Sex,Race,Ethnicity,
CONVERT(VARCHAR(6), DtEpisode, 12) as EventDate,
CONVERT(VARCHAR(6), DtOnset, 12) AS 'EventDate1',
CONVERT(VARCHAR(6), DtDiagnosis, 12) AS 'EventDate2',
-- ******* Changed to DtCreate PER THB and Denise on 11/20/12
(Substring(dtlabcollect,9,2) + Substring(dtlabcollect,1,2) + Substring(dtlabcollect,4,2)) as 'EventDate3',
CONVERT(VARCHAR(6), DtCreate, 12) AS 'EventDate4',
@DateType as 'DateType',RStatus as 'CaseStatus',IStatus as 'Imported',OutbreakType, @Future as 'Future', 
-- PStatus added to handle LYME request
PStatus
From dbo.CDPH_SystemTab_Export
Where DtEpisode between @beginYear and @EndYear and DisShort <> 'TB' 
-- The line above was changed to DtEpisode from DtCreate on 1/28/15 to eliminate old episode dates 

-- Update below is used to match 2018 CDC Calendar
Update CDPH_TBL_NETSS_Yr
            Set [Week] = '01', [YEAR] = '18'
From CDPH_TBL_NETSS_Yr
Where EventDate in ('171231')

-- Update below is used to match 2015 CDC Calendar
Update CDPH_TBL_NETSS_Yr
	Set [Week] = DATEPART(wk,DtEpisode)-1 
From CDPH_TBL_NETSS_Yr ny
Inner Join CDPH_SystemTab_Export se on se.IncidentID = ny.IncidentID_Org
Where [YEAR] = '15'

---------------Update below is used to match 2016 CDC Calendar per Larry & Timmy 01/04/2016-------------------
Update CDPH_TBL_NETSS_Yr
            Set [Week] = DATEPART(wk,DtEpisode)-1 
From CDPH_TBL_NETSS_Yr ny
Inner Join CDPH_SystemTab_Export se on se.IncidentID = ny.IncidentID_Org
Where ny.[YEAR] = '16'
 
Update CDPH_TBL_NETSS_Yr
            Set [Week] = '52', [YEAR] = '15'
From CDPH_TBL_NETSS_Yr ny
Where EventDate in ('151227', '151228','151229','151230','151231','160101','160102')
----------------------------------------------------------------------------------------------------------------

-- For Dates 12/28/13 - 1/3/14 they need to be included as week #1
Update CDPH_TBL_NETSS_Yr
	Set [Week] = '1', [YEAR] = '14'
From CDPH_TBL_NETSS_Yr ny
Where EventDate in ('131229','131230','131231','140101','140102','140103','140104')

-- For Dates 12/29/13 - 12/31/14 they need to be included as week #1
Update CDPH_TBL_NETSS_Yr
	Set [Year] = '14'
From CDPH_TBL_NETSS_Yr ny
Where [Week] = '53' and [YEAR] = '13'

Update CDPH_TBL_NETSS_Yr
	Set [Year] = '13'
From CDPH_TBL_NETSS_Yr ny
Where [Week] = '53' and [YEAR] = '12'

-- For Dates 12/28/14 - 1/3/15 they need to be included as week #1
Update CDPH_TBL_NETSS_Yr
	Set [Week] = '53', [YEAR] = '14'
From CDPH_TBL_NETSS_Yr ny
Where EventDate in ('141228', '141229','141230','141231','150101','150102','150103')

-- Format the NETSS File
-- Need to add 0 to pad the IncidentId
Update CDPH_TBL_NETSS_Yr
	Set IncidentID = 
		Case 
			When (len(IncidentID)) = 1 then ('00000' + (Select IncidentID))
			When (len(IncidentID)) = 2 then ('0000' + (Select IncidentID))
			When (len(IncidentID)) = 3 then ('000' + (Select IncidentID))
			When (len(IncidentID)) = 4 then ('00' + (Select IncidentID))
			When (len(IncidentID)) = 5 then ('0' + (Select IncidentID))
			When (len(IncidentID)) = 6 then ((Select IncidentID))
			When (len(IncidentID)) = 7 then ((Select Right(IncidentID,6)))

		Else (Select IncidentID)
		End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

-- Need to add 0 to pad the single digit weeks
Update CDPH_TBL_NETSS_Yr
	Set [Week] = 
		Case 
			When (len([week])) = 1 then ('0' + (Select [week]))
		Else (Select [week])
		End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

-- Added Site Code to fix Ying's Issue with Sub Counties

Update CDPH_TBL_NETSS_Yr
	Set SiteCode = 
			Case 
				When County = 'Berkeley' then 'S' + substring(RIGHT('000000'+CAST(se.IncidentID AS VARCHAR(7)),7), 1, 1) + '8'		--'S98'
				When County = 'Long Beach' then 'S' + substring(RIGHT('000000'+CAST(se.IncidentID AS VARCHAR(7)),7), 1, 1) + '6'	--'S96'
				When County = 'Pasadena' then 'S' + substring(RIGHT('000000'+CAST(se.IncidentID AS VARCHAR(7)),7), 1, 1) + '4'		--'S94'
				Else SiteCode
			End
	From CDPH_TBL_NETSS_Yr n
	Inner join dbo.CDPH_SystemTab_Export se on n.IncidentID_Org = se.IncidentID
	
Update CDPH_TBL_NETSS_Yr
	Set Sex = 
			Case 
				When sex = 'M' then '1'
				When sex = 'MTF' then '2'
				When sex = 'F' then '2'
				When sex = 'FTM' then '1'
				When sex = 'U' then '9'
				When sex = 'O' then '9'
				When sex IS NULL then '9'
			Else '9'
			End
	From CDPH_TBL_NETSS_Yr
	--Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
	Set Race = 
			Case 
				When Race = 'American Indian/Alaska Native' then '1'
				When Race like 'Asian%' then '2'
				When Race like 'Pacific%' then '2'
				When Race like 'Black%' then '3'
				When Race = 'White' then '5'
				When Race = 'Other' then '8'
				When Race = 'Unknown' then '9'
			Else '9'
			End
	From CDPH_TBL_NETSS_Yr
	-- Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
	Set Ethnicity =
		Case 
			When Ethnicity = 'Hispanic/Latino' then '1'
			When Ethnicity = 'Hispanic or Latino' then '1'
			When Ethnicity = 'Non-Hispanic/Non-Latino' then '2'
			When Ethnicity = 'Not Hispanic or Latino' then '2'
			When Ethnicity = 'Unknown' then '9'
		Else '9'
			End
	From CDPH_TBL_NETSS_Yr
	--Where @NETSS_ID = ##tmp_NETSS.Id

-- Change Flag to D when NOT A CASE
Update CDPH_TBL_NETSS_Yr
		Set RecordType =
			Case when CaseStatus = 'Not A Case' then 'D'
		Else RecordType
			End
From CDPH_TBL_NETSS_Yr

-- Added the following code Per Denise 11/15/2012.  May need to add to all cases, but need confirmation from THB 
Update CDPH_TBL_NETSS_Yr
	Set RecordType = 
	Case 
		when CaseStatus = 'Previously Reported' then 'D'
	    when CaseStatus = 'Out of State' then 'D'
	    when CaseStatus = 'NPJ Incident' then 'D'
	    when CaseStatus = 'Not Reportable' then 'D'
		when CaseStatus = 'Not A Case' then 'D'
		when CaseStatus = 'Out of Country' then 'D'
	  Else RecordType
			End
From CDPH_TBL_NETSS_Yr


Update CDPH_TBL_NETSS_Yr
		Set CaseStatus =
			Case 
				When CaseStatus = 'Confirmed' then '1'
				When CaseStatus = 'Probable' then '2'
				When CaseStatus = 'Suspect' then '3'
				When CaseStatus = 'Unknown' then '9'
		Else '9'
			End
	From CDPH_TBL_NETSS_Yr

Update CDPH_TBL_NETSS_Yr
		Set Imported =
			Case 
				When Imported = 'Indigenous' then '1'
				When Imported like 'International%' then '2'
--Verify the following as Out
				When Imported = 'Out Of State' then '3'
				When Imported = 'Unknown' then '9'				
		Else '9'
			End
	From CDPH_TBL_NETSS_Yr
	--Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
		Set Outbreak =
			Case 
				When (len(Outbreak)) > 1 then '1'
				When Outbreak is null then '2'
				When (len(Outbreak)) < 1 then '2'
		Else '9'
			End
	From CDPH_TBL_NETSS_Yr
	--Where @NETSS_ID = ##tmp_NETSS.Id

--Update EventCode
Update CDPH_TBL_NETSS_Yr
	Set EventCode = 

					(
						Select netss from VCP_Disease vd
						Inner Join V_UnifiedCodeSet vu1 on vu1.ID =  vd.SubjCode_Id
						where VU1.FullName = EventCode
					)

From CDPH_TBL_NETSS_Yr
	
-- Update the DateType
Update CDPH_TBL_NETSS_Yr
	Set DateType =
			Case 
					when EventDate1 is not null then '1' 
					when EventDate2 is not null then '2'
					when EventDate3 is not null then '3'
					when EventDate4 is not null then '4'
				Else '9'
	End
From CDPH_TBL_NETSS_Yr

-- Pad the DOB in no Birthdate is given

Update CDPH_TBL_NETSS_Yr
	Set DOB = 
				Case 
					when DOB is null then '99999999' 
				Else (Select DOB)
				End
From CDPH_TBL_NETSS_Yr
	--Where @NETSS_ID = ##tmp_NETSS.Id

-- Pad the AGE in no Birthdate is given

Update CDPH_TBL_NETSS_Yr
	Set AGE = 
				Case 
					when AGE is null then '999' 
				Else (Select AGE)
				End
From CDPH_TBL_NETSS_Yr
	--Where @NETSS_ID = ##tmp_NETSS.Id

-- If AGE = 999 then change AGEType to 9
Update CDPH_TBL_NETSS_Yr
 Set AGEType = 
    Case 
     when AGE = '999' Then '9'
    Else (Select AGEType)
    End
From CDPH_TBL_NETSS_Yr
 --Where @NETSS_ID = ##tmp_NETSS.Id
			
-- Pad the AGE to make it 3 digits

Update CDPH_TBL_NETSS_Yr
	Set Age = 
		Case 
			When (len(Age)) = 1 then ('00' + (Select Age))
			When (len(Age)) = 2 then ('0' + (Select Age))
		Else (Select Age)
		End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id


-- Update the county with the proper Fips Code
Update CDPH_TBL_NETSS_Yr
		Set County =
			Case 
				When County = 'San Benito' then '069'
				When County = 'Stanislaus' then '099'
				When County = 'San Joaquin' then '077'
				When County = 'Trinity' then '105'
				When County = 'El Dorado' then '017'
				When County = 'Inyo' then '027'
				When County = 'Monterey' then '053'
				When County = 'Shasta' then '089'
				When County = 'Yolo' then '113'
				When County = 'Calaveras' then '009'
				When County = 'Mariposa' then '043'
				When County = 'Mono' then '051'
				When County = 'Amador' then '005'
				When County = 'Kern' then '029'
				When County = 'Contra Costa' then '013'
				When County = 'Glenn' then '021'
				When County = 'Lassen' then '035'
				When County = 'Fresno' then '019'
				When County = 'Merced' then '047'
				When County = 'Alpine' then '003'
				When County = 'San Bernardino' then '071'
				When County = 'Placer' then '061'
				When County = 'Sonoma' then '097'
				When County = 'Sierra' then '091'
				When County = 'San Francisco' then '075'
				When County = 'Madera' then '039'
				When County = 'San Diego' then '073'
				When County = 'Santa Cruz' then '087'
				When County = 'San Mateo' then '081'
				When County = 'Mendocino' then '045'
				When County = 'Solano' then '095'
				When County = 'Imperial' then '025'
				When County = 'Modoc' then '049'
				When County = 'Tuolumne' then '109'
				When County = 'Santa Barbara' then '083'
				When County = 'Riverside' then '065'
				When County = 'Sutter' then '101'
				When County = 'Humboldt' then '023'
				When County = 'Los Angeles' then '037'
				When County = 'Lake' then '033'
				When County = 'Butte' then '007'
				When County = 'Kings' then '031'
				When County = 'Plumas' then '063'
				When County = 'Napa' then '055'
				When County = 'Orange' then '059'
				When County = 'Tehama' then '103'
				When County = 'Nevada' then '057'
				When County = 'Del Norte' then '015'
				When County = 'Colusa' then '011'
				When County = 'Sacramento' then '067'
				When County = 'Siskiyou' then '093'
				When County = 'Alameda' then '001'
				When County = 'Marin' then '041'
				When County = 'Tulare' then '107'
				When County = 'San Luis Obispo' then '079'
				When County = 'Yuba' then '115'
				When County = 'Ventura' then '111'
				When County = 'Santa Clara' then '085'
				When County = 'Berkeley' then '001'
				When County = 'Long Beach' then '037'
				When County = 'Pasadena' then '037'
			Else '999'
			End
	From CDPH_TBL_NETSS_Yr
	--Where @NETSS_ID = ##tmp_NETSS.Id
-- When RecordType = D only Include Lines 1 - 17

Update CDPH_TBL_NETSS_Yr
		Set EventCode =
			Case 
				When RecordType = 'D' then '     '
			Else (Select EventCode)
				End
From CDPH_TBL_NETSS_Yr

Update CDPH_TBL_NETSS_Yr
		Set EventCode =
			Case 
				When EventDate < '171231' and (EventCode = '50236' or EventCode = '50242') then '11000'
			Else (Select EventCode)
				End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
		Set [Count] =
			Case 
				When RecordType = 'D' then '     '
			Else (Select [Count])
				End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
		Set County =
			Case 
				When RecordType = 'D' then '   '
			Else (Select County)
				End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
		Set DOB =
			Case 
				When RecordType = 'D' then '        '
			Else (Select DOB)
				End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
		Set AGE =
			Case 
				When RecordType = 'D' then '   '
			Else (Select AGE)
				End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
		Set AGEType =
			Case 
				When RecordType = 'D' then ' '
			Else (Select AGEType)
				End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
		Set SEX =
			Case 
				When RecordType = 'D' then ' '
			Else (Select SEX)
				End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
		Set RACE =
			Case 
				When RecordType = 'D' then ' '
			Else (Select RACE)
				End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
		Set Ethnicity =
			Case 
				When RecordType = 'D' then ' '
			Else (Select Ethnicity)
				End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
		Set EventDate =
			Case 
				When RecordType = 'D' then '      '
			Else (Select EventDate)
				End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
		Set DateType =
			Case 
				When RecordType = 'D' then ' '
			Else (Select DateType)
				End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
		Set CaseStatus =
			Case 
				When RecordType = 'D' then ' '
			Else (Select CaseStatus)
				End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
		Set Imported =
			Case 
				When RecordType = 'D' then ' '
			Else (Select Imported)
				End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
		Set Outbreak =
			Case 
				When RecordType = 'D' then ' '
			Else (Select Outbreak)
				End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

Update CDPH_TBL_NETSS_Yr
		Set Future =
			Case 
				When RecordType = 'D' then '     '
			Else (Select Future)
				End
From CDPH_TBL_NETSS_Yr
--Where @NETSS_ID = ##tmp_NETSS.Id

-- EVENT CODE 99999 OR  88888
Delete from CDPH_TBL_NETSS_Yr where EventCode in ('10273','10274','10280','10311','10312','10313','10314','10316','10319')

-- The Measles Deletion now runs from the Measles NETSS SSIS Job, because if it runs here it deletes all the Measles records before extracting
-- Remove Measles files that are not going to be sent
Delete From CDPH_TBL_NETSS_Yr Where (Disease like 'Measles' + '%')
-- End Measles Process

-- Rubella Syndrome, Congenital (CRS)
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10370' and CaseStatus <> '1')
Delete from CDPH_TBL_NETSS_Yr where (EventCode = '10200')

-- Mumps
Delete From CDPH_TBL_NETSS_Yr Where (EventCode = '10180')


Delete from CDPH_TBL_NETSS_Yr where EventCode = '10680'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '10050'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '10480'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '10105'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '10106'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '10103'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '11062'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '11061'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '11645'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '11064'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '11661'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '10309'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '10340'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '11050'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '10315'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '10220'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '10056'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '10049'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '10056'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '10660'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '50000'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '81111'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '81112'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '81113'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '81114'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '81115'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '81116'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '81117'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '81118'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '99999'
Delete from CDPH_TBL_NETSS_Yr where EventCode = '88888'

-- Chancroid
Delete from CDPH_TBL_NETSS_Yr Where (Disease = 'Chancroid' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Cholera
Delete From CDPH_TBL_NETSS_Yr Where Disease = 'Cholera' and CaseStatus <> '1'

-- Coccidioidomycosis
Delete From CDPH_TBL_NETSS_Yr Where Disease = 'Coccidioidomycosis' and CaseStatus <> '1'

-- Cryptosporidiosis 
Delete from CDPH_TBL_NETSS_Yr Where (Disease = 'Cryptosporidiosis' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Cyclosporiasis
Delete from CDPH_TBL_NETSS_Yr Where (Disease = 'Cyclosporiasis' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Anaplasmosis/Ehrlichiosis 
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '11091' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Giardiasis
Delete from CDPH_TBL_NETSS_Yr Where (Disease = 'Giardiasis' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Hantavirus Infections
Delete From CDPH_TBL_NETSS_Yr Where Disease like 'Hantavirus%' and CaseStatus <> '1'

-- 11550  Hemolytic Uremic Syndrome (HUS) without evidence of E. coli O157, other STEC, or Shiga toxin positive feces
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '11550' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Anthrax 10350
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10350' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Babesiosis	12010
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '12010' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Botulism, foodborne	10530
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10530' and CaseStatus <> '1')

-- Botulism, infant	10540
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10540' and CaseStatus <> '1')

-- Botulism, other unspecified	10548
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10548' and CaseStatus <> '1')

-- Botulism, wound	10549
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10549' and CaseStatus <> '1')

-- Anaplasmosis/Ehrlichiosis 11091

-- Brucellosis	10020
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10020' and CaseStatus <> '1') and (CaseStatus <> '2')

-- 11563 E-Coli
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '11563' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Legionellosis 
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10490' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Leprosy (Hansen Disease)
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10380' and CaseStatus <> '1')

-- Leptospirosis
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10390' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Listeriosis
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10640' and CaseStatus <> '1')

-- Malaria 
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10130' and CaseStatus <> '1') and (CaseStatus <> '3')

-- Paratyphoid
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '50236' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Plague
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10440' and CaseStatus <> '1') and (CaseStatus <> '2') and (CaseStatus <> '3')

-- Poliovirus Infection or Poliomyelitis
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10410' and CaseStatus <> '1')

-- Psittacosis
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10450' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Q Fever
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10257' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Human Rabies 10460
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10460' and CaseStatus <> '1')

-- Rocky Mountain Spotted Fever
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10250' and CaseStatus <> '1') and (CaseStatus <> '2')

--Salmonellosis
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '50242' and CaseStatus <> '1') and (CaseStatus <> '2')

-- SARS
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10575' and CaseStatus <> '1') and (CaseStatus <> '2') and (CaseStatus <> '3')

-- Shigellosis
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '11010' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Toxic Shock Syndrome  
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10520') --Delete all per Mantis 11170
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '11700')

-- Trichinosis
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10270' and CaseStatus <> '1') and (CaseStatus <> '2') and (CaseStatus <> '3')

-- Tularemia
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10230' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Typhoid Fever
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10240' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Varicella Hospitalization/Death
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '10030' and CaseStatus <> '1') and (CaseStatus <> '2')

-- Vibrio Infections (Non-Cholera)
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '11545' and CaseStatus <> '1')

-- Viral Hemorrhagic Fevers (e.g., Crimean-Congo, Ebola, Lassa and Marburg viruses)
Delete from CDPH_TBL_NETSS_Yr Where (EventCode = '11647' and CaseStatus <> '1') and (CaseStatus <> '3')

-- DMV Reportable
Delete from CDPH_TBL_NETSS_Yr where Disease = 'DMV Reportable'

-- EBOLI Contact
Delete From CDPH_TBL_NETSS_Yr Where Disease Like 'Ebola Contact%'

-- Hepatitis A
Delete From CDPH_TBL_NETSS_Yr Where Disease = 'Hepatitis A' and CaseStatus <> '1'

-- Hepatitis B, acute
Delete From CDPH_TBL_NETSS_Yr Where Disease = 'Hepatitis B, Acute' and CaseStatus <> '1'

-- Hepatitis B (Chronic)-Rina asked that all Hep B Chronic be removed 12/8/2010
Delete From CDPH_TBL_NETSS_Yr Where Disease = 'Hepatitis B, Chronic'  

-- Hepatitis B (Perinatal Case)
Delete From CDPH_TBL_NETSS_Yr Where (Disease = 'Hepatitis B (Perinatal Case)' and CaseStatus <> '1') 

-- Hepatitis C (Acute)
Delete From CDPH_TBL_NETSS_Yr Where Disease = 'Hepatitis C, Acute' and CaseStatus <> '1' and (CaseStatus <> '2')  --By Tma, added CaseStatus <> '2'

-- Hepatitis C (Chronic)-Rina asked that all Hep B Chronic be removed 12/8/2010
Delete From CDPH_TBL_NETSS_Yr Where Disease = 'Hepatitis C, Chronic' 

-- Meningococcal 
Delete From CDPH_TBL_NETSS_Yr Where (Disease like 'Meningococcal' + '%' and CaseStatus <> '1') and (CaseStatus <> '2') 

-- Diphtheria
Delete From CDPH_TBL_NETSS_Yr Where (Disease like 'Diphtheria' + '%' and CaseStatus <> '1') and (CaseStatus <> '2') and (CaseStatus <> '3')

-- Haemophilus 
Delete From CDPH_TBL_NETSS_Yr Where (Disease like 'Haemophilus Influ' + '%' and CaseStatus <> '1') and (CaseStatus <> '2') and (CaseStatus <> '3')

-- Rubella
--Delete From CDPH_TBL_NETSS_Yr Where (Disease like 'Rubella' + '%' and CaseStatus <> '1')

-- Polio
Delete From CDPH_TBL_NETSS_Yr Where (Disease like 'Polio' + '%' and CaseStatus <> '1')

--Tetanus
Delete From CDPH_TBL_NETSS_Yr Where (Disease like 'Tetanus' + '%' and CaseStatus <> '1') and (CaseStatus <> '2') and (CaseStatus <> '3') and (CaseStatus <> '9')

-- Pertussis
Delete From CDPH_TBL_NETSS_Yr Where (Disease like 'Pertussis' + '%')

-- PER THB 12/13/2012 Although reportable to CDC, Lyme disease cases will not be transmitted to CDC until the Process status is set to “Closed by State”
Delete From CDPH_TBL_NETSS_Yr Where (Disease like 'Lyme' + '%' and PStatus <> 'Closed by State') 

-- Mantis 11075 - supspect for Lyme disease cases will not be transmitted to CDC
Delete From CDPH_TBL_NETSS_Yr Where (Disease like 'Lyme' + '%' and CaseStatus <> '1') and (CaseStatus <> '2') 

Delete From CDPH_TBL_NETSS_Yr Where (Disease like 'Polio' + '%' and PStatus <> 'Closed by State') 

Delete From CDPH_TBL_NETSS_Yr Where (Disease like 'Diphtheria' + '%' and PStatus <> 'Closed by State') 

Delete From CDPH_TBL_NETSS_Yr Where (Disease like 'smallpox' + '%' and PStatus <> 'Closed by State') 

-- Delete all records with no Site Codes per Ying 11/21/2011
Delete From CDPH_TBL_NETSS_Yr Where EventCode = '' and RecordType <> 'D'

-- Mantis 11126 - Remove Acute Flaccid Myelitis
Delete From CDPH_TBL_NETSS_Yr Where (Disease = 'Acute Flaccid Myelitis')

-- The following code was added to count both E. Coli and STEC twice when it is with HUS per THB 12/1/2014

Insert Into CDPH_TBL_NETSS_Yr
	(DI_RowId, RecordType, Pad, State, Year, IncidentID, SiteCode, Week, Disease, EventCode, Count, County, DOB, Age, AgeType, Sex, Race, 
	Ethnicity, EventDate, EventDate1, EventDate2, EventDate3, EventDate4, DateType, CaseStatus, Imported, Outbreak, Future, PStatus)
Select 	
	DI_RowId, RecordType, Pad, State, Year, IncidentID, 'S99' as SiteCode, Week, Disease, '11550' as Eventcode, Count, County, DOB, Age, AgeType, Sex, Race, 
	Ethnicity, EventDate, EventDate1, EventDate2, EventDate3, EventDate4, DateType, CaseStatus, Imported, Outbreak, Future, PStatus
From CDPH_TBL_NETSS_Yr 
where EventCode = 11563 and Disease in ('E. coli O157 with HUS', 'STEC non-O157 with HUS')
GO
