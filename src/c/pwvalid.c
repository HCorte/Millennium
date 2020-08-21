/******************************************************************************
 *
 * Copyright Information
 *
 ******************************************************************************
 *
 * Copyright Accenture 2004
 *
 * The copyright to the computer program(s) herein is the property
 * of Accenture. The program(s) may be used and/or
 * copied only with the written permission of Accenture
 * or in accordance with the terms and conditions stipulated in
 * the agreement/contract under which the program(s) have been
 * supplied.
 ******************************************************************************/

/******************************************************************************** 
 *
 * Module Name     : Prized Wagers Validation
 *
 ********************************************************************************
 *
 * File Name       : pwvalid.c
 *
 * Author          : Accenture
 *
 * Date            : 05-08-2004
 *
 * Functions in this module:
 *
 *      main()
 * 		PWV_CallMenu()
 * 		PWV_DisplayWagerData()
 * 		PWV_efnCleanEnvironment()
 * 		PWV_efnCloseLog()
 * 		PWV_efnGetRTSWFiles()
 * 		PWV_efnInitializeBasicEnv()
 * 		PWV_efnInitializeEnvironment()
 * 		PWV_efnLogMessage()
 * 		PWV_efnOpenLog()
 * 		PWV_efnSearchRefNumInFiles()
 *		PWV_efnSearchAllFile()
 *		PWV_CopyTicketInfo()
 * 		PWV_GetReferenceNumber()
 *		PWV_GetTicketCDCDate()
 *		PWV_CallRTSMenu()
 *    PWV_GetTicketYear()
 *      
 * Modifications History
 * ==============================================================================
 * VER  DD-MM-YYYY  AUTHOR        COMMENTS
 *
 * V03  07-10-2013  Accenture     Single line comment substituted by block comment
 * V02  10-04-2013  Accenture     Changed PWV_GetTicketCDCDate() function signature
 *                                Created PWV_GetTicketYear() function
 *                                Added   PWV_GetTicketYear() call to main function
 *
 * Date: 05-08-2004  Author: Pedro Miguel Sousa
 * Comments: Stand Alone application for use by the Jury in the "Jury's Process"
 *	     that makes a verification of the prized wagers in a certain contest
 *	     by searching Wager Files from RTS EuroMillion Module.
 *
 *******************************************************************************/
 
 /*----------------------------------------------------------------------------*/
 /* Includes                                                                   */
 /*----------------------------------------------------------------------------*/
 #include "pwvalid.h"
 
#include <sys/types.h>
#include <sys/stat.h>

 /*---------------------------------------------------------------------------*/
 /* Internal variable declarations (static)                                   */
 /*---------------------------------------------------------------------------*/

/* Local file pointer to log file */
static FILE *gpfdLogFile=NULL;

/* Local translator for message priorities */
static char *gaszMessagePriorities[]=
{
  "INFORMACAO",
  "AVISO",
  "ERRO",
  "FATAL"
};

 /*----------------------------------------------------------------------------*/
 /* Global variable definitions                                                */
 /*----------------------------------------------------------------------------*/
 
 /*----------------------------------------------------------------------------*/
 /* Main Function                                                              */
 /*----------------------------------------------------------------------------*/
main(int argc, char **argv)
{
 	/* variable definitions	*/
 	PWV_ReturnValue eRetVal;
 	char szMessage		[PWV_LOG_MAXPATHLEN];
 	char filename       [PWV_LOG_MAXPATHLEN];

	int iMenuChoice = 0;
	Boolean bContinuePWV = B_FALSE;
	/* Bruno - 03-10-2006 */
	Boolean bFilesCopy = B_FALSE;
	
	int iCheckSumError;
	
	int iSysComRet;
	/* Function */
	
	/* Pointers */
	tProcessEnvironment *ptEnvironment = NULL;
	
	/* Pointer to a file */
   
	/* Initialization */
	if((ptEnvironment = (tProcessEnvironment *) malloc (sizeof(tProcessEnvironment))) == NULL)
	{
		PWV_efnLogMessage(PWV_MP_FATAL, "main", PWV_MALLOC_FAILURE_MSG);
		return PWV_RV_FATAL;
	}
	memset((tProcessEnvironment *) ptEnvironment, PWV_ZERO_MEM, sizeof(tProcessEnvironment));

   /*----------------------------------------------------------------------------*/
   /* Basic Environment Creation (LOG directory)                                 */
   /*----------------------------------------------------------------------------*/
	eRetVal = PWV_efnInitializeBasicEnv(ptEnvironment);
	if(PWV_RV_OK != eRetVal)
	{
		return PWV_RV_FATAL;
	}
	
   /*----------------------------------------------------------------------------*/
   /* Open Log Files                                                             */
   /*----------------------------------------------------------------------------*/
	eRetVal = PWV_efnOpenLog(ptEnvironment, PWV_MODULE_NAME);
	if(PWV_RV_OK != eRetVal)
	{
		return PWV_RV_FATAL;
	}

   /*----------------------------------------------------------------------------*/
   /* Initialize the Process Environment structure				                 */
   /*----------------------------------------------------------------------------*/
	eRetVal = PWV_efnInitializeEnvironment(ptEnvironment);
	if(PWV_RV_OK != eRetVal)
	{
		UTILS_CLOSE_LOG();
	}

   /*----------------------------------------------------------------------------*/
   /* Clean old files from PWVALID environment				                 	 */
   /*----------------------------------------------------------------------------*/
	eRetVal = PWV_efnCleanEnvironment(ptEnvironment);
	if(PWV_RV_OK != eRetVal)
	{
		UTILS_CLOSE_LOG();
	}

   /*----------------------------------------------------------------------------*/
   /* Starts the main processing                                                 */
   /*----------------------------------------------------------------------------*/

	/* Wellcome Message */
	memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
	sprintf(szMessage, "%s", PWV_WELLCOME_MSG);
	printf("\n%s", szMessage);

	/* ##CF1## Get RTS Wager Files #CF# */
	do
	{		
		/* RTS Menu */
		PWV_CallRTSMenu(&iMenuChoice);
		/* Get files */
		if(iMenuChoice == 1)
		{
			bContinuePWV = B_TRUE;
			eRetVal = PWV_efnGetRTSWFiles(ptEnvironment);
			if(PWV_RV_OK != eRetVal)
			{
				UTILS_CLOSE_LOG();
			}
			bFilesCopy = B_TRUE;			
		}
		else if(iMenuChoice == 2)
		{
			/* Continue */
			sprintf(filename, "%s%s", ptEnvironment->szInputFullPath, PWV_ALLWAGERSFILES_FILENAME);

			PWV_efnReadAllFilesToBuffer(ptEnvironment);

			bContinuePWV = B_FALSE;
		}
		else if(iMenuChoice == 3)
		{
			bContinuePWV = B_TRUE;
			/* Bruno 2007-01-25 - Deixa entrar na opcao 3 do menu */
			bFilesCopy = B_TRUE;
			if (bFilesCopy)
			{
			   eRetVal = PWV_efnGenerateCRC32(ptEnvironment);
			   if(PWV_RV_OK != eRetVal)
			   {
			   	   /* -- Bruno 03-10-2006 */
				   printf("%s",PWV_INFO_INVALID_CD_CHECK);
				   UTILS_CLOSE_LOG();
			   }
			   else 
			     printf("%s",PWV_INFO_OK_CD_CHECK);	
			 }
			 else
			    printf("%s",PWV_INFO_MENU_CD_CHECK);
		}
		else
		{
			/* EXIT APPLICATION */
			bContinuePWV = B_FALSE;
			PWV_efnLogMessage(PWV_MP_INFORMATION, "VAP", PWV_FINISHED_MESG);
			UTILS_CLOSE_LOG();
		}
	}while(bContinuePWV);
	
	/* *****************************************
	 * 	MENU
	 *	 1 - Insert Reference Number to search
	 *	 2 - Exit application
	 * *****************************************/
	do
	{
		/* MENU */
		PWV_CallMenu(&iMenuChoice);
		
		if(iMenuChoice == 1)
		{
			bContinuePWV = B_TRUE;
			
			/* V02 - Added */
			PWV_GetTicketYear(ptEnvironment);
			
			/* ##CF2## Get Wager Reference Number #CF# */
			iCheckSumError = 0;
			PWV_GetReferenceNumber(ptEnvironment, &iCheckSumError);
			/* ##CF3## Get Wager Reference Number #CF# */
			if(iCheckSumError == 0)
			{
				eRetVal = PWV_efnSearchRefNumInFiles(ptEnvironment);
				if(PWV_RV_OK != eRetVal)
				{
					UTILS_CLOSE_LOG();
				}
			}
		}
		else
		{
			/* EXIT APPLICATION */
			bContinuePWV = B_FALSE;
			/* move files from PWV INPUT to PWV SUCCESS directory */
			memset(ptEnvironment->szRunSysCom, PWV_ZERO_MEM, PWV_LEN_COM);
			sprintf(ptEnvironment->szRunSysCom, "%s%s%s%s %s", PWV_COM_MV_OVER, ptEnvironment->szInputFullPath,
					PWV_COM_GEN_STRING, ptEnvironment->szRTSWagerFilesExtension, ptEnvironment->szSuccessFullPath);
			if(system(ptEnvironment->szRunSysCom) != 1)
			{
				memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
				sprintf(szMessage, "%s", PWV_ERROR_MOVING_RTSWF_TO_SUCCESS);
				PWV_efnLogMessage(PWV_MP_ERROR, "main", szMessage);
				return PWV_RV_ERROR;
			}
		}
	}while(bContinuePWV);

   /*----------------------------------------------------------------------------*/
   /* Ends the main processing                                                   */
   /*----------------------------------------------------------------------------*/

   /*----------------------------------------------------------------------------*/
   /* Close Log Files                                                            */
   /*----------------------------------------------------------------------------*/
   
		/* PWVALID end message before LOG close --> log file */
	PWV_efnLogMessage(PWV_MP_INFORMATION, "VAP", PWV_FINISHED_MESG);
   		
   		/* close log */
		/* PWVALID end message after LOG close --> screen */
	UTILS_CLOSE_LOG();
} /* end main */
 
 /*----------------------------------------------------------------------------*/
 /* Functions                                                                  */
 /*----------------------------------------------------------------------------*/

/******************************************************************************
 *
 * Function:    PWV_efnInitializeBasicEnv
 *
 * Author:      Accenture 
 *
 * Date :       05-08-2004
 * 
 * Description: Initialize Basic Environment as Home and LOG directories
 *
 * Input Parameters:
 *
 * 	N/A
 *	
 * Output Parameters:
 *
 *	ptEnvironment: environment structure
 *   
 * Return Values:	PWV_RV_OK: Success
 *					PWV_RV_FATAL: Error
 *
 * Changes History:
 *
 ******************************************************************************/
PWV_ReturnValue
PWV_efnInitializeBasicEnv(tProcessEnvironment *ptEnvironment)
{	
	/* HOME dir (it is assumed that already exists)*/
	sprintf(ptEnvironment->szHomePath, "%s", PWV_HOME_PATH);

	/* LOG dir */
	sprintf(ptEnvironment->szLogPath, "%s", PWV_REL_LOG_PATH);
	sprintf(ptEnvironment->szLogFullPath, ptEnvironment->szHomePath, ptEnvironment->szLogPath);
	
	return PWV_RV_OK;
}

/******************************************************************************
 *
 * Function:    PWV_efnInitializeEnvironment
 *
 * Author:      Accenture 
 *
 * Date :       05-08-2004
 * 
 * Description: Initialize Environment variables
 *
 * Input Parameters:
 *
 * 	ptEnvironment: environment structure
 *	
 * Output Parameters:
 *
 *	ptEnvironment: environment structure
 *   
 * Return Values:	PWV_RV_OK: Success
 *					PWV_RV_ERROR: Error
 *
 * Changes History:
 *
 ******************************************************************************/
PWV_ReturnValue
PWV_efnInitializeEnvironment(tProcessEnvironment *ptEnvironment)
{
	/* RTS Wager Files dir */
	sprintf(ptEnvironment->szRTSWagersFilesPath, "%s", PWV_FULL_RTSWFILES_PATH);
	sprintf(ptEnvironment->szRTSWFilesFullPath, "%s", ptEnvironment->szRTSWagersFilesPath);
	
	/* RTS Wager Files Extension dir */
	sprintf(ptEnvironment->szRTSWagerFilesExtension, "%s", PWV_RTSWFILE_EXTENSION);
	
	/* INPUT dir */
	sprintf(ptEnvironment->szInputPath, "%s", PWV_REL_INPUT);
	sprintf(ptEnvironment->szInputFullPath, ptEnvironment->szHomePath, ptEnvironment->szInputPath);
	
	/* ERROR dir */
	sprintf(ptEnvironment->szErrorPath, "%s", PWV_REL_ERROR);
	sprintf(ptEnvironment->szErrorFullPath, ptEnvironment->szHomePath, ptEnvironment->szErrorPath);
	
	/* SUCCESS dir */
	sprintf(ptEnvironment->szSuccessPath, "%s", PWV_REL_SUCCESS);
	sprintf(ptEnvironment->szSuccessFullPath, ptEnvironment->szHomePath, ptEnvironment->szSuccessPath);
	
	return PWV_RV_OK;
}

/******************************************************************************
 *
 * Function:    PWV_efnCleanEnvironment
 *
 * Author:      Accenture 
 *
 * Date :       05-08-2004
 * 
 * Description: Clean old files from the application environment directories
 *
 * Input Parameters:
 *
 * 	ptEnvironment: environment structure
 *	
 * Output Parameters:
 *
 *	N/A
 *   
 * Return Values:	PWV_RV_OK: Success
 *					PWV_RV_ERROR: Error
 *
 * Changes History:
 *
 ******************************************************************************/
PWV_ReturnValue
PWV_efnCleanEnvironment(tProcessEnvironment *ptEnvironment)
{
	char szMessage	[PWV_LOG_MAXPATHLEN];

	memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
	
	/* cleaning INPUT dir */
	memset(ptEnvironment->szRunSysCom, PWV_ZERO_MEM, PWV_LEN_COM);
	sprintf(ptEnvironment->szRunSysCom, "%s%s%s", PWV_COM_RM_OVER, ptEnvironment->szInputFullPath, PWV_ALL_FILES);
	system(ptEnvironment->szRunSysCom);
	/* cleaning ERROR dir */
	memset(ptEnvironment->szRunSysCom, PWV_ZERO_MEM, PWV_LEN_COM);
	sprintf(ptEnvironment->szRunSysCom, "%s%s%s", PWV_COM_RM_OVER, ptEnvironment->szErrorFullPath, PWV_ALL_FILES);
	system(ptEnvironment->szRunSysCom);
	/* cleaning SUCCESS dir */
	memset(ptEnvironment->szRunSysCom, PWV_ZERO_MEM, PWV_LEN_COM);
	sprintf(ptEnvironment->szRunSysCom, "%s%s%s", PWV_COM_RM_OVER, ptEnvironment->szSuccessFullPath, PWV_ALL_FILES);
	system(ptEnvironment->szRunSysCom);
	return PWV_RV_OK;
}

/******************************************************************************
 *
 * Function:    PWV_CallRTSMenu
 *
 * Author:      Accenture 
 *
 * Date :       05-08-2004
 * 
 * Description: Displays a menu to the user
 *
 * Input Parameters:
 *
 * 	N/A
 *	
 * Output Parameters:
 *
 *	iChoice: user's choice
 *   
 * Return Values:
 *
 * Changes History:
 *
 ******************************************************************************/
void PWV_CallRTSMenu(int *iChoice)
{
	char szMessage		[PWV_LOG_MAXPATHLEN];
	char szTempChoice 	[PWV_LOG_MAXPATHLEN];
	
	Boolean bNotDone = B_TRUE;
    	memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);

	do
	{
		memset(szTempChoice, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
		sprintf(szMessage, "%s", PWV_GET_RTSWFILES_MSG);
		printf("\n%s", szMessage);
		scanf("%s", szTempChoice);
		szTempChoice[1] = PWV_NULL_CHAR;
		if(((szTempChoice[0] - 48) != 1) && ((szTempChoice[0] - 48) != 2) && ((szTempChoice[0] - 48) != 3) && ((szTempChoice[0] - 48) != 4))
		{
			bNotDone = B_TRUE;
			memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
			sprintf(szMessage, "%s", PWV_INFO_INVALID_RTSMENU_OPTION);
			PWV_efnLogMessage(PWV_MP_INFORMATION, "VAP", szMessage);
		}
		else
		{
			bNotDone = B_FALSE;
			*(iChoice) = szTempChoice[0] - 48;
		}
	}while(bNotDone);
}

/******************************************************************************
 *
 * Function:    PWV_efnGetRTSWFiles
 *
 * Author:      Accenture 
 *
 * Date :       05-08-2004
 * 
 * Description: Copies RTS Wager Files from a specified directory to 
 *				application INPUT directory
 *
 * Input Parameters:
 *
 * 	ptEnvironment: environment structure
 *	
 * Output Parameters:
 *
 *	N/A
 *   
 * Return Values:	PWV_RV_OK: Success
 *					PWV_RV_ERROR: Error
 *
 * Changes History:
 *
 ******************************************************************************/
PWV_ReturnValue
PWV_efnGetRTSWFiles(tProcessEnvironment *ptEnvironment)
{
	char szMessage	[PWV_LOG_MAXPATHLEN];

	memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);

	/* Copy RTS Wager Files PWVALID INPUT */
	sprintf(szMessage, "%s", "A copiar fiheiros de apostas para o directorio de INPUT.");
	PWV_efnLogMessage(PWV_MP_INFORMATION, "VAP", szMessage);
	
	memset(ptEnvironment->szRunSysCom, PWV_ZERO_MEM, PWV_LEN_COM);
	sprintf(ptEnvironment->szRunSysCom, "%s%s%s%s %s", PWV_COM_CP_OVER, ptEnvironment->szRTSWFilesFullPath,
			PWV_COM_GEN_STRING, PWV_RTSWFILE_EXTENSION, ptEnvironment->szInputFullPath);
	if(system(ptEnvironment->szRunSysCom) != 1)
	{
		sprintf(szMessage, "%s", PWV_ERROR_COPYING_RTSWFILES);
		PWV_efnLogMessage(PWV_MP_ERROR, "PWV_efnGetRTSWFiles", szMessage);
		return PWV_RV_ERROR;
	}

	return PWV_RV_OK;
}

/******************************************************************************
 *
 * Function:    PWV_CallMenu
 *
 * Author:      Accenture 
 *
 * Date :       05-08-2004
 * 
 * Description: Displays a menu to the user
 *
 * Input Parameters:
 *
 * 	N/A
 *	
 * Output Parameters:
 *
 *	iChoice: user's choice
 *   
 * Return Values:
 *
 * Changes History:
 *
 ******************************************************************************/
void PWV_CallMenu(int *iChoice)
{
	char szMessage		[PWV_LOG_MAXPATHLEN];

	char szTempChoice 	[PWV_LOG_MAXPATHLEN];
	
	Boolean bNotDone = B_TRUE;

	do
	{
		sprintf(szMessage, "%s", PWV_MENU_MSG);
		printf("\n%s", szMessage);
		scanf("%s", szTempChoice);
		szTempChoice[1] = PWV_NULL_CHAR;
		if(((szTempChoice[0] - 48) != 1) && ((szTempChoice[0] - 48) != 2))
		{
			bNotDone = B_TRUE;
			memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
			sprintf(szMessage, "%s", PWV_INFO_INVALID_MENU_OPTION);
			PWV_efnLogMessage(PWV_MP_INFORMATION, "VAP", szMessage);
		}
		else
		{
			bNotDone = B_FALSE;
			*(iChoice) = szTempChoice[0] - 48;
		}
	}while(bNotDone);
}

/******************************************************************************
 *
 * Function:    PWV_GetTicketYear
 *
 * Author:      Accenture 
 *
 * Date :       10-04-2013
 * 
 * Description: Get Ticket Year from USER to further calculate the Reference Number
 *
 * Input Parameters:
 *
 * 	N/A
 *	
 * Output Parameters:
 *
 *	ptEnvironment: environment structure to store Ticket Year
 *   
 * Return Values:
 *
 * Changes History:
 *
 *  VER  DD-MM-YYYY  AUTHOR        COMMENTS
 *
 ******************************************************************************/
void PWV_GetTicketYear(tProcessEnvironment *ptEnvironment)
{
	char szMessage[PWV_LOG_MAXPATHLEN];
	char szTMPTicketYear[PWV_LOG_MAXPATHLEN];
	
	int iTicketYear;
	int iTMPTicketYear = 0;
	int iCntGRN = 0;

	Boolean bNotDone = B_TRUE;
	
	/* Get Ticket Year from USER */
	do
	{
		printf("\n%s", PWV_INSERT_TICKET_YEAR_MSG);
		scanf("%s", szTMPTicketYear);
		szTMPTicketYear[PWV_TICKET_YEAR + 1] = PWV_NULL_CHAR;

		for(iCntGRN = PWV_CICLE_LL; iCntGRN < PWV_TICKET_YEAR; iCntGRN++)
		{

			if((((szTMPTicketYear[iCntGRN] - 48) < 0) || ((szTMPTicketYear[iCntGRN] - 48) > 9)))
			{
				bNotDone = B_TRUE;
				memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
				sprintf(szMessage, "%s", PWV_INFO_INVALID_TICKET_YEAR);
				PWV_efnLogMessage(PWV_MP_INFORMATION, "VAP", szMessage);
				break; /* exit FOR cicle */
			}
			
			iTicketYear = (int) atoi(szTMPTicketYear);
			/* First EM contest - 35/2004, 02/10/2004 thru 08/10/2004 */
			/* Last  EM contest - 18/2011, 30/04/2011 thru 06/05/2011 */
			if(iTicketYear < 2004 || iTicketYear > 2011)
			{
				bNotDone = B_TRUE;
				memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
				sprintf(szMessage, "%s", PWV_INFO_INVALID_TICKET_YEAR);
				PWV_efnLogMessage(PWV_MP_INFORMATION, "VAP", szMessage);
				break; /* exit FOR cicle */
		  }
			
			bNotDone = B_FALSE;
		}
		
	}while(bNotDone);
	
	/* Put the Ticket Year into environment structure */
	ptEnvironment->iTicketYear = iTicketYear;
	
}


/******************************************************************************
 *
 * Function:    PWV_GetReferenceNumber
 *
 * Author:      Accenture 
 *
 * Date :       05-08-2004
 * 
 * Description: Get Wager External Serial Number from USER to search in files
 *				Obtain Internal Serial Number -> Reference Number
 *
 * Input Parameters:
 *
 * 	N/A
 *	
 * Output Parameters:
 *
 *	ptEnvironment: environment structure to store Reference Number
 *   
 * Return Values:
 *
 * Changes History:
 *
 *  VER  DD-MM-YYYY  AUTHOR        COMMENTS
 *
 *  V02	 10-04-2013	 Accenture	   Changed PWV_GetTicketCDCDate function signature
 *                                 Added   PWV_GetTicketYear function
 *
 ******************************************************************************/
void PWV_GetReferenceNumber(tProcessEnvironment *ptEnvironment, int *iCheckSumError)
{
	char szMessage			[PWV_LOG_MAXPATHLEN];

	char szTMPSerialNumber	[PWV_LOG_MAXPATHLEN];
	char szTMPJulianDate	[PWV_JULIAN_DATE_LEN + 1];
	char szTMPExtSerialNum	[PWV_EXTERNAL_SERIAL_NUMBER + 1];
	char szTMPCheckDigit	[PWV_CHECK_DIGIT_LEN + 1];
	
	int iCDCDate;
	
	char *pcTMPPointer;
	
	int iTMPRefNum = 0;
	
	int iCntGRN = 0;
	Boolean bNotDone;
	
	int iInpverRetVal;	/* 0 = success; 15 = checksum error */

	bNotDone = B_TRUE;
	/* get External Serial Number from USER */
	do
	{
		printf("\n%s", PWV_INSERT_EXT_SERIAL_NUM_MSG);
		scanf("%s", szTMPSerialNumber);
		szTMPSerialNumber[PWV_TICKET_SERIAL_NUMBER + 2] = PWV_NULL_CHAR;
		if((szTMPSerialNumber[3] == '-') && (szTMPSerialNumber[12] == '-'))
		{
			for(iCntGRN = PWV_CICLE_LL; iCntGRN < (PWV_TICKET_SERIAL_NUMBER + 2); iCntGRN++)
			{
				if((((szTMPSerialNumber[iCntGRN] - 48) < 0) || ((szTMPSerialNumber[iCntGRN] - 48) > 9)) && ((iCntGRN != 3) && (iCntGRN != 12))) /* '-' ifen positions */
				{
					bNotDone = B_TRUE;
					memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
					sprintf(szMessage, "%s", PWV_INFO_INVALID_REF_NUM);
					PWV_efnLogMessage(PWV_MP_INFORMATION, "VAP", szMessage);
					break; /* exit FOR cicle */
				}
				bNotDone = B_FALSE;
			}
		}
		else
		{
			bNotDone = B_TRUE;
			memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
			sprintf(szMessage, "%s", PWV_INFO_INVALID_REF_NUM);
			PWV_efnLogMessage(PWV_MP_INFORMATION, "VAP", szMessage);
		}	
	}while(bNotDone);
	
	/* Ticket Julian Date */
	pcTMPPointer = szTMPSerialNumber;
	strncpy(szTMPJulianDate, pcTMPPointer, PWV_JULIAN_DATE_LEN);
	szTMPJulianDate[PWV_JULIAN_DATE_LEN] = PWV_NULL_CHAR;
	ptEnvironment->iJulianDate = atoi(szTMPJulianDate) - PWV_JULIANDATE_OFFSET;
	/* Ticket External Serial Number */
	pcTMPPointer = pcTMPPointer + PWV_JULIAN_DATE_LEN + 1;
	strncpy(szTMPExtSerialNum, pcTMPPointer, PWV_EXTERNAL_SERIAL_NUMBER);
	szTMPExtSerialNum[PWV_EXTERNAL_SERIAL_NUMBER] = PWV_NULL_CHAR;
	ptEnvironment->iExternalSerialNum = atoi(szTMPExtSerialNum);
	/* Check Digit */
	pcTMPPointer = pcTMPPointer + PWV_EXTERNAL_SERIAL_NUMBER + 1;
	strncpy(szTMPCheckDigit, pcTMPPointer, PWV_CHECK_DIGIT_LEN);
	szTMPCheckDigit[PWV_CHECK_DIGIT_LEN] = PWV_NULL_CHAR;
	ptEnvironment->iCheckDigit = atoi(szTMPCheckDigit);
	
	/* printf("\nSerial Number: %s-%s-%s\n", szTMPJulianDate, szTMPExtSerialNum, szTMPCheckDigit); */
	iTMPRefNum = atoi(szTMPExtSerialNum);

  /* V02 - Changed function signature */
	/* PWV_GetTicketCDCDate(ptEnvironment->iJulianDate, &(iCDCDate)); */
	PWV_GetTicketCDCDate(ptEnvironment->iTicketYear, ptEnvironment->iJulianDate, &(iCDCDate));

	/* get Internal Serial Number -> Reference Number */
	iInpverRetVal = inpver(&(iCDCDate), &(ptEnvironment->iExternalSerialNum), &(iTMPRefNum), &(ptEnvironment->iCheckDigit));
	if(iInpverRetVal != 0)
	{
		*iCheckSumError = 1;
		memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
		sprintf(szMessage, "%s", PWV_ERROR_SERIALNUM_CONVERSION);
		PWV_efnLogMessage(PWV_MP_INFORMATION, "VAP", szMessage);
	}

	/* printf("Ref Num: %d\n", iTMPRefNum); */
		
	sprintf(ptEnvironment->szUserIDem, "%04d%07d", iCDCDate, iTMPRefNum);
}

/******************************************************************************
 *
 * Function:    PWV_GetTicketCDCDate
 *
 * Author:      Accenture 
 *
 * Date :       05-08-2004
 * 
 * Description: Get Ticket CDC Date using Ticket Julian Date
 *
 * Input Parameters:
 *
 * 	iTicketYear
 
 * 	iTicketJulianDate
 *	
 * Output Parameters:
 *
 *	iTicketCDCDate
 *   
 * Return Values:
 *
 * Changes History:
 *
 *  VER  DD-MM-YYYY  AUTHOR        COMMENTS
 *
 *  V02	 10-04-2013	 Accenture	   Added parameter iTicketYear to function
 *                                 Changed the way the CDC date is calculated in order
 *                                 to be possible calculate CDC dates of years other than
 *                                 the current year and the year before)
 *
 ******************************************************************************/
void PWV_GetTicketCDCDate(int iTicketYear, int iTicketJulianDate, int *iTicketCDCDate)
{
	time_t tRawTime;
	struct tm *ptTimeInfo;
	int iCDCOffset;

	/* Get current time info and modify it to first day of the year of registration of the ticket */
	tRawTime = time(NULL);
	ptTimeInfo = localtime (&tRawTime);
	ptTimeInfo->tm_year = iTicketYear - 1900;
	ptTimeInfo->tm_mon = 0; /* January */
	ptTimeInfo->tm_mday = 1; /* 01 January */
	tRawTime = mktime(ptTimeInfo);
	
	/* Get CDC date corresponding to the first day of the year of registration of the ticket */
	iCDCOffset = (tRawTime - PWV_CDCINITIALDATE_IN_SEC) / PWV_24HOURS_IN_SECONDS;

	/* Calculate Ticket CDC Date */
	*(iTicketCDCDate) = iCDCOffset + iTicketJulianDate;
	
}

/******************************************************************************
 *
 * Function:    PWV_efnSearchRefNumInFiles
 *
 * Author:      Accenture 
 *
 * Date :       05-08-2004
 * 
 * Description: Obtain all RTS Wagers Filenames
 *				Search Reference Number in valid RTS Wagers Files File
 *
 * Input Parameters:
 *
 * 	ptEnvironment: environment structure
 *	
 * Output Parameters:
 *
 *	N/A
 *   
 * Return Values:	PWV_RV_OK: Success
 *					PWV_RV_ERROR: Error
 *
 * Changes History:
 *
 ******************************************************************************/
PWV_ReturnValue
PWV_efnSearchRefNumInFiles(tProcessEnvironment *ptEnvironment)
{
	PWV_ReturnValue eRetVal;
	char szMessage				[PWV_LOG_MAXPATHLEN];
	
	FILE *pfdRTSWFile = NULL;
	FILE *pfdRTSWFileSizesFile = NULL;
	char szRTSWFileName			[PWV_MAX_PATH_LEN];

	char szActiveTicket			[PWV_RTSWF_TICKET_REG_LEN + 1];
	char szActTickRTSWFHeader	[PWV_RTSWF_HEADER_LEN + 1];
	char szCancelledTicket 		[PWV_RTSWF_TICKET_REG_LEN + 1];
	char szCancTickRTSWFHeader	[PWV_RTSWF_HEADER_LEN + 1];

	Boolean bMVSystemError = B_FALSE;		/* mv system command error flag */
	Boolean bRefNumNotFound = B_TRUE;		/* Reference Number Not Found flag */
	Boolean bRefNumCancelled = B_FALSE;		/* Found Reference Number is a Cancelled Wager */
/* Release 2 -> include Status 2 - Auto Cancel */
	Boolean bRefNumCancelledAuto = B_FALSE;
	int iNumberOfOccurrences, i;
	int piOffsets[10];
	int piHeaderOffsets[10];

	/* Get filenames in WM input */
	memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
	sprintf(szMessage, "A abrir os ficheiros do directorio de INPUT: %s", ptEnvironment->szInputFullPath);
	PWV_efnLogMessage(PWV_MP_INFORMATION, "VAP", szMessage);

	
	if (PWV_efnHuntReferenceNumber(ptEnvironment->buffer, ptEnvironment->filesize, ptEnvironment->szUserIDem, &iNumberOfOccurrences, piOffsets, piHeaderOffsets) != PWV_RV_OK)
	{
		printf("Error hunting ticket!\n");
		return PWV_RV_ERROR;
	}
	
	
	for (i=0; i< iNumberOfOccurrences; i++)
	{
		PWV_GetTicketData(szActiveTicket, szActTickRTSWFHeader, &bRefNumCancelled, piOffsets[i], piHeaderOffsets[i], /*pfdRTSWFile, */ptEnvironment,&bRefNumCancelledAuto);
		PWV_DisplayWagerData(szActiveTicket, szActTickRTSWFHeader, bRefNumCancelled, bRefNumCancelledAuto);
	}
	
			
	return PWV_RV_OK;
}







PWV_ReturnValue
PWV_GetTicketData(char *szActiveTicket, char *szActTickRTSWFHeader, 
                  Boolean *bRefNumCancelled, int iOffsets, int iHeaderOffsets, /*FILE *pfdRTSWFile,*/
                  tProcessEnvironment *ptEnvironment, Boolean *bRefNumCancelledAuto)
{
	
	char szMessage				[PWV_LOG_MAXPATHLEN];
	
	int iCnt2 = 0, 
		iCnt3 = 0;
	char szFileSize				[PWV_FILESIZE_LEN];
	
	int iNumBytes = 0;

	int iLastFilePos = 0;
	int iCurrFilePos = 0;
	
	char szRTSWFHeader			[PWV_RTSWF_HEADER_LEN + 1];
	char aRTSWFBodyBlock		[PWV_RTSWF_BODY_BLOCK_LEN];	/* array that receive block of tickets */
	char *pszRTSWFBodyBlockPos = aRTSWFBodyBlock;
	char szRTSWFTicketReg 		[PWV_RTSWF_TICKET_REG_LEN + 1];
	int iTicketInBlock = 0;

	char szRefNum				[PWV_RTSWF_TICK_EMID_LEN + 1];
	char szTimeStamp			[PWV_RTSWF_TICK_TIMESTAMP_LEN + 1];
	char szState				[PWV_RTSWF_TICK_STATE_LEN + 1];
	
	char *pcTempPtr = NULL;
	/* Boolean *bRefNumNotFound = NULL; */



	memcpy(szRTSWFHeader, &ptEnvironment->buffer[iHeaderOffsets], PWV_RTSWF_HEADER_LEN);
	memset(szRTSWFTicketReg, PWV_NULL_CHAR, PWV_RTSWF_TICKET_REG_LEN + 1);
	memcpy(szRTSWFTicketReg, &ptEnvironment->buffer[iOffsets-1], PWV_RTSWF_TICKET_REG_LEN);


	szRTSWFTicketReg[PWV_RTSWF_TICKET_REG_LEN] = PWV_NULL_CHAR;
	
	pcTempPtr = szRTSWFTicketReg;
	/* Store REFERENCE NUMBER (ID EM) to compare */
	strncpy(szRefNum, pcTempPtr, PWV_RTSWF_TICK_EMID_LEN);
	szRefNum[PWV_RTSWF_TICK_EMID_LEN] = PWV_NULL_CHAR;
		/* printf("\nRef. Num.: <%s>\n", szRefNum); */
/*						fprintf(fpx,"\nREF NUMBER: [%s]",szRefNum); */
    /* printf("\n szRefNum: [%s]",szRefNum); */
	pcTempPtr += (	PWV_RTSWF_TICK_EMID_LEN + PWV_RTSWF_TICK_P_EMID_LEN	);
	
	/* Store TimeStamp to display */
	strncpy(szTimeStamp, pcTempPtr, PWV_RTSWF_TICK_TIMESTAMP_LEN);
	szTimeStamp[PWV_RTSWF_TICK_TIMESTAMP_LEN] = PWV_NULL_CHAR;
		/* printf("\nTimeStamp: <%s>\n", szTimeStamp); */
    /* printf("\n szTimeStamp: [%s]",szTimeStamp); */
	pcTempPtr += (	PWV_RTSWF_TICK_TIMESTAMP_LEN + PWV_RTSWF_TICK_RETRYID_LEN	);
	
	/* Store STATE to compare */
	strncpy(szState, pcTempPtr, PWV_RTSWF_TICK_STATE_LEN);
	szState[PWV_RTSWF_TICK_STATE_LEN] = PWV_NULL_CHAR;
		/* printf("\nStatus: <%s>\n", szState); */
    /* printf("\n szState [%s]",szState);	 */
	/* compare Active Reference Number */
	/* if((strcmp(szRefNum, ptEnvironment->szUserIDem) == 0) && (atoi(szState) == 1)) */
	/* { */
		/* printf("\n Activo"); */
		/* (*bRefNumNotFound) = B_FALSE; */
		/* printf("\n Activo"); */
		/* PWV_CopyTicketInfo(szRTSWFTicketReg, szRTSWFHeader, szActiveTicket, szActTickRTSWFHeader); */
		/* printf("\n Activo"); */
	/* } */
	/* compare Cancelled Reference Number */
	if((strcmp(szRefNum, ptEnvironment->szUserIDem) == 0)/* && (atoi(szState) == 0) */)
	{
		/* printf("\n Cancel"); */
		(*bRefNumCancelled) = (atoi(szState) == 0);
/* Release 2 - Include Status 2 - Auto Cancel */
		if (atoi(szState) == 2) 
		{
		   (*bRefNumCancelled) = B_TRUE;
		   (*bRefNumCancelledAuto) = B_TRUE;
		}
		PWV_CopyTicketInfo(szRTSWFTicketReg, szRTSWFHeader, szActiveTicket, szActTickRTSWFHeader);
	}
	
	return PWV_RV_OK;
}




/******************************************************************************
 *
 * Function:    PWV_efnSearchAllFile
 *
 * Author:      Accenture 
 *
 * Date :       05-08-2004
 * 
 * Description: Search Ref Num in all wagers files (into Big File)
 *
 * Input Parameters:
 *
 * 	ptEnvironment: environment structure
 * 	pfdRTSWFileSizesFile: Wagers Files Sizes FILE
 *	pfdRTSWFile: Wagers Files Concatenation FILE
 *	
 * Output Parameters:
 *
 *	szActiveTicket: Active Ticket found in file
 *	szActTickRTSWFHeader: File Header where the Active Ticket was found
 *	szCancelledTicket: Cancelled Ticket found in file
 *	szCancTickRTSWFHeader: File Header where the Cancelled Ticket was found
 *	bMVSystemError: System Error occurrence
 *	bRefNumNotFound: Reference Number found/not found
 *	bRefNumCancelled: Reference Number found is/is not cancelled
 *   
 * Return Values:	PWV_RV_OK: Success
 *					PWV_RV_ERROR: Error
 *
 * Changes History:
 *
 ******************************************************************************/
PWV_ReturnValue
PWV_efnSearchAllFile(tProcessEnvironment *ptEnvironment, FILE *pfdRTSWFileSizesFile, FILE *pfdRTSWFile,
					 char *szActiveTicket, char *szActTickRTSWFHeader,
					 char *szCancelledTicket, char *szCancTickRTSWFHeader,
					 Boolean *bMVSystemError, Boolean *bRefNumNotFound, Boolean *bRefNumCancelled)
{	
	char szMessage				[PWV_LOG_MAXPATHLEN];
	
	int iCnt2 = 0, 
		iCnt3 = 0;
	char szFileSize				[PWV_FILESIZE_LEN];
	
	int iNumBytes = 0;

	int iLastFilePos = 0;
	int iCurrFilePos = 0;
	
	char szRTSWFHeader			[PWV_RTSWF_HEADER_LEN + 1];
	char aRTSWFBodyBlock		[PWV_RTSWF_BODY_BLOCK_LEN];	/* array that receive block of tickets */
	char *pszRTSWFBodyBlockPos = aRTSWFBodyBlock;
	char szRTSWFTicketReg 		[PWV_RTSWF_TICKET_REG_LEN + 1];
	int iTicketInBlock = 0;

	char szRefNum				[PWV_RTSWF_TICK_EMID_LEN + 1];
	char szTimeStamp			[PWV_RTSWF_TICK_TIMESTAMP_LEN + 1];
	char szState				[PWV_RTSWF_TICK_STATE_LEN + 1];
	
	char *pcTempPtr = NULL;
	
	/* FILE *fpx; */
	
	/* fpx = fopen("bruno.txt","w"); */
	
	
	
	while(!feof(pfdRTSWFileSizesFile))
	{
		memset(szFileSize, PWV_NULL_CHAR, PWV_FILESIZE_LEN);
		fgets(szFileSize, PWV_FILESIZE_LEN, pfdRTSWFileSizesFile);
                /* fprintf(fpx,"\nszFileSize: [%s]",szFileSize); */
		iCurrFilePos = atoi(szFileSize);
		if(strcmp(szFileSize, "\0") != 0)/* if(iCurrFilePos > 0) */
		{
                	/* fprintf(fpx,"\n IF szFileSize: [%s] - %d",szFileSize,iCurrFilePos); */

			if(fseek(pfdRTSWFile, iLastFilePos, SEEK_SET) != 0)
			{
				/* printf("\n iLastFilePos:[%d] - iCurrFilePos:[%d]\n",iLastFilePos,iCurrFilePos); */
				memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
				sprintf(szMessage, PWV_FSEEK_FAILURE_MSG, PWV_ALLWAGERSFILES_FILENAME);
				PWV_efnLogMessage(PWV_MP_ERROR, "PWV_efnSearchAllFile", szMessage);
				/* fclose(fpx); */
				return PWV_RV_ERROR;
			}

			iLastFilePos = iLastFilePos + iCurrFilePos;
			
			/* read file header */
		
		
		
		
			
		
		
			iNumBytes = fread(szRTSWFHeader, sizeof(char), PWV_RTSWF_HEADER_LEN, pfdRTSWFile);
/*			fprintf(fpx,"szRTSWFHeader: [%s] \n",szRTSWFHeader); */
			
			if((iNumBytes < 0) && (feof(pfdRTSWFile) == 0))
			{
				memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
				sprintf(szMessage, PWV_ERROR_READING_RTSWFILE, PWV_ALLWAGERSFILES_FILENAME);
				PWV_efnLogMessage(PWV_MP_ERROR, "PWV_efnSearchAllFile", szMessage);
				return PWV_RV_ERROR;
			}

			if(iNumBytes == PWV_RTSWF_HEADER_LEN)
			{
/*				fprintf(fpx,"szRTSWFHeader: [%13.13s] \n",szRTSWFHeader); */
				memset(aRTSWFBodyBlock, PWV_NULL_CHAR, (iCurrFilePos - PWV_RTSWF_HEADER_LEN));
				iNumBytes = fread(aRTSWFBodyBlock, sizeof(char), (iCurrFilePos - PWV_RTSWF_HEADER_LEN), pfdRTSWFile);
				if(iNumBytes < 0)
				{
					memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
					sprintf(szMessage, PWV_ERROR_READING_RTSWFILE, PWV_ALLWAGERSFILES_FILENAME);
					PWV_efnLogMessage(PWV_MP_ERROR, "PWV_efnSearchAllFile", szMessage);
					return PWV_RV_ERROR;
				}

				iTicketInBlock = iNumBytes / PWV_RTSWF_TICKET_REG_LEN;
/*				fprintf(fpx,"iNumBytes PWV_RTSWF_TICKET_REG_LEN: - %d \n",(iNumBytes % PWV_RTSWF_TICKET_REG_LEN)); */
				
				if((iNumBytes % PWV_RTSWF_TICKET_REG_LEN) == 0)
				{
/*					fprintf(fpx,"aRTSWFBodyBlock: [%109.109s] - %d \n",aRTSWFBodyBlock,(iNumBytes % PWV_RTSWF_TICKET_REG_LEN)); */
					pszRTSWFBodyBlockPos = aRTSWFBodyBlock;
					for(iCnt3 = PWV_CICLE_LL; iCnt3 < iTicketInBlock; iCnt3++)
					{
						memset(szRTSWFTicketReg, PWV_NULL_CHAR, PWV_RTSWF_TICKET_REG_LEN + 1);

						for(iCnt2 = PWV_CICLE_LL; iCnt2 < PWV_RTSWF_TICKET_REG_LEN; iCnt2++)
						{
							szRTSWFTicketReg[iCnt2] = *(pszRTSWFBodyBlockPos++);
						}
						szRTSWFTicketReg[PWV_RTSWF_TICKET_REG_LEN] = PWV_NULL_CHAR;
						
						pcTempPtr = szRTSWFTicketReg;
						/* Store REFERENCE NUMBER (ID EM) to compare */
						strncpy(szRefNum, pcTempPtr, PWV_RTSWF_TICK_EMID_LEN);
						szRefNum[PWV_RTSWF_TICK_EMID_LEN] = PWV_NULL_CHAR;
							/* printf("\nRef. Num.: <%s>\n", szRefNum); */
/*						fprintf(fpx,"\nREF NUMBER: [%s]",szRefNum); */
						pcTempPtr += (	PWV_RTSWF_TICK_EMID_LEN + PWV_RTSWF_TICK_P_EMID_LEN	);
						
						/* Store TimeStamp to display */
						strncpy(szTimeStamp, pcTempPtr, PWV_RTSWF_TICK_TIMESTAMP_LEN);
						szTimeStamp[PWV_RTSWF_TICK_TIMESTAMP_LEN] = PWV_NULL_CHAR;
							/* printf("\nTimeStamp: <%s>\n", szTimeStamp); */

						pcTempPtr += (	PWV_RTSWF_TICK_TIMESTAMP_LEN + PWV_RTSWF_TICK_RETRYID_LEN	);
						
						/* Store STATE to compare */
						strncpy(szState, pcTempPtr, PWV_RTSWF_TICK_STATE_LEN);
						szState[PWV_RTSWF_TICK_STATE_LEN] = PWV_NULL_CHAR;
							/* printf("\nStatus: <%s>\n", szState); */
						
						/* compare Active Reference Number */
						if((strcmp(szRefNum, ptEnvironment->szUserIDem) == 0) && (atoi(szState) == 1))
						{
							(*bRefNumNotFound) = B_FALSE;
							PWV_CopyTicketInfo(szRTSWFTicketReg, szRTSWFHeader, szActiveTicket, szActTickRTSWFHeader);
						}
						/* compare Cancelled Reference Number */
						if((strcmp(szRefNum, ptEnvironment->szUserIDem) == 0) && (atoi(szState) == 0))
						{
							(*bRefNumCancelled) = B_TRUE;
							PWV_CopyTicketInfo(szRTSWFTicketReg, szRTSWFHeader, szCancelledTicket, szCancTickRTSWFHeader);
						}
					}/* for(iCnt3 = PWV_CICLE_LL; iCnt3 < iTicketInBlock; iCnt3++) --> read all tickets in block */
				}/* if((iNumBytes % PWV_RTSWF_TICKET_REG_LEN) == 0) --> read block of tickets with correct number of bytes */

			}/* if(iNumBytes == PWV_RTSWF_HEADER_LEN) --> read header correct number of bytes */
			else
			{
				/* wrong number of bytes read */
				memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
				sprintf(szMessage, PWV_RTSWFILE_STATUS_DISPLAY, PWV_ALLWAGERSFILES_FILENAME, PWV_RTSWFILE_NOT_OK);
				strcat(szMessage, PWV_RTSWF_WRONG_NB_READ);
				PWV_efnLogMessage(PWV_MP_WARNING, "PWV_efnSearchAllFile", szMessage);
				(*bMVSystemError) = B_TRUE;
				return PWV_RV_ERROR; /* exit from while(feof(pfdRTSWFileSizesFile) == 0) cicle */
			}
		} /* if(strcmp(szFileSize, "\0") != 0) --> do not care about the last line from FileSizes file */
	} /* while(feof(pfdRTSWFileSizesFile) == 0) --> read file until the EOF */
        /* fclose(fpx); */
	return PWV_RV_OK;
}

/******************************************************************************
 *
 * Function:    PWV_DisplayWagerData
 *
 * Author:      Accenture 
 *
 * Date :       05-08-2004
 * 
 * Description: Display Wager Data with pretended Reference Number
 *
 * Input Parameters:
 *
 * 	szTicket: ticket (wager) data
 *	szFileHeader: file header where ticket (wager) was found
 *	bStatus: ticket status
 *      bStatusAutoCancel: Automatic Cancel
 *	
 * Output Parameters:
 *
 *	N/A
 *   
 * Return Values:	PWV_RV_OK: Success
 *					PWV_RV_ERROR: Error
 *
 * Changes History:
 *
 ******************************************************************************/
void PWV_DisplayWagerData(char *szTicket, char *szFileHeader, Boolean bStatus, Boolean bStatusAutoCancel)
{
	char szMessage		[PWV_LOG_MAXPATHLEN];
	
	char *pcTempPtr = NULL;
	RTSWFBodyTicketVar tPrizedWager;
	RTSWFHeaderVar tPrizedWagerHeaderFile;
/*	char szStatus 		[10]; */
	char szStatus 		[30];
	char szMultipleText	[4];
	int iNumMatrixes = 0;
	char szTicketDate	[10 + 1];
	char szTicketTime	[8 + 1];
	
	int iCntDWD = 0;

	/* Terminal Code */
	strncpy(tPrizedWagerHeaderFile.szTermCode, szFileHeader, PWV_TERMINAL_CODE_LEN);
	tPrizedWagerHeaderFile.szTermCode[PWV_TERMINAL_CODE_LEN] = PWV_NULL_CHAR;
	
	/* Draw ID */
	strncpy(tPrizedWagerHeaderFile.szDrawID, szFileHeader + PWV_TERMINAL_CODE_LEN, PWV_AAAA_DRAWID_LEN);
	tPrizedWagerHeaderFile.szDrawID[PWV_AAAA_DRAWID_LEN] = PWV_NULL_CHAR;
	
	pcTempPtr = szTicket;
	/* ID EM */
	strncpy(tPrizedWager.szIDem, pcTempPtr, PWV_RTSWF_TICK_EMID_LEN);
	tPrizedWager.szIDem[PWV_RTSWF_TICK_EMID_LEN] = PWV_NULL_CHAR;
		/* printf("<%s>", tPrizedWager.szIDem); */
	
	pcTempPtr += (	PWV_RTSWF_TICK_EMID_LEN + PWV_RTSWF_TICK_P_EMID_LEN	);
	
	/* TimeStamp */
	strncpy(tPrizedWager.szTimeStamp, pcTempPtr, PWV_RTSWF_TICK_TIMESTAMP_LEN);
	tPrizedWager.szTimeStamp[PWV_RTSWF_TICK_TIMESTAMP_LEN] = PWV_NULL_CHAR;
		/* printf("<%s>", tPrizedWager.szTimeStamp); */

	pcTempPtr += (	PWV_RTSWF_TICK_TIMESTAMP_LEN + PWV_RTSWF_TICK_RETRYID_LEN +\
					PWV_RTSWF_TICK_STATE_LEN	);

	/* MULTI CONTEST */
	strncpy(tPrizedWager.szMContest, pcTempPtr, PWV_RTSWF_TICK_MCONTEST_LEN);
	tPrizedWager.szMContest[PWV_RTSWF_TICK_MCONTEST_LEN] = PWV_NULL_CHAR;
		/*printf("<%s>", tPrizedWager.szMContest); */
	
	pcTempPtr += PWV_RTSWF_TICK_MCONTEST_LEN;
	
	/* MULTIPLE */
	strncpy(tPrizedWager.szMultiple, pcTempPtr, PWV_RTSWF_TICK_MULTIPLE_LEN);
	tPrizedWager.szMultiple[PWV_RTSWF_TICK_MULTIPLE_LEN] = PWV_NULL_CHAR;
		/* printf("<%s>", tPrizedWager.szMultiple); */
		/* make multiple text */
	if(atoi(tPrizedWager.szMultiple) == 0)
	{
		strcpy(szMultipleText, "SIM");
		szMultipleText[3] = PWV_NULL_CHAR;
		iNumMatrixes = 1;
	}
	else
	{
		strcpy(szMultipleText, "NAO");
		szMultipleText[3] = PWV_NULL_CHAR;
		iNumMatrixes = atoi(tPrizedWager.szMultiple);
	}
	
	pcTempPtr += PWV_RTSWF_TICK_MULTIPLE_LEN;
	/* MATRIX 1 */
	tPrizedWager.iM1NumNumbers = 0;
	tPrizedWager.iM1NumStars = 0;
	iCntDWD = PWV_CICLE_LL;
	while(iCntDWD < PWV_RTSWF_TICK_M1_LEN)
	{
		if(iCntDWD < NUM_MIN_NUMBERS)
		{
			tPrizedWager.iM1[tPrizedWager.iM1NumNumbers] = (int)(pcTempPtr[iCntDWD]);
			/* printf("<%d>",tPrizedWager.iM1[tPrizedWager.iM1NumNumbers]); */
			(tPrizedWager.iM1NumNumbers)++; /* printf("(1ºIF)"); */
		}
		else if((iCntDWD < NUM_MAX_NUMBERS) && (pcTempPtr[iCntDWD] != PWV_RTSWF_IRR_NUMSTAR))
		{
			tPrizedWager.iM1[tPrizedWager.iM1NumNumbers] = (int)(pcTempPtr[iCntDWD]);
			/* printf("<%d>",tPrizedWager.iM1[tPrizedWager.iM1NumNumbers]); */
			(tPrizedWager.iM1NumNumbers)++; /* printf("(2ºIF)>"); */
		}
		if((iCntDWD >= NUM_MAX_NUMBERS) && (iCntDWD < (NUM_MAX_NUMBERS + NUM_MIN_STARS)))
		{
			tPrizedWager.iM1[tPrizedWager.iM1NumNumbers + tPrizedWager.iM1NumStars] = (int)(pcTempPtr[iCntDWD]);
			/* printf("<%d>",tPrizedWager.iM1[tPrizedWager.iM1NumNumbers + tPrizedWager.iM1NumStars]); */
			(tPrizedWager.iM1NumStars)++; /* printf("(3ºIF)"); */
		}
		else if((iCntDWD >= (NUM_MAX_NUMBERS + NUM_MIN_STARS)) && (iCntDWD < PWV_RTSWF_TICK_M1_LEN) && (pcTempPtr[iCntDWD] != PWV_RTSWF_IRR_NUMSTAR))
		{
			tPrizedWager.iM1[tPrizedWager.iM1NumNumbers + tPrizedWager.iM1NumStars] = (int)(pcTempPtr[iCntDWD]);
			/*printf("<%d>",tPrizedWager.iM1[tPrizedWager.iM1NumNumbers + tPrizedWager.iM1NumStars]); */
			(tPrizedWager.iM1NumStars)++; /* printf("(4ºIF)"); */
		}
		iCntDWD++;
	} /* while(iCntDWD < PWV_RTSWF_TICK_M1_LEN) --> Counting M1 Numbers and Stars */
	
	pcTempPtr += PWV_RTSWF_TICK_M1_LEN;
	/* MATRIX 2 */
	for(iCntDWD = PWV_CICLE_LL; iCntDWD < PWV_RTSWF_TICK_M2_LEN; iCntDWD++)
	{
		tPrizedWager.iM2[iCntDWD] = *(pcTempPtr++);
	}
	/* MATRIX 3 */
	for(iCntDWD = PWV_CICLE_LL; iCntDWD < PWV_RTSWF_TICK_M3_LEN; iCntDWD++)
	{
		tPrizedWager.iM3[iCntDWD] = *(pcTempPtr++);
	}
	/* MATRIX 4 */
	for(iCntDWD = PWV_CICLE_LL; iCntDWD < PWV_RTSWF_TICK_M4_LEN; iCntDWD++)
	{
		tPrizedWager.iM4[iCntDWD] = *(pcTempPtr++);
	}
	/* MATRIX 5 */
	for(iCntDWD = PWV_CICLE_LL; iCntDWD < PWV_RTSWF_TICK_M5_LEN; iCntDWD++)
	{
		tPrizedWager.iM5[iCntDWD] = *(pcTempPtr++);
	}

	/* WAGER TOTAL PRICE */
	strncpy(tPrizedWager.szWagerPrice, pcTempPtr, PWV_RTSWF_TICK_WPRICE_LEN);
	tPrizedWager.szWagerPrice[PWV_RTSWF_TICK_WPRICE_LEN] = PWV_NULL_CHAR;
		/* printf("<%s>", tPrizedWager.szWagerPrice); */

	/* DISPLAY WAGER DATA */
		/* Wager Time and Local */
	memset(szTicketDate, PWV_ZERO_MEM, 10 + 1);
	strncpy(szTicketDate, tPrizedWager.szTimeStamp, 4);
	strcat(szTicketDate, "-");
	strncat(szTicketDate, tPrizedWager.szTimeStamp + 4, 2);
	strcat(szTicketDate, "-");
	strncat(szTicketDate, tPrizedWager.szTimeStamp + 4 + 2, 2);
	szTicketDate[10] = PWV_NULL_CHAR;
	
	memset(szTicketTime, PWV_ZERO_MEM, 8 + 1);
	strncpy(szTicketTime, tPrizedWager.szTimeStamp + 8, 2);
	strcat(szTicketTime, ":");
	strncat(szTicketTime, tPrizedWager.szTimeStamp + 8 + 2, 2);
	strcat(szTicketTime, ":");
	strncat(szTicketTime, tPrizedWager.szTimeStamp + 8 + 2 + 2, 2);
	szTicketTime[8] = PWV_NULL_CHAR;
	
		/* Ticket Status Treatment */
	if(!bStatus)
		sprintf(szStatus, "activo");
	else
	{
	    if(!bStatusAutoCancel)
		sprintf(szStatus, "cancelado");
	    else 
	    	sprintf(szStatus, "cancelado administrativamente");
	}
	memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
	sprintf(szMessage, PWV_MSG_WAGER_TIME_LOCAL, \
		tPrizedWagerHeaderFile.szDrawID, szTicketDate, szTicketTime, \
		tPrizedWagerHeaderFile.szTermCode, szStatus);
	printf("%s", szMessage);
	
		/* Table Header */
	memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
	sprintf(szMessage, PWV_DISP_WAGER_DATA_HEADER, \
		PWV_MESSAGE_TAB, 			PWV_MESSAGE_TAB, 			PWV_MESSAGE_TAB	);
	printf("%s", szMessage);
			
		/* Line */
	memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
	sprintf(szMessage, "%s", PWV_DISP_WAGERS_LINE);
	printf("%s", szMessage);
	
		/* Header Data */
	memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
	sprintf(szMessage, PWV_DISP_WAGER_DATA, \
			tPrizedWager.szIDem, 		PWV_MSG_REF_NUM_SPACES, 	PWV_MESSAGE_TAB, \
			tPrizedWager.szMContest, 	PWV_MSG_CONTEST_SPACES, PWV_MESSAGE_TAB, \
			szMultipleText, 			PWV_MSG_MULTIPLE_SPACES, 	PWV_MESSAGE_TAB, \
			tPrizedWager.szWagerPrice, 	PWV_MSG_TOTPRICE_SPACES	);
	printf("%s", szMessage);
		
		/* Line */
	memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
	sprintf(szMessage, "%s", PWV_DISP_WAGERS_LINE);
	printf("%s", szMessage);
	printf("\n");
	
		/* M1 */
	memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
	sprintf(szMessage, "%s", PWV_DISP_WAGER_M1);
	printf("%s", szMessage);
	printf("\t");
	for(iCntDWD = PWV_CICLE_LL; iCntDWD < tPrizedWager.iM1NumNumbers; iCntDWD++)
		printf("%3d ", tPrizedWager.iM1[iCntDWD]);
	printf(" / ");
	for(iCntDWD = tPrizedWager.iM1NumNumbers; iCntDWD < (tPrizedWager.iM1NumNumbers + tPrizedWager.iM1NumStars); iCntDWD++)
		printf("%3d ", tPrizedWager.iM1[iCntDWD]);
	printf("\n\n");
		/* M2 */
	if(iNumMatrixes > 1)
	{
		memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
		sprintf(szMessage, "%s", PWV_DISP_WAGER_M2);
		printf("%s", szMessage);
		printf("\t");
		for(iCntDWD = PWV_CICLE_LL; iCntDWD < NUM_MIN_NUMBERS; iCntDWD++)
			printf("%3d ", tPrizedWager.iM2[iCntDWD]);
		printf(" / ");
		for(iCntDWD = NUM_MIN_NUMBERS; iCntDWD < (NUM_MIN_NUMBERS + NUM_MIN_STARS); iCntDWD++)
			printf("%3d ", tPrizedWager.iM2[iCntDWD]);
		printf("\n\n");
		/* M3 */
		if (iNumMatrixes > 2)
		{
			memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
			sprintf(szMessage, "%s", PWV_DISP_WAGER_M3);
			printf("%s", szMessage);
			printf("\t");
			for(iCntDWD = PWV_CICLE_LL; iCntDWD < NUM_MIN_NUMBERS; iCntDWD++)
				printf("%3d ", tPrizedWager.iM3[iCntDWD]);
			printf(" / ");
			for(iCntDWD = NUM_MIN_NUMBERS; iCntDWD < (NUM_MIN_NUMBERS + NUM_MIN_STARS); iCntDWD++)
				printf("%3d ", tPrizedWager.iM3[iCntDWD]);
			printf("\n\n");
		/* M4 */
			if (iNumMatrixes > 3)
			{
				memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
				sprintf(szMessage, "%s", PWV_DISP_WAGER_M4);
				printf("%s", szMessage);
				printf("\t");
				for(iCntDWD = PWV_CICLE_LL; iCntDWD < NUM_MIN_NUMBERS; iCntDWD++)
					printf("%3d ", tPrizedWager.iM4[iCntDWD]);
				printf(" / ");
				for(iCntDWD = NUM_MIN_NUMBERS; iCntDWD < (NUM_MIN_NUMBERS + NUM_MIN_STARS); iCntDWD++)
					printf("%3d ", tPrizedWager.iM4[iCntDWD]);
				printf("\n\n");
		/* M5 */
				if (iNumMatrixes > 4)
				{
					memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
					sprintf(szMessage, "%s", PWV_DISP_WAGER_M5);
					printf("%s", szMessage);
					printf("\t");
					for(iCntDWD = PWV_CICLE_LL; iCntDWD < NUM_MIN_NUMBERS; iCntDWD++)
						printf("%3d ", tPrizedWager.iM5[iCntDWD]);
					printf(" / ");
					for(iCntDWD = NUM_MIN_NUMBERS; iCntDWD < (NUM_MIN_NUMBERS + NUM_MIN_STARS); iCntDWD++)
						printf("%3d ", tPrizedWager.iM5[iCntDWD]);
					printf("\n\n");
				} /* if (iNumMatrixes > 4) */
			} /* if (iNumMatrixes > 3) */
		} /* if (iNumMatrixes > 2) */
	} /* if(iNumMatrixes > 1) */
	
	memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
	sprintf(szMessage, "%s", PWV_DISP_WAGERS_LINE);
	printf("%s", szMessage);
	
	/* end	DISPLAY WAGER DATA */
}

/******************************************************************************
 *
 * Function:    PWV_CopyTicketInfo
 *
 * Author:      Accenture 
 *
 * Date :       05-08-2004
 * 
 * Description: Copies Ticket and Agent info to a different string
 *
 * Input Parameters:
 *
 * 	szOldTicket: ticket original string
 *	szOldHeader: header original string
 *	
 * Output Parameters:
 *
 *	szNewTicket: ticket destiny string
 *	szNewHeader: header destiny string
 *   
 * Return Values:	N/A
 *
 * Changes History:
 *
 ******************************************************************************/
void PWV_CopyTicketInfo(char *szOldTicket, char *szOldHeader, char *szNewTicket, char *szNewHeader)
{
	int iCounter1 = 0;
	
	/* reset destiny strings */
	memset(szNewTicket, PWV_NULL_CHAR, PWV_RTSWF_TICKET_REG_LEN + 1);
	memset(szNewHeader, PWV_NULL_CHAR, PWV_RTSWF_HEADER_LEN + 1);

	for(iCounter1 = PWV_CICLE_LL; iCounter1 < PWV_RTSWF_TICKET_REG_LEN; iCounter1++)
	{
		szNewTicket[iCounter1] = szOldTicket[iCounter1];
	}
	szNewTicket[PWV_RTSWF_TICKET_REG_LEN] = PWV_NULL_CHAR;

	for(iCounter1 = PWV_CICLE_LL; iCounter1 < PWV_RTSWF_HEADER_LEN; iCounter1++)
	{
		szNewHeader[iCounter1] = szOldHeader[iCounter1];
	}
	szNewHeader[PWV_RTSWF_HEADER_LEN] = PWV_NULL_CHAR;
}

/******************************************************************************
 *
 * Function:    PWV_efnOpenLog
 *
 * Author:      Accenture 
 *
 * Date :       05-08-2004
 * 
 * Description: This function creates a log file for the current program.
 *              The filename of the log looks like:
 *              <home directory>/<log directory>/<szProgramName>_<YYYYDDhh24mmss>_<pid>.log.
 *              The file is created and the file descriptor saved for use
 *              by efnLogMessage.
 *
 * Input Parameters:
 *
 * 	szName: module or application name to add in LOG name
 *	ptEnvironment: environment structure
 *	
 * Output Parameters:
 *
 *	N/A
 *   
 * Return Values:	PWV_RV_OK: Success
 *					PWV_RV_FATAL: Fatal Error
 *
 * Changes History:
 *
 ******************************************************************************/
PWV_ReturnValue
PWV_efnOpenLog(tProcessEnvironment *ptEnvironment, const char *szName)
{
	char szFilename			[PWV_LOG_MAXPATHLEN];
	char szDate				[PWV_LOG_MAXPATHLEN];
	char szPid				[PWV_LOG_MAXPATHLEN];
	struct tm *ptTime;
	time_t tTimeNow;
	
	char szMessage			[PWV_LOG_MAXPATHLEN];

	memset(szFilename, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
	
	/* Get date and time */
	(void)time(&tTimeNow);
	ptTime=localtime(&tTimeNow);
	(void)strftime(szDate,
		 PWV_LOG_MAXPATHLEN,
		 "%Y%m%d%H%M%S",
		 ptTime); /* ISO standard date format */
	
	/* Buid LOG file name - Add directories, program name, date and pid */
	sprintf(szFilename, ptEnvironment->szLogFullPath);
	(void)strcat(szFilename, szName);
	(void)strcat(szFilename, "_");
	(void)strcat(szFilename, szDate);
	(void)strcat(szFilename, "_");
	(void)sprintf(szPid, "%d", getpid());
	(void)strcat(szFilename, szPid);
	(void)strcat(szFilename, ".log");
	
	/* Open log file */
	gpfdLogFile = fopen(szFilename, PWV_WRITE_M);
	if (NULL == gpfdLogFile)
	{
		memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
		sprintf(szMessage, PWV_ERROR_OPEN_LOG, __FILE__, __LINE__, errno, sys_errlist[errno]);
		PWV_efnLogMessage(PWV_MP_FATAL, "PWV_efnOpenLog", szMessage);
	  	return PWV_RV_FATAL;
	}
	
	return PWV_RV_OK;
}

/******************************************************************************
 *
 * Function:    PWV_efnCloseLog
 *
 * Author:      Accenture 
 *
 * Date :       05-08-2004
 * 
 * Description: Close the log file.
 *
 * Input Parameters:
 *
 * 	N/A
 *	
 * Output Parameters:
 *
 *	N/A
 *   
 * Return Values:	PWV_RV_OK: Success
 *					PWV_RV_ERROR: Error
 *
 * Changes History:
 *
 ******************************************************************************/
PWV_ReturnValue
PWV_efnCloseLog()
{
	char szMessage	[PWV_LOG_MAXPATHLEN];	
	
	if (fclose(gpfdLogFile) == EOF)
	{
		memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);
		sprintf(szMessage, "%s", PWV_ERROR_CLOSE_LOG);
		PWV_efnLogMessage(PWV_MP_ERROR, "PWV_efnCloseLog", szMessage);
	  	return PWV_RV_ERROR;
	}
	gpfdLogFile = NULL;
	
	return PWV_RV_OK;
}

/******************************************************************************
 *
 * Function:    PWV_efnLogMessage
 *
 * Author:      Accenture 
 *
 * Date :       05-08-2004
 * 
 * Description: This function first checks if the logfile has been opened,
 *              if it has it's used for printing and otherwise all output
 *              goes to the stderr.
 *              The function continues with printing a header containing
 *              eMessagePriority, function name as well as the current
 *				date and time.
 *
 * Input Parameters:
 *
 * 	eMessagePriority: priority index for translator for message priorities
 *	szFuncName: function where the error occurred
 *	szMessage: log message to display or write
 *	
 * Output Parameters:
 *
 *	N/A
 *   
 * Return Values:	PWV_RV_OK: Success
 *					PWV_RV_ERROR: Error
 *
 * Changes History:
 *
 ******************************************************************************/
PWV_ReturnValue
PWV_efnLogMessage(const PWV_MessagePriority eMessagePriority, const char *szFuncName,
				  const char *szMessage)
{
	FILE *pfdOutput[2];
	int iNumFiles = 0,
		iPFD;
	char szHeader[PWV_LOG_MAXPATHLEN];
	struct tm *ptTime=NULL;
	time_t tTimeNow;
	
	/* Initialize */
	pfdOutput[0] = NULL;
	pfdOutput[1] = NULL;
	
	if (NULL == szMessage)
	 return PWV_RV_ERROR;
	
	/* Check that error log file is open */
	if (NULL == gpfdLogFile)
	{
		iNumFiles = 1;
		pfdOutput[0] = stderr;
	}
	else
	{
		iNumFiles = 2;
		pfdOutput[0] = stderr;
		pfdOutput[1] = gpfdLogFile;
	}
	
	for(iPFD = PWV_CICLE_LL; iPFD < iNumFiles; iPFD++)
	{
		/* Print banner */
		(void)fprintf(pfdOutput[iPFD], "----------------------------------------");
		(void)fprintf(pfdOutput[iPFD], "----------------------------------------\n");
		/* Get current time */
		(void)time(&tTimeNow);
		ptTime=localtime(&tTimeNow);
	
		/* Create header with function name */
		(void)strftime(szHeader,
			   PWV_LOG_MAXPATHLEN,
			   "%Y/%m/%d %T",
			   ptTime); /* ISO standard date format */
		(void)fprintf(pfdOutput[iPFD], "%s @ %s @ %s\n",
			  gaszMessagePriorities[eMessagePriority],
			  szFuncName,
			  szHeader);
	
		/* Print message in log file */
		(void)vfprintf(pfdOutput[iPFD], szMessage, NULL);
		(void)fprintf(pfdOutput[iPFD], "\n");
		(void)fflush(pfdOutput[iPFD]);
	}

	return PWV_RV_OK;
}

 /* EOF */



int compare(char *string1,char *string2,int len)
{

    while(len)
    {
        if(*string1 != *string2)
            break;
        string1++;
        string2++;
        len--;
    }

    return(!len);
}



PWV_ReturnValue
PWV_efnReadCompleteFile(char *filename, char *buffer)
{
    FILE *f;
	long read=0;
	int filesize;

	if((f = fopen(filename,"rb")) == NULL)
    {
        printf("Error opening %s\n",filename);
        return PWV_RV_ERROR;
    }

	fseek(f, 0, SEEK_END);
    filesize = ftell(f);     
    fseek(f, 0, SEEK_SET);
    
    if(buffer==NULL)
    {
        printf("Invalid buffer where to read file %s\n",filename);
        return PWV_RV_ERROR;
    }


    read = fread(buffer,sizeof(char),filesize,f);
    
    fclose(f);

	return PWV_RV_OK;
}



PWV_ReturnValue
PWV_efnHuntReferenceNumber(char *buffer, int filesize, char *text, int *piNumberOfOcurrences, int *ppiOffsets, int *ppiHeaderOffsets)
{
	char szMessage	[PWV_LOG_MAXPATHLEN];	

    long i,read;
    long matches = 0;
    char *b;
    int textsize;


    textsize=strlen(text);
    
/* Scan for a matching byte */

    i = 0;
    b = buffer;

    while(i<filesize)
    {
        if(*b == text[0])
            if(compare(b,text,textsize))
            {
                if (strncmp(b+11+1, "0000000000", 10) == 0)
                {
                	matches++;
                	ppiOffsets[matches-1] = b-buffer+1;
                	if ((ppiHeaderOffsets[matches-1] = findHeaderOffset(buffer, b-buffer+1)) < 0)
                		return PWV_RV_ERROR;
                	b += 11+1;
            	}	
            }
        b++;
        i++;
    }

	printf("%d registos encontrados.\n", matches);

    *piNumberOfOcurrences = matches;
	
	return PWV_RV_OK;
}


int
findHeaderOffset(char *buffer, int offset)
{
	int headerOffset;
	int currentOffset = offset + 11;
	
	
	while (strncmp(&buffer[currentOffset], "0000000000", 10) == 0)
	{
		currentOffset -= 109;
		if (currentOffset <= 0)
		{
			/* printf("Could not find header offset (current offset was %d)!!\n", currentOffset); */
			/* return -1; */
			return 0;
		}
	}
		
	headerOffset = currentOffset + 109 - 11 - 1 - 13;
	
	return headerOffset;
}



int PWV_efnValidFile(struct dirent *entry)
{
	char *ptr;
	char szExtension[5];
	
	if((strcmp(entry->d_name, ".") == 0) || (strcmp(entry->d_name, "..")==0))
		return 0;
		
	ptr = strrchr(entry->d_name, '.');
	
	strncpy(szExtension, ptr, 4);
	szExtension[4]='\0';

	/* CONSTANT SHOULD NOT BE USED... No arguments can be passed to this funcion, though. */	
	if((ptr != NULL) && (strcmp(szExtension, ".WGR")==0))	
		return 1;
	else
		return 0;
}



/******************************************************************************
 *
 * Function:    PWV_efnFileSelect
 *
 * Author:      Accenture
 *
 * Date :       05-08-2004
 *
 * Description: Select files for SCANDIR function
 *
 * Input Parameters:
 *
 *      tEntry: pointer to directory entry
 *
 * Output Parameters:
 *
 *      N/A
 *
 * Return Values:       PWV_TRUE: found desired values
 *                      PWV_FALSE: found unwanted values
 *
 * Changes History:
 *
 ******************************************************************************/
int
PWV_efnFileSelect(struct dirent *tEntry)
{
        char *szPtr;

        /* Exclude current and above DIRs file type */
        if ((strcmp(tEntry->d_name, PWV_CUR_DIR) == 0) || (strcmp(tEntry->d_name, PWV_PREV_DIR) == 0))
                return PWV_FALSE;

        /* Check for filename extension */
        szPtr = rindex(tEntry->d_name, '.');
        if((szPtr != NULL) && ((strcmp(szPtr, PWV_RTSWFILE_EXTENSION) == 0)))
                return PWV_TRUE;
        else
                return PWV_FALSE;
}



int
PWV_efnReadAllFilesToBuffer(tProcessEnvironment *ptEnvironment)
{
	int i, iAllFiles=0;
	struct dirent **ptDP;
	char szFilename[1024];
	int filesizes[48];
	int currentBufferPos=0;
	FILE *pfdRTSWFile = NULL;
    DIR *pddWorkDir = NULL;
    char **pszRTSWFileNames = (char **)malloc(sizeof(char **));
    struct dirent *tNewEntry = NULL;
	
	/* iAllFiles = scandir(ptEnvironment->szInputFullPath, &ptDP, PWV_efnValidFile, alphasort); */

    /* ********************** *
     * SCANDIR implementation */
    pddWorkDir = opendir(ptEnvironment->szInputFullPath);
    while((tNewEntry = readdir(pddWorkDir)) != NULL)
    {
        /* if(PWV_efnFileSelect(tNewEntry) == PWV_TRUE) */
        if(PWV_efnValidFile(tNewEntry) == PWV_TRUE)
        {
                pszRTSWFileNames = (char **)realloc(pszRTSWFileNames, sizeof(char *)*(iAllFiles + 1));
                pszRTSWFileNames[iAllFiles] = (char *)malloc(sizeof(char)*1024);
                sprintf(pszRTSWFileNames[iAllFiles], "%s", tNewEntry->d_name);
                iAllFiles += 1;
        }
        tNewEntry = NULL;
    }
    closedir(pddWorkDir);
    /* END                                    *
     * SCANDIR implementation *
     * ********************** */


	ptEnvironment->filesize = 0;

	for (i=0; i<iAllFiles; i++)
	{
		sprintf(szFilename,"%s%s", ptEnvironment->szInputFullPath, /* ptDP[i]->d_name */pszRTSWFileNames[i]);
		if ((filesizes[i] = PWV_efnGetFileSize(szFilename)) < 0)
		{
			printf("Error getting file size for file %s\n", szFilename);
			return PWV_RV_ERROR;
		}
		ptEnvironment->filesize += filesizes[i];
	}
	
    ptEnvironment->buffer = (char *)malloc(ptEnvironment->filesize);
    if(ptEnvironment->buffer==NULL)
    {
        printf("Invalid buffer where to read files\n");
        return PWV_RV_ERROR;
    }
	
	
	
	for (i=0; i<iAllFiles; i++)
	{
		sprintf(szFilename,"%s%s", ptEnvironment->szInputFullPath, /* ptDP[i]->d_name */pszRTSWFileNames[i]);
		if (PWV_RV_OK != PWV_efnReadCompleteFile(szFilename, &(ptEnvironment->buffer[currentBufferPos])) )
		{
			printf("Error reading complete file %s\n", szFilename);
			return PWV_RV_ERROR;
		}
		currentBufferPos += filesizes[i];
	}
	
	return PWV_RV_OK;
}



int
PWV_efnGetFileSize(char *szFilename)
{
	int filesize;
    FILE *f;
	
	if((f = fopen(szFilename,"rb")) == NULL)
    {
        printf("Error opening %s\n",szFilename);
        return PWV_RV_ERROR;
    }

	fseek(f, 0, SEEK_END);
    filesize = ftell(f);     
	fclose(f);
	
	return filesize;
}



/******************************************************************************
 *
 * Added 18 January 2006
 * Calculate CRC32 to validate wager CDs
 *
 ******************************************************************************/
 unsigned long CRCTable[ 256 ];

/******************************************************************************
 *
 * Function:    PWV_efnGenerateCRC32
 *
 * Author:      Accenture 
 *
 * Date :       18-01-2006
 * 
 * Description: Calculate CRC32 of wgr files in CD
 *				
 *
 * Input Parameters:
 *
 * 	N/A
 *	
 * Output Parameters:
 *
 *	N/A
 *   
 * Return Values:	PWV_RV_OK: Success
 *			PWV_RV_ERROR: Error
 *
 * Changes History:
 *
 ******************************************************************************/
PWV_ReturnValue PWV_efnGenerateCRC32(tProcessEnvironment *ptEnvironment)
{
  FILE  *pfdCRCFile = NULL;
  FILE  *pfdWGRFile = NULL;
  char  pszCRCFileName[FILE_NAME_MAX_LENGTH];
  char  pszWGRFileName[FILE_NAME_MAX_LENGTH];
  long  numread = 0;
  long  recordsRead = 0;
  int   i = 0;
  int   a = 0;
  int   bufferPosition = 0;
  char  fileName[FILE_NAME_MAX_LENGTH];
  char  originalCRC[10];
  unsigned long newCRC;
  char  calculatedCRC[10];

  char bufferAll[RECORDS_READ * RECORD_LENGTH+1];
  char buffer[RECORD_LENGTH+1];

  char szMessage[PWV_LOG_MAXPATHLEN];
  memset(szMessage, PWV_ZERO_MEM, PWV_LOG_MAXPATHLEN);

  BuildCRCTable();

  /* Ler ficheiro CRC */
  sprintf(pszCRCFileName, "%s%s", ptEnvironment->szRTSWFilesFullPath, CRC_FILE_NAME);
  pfdCRCFile = fopen(pszCRCFileName, PWV_READ_ASCII_M);

  if (NULL != pfdCRCFile)
  {
    do
    {
	memset(bufferAll, '\0', ((RECORDS_READ * RECORD_LENGTH + 1) * sizeof(char)));
      numread = fread(bufferAll, sizeof(char), (RECORDS_READ * RECORD_LENGTH), pfdCRCFile);
      
      bufferAll[RECORDS_READ * RECORD_LENGTH]= '\0';

      if( ferror(pfdCRCFile))
      {
        sprintf(szMessage, "%s %s", PWV_ERROR_OPENING_CRCFILE, pszCRCFileName);
        PWV_efnLogMessage(PWV_MP_ERROR, "PWV_efnGenerateCRC32", szMessage);
        fclose(pfdCRCFile);
        return PWV_RV_ERROR;
      } 

      recordsRead = 0;
      i = 0;
      recordsRead = (long)(numread / sizeof(RECORD_LENGTH));
        
      if (recordsRead > 0)
      {
        for (i=0; i<=recordsRead; i++)
        {
        	
        
          memset(buffer, '\0', ((RECORD_LENGTH + 1) * sizeof(char)));
          
          bufferPosition = (RECORD_LENGTH+1)*i; 
          
          if(bufferAll[bufferPosition] != 'E'){
          	break;
          }
          
          for ( a=0; a<RECORD_LENGTH; a++)
          {
            if ( i == 0 )
              bufferPosition = a+RECORD_LENGTH*i;
            else
              bufferPosition = i + a + RECORD_LENGTH*i;

            sprintf(buffer, "%s%c", buffer, bufferAll[bufferPosition]);
            
          }
          
          buffer[RECORD_LENGTH]= '\0';

	  
          memset(fileName, '\0', IN_CRC_FILE_NAME * sizeof(char));

          memset(originalCRC, '\0', IN_CRC_FILE_CRC * sizeof(char));
          

          for ( a=0; a<IN_CRC_FILE_NAME; a++)
          {
            sprintf(fileName, "%s%c", fileName, buffer[a]);
          }

          for ( a=45; a<55; a++)
          {
            sprintf(originalCRC, "%s%c", originalCRC, buffer[a]);
          }

          fileName[IN_CRC_FILE_NAME-1]= '\0';

          originalCRC[IN_CRC_FILE_CRC-1]= '\0';

          /* Calcular novo CRC */
          sprintf(pszWGRFileName, "%s%s", ptEnvironment->szRTSWFilesFullPath, fileName);

          pfdWGRFile = fopen(pszWGRFileName, PWV_READ_M);

          if (NULL != pfdWGRFile)
          {
            if( ferror(pfdWGRFile))
            {
              	sprintf(szMessage, "%s%s", PWV_ERROR_OPENING_WGRFILE, fileName);
              	PWV_efnLogMessage(PWV_MP_ERROR, "PWV_efnGenerateCRC32", szMessage);
              	fclose(pfdCRCFile);
              	return PWV_RV_ERROR;
            } 
            
            sprintf(szMessage, "A processar o ficheiro %s...", fileName);
            PWV_efnLogMessage(PWV_MP_INFORMATION, "PWV_efnGenerateCRC32", szMessage);
            
            newCRC = CalculateFileCRC( pfdWGRFile );
            sprintf(calculatedCRC, "%08lx", newCRC);

            /* Validar que os CRC são iguais */
            if ( strncmp(originalCRC, calculatedCRC,strlen(calculatedCRC)) != 0 )
            {
              sprintf(szMessage, "%s%s - Novo CRC = %s", PWV_ERROR_CRC_INCORRECT, fileName, calculatedCRC);
              PWV_efnLogMessage(PWV_MP_ERROR, "PWV_efnGenerateCRC32", szMessage);
              fclose(pfdCRCFile);
              return PWV_RV_ERROR;
            }
            else
            {
              sprintf(szMessage, "Ficheiro %se valido!", fileName);
              PWV_efnLogMessage(PWV_MP_INFORMATION, "PWV_efnGenerateCRC32", szMessage);
            }
          }
          else
          {
            if ( strncmp(fileName, "EM", 2) == 0 )
            {
              sprintf(szMessage, "%s%s", PWV_ERROR_WGR_NOT_EXIST, fileName);
              PWV_efnLogMessage(PWV_MP_ERROR, "PWV_efnGenerateCRC32", szMessage);
            }
          }          
        }        
      }
    } while (numread == (RECORDS_READ * sizeof(RECORD_LENGTH)));

    
    fclose(pfdCRCFile); 
  }
  else
  {
     sprintf(szMessage, "%s%s", PWV_INFO_INVALID_CRC_FILE, pszCRCFileName);
     PWV_efnLogMessage(PWV_MP_ERROR, "PWV_efnGenerateCRC32", szMessage);
     return PWV_RV_ERROR;
  }
  return PWV_RV_OK;
}


void BuildCRCTable()
{
  int i;
  int j;
  unsigned long crc;

  for ( i = 0; i <= 255 ; i++ ) 
  {
    crc = i;
    for ( j = 8 ; j > 0; j-- ) 
    {
      if ( crc & 1 )
        crc = ( crc >> 1 ) ^ CRC32_POLYNOMIAL;
      else
        crc >>= 1;
    }
    CRCTable[ i ] = crc;
  }
}

unsigned long CalculateFileCRC( FILE *file )
{
  unsigned long crc;
  int count;
  unsigned char buffer[ 512 ];
  int i;

  crc = 0xFFFFFFFFL;
  i = 0;
  for ( ; ; ) 
  {
    count = fread( buffer, 1, 512, file );
    if ( ( i++ % 32 ) == 0 )
      if ( count == 0 )
        break;
    crc = CalculateBufferCRC( count, crc, buffer );
  }
  return( crc ^= 0xFFFFFFFFL );
}

unsigned long CalculateBufferCRC( unsigned int count, unsigned long crc, void *buffer )
{
  unsigned char *p;
  unsigned long temp1;
  unsigned long temp2;

  p = (unsigned char*) buffer;
  while ( count-- != 0 ) 
  {
    temp1 = ( crc >> 8 ) & 0x00FFFFFFL;
    temp2 = CRCTable[ ( (int) crc ^ *p++ ) & 0xff ];
    crc = temp1 ^ temp2;
  }
  return( crc );
}


