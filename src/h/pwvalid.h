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

/******************************************************************************
 *
 * Module Name     : Prized Wagers Validation 
 *
 ******************************************************************************
 *
 * File Name       : pwvalid.h
 *
 * Author          : Accenture
 *
 * Date            : 05-08-2004
 *
 *
 * Modifications History
 * ============================================================================
 * VER  DD-MM-YYYY  AUTHOR        COMMENTS
 *
 * V03  07-10-2013  Accenture     Single line comment substituted by block comment
 * V02  10-04-2013  Accenture     Changed PWV_FULL_RTSWFILES_PATH (new CD drive is DNA0)
 *                                Changed PWV_WELLCOME_MSG (new version is v3.0)
 *                                Changed PWV_INSERT_EXT_SERIAL_NUM_MSG
 *                                Changed PWV_CDCINITIALDATE_IN_SEC
 *                                Added   PWV_INSERT_TICKET_YEAR_MSG
 *                                Added   PWV_TICKET_YEAR
 *                                Added   PWV_INFO_INVALID_TICKET_YEAR
 *                                Changed tProcessEnvironment structure (added new field iTicketYear)
 *                                Changed PWV_GetTicketCDCDate function signature
 *                                Created PWV_GetTicketYear function
 *
 * Date: 07-08-2004  Author: Pedro Miguel Sousa
 * Comments: Stand Alone application for use by the Jury in the "Jury's Process"
 *			 that makes a verification of the prized wagers in a certain contest
 *			 by searching Wager Files from RTS EuroMillion Module.
 *
 ******************************************************************************/

 #ifndef PWVALID_H
 #define PWVALID_H

 /*----------------------------------------------------------------------------*/
 /* Includes 								       							   */
 /*----------------------------------------------------------------------------*/
 #include <stdio.h>
 #include <stdlib.h>
 #include <string.h>
 #include <time.h>
 #include <errno.h>
 
 #include <math.h>
 
 #include <sys/types.h>
 #include <strings.h>
 #include <dirent.h>
 #include <unistd.h>

 /*----------------------------------------------------------------------------*/
 /* Function-like Macros                                                       */
 /*----------------------------------------------------------------------------*/
#define UTILS_CLOSE_LOG()\
		eRetVal = PWV_efnCloseLog();\
	   	if(PWV_RV_OK != eRetVal)\
			return PWV_RV_ERROR;\
		PWV_efnLogMessage(PWV_MP_INFORMATION, "main", PWV_INFO_LOG_CLOSED);\
   		printf("\n");\
   		return PWV_RV_OK;\

 /*----------------------------------------------------------------------------*/
 /* Constant Definitions                                                       */
 /*----------------------------------------------------------------------------*/

 /*--- Functions Control ---*/
 #define PWV_SUCCESS			1
 #define PWV_FAILURE			!PWV_SUCCESS
 #define PWV_TRUE			1
 #define PWV_FALSE			0

 /*--- Program name for log ---*/
 #define PWV_MODULE_NAME         	"PWVALID"
 #define PWV_FINISHED_MESG       	"Validacao de Apostas Premiadas finalizou com sucesso."
 /*--- Program Paths ---*/
 #define PWV_HOME_PATH		 	"[.PRIZED_WAGERS_VALIDATION%s]" /* "[WRK.PMJ.PRIZED_WAGERS_VALIDATION%s]" */
 #define PWV_REL_LOG_PATH	 	".LOG"
 
 /* V02 - Changed CD drive */
 /*#define PWV_FULL_RTSWFILES_PATH 	"DQA0:[000000]"//"[WRK.PMJ.RTS]"//*/
 #define PWV_FULL_RTSWFILES_PATH 	"DNA0:[000000]" /* "[WRK.PMJ.RTS]" */
 
 #define PWV_REL_INPUT		 	".INPUT"
 #define PWV_REL_SUCCESS	 	".SUCCESS"
 #define PWV_REL_ERROR		 	".ERROR"

 /*--- Error Messages ---*/
 #define PWV_MALLOC_FAILURE_MSG			"ERRO FATAL: alocacao de memoria falhou"
 #define PWV_ERROR_OPEN_LOG			"ERRO FATAL: impossivel abrir o ficheiro de LOG de %s, linha %d, erro %d - %s"
 #define PWV_ERROR_CLOSE_LOG			"ERRO: impossivel fechar o ficheiro de LOG da aplicacao"
 #define PWV_ERROR_INIT_BASIC_ENV		"ERRO FATAL: impossivel inicializar o ambiente debase de %s, linha %d, erro %d - %s"
 #define PWV_ERROR_INIT_ENVIRONMENT	   	"ERRO: erro a inicializar a estrutura de ambiente"
 #define PWV_ERROR_CLEANING_ENV			"ERRO: erro a limpar ficheiros antigos do ambiente"
 #define PWV_ERROR_SEARCHING_REF_NUM		"ERRO: erro a pesquisar número de serie"
 #define PWV_ERROR_COPYING_RTSWFILES		"ERRO: erro a copiar os ficheiros de apostas"
 #define PWV_ERROR_MOVING_RTSWF_TO_ERROR 	"ERRO: erro a mover os ficheiros de apostas para o directorio de ERRO"
 #define PWV_ERROR_MOVING_RTSWF_TO_SUCCESS	"ERRO: erro a mover os ficheiros de apostas para o directorio de SUCCESSO"
 #define PWV_ERROR_GETTING_RTSWFILENAMES	"ERRO: erro a obter os nomes dos ficheiros de apostas"
 #define PWV_ERROR_OPENING_RTSWFILE				"\
ERRO: programa %s, linha %d, erro %d - %s\n\
      nao foi possivel abrir o ficheiro de apostas %s"
 #define PWV_ERROR_CLOSING_RTSWFILE				"\
ERRO: programa %s, linha %d, erro %d - %s\n\
      nao foi possivel fechar o ficheiro de apostas %s"
 #define PWV_FSEEK_FAILURE_MSG				"ERRO: erro a colocar-se no ficheiro: %s"
 #define PWV_ERROR_READING_RTSWFILE			"ERRO: erro a ler do ficheiro de apostas: %s"
 
 #define PWV_ERROR_SERIALNUM_CONVERSION			"ERRO: erro na descodificacao do Numero de Serie"
 
 /*--- Information Messages ---*/
 #define PWV_INFO_LOG_CLOSED				"INFORMACAO: ficheiro de LOG fechado com sucesso"
 #define PWV_INFO_INVALID_WEEK				"Semana invalida!"
 #define PWV_INFO_INVALID_YEAR				"Ano invalido!"
 #define PWV_INFO_VALID_DRAWID				"Codigo de Concurso: [%s]"
 /* Alterado 18-01-2006 isjacinto */
 #define PWV_INFO_INVALID_RTSMENU_OPTION		"Opcao invalida {1, 2, 3, 4}!"
 #define PWV_INFO_INVALID_MENU_OPTION			"Opcao invalida {1, 2}!"
 #define PWV_INFO_INVALID_REF_NUM			"Numero de Serie invalido!"
 /* Alterado 03-10-2006 Bruno */
 #define PWV_INFO_INVALID_CD_CHECK	"\n\n\t\t\t----- CD DE APOSTAS INVALIDO -----\n"
 #define PWV_INFO_OK_CD_CHECK	"\n\n\t\t\t----- CD DE APOSTAS VALIDO -----\n"
 #define PWV_INFO_MENU_CD_CHECK	"\n\n\t---- AVISO: Deve copiar os ficheiros antes de validar o CD ----\n"
 #define PWV_INFO_INVALID_CRC_FILE	"Ficheiro de CRC Errado."
 /*--- Warning Messages ---*/
 #define PWV_RTSWFILE_OK				"OK"
 #define PWV_RTSWFILE_NOT_OK				"NOT OK "
 #define PWV_RTSWFILE_STATUS_DISPLAY			"%s ---> %s: "
 
 #define PWV_RTSWF_WRONG_NB_READ			"Numero errado de bytes lidos do ficheiro"
 #define PWV_DIF_FILE_CONTEST_CODE			"Codigo de Concurso do ficheiro nao corresponde ao introduzido pelo utilizador" 
 #define PWV_INVALID_STATE_PROP				"AVISO: Propriedade EstadoAposta Multipla do bilhete (ID: %s) no ficheiro %s nao esta entre (0..5)"
 #define PWV_INVALID_VALUE_IN_MATRIX			"AVISO: Aposta de bilhete (ID: %s) no ficheiro %s tem valores invalidos (can't be <1, >50, or 0 in simple matrix)"
 #define PWV_INVALID_MULTIPLE_MATRIX			"AVISO: Aposta Multipla de bilhete (ID: %s) no ficheiro %s invalida (5 números e 2 estrelas)"
 
 /*--- WELLCOME Messages ---*/
 /* V02 - Changed version to v3.0 */
 #define PWV_WELLCOME_MSG						"\
\t---------------------------------------------------------------\n\
\t----   VALIDACAO DE APOSTAS PREMIADAS v3.0   ------------------\n\n"
 
  /*--- Get RTS Files Messages ---*/
 #define PWV_GET_RTSWFILES_MSG					"\
\t---------------------------------------------------------------\n\
\t----------  Pronto para Copiar Ficheiros de Apostas  ----------\n\
\t    [1] Copiar agora (ficheiros previamente disponibilizados)\n\
\t    [2] Continuar\n\
\t    [3] Validar ficheiros de CD\n\
\t    [4] Sair para o sistema\n\
\t   > "
 
 /*--- MENU Messages ---*/
 #define PWV_MENU_MSG							"\
\t---------------------------------------------------------------\n\
\t-----------  Pronto para Validar Apostas Premiadas  -----------\n\
\t    [1] Procurar Numeros de Serie de bilhetes premiados\n\
\t    [2] Sair para o sistema\n\
\t   > "

 /* V02 - Changed option number */
/* #define PWV_INSERT_EXT_SERIAL_NUM_MSG					"\*/
/*\t    [1] Insira o Numero de Serie do bilhete a procurar\n\*/
/*\t   > "*/
 #define PWV_INSERT_EXT_SERIAL_NUM_MSG					"\
\t    [2] Insira o Numero de Serie do bilhete a procurar\n\
\t   > "

 /*--- DISPLAY Messages ---*/
 #define PWV_MSG_WAGER_TIME_LOCAL	"\n\n\n\n\nBILHETE:   registado no concurso %s em %s %s\nTERMINAL:  %s\nESTADO:    %s\n\n"
 #define PWV_MESSAGE_TAB		"   "
 #define PWV_MSG_REF_NUM_SPACES		"    "
 #define PWV_MSG_CONTEST_SPACES		"               "
 #define PWV_MSG_MULTIPLE_SPACES	"     "
 #define PWV_MSG_TOTPRICE_SPACES	"                   "
 #define PWV_DISP_WAGER_DATA_HEADER	"\n\
NUMERO DE SERIE%sNUMERO DE SEMANAS%sMULTIPLA%sPRECO TOTAL DO BILHETE(EUR)\n"
/*\tREFERENCE NUMBER%sCONTESTS%sMULTIPLE%sTOTAL PRICE (EUR)\n"*/
 #define PWV_DISP_WAGER_DATA					"\
%s%s%s%s%s%s%s%s%s%s%s\n"

 #define PWV_DISP_WAGER_M1	"Conjunto 1:  "
 #define PWV_DISP_WAGER_M2	"Conjunto 2:  "
 #define PWV_DISP_WAGER_M3	"Conjunto 3:  "
 #define PWV_DISP_WAGER_M4	"Conjunto 4:  "
 #define PWV_DISP_WAGER_M5	"Conjunto 5:  "

 #define PWV_DISP_WAGERS_LINE				"\
----------------------------------------------------------------------------\n"

  /*## Command Line ##*/
 #define PWV_LEN_COM            	 255
 #define PWV_COM_CP_OVER		"copy "		/* command to copy files */
 #define PWV_COM_MV_OVER		"rename "	/* command to move files */
 #define PWV_COM_RM_OVER		"delete "	/* command to delete files */
 #define PWV_COM_TEST_DIR		"test -d %s"	/* command test that return TRUE if file exists and is a directory */
 #define PWV_COM_MKDIR_INTERM		"create/directory %s"	/* command to create directories (and intermediate ones) */
 #define PWV_COM_GEN_STRING		"*"		/* command generic string */
 #define PWV_DOT			"."		/* dot */
 #define PWV_CUR_DIR			"."		/* current directory */
 #define PWV_PREV_DIR			".."		/* previous directory */
 #define PWV_ALL_FILES			"*.*;*"		/* version number 1 of files */

 /*## other constants ##*/
 #define PWV_CICLE_LL			0		/* cicle counting lower level */
 #define PWV_0_ASC_VAL			48		/* value for 0 (zero) ascii value */
 #define PWV_NULL_CHAR			'\0'
 #define PWV_SPACE_CHAR			' '
 #define PWV_ZERO_MEM			0x00
 #define PWV_NULL			NULL
 
 #define PWV_MAX_PATH_LEN		255
 #define PWV_DATE_LEN			8
 #define PWV_LOG_MAXPATHLEN		1024

 #define PWV_24HOURS_IN_SECONDS		86400		/* (86400 seconds) = (24 hours) = (1 day) */
 /* V02 - Changed */
 /*#define PWV_CDCINITIALDATE_IN_SEC	988585200	// (in Seconds) CDC initial date is a base for CDC calculations */
 #define PWV_CDCINITIALDATE_IN_SEC	988675200	/* Number of seconds from 01 Jan 1970 00:00:00 GMT until 01 May 2001 00:00:00 GMT (CDC initial date) */
 #define PWV_JULIANDATE_OFFSET		500

 /*--- Contest Definitions ---*/
 #define NUM_TOTAL_NUMBERS		50
 #define NUM_MAX_NUMBERS		11
 #define NUM_MIN_NUMBERS		5
 #define NUM_TOTAL_STARS		9
 #define NUM_MAX_STARS			9
 #define NUM_MIN_STARS			2
 
 #define PWV_TERMINAL_CODE_LEN		7	/* bytes for ALTURA's terminal code */
 #define PWV_AAAA_DRAWID_LEN		6	/* bytes for Draw ID with SSAAAA format */

 #define PWV_EXTERNAL_SERIAL_NUMBER	8	/* bytes for External Serial Number that's related with ID EuroMillion (Reference Number) */
 	/* External Serial Number */
 #define PWV_JULIAN_DATE_LEN		3	/* bytes dor ExtSerialNumber's Julian Date */
 #define PWV_CHECK_DIGIT_LEN		3	/* bytes dor ExtSerialNumber's Check Digit */
 #define PWV_TICKET_SERIAL_NUMBER	PWV_JULIAN_DATE_LEN + PWV_EXTERNAL_SERIAL_NUMBER + PWV_CHECK_DIGIT_LEN

 /*## File ##*/
 #define PWV_READ_M			"rb"	/* read openfile mode */
 #define PWV_READ_ASCII_M		"r"	/* read ascii openfile mode */
 #define PWV_WRITE_M			"wb"	/* write openfile mode */

 /*## RTS Wager Files ##*/
 	/* --- Offsets --- */
 #define PWV_FILESIZE_LEN				100
 	/* --- Name --- */
 #define PWV_ALLWAGERSFILES_FILENAME	"RTS_FILES.WGR"
 #define PWV_FILESIZES_FILENAME			"FILE_SIZES.WGR"
 #define PWV_LEN_RTSWF_EXTENSION		4
 #define PWV_RTSWFILE_EXTENSION			".wgr"
  	/* --- Header --- */
 #define PWV_RTSWF_HEADER_LEN			(PWV_TERMINAL_CODE_LEN + PWV_AAAA_DRAWID_LEN) /* bytes for file header */
  	/* --- Body --- */
 #define PWV_RTSWF_TICKET_REG_LEN		109	/* bytes for ticket register */
 
 #define PWV_RTSWF_TICK_EMID_LEN		11	/* bytes for ticket EuroMillion ID */
 #define PWV_RTSWF_TICK_P_EMID_LEN		11	/* bytes for ticket Parent EuroMillion ID */
 #define PWV_RTSWF_TICK_TIMESTAMP_LEN	14	/* bytes for ticket Timestamp */
 #define PWV_RTSWF_TICK_RETRYID_LEN		13	/* bytes for ticket Retry ID */
 #define PWV_RTSWF_TICK_STATE_LEN		1	/* bytes for ticket State property */
 #define PWV_RTSWF_TICK_MCONTEST_LEN	2	/* bytes for ticket Multi Contest property */
 #define PWV_RTSWF_TICK_MULTIPLE_LEN	1	/* bytes for ticket Multiple property */
 #define PWV_RTSWF_TICK_M1_LEN			20	/* bytes for ticket 1st Matrix */
 #define PWV_RTSWF_TICK_M2_LEN			7	/* bytes for ticket 2nd Matrix */
 #define PWV_RTSWF_TICK_M3_LEN			7	/* bytes for ticket 3rd Matrix */
 #define PWV_RTSWF_TICK_M4_LEN			7	/* bytes for ticket 4th Matrix */
 #define PWV_RTSWF_TICK_M5_LEN			7	/* bytes for ticket 5th Matrix */
 #define PWV_RTSWF_TICK_WPRICE_LEN		8	/* bytes for ticket Wager Price */
	/* --- Ticket Reading format --- */
 #define PWV_RTSWF_NB_TICKET_P_BLOCK		10000	/* Nb of tickets in the Body Read Block */
 #define PWV_RTSWF_BODY_BLOCK_LEN			(PWV_RTSWF_NB_TICKET_P_BLOCK * PWV_RTSWF_TICKET_REG_LEN)	/* bytes for Body Read Block */
	/* --- Ticket Properties --- */
 #define PWV_RTSWF_ACTIVE_TICK			"1"	/* value for ACTIVE tickets */
 #define PWV_RTSWF_MULTI_WAGER			"0"	/* value for multiple matrix in ticket */
 #define PWV_RTSWF_1_SMATRIX			"1"	/* value for 1 simple matrix */
 #define PWV_RTSWF_2_SMATRIX			"2"	/* value for 2 simple matrix */
 #define PWV_RTSWF_3_SMATRIX			"3"	/* value for 3 simple matrix */
 #define PWV_RTSWF_4_SMATRIX			"4"	/* value for 4 simple matrix */
 #define PWV_RTSWF_5_SMATRIX			"5"	/* value for 5 simple matrix */
 #define PWV_RTSWF_IRR_NUMSTAR			0 	/* irrelevant number/star in multiple */

/******************************************************************************
 *
 * Added 18 January 2006
 * Calculate CRC32 to validate wager CDs
 * 
 ******************************************************************************/
 
 #define CRC32_POLYNOMIAL     		0xEDB88320L
 #define CRC_FILE_NAME			"EMCRC.crc"
 #define RECORDS_READ 			20
 #define FILE_NAME_MAX_LENGTH		30
 #define RECORD_LENGTH			56
 #define PWV_ERROR_OPENING_CRCFILE	"ERRO: Erro a abrir o ficheiro de CRCs - "
 #define PWV_ERROR_OPENING_WGRFILE	"ERRO: Erro a abrir o ficheiro "
 #define PWV_ERROR_CRC_INCORRECT	"ERRO: Erro no CRC do ficheiro "
 #define PWV_ERROR_WGR_NOT_EXIST	"ERRO: Ficheiro nao existe no CD - "
 
 #define IN_CRC_FILE_NAME		25
 #define IN_CRC_FILE_CRC		10

/****************************************************************************** 
 *                                                                              
 * V02 - Added for Ticket Year validation (USER input)
 *                                                                              
 ******************************************************************************/
 #define PWV_INSERT_TICKET_YEAR_MSG					"\
\t    [1] Insira o Ano de registo do bilhete\n\
\t   > "
 #define PWV_TICKET_YEAR		4	/* bytes for ticket's year */
 #define PWV_INFO_INVALID_TICKET_YEAR			"Ano invalido!"

 /*---------------------------------------------------------------------------*/
 /* Enumerations                                                              */
 /*---------------------------------------------------------------------------*/

	typedef enum
	{
	  B_FALSE=0,
	  B_TRUE=1
	} Boolean;
	
	typedef enum
	{
	  PWV_MP_INFORMATION=0,
	  PWV_MP_WARNING=1,
	  PWV_MP_ERROR=2,
	  PWV_MP_FATAL=3
	} PWV_MessagePriority;
	
	typedef enum
	{
	  PWV_RV_OK,		/* Function succeeded */
	  PWV_RV_ERROR,		/* Function failed */
	  PWV_RV_FATAL
	} PWV_ReturnValue;

 /*----------------------------------------------------------------------------*/
 /* Type Definitions                                                           */
 /*----------------------------------------------------------------------------*/
	/*## Environment Structure ##*/
	typedef struct
	{ 
		char szHomePath				[PWV_MAX_PATH_LEN];		/* PWV_HOME_PATH */
		char szRTSWagersFilesPath		[PWV_MAX_PATH_LEN];		/* PWV_REL_RTSWFILES_PATH */
		char szRTSWFilesFullPath		[PWV_MAX_PATH_LEN];
		char szRTSWagerFilesExtension		[PWV_LEN_RTSWF_EXTENSION + 1];	/* PWV_RTSWFILE_EXTENSION */
		char szInputPath			[PWV_MAX_PATH_LEN];		/* PWV_REL_INPUT */
		char szInputFullPath			[PWV_MAX_PATH_LEN];
		char szErrorPath			[PWV_MAX_PATH_LEN];		/* PWV_REL_ERROR */
		char szErrorFullPath			[PWV_MAX_PATH_LEN];
		char szSuccessPath			[PWV_MAX_PATH_LEN];		/* PWV_REL_SUCCESS */
		char szSuccessFullPath			[PWV_MAX_PATH_LEN];
		char szLogPath				[PWV_MAX_PATH_LEN];		/* PWV_REL_LOG_PATH */
		char szLogFullPath			[PWV_MAX_PATH_LEN];
		
		char szDrawID				[PWV_AAAA_DRAWID_LEN + 1];
		char szUserIDem				[PWV_RTSWF_TICK_EMID_LEN + 1];
		
		int  iTicketYear; /* V02 - Added */
		
		int  iJulianDate;
		int  iExternalSerialNum;
		int  iCheckDigit;
		
		char szRunSysCom			[PWV_LEN_COM];
		
		char *buffer;
		int filesize;

	} tProcessEnvironment;
	
	/*## RTS Wager Files Header ##*/
	typedef struct
	{
		char szTermCode		[PWV_TERMINAL_CODE_LEN + 1];
		char szDrawID		[PWV_AAAA_DRAWID_LEN + 1];
	}RTSWFHeaderVar;
	
	/*## RTS Wager Files Body Ticket ##*/
	typedef struct
	{
		char szTermCode		[PWV_TERMINAL_CODE_LEN + 1]; 
		char szIDem		[PWV_RTSWF_TICK_EMID_LEN + 1];
		char szParentIDem	[PWV_RTSWF_TICK_P_EMID_LEN + 1];
		char szTimeStamp	[PWV_RTSWF_TICK_TIMESTAMP_LEN + 1];
		char szRetryID		[PWV_RTSWF_TICK_RETRYID_LEN + 1];
		char szState		[PWV_RTSWF_TICK_STATE_LEN + 1];
		char szMContest		[PWV_RTSWF_TICK_MCONTEST_LEN + 1];
		char szMultiple		[PWV_RTSWF_TICK_MULTIPLE_LEN + 1];
		unsigned int iM1	[PWV_RTSWF_TICK_M1_LEN];
		int iM1NumNumbers;
		int iM1NumStars;
		unsigned int iM2	[PWV_RTSWF_TICK_M2_LEN];
		unsigned int iM3	[PWV_RTSWF_TICK_M3_LEN];
		unsigned int iM4	[PWV_RTSWF_TICK_M4_LEN];
		unsigned int iM5	[PWV_RTSWF_TICK_M5_LEN];
		char szWagerPrice	[PWV_RTSWF_TICK_WPRICE_LEN + 1];
	}RTSWFBodyTicketVar;

 /*----------------------------------------------------------------------------*/
 /* Data Declarations                                                          */
 /*----------------------------------------------------------------------------*/
	/* Should be part of errno.h, but isn't */
	extern char *sys_errlist[];
	extern int sys_nerr;

 /*----------------------------------------------------------------------------*/
 /* Function Prototypes                                                        */
 /*----------------------------------------------------------------------------*/

	/* Initialization and environment functions */
	PWV_ReturnValue	PWV_efnInitializeBasicEnv(tProcessEnvironment *);
	PWV_ReturnValue	PWV_efnInitializeEnvironment(tProcessEnvironment *);
	PWV_ReturnValue	PWV_efnCleanEnvironment(tProcessEnvironment *);

	void PWV_CallRTSMenu(int *);
	PWV_ReturnValue	PWV_efnGetRTSWFiles(tProcessEnvironment *);
	void PWV_CallMenu(int *);
	void PWV_GetReferenceNumber(tProcessEnvironment *, int *);
	
	/* V02 - Changed function signature */
	/* void PWV_GetTicketCDCDate(int , int *); */
	void PWV_GetTicketCDCDate(int, int , int *);
	
	PWV_ReturnValue	PWV_efnSearchRefNumInFiles(tProcessEnvironment *);
	PWV_ReturnValue PWV_efnSearchAllFile(tProcessEnvironment *, FILE *, FILE *,
						 char *, char *, char *, char *,
					 	 Boolean *, Boolean *, Boolean *);
	
	void PWV_DisplayWagerData(char *, char *, Boolean, Boolean);

	void PWV_CopyTicketInfo(char *, char *, char *, char *);
	
	/* Added 18-01-2006 Calculate CRC32 to validate wgr files */
	PWV_ReturnValue PWV_efnGenerateCRC32(tProcessEnvironment *ptEnvironment);
	void BuildCRCTable();
	unsigned long CalculateFileCRC( FILE *file );
	unsigned long CalculateBufferCRC( unsigned int count, unsigned long crc, void *buffer );

	
	/* LOG functions */
	PWV_ReturnValue PWV_efnOpenLog(tProcessEnvironment *, const char *);	
	PWV_ReturnValue	PWV_efnLogMessage(const PWV_MessagePriority, const char *, const char *);
	PWV_ReturnValue	PWV_efnCloseLog();

	/* Fortran functions */
	extern int inpver(int *xcdc, int *xextser, int *xintser, int *xchkdig);
	
	PWV_ReturnValue PWV_GetTicketData(char *szActiveTicket, char *szActTickRTSWFHeader, Boolean *bRefNumCancelled, int iOffsets, int iHeaderOffsets, /*FILE *pfdRTSWFile, */tProcessEnvironment *ptEnvironment,Boolean *bRefNumCancelledAuto);
	PWV_ReturnValue PWV_efnHuntReferenceNumber(char *filename, int, char *text, int *piNumberOfOcurrences, int *ppiOffsets, int *ppiHeaderOffsets);
	int findHeaderOffset(char *buffer, int offset);
	
	PWV_ReturnValue
	PWV_efnReadCompleteFile(char *, char * /*, int * */);

	int PWV_efnValidFile(struct dirent *);
	int PWV_efnReadAllFilesToBuffer(tProcessEnvironment *);
	int PWV_efnGetFileSize(char *);

	int PWV_efnFileSelect(struct dirent *);

	/* V02 - Added */
	void PWV_GetTicketYear(tProcessEnvironment *);

 #endif /* PWVALID_H */
 /* EOF */
