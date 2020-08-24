C
C *** PROGRAM X2BLDNET ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2BLDNET.FOV                                 $
C  $Date::   18 Dec 1996 12:04:04                                         $
C  $Revision::   1.1                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C *** Pre-Baseline Source - x2bldnet.for ***
C
C V13 19-SEP-96 wsm Comment out file bitmap set in x2xstn.fil.
C X2X Upgrade: 01-MAR-96 wsm Added AGTINF.DEF in UPD_X2XTER_TBL routine,
C                            uncommented relay groups logic for Finland x21.
C V12 07-DEC-95 SCD Add counter for valid ASF records.  If there are no
C                   valid ASF records, then print an error message and exit.
C V11 31-Jul-95 das Added call to X2cnvdrp                                   
C V10 19-JUN-95 SCD Use range of stations in STNDEF.FIL for GTX station class
C		    instead of setting LAST_STN to the last ASYNCH station
C V09 22-OCT-94 SCD Use flag to skip start/end date format in ASF
C V08 06-OCT-94 SCD Use flag to determine start/end date format in ASF (could 
C		    be day month year or month day year)
C V06 22-SEP-94 GPR Check if GTX Dial station is needed
C V05 15-SEP-94 SCD Print an error message if a dummy record is found in
C		    the ASF, but no dummy range has been specified in the
C		    station definition file.
C V04 14-SEP-94 SCD Change station number assignment for X28 GVTs.  Instead
C		    of dynamically assigning the station number, we will
C		    now use the station number in the ASF and verify that the
C		    station number is in range based upon the station definition
C		    file.
C V03 02-AUG-94 SCD ADD DUMMMY STATION PROCESSING TO SUPPORT GVT INSTALLATION
C                   AND CHANGE VALID ADDRESS CHECKING FOR X.21 AND X.25
C V02 17AUG94 GPR CHECK START DATE IN ASF AND ALWAYS DO EDIT CHECKS
C V01 01-05-93 XXX INITIAL RELEASE FOR IRELAND
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C PURPOSE:
C	This program will scan the ASF file and automatically rebuild the
C	X2X files. This version  is intended for all network types. It is
C	based on the assumption that station class files are set up as follows:
C
C NOTE:
C	WHILE THE INTENT IS FOR THIS PROGRAM TO EVENTUALLY HANDLE ALL NETWORK
C	TYPES, IT CURRENTLY CAN ONLY BE USED FOR CLASSES 1, 2, 3, 4, 5, 
C       7, 8, 9 & 10. ALSO NOTE THAT NOT ALL COMBINATIONS ARE POSSIBLE.
C
C	STATION CLASS:	TYPE:
C             1         X.21
C             2         X.25
C             3         ASYNC
C             4         ISOCH
C             5         USAT
C             6         X25PVC
C             7         GTECH_DIAL (GVT)
C             8         GTX_DIAL   (PSEUDO)
C             9         X28PAD     (GVT)          
C            10         X25REMOTE                 
C
C The station configurations by class are as follows:
C                                                              Conn
C                  Addr  Port  Pvc_circuit  EVSN     Class     Type    Proto
C X.21   stations:  -     -      -           0         1         1       -
C X.25   stations:  -    N/D     -           0         2         2       -
C ASYPVC stations:  0    PVC     0           0         3         6       1
C ISOCH  stations:  0    PVC     0           0         4         6       1
C USAT   stations:  0    PVC     Y           Y         5        10       1
C X25PVC stations:  -     -      -           0         6         -       -
C GTECH  stations:  0     0      0           Y         7         9       3
C GTX    stations:  0    PVC     0           0         8         6       3
C X28    stations:  0    N/D     0           Y         9         4       1
C X25REM stations:  Y    N/D     0           N        10         2       1
C
C        
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Stations will be dynamically allocated based on the following criteria:
C
C    X.21:             Both station and drop should be defined in the ASF.
C                      This network type cannot have the station number
C                      assigned dynamically. Relationship between station
C                      and terminal is one to many. All terminals on a
C                      station of this type must have identical addresses
C                      defined in the ASF. Relationship between station and
C                      address is one to one. The port number is always 1.
C                      Since X.21 is almost always accomplished through relay
C                      broadcast, there is a requirement for a group number
C                      which must be present in the ASF. This group number
C                      is placed in the corresponding station record. All
C                      terminals on a station of this type must have identical
C                      group numbers defined in the ASF.
C
C    X.21 IMPORTANT:   The group number capability is new and requires changes
C                      made to the ASF to support it. The code is included to
C                      support the concept (search for *NEW*), however, it is
C                      currently commented out. To activate the code after the
C                      modifications to the ASF are made, simply un-comment it,
C                      and re-compile & re-link the program.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C    X.25:             The station should be defined in the ASF. This network
C                      type cannot have the station number assigned dynamically.
C                      Relationship between station and terminal is one to one.
C                      A terminal on a station of this type, must have an
C                      address defined in the ASF. Relationship between station
C                      and address is one to one. The port number is always 1.
C                      The drop is always @. Since X.25 is sometimes
C                      accomplished through relay broadcast, the use of a
C                      group number in the ASF is optional. This group number
C                      is placed in the corresponding station record.
C
C    X.25 IMPORTANT:   The group number capability is new and requires changes
C                      made to the ASF to support it. The code is included to
C                      support the concept (search for *NEW*), however, it is
C                      currently commented out. To activate the code after the
C                      modifications to the ASF are made, simply un-comment it,
C                      and re-compile & re-link the program.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C    ASYNC/ISOCH:      Both station and drop should be defined in the ASF.
C                      These network types cannot have the station number
C                      assigned dynamically. Relationship between station
C                      and terminal is one to many. Station resides in GTX. 
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C    GTECH_DIAL:       Station number should be defined in the ASF.
C                      This type of station must have an EVSN defined.
C                      Drop is always @. Relationship between station
C                      and terminal is one to one.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C    GTX_DIAL:         This station type is traditionally the last to be
C                      defined in the station file. Like the GTECH_DIAL
C                      station it will be dynamically assigned based on 
C                      the range for this class. Relationship between
C                      station and terminal is one to many. Station 
C                      resides in GTX.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C    X28GVT:           Stations numbers will NO LONGER be assigned dynamically
C                      based on the range of station numbers found in
C                      the station range definition file as of 14-SEP-94.
C		       This type of station must have an EVSN defined. Drop is
C		       always @. Relationship between station and terminal is
C                      one to one. Needs network/dial ports.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C    X25REMOTE:        Both station and drop should be defined in the ASF.
C                      This  network type  cannot have the station number
C                      assigned dynamically. Relationship between station
C                      and terminal is one to many. Station resides in
C                      GTX. One station per GTX. Requires a port to be
C                      defined in ASF, this port is similiar to line
C                      and is defined by the placement of linecard in the
C                      GTX by slot Needs X25 addresses. Must have network
C                      port defined in station class record.
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C    USAT (GSAT):      Both station and drop should be defined in the ASF.
C                      This  network type  cannot have the station number
C                      assigned dynamically. Relationship between station
C                      and terminal is one to many. Station resides in
C                      in gsat RPP (remote polling processer). One station per 
C                      RPP. Requires a port to be defined in stndef, this port
C                      refers to the local port of the satellite GTX. At
C                      best there can only be two (2) ports for usat stations
C                      per GTX. Usat stations can switch to dial backup.
C                      This requires psuedo station to be defined. 
C
C Given a Station Class, use that station class record to update the defaults. 
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	PROGRAM X2BLDNET
C
	IMPLICIT NONE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C INCLUDE FILES.
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:ASFSUBS.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'             !V03
	INCLUDE 'INCLIB:X2XSCL.DEF'
	INCLUDE 'INCLIB:X2XSPC.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
	INCLUDE 'INCLIB:X2XTER.DEF'
C
	INCLUDE 'INCLIB:X2BLDNET.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'			!V02
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PARAMETER DECLARATIONS.
C
	INTEGER*4	X2XSTN_BIGREC_SIZ_C1,		! BIG STN REC C*1 SIZE.
     *			X2XSTN_BIGREC_SIZ_I4,		! BIG STN REC I*4 SIZE.
C
     *			X2XTER_BIGREC_SIZ_C1,		! BIG TER REC C*1 SIZE.
     *			X2XTER_BIGREC_SIZ_I4		! BIG TER REC I*4 SIZE.
C
	PARAMETER (X2XSTN_BIGREC_SIZ_C1 =
     *             (RMS_MAXBYT / (X2XSTN_SECT*CON_SECBYT)) * DEC_BLKBYT)
	PARAMETER (X2XSTN_BIGREC_SIZ_I4 = X2XSTN_BIGREC_SIZ_C1 / 4)
C
	PARAMETER (X2XTER_BIGREC_SIZ_C1 =
     *             (RMS_MAXBYT / (X2XTER_SECT*CON_SECBYT)) * DEC_BLKBYT)
	PARAMETER (X2XTER_BIGREC_SIZ_I4 = X2XTER_BIGREC_SIZ_C1 / 4)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C LOCAL DECLARATIONS.
C
	INTEGER*4	ACLASS,
     *			AGENT,
     *			ANSWER,
     *			C1OFF,
     *			ERRCNT,
     *			FIRST_TYP,
     *			GTXSTN,
     *			GROUP,
     *			GVTSTN,
     *			I,
     *			ICLASS,
     *			IFILNAM(5),
     *			I4OFF,
     *			J,
     *			LAST_STN,
     *			PORT_COUNT(X2X_STATIONS),
     *			ST,
     *			STN,
     *			STNBLK,
     *			STNCLS,
     *			STN_COUNT(X2XC_CLASSES),
     *			TER,
     *			TERBLK,
     *			TPORT,
     *			USATSTN
CV04 *			X28STN
C
C ***** Start V02 changes *****
C
        INTEGER*2	DATBUF(12)                  !Date buffer
        INTEGER*2       DATBUF2(12)                 !Date buffer for sys time
        INTEGER*4       CERR                        !Conversion error check
        INTEGER*4       MON,DAY,YEAR                !Date variables
        INTEGER*4       BEGCDC,ENDCDC               !Begin/end CDC dates
        INTEGER*4       SYSDATE(3)                  !System date
C
C ***** End V02 changes *****
C
	INTEGER*4	X2XGRP(X2X_STATIONS),
C
     *			X2XSTN_BEG,
     *			X2XSTN_BIG_IOS,
     *			X2XSTN_BIGREC_I4(X2XSTN_BIGREC_SIZ_I4),
     *			X2XSTN_END,
     *			X2XSTN_RECSIZ,
     *			X2XSTN_RECS_PER_IO,
     *			X2XSTN_SML_IOS,
C
     *			X2XTER_BEG,
     *			X2XTER_BIG_IOS,
     *			X2XTER_BIGREC_I4(X2XTER_BIGREC_SIZ_I4),
     *			X2XTER_BITMAP_OFF,
     *			X2XTER_DROP_OFF,
     *			X2XTER_END,
     *			X2XTER_PORT_OFF,
     *			X2XTER_RECSIZ,
     *			X2XTER_RECS_PER_IO,
     *			X2XTER_SML_IOS,
     *			X2XTER_STATE_OFF,
     *			X2XTER_STN_OFF,
     *			X2XTER_TER_OFF,
     *			X2XTER_UPDATE_OFF
C
        INTEGER*4 DUMMY_STN_COUNTER(X2XC_CLASSES)       ! COUNTER FOR # OF !V03
                                                        ! DUMMY STNS ASSIGNED
        INTEGER*4 VALID_ASF_COUNT                       ! COUNTER FOR # OF NON-
                                                        ! EMPTY ASF RECORDS-V12
	INTEGER*4 DATE_FORMAT
C
	CHARACTER*20	FILNAM,
     *			X2FILNAM			! FILE NAME FUNCTION.
C
	CHARACTER	ASFXADR*(LXADR),		! ASF TERMINAL ADDRESS.
     *			NULL_X2XADR*(LXADR)		! ALL 0'S.
C
	CHARACTER	ASFGVTID*(LGSER),
     *			GVTID(X2X_TERMS)*(LGSER),	! GVT ID TABLE.
     *			NULL_GVTID*(LGSER)		! ALL 0'S.
C
	CHARACTER	ASFDROP*(LDROP),		! DROP NUMBER.
     *			DIALDROP*(LDROP)/'@ '/		! DIAL DROP.
C
	CHARACTER*1	BELL		/Z07/,
     *			X2XTER_BIGREC_C1(X2XTER_BIGREC_SIZ_C1)
C
	LOGICAL*4	FAST,
     *			NEW_STNBLK	/.TRUE./,
     *			NEW_TERBLK	/.TRUE./,
     *			PRTFLG		/.FALSE./,
     *			X2XSCL_REC_EXISTS(X2XC_CLASSES)
     *					/X2XC_CLASSES * .FALSE./
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C DATA EQUIVALENCES.
C
	EQUIVALENCE (ASFDROP,  ASFBYT(SDROP))
	EQUIVALENCE (ASFGVTID, ASFBYT(SGSER))
	EQUIVALENCE (ASFXADR,  ASFBYT(SXADR))
	EQUIVALENCE (FILNAM,   IFILNAM)
	EQUIVALENCE (X2XTER_BIGREC_I4(1), X2XTER_BIGREC_C1(1))
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C COMMON AREA DECLARATIONS.
C
	COMMON	/STN_INFO/
     *			LAST_STN,
     *			PORT_COUNT
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C SET THE ASF START DATE FORMAT	- V08
	DATE_FORMAT = SKIP_START_DATE				!V09

C INITIALIZE THE X2XADR() STATION TABLE.
C
	DO 10 I = 1, LXADR
	  NULL_X2XADR(I:I) = '0'
10	CONTINUE
C
	DO 20 STN = 1, X2X_STATIONS
	  X2XADR(STN) = NULL_X2XADR
20	CONTINUE
C
C INITIALIZE THE GVTID() TERMINAL TABLE.
C
	DO 30 I = 1, LGSER
	  NULL_GVTID(I:I) = '0'
30	CONTINUE
C
	DO 40 TER = 1, X2X_TERMS
	  GVTID(TER) = NULL_GVTID
40	CONTINUE
C
C V03 - INITIALIZE THE DUMMY STATION COUNTER FOR EACH CLASS
C
        DO 45 I = 1, X2XC_CLASSES
          X2XC_DUMMY_STN_COUNT(I) = 0
45      CONTINUE
C
C	CLEAR THE GTX DIAL STATION NEEDED FLAG				    !V06
C
	GTX_DIAL_NEEDED = .FALSE.					    !V06
C
C DISPLAY THE COPYRIGHT.
C
	CALL COPYRITE
C
	TYPE *,IAM()
	TYPE *,IAM(),'<<<<< X2BLDNET - V1.20 - Build X2X Files >>>>>'
	TYPE *,IAM()
C
C IF CDC DATE IS NOT SET, ASSUME THAT COMMONS ARE NOT INITIALIZED.
C
	IF (DAYCDC .LE. 0) THEN
	  TYPE *, IAM(), 'Commons have NOT been initialized ...'
	  TYPE *, IAM()
C
	  CALL INPYESNO('Do you wish to disable ALL terminals (y/n) ? ',
     *                  ANSWER)
C
	  IF (ANSWER .NE. 1) DISABLE_FLAG = .FALSE.
	ENDIF
C
C CLEAR VARIABLES.
C
	LAST_STN = 0
C
	CALL FASTSET(0, COMMTBL,      X2X_STATIONS*X2X_MAXTERMS*3)
	CALL FASTSET(0, PORT_COUNT,   X2X_STATIONS)
	CALL FASTSET(0, STN_COUNT,    X2XC_CLASSES)
	CALL FASTSET(0, TER_PER_PORT, X2X_STATIONS*X2X_MAXPORT)
	CALL FASTSET(0, TERCNT,       X2X_STATIONS)
	CALL FASTSET(0, X2XGRP,       X2X_STATIONS)
C
C READ STATION DEFINITION FILE.
C
	CALL READ_STNDEF
C
C CURRENTLY WE USE THE LAST STATION VARIABLE TO ASSIGN PVC PORT NUMBERS
C TO GTX DIAL STATIONS ...
C
CV10	LAST_STN = END_STN(CLASS_ASYNC)
	LAST_STN = START_STN(CLASS_GTX) - 1	!V10 - THIS IS INCREMENTED IN 
						!X2NET_UPDSTN SO SUBTRACT 1
	GTXSTN   = START_STN(CLASS_GTX)
	USATSTN  = START_STN(CLASS_USAT)
CV04	X28STN   = START_STN(CLASS_X28)
C
C CLEAR THE X2XTER, X2XSTN AND X2XSPC FILES. FILES WILL BE REBUILT DYNAMICALLY.
C
	FILNAM = X2FILNAM(XTER)
	TYPE *, IAM()
	CALL CRTFIL(IFILNAM, X2XTER_SECT*X2X_TERMS/2, ST)
	IF (ST .NE. 0) THEN
	  TYPE *, IAM(), '*** ERROR CREATING ', FILNAM, ', ST = ', ST
	  GOTO 10000
	ENDIF
C
	FILNAM = X2FILNAM(XSPC)
	TYPE *, IAM()
	CALL CRTFIL(IFILNAM, X2XSPC_SECT*X2X_STATIONS*X2X_MAXPORT/2, ST)
	IF (ST .NE. 0) THEN
	  TYPE *, IAM(), '*** ERROR CREATING ', FILNAM, ', ST = ', ST
	  GOTO 10000
	ENDIF
C
	FILNAM = X2FILNAM(XSTN)
	TYPE *, IAM()
	CALL CRTFIL(IFILNAM, X2XSTN_SECT*X2X_STATIONS/2, ST)
	IF (ST .NE. 0) THEN
	  TYPE *, IAM(), '*** ERROR CREATING ', FILNAM, ', ST = ', ST
	  GOTO 10000
	ENDIF
C
C OPEN THE STATION FILE (USE BIG IOS IF POSSIBLE).
C
	CALL OPENX(XSTN, X2FILNAM(XSTN), 4, 0, 0, ST)
	IF (ST .NE. 0) THEN
	  TYPE *, IAM()
	  CALL OS32ER(6, X2FILNAM(XSTN), 'OPENX', ST, 0)
	  GOTO 10000
	ENDIF
C
	IF (X2XSTN_BIGREC_SIZ_C1 .GT.
     *      X2X_STATIONS*X2XSTN_SECT*CON_SECBYT) THEN
	  X2XSTN_RECSIZ      = X2XSTN_SECT * CON_SECBYT
	  X2XSTN_RECS_PER_IO = 1
	  X2XSTN_BIG_IOS     = 0
	  X2XSTN_SML_IOS     = X2X_STATIONS
	ELSE
	  X2XSTN_RECSIZ      = X2XSTN_BIGREC_SIZ_C1
	  X2XSTN_RECS_PER_IO = X2XSTN_RECSIZ / (X2XSTN_SECT*CON_SECBYT)
	  X2XSTN_BIG_IOS     = X2X_STATIONS / X2XSTN_RECS_PER_IO
	  X2XSTN_SML_IOS     = MOD(X2X_STATIONS, X2XSTN_RECS_PER_IO)
	ENDIF
C
	CALL IOINIT(X2XSTN_FDB, XSTN, X2XSTN_RECSIZ)
C
C OPEN THE TERMINAL FILE (USE BIG IOS IF POSSIBLE).
C
	CALL OPENX(XTER, X2FILNAM(XTER), 4, 0, 0, ST)
	IF (ST .NE. 0) THEN
	  TYPE *, IAM()
	  CALL OS32ER(6, X2FILNAM(XTER), 'OPENX', ST, 0)
	  GOTO 10000
	ENDIF
C
	IF (X2XTER_BIGREC_SIZ_C1 .GT.
     *      X2X_TERMS*X2XTER_SECT*CON_SECBYT) THEN
	  X2XTER_RECSIZ      = X2XTER_SECT * CON_SECBYT
	  X2XTER_RECS_PER_IO = 1
	  X2XTER_BIG_IOS     = 0
	  X2XTER_SML_IOS     = X2X_TERMS
	ELSE
	  X2XTER_RECSIZ      = X2XTER_BIGREC_SIZ_C1
	  X2XTER_RECS_PER_IO = X2XTER_RECSIZ / (X2XTER_SECT*CON_SECBYT)
	  X2XTER_BIG_IOS     = X2X_TERMS / X2XTER_RECS_PER_IO
	  X2XTER_SML_IOS     = MOD(X2X_TERMS, X2XTER_RECS_PER_IO)
	ENDIF
C
	CALL IOINIT(X2XTER_FDB, XTER, X2XTER_RECSIZ)
C
C OPEN THE STATION PORT CONFIGURATION FILE.
C
	CALL OPENX2X(X2FILNAM(XSPC), X2XSPC_LUN)
C
C OPEN THE STATION CLASS FILE.
C
	CALL OPENX(XSCL, X2FILNAM(XSCL), 4, 0, 0, ST)
	IF (ST .NE. 0) THEN
	  TYPE *, IAM()
	  CALL OS32ER(6, X2FILNAM(XSCL), 'OPENX', ST, 0)
	  GOTO 10000
	ENDIF
	CALL IOINIT(X2XSCL_FDB, XSCL, X2XSCL_SECT*CON_SECBYT)
C
C OPEN THE ASF FILE (SAME AS OPENASF BUT DOES NOT RELY ON SCF MEMORY).
C
	CALL OPENX(ASF, X2FILNAM(ASF), 4, 0, 0, ST)
	IF (ST .NE. 0) THEN
	  TYPE *, IAM()
	  CALL OS32ER(6, X2FILNAM(ASF), 'OPENX', ST, 0)
	  GOTO 10000
	ENDIF
	CALL IOINIT(FDB, ASF, ASFSEC*RECSPERBKT*CON_SECBYT)
C
	BKTNUM = -1					! FORCE A READ.
	BKTCHG =  0					! NOTHING HAS CHANGED.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C SECTION 1
C THIS SECTION WILL BUILD STATIONS WHICH HAVE BEEN DEFINED IN THE ASF.
C
C THE FOLLOWING INFO IS REQUIRED IN THE ASF FOR A STATION TO BE BUILT:
C
C ALL CLASSES REQUIRE:
C   AGENT NUMBER MUST BE PRESENT.
C   STATION CLASS MUST BE PRESENT.
C   VALID OR NO START CDC DATE.
C
C   X.21 REQUIRES A STATION NUMBER, DROP ADDRESS, GROUP NUMBER AND X2X ADDRESS.
C   AT LEAST ONE TERMINAL MUST BE ASSIGNED TO A STATION FOR
C   THAT STATION TO BE CREATED. ALSO, EDIT CHECKS WILL ENSURE THAT
C   ALL TERMINALS ON THE STATION HAVE THE SAME X2X ADDRESS & GROUP NUMBER
C   AND THAT NO TWO STATIONS HAVE THE SAME X2X ADDRESS. PORT IS ALWAYS SET TO 1.
C
C   X.25 REQUIRES A STATION NUMBER, X2X ADDRESS AND OPTIONAL GROUP NUMBER.
C   ONLY ONE TERMINAL CAN BE ASSIGNED TO A STATION FOR THAT STATION TO BE
C   CREATED. ALSO, EDIT CHECKS WILL ENSURE THAT NO TWO STATIONS HAVE THE SAME
C   X2X ADDRESS. DROP IS ALWAYS SET TO @ AND PORT IS ALWAYS SET TO 1.
C           
C   ASYNC/ISOCH REQUIRE A STATION NUMBER AND DROP ADDRESS.
C   AT LEAST ONE TERMINAL MUST BE ASSIGNED TO A STATION FOR
C   THAT STATION TO BE CREATED.
C
C   GTECH DIAL, IF PRESENT IN THE ASF, SHOULD BE PROVIDED
C   WITH A GVT SERIAL NUMBER. THE STATION NUMBER MUST BE PRESENT.
C   DROP IS ALWAYS SET TO @. AT LEAST ONE DIAL STATION MUST BE
C   DEFINED IN THE ASF IN ORDER FOR GTX DIAL STATIONS TO BE GENERATED.
C 
C   X28PAD, IF PRESENT IN THE ASF, SHOULD BE PROVIDED WITH A
C   GVT SERIAL NUMBER UNLESS THE ASF RECORD IS FOR A DUMMY GVT STATION.
C   THE DROP NEED NOT BE PRESENT.  AS OF 14-SEP-94, THE STATION NUMBER 
C   MUST BE PRESENT AND IS NO LONGER ASSIGNED DYNAMICALLY BASED ON THE 
C   STARTING STATION NUMBER IN THE STNDEF FILE, DROP IS ALWAYS SET TO @.
C
C   X25REMOTE, REQUIRES THAT A STATION, PORT AND DROP BE DEFINED.
C   X25 ADRESSES COME FROM STNDEF.FIL.
C
C   USAT, REQUIRE A STATION NUMBER AND DROP ADDRESS.
C   AT LEAST ONE TERMINAL MUST BE ASSIGNED TO A STATION FOR THAT
C   STATION TO BE CREATED. IN ADDITION A USAT ID MUST BE PROVIDED.
C   THE USAT ID IS IN THE FOLLOWING FORMAT:
C
C     000100CA0000
C        ^   ^
C        |   |
C      LAPB PVC CIRCUIT NUMBER (HEX)
C
c     ONLY TWO LAPB PORTS CAN BE CONFIGURED FOR USAT PER GTX.
C     LAPB PORT NUMBERS TAKEN FROM STNDEF.FIL.
C     IT MAY BE POSSIBLE TO HAVE MULTI-PORTS ON A USAT STATION.
C     IF A STATION IS MULTI-PORT OR SINGLE PORT, THE STATION PORT
C     FIELD MUST BE FILLED IN ASF.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C SCAN THROUGH THE ASF BUILDING THE TERMINAL AND STATION TABLES.
C
	TYPE *, IAM()
	WRITE(6, 9000) IAM()
C
C ***** Start V02 changes *****
C
C GET THE CURRENT CDC
C
        CALL XDAT(SYSDATE)
        DATBUF2(VYEAR)=SYSDATE(1)
        DATBUF2(VMON)=SYSDATE(2)
        DATBUF2(VDAY)=SYSDATE(3)
        CALL BDATE(DATBUF2)
C
C ***** End V02 changes *****
C
        VALID_ASF_COUNT = 0                      !initialize counter for number
                                                 !of non-empty ASF records-V12
	DO 1200 TER = 1, X2X_TERMS
	  CALL READASF(TER, ASFREC, ST)
C
	  IF (MOD(TER, 1000) .EQ. 0)
     *      TYPE *, IAM(), TER, ' TERMINALS PROCESSED ...'
C
C TEST FOR VALID AGENT NUMBER.
C
	  AGENT = 0 
	  CALL ASCBIN(ASFINF, SAGNO, LAGNO, AGENT, ST)
	  IF (ST .NE. 0 .OR. AGENT .LE. 0) GOTO 1200
          VALID_ASF_COUNT = VALID_ASF_COUNT + 1  !increment counter - V12
C
C DJO - SKIP OVER LOGICAL RECORDS (CHAIN HEADS, DIV HEADS, FRANCHISE HEADS)
C
CV10      IF (ASFBYT(SCSTS).EQ.'C')  GOTO 1200 !DJO - SKIP CHAIN HEADS
CV10      IF (ASFBYT(SCSTS).EQ.'F')  GOTO 1200 !DJO - SKIP DIV HEADS
CV10      IF (ASFBYT(SCSTS).EQ.'D')  GOTO 1200 !DJO - SKIP FRANCH HEADS
C
C ***** Start V09 changes *****
C
	IF (.NOT.(DATE_FORMAT.EQ.SKIP_START_DATE)) THEN
C
C ***** Start V02 changes *****
C
C EXTRACT THE START/END DATE TO VERIFY WHETHER THE TERMINAL
C SHOULD BE DISABLED OR ACTIVATED.
C
          CERR=0
	  MON=0
	  DAY=0
	  YEAR=0

C ***** Start V08 changes *****
	  IF (DATE_FORMAT .EQ. MONTH_DAY_YEAR) THEN    !FORMAT IS MONTH DAY YEAR
              CALL ASCBIN(ASFINF,SSDAT,2,MON,CERR)
              IF(MON.LT.1 .OR. MON.GT.12) CERR=-1
              IF(CERR.EQ.0) CALL ASCBIN(ASFINF,SSDAT+2,2,DAY,CERR)
              IF(DAY.LT.1 .OR. DAY.GT.31) CERR=-1
	  ELSEIF (DATE_FORMAT .EQ.DAY_MONTH_YEAR) THEN !FORMAT IS DAY MONTH YEAR
              IF(CERR.EQ.0) CALL ASCBIN(ASFINF,SSDAT,2,DAY,CERR)
              IF(DAY.LT.1 .OR. DAY.GT.31) CERR=-1
              CALL ASCBIN(ASFINF,SSDAT+2,2,MON,CERR)
              IF(MON.LT.1 .OR. MON.GT.12) CERR=-1
	  ELSE
	    TYPE *, IAM(), 'INVALID DATE FORMAT FLAG ', DATE_FORMAT
	  ENDIF
C ***** End V08 changes *****

          IF(CERR.EQ.0) CALL ASCBIN(ASFINF,SSDAT+4,2,YEAR,CERR)	!YEAR IS ALWAYS
          IF(CERR.NE.0) THEN					!LAST
C
C DON'T ADD TERMINAL IF BAD START DATE IN ASF
C
	    IF(MON+DAY+YEAR.NE.0)THEN
              WRITE(5,9130) IAM(), TER
              WRITE(5,9050) IAM(), TER
              ERRCNT=ERRCNT+1
              GOTO 1200
	    ENDIF
          ENDIF
C
C SET BEGCDC TO ZERO IF NONE ENTERED IN ASF
C
	  IF(CERR.NE.0) THEN
	    BEGCDC=0
C
C OTHERWISE GET THE REAL STARTING CDC IN THE ASF
C
	  ELSE
            DATBUF(VMON)=MON
            DATBUF(VDAY)=DAY
            DATBUF(VYEAR)=YEAR
            CALL BDATE(DATBUF)
            BEGCDC=DATBUF(VCDC)
	  ENDIF
C
C EXTRACT THE ENDING DATE.
C
          CERR=0

C ***** Start V08 changes *****
	  IF (DATE_FORMAT .EQ. MONTH_DAY_YEAR) THEN    !FORMAT IS MONTH DAY YEAR
             CALL ASCBIN(ASFINF,SEDAT,2,MON,CERR)
             IF(MON.LT.1 .OR. MON.GT.12) CERR=-1
             IF(CERR.EQ.0) CALL ASCBIN(ASFINF,SEDAT+2,2,DAY,CERR)
             IF(DAY.LT.1 .OR. DAY.GT.31) CERR=-1
	  ELSEIF (DATE_FORMAT .EQ.DAY_MONTH_YEAR) THEN !FORMAT IS DAY MONTH YEAR
             IF(CERR.EQ.0) CALL ASCBIN(ASFINF,SEDAT,2,DAY,CERR)
             IF(DAY.LT.1 .OR. DAY.GT.31) CERR=-1
             CALL ASCBIN(ASFINF,SEDAT+2,2,MON,CERR)
             IF(MON.LT.1 .OR. MON.GT.12) CERR=-1
	  ELSE
	    TYPE *, IAM(), 'INVALID DATE FORMAT FLAG ', DATE_FORMAT
	  ENDIF
C ***** End V08 changes *****

          IF(CERR.EQ.0) CALL ASCBIN(ASFINF,SEDAT+4,2,YEAR,CERR)
          IF(CERR.NE.0) THEN
            ENDCDC=99999999
          ELSE
            DATBUF(VMON)=MON
            DATBUF(VDAY)=DAY
            DATBUF(VYEAR)=YEAR
            CALL BDATE(DATBUF)
            ENDCDC=DATBUF(VCDC)
          ENDIF
C
C CHECK IF THE DATE IS VALID
C
          IF(.NOT.(BEGCDC.LE.DATBUF2(VCDC)+1 .AND.
     *       ENDCDC.GT.DATBUF2(VCDC)+1)) THEN
            WRITE(5,9140) IAM(), TER
            WRITE(5,9050) IAM(), TER
	    GOTO 1200
	  ENDIF
C
C ***** End V02 changes *****
C
	ENDIF
C
C ***** End V09 changes *****
C
C IS STATION CLASS WITHIN VALID RANGE.
C
	  ACLASS = 0
	  CALL ASCBIN(ASFINF, SSCLS, LSCLS, ACLASS, ST)
	  IF (ST .NE. 0 .OR.
     *        ACLASS .LT. 1 .OR.
     *        ACLASS .GT. X2XC_CLASSES) THEN
	    TYPE *, IAM(), 'TERMINAL NUMBER ', TER,
     *              ' HAS INVALID STN CLASS = ', ACLASS
	    GOTO 1200
C
C TEST FOR EQUIVALENT CLASS.
C
	  ELSEIF (ACLASS .GT. X2XC_PERM_CLASSES) THEN
	    DO 60 ICLASS = 1, X2XC_PERM_CLASSES
	      DO 50 J = 1, MAX_EQUIV
		IF (ACLASS .EQ. EQUIV_CLASS(ICLASS, J)) GOTO 70
50	      CONTINUE
60	    CONTINUE
	    TYPE *, IAM(), 'XXX TERMINAL NUMBER ', TER,
     *              ' HAS INVALID STN CLASS = ', ACLASS
	    DO 65 ICLASS = 1, X2XC_PERM_CLASSES
	      DO 62 J = 1, MAX_EQUIV
		TYPE *,ICLASS,J,EQUIV_CLASS(ICLASS, J)
62	      CONTINUE
65	    CONTINUE

	    GOTO 1200
C
70	    CONTINUE
	    STNCLS = ICLASS  
C
C CLASS IS A STANDARD CLASS.
C
	  ELSE
	    STNCLS = ACLASS
	  ENDIF
C
C RETRIEVE STATION NUMBER FROM ASF.
C
	  STN = 0
	  CALL ASCBIN(ASFINF, SXSTN, LXSTN, STN, ST)
	  IF (ST .NE. 0 .OR. STN .LE. 0) THEN
CV04	    IF (STNCLS .NE. CLASS_X28) THEN
	      TYPE *, IAM(), 'NO STN DEFINED IN ASF FOR TERMINAL ', TER
	      GOTO 1200
CV04	    ENDIF
	  ENDIF
C
	  IF ( (STN .GT. X2X_STATIONS) .AND.			! V03
     *         (STN .NE. DUMMY_STN_NUMBER) ) THEN               ! V03
	    TYPE *, IAM(), 'TERMINAL NUMBER ', TER,
     *              ' HAS STN # ', STN,
     *              ' WHICH EXCEEDS MAX STN VALUE ', X2X_STATIONS
	    GOTO 1200
	  ENDIF
C
C RETRIEVE PORT NUMBER FROM ASF.
C
	  TPORT = 0
	  CALL ASCBIN(ASFINF, SXPRT, LXPRT, TPORT, ST)
	  IF (ST .NE. 0 .OR. TPORT .LE. 0) THEN
	    IF (STNCLS .EQ. CLASS_X25_REMOTE) THEN 
	      TYPE *, IAM(), 'NO PORT DEFINED IN ASF FOR TERMINAL ', TER
	      GOTO 1200
	    ELSE
	      TPORT = 1
	    ENDIF
	  ENDIF
C
	  IF (TPORT .GT. X2X_MAXPORT) THEN
	    TYPE *, IAM(), 'TERMINAL NUMBER ', TER,
     *              ' HAS PORT NUMBER ', TPORT,
     *              ' WHICH EXCEEDS MAX PORT VALUE ', X2X_MAXPORT
	    GOTO 1200
	  ENDIF
C
C READ STATION CLASS RECORD TO SEE IF IT EXISTS.
C INITIALLY, THIS FLAG IS SET TO FALSE FOR ALL STATION CLASSES.
C IF THE RECORD IS SUCCESSFULLY READ, THEN THE FLAG IS SET TO TRUE.
C ONLY IF CLASS RECORD EXISTS DO WE ATTEMPT TO PROCESS AGENT/TERMINAL.
C
	  IF (.NOT. X2XSCL_REC_EXISTS(ACLASS)) THEN
	    CALL READW(X2XSCL_FDB, ACLASS, X2XSCL_REC, ST)
	    IF (ST .NE. 0) THEN
	      CALL OS32ER(6, X2FILNAM(XSCL), 'READW', ST, ACLASS)
	      CALL GPAUSE
	      GOTO 1200
	    ENDIF
	    X2XSCL_REC_EXISTS(ACLASS) = .TRUE.
	  ENDIF
C                                               
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C               X21  X25  ASY  ISO  UST  PVC  GVT  GTX  X28  REM
C
	  GOTO (100, 200, 300, 400, 500, 600, 700, 800, 900, 1000) STNCLS
C
	  TYPE *, IAM(), 'INVALID STN CLASS ', STNCLS,
     *                   ' ... NOT CONFIGURABLE'
	  GOTO 1200
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF X.21 NETWORK TYPE, USE STATION NUMBER
C FROM ASF RECORD (TEST VALID RANGE FOR CLASS).
C
100	  CONTINUE
	  C2DROP(TER) = ASFDROP
C
	  IF (STN .LT. START_STN(STNCLS) .OR.
     *        STN .GT. END_STN(STNCLS)) THEN
	    WRITE(6, 9010) IAM(), STN, START_STN(STNCLS),
     *                     END_STN(STNCLS), STNCLS
	    WRITE(6, 9050) IAM(), TER
	    GOTO 1200
	  ENDIF
C
C VERIFY MAX TERMINALS PER STATION NOT EXCEEDED.
C NOTE THAT TERMINAL COUNT IS INCREMENTED AFTER THIS TEST.
C (MAXTERMS PER STATION).
C
	  IF (TERCNT(STN) + 1 .GT. X2X_MAXTERMS) THEN
	    WRITE(6, 9060) IAM(), STN
	    WRITE(6, 9050) IAM(), TER
	    GOTO 1200
	  ENDIF
C
C*NEW* RETRIEVE & VERIFY THAT THE GROUP NUMBER IS VALID (REQUIRED FOR X.21).
C
 	  GROUP = 0
 	  CALL ASCBIN(ASFINF, SXGRP, LXGRP, GROUP, ST)
 	  IF (ST    .NE. 0 .OR.
     *        GROUP .LT. 1 .OR.
     *        GROUP .GT. X2X_NUM_GROUPS) THEN
 	    TYPE *, IAM(), 'TERMINAL NUMBER ', TER,
     *              ' HAS INVALID GROUP NUMBER = ', GROUP
 	    GOTO 1200
 	  ENDIF
C
 	  IF (X2XGRP(STN) .GT. 0) THEN
 	    IF (X2XGRP(STN) .NE. GROUP) THEN
 	      WRITE(6, 9020) IAM(), TER, GROUP, STN, X2XGRP(STN)
 	      WRITE(6, 9050) IAM(), TER
 	      GOTO 1200
 	    ENDIF
 	  ELSE
 	    X2XGRP(STN) = GROUP
 	  ENDIF
C
C VERIFY THAT THE TERMINAL ADDRESS & STATION ADDRESS ARE THE SAME.
C
          IF (ASFXADR .GT. NULL_X2XADR) THEN                    !V03
              X2XADR(STN) = ASFXADR                             !V03
	  ELSE
	    WRITE(6, 9040) IAM(), TER
	    WRITE(6, 9050) IAM(), TER
	    GOTO 1200
	  ENDIF
C
	  GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF X.25 NETWORK TYPE, USE STATION NUMBER
C FROM ASF RECORD (TEST VALID RANGE FOR CLASS).
C
200	  CONTINUE
	  C2DROP(TER) = DIALDROP
C
	  IF (STN .LT. START_STN(STNCLS) .OR.
     *        STN .GT. END_STN(STNCLS)) THEN
	    WRITE(6, 9010) IAM(), STN, START_STN(STNCLS),
     *                     END_STN(STNCLS), STNCLS
	    WRITE(6, 9050) IAM(), TER
	    GOTO 1200
	  ENDIF
C
C VERIFY MAX TERMINALS PER STATION NOT EXCEEDED.
C NOTE THAT TERMINAL COUNT IS INCREMENTED AFTER THIS TEST.
C (1 TERM PER STATION).
C
	  IF(TERCNT(STN) + 1 .GT. 1) THEN
	    WRITE(6, 9060) IAM(), STN
	    WRITE(6, 9050) IAM(), TER
	    GOTO 1200
	  ENDIF
C
C*NEW* VERIFY THAT THE GROUP NUMBER IS VALID (OPTIONAL FOR X.25).
C
 	  GROUP = 0
 	  CALL ASCBIN(ASFINF, SXGRP, LXGRP, GROUP, ST)
 	  IF (ST    .NE. 0 .OR.
     *        GROUP .LT. 0 .OR.
     *        GROUP .GT. X2X_NUM_GROUPS) THEN
 	    TYPE *, IAM(), 'TERMINAL NUMBER ', TER,
     *              ' HAS INVALID GROUP NUMBER = ', GROUP
 	    GOTO 1200
 	  ENDIF
C
 	  IF (X2XGRP(STN) .GT. 0) THEN
 	    IF (X2XGRP(STN) .NE. GROUP) THEN
 	      WRITE(6, 9020) IAM(), TER, GROUP, STN, X2XGRP(STN)
 	      WRITE(6, 9050) IAM(), TER
 	      GOTO 1200
 	    ENDIF
 	  ELSEIF (GROUP .GT. 0) THEN
 	    X2XGRP(STN) = GROUP
 	  ENDIF
C
C VERIFY THAT THE TERMINAL ADDRESS & STATION ADDRESS ARE THE SAME.
C
          IF (ASFXADR .GT. NULL_X2XADR) THEN                    !V03
              X2XADR(STN) = ASFXADR                             !V03
	  ELSE
	    WRITE(6, 9040) IAM(), TER
	    WRITE(6, 9050) IAM(), TER
	    GOTO 1200
	  ENDIF
C
	  GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF ASYNC/ISOCH NETWORK TYPE USE STATION NUMBER
C FROM ASF RECORD (TEST VALID RANGE FOR CLASS).
C
300	  CONTINUE
400	  CONTINUE
	  C2DROP(TER) = ASFDROP
C
	  IF (STN .LT. START_STN(STNCLS) .OR.
     *        STN .GT. END_STN(STNCLS)) THEN
	    WRITE(6, 9010) IAM(), STN, START_STN(STNCLS),
     *                     END_STN(STNCLS), STNCLS
	    WRITE(6, 9050) IAM(), TER
	    GOTO 1200
	  ENDIF
C
C VERIFY MAX TERMINALS PER STATION NOT EXCEEDED.
C NOTE THAT TERMINAL COUNT IS INCREMENTED AFTER THIS TEST.
C (MAXTERMS PER STATION).
C
	  IF (TERCNT(STN) + 1 .GT. X2X_MAXTERMS) THEN
	    WRITE(6, 9060) IAM(), STN
	    WRITE(6, 9050) IAM(), TER
	    GOTO 1200
	  ENDIF
C
	  GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF GSAT/USAT NETWORK TYPE USE STATION NUMBER
C FROM ASF RECORD (TEST VALID RANGE FOR CLASS)
C
500	  CONTINUE
	  C2DROP(TER) = ASFDROP
	  GVTID(TER)  = ASFGVTID

D	  TYPE *,'TER, GVTID ',TER,GVTID(TER)

	  LAST_STN    = END_STN(CLASS_USAT)
C
	  IF (STN .LT. START_STN(STNCLS) .OR.
     *        STN .GT. END_STN(STNCLS)) THEN
	    WRITE(6, 9010) IAM(), STN, START_STN(STNCLS),
     *                     END_STN(STNCLS), STNCLS
	    WRITE(6, 9050) IAM(), TER
	    GOTO 1200
	  ENDIF
C
C VERIFY MAX TERMINALS PER PORT NOT EXCEEDED.
C NOTE THAT TERMINAL COUNT IS INCREMENTED AFTER THIS TEST.
C (MAXTERMS PER PORT).
C
	  IF (TER_PER_PORT(STN, TPORT) + 1 .GT. X2X_MAXTERMS) THEN
	    WRITE(6, 9060) IAM(), STN
	    WRITE(6, 9050) IAM(), TER
	    GOTO 1200
	  ENDIF
C
	  GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C X.25 PVC NOT CURRENTLY SUPPORTED.
C 
600	  CONTINUE
	  GOTO 1200
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C GTECH_DIAL (GVT) STATIONS ARE NO LONGER ASSIGNED DYNAMICALLY
C 
C
700	  CONTINUE
	  C2DROP(TER) = DIALDROP
	  GVTID(TER)  = ASFGVTID
	  GVTSTN      = STN
C
	  IF (STN .LT. START_STN(STNCLS) .OR.
     *        STN .GT. END_STN(STNCLS)) THEN
	    WRITE(6, 9010) IAM(), STN, START_STN(STNCLS),
     *                     END_STN(STNCLS), STNCLS
	    WRITE(6, 9050) IAM(), TER
	    GOTO 1200
	  ENDIF
C
C VERIFY MAX TERMINALS PER STATION NOT EXCEEDED.
C NOTE THAT TERMINAL COUNT IS INCREMENTED AFTER THIS TEST.
C (1 TERM PER STATION).
C
	  IF(TERCNT(STN) + 1 .GT. 1) THEN
	    WRITE(6, 9060) IAM(), STN
	    WRITE(6, 9050) IAM(), TER
	    GOTO 1200
	  ENDIF
C
	  GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PSEUDO (GTX) WILL BE BUILT LATER (SECTION 2).
C
800	  CONTINUE
	  TYPE *, IAM(), 'STATION CLASS 8 SHOULD NOT BE ASSIGNED'
	  TYPE *, IAM(), 'IN THE ASF FOR TERMINAL # ', TER
	  WRITE(6, 9050) IAM(), TER 
	  GOTO 1200
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C X.28PAD (GVT) STATIONS WILL NO LONGER BE ASSIGNED DYNAMICALLY		!V04
C AS OF 9/14/94 WE WILL NOW USE THE STATION NUMBER FROM THE ASF RECORD	!V04
C (TEST VALID RANGE FOR CLASS)						!V04
C
900	  CONTINUE
	  C2DROP(TER) = DIALDROP
	  GVTID(TER)  = ASFGVTID

C         START OF V03 CHANGE BLOCK

C         IF THIS ASF RECORD DEFINES A DUMMY STATION, THEN ASSIGN THE NEXT
C         NEXT AVAILABLE DUMMY STATION NUMBER FOR THE CLASS.  IF WE EXCEED THE
C         NUMBER OF DUMMY STATIONS ALLOWED FOR THIS CLASS, THEN SKIP THIS
C         ASF RECORD.  IF THIS ASF RECORD DOES NOT DEFINE A DUMMY STATION,
C	 THEN JUST VERIFY THAT THE STATION NUMBER IN THE ASF IS IN RANGE.  !V04

	  IF (STN .EQ. DUMMY_STN_NUMBER) THEN			!V05
	    IF (X2XC_DUMMY_STN_COUNT(STNCLS) .EQ. 0) THEN	!V05
	        WRITE(6, 9015) IAM(), STNCLS			!V05
	        WRITE(6, 9050) IAM(), TER			!V05
		GOTO 1200					!V04
	    ELSEIF (DUMMY_STN_COUNTER(STNCLS) .LT. 		!V05
     *	        X2XC_DUMMY_STN_COUNT(STNCLS)) THEN
		STN = X2XC_DUMMY_START_STN(STNCLS) + DUMMY_STN_COUNTER(STNCLS)
		DUMMY_STN_COUNTER(STNCLS) = DUMMY_STN_COUNTER(STNCLS) + 1
            ELSE
		TYPE *, IAM(), 'TERMINAL NUMBER ', TER,
     *		' EXCEEDS MAX DUMMY STN COUNT FOR CLASS ', STNCLS
                GOTO 1200
            ENDIF
          ELSE
	     IF (STN .LT. START_STN(STNCLS) .OR.		!V04
     *           STN .GT. END_STN(STNCLS)) THEN			!V04
	         WRITE(6, 9010) IAM(), STN, START_STN(STNCLS),	!V04
     *                          END_STN(STNCLS), STNCLS		!V04
	         WRITE(6, 9050) IAM(), TER			!V04
	         GOTO 1200					!V04
	     ENDIF						!V04
          ENDIF

C         END OF V03 CHANGE BLOCK

CV04	  IF (STN .LE. 0) THEN
CV04	    TYPE *, IAM(), ' STN CLASS DEFINED IN ASF'
CV04	    TYPE *, IAM(), ' FOR TERMINAL # ',TER
CV04	    TYPE *, IAM(), ' NO RANGE DEFINED IN STNDEF.FIL'
CV04	    GOTO 1200
CV04	  ENDIF
C
C VERIFY MAX TERMINALS PER STATION NOT EXCEEDED.
C NOTE THAT TERMINAL COUNT IS INCREMENTED AFTER THIS TEST.
C (1 TERM PER STATION).
C
	  IF(TERCNT(STN) + 1 .GT. 1) THEN
	    WRITE(6, 9060) IAM(), STN
	    WRITE(6, 9050) IAM(), TER
	    GOTO 1200
	  ENDIF
C
	  GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF X25 REMOTE NETWORK TYPE, USE STATION NUMBER
C FROM ASF RECORD (TEST VALID RANGE FOR CLASS).
C
1000	  CONTINUE
	  C2DROP(TER) = ASFDROP
C
	  IF (STN .LT. START_STN(STNCLS) .OR.
     *        STN .GT. END_STN(STNCLS)) THEN
	    WRITE(6, 9010) IAM(), STN, START_STN(STNCLS),
     *                     END_STN(STNCLS), STNCLS
	    WRITE(6, 9050) IAM(), TER
	    GOTO 1200
	  ENDIF
C
C VERIFY MAX TERMINALS PER STATION NOT EXCEEDED.
C NOTE THAT TERMINAL COUNT IS INCREMENTED AFTER THIS TEST.
C (MAXTERM PER PORT).
C
	  IF (TER_PER_PORT(STN, TPORT) + 1 .GT. X2X_MAXTERMS) THEN
	    WRITE(6, 9060) IAM(), STN
	    WRITE(6, 9050) IAM(), TER
	    GOTO 1200
	  ENDIF
C
	  GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CREATE TABLES.
C
1100	  CONTINUE
	  IF (STN .LE. 0) GOTO 1200
C
	  TERCNT(STN)                          = TERCNT(STN) + 1
	  COMMTBL(AGT_CLASS, STN, TERCNT(STN)) = ACLASS
	  COMMTBL(STD_CLASS, STN, TERCNT(STN)) = STNCLS
	  COMMTBL(TERNUM,    STN, TERCNT(STN)) = TER
C
C PORT ASSIGNMENT FROM THE ASF IS ONLY USED FOR REMOTE AND USAT STATIONS.
C PORT NUMBER IN THE ASF FOR REMOTE STATIONS IS EQUIVALENCED TO LINE NUMBER.
C
	  IF ((TPORT  .GE. 1 .AND. TPORT .LE. X2X_MAXPORT) .AND.
     *        (STNCLS .EQ. CLASS_X25_REMOTE .OR.
     *         STNCLS .EQ. CLASS_USAT)) THEN
	    X2XTER_TBL(TER).T_PORT = TPORT
C
C WE WISH TO COUNT THE NUMBER OF PORTS ON A STATION
C SO ONLY DO THIS ONCE FOR FIRST ENCOUNTER IN ASF.FIL.
C
	    IF (TER_PER_PORT(STN, TPORT) .LE. 0)
     *        PORT_COUNT(STN) = PORT_COUNT(STN) + 1
	    TER_PER_PORT(STN, TPORT) = TER_PER_PORT(STN, TPORT) + 1
	  ELSE
	    X2XTER_TBL(TER).T_PORT = 1
	  ENDIF
C
1200	CONTINUE

C       Start of V12 Changes
        IF (VALID_ASF_COUNT .EQ. 0) THEN 
            WRITE(6, 9150) IAM()
	    CALL GSTOP(GEXIT_FATAL)
        ENDIF
C       End of V12 Changes
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C BEGIN STATION FILE UPDATE.
C
	TYPE *, IAM()
	WRITE(6, 9070) IAM()
C
C DO ANY BIG IOS FIRST.
C
	IF (X2XSTN_BIG_IOS .GT. 0) THEN
	  DO 1500 STNBLK = 1, X2XSTN_BIG_IOS
	    NEW_STNBLK = .TRUE.
	    X2XSTN_BEG = (STNBLK-1) * X2XSTN_RECS_PER_IO + 1
	    X2XSTN_END = X2XSTN_BEG + X2XSTN_RECS_PER_IO - 1
C
	    DO 1400 STN = X2XSTN_BEG, X2XSTN_END
	      IF (MOD(STN, 1000) .EQ. 0)
     *          TYPE *, IAM(), STN, ' STATIONS PROCESSED ...'
C
C VERIFY THAT ALL TERMINALS ON THIS STATION HAVE THE SAME COMM TYPE.
C
	      IF (TERCNT(STN) .GT. 0) THEN
		IF (NEW_STNBLK) THEN
		  NEW_STNBLK = .FALSE.
		  CALL FASTSET(0, X2XSTN_BIGREC_I4, X2XSTN_BIGREC_SIZ_I4)
		ENDIF
C
	        CALL FASTSET(0, X2XSTN_REC, X2XSTN_SECT*CON_SECBYT/4)
C
		FIRST_TYP = COMMTBL(AGT_CLASS, STN, 1)
		STNCLS    = COMMTBL(STD_CLASS, STN, 1)
C
		DO 1300 I = 2, TERCNT(STN)
		  IF (COMMTBL(AGT_CLASS, STN, I) .NE. FIRST_TYP)
     *              WRITE(6, 9080) IAM(), COMMTBL(TERNUM, STN, 1), 
     *                             COMMTBL(TERNUM, STN, I), IAM(), STN,
     *                             IAM(), FIRST_TYP, BELL
1300		CONTINUE
C
		TER = COMMTBL(TERNUM, STN, 1)
		CALL X2NET_UPDSTN(STN, FIRST_TYP, STNCLS,
     *                            X2XADR(STN), GVTID(TER))
CV02		X2XSTN_BITMAP = -1
		X2XSTN_GROUP  = X2XGRP(STN)
		X2XSTN_UPDATE = DAYCDC
C
		I4OFF = (STN - X2XSTN_BEG)
     *                * (X2XSTN_SECT * CON_SECBYT / 4) + 1
C
		CALL FASTMOV(X2XSTN_REC,
     *                       X2XSTN_BIGREC_I4(I4OFF),
     *                       X2XSTN_SECT*CON_SECBYT/4)
C
		STN_COUNT(COMMTBL(STD_CLASS, STN, 1)) = 
     *          STN_COUNT(COMMTBL(STD_CLASS, STN, 1)) + 1
C
C UPDATE THE TERMINAL TABLE.
C
		CALL UPD_X2XTER_TBL(STN)
	      ENDIF
1400	    CONTINUE
C
	    IF (.NOT. NEW_STNBLK) THEN
	      CALL WRITEW(X2XSTN_FDB, STNBLK, X2XSTN_BIGREC_I4, ST)
	      IF (ST .NE. 0) THEN
		CALL OS32ER(6, X2FILNAM(XSTN), 'BIG_WRITEW', ST, STNBLK)
		CALL GPAUSE
		GOTO 1500
	      ENDIF
	    ENDIF
1500	  CONTINUE
	ENDIF
C
C IF NECESSARY, CLOSE & RE-OPEN THE STATION FILE FOR REMAINING SMALL IOS.
C
	IF (X2XSTN_SML_IOS .GT. 0) THEN
	  IF (X2XSTN_BIG_IOS .GT. 0) THEN
	    CALL CLOSEFIL(X2XSTN_FDB)
	    CALL OPENX(XSTN, X2FILNAM(XSTN), 4, 0, 0, ST)
	    IF (ST .NE. 0) THEN
	      TYPE *, IAM()
	      CALL OS32ER(6, X2FILNAM(XSTN), 'OPENX', ST, 0)
	      GOTO 10000
	    ENDIF
	    CALL IOINIT(X2XSTN_FDB, XSTN, X2XSTN_SECT*CON_SECBYT)
	  ENDIF
C
C UPDATE & RE-WRITE THE REMAINING TERMINAL RECORDS.
C
	  DO 1700 STN = X2X_STATIONS-X2XSTN_SML_IOS+1, X2X_STATIONS
	    IF (MOD(STN, 1000) .EQ. 0)
     *        TYPE *, IAM(), STN, ' STATIONS PROCESSED ...'
C
C VERIFY THAT ALL TERMINALS ON THIS STATION HAVE THE SAME COMM TYPE.
C
	    IF (TERCNT(STN) .GT. 0) THEN
	      CALL FASTSET(0, X2XSTN_REC, X2XSTN_SECT*CON_SECBYT/4)
C
	      FIRST_TYP = COMMTBL(AGT_CLASS, STN, 1)
	      STNCLS    = COMMTBL(STD_CLASS, STN, 1)
C
	      DO 1600 I = 2, TERCNT(STN)
		IF (COMMTBL(AGT_CLASS, STN, I) .NE. FIRST_TYP)
     *            WRITE(6, 9080) IAM(), COMMTBL(TERNUM, STN, 1), 
     *                             COMMTBL(TERNUM, STN, I), IAM(), STN,
     *                             IAM(), FIRST_TYP, BELL
1600	      CONTINUE
C
	      TER = COMMTBL(TERNUM, STN, 1)
	      CALL X2NET_UPDSTN(STN, FIRST_TYP, STNCLS,
     *                          X2XADR(STN), GVTID(TER))
CV02	      X2XSTN_BITMAP = -1
	      X2XSTN_GROUP  = X2XGRP(STN)
	      X2XSTN_UPDATE = DAYCDC
C
	      CALL WRITEW(X2XSTN_FDB, STN, X2XSTN_REC, ST)
	      IF (ST .NE. 0) THEN
		CALL OS32ER(6, X2FILNAM(XSTN), 'WRITEW', ST, STN)
		CALL GPAUSE
		GOTO 1700
	      ENDIF
C
	      STN_COUNT(COMMTBL(STD_CLASS, STN, 1)) = 
     *        STN_COUNT(COMMTBL(STD_CLASS, STN, 1)) + 1
C
C UPDATE THE TERMINAL TABLE.
C
	      CALL UPD_X2XTER_TBL(STN)
	    ENDIF
1700	  CONTINUE
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C BEGIN TERMINAL FILE UPDATE.
C
	TYPE *, IAM()
	WRITE(6, 9090) IAM()
C
C DO ANY BIG IOS FIRST.
C
	IF (X2XTER_BIG_IOS .GT. 0) THEN
C
C GET THE OFFSETS OF THE TERMINAL INFORMATION WITHIN THE X2XTER_REC.
C
	  X2XTER_TER_OFF     = (%LOC(X2XTER_TER)
     *                       -  %LOC(X2XTER_REC(1))) / 4	! I*4 OFFSET.
C
	  X2XTER_STN_OFF     = (%LOC(X2XTER_STN)
     *                       -  %LOC(X2XTER_REC(1))) / 4	! I*4 OFFSET.
C
	  X2XTER_PORT_OFF    = (%LOC(X2XTER_PORT)
     *                       -  %LOC(X2XTER_REC(1))) / 4	! I*4 OFFSET.
C
	  X2XTER_DROP_OFF    = (%LOC(X2XTER_DROP)
     *                       -  %LOC(X2XTER_REC(1)))		! C*1 OFFSET.
C
	  X2XTER_STATE_OFF   = (%LOC(X2XTER_STATE)
     *                       -  %LOC(X2XTER_REC(1))) / 4	! I*4 OFFSET.
C
	  X2XTER_UPDATE_OFF  = (%LOC(X2XTER_UPDATE)
     *                       -  %LOC(X2XTER_REC(1))) / 4	! I*4 OFFSET.
C
	  X2XTER_BITMAP_OFF  = (%LOC(X2XTER_BITMAP)
     *                       -  %LOC(X2XTER_REC(1))) / 4	! I*4 OFFSET.
C
C LOOP FOR BIG IOS.
C
	  DO 1900 TERBLK = 1, X2XTER_BIG_IOS
	    NEW_TERBLK = .TRUE.
	    X2XTER_BEG = (TERBLK-1) * X2XTER_RECS_PER_IO + 1
	    X2XTER_END = X2XTER_BEG + X2XTER_RECS_PER_IO - 1
C
	    DO 1800 TER = X2XTER_BEG, X2XTER_END
	      IF (X2XTER_TBL(TER).T_STN .GT. 0) THEN
		IF (NEW_TERBLK) THEN
		  NEW_TERBLK = .FALSE.
		  CALL FASTSET(0, X2XTER_BIGREC_I4, X2XTER_BIGREC_SIZ_I4)
		ENDIF
C
		C1OFF = (TER - X2XTER_BEG) * X2XTER_SECT * CON_SECBYT
		I4OFF = C1OFF / 4 + 1
C
		X2XTER_BIGREC_I4(I4OFF + X2XTER_TER_OFF)      = TER
C
		X2XTER_BIGREC_I4(I4OFF + X2XTER_STN_OFF)      =
     *          X2XTER_TBL(TER).T_STN
C
		X2XTER_BIGREC_I4(I4OFF + X2XTER_PORT_OFF)     =
     *          X2XTER_TBL(TER).T_PORT
C
		X2XTER_BIGREC_C1(C1OFF + X2XTER_DROP_OFF + 1) =
     *          X2XTER_TBL(TER).T_DROP(1:1)
C
		X2XTER_BIGREC_C1(C1OFF + X2XTER_DROP_OFF + 2) =
     *          X2XTER_TBL(TER).T_DROP(2:2)
C
		X2XTER_BIGREC_I4(I4OFF + X2XTER_STATE_OFF)    =
     *          X2XTER_TBL(TER).T_STATE
C
		X2XTER_BIGREC_I4(I4OFF + X2XTER_UPDATE_OFF)   = DAYCDC
		X2XTER_BIGREC_I4(I4OFF + X2XTER_BITMAP_OFF)   = 0
	      ENDIF
1800	    CONTINUE
C
	    IF (.NOT. NEW_TERBLK) THEN
	      CALL WRITEW(X2XTER_FDB, TERBLK, X2XTER_BIGREC_I4, ST)
	      IF (ST .NE. 0) THEN
		CALL OS32ER(6, X2FILNAM(XTER), 'BIG_WRITEW', ST, TERBLK)
		CALL GPAUSE
		GOTO 1900
	      ENDIF
	    ENDIF
1900	  CONTINUE
	ENDIF
C
C IF NECESSARY, CLOSE & RE-OPEN THE TERMINAL FILE FOR REMAINING SMALL IOS.
C
	IF (X2XTER_SML_IOS .GT. 0) THEN
	  IF (X2XTER_BIG_IOS .GT. 0) THEN
	    CALL CLOSEFIL(X2XTER_FDB)
	    CALL OPENX(XTER, X2FILNAM(XTER), 4, 0, 0, ST)
	    IF (ST .NE. 0) THEN
	      TYPE *, IAM()
	      CALL OS32ER(6, X2FILNAM(XTER), 'OPENX', ST, 0)
	      GOTO 10000
	    ENDIF
	    CALL IOINIT(X2XTER_FDB, XTER, X2XTER_SECT*CON_SECBYT)
	  ENDIF
C
C UPDATE & RE-WRITE THE REMAINING TERMINAL RECORDS.
C
	  DO 2000 TER = X2X_TERMS-X2XTER_SML_IOS+1, X2X_TERMS	
	    IF (X2XTER_TBL(TER).T_STN .GT. 0) THEN
	      CALL FASTSET(0, X2XTER_REC, X2XTER_SECT*CON_SECBYT/4)
C
	      X2XTER_TER     = TER
	      X2XTER_STN     = X2XTER_TBL(TER).T_STN
	      X2XTER_PORT    = X2XTER_TBL(TER).T_PORT
	      X2XTER_DROP    = X2XTER_TBL(TER).T_DROP
	      X2XTER_STATE   = X2XTER_TBL(TER).T_STATE
	      X2XTER_UPDATE  = DAYCDC
	      X2XTER_BITMAP  = 0
C
	      CALL WRITEW(X2XTER_FDB, TER, X2XTER_REC, ST)
	      IF (ST .NE. 0) THEN
		CALL OS32ER(6, X2FILNAM(XTER), 'WRITEW', ST, TER)
		CALL GPAUSE
		GOTO 2000
	      ENDIF
	    ENDIF
2000	  CONTINUE
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C SECTION 2
C THIS SECTION WILL BUILD GTX (PSEUDO) STATIONS IF ANY HAVE
C BEEN DEFINED IN THE STATION RANGE FILE.
C
	STNCLS = CLASS_GTX
	IF (GVTSTN .LE. 0 .AND. USATSTN .LE. 0) GOTO 2200
	IF (.NOT.GTX_DIAL_NEEDED) GOTO 2200				    !V06
C
	IF (START_STN(STNCLS) .LE. 0 .AND.
     *        END_STN(STNCLS) .LE. 0) THEN
	  TYPE *, IAM(), 'GTECH DIAL OR USAT STATIONS HAVE BEEN'
	  TYPE *, IAM(), 'DEFINED IN THE ASF. THESE TYPES OF STATIONS'
	  TYPE *, IAM(), 'REQUIRE GTX DIAL STATIONS TO BE DEFINED.'
	  TYPE *, IAM(), 'NO RANGE GIVEN FOR THIS TYPE OF STATION.'
	  CALL GPAUSE
	  GOTO 2200
	ENDIF
C
C IF NECESSARY, CLOSE & RE-OPEN THE STATION FILE FOR SMALL IOS.
C
	IF (X2XSTN_SML_IOS .LE. 0) THEN
	  CALL CLOSEFIL(X2XSTN_FDB)
	  CALL OPENX(XSTN, X2FILNAM(XSTN), 4, 0, 0, ST)
	  IF (ST .NE. 0) THEN
	    TYPE *, IAM()
	    CALL OS32ER(6, X2FILNAM(XSTN), 'OPENX', ST, 0)
	    GOTO 10000
	  ENDIF
	  CALL IOINIT(X2XSTN_FDB, XSTN, X2XSTN_SECT*CON_SECBYT)
	ENDIF
C
C UPDATE THE STATION RECORD, THIS RECORD SHOULD BE CLEAN, SINCE THE
C STATION FILE HAS BEEN INITIALIZED AT THE START OF THIS PROCEDURE.
C
	DO 2100 STN = START_STN(STNCLS), END_STN(STNCLS)
	  CALL FASTSET(0, X2XSTN_REC, X2XSTN_SECT*CON_SECBYT/4)
C
C CREATE A STATION RECORD USING THE DEFAULT VALUES FROM THE
C STATION CLASS RECORD.
C
	  CALL X2NET_UPDSTN(STN, STNCLS, STNCLS,
     *                      '0000000000000000', '000000000000')
C
C REWRITE THE STATION RECORD.
C
	  CALL WRITEW(X2XSTN_FDB, STN, X2XSTN_REC, ST)
	  IF (ST .NE. 0) THEN
	    CALL OS32ER(6, X2FILNAM(XSTN), 'WRITEW', ST, STN)
	    CALL GPAUSE
	    GOTO 2100
	  ENDIF
C
	  STN_COUNT(CLASS_GTX) = STN_COUNT(CLASS_GTX) + 1
2100	CONTINUE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C UPDATE IS COMPLETE ...
C CLOSE THE FILES.
C PERFORM EDIT CHECKS OF X2X DATABASE IF REQUESTED.
C CLEAR RECORD UPDATE BITMAPS IF NO ERRORS.
C
2200	CONTINUE
	TYPE *, IAM()
	WRITE(6, 9100) IAM()
	CALL CLOSEFIL(X2XSTN_FDB)
	CALL CLOSEFIL(X2XTER_FDB)
	CALL CLOSX2X(X2XSPC_LUN)
	CALL CLOSASF
	CALL CLOSEFIL(X2XSCL_FDB)
C
	DO 2300 I = 1, X2XC_PERM_CLASSES
	  IF (STN_COUNT(I) .NE. 0)
     *      WRITE(6, 9110) IAM(), STN_COUNT(I), CLASS_NAMES(I), I
2300    CONTINUE
C
C ***** Start V02 changes *****
C
C Always do edit checks and clear the Bit maps
C
C PROMPT FOR EDIT CHECKS.
C
CVO2	TYPE *, IAM()
CV02	CALL INPYESNO('Do the EDIT CHECKS of the files (y/n) ? ',
CV02 *                ANSWER)
C
	ANSWER=1
C
C CHECK THE TERMINAL CONFIGURATION FILE.
C
	IF (ANSWER .EQ. 1) THEN
	  TYPE *, IAM()
	  FAST = .FALSE.
	  CALL X2CHKTER(PRTFLG, FAST, ERRCNT)
CV02      IF (ERRCNT .EQ. 0) THEN
            TYPE *, IAM()
CV02        CALL X2CLRMOD(XTER)                         ! CLEARS FIELD BITMAP.
            CALL X2CHKMOD(XTER, 0)                      ! CLEARS FILE BITMAP.
CV02      ELSE
CV02        CALL X2CHKMOD(XTER, 1)
CV02      ENDIF
C
C CHECK THE STATION PORT CONFIGURATION FILE.
C
	  TYPE *, IAM()
	  FAST = .FALSE.
	  CALL X2CHKSPC(PRTFLG, FAST, ERRCNT)
CV02      IF (ERRCNT .EQ. 0) THEN
            TYPE *, IAM()
CV02        CALL X2CLRMOD(XSPC)                         ! CLEARS FIELD BITMAP.
            CALL X2CHKMOD(XSPC, 0)                      ! CLEARS FILE BITMAP.
CV02      ELSE
CV02        CALL X2CHKMOD(XSPC, 1)
CV02      ENDIF
C
C CHECK THE STATION CONFIGURATION FILE.
C
	  TYPE *, IAM()
	  FAST = .FALSE.
	  CALL X2CHKSTN(PRTFLG, FAST, ERRCNT)
CV02      IF (ERRCNT .EQ. 0) THEN
            TYPE *, IAM()
CV02        CALL X2CLRMOD(XSTN)                         ! CLEARS FIELD BITMAP.
            CALL X2CHKMOD(XSTN, 0)                      ! CLEARS FILE BITMAP.
CV02      ELSE
CV13        CALL X2CHKMOD(XSTN, 1)
CV02      ENDIF
        ENDIF
C
C ***** End V02 changes *****
C
C
C ALL DONE.
C
	TYPE *, IAM()
	WRITE(6, 9120) IAM()
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FORMAT STATEMENTS.
C
9000	FORMAT(X, A, 'BUILDING TERMINAL LOOKUP ...')
C
9010	FORMAT(X, A, 'INVALID STN # ', I5,
     *         X,    'OUTSIDE RANGE ', I5, ':', I5,
     *         X,    'FOR STN CLASS ', I2)

9015	FORMAT(X, A, 'DUMMY STATION RANGE IS UNDEFINED FOR STN CLASS ',	!V05
     *	       X,     I2)						!V05
C
9020	FORMAT(X, A, 'TER ', I5, ' GROUP ', I5, ' DIFFERS FROM',
     *         X,    'STN ', I5, ' GROUP ', I5)
C
9030	FORMAT(X, A, 'TER ', I5, ' ADDR ', A, ' DIFFERS FROM',
     *         X,    'STN ', I5, ' ADDR ', A)
C
9040	FORMAT(X, A, 'TER ', I5, ' X2X ADDRESS IS MISSING')
C
9050	FORMAT(X, A, 'TERMINAL ', I5,
     *         X,    'WILL NOT BE ADDED TO X2X DATABASE')
C
9060	FORMAT(X, A, 'MAX NUMBER OF TERMINALS EXCEEDED FOR STN ', I5)
C
9070	FORMAT(X, A, 'BEGINNING X2X STN & STN-PORT FILE UPDATES ...')
C
9080	FORMAT(X, A, 'WARNING ... TERMINALS ', I5, ' AND ', I5, /,
     *         X, A, 'HAVE DIFFERENT STN CLASSES FOR STN ', I5, '.', /,
     *         X, A, 'USING STN CLASS ', I2, ' TO BUILD STN RECORD!', A)
C
9090	FORMAT(X, A, 'BEGINNING X2X TERMINAL FILE UPDATES ...')
C
9100	FORMAT(X, A, 'UPDATES COMPLETE ...')
C
9110	FORMAT(X, A, I5, ' STATIONS HAVE BEEN BUILT FOR ',
     *            A10, ' (', I2, ')')
C
9120	FORMAT(X, A, 'X2X FILE BUILD PROCEDURE COMPLETE.')
9130    FORMAT(1X,A,'Term # ',I7,' invalid start date in ASF ')
9140    FORMAT(1X,A,'Term # ',I7,' invalid date in ASF ')
9150	FORMAT(1X,A,'NO valid records in ASF')                     !V12
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PROGRAM EXIT.
C
10000	CONTINUE
C
	CALL GSTOP(GEXIT_SUCCESS)
	END
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C UPDATE THE TERMINAL TABLE +++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	SUBROUTINE UPD_X2XTER_TBL(STN)
C
	IMPLICIT NONE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C INCLUDE FILES.
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XSPC.DEF'
C
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:X2BLDNET.DEF'
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C LOCAL DECLARATIONS
C
	INTEGER*4	DROP,
     *			EXSREC,
     *			SREC,
     *			ST,
     *			STN,
     *			TER,
     *			TERM
C
	CHARACTER*20	X2FILNAM			! FILE NAME FUNCTION.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C LOOP FOR TERMINAL TABLE UPDATES.
C
	DO 300 TERM = 1, TERCNT(STN)
	  TER = COMMTBL(TERNUM, STN, TERM)
C
C A TERMINAL WILL BE DISABLED ONLY IF REQUESTED.
C                        
	  IF (DISABLE_FLAG) THEN
	    X2XTER_TBL(TER).T_STATE = X2XTS_DISABLED
	  ELSE
	    X2XTER_TBL(TER).T_STATE = X2XTS_DEFINED
	  ENDIF
C
C UPDATE THE TERMINAL DATA.
C
          CALL X2CNVDRP(C2DROP(TER), DROP)   !.....v11
	  IF (DROP .GE. 1 .AND.
     *        DROP .LE. X2X_MAXTERMS) THEN
	    X2XTER_TBL(TER).T_DROP = C2DROP(TER)
	  ELSE
	    X2XTER_TBL(TER).T_DROP = '  '
	  ENDIF
C
	  X2XTER_TBL(TER).T_STN = STN
C
C STATION PORT FILE UPDATE.
C
	  CALL X2FNDSPC(STN, X2XTER_TBL(TER).T_PORT, ST, SREC, EXSREC)
	  IF (ST .NE. -1) THEN
	    CALL FASTSET(0, X2XSPC_REC, X2XSPC_SECT*CON_SECBYT/4)
C
	    X2XSPC_PORT = X2XTER_TBL(TER).T_PORT
	    X2XSPC_STN  = STN
C
	    IF (COMMTBL(STD_CLASS, STN, TERM) .EQ. CLASS_X25_REMOTE .OR.
     *          COMMTBL(STD_CLASS, STN, TERM) .EQ. CLASS_USAT) THEN
	      X2XSPC_TERCNT = TER_PER_PORT(X2XSPC_STN, X2XSPC_PORT)
	    ELSE 
	      X2XSPC_TERCNT = TERCNT(STN)
	    ENDIF
C
CV02	    X2XSPC_BITMAP = -1
C
	  ELSE
CV02	    DO 100 J = 1, X2XSPC_ENTRIES
CV02	      CALL BSET(X2XSPC_BITMAP, J)
100	    CONTINUE
	    SREC = EXSREC
	  ENDIF
C
C SET DROP ADDRESS FOR X2XSPC FILE , BLANK OUT ANY NULLS.
C
	  IF (DROP .GE. 1 .AND.
     *        DROP .LE. X2X_MAXTERMS) X2XSPC_DROPS(DROP) = C2DROP(TER)
C
	  DO 200 DROP = 1, X2X_MAXTERMS
	    IF (X2XSPC_DROPS(DROP) .LE. '  ') X2XSPC_DROPS(DROP) = '  '
200	  CONTINUE
C
	  X2XSPC_UPDATE = DAYCDC
C
	  CALL WRITX2X(X2XSPC_LUN, SREC, X2XSPC_REC, ST)
	  IF (ST .NE. 0) THEN
	    CALL OS32ER(6, X2FILNAM(XSPC), 'WRITX2X', ST, SREC)
	    CALL GPAUSE
	  ENDIF
300	CONTINUE
C
	RETURN
	END
