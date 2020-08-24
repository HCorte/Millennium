C
C SUBROUTINE X2NET_UPDSTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2NET_UPDSTN.FOV                             $
C  $Date::   17 Apr 1996 16:25:22                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C *** Pre-Baseline Source - x2net_updstn.for ***
C
C
C X2X Upgrade: 22-FEB-96 wsm Added AGTINF.DEF for Finland.
C
C V05 19-JUN-95 SCD Fix comment.  X2BLDNET has been changed to use range of 
C		    stations in STNDEF.FIL for GTX station class
C		    instead of setting LAST_STN to the last ASYNCH station.
C V04 22-SEP-94 GPR Set the GTX Dial needed flag - Integrate UK changes 
C		    into X2X Baseline
C V03 09-SEP-94 GPR LOAD STATION TYPE FOR BROADCAST SERVERS - Integrate 
C		    UK changes into X2X Baseline
C V02 05-AUG-94 SCD USE VARIABLE FORMAT STATEMENTS AND INCREASE MAXIMUM
C		    ADDRESS LENGTH FROM 9 TO 12 FOR U.K.
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
C	This program is part of the x2bldnet process. It is responsible for
C	creating the station record based on various station class requirements.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	SUBROUTINE X2NET_UPDSTN(STN, CLASS, CLASS_TYPE, STNADR, GVTID)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
	INCLUDE 'INCLIB:X2XSCL.DEF'
C
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:X2BLDNET.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'				!V03
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C LOCAL DECLARATIONS.
C
	INTEGER*4	CLASS,
     *			CLASS_TYPE,
     *			ERR,
     *			GVTVAL,
     *			LAPB_INDEX,
     *			LASTTYP		/0/,
     *			LAST_STN,
     *			PORT_COUNT(X2X_STATIONS),
     *			ST,
     *			STN
C
	INTEGER*4	LEN,		!V02 - # OF DIGITS IN ADDRESS LENGTH
     *			PAD		!V02 - # OF LEADING 0s IN ADDRESS

CV03	INTEGER*4	MAX_ADDR_CHAR		!V02 - MAX ADDRESS LENGTH
CV03	PARAMETER 	(MAX_ADDR_CHAR = 12)	!V02
C
	CHARACTER*20	X2FILNAM		! FILE NAME FUNCTION.
C
	CHARACTER	STNADD*(LXADR),
     *			STNADR*(LXADR)
C
	CHARACTER	GVTID*(LGSER),
     *			TMPGVT*(LGSER)
C
	CHARACTER*1	CHRSTR(LXADR)
C
	LOGICAL*4	ADDRESS_ASSIGNED
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C DATA EQUIVALENCES.
C
	EQUIVALENCE	(STNADD, CHRSTR)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C COMMON AREA DECLARATIONS.
C
	COMMON	/STN_INFO/
     *			LAST_STN,
     *			PORT_COUNT
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C SAVED VARIABLES.
C
	SAVE	LASTTYP
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	ADDRESS_ASSIGNED = .FALSE.
C
C READ THE STATION CLASS FILE.
C IF THE LAST ONE READ IS THE SAME THEN THERE IS NO REASON TO READ IT AGAIN.
C
	IF (CLASS .NE. LASTTYP) THEN
	  CALL READW(X2XSCL_FDB, CLASS, X2XSCL_REC, ST)
	  IF (ST .NE. 0) THEN
	    CALL OS32ER(6, X2FILNAM(XSCL), 'READW', ST, CLASS)
	    CALL GPAUSE
	    GOTO 10000
	  ENDIF
	  LASTTYP = CLASS
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ASSIGN STATION FIELD VALUES TAKEN DIRECTLY FROM THE STATION CLASS RECORD.
C
	X2XSTN_ADDLEN     = X2XSCL_ADDLEN		!+
	X2XSTN_AUTOUPD    = X2XSCL_AUTOUPD		!+-
CV03	X2XSTN_BAUD       = X2XSCL_BAUD
CV03	X2XSTN_CLOCK      = X2XSCL_CLOCK
	X2XSTN_DELACK     = 1				!!!!X2XSCL_DELACK
CV03	X2XSTN_DIALENA    = X2XSCL_DIALENA
CV03	X2XSTN_DIAL_PORT1 = X2XSCL_DIAL_PORT1
CV03	X2XSTN_DIAL_PORT2 = X2XSCL_DIAL_PORT2
	X2XSTN_ERRREP     = 1				!!!!X2XSCL_ERRREP
	X2XSTN_EVSN_LEN   = X2XSCL_EVSN_LEN		!+
	X2XSTN_FEDIS      = 0				!!!!X2XSCL_FEDIS
CV03	X2XSTN_NETPT1     = X2XSCL_NETPT1
CV03	X2XSTN_NETPT2     = X2XSCL_NETPT2
CV03	X2XSTN_NETPT3     = X2XSCL_NETPT3
CV03	X2XSTN_NETPT4     = X2XSCL_NETPT4
CV03	X2XSTN_NETPT5     = X2XSCL_NETPT5
CV03	X2XSTN_NETPT6     = X2XSCL_NETPT6
CV03	X2XSTN_NETPT7     = X2XSCL_NETPT7
	X2XSTN_NETSTAT    = X2XSCL_NETSTAT		!+-
CV03	X2XSTN_POLTIM     = X2XSCL_POLTIM
CV03	X2XSTN_PROTO      = X2XSCL_PROTO
CV03	X2XSTN_PRTCNT     = X2XSCL_PRTCNT
	X2XSTN_STNCLS     = X2XSCL_CLASS		!+ 
CV03	X2XSTN_STNDIS     = X2XSCL_STNDIS
CV03	X2XSTN_SYNC       = X2XSCL_SYNC
CV03	X2XSTN_TTN_PORT1  = X2XSCL_TTN_PORT1
CV03	X2XSTN_TTN_PORT2  = X2XSCL_TTN_PORT2
	X2XSTN_TYPE       = X2XSCL_TYPE			!+
C
C ASSIGN DEFAULT VALUES FOR FIELDS NOT FOUND IN CLASS RECORD.
C
	X2XSTN_ADDRES(1) = 0
	X2XSTN_ADDRES(2) = 0
	X2XSTN_DEF_PORT1 = 0
	X2XSTN_DEF_PORT2 = 0
	X2XSTN_DEF_PORT3 = 0
	X2XSTN_DEF_PORT4 = 0
	X2XSTN_EVSN(1)   = 0
	X2XSTN_EVSN(2)   = 0
	X2XSTN_GROUP     = 0
	X2XSTN_POLL      = 0
	X2XSTN_PRTFLG    = X2X_NONE_PRINT
	X2XSTN_PVC       = 0
CV03	X2XSTN_REPCLS    = 1
	X2XSTN_SERIAL    = STN
	X2XSTN_STATE     = X2XS_IDLE
	X2XSTN_STN       = STN
	X2XSTN_VSP       = 0
C
C ***** Start V03 changes *****
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C ASSIGN STATION TYPE 
C
	IF(IS_BROADCAST_SERVER(STN)) THEN
	  X2XSTN_STATION_TYPE=X2XST_BCST
	ELSE
	  X2XSTN_STATION_TYPE=0
	ENDIF
C
C ***** End V03 changes *****
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C DETERMINE VALUES WHICH VARY ACCORDING TO CLASS.
C
	GOTO (100, 200, 300, 400, 500,
     *        600, 700, 800, 900, 1000) CLASS_TYPE
C
	TYPE *, IAM(), 'INVL STN CLASS = ', CLASS_TYPE,
     *                 ' FOR STN = ', STN, ' ... NOT CONFIGURABLE'
C
	X2XSTN_PVCPORT = 0
C
	GOTO 10000
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C X.21 STATION CLASS.
C
100     CONTINUE
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C X.25 STATION CLASS.
C         
200     CONTINUE
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ASYNC / ISOCH STATION CLASSES.
C
300     CONTINUE
400     CONTINUE
	X2XSTN_PVCPORT   = STN
	ADDRESS_ASSIGNED = .TRUE.
C
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C GSAT/USAT STATION CLASS.
C
500     CONTINUE
	CALL LIB$MOVC3(X2XSTN_EVSN_LEN, %REF(GVTID), %REF(TMPGVT))
C
	CALL OTS$CVT_TZ_L(TMPGVT(5:8), GVTVAL, %VAL(4))
	CALL OTS$CVT_TI_L(TMPGVT(1:4), LAPB_INDEX, %VAL(4))
C
	IF (LAPB_INDEX .LE. 0 .OR.
     *      LAPB_INDEX .GT. X2X_MAXPVC_LINES) THEN
	  TYPE *, IAM(), 'INVALID SATELITE ID FOR STATION (LAPB) ', STN
	  GOTO 1100
	ENDIF
C
C IF A GSAT STATION HAS MULTIPLE PORTS, THEY MUST BE ENTERED IN THE ASF,
C DEFAULT TO 1 IF NOT PRESENT.
C
CV03	IF (PORT_COUNT(STN) .GT. 1) X2XSTN_PRTCNT = PORT_COUNT(STN)
C
	X2XSTN_PVCPORT = LAPB_PORTS(LAPB_INDEX)
	X2XSTN_PVC     = GVTVAL
C
	CALL ATOH(GVTID, 1, X2XSTN_EVSN_LEN, X2XSTN_EVSN, ERR)
C
	IF (X2XSCL_DIAL_TYPE.EQ.X2XC_GTX_DIAL) THEN			!V04
	   GTX_DIAL_NEEDED = .TRUE.					!V04
	ENDIF								!V04
C
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C X.25 PVC STATION CLASS.
C
600     CONTINUE
	GOTO 10000
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C GTECH_DIAL (GVT) STATION CLASS.
C
700     CONTINUE
	X2XSTN_PVCPORT   = 0
	ADDRESS_ASSIGNED = .TRUE.
C
	CALL ATOH(GVTID, 1, X2XSTN_EVSN_LEN, X2XSTN_EVSN, ERR)
C
	IF (X2XSCL_DIAL_TYPE.EQ.X2XC_GTX_DIAL) THEN			!V04
	   GTX_DIAL_NEEDED = .TRUE.					!V04
	ENDIF								!V04
C
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PSEUDO (GTX) STATION CLASS.
C
800     CONTINUE
	LAST_STN         = LAST_STN + 1		! V05 - VALUE NOW SET BASED
						! UPON STNDEF.FIL, NOT
						! END_STATION (CLASS_ASYNC).
	X2XSTN_PVCPORT   = LAST_STN
	ADDRESS_ASSIGNED = .TRUE.
C
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C X.28 PAD (GVT) STATION CLASS.
C
900     CONTINUE
	X2XSTN_PVCPORT   = 0
	ADDRESS_ASSIGNED = .TRUE.
C
	CALL ATOH(GVTID, 1, X2XSTN_EVSN_LEN, X2XSTN_EVSN, ERR)
C
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C X25 REMOTE STATION CLASS.
C
1000    CONTINUE
CV03	X2XSTN_PRTCNT = PORT_COUNT(STN)
C
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ASSIGN THE STATION ADDRESS WHICH MAY BE DEFINED IN TWO WAYS:
C   1. THE ADDRESS IS ASSIGNED BASE ON STATION NUMBER.
C   2. THE ADDRESS IS AN ACTUAL X.21/X.25 ADDRESS
C (12 CHARACTERS SHOULD BE SUFFICIENT TO HANDLE THOSE BASED ON STATION #) - V02
C
1100	CONTINUE      
	IF (X2XSTN_ADDLEN .GT. 0) THEN
C
	  IF (ADDRESS_ASSIGNED) THEN
C
C	START OF V02 CHANGE BLOCK - USE VARIABLE FORMAT STATEMENT 
C	INSTEAD OF SEPARATE CASES

	    LEN = X2XSTN_ADDLEN
CV03	    PAD = MAX_ADDR_CHAR - LEN		!# OF LEADING 0s TO PREPEND
            PAD = DADRL - LEN
C
            
C
	    WRITE (STNADD,'(I<PAD>.<PAD>, I<LEN>.<LEN>)') 0, STN

	    CALL ATOH(CHRSTR, 1, DADRL, X2XSTN_ADDRES, ERR)		!V03

C	END OF V02 CHANGE BLOCK

	  ELSE
C
	    CALL ATOH(STNADR, 1, X2XSTN_ADDLEN, X2XSTN_ADDRES, ERR)
	  ENDIF
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
10000	CONTINUE
C
	RETURN
	END
