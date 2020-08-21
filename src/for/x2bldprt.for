C
C PROGRAM X2BLDPRT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2BLDPRT.FOV                                 $
C  $Date::   17 Apr 1996 16:10:02                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2bldprt.for **
C
C X2BLDPRT.FOR
C
C V05 06-FEB-96 DAS/SMW ADDED CODE TO ENTER NETWORK PORT ADDR. LEN
C V04 01-DEC-95 SCD CREATE NETWORK PORT AND LOCAL PORT FILES IF THEY DON'T
C                   EXIST
C V03 01-JUN-95 PJS MODIFIED TO PROMPT FOR PORT TYPE.
C V02 01-FEB-94 GRP ALLOW 100 SAPS
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This program will build a test network for the X2X Distributed
C Network System.
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM X2BLDPRT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XLPC.DEF'
	INCLUDE 'INCLIB:X2XNPC.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*2   DATBUF(12)      !Date buffer
	INTEGER*4   SYSDATE(3)      !System date
	INTEGER*4   SREC, ANS, PRT, ST, REPLACE
	INTEGER*4   ENDPRT, EXT, BEGPRT
	INTEGER*4   SAP, ERR, SAP_PORT
	INTEGER*4   UPDNET, NPLEN	
	INTEGER*4   PTYP            !Port Type
        INTEGER*4   IFILNAM(5)      !V04 - File name as integer
        INTEGER*4   NONEXISTENT_FILE !V04 - Status returned from OPENX when
        PARAMETER  (NONEXISTENT_FILE = -1) !when a file does not exist
C
	CHARACTER   PROMPT*60       !Output prompt
	CHARACTER   NULL*60         !Null string
	CHARACTER   NULLEQV(60)*1   !Null string
	CHARACTER   X2FILNAM*20     !File name function
	CHARACTER*20	FILNAM      !V04 - File name as character
C
	CHARACTER   PRTADD*16
	CHARACTER   CHRSTR(16)*1
	EQUIVALENCE (PRTADD,CHRSTR)
C
	LOGICAL*4   PRTUPD,
     *              LPCOPEN /.FALSE./,		! Has X2XLPC.FIL been opened?
     *              NPCOPEN /.FALSE./		! Has X2XNPC.FIL been opened?
C
	DATA        NULLEQV /60*Z00/
	EQUIVALENCE (NULL,NULLEQV)

	EQUIVALENCE (FILNAM,   IFILNAM)         ! V04
C
	CALL COPYRITE
C
C CLEAR SCREEN AND DISPLAY TITLE.
C
	CALL CLRSCR(5)
	WRITE(5,9050)
C
C PROMPT FOR INPUT DATA.
C
10	CONTINUE
	SAP_PORT=0
C
	WRITE(5,*)
	WRITE(5,*)
C
        CALL INPNUM('Enter Beginning PORT: ',BEGPRT,1,2000,EXT)
	IF (EXT .LT. 0) GOTO 200
C
	CALL INPNUM('Enter Ending PORT:    ',ENDPRT,BEGPRT,2000,EXT)
	IF (EXT .LT. 0) GOTO 200
C
	CALL INPNUM('Enter SSAP:           ',SAP,1,100,EXT)		! V02
	IF (EXT .LT. 0) GOTO 200
C
	CALL INPNUM('Enter Port TYPE:      ',PTYP,1,X2XPT_MAX_TYPE,EXT)	! V03
	IF (EXT .LT. 0) GOTO 200
C
	CALL INPNUM('Enter Network port address length',NPLEN,
     *               1,10,EXT)
	IF (EXT .LT. 0) GOTO 200
C
	CALL WIMG(5,'Auto replace existing data: y/n ')
	CALL YESNO(REPLACE)
C
	CALL WIMG(5,'Update Local to Network relation: y/n ')
	CALL YESNO(UPDNET)
c
	WRITE(5,*)
C
C OPEN THE NETWORK PORT FILE.
C
	CALL OPENX(2,X2FILNAM(XNPC),4,0,0,ST)
C       Start of V04 Changes
	IF (ST .NE. 0) THEN
	    IF (ST .EQ. NONEXISTENT_FILE) THEN   !create the file and try
	        FILNAM = X2FILNAM(XNPC)          !again to open the file
	        TYPE *, IAM()
	        CALL CRTFIL(IFILNAM, X2XNPC_SECT*X2X_NETWORK_PORTS/2, ST)
	        IF (ST .NE. 0) THEN
	            TYPE *, IAM(), '*** ERROR CREATING ', FILNAM, ', ST = ', ST
	            GOTO 200
	        ENDIF
	        CALL OPENX(2,X2FILNAM(XNPC),4,0,0,ST)
            ELSE                  ! any other bad status is unexpected
	        CALL OS32ER(5,X2FILNAM(XNPC),'OPENX',ST,0)
	        GOTO 200
            ENDIF
        ENDIF
C       End of V04 Changes
	CALL IOINIT(X2XNPC_FDB,2,X2XNPC_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XNPC),'OPENX',ST,0)
	  GOTO 200
	ENDIF
	NPCOPEN = .TRUE.						! V03
C
C OPEN THE LOCAL PORT FILE.
C
	CALL OPENX(3,X2FILNAM(XLPC),4,0,0,ST)
C       Start of V04 Changes
	IF (ST .NE. 0) THEN
	    IF (ST .EQ. NONEXISTENT_FILE) THEN   !create the file and try
	        FILNAM = X2FILNAM(XLPC)          !again to open the file
 	        TYPE *, IAM()
	        CALL CRTFIL(IFILNAM, X2XLPC_SECT*X2X_LOCAL_PORTS/2, ST)
	        IF (ST .NE. 0) THEN
	            TYPE *, IAM(), '*** ERROR CREATING ', FILNAM, ', ST = ', ST
	            GOTO 200
	        ENDIF
	        CALL OPENX(3,X2FILNAM(XLPC),4,0,0,ST)
            ELSE                  ! any other bad status is unexpected
	        CALL OS32ER(5,X2FILNAM(XLPC),'OPENX',ST,0)
	        GOTO 200
            ENDIF
        ENDIF
C       End of V04 Changes
	CALL IOINIT(X2XLPC_FDB,3,X2XLPC_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XLPC),'OPENX',ST,0)
	  GOTO 200
	ENDIF
	LPCOPEN = .TRUE.						! V03
C
C DETERMINE THE CDC DATE FROM THE SYSTEM DATE.
C
	CALL XDAT(SYSDATE)
	DATBUF(VYEAR)=SYSDATE(1)
	DATBUF(VMON)=SYSDATE(2)
	DATBUF(VDAY)=SYSDATE(3)
	CALL BDATE(DATBUF)
C
C READ THE NEXT RECORD FROM THE BLD FILE.
C
	DO 100 PRT=BEGPRT,ENDPRT
C
C ================== NETWORK PORT FILE UPDATE ==================
C
C READ THE NETWORK PORT RECORD - IF IT DOES NOT EXIST, CREATE IT.
C
	   CALL READW(X2XNPC_FDB,PRT,X2XNPC_REC,ST)
	   IF(ST.NE.0) THEN
	     CALL OS32ER(5,X2FILNAM(XNPC),'READW',ST,PRT)
	     CALL GPAUSE
	   ENDIF
C
C IF AN EXISTING PORT ASK THE USER WHETHER THEY WISH TO
C UPDATE WITH THE NEW INPUT PORT PARAMETERS.
C
	PRTUPD=.TRUE.
	IF(X2XNPC_REC(1).GT.0.AND.REPLACE.NE.1) THEN
	  PROMPT=NULL
	  WRITE (PROMPT,9000) PRT
	  CALL WIMG(5,PROMPT)
	  CALL YESNO(ANS)
	  IF(ANS.EQ.3) GOTO 100
	  IF(ANS.NE.1) PRTUPD=.FALSE.
	ENDIF
C
C REWRITE THE STATION RECORD.
C
	IF(PRTUPD) THEN
	  X2XNPC_PORT=PRT
	  X2XNPC_FAST=1
	  X2XNPC_DDIS=0
	  X2XNPC_RETCNT=3
	  X2XNPC_RETTIM=10000
	  X2XNPC_ADDLEN=NPLEN
	  WRITE (PRTADD,'(I<NPLEN>.<NPLEN>)') PRT+10**(NPLEN-1)
	  CALL ATOH(CHRSTR,1,NPLEN,X2XNPC_ADDRES,ERR)
	  X2XNPC_ASSIGN=0
	  X2XNPC_CAPACITY=1
	  X2XNPC_STATE=1
	  X2XNPC_HUNTADR(1)=0
	  X2XNPC_HUNTADR(2)=0
	  X2XNPC_REVCHRG=1
	  X2XNPC_TYPE=PTYP						! V03
	  X2XNPC_NUMPVC=0
	  X2XNPC_UPDATE=DATBUF(VCDC)
	  CALL WRITEW(X2XNPC_FDB,PRT,X2XNPC_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XNPC),'WRITEW',ST,SREC)
	    CALL GPAUSE
	  ENDIF
	ENDIF
C
C READ THE LOCAL PORT RECORD - IF IT DOES NOT EXIST, CREATE IT.
C
	CALL READW(X2XLPC_FDB,PRT,X2XLPC_REC,ST)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XLPC),'READW',ST,PRT)
	  CALL GPAUSE
	ENDIF
C
	PRTUPD=.TRUE.
	IF(X2XLPC_REC(1).GT.0.AND.REPLACE.NE.1) THEN
	  PROMPT=NULL
	  WRITE (PROMPT,9001) PRT
	  CALL WIMG(5,PROMPT)
	  CALL YESNO(ANS)
	  IF(ANS.EQ.3) GOTO 100
	  IF(ANS.NE.1) PRTUPD=.FALSE.
	ENDIF
C
C REWRITE THE LOCAL PORT RECORD.
C
	IF(PRTUPD) THEN
	  X2XLPC_PORT=PRT
	  X2XLPC_SAP=SAP
	  SAP_PORT=SAP_PORT+1
	  X2XLPC_SAP_PORT=SAP_PORT
	  X2XLPC_STATE=1
	  X2XLPC_NETPORT = 0
	  IF(UPDNET.EQ.1) X2XLPC_NETPORT=PRT
	  X2XLPC_SITE=1
	  X2XLPC_UPDATE=DATBUF(VCDC)
	  CALL WRITEW(X2XLPC_FDB,PRT,X2XLPC_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XLPC),'WRITEW',ST,SREC)
	    CALL GPAUSE
	  ENDIF
	ENDIF
100	CONTINUE
C
	GOTO 10
C
C PROGRAM EXIT.
C
200	CONTINUE
	IF (LPCOPEN) CALL CLOSEFIL(X2XLPC_FDB)				! V03
	IF (NPCOPEN) CALL CLOSEFIL(X2XNPC_FDB)				! V03
	CALL USRCLOS1(6)
C
C     ===================== Format Statements ======================
C
9000	FORMAT(1X,'Network Pt. ',I5,' already exists - update it [Y/N/E] ')
9001	FORMAT(1X,'Local Pt.   ',I5,' already exists - update it [Y/N/E] ')
9050	FORMAT(//,T26,'GTECH Distributed Network',/,
     *	          T28,'Build Test Network',//)
	END
