C
C PROGRAM X2NETCTL
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2NETCTL.FOV                                 $
C  $Date::   17 Apr 1996 16:24:40                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2netctl.for;1 **
C
C X2NETCTL.FOR
C
C V03 29-DEC-94 WJK MOVE UNSOLICITED STATION CONNECT AND DISCONNECT FROM GLOBAL
C                   TO STATION CLASS
C V02 05-APR-94 GPR USE X2X_I4_STATION TO DETERMINE STATION AND TERNUM
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This program will allow an operator to: send a hard reset,
C soft reset, or statistics request to all or to  a specific
C range of X2X stations.
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
	PROGRAM X2NETCTL
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
C
	INTEGER*4   BEGSTN          !Beginning station
	INTEGER*4   ENDSTN          !Ending station
	INTEGER*4   OPT             !Menu option
	INTEGER*4   STN             !Station number
	INTEGER*4   PROBUF          !Buffer number
	INTEGER*4   BUFCNT, ST, EXT
	CHARACTER   PROMPT*60       !Output prompt
	CHARACTER   NULLEQV(60)*1   !Null string
	CHARACTER   NULL*60         !Null string
C
	DATA        NULLEQV /60*Z00/
	EQUIVALENCE (NULL,NULLEQV)
C
	CALL COPYRITE
C
50	CONTINUE
	CALL CLRSCR(5)
	WRITE(5,9000)
C
C READ THE MENU OPTION.
C
	PROMPT=NULL
	WRITE (PROMPT,9010)
	CALL INPNUM(PROMPT,OPT,1,3,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
9010	FORMAT(25(' '),'Enter option [1-3] ')
C
C READ THE RANGE OF STATIONS.
C
52	CONTINUE
	PROMPT=NULL
	WRITE (PROMPT,9020)
	CALL INPNUM(PROMPT,BEGSTN,1,X2X_STATIONS,EXT)
C
	PROMPT=NULL
	WRITE (PROMPT,9030)
	CALL INPNUM(PROMPT,ENDSTN,BEGSTN,X2X_STATIONS,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C MAX RESET WILL BE 60 AT A TIME.
C
	IF(ENDSTN-BEGSTN+1.GT.60) THEN
	  WRITE(5,9040)
	  CALL XWAIT(2,2,ST)
	  GOTO 52
	ENDIF
C
C ================ MAIN PROCESSING LOOP ==================
C
	DO 100 STN=BEGSTN,ENDSTN
C
C SKIP UNUSED STATIONS.
C
	  IF(X2XS_ADRESS(1,STN).EQ.0 .AND. X2XS_ADRESS(2,STN).EQ.0)
     *	    GOTO 100
C
C GET A PROCOM BUFFER.
C
	  BUFCNT=0
110	  CONTINUE
	  CALL GETBUF(PROBUF)
	  IF(PROBUF.LE.0) THEN
	    BUFCNT=BUFCNT+1
	    IF(BUFCNT.GT.10) THEN
	      TYPE *,'NO PROCOM BUFFERS AVAILABLE'
	      CALL GPAUSE
	    ENDIF
	    GOTO 110
	  ENDIF
C
C FILL UP PROCOM BUFFER WITH STANDARD INFORMATION.
C

C         ***** Start V02 changes *****

          IF (X2X_I4_STATION) THEN
	     PRO(TERNUM,PROBUF)=0
	     PRO(LINENO,PROBUF)=STN
	  ELSE
	     HPRO(TERNUM,PROBUF)=0
	     HPRO(LINENO,PROBUF)=STN
          ENDIF

C       ***** End V02 changes *****

	  HPRO(PRCSRC,PROBUF)=X2X_COM
	  HPRO(PRCDST,PROBUF)=0
	  HPRO(TRCODE,PROBUF)=TYPX2X_PRO
	  HPRO(QUENUM,PROBUF)=QIN
	  HPRO(MSGNUM,PROBUF)=0
	  HPRO(INPLEN,PROBUF)=30
	  HPRO(X2X_LINK,PROBUF)=0
	  HPRO(X2X_DEST,PROBUF)=X2DEST_FE+X2DEST_STATION+
     *	                        X2DEST_TRANSPORT
	  HPRO(X2X_CONNCTL_OVR,PROBUF)=256*
     *        (X2X_UNSOLICIT_FE_CONNECT*16+X2X_UNSOLICIT_FE_DISCONNECT)
C V03 *	        +(X2X_UNSOLICIT_STATION_CONNECT*16+
C V03 *	          X2X_UNSOLICIT_STATION_DISCONNEC )
     *          +(X2XC_UNSO_STN_CON(X2XS_STNCLS(STN))*16+               ! V03
     *            X2XC_UNSO_STN_DIS(X2XS_STNCLS(STN)) )                 ! V03
C
C BUILD THE MESSAGE BUFFER FOR THE STATION SOFT RESET COMMAND.
C
	  IF (OPT.EQ.1) THEN
	    PRO(INPTAB+00,PROBUF)='11A00400'X
	    PRO(INPTAB+01,PROBUF)='00500000'X
	    PRO(INPTAB+02,PROBUF)='000B0104'X
	    PRO(INPTAB+03,PROBUF)='0000FE00'X
	    PRO(INPTAB+04,PROBUF)='00000000'X
	    HPRO(INPTAB*2+2,PROBUF)=STN
	    HPRO(X2X_DELIVER_OVR,PROBUF)=X2FEMES_FLAGS_ER+
     *	                                 X2FEMES_FLAGS_DA
C
C BUILD THE MESSAGE FOR THE STATION HARD RESET COMMAND.
C
	  ELSE IF (OPT.EQ.2) THEN
	    PRO(INPTAB+00,PROBUF)='11A00400'X
	    PRO(INPTAB+01,PROBUF)='00500000'X
	    PRO(INPTAB+02,PROBUF)='000B0104'X
	    PRO(INPTAB+03,PROBUF)='000000FF'X
	    PRO(INPTAB+04,PROBUF)='00000000'X
	    HPRO(INPTAB*2+2,PROBUF)=STN
	    HPRO(X2X_DELIVER_OVR,PROBUF)=X2FEMES_FLAGS_ER+
     *	                                 X2FEMES_FLAGS_DA
C
C BUILD THE STATISTICS REQUEST MESSAGE.
C
	  ELSE IF(OPT.EQ.3) THEN
	    PRO(INPTAB+00,PROBUF)='11A00400'X
	    PRO(INPTAB+01,PROBUF)='00520000'X
	    PRO(INPTAB+02,PROBUF)='000B0103'X
	    PRO(INPTAB+03,PROBUF)='00000003'X
	    PRO(INPTAB+04,PROBUF)='00000000'X
	    HPRO(INPTAB*2+2,PROBUF)=STN
	    HPRO(X2X_DELIVER_OVR,PROBUF)=0
	    CALL I4TOBUF2(STN,PRO(INPTAB,PROBUF),
     *	                  11+X2STMES_CONF_STNO-1)
	  ENDIF
C
C QUEUE THE BUFFER TO X2XMGR.
C
	  CALL I4TOBUF2(STN,PRO(INPTAB,PROBUF),
     *	                10+X2STMES_STATION_NO-1)
	  CALL X2ADDPRO(PROBUF)
	  TYPE *,'BUFFER QUEUE: ',PROBUF
100	CONTINUE
	GOTO 50
C
C     ======================== FORMAT STATEMENTS ===================
C
9000	FORMAT(////,T26,'GTECH Distributed Network',/,
     *	          T28,'Network Control Task',//,
     *	          T20,'  1. Send Soft Reset',/,
     *	          T20,'  2. Send Hard Reset',/,
     *	          T20,'  3. Send Statistics Request',/,
     *	          T20,'  E. Exit',//)
9020	FORMAT(25(' '),'Enter beginning station:     ')
9030	FORMAT(25(' '),'Enter ending station:        ')
9040	FORMAT(25(' '),'Only 60 stations can be reset at a time')
	END
