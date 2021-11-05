C
C PROGRAM VISION
C
C V05 21-OCT-1999 UXN Usernames started with 'L' treated the same way as other.
C V04 19-MAY-1996 HXK Wojtek's security stuff added
C V03 24-JUN-1993 SXH Added PRMAGT.DEF and AGTINF.DEF
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM VISION
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
C
        INCLUDE '(LIB$ROUTINES)'   	
C

	INTEGER*4  SUPLOC                     !
	PARAMETER (SUPLOC=1)
	INTEGER*4  SOFFIND                    !
	PARAMETER (SOFFIND=4)

	INTEGER*4  I                          !
	INTEGER*4  NOCHECK0                   !
	INTEGER*4  EXIT                       !
	INTEGER*4  NAME(2)                    !
	INTEGER*4  DFLAG                      !
	INTEGER*4  SECLEV                     !
	INTEGER*4  STATUS                     !
	INTEGER*4  UID                        !
	INTEGER*4  SIND                       !
	INTEGER*4  PASCMD                     !
	INTEGER*4  ST                         !

	CHARACTER     CNAME(8)                !
	CHARACTER*20  PASPAS                  !
	CHARACTER*6   PASSENT                 !
        CHARACTER*6   DEFTPASS                !

        INTEGER*8  STAT_LOGICAL

	EQUIVALENCE(PASPAS,PASSENT,EXIT)
	EQUIVALENCE(NAME,CNAME)

	DATA DEFTPASS/'DONUTS'/
	DATA DFLAG/0/
	DATA SECLEV/0/
	DATA LOTVIS/0/
	DATA PASCMD/05002/
C
	COMMON /NOCHECK0/ NOCHECK0
C
	CALL COPYRITX(5)
C
	NOCHECK0=-1
C
C
C GET TASK ID NUMBER FOR COMMANDS
C
	CALL GETNAM(NAME)
	IDNUM=NAME(1)
C
C Get  security levels   and if there is no LEVELS file set
C default flag
C
	CALL BUILDLEV(VINDEX,STATUS)
	CALL USRCLOS1(     1)
	IF (STATUS.EQ.1) THEN
	   SECLEV=SUPERUSE
	   DFLAG=1
	   STATUS=1
	   GOTO 5
	ENDIF
C
C Prompt user to sign on with user ID and password
C
5	 CONTINUE
	 PASS=0
	 STP=0
C
C If default flag has been set then goto default password signon
C
	 IF (DFLAG.EQ.1) THEN
	    STATUS=1
	    GOTO 6
	 ENDIF

	 CALL CLRSCR(5)
	 CALL SGNON(VINDEX,SECLEV,UID,SIND,STATUS)
	 CALL USRCLOS1(     2)
C
C Call logging routine to write signon transaction to TMF
C
C**    CALL LOGSON(VINDEX,UID,SIND)
C                                            LEVEL.FIL not found
C Default to PERKIN  as password
C
6	CONTINUE
	 IF (STATUS.EQ.1) THEN                !USER.FIL not found or
C
C Ask for default password
C
 10	     CONTINUE
             CALL BUILDALL(VINDEX,STATUS)
	     PASS=SUPLOC
	     CALL CLRSCR(5)
	     STP=0
	     CALL PASSWORD(5,PASPAS)
	     IF(EXIT.EQ.'EXIT') THEN
	        CALL CLRSCR(5)
	        CALL GSTOP(GEXIT_SUCCESS)
	     ENDIF
	     IF(PASSENT.EQ.DEFTPASS.AND.PASSENT.NE.'        ') GOTO 30
	     GOTO 10
	 ENDIF
C
C Get proper group level and set user id in memory
C
	 USENUM=UID
	 DO 20 I=1,16
	    IF (SECLEV.EQ.LEVELS(I)) THEN
	       PASS=I
	       GOTO 30
	    ENDIF
 20	 CONTINUE
C
C Security level not declared in levels file
C
	  CALL CLRSCR(5)
	  WRITE(5,9000)
 9000	  FORMAT(' Security level not declared in LEVELS file')
	  CALL XWAIT(2,2,ST)
	  GOTO 5
C
C Check if PASNUM can be changed by this user
C
 30	CONTINUE
C
	DO 80 I=1,100
	     IF(CMDS(I,PASS).EQ.PASCMD) LOTVIS=1
 80	CONTINUE
C
C
	VINIT=1
C	IF(CNAME(1).EQ.'L') THEN
C	   CALL BEGIN1               !INPUT DRIVEN
C	   LOTVIS=1
C	ELSE
	   IF(LIB$GET_LOGICAL("OLM_DMQSRV_FAILOVER_HOST")) THEN
		  STAT_LOGICAL = LIB$DELETE_LOGICAL("OLM_DMQSRV_FAILOVER_HOST","LNM$GROUP")
		  STAT_LOGICAL = LIB$DELETE_LOGICAL("OLM_DMQSRV_PRIMARY_HOST","LNM$GROUP")
		  CALL OPSTXT('...MESSAGEQ HOST LOGICAL NAMES REMOVED...')		
	   ENDIF
	   CALL BEGIN                !INTERVAL DRIVEN
	   LOTVIS=0
C	ENDIF
	GOTO 5
	END
