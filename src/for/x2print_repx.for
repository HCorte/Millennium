C
C SUBROUTINE PRINT_REPORT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2PRINT_REPX.FOV                             $
C  $Date::   17 Apr 1996 16:26:42                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C ** Source - X2TERMSTA.FOR;1 **
C
C V01 13-DEC-94 GPR RELEASED FOR UK
C
C X2_PRINTREP.FOR
C
C This routine will produce a report which lists 
C terminal status.
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
	SUBROUTINE X2PRINT_REPORT( SORTARRAY ,END_NORMAL, BEG_ACK_ERR,
     *	      END_INSTALLED,SUBNET)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:X2TERMSTA.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 SORTARRAY(LEN_SORTARRAY,X2X_TERMS)
	INTEGER*4 END_INSTALLED,END_NORMAL,BEG_ACK_ERR
	INTEGER*4 SUBNET
C
	CHARACTER*1 CHRSTR(16)			!ASCII Address
C	CHARACTER*16 CHRSTR			!ASCII Address
	CHARACTER*20 FILENAME
C
	INTEGER*4   ST                          !File status
	INTEGER*4   LINCNT   /60/               !Line counter
	INTEGER*4   PAGE     /0/                !Page counter
	INTEGER*4   TOTAL    /0/
	INTEGER*4   AGNT_NR			!Agent number
	INTEGER*4   TERM_NR			!Terminal number
	INTEGER*4   STAT_NR			!Station number
	INTEGER*4   ACK_CNT			!Delivery ack
	INTEGER*4   ERR_CNT			!Delivery ERRORS
C
	INTEGER*4   ERRCOUNT_INST(2,100)	!array counting number of
						!occurrences of each occured
						!error for installed terms
	INTEGER*4   ERRCOUNT_NINST(2,100)	!array counting number of
						!occurrences of each occured
						!error for non-installed
						!terms
C
C
	INTEGER*4   RCV_TIME, RCV_STN_TIME
	INTEGER*4   EXT, COPY, NOCHECK0, I, J, ADR_LEN
	INTEGER*4   DELAY
	INTEGER*4   STATION_STATE
C
	COMMON /NOCHECK0/ NOCHECK0
	NOCHECK0=13
C
	CALL COPYRITE
C
	TYPE *
	TYPE *,'<<<<< X2TERMSTA X2X Terminal Status Report V01 >>>>>'
	TYPE *
	CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C OPEN THE REPORT FILE.
C
	WRITE (FILENAME,9200) SUBNET
9200	FORMAT ('X2TERMSTAX',I2.2,'.REP')
C	FILENAME='X2TERMSTAX'//'03'//'.REP'
	OPEN(6,FILE=FILENAME,RECL=132,
     *	       STATUS='NEW',     IOSTAT=ST)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,'X2TERMSTAX.REP','OPEN',ST,0)
	  CALL GPAUSE
	ENDIF
C
C INITIALIZE ERRORCOUNT-ARRAY
C
	DO I=1,100
	    ERRCOUNT_INST(1,I) = -1
	    ERRCOUNT_NINST(1,I) = -1
	END DO
C
C WRITE EACH RECORD TO REPORT FILE
C
	DO 1000, I=1, X2X_TERMS
C
C PRINT A NEW PAGE IF NECESSARY.
C
	      IF(LINCNT.GT.55 .OR. I.EQ.END_INSTALLED
     *			      .OR. I.EQ.BEG_ACK_ERR)   THEN
	       IF (I.GE.END_INSTALLED.AND.I.LE.END_NORMAL) THEN
     		CALL TITLE('X2X TERMINAL STATUS REPORT NON-INSTALLED TERMS',
     *			  'X2TERMSTA',1,6,PAGE,DAYCDC)
	       ELSEIF (I.GE.BEG_ACK_ERR) THEN
     		CALL TITLE('X2X TERMINAL STATUS REP. DEL_ACK.NE.0/ERR.EQ.0',
     *			  'X2TERMSTA',1,6,PAGE,DAYCDC)
	       ELSE
     		CALL TITLE('X2X TERMINAL STATUS REPORT INSTALLED TERMS',
     *			  'X2TERMSTA',1,6,PAGE,DAYCDC)
  	       ENDIF
C
	       WRITE( 6, 9002 )
	       WRITE( 6, 9003 )
	       WRITE( 6, 9004 )
	       LINCNT=5
	      ENDIF
C
C GET AGENT, TERMINAL AND STATION NUMBERS FOR THIS RECORD
C
	      TERM_NR = SORTARRAY( TERM_NUMBER, I )
	      IF (TERM_NR.LE.0) GOTO 1000
	      AGNT_NR = AGTTAB( AGTNUM, TERM_NR )
	      STAT_NR = X2XT_STATION_NO( TERM_NR )
	      IF (STAT_NR.LE.0) GOTO 1000
	      DELAY=(X2XT_NETWORK_DELAY(TERM_NR)-X2XT_DELAY(TERM_NR))
     *			  /1000
	      ACK_CNT = X2XS_ACK_CNT(STAT_NR)
	      ERR_CNT = X2XS_ERR_CNT(STAT_NR)
	      RCV_TIME= X2XT_TIME(TERM_NR)
	      STATION_STATE=ZEXT(BX2XS_STATE(STAT_NR))
	      IF (STATION_STATE.GT.X2XS_IDLE) GOTO 1000 !!!!!!.AND. 
C*******     *				  RCV_TIME.EQ.0) GOTO 1000
	      RCV_STN_TIME=X2XS_TIME(STAT_NR)
	      IF (RCV_TIME.GT.3600*24 .OR. RCV_TIME.LT.0 .OR.
     *	          RCV_STN_TIME.GT.3600*24 .OR. RCV_STN_TIME.LT.0)THEN
			  TYPE *,'invalid time ',STAT_NR,TERM_NR,
     *			          RCV_TIME,RCV_STN_TIME
		  RCV_TIME=0
		  RCV_STN_TIME=0
	      ENDIF
C
C CONVERT X2X ADDRESS TO STRING FORMAT.
C
	      ADR_LEN = X2XS_ADRESS_LEN( STAT_NR )
	      CHRSTR(1) = '0'
	      CALL HTOA( CHRSTR, 1, ADR_LEN, X2XS_ADRESS( 1, STAT_NR ), ST )
C
C WRITE FIELDS TO REPORT FILE ONLY IF STATION IS CONFIGURED
C IN DATABASE
C
	      IF (STAT_NR.NE.0) THEN
C********       D_INST(VCDC) = SORTARRAY(INST_DATE,I)
C	        D_FIRST(VCDC) = SORTARRAY(FIRST_DATE,I)
C	        D_LAST(VCDC) = SORTARRAY(LAST_DATE,I)
C	        CALL CDATE(D_INST)
C	        CALL CDATE(D_FIRST)
C********       CALL CDATE(D_LAST)
C

		IF (SORTARRAY(INST_DATE,I).EQ.0)  THEN
     		  CALL ERROR_COUNTS(SORTARRAY(ERR_NR,I),ERRCOUNT_NINST)
    		ELSE 
     		  CALL ERROR_COUNTS(SORTARRAY(ERR_NR,I),ERRCOUNT_INST)
		ENDIF
C
		WRITE(6,9000) SORTARRAY( ERR_NR, I ), 
     *		    (CHRSTR(J),J=1,MAX(ADR_LEN,1)),ACK_CNT, 
     *		      ERR_CNT,AGNT_NR,
     *		      STAT_NR, TERM_NR, 
C    *		      D_INST(VMON),D_INST(VDAY),D_INST(VYEAR),
C     *		      D_FIRST(VMON),D_FIRST(VDAY),D_FIRST(VYEAR),
C     *		      D_LAST(VMON),D_LAST(VDAY),D_LAST(VYEAR),
     *		      SORTARRAY(INST_DATE,I),SORTARRAY(FIRST_DATE,I),
     *		      SORTARRAY(LAST_DATE,I),
     *		      RCV_TIME/3600,MOD(RCV_TIME/60,60),MOD(RCV_TIME,60),
     *		      RCV_STN_TIME/3600,MOD(RCV_STN_TIME/60,60),
     *		      MOD(RCV_STN_TIME,60),DELAY
C
		LINCNT=LINCNT+1
	      ENDIF
C
1000	CONTINUE    ! END OF DO LOOP
C
     	CALL TITLE('X2X TERMINAL STATUS ERR. CNT. INSTALLED TERMS',
     *			  'X2TERMSTA',1,6,PAGE,DAYCDC)
C
	WRITE (5,9070)
	WRITE (5,9030)
	WRITE (6,9030)
	WRITE (5,9040)
	WRITE (6,9040)
C
	CALL I4XSORT(ERRCOUNT_INST,2,100,2,0,0)
	CALL I4XSORT(ERRCOUNT_NINST,2,100,2,0,0)
C
	DO I=1,100
	    IF (ERRCOUNT_INST(1,I).NE.-1) THEN
	       WRITE (5,9020),ERRCOUNT_INST(1,I),ERRCOUNT_INST(2,I)
	       WRITE (6,9020),ERRCOUNT_INST(1,I),ERRCOUNT_INST(2,I)
	       TOTAL = TOTAL + ERRCOUNT_INST(2,I)
	    ENDIF
	END DO
C
	WRITE (5,9050),TOTAL
	WRITE (6,9050),TOTAL
C
     	CALL TITLE('X2X TERMINAL STATUS ERR. CNT. NON-INSTALLED TERMS',
     *			  'X2TERMSTA',1,6,PAGE,DAYCDC)
C
	WRITE (5,9080)
	WRITE (5,9030)
	WRITE (6,9030)
	WRITE (5,9040)
	WRITE (6,9040)
C
	TOTAL = 0
	DO I=1,100
	    IF (ERRCOUNT_NINST(1,I).NE.-1) THEN
	       WRITE (5,9020),ERRCOUNT_NINST(1,I),ERRCOUNT_NINST(2,I)
	       WRITE (6,9020),ERRCOUNT_NINST(1,I),ERRCOUNT_NINST(2,I)
	       TOTAL = TOTAL + ERRCOUNT_NINST(2,I)
	    ENDIF
	END DO
C
	WRITE (5,9050),TOTAL
	WRITE (6,9050),TOTAL
C
	CALL USRCLOS1(6)
	CALL USRCLOS1(PTMF)
	CALL SPOOL('X2TERMSTA.REP',COPY,ST)
C
C     =================== Format Statements ====================
C
9000	FORMAT( 1X, Z8.8, 3X, <16-MAX(ADR_LEN,1)>X, <MAX(ADR_LEN,1)>A, 
C     *  X,I4, X, I4,2X, I8, 3X, I8, 2X, I8, 3X, I2.2,'/',I2.2,'/',
C     *  I2.2,2X, I2.2,'/',I2.2,'/',I2.2, 3X, I2.2,'/',I2.2,'/',I2.2,
     *  X,I4, X, I4,2X, I8, 3X, I8, 2X, I8, 3X, I4,2X,I4,3X,I4,
     *  2X,I2,':',I2.2,':',I2.2, 2X, I2,':',I2.2,':',I2.2,1X,I6)
9002	FORMAT('0 ERR CODE       X2X-ADDRESS   ACK/ERR '
     *	  '  AGENT NO. '
     *    ' STAT NO.  TERM NO.  INST   1ST   LAST '
     *    'TERM  TIME  STN TIME DELAY' )
9003	FORMAT('  ========  ================   =======   =======   '
     *    '========= '
     *    ' ========  ====  =====  ====  ========='
     *    '  ======== ======' )
9004	FORMAT(' ')
9020	FORMAT(1X,Z8.8,3X,I8)
9030	FORMAT(' ERR CODE  ERR COUNT')
9040	FORMAT(' ========  =========')
9050	FORMAT(' TOTAL:     ',I8)
9070	FORMAT(' ERROR COUNTS INSTALLED TERMINALS ')
9080	FORMAT(' ERROR COUNTS NON-INSTALLED TERMINALS ')
	END

C
C**************** ERROR_COUNTS () ***********************
C
	SUBROUTINE ERROR_COUNTS(ERRNR,ERRCOUNTARRAY)
	IMPLICIT NONE

	INTEGER*4 ERRNR
	INTEGER*4   ERRCOUNTARRAY(2,100)	!array counting number of
						!occurrences of each occured
						!error
	INTEGER*4 I
C
	DO I=1,100
	    IF (ERRCOUNTARRAY(1,I).EQ.ERRNR) THEN
     	      ERRCOUNTARRAY(2,I) = ERRCOUNTARRAY(2,I)+1
	      RETURN
	    ENDIF
	    IF (ERRCOUNTARRAY(1,I).EQ.-1) THEN
		ERRCOUNTARRAY(1,I) = ERRNR
		ERRCOUNTARRAY(2,I) = 1
	        RETURN
	    ENDIF
	END DO
C
	RETURN
C
	END
