C
C DISP_SSC.FOR
C
C V02 05-JUN-2000 UXN SSOCOM added
C V01 27-FEB-1998 UXN Initial release.
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	PROGRAM DISP_SSC
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:SSPCOM.DEF'
	INCLUDE 'INCLIB:SSCCOM.DEF'
	INCLUDE 'INCLIB:SSOREC.DEF'
	INCLUDE 'INCLIB:SSOCOM.DEF'
	INCLUDE 'INCLIB:SSFREC.DEF'
C
	INTEGER*4   FDB(7),ST,EXT,OPT,GIND
	INTEGER*4   DISP_CNT,TOT_CNT
	INTEGER*4   BUCKET,ENTR,ELEM,I
	INTEGER*4   I4TEMP
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I1TEMP,I4TEMP)
	INTEGER*4   TOT_AMOUNT(2)
C
5       CONTINUE
	CALL INPNUM('Enter Super Score game index ',GIND,1,6,EXT)
	IF(EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
C
10	CONTINUE
	TYPE*,IAM()
	TYPE*,IAM(),'1 - Display overpool file '
	TYPE*,IAM(),'2 - Display pool file'
	TYPE*,IAM(),'3 - Display main table in memory'
	TYPE*,IAM(),'4 - Display top table in memory'
	TYPE*,IAM(),'5 - Display overpool in memory '
	TYPE*,IAM(),'E - Exit'
	TYPE*,IAM()
	CALL INPNUM('Enter selection ',OPT,1,5,EXT)
	IF(EXT.NE.0) GOTO 5
	GOTO (1000,2000,3000,4000,5000) OPT
	CALL GSTOP(GEXIT_FATAL)
C
C DISPLAY OVERPOOL FILE
C
1000	CONTINUE
	CALL OPENW(3,SSCPOF(1,GIND),4,0,0,ST)
        CALL IOINIT(FDB,3,SSOSEC*256)
        IF(ST.NE.0) CALL FILERR(SSCPOF(1,GIND),1,ST,0)
        CALL READW(FDB,1,SSOREC,ST)
	CALL CLOSEFIL(FDB)
	DISP_CNT = 0
	TOT_CNT  = 0
	TOT_AMOUNT(1)=0
	TOT_AMOUNT(2)=0
	DO I=1,SSONUM
	   IF(SSOAMT(I).NE.0) THEN
	      DISP_CNT = DISP_CNT + 1
	      TOT_CNT  = TOT_CNT  + 1
	      I4TEMP = SSOCMB(I)
	      WRITE(6,900) IAM(),TOT_CNT,
     *                     IAND(ISHFT(I4TEMP,-20),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-16),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-12),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-8),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-4),'0F'X),
     *                     IAND(I4TEMP,'0F'X),
     *                     CSMONY(SSOAMT(I),14,BETUNIT)
	     CALL ADDI8I4(TOT_AMOUNT,SSOAMT(I),BETUNIT)
	   ENDIF
	   IF(DISP_CNT.GT.21) THEN
		DISP_CNT = 0
                TYPE*,IAM(),'Press ENTER to continue...'
		READ(5,'(I4)') ST
	   ENDIF
	ENDDO	
	TYPE*,IAM(),'Total number of combinations in overpool file ',TOT_CNT
	TYPE*,IAM(),'Amount in total ',CSMONYI8(TOT_AMOUNT,20,BETUNIT)	
	GOTO 10
C
C Display pool file....
C
2000	CONTINUE
	CALL OPENQW(3,SSCPFN(1,GIND),4,0,0,ST)
        CALL IOQINIT(FDB,3,SSFSEC*256)
        IF(ST.NE.0) CALL FILERR(SSCPFN(1,GIND),1,ST,0)
        CALL READQW(FDB,1,SSFREC,ST)
	CALL CLOSEQFIL(FDB)
	DISP_CNT=0
	TOT_CNT =0
	TOT_AMOUNT(1)=0
	TOT_AMOUNT(2)=0
	DO I = 1,SSGPOL
	  IF(SSFMAIN(I).NE.-1) THEN  
	    IF(SSFCAMT(I).NE.0) THEN
		DISP_CNT = DISP_CNT + 1
		TOT_CNT  = TOT_CNT  + 1
		I4TEMP = IAND(SSFMAIN(I),'00FFFFFF'X) 
	        WRITE(6,900) IAM(),TOT_CNT,
     *                     IAND(ISHFT(I4TEMP,-20),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-16),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-12),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-8),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-4),'0F'X),
     *                     IAND(I4TEMP,'0F'X),
     *                     CSMONY(SSFCAMT(I),14,BETUNIT)
	         CALL ADDI8I4(TOT_AMOUNT,SSFCAMT(I),BETUNIT)
               ENDIF
	       IF(DISP_CNT.GT.21) THEN
		  DISP_CNT = 0
		  TYPE*,IAM(),'Press ENTER to continue...'
		  READ(5,'(I4)') ST
	       ENDIF
	    ENDIF
	ENDDO	 		
	TYPE*,IAM(),'Total number of combinations in pools file ',TOT_CNT	
	TYPE*,IAM(),'Amount in total ',CSMONYI8(TOT_AMOUNT,20,BETUNIT)	
	GOTO 10
C
C Display main table in memory... 
C
3000	CONTINUE
	DISP_CNT = 0
	TOT_CNT  = 0
	TOT_AMOUNT(1)=0
	TOT_AMOUNT(2)=0
	DO BUCKET=0,SSGNOB
	  DO ENTR=1,SSGISZ
	    IF(ISSPMAIN(ENTR,BUCKET,GIND).NE.-1) THEN  
	      IF(SSPCAMT(ENTR,BUCKET,GIND).NE.0) THEN
		DISP_CNT = DISP_CNT + 1
		TOT_CNT  = TOT_CNT  + 1
		I4TEMP = IAND(ISSPMAIN(ENTR,BUCKET,GIND),'00FFFFFF'X) 
	        WRITE(6,900) IAM(),TOT_CNT,
     *                     IAND(ISHFT(I4TEMP,-20),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-16),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-12),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-8),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-4),'0F'X),
     *                     IAND(I4TEMP,'0F'X),
     *                     CSMONY(SSPCAMT(ENTR,BUCKET,GIND),14,BETUNIT)
	         CALL ADDI8I4(TOT_AMOUNT,SSPCAMT(ENTR,BUCKET,GIND),BETUNIT)
               ENDIF
	       IF(DISP_CNT.GT.21) THEN
		  DISP_CNT = 0
		  TYPE*,IAM(),'Press ENTER to continue...'
		  READ(5,'(I4)') ST
	       ENDIF
	    ENDIF
	  ENDDO
	ENDDO	 		
	TYPE*,IAM(),'Total number of combinations in main table ',TOT_CNT
	TYPE*,IAM(),'Amount in total ',CSMONYI8(TOT_AMOUNT,20,BETUNIT)	
	GOTO 10
C
C Display TOP table in memory... 
C
4000	CONTINUE
	DISP_CNT = 0
	TOT_CNT  = 0
	TOT_AMOUNT(1)=0
	TOT_AMOUNT(2)=0
	BUCKET = 0
	DO 4010 ELEM = 1, SSGTOP
	  ENTR = SSPTOPC(SSGAME,ELEM,GIND)
	  IF(ENTR.EQ.0) GOTO 4010   ! EMPTY ...
	  IF(ENTR.LT.0.OR.ENTR.GT.SSGISZ) THEN
	    TYPE*,IAM(),'Invalid entry number >',ENTR
	    GOTO 4010
	  ENDIF
	  CALL MOVBYT(SSPTOPC(SSGAMB,ELEM,GIND),1,BUCKET,1,2)
	  IF(BUCKET.LT.0.OR.BUCKET.GT.SSGNOB) THEN
	    TYPE*,IAM(),'Invalid bucket number >',BUCKET
	    GOTO 4010
	  ENDIF
          IF(SSPCAMT(ENTR,BUCKET,GIND).NE.0) THEN
	      DISP_CNT  = DISP_CNT+1
	      TOT_CNT   = TOT_CNT+1
	      I4TEMP    = 0
	      I1TEMP(3) = SSPTOPC(SSGCID+2,ELEM,GIND)
	      I1TEMP(2) = SSPTOPC(SSGCID+1,ELEM,GIND) 
	      I1TEMP(1) = SSPTOPC(SSGCID+0,ELEM,GIND) 
	      WRITE(6,900) IAM(),TOT_CNT,
     *                     IAND(ISHFT(I4TEMP,-20),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-16),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-12),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-8),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-4),'0F'X),
     *                     IAND(I4TEMP,'0F'X),
     *                     CSMONY(SSPCAMT(ENTR,BUCKET,GIND),14,BETUNIT)
	         CALL ADDI8I4(TOT_AMOUNT,SSPCAMT(ENTR,BUCKET,GIND),BETUNIT)
	  ENDIF
	  IF(DISP_CNT.GT.21) THEN
	    DISP_CNT = 0
            TYPE*,IAM(),'Press ENTER to continue...'
	    READ(5,'(I4)') ST
	  ENDIF
4010	CONTINUE
	TYPE*,IAM(),'Total number of combinations in top table ',TOT_CNT
	TYPE*,IAM(),'Amount in total ',CSMONYI8(TOT_AMOUNT,20,BETUNIT)	
	GOTO 10
C
C DISPLAY OVERPOOL COMMON
C
5000	CONTINUE
	DISP_CNT = 0
	TOT_CNT  = 0
	TOT_AMOUNT(1)=0
	TOT_AMOUNT(2)=0
	DO I=1,SSOCOMNUM
	   IF(SSOCOMAMT(I,GIND).NE.0) THEN
	      DISP_CNT = DISP_CNT + 1
	      TOT_CNT  = TOT_CNT  + 1
	      I4TEMP = SSOCOMCMB(I,GIND)
	      WRITE(6,900) IAM(),TOT_CNT,
     *                     IAND(ISHFT(I4TEMP,-20),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-16),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-12),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-8),'0F'X),
     *                     IAND(ISHFT(I4TEMP,-4),'0F'X),
     *                     IAND(I4TEMP,'0F'X),
     *                     CSMONY(SSOCOMAMT(I,GIND),14,BETUNIT)
	     CALL ADDI8I4(TOT_AMOUNT,SSOCOMAMT(I,GIND),BETUNIT)
	   ENDIF
	   IF(DISP_CNT.GT.21) THEN
		DISP_CNT = 0
                TYPE*,IAM(),'Press ENTER to continue...'
		READ(5,'(I4)') ST
	   ENDIF
	ENDDO	
	TYPE*,IAM(),'Total number of combinations in overpool file ',TOT_CNT
	TYPE*,IAM(),'Amount in total ',CSMONYI8(TOT_AMOUNT,20,BETUNIT)	
	GOTO 10
C
900	FORMAT(1X,A,I5,' - ',I2.2,'-',I2.2,' : ',
     *                       I2.2,'-',I2.2,' : ',
     *                       I2.2,'-',I2.2,' amount = ',A)
	END
