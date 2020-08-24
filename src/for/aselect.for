C ASELECT.FOR
C
C V01 13-MAR-91 MTK INITIAL RELEASE FOR MARYLAND
C
C
C SUBROUTINE TO SELECT AGENTS FOR UNMESS/PROMO PROGRAMS
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ASELECT(UNIT,LIST,COUNT,BROADCAST,EXT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:X2XPRM.DEF'
C
C
	INTEGER*4 J, ST, AGT, UNIT, FIELD, COUNT,IND, K , EXT
	INTEGER*4 LIST(NUMAGT),BEG(10),END(10)
	
	INTEGER*4 BEGDISTR, ENDDISTR, BEGMED, ENDMED ! created in 2008/03/28
	INTEGER*4 MINAGT, MAXAGT, MINDISTR, MAXDISTR ! created in 2008/03/28
	PARAMETER (MINAGT=0)    ! created in 2008/03/28
	PARAMETER (MAXAGT=9999) ! created in 2008/03/28
	PARAMETER (MINDISTR=0)  ! created in 2008/03/28
	PARAMETER (MAXDISTR=99) ! created in 2008/03/28
	INTEGER*4 DISTR, AGTNBR, I, TYP ! created in 2008/03/28
	CHARACTER*8 AGNO ! AGENT NUMBER - created in 2008/03/28
	CHARACTER*3 AGTP ! AGENT TYPE - created in 2008/03/28
	
	CHARACTER*8 MENU(12)
	CHARACTER VALUE(20)
        LOGICAL   BROADCAST
        INTEGER*4   BROADSTN    
        CHARACTER*1  CHR80(80)
        CHARACTER*5  CHR_STN
        EQUIVALENCE (CHR80(3),CHR_STN)
C
C
	DATA MENU/'ALL TERS','ONE TERM','ZIPCODE ','LINE    ',
     *            'CITY    ','AGT TYPE','STATION#','BUS CODE',
     *            'CARTELL ','TERITORY','BRDC SRV','SCML GRP'/ ! changed in 2008/03/28
C
	DATA BEG/0,0,SZIPC,SXSTN,SCITY,SATYP,SXSTN,SBUSC,ECHAN,STERR/
	DATA END/0,0,EZIPC,EXSTN,ECITY,EATYP,EXSTN,EBUSC,ECHAN,ETERR/
C                         (ONLY LAST POSITION OF CARTELL/CHAIN FIELD IS USED)                                                    
C
C GET FIELD
C
10	CONTINUE
	COUNT=0
	CALL FASTSET(0,LIST,NUMAGT)
	WRITE(5,900) IAM(),(K,MENU(K),K=1,12)
	CALL INPNUM('Enter field number (E-exit) ',FIELD,1,12,EXT)
	IF(EXT.LT.0) RETURN
C
C CHECK FOR ALL AGENTS
C
	IF(FIELD.EQ.1) THEN
	  COUNT=NUMAGT
	  RETURN
	ENDIF
C
C CHECK FOR SINGLE TERMINAL
C
	IF(FIELD.EQ.2) THEN
	  CALL INPNUM('Enter terminal number ',LIST(1),1,NUMAGT,EXT)
	  IF(EXT.LT.0) GOTO 10
	  COUNT=1
	  RETURN
	ENDIF
C
C IF BROADCAST SERVER THEN GET SERVER NUMBERS FROM STNDEF.FIL
C
        IF(FIELD.EQ.11) THEN

C OPEN THE STATION DEFINITION FILE
C
           CALL OPENX(2,'STNDEF.FIL',4,0,0,ST)
           IF(ST.NE.0) THEN
              CALL OS32ER(5,'STNDEF.FIL','OPENW',ST,0)
              CALL GPAUSE
           ENDIF
C
C READ THE STATION DEFINITION FILE
C
           COUNT=0
20         CONTINUE
           CHR80(1) = ' '
           READ(2,9001,END=25) CHR80
C
           IF(CHR80(1).EQ.'#') THEN
              GOTO 20
           ELSEIF (CHR80(1).EQ.'B' .OR. CHR80(1).EQ.'b') THEN
              BROADSTN = CTOI(CHR_STN,K)
              IF((BROADSTN.GT.0).AND.(BROADSTN.LE.X2X_STATIONS)) THEN
                 COUNT=COUNT+1
                 LIST(COUNT)=BROADSTN
              ENDIF
              GOTO 20
           ELSE
              GOTO 20
           ENDIF

25         CONTINUE
           CLOSE(2)
           BROADCAST=.TRUE.
           RETURN
        ENDIF
C
C CHECK FOR SCML GROUP (CREATED IN 2008/03/28)
C
	IF(FIELD.EQ.12) THEN
	  CALL INPNUM2('Enter start district code   ',BEGDISTR,MINDISTR,MAXDISTR,EXT)
	  IF(EXT.LT.0) GOTO 10
	  CALL INPNUM2('Enter end district code (S-skip)  ',ENDDISTR,BEGDISTR,MAXDISTR,EXT)
	  IF(EXT.LT.0 .AND. EXT.NE.-13) GOTO 10
	  IF(EXT.EQ.-13) THEN
	  	EXT=0
	  	ENDDISTR = BEGDISTR
	  	CALL INPNUM2('Enter first mediator code (A-all) ',BEGMED,MINAGT,NUMAGT,EXT)
	  	IF((EXT.LT.0).AND.(EXT.NE.-4)) GOTO 10
	  	IF(EXT.EQ.-4) THEN
	  	  EXT=0
	      ENDMED=NUMAGT ! ALL AGENTS
	    ELSE
	  	  CALL INPNUM2('Enter last mediator code          ',ENDMED,BEGMED,NUMAGT,EXT)
	  	  IF(EXT.LT.0) GOTO 10
	    ENDIF
	  ELSE
	  	BEGMED = 1 
	  	ENDMED = NUMAGT
	  ENDIF
C
C FILTER ALL AGENTS SUBJECT TO THE USER RESTRICTIONS 
C
	    TYPE*,IAM(),' Searching agent file...'
	    CALL OPENASF(UNIT)
	    DO 101 AGT=1,NUMAGT
	      CALL READASF(AGT,ASFREC,ST)
	      IF(ST.NE.0) THEN
	        COUNT=0
	        RETURN
	      ENDIF
C
		  WRITE(AGTP,*) (ASFBYT(I),I=SATYP,EATYP) ! GETS THE AGENT TYPE
		  READ(AGTP(3:3),'(I1.1)',ERR=101) TYP

		  IF(TYP.EQ.1) THEN ! AGENT TYPE: 1 - ONLINE | 2 - OFFLINE
		    WRITE(AGNO,*) (ASFBYT(I),I=SAGNO,EAGNO) ! GETS THE AGENT NUMBER
		    READ(AGNO(2:3),'(I2.2)',ERR=101) DISTR 
		    READ(AGNO(4:8),'(I5.5)',ERR=101) AGTNBR
		  
		    IF((DISTR.GE.BEGDISTR) .AND. (DISTR.LE.ENDDISTR) .AND. (AGTNBR.GE.BEGMED) .AND. (AGTNBR.LE.ENDMED)) THEN
		  	  COUNT=COUNT+1
	     	  LIST(COUNT)=AGT
		    ENDIF
		  ENDIF
101	    CONTINUE
	      CALL USRCLOS1(UNIT)
		  RETURN
	ENDIF
C
C GET FIELD VALUE
C
	CALL WIMG(5,'Enter field value ')
	READ(5,901) VALUE
C
        IF(FIELD.EQ.9 .AND. (VALUE(1).LT.'0' .OR. VALUE(1).GT.'9'))
     *  THEN
	  TYPE*,IAM(),'Invalid value'
          GOTO 10
        ENDIF
C
C READ ALL AGENTS
C
	TYPE*,IAM(),' Searching agent file...'
	CALL OPENASF(UNIT)
	DO 100 AGT=1,NUMAGT
	CALL READASF(AGT,ASFREC,ST)
	IF(ST.NE.0) THEN
	  COUNT=0
	  RETURN
	ENDIF
C
C
	IND=1
	DO 30 J=BEG(FIELD),END(FIELD)
	IF(ASFBYT(J).NE.VALUE(IND)) GOTO 100
	IND=IND+1
30	CONTINUE
	COUNT=COUNT+1
	LIST(COUNT)=AGT
100	CONTINUE
        CALL USRCLOS1(     UNIT)
	RETURN
900	FORMAT(1X,A,' The following options are available:',/,
     *         12(20X,I2,' - ',A8,/),/)
901	FORMAT(20A1)
9001    FORMAT(80A1)
	END
 
