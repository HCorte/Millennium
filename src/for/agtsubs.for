C
C AGTSUBS.FOR                                                                    
C
C V18 18-DEC-2015 FRP CR31 PIN for Owner and Clerk
C V17 31-MAR-2011 FRP Taken from INTERFACES_IO.FOR
C V16 29-NOV-2010 FRP Lotto2 Changes
C                 FJG Compilation error
C V15 27-APR-2010 FRP ePassive
C V14 06-APR-2009 FRP Modify for EM Joker
C v13 21-NOV-2002 TRG FIX LINHA_DISTIBUICAO TO CORRECT OFFSET
C V12 25-OCT-2002 TRG CONVERT 2->4 DIGITS YEAR IN AGTMIL
C V11 11-MAY-2001 EPH INCLUDE READING OF LINHA_DISTRIBUICAO / CENTRAL_RECEPCAO / STATUS_TRANSPORTE FROM AGTMIL
C V10 28-MAY-2001 EPH INCLUDE READING OF TIPCENREC FIELD IN OFFWAGFIN DETAIL RECORD
C V09 27-MAY-2001 EPH READS ODJ USING SCML CONVENTION (IF GAME NUMBER GT 5 THEN IT IS ADDED OF 1)
C V08 27-MAY-2001 EPH GENERATES ODJ USING SCML CONVENTION (IF GAME NUMBER GT 5 THEN IT IS SUBTRACTED FROM 1)
C V07 27-MAY-2001 EPH USE ONLY 10 DIGTS OF THE RIGTH FOR THE ACCOUNT IN OCR LINE (WITHOUT DV)
C V06 17-MAY-2001 EPH CHANGE OPEN_BNK TO FIXED SIZE FILE (80 BYTES)
C V05 03-MAY-2001 EPH INCLUDED WORSAP FILE ROUTINES
C V04 27-APR-2001 ANG INCLUDED OFFWAG AND OFFWAGFIN ROUTINES
C V03 28-MAR-2001 EPH INCLUDE ODJ FILE MANIPULATING ROUTINES
C V02 16-FEB-2001 EPH INCLUDE X2XADDRESS
C V01 31-JAN-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C ROUTINES FOR OPENING, READING AND WRITING FROM/TO AGTMIL INTERFACE FILE
C                                                                               
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C This item is the property of GTECH Corporation, W.Greenwich, Rhode            
C Island, and contains confidential and trade secret information. It            
C may not be transferred from the custody or control of GTECH except            
C as authorized in writing by an officer of GTECH. Neither this item            
C nor the information it contains may be used, transferred,                     
C reproduced, published, or disclosed, in whole or in part, and                 
C directly or indirectly, except as expressly authorized by an                  
C officer of GTECH, pursuant to written agreement.                              
C                                                                               
C Copyright 1993 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C 	**************************************************
     	SUBROUTINE OPEN_AGTMIL (LUN, ST)
C	**************************************************
   	IMPLICIT NONE                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

	INTEGER*4 ST
	INTEGER*4 LUN

	AGTMIL_REC.LUN = 0

        CALL CLEAR_AGTMIL_REC

	AGTMIL_REC.LUN = LUN

        OPEN (UNIT            =  AGTMIL_REC.LUN,
     *        FILE            = 'FILE:AGTMIL.ASC',
     *        STATUS          = 'OLD', 
     *        CARRIAGECONTROL = 'LIST',
     *        RECL            = 469,
     *        IOSTAT          =  ST)

	IF (ST.NE.0) THEN    
	   AGTMIL_REC.ERRSTR  = 'Error opening file AGTMIL.ASC'
	ENDIF

	RETURN
	END



C 	**************************************************
     	SUBROUTINE READ_AGTMIL (ST)
C	**************************************************
   	IMPLICIT NONE                                                  
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

	INTEGER*4     ST            ! (OUTPUT)

	INTEGER*4     SZ
	CHARACTER*469 ASCREC
	CHARACTER*2   REFREC
        CHARACTER*50  AUXSTR

	CHARACTER*1   TIPOAG

	CHARACTER*7   AGTSTR

        CHARACTER*80 BLKLIN  /'                                                                                '/
        CHARACTER*80 ZERLIN  /'00000000000000000000000000000000000000000000000000000000000000000000000000000000'/

	INTEGER*4    TERMINAL
	INTEGER*4    CERR

	ST = 0
        CALL CLEAR_AGTMIL_REC

  	READ (AGTMIL_REC.LUN, 100, END=200, ERR=300, IOSTAT=ST) ASCREC
100	FORMAT (A469)

        AUXSTR = ITOC(ST,SZ)
        AGTMIL_REC.ERRSTR = 'Reading status = ' // AUXSTR

        REFREC = ASCREC(1:2)

        IF (REFREC.EQ.'HA' .OR. REFREC.EQ.'TA' .OR. REFREC.EQ.'01' .OR. REFREC.EQ.'02' .OR. REFREC.EQ.'05') THEN

           AGTMIL_REC.RECNUM = AGTMIL_REC.RECNUM + 1

	   IF (REFREC.EQ.'HA') THEN
	      IF (AGTMIL_REC.RECTYPE.NE.'  ') THEN
C	         * HEADER SHOULD BE THE FIRST RECORD ON FILE *
	         ST = -19
                 AUXSTR = ITOC(AGTMIL_REC.RECNUM,SZ)
                 AGTMIL_REC.ERRSTR = 'Header found in REC = ' // AUXSTR(1:SZ) 
	      ENDIF
              AGTMIL_REC.RECTYPE    = 'HD'
              AGTMIL_REC.YEAR       = CTOI(ASCREC(3:6), SZ) 
              AGTMIL_REC.MONTH      = CTOI(ASCREC(7:8), SZ) 
              AGTMIL_REC.DAY        = CTOI(ASCREC(9:10),SZ) 
              AGTMIL_REC.DATAHD     = ASCREC(9:10) // '/' // ASCREC(7:8) // '/' // ASCREC(3:6)

              IF (AGTMIL_REC.YEAR.LE.0)                              ST = -11    
              IF (AGTMIL_REC.MONTH.LE.0 .OR. AGTMIL_REC.MONTH.GT.12) ST = -11
              IF (AGTMIL_REC.DAY.LE.0 .OR. AGTMIL_REC.DAY.GT.31)     ST = -11
	      IF (ST.EQ.-11) THEN
	         AGTMIL_REC.ERRSTR = 'Invalid date on Header : ' // AGTMIL_REC.DATAHD
              ENDIF
	   ENDIF

           IF (REFREC.EQ.'01' .OR. REFREC.EQ.'02' .OR. REFREC.EQ.'05') THEN

	      IF (AGTMIL_REC.RECTYPE.EQ.'  ') THEN
C	         * DETAIL IS THE FIRST RECORD IN FILE *
	         ST = -20
                 AGTMIL_REC.ERRSTR = 'Header not found'
	      ENDIF
	      IF (AGTMIL_REC.RECTYPE.EQ.'TL') THEN
C	         * TRAILLER SHOUL BE THE LAST RECORD *
	         ST = -21
                 AGTMIL_REC.ERRSTR = 'There are other records after the trailler'
	      ENDIF
              AGTMIL_REC.RECTYPE    = 'DT'               

              AGTMIL_REC.OPER_TYPE = REFREC(2:2)

	      AGTMIL_REC.AGENT = CTOI(ASCREC(3:9), SZ)
              AGTSTR =  ASCREC(3:9)
              IF (AGTMIL_REC.AGENT.LE.0) THEN
                 ST = -14
 	         AGTMIL_REC.ERRSTR = 'Invalid agent number : ' // AGTSTR
              ENDIF

              TIPOAG = ASCREC(10:10)
              IF (TIPOAG.NE.'1' .AND.      ! ONLINE
     *            TIPOAG.NE.'2' .AND.      ! OFFLINE
     *            TIPOAG.NE.'3' .AND.      ! BANK
     *            TIPOAG.NE.'4') THEN      ! PRIVILIGED
                 ST = -24
 	         AGTMIL_REC.ERRSTR = 'Invalid agent type : ' // AGTMIL_REC.AGENT_TYPE
              ELSE
	         IF (TIPOAG.EQ.'1' .OR. TIPOAG.EQ.'2') THEN
                    AGTMIL_REC.AGENT_TYPE = TIPOAG
                 ELSE
                    IF (TIPOAG.EQ.'3') THEN
                       AGTMIL_REC.BANK       = 'Y' 
                       AGTMIL_REC.PRIVILIGED = 'Y'
		       AGTMIL_REC.AGENT_TYPE = '1'
                    ELSE
                       AGTMIL_REC.PRIVILIGED = 'Y'
                       AGTMIL_REC.AGENT_TYPE = '1'
                    ENDIF
                 ENDIF
              ENDIF 
	      AGTMIL_REC.AGENT_NAME     = ASCREC(11:65)
              IF (AGTMIL_REC.AGENT_NAME.EQ.BLKLIN(1:55)) THEN
                 ST = -23
 	         AGTMIL_REC.ERRSTR = 'Agent name is blank'
              ENDIF
	      AGTMIL_REC.MANAGER_NAME   = ASCREC(66:120)
              AGTMIL_REC.STORE_NAME     = ASCREC(121:165)
	      AGTMIL_REC.BUSINESS_CODE  = ASCREC(166:171)
	      AGTMIL_REC.LOCATION_CODE  = ASCREC(172:177)
	      AGTMIL_REC.ADDRESS        = ASCREC(178:222)
	      AGTMIL_REC.ZIP_CODE       = ASCREC(223:229)
	      AGTMIL_REC.ZIP_CODE_NAME  = ASCREC(230:259)
	      AGTMIL_REC.AGENT_PHONE    = ASCREC(260:268)
	      AGTMIL_REC.AGENT_FAX      = ASCREC(269:277)
	      AGTMIL_REC.BUSINESS_TYPE  = CTOI(ASCREC(278:279),SZ) 
	      AGTMIL_REC.MANAGER_PHONE  = ASCREC(280:288)

   	      AGTMIL_REC.WAGER_STATUS   = ASCREC(289:289)
C
C	      SEMANA OF MUTUAS BEGIN
C
              AGTMIL_REC.WAGER_BSAD     = ASCREC(290:295)
C
C	      SEMANA FOR MUTUS SUSPENSION BEGIN
C
              AGTMIL_REC.WAGER_BSUD     = ASCREC(296:301)
C
C	      SEMANA FOR MUTUAS SUSPENSION END
C
              AGTMIL_REC.WAGER_ESUD     = ASCREC(302:307)
C
C	      SEMANA FOR MUTUAS END
C
              AGTMIL_REC.WAGER_ESAD     = ASCREC(308:313)

	      AGTMIL_REC.WAGER_ACCOUNT  = ASCREC(314:334) 

   	      IF (AGTMIL_REC.WAGER_STATUS.NE.' '.AND.AGTMIL_REC.WAGER_STATUS.NE.'V') THEN
                 ST = -25
 	         AGTMIL_REC.ERRSTR = 'Invalid wager status : ' // AGTMIL_REC.WAGER_STATUS
              ENDIF
C
              CALL CHECK_AGTMIL_DATE(AGTMIL_REC.WAGER_BSAD,SZ)
	      IF(SZ .NE. 0) THEN
                ST = -26
 	        AGTMIL_REC.ERRSTR = 'Invalid wager begin sales : ' // AGTMIL_REC.WAGER_BSAD
              ENDIF
C
              CALL CHECK_AGTMIL_DATE(AGTMIL_REC.WAGER_BSUD,SZ)
	      IF(SZ .NE. 0) THEN
                ST = -27
 	        AGTMIL_REC.ERRSTR = 'Invalid wager begin supression : ' // AGTMIL_REC.WAGER_BSUD
              ENDIF
C
              CALL CHECK_AGTMIL_DATE(AGTMIL_REC.WAGER_ESUD,SZ)
	      IF(SZ .NE. 0) THEN
                ST = -28
                AGTMIL_REC.ERRSTR = 'Invalid wager end supression : ' // AGTMIL_REC.WAGER_ESUD
              ENDIF
C
              CALL CHECK_AGTMIL_DATE(AGTMIL_REC.WAGER_ESAD,SZ)
	      IF(SZ .NE. 0) THEN
                ST = -29
                AGTMIL_REC.ERRSTR = 'Invalid wager end sales : ' // AGTMIL_REC.WAGER_ESAD
              ENDIF
C
	      IF (AGTMIL_REC.WAGER_ACCOUNT.EQ.BLKLIN(1:21) .AND. AGTMIL_REC.WAGER_STATUS.EQ.'V') THEN
                 ST = -30
 	         AGTMIL_REC.ERRSTR = 'Wager NIB account is blank'
              ENDIF

   	      AGTMIL_REC.PASSIVE_STATUS = ASCREC(381:381)
C
C	      SEMANA OF PASSIVE BEGIN
C
              AGTMIL_REC.PASSIVE_BSAD     = ASCREC(382:387)
C
C	      SEMANA OF PASSIVE SUSPENSION BEGIN
C
              AGTMIL_REC.PASSIVE_BSUD     = ASCREC(388:393)
C
C	      SEMANA OF PASSIVE SUSPENSION END
C
              AGTMIL_REC.PASSIVE_ESUD     = ASCREC(394:399)
C
C	      SEMANA OF PASSIVE END
C
              AGTMIL_REC.PASSIVE_ESAD     = ASCREC(400:405)

	      AGTMIL_REC.PASSIVE_ACCOUNT= ASCREC(406:426)

   	      IF (AGTMIL_REC.PASSIVE_STATUS.NE.' '.AND.AGTMIL_REC.PASSIVE_STATUS.NE.'V') THEN
                 ST = -37
 	         AGTMIL_REC.ERRSTR = 'Invalid PASSIVE status : ' // AGTMIL_REC.PASSIVE_STATUS
              ENDIF
C
              CALL CHECK_AGTMIL_DATE(AGTMIL_REC.PASSIVE_BSAD,SZ)
	      IF(SZ .NE. 0) THEN
                ST = -38
                AGTMIL_REC.ERRSTR = 'Invalid PASSIVE begin sales : ' // AGTMIL_REC.PASSIVE_BSAD
              ENDIF
C
              CALL CHECK_AGTMIL_DATE(AGTMIL_REC.PASSIVE_BSUD,SZ)
	      IF(SZ .NE. 0) THEN
                ST = -39
                AGTMIL_REC.ERRSTR = 'Invalid PASSIVE begin supression : ' // AGTMIL_REC.PASSIVE_BSUD
              ENDIF
C
              CALL CHECK_AGTMIL_DATE(AGTMIL_REC.PASSIVE_ESUD,SZ)
	      IF(SZ .NE. 0) THEN
                ST = -40
                AGTMIL_REC.ERRSTR = 'Invalid PASSIVE end supression : ' // AGTMIL_REC.PASSIVE_ESUD
              ENDIF
C
              CALL CHECK_AGTMIL_DATE(AGTMIL_REC.PASSIVE_ESAD,SZ)
	      IF(SZ .NE. 0) THEN
                ST = -41
                AGTMIL_REC.ERRSTR = 'Invalid PASSIVE end sales : ' // AGTMIL_REC.PASSIVE_ESAD
              ENDIF
C
              IF (TIPOAG.NE.'2' .AND. TIPOAG.NE.'4') THEN
	         IF ((AGTMIL_REC.PASSIVE_ACCOUNT.EQ.BLKLIN(1:21) .OR. AGTMIL_REC.PASSIVE_ACCOUNT.EQ.ZERLIN(1:21))
     *           .AND. AGTMIL_REC.PASSIVE_STATUS.EQ.'V') THEN
                    ST = -42
 	            AGTMIL_REC.ERRSTR = 'PASSIVE NIB account is blank.  Agent = '//AGTSTR
                 ENDIF
              ENDIF

	      AGTMIL_REC.SAP_NUMBER     = CTOI(ASCREC(427:432),SZ)
              IF (AGTMIL_REC.SAP_NUMBER.EQ.0 .AND. TIPOAG.NE.'3' .AND. TIPOAG.NE.'4') THEN    !TIPOAG 3/4 HAS SAP# = 0
                 ST = -42
 	         AGTMIL_REC.ERRSTR = 'SAP number is equal to zero - Agent' // AGTSTR
              ENDIF 
	      AGTMIL_REC.BANK_OP        = ASCREC(433:436) 
	      AGTMIL_REC.BRANCH_OP      = ASCREC(437:440)  
	      
	      AGTMIL_REC.AGENT_PASSWORD = CTOI(ASCREC(441:444),SZ)

	      AGTMIL_REC.X2XADDRESS     = ASCREC(445:453)

	      AGTMIL_REC.LINHA_DISTRIBUICAO = CTOI(ASCREC(456:457),SZ)

	      AGTMIL_REC.CENTRAL_RECEPCAO   = CTOI(ASCREC(458:464),SZ)
              IF (AGTMIL_REC.CENTRAL_RECEPCAO.GT.0       .AND.
     *            AGTMIL_REC.CENTRAL_RECEPCAO.NE.9900001 .AND.
     *		  AGTMIL_REC.CENTRAL_RECEPCAO.NE.9900002 .AND.
     *            AGTMIL_REC.CENTRAL_RECEPCAO.NE.9900999 .AND.
     *            AGTMIL_REC.CENTRAL_RECEPCAO.NE.9900500 .AND.
     *            AGTMIL_REC.CENTRAL_RECEPCAO.NE.9900501 .AND. 
     *            AGTMIL_REC.CENTRAL_RECEPCAO.NE.9900503 .AND.
     *            AGTMIL_REC.CENTRAL_RECEPCAO.NE.9900504 ) THEN
                 CALL GET_TERM (AGTMIL_REC.CENTRAL_RECEPCAO, TERMINAL, CERR)
                 IF (CERR.NE.0) THEN
C                    ST = -45
 	            AGTMIL_REC.ERRSTR = 'Central Recepcao not found in ASF ='// ASCREC(458:464) 
	         ENDIF
              ENDIF
	      AGTMIL_REC.STATUS_TRANSPORTE  = CTOI(ASCREC(465:465),SZ)
	      IF (AGTMIL_REC.STATUS_TRANSPORTE.LT.0 .OR. AGTMIL_REC.STATUS_TRANSPORTE.GT.3) THEN
                 ST = -46
 	         AGTMIL_REC.ERRSTR = 'Transport status out of range = '// ASCREC(465:465) // ' Agent = ' // AGTSTR
              ENDIF

	      AGTMIL_REC.MANAGER_PASSWORD = CTOI(ASCREC(466:469),SZ)

	   ENDIF

	   IF (REFREC.EQ.'TA') THEN
              AGTMIL_REC.RECTYPE= 'TL'               
	      AGTMIL_REC.NUM_RECORDS= CTOI(ASCREC(3:8), SZ) 
              IF (AGTMIL_REC.NUM_RECORDS.NE.AGTMIL_REC.RECNUM-2) THEN
                 ST = -22
		 AGTMIL_REC.ERRSTR = 'Number of records declared <> found'
              ENDIF
           ENDIF           

        ELSE
           ST = -10   ! INVALID RECORD
	   AGTMIL_REC.ERRSTR = 'Invalid record type : ' // REFREC
        ENDIF

        RETURN       !NORMAL RETURN
	
200	CONTINUE     !END-OF-FILE
        IF (AGTMIL_REC.RECTYPE.NE.'TL') THEN
           ST = -144
           AGTMIL_REC.ERRSTR = 'End of file and Trailler Record not found'
        ELSE
           ST = 144
        ENDIF
        RETURN

300     CONTINUE     !READ ERROR
	AGTMIL_REC.ERRSTR = 'Error reading AGTMIL.ASC'
        RETURN

	END

C 	**************************************************
     	SUBROUTINE CHECK_AGTMIL_DATE(AGTMIL_DATE,ST)
C	**************************************************
   	IMPLICIT NONE                                                  
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
C
	INTEGER*4 ST,XDD,XMM,XYY,SZ
	CHARACTER AGTMIL_DATE*6
        INTEGER*4 LIM_DAYS(12)
        LOGICAL   IS_LEAP
C
	ST = 0
C
        DO SZ = 1,6
          IF(AGTMIL_DATE(SZ:SZ) .LT. '0' .OR. AGTMIL_DATE(SZ:SZ) .GT. '9') THEN    !non-numeric
            ST = -1
            RETURN
          ENDIF
        ENDDO
C
        XDD = CTOI(AGTMIL_DATE(1:2),SZ)
        XMM = CTOI(AGTMIL_DATE(3:4),SZ)
        XYY = CTOI(AGTMIL_DATE(5:6),SZ)
C
        IF(XDD .EQ. 0 .AND. XMM .EQ. 0 .AND. XYY .EQ. 0) RETURN  !'000000' allowed
C
        LIM_DAYS(01) = 31
        LIM_DAYS(02) = 28
        LIM_DAYS(03) = 31
        LIM_DAYS(04) = 30
        LIM_DAYS(05) = 31
        LIM_DAYS(06) = 30
        LIM_DAYS(07) = 31
        LIM_DAYS(08) = 31
        LIM_DAYS(09) = 30
        LIM_DAYS(10) = 31
        LIM_DAYS(11) = 30
        LIM_DAYS(12) = 31
C
        IF(XDD .LT. 0 .OR. XDD .GT. 31) ST = -1
        IF(XMM .LT. 0 .OR. XMM .GT. 12) ST = -1
        IF(XYY .LT. 0 .OR. XYY .GT. 99) ST = -1
C
        IF(XDD .EQ. 0 .AND. XMM .NE. 0) ST = -1
        IF(XDD .NE. 0 .AND. XMM .EQ. 0) ST = -1
        IF((XDD .EQ. 0 .OR. XMM .EQ. 0) .AND. XYY .NE. 0) ST = -1
C
        IF(XDD .NE. 0 .AND. XMM .NE. 0) THEN
C
          IF(XYY .GT. 40) THEN  !Consider from 1900 dates after year 40
            XYY = XYY + 1900
          ELSE
            XYY = XYY + 2000
          ENDIF
          CALL IS_LEAP_YEAR(XYY,IS_LEAP)
          IF(IS_LEAP) LIM_DAYS(2) = 29
C
          IF(XDD .GT. LIM_DAYS(XMM)) ST = -1  !inconsistent date
        ENDIF
C
	RETURN
	END

C
C************************
C SUBROUTINE IS_LEAP_YEAR
C************************
C TO KNOW IF IS LEAP YEAR
C
C==== OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE IS_LEAP_YEAR(YEAR,IS_LEAP)
      IMPLICIT NONE
C
      INTEGER*4 YEAR
      LOGICAL IS_LEAP
C
      IS_LEAP = .FALSE.
C
C IF YEAR IS DIVISIBLE BY 400 IS LEAP YEAR
      IF(MOD(YEAR,400) .EQ. 0) IS_LEAP = .TRUE.
C
C IF YEAR IS DIVISIBLE BY 4 AND NOT BY 100 IT'S LEAP YEAR
      IF(MOD(YEAR,4) .EQ. 0 .AND. MOD(YEAR,100) .NE. 0) IS_LEAP = .TRUE.
C
      RETURN
      END

C	*****************************************************
	SUBROUTINE CLEAR_AGTMIL_REC
C	*****************************************************
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

        CHARACTER*80 BLKLIN  /'                                                                                '/

	IF (AGTMIL_REC.LUN.EQ.0) THEN   
C
C	   FIRST TIME. CLEAR CONTROL, HEADER AND TRAIL DATA TOO.
C
	   AGTMIL_REC.RECTYPE    = '  '
	   AGTMIL_REC.RECNUM     = 0

	   AGTMIL_REC.DAY        = 0
	   AGTMIL_REC.MONTH      = 0
	   AGTMIL_REC.YEAR       = 0
	   AGTMIL_REC.DATAHD     = '  /  /    '

	   AGTMIL_REC.NUM_RECORDS= 0
	ENDIF

	AGTMIL_REC.ERRSTR     = '  '

	AGTMIL_REC.AGENT          = 0
        AGTMIL_REC.OPER_TYPE      = ' '
        AGTMIL_REC.AGENT_TYPE     = ' '
        AGTMIL_REC.BANK           = 'N'
        AGTMIL_REC.PRIVILIGED     = 'N'
	AGTMIL_REC.AGENT_NAME     = BLKLIN(1:55)
	AGTMIL_REC.MANAGER_NAME   = BLKLIN(1:55)
        AGTMIL_REC.STORE_NAME     = BLKLIN(1:45)
	AGTMIL_REC.BUSINESS_CODE  = BLKLIN(1:6)
	AGTMIL_REC.LOCATION_CODE  = BLKLIN(1:6)
	AGTMIL_REC.ADDRESS        = BLKLIN(1:45)
	AGTMIL_REC.ZIP_CODE       = BLKLIN(1:7)
	AGTMIL_REC.ZIP_CODE_NAME  = BLKLIN(1:30)
	AGTMIL_REC.AGENT_PHONE    = BLKLIN(1:9)
	AGTMIL_REC.AGENT_FAX      = BLKLIN(1:9)
	AGTMIL_REC.BUSINESS_TYPE  = 0
        AGTMIL_REC.MANAGER_PHONE  = BLKLIN(1:9)

	AGTMIL_REC.WAGER_STATUS   = ' '
	AGTMIL_REC.WAGER_BSAD     = '      '
	AGTMIL_REC.WAGER_BSUD     = '      '
	AGTMIL_REC.WAGER_ESUD     = '      '
	AGTMIL_REC.WAGER_ESAD     = '      '
	AGTMIL_REC.WAGER_ACCOUNT  = BLKLIN(1:21)

	AGTMIL_REC.PASSIVE_STATUS = ' '
	AGTMIL_REC.PASSIVE_BSAD   = '      '
	AGTMIL_REC.PASSIVE_BSUD   = '      '
	AGTMIL_REC.PASSIVE_ESUD   = '      '
	AGTMIL_REC.PASSIVE_ESAD   = '      '
	AGTMIL_REC.PASSIVE_ACCOUNT= BLKLIN(1:21)

	AGTMIL_REC.SAP_NUMBER     = 0
	AGTMIL_REC.BANK_OP        = '    '
	AGTMIL_REC.BRANCH_OP      = '    '
	AGTMIL_REC.AGENT_PASSWORD = 0
	AGTMIL_REC.MANAGER_PASSWORD = 0
	AGTMIL_REC.X2XADDRESS     = '000000000'

	RETURN
	END
