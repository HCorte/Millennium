C PASVER.FOR
C
C V03 25-AUG-05 FRP Modify for Natal 2005: Allow to enter 0 in winning serie.
C V02 25-OCT-02 TRG CHANGE NUMBER ENTRY 
C V01 17-DEC-00 CS  INITIAL RELEASE FOR PORTUGAL
C
C SUBROUTINE TO PROCESS LOTTERY ENTRY OF PASSIVE LOTTERY RESULTS
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
	SUBROUTINE PASVER(GNUM,GIND,DRAW,ST)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
C LOCAL VARIABLES
C
	CHARACTER*19 STRING
	CHARACTER*2  THR(4)
        CHARACTER*25 STR
        CHARACTER*3 CYESNO(0:1)

        DATA CYESNO /'NO ','YES'/

	INTEGER*4 Y, EXT, ST, DRAW, GIND, GNUM
	INTEGER*4 DIVS,K,FLAG
	INTEGER*4 WINSUM,OPSUM,NUM
	INTEGER*4 DPAHLD(PAGNBR,PAGDIV),DPAHWSER,HLDEXTPRZ,NBR,WINSER,QTD
	INTEGER*4 NUMSER,INISER

C
C  DATA INICIALIZATION
C
         DATA THR    /'st','nd','rd','th'/
C
C CODE STARTS HERE
C

C
C INITIALIZE SOME VARIBLES
C
20	CALL FASTSET(0,DPAHLD,PAGNBR)
	DPAHWSER = 0

	WINSUM = 1
	OPSUM  = 0
	DO WHILE( WINSUM.NE.OPSUM )
	    WINSUM = 0
	    OPSUM  = 0
C
C ENTER REGULAR WINNING RESULTS
C
            DO DIVS = 1, DPADIV
                IF (DPAWNUM(DIVS).GT.0) THEN
                   TYPE*,IAM(),'*** WINNING NUMBERS FOR DIVISION ',DIVS
                   DO NBR=1,DPAWNUM(DIVS)
                      IF(NBR.LE.3) THEN
	                WRITE(STRING,906) NBR,THR(NBR) 
	              ELSE 
	              	WRITE(STRING,906) NBR,(THR(4))
	              ENDIF
                      CALL INPNUM(STRING,NUM,0,DPANUMTCK,EXT)
                      IF(EXT.LT.0) CALL MY_GSTOP(GEXIT_OPABORT)

                      DPAHLD(NBR,DIVS) = NUM
                      WINSUM           = WINSUM + NUM
                      QTD              = QTD + 1
                   ENDDO
                   TYPE*,IAM(),' '
	        ENDIF 
            ENDDO
C
C IF POPULAR OR EXTRAORDINARIA, ASK FOR WINNING SERIE
C
            IF (GIND.EQ.PSBPOP.OR.DPAEMT.EQ.EM_EXT) THEN
		INISER = 0
	        NUMSER = DPANUMSER
                IF (GIND.EQ.PSBPOP) THEN
		  INISER = 1
		  NUMSER = DPANOFFRA
		ENDIF
                CALL INPNUM('Enter winning serie: ',WINSER,INISER,NUMSER,EXT)
                IF (EXT.LT.0) CALL MY_GSTOP(GEXIT_OPABORT)

                DPAHWSER = WINSER
                WINSUM    = WINSUM + WINSER
                QTD      = QTD + 1
                STR = ' including winning serie '
            ENDIF
C
C IF ESPECIAL, ASK FOR EXTRA PRIZE
C
            FLAG = 0
            IF (DPAEMT.EQ.EM_ESP) THEN
                CALL INPYESNO('Add especial prize in firts division ?',FLAG,EXT)
                IF (EXT.LT.0) CALL MY_GSTOP(GEXIT_OPABORT)
                IF(FLAG.NE.1) FLAG=0
                HLDEXTPRZ = FLAG
            ENDIF
C
C ENTER WINNING NUMBERS SUM
C
            CALL INPNUM('Enter sum of winning numbers '//STR,
     *                      OPSUM,0,DPANUMTCK*QTD,EXT)
            IF (EXT.LT.0) CALL MY_GSTOP(GEXIT_OPABORT)


C	
C CHECK VALUES AGAINST OPERATOR ENTRY
C

	    IF(OPSUM.NE.WINSUM) TYPE *,IAM(),
     *		    ' Winning numbers sum incorrect, please re-enter '
	ENDDO
C
C DISPLAY ALL ENTERED INFORMATION AND ASK
C IF IT IS OK.
C
        IF (DPAEMT.EQ.EM_ESP) THEN
            WRITE(6,903) IAM(),CYESNO(FLAG)
        ENDIF

        IF (GIND.EQ.PSBPOP.OR.DPAEMT.EQ.EM_EXT) THEN
            WRITE(6,905) IAM(),DPAHWSER
        ENDIF

        DO DIVS=1,DPADIV
	    IF (DPAWNUM(DIVS).GT.0) THEN
               WRITE(6,904) IAM(),DIVS,(K,DPAHLD(K,DIVS),K=1,DPAWNUM(DIVS))
	    ENDIF
        ENDDO


	CALL WIMG(5,'Are the results entered ok [Y/N]? ')

	CALL YESNO(Y)
	IF(Y.NE.1) GOTO 20
C
C VERIFY AGAINST OPERATOR ENTRY
C
	DO DIVS = 1,DPADIV
            DO NBR=1,DPAWNUM(DIVS)
	       IF( DPAWIN(NBR,DIVS).NE.DPAHLD(NBR,DIVS) ) THEN
		  TYPE*,'Verification error, please re-enter'

		  OPDONE=0
		  DPASTS=GAMBFD
		  ST=-1
		  RETURN
	       ENDIF
	    ENDDO
	ENDDO

	IF (GIND.EQ.PSBPOP.OR.DPAEMT.EQ.EM_EXT) THEN
	    IF (DPAWSER.NE.DPAHWSER) THEN
		TYPE*,'Verification error, please re-enter'

		OPDONE=0
		DPASTS=GAMBFD
		ST=-1
		RETURN
	     ENDIF
	ENDIF

	IF (DPAEMT.EQ.EM_ESP) THEN
	    IF (HLDEXTPRZ.NE.EXTPRZ) THEN
		TYPE*,'Verification error, please re-enter'

		OPDONE=0
		DPASTS=GAMBFD
		ST=-1
		RETURN
	    ENDIF
	ENDIF
C

	ST=0
	DPASTS=GAMENV

	RETURN
C
C FORMAT STATEMENTS
C
900	FORMAT(1X,A8,I1,' invalid game status> ',I4)
901	FORMAT(1X,A8,I1,' results entered ',13(A1,1X))
903     FORMAT(1X,A,' Add special prize: ',A3)
904     FORMAT(1X,A,' Winning Numbers for division ',I2.2,': ',/,<DPAWNUM(DIVS)>(1X,I2.2,' - ',I5.5,/))
905     FORMAT(1X,/,A,' Winning Serie: ',I2.2)
906	FORMAT(1X,'Enter ',I2,A,' number')

	END
C
C ROUTINE TO ABORT PROGRAM A CLEAR SOME VARIABLES
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE MY_GSTOP(STATUS_STOP)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:RESCOM.DEF'
C
C PARAMETERS
C
        INTEGER*4   STATUS_STOP
C
C CLEAR SOME VARIABLES
C
        VERREADY = 0
C
C ABORT PROGRAM
C
        CALL GSTOP(STATUS_STOP)
        RETURN
        END

