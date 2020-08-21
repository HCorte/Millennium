C
C SUBROUTINE ALIST
C
C V08 15-MAR-2011 GPW NUMAGT=12288
C V07 27-MAY-2001 EPH VARIABLE CODE HAS 2 MORE POSITIONS (TRANSPORTE / CENTRAL DE RECESSAO)
C V06 22-SEP-2000 UXN USE_LOOKUP added.
C V05 10-JUN-1993 HXN Move HASF.DEF, which contains AGTINF.DEF, 
C                     before RECAGT.DEF.
C V04 01-MAR-1993 EBD DAS update 3/1/93
C                     Changing format of ASF file
C V03 12-MAR-1991 JPJ INITIAL RELEASE FOR MARYLAND
C V02 15-FEB-1991 WOL USES NEW CHARACTER CMONY ROUTINES
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C ALIST - LIST AGENTS ID AND MONETARY TRANSACTIONS.
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
	SUBROUTINE ALIST(FDB,MODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:HASF.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

	INTEGER*4 FDB(7), CN, LNUM, K, J, I, IND1, ST, EXT
	INTEGER*4 AGT, UNIT, MODE,TER
	INTEGER*2 DATE(LDATE_LEN)
	CHARACTER*68 LINE1
	CHARACTER*33 LINE2
	CHARACTER*20 MNAME(7)
	CHARACTER CODE(6),CZERO
	LOGICAL*4 USE_LOOKUP
	COMMON /HASF/ LOOKUP(NUMAGT),ASFREC,SCFREC,USE_LOOKUP
	INTEGER*4 LOOKUP
	DATA MNAME/'Identification data',
     *	           'Banking data       ',
     *	           'Marketing data     ',
     *	           'Hotline data       ',
     *             'Gtrack data        ',
     *             'Communication data ',
     *             'Suspension dates   '/
 
	DATA CODE/'P','C','D','X','T','R'/       !PAYMENT/CREDITS/DEBITS/PENALTY/TRANSPORTE/CENTRAL RECESSAO
	DATA CZERO/Z0/
	CALL CLRSCR(5)
C
C REQUEST AGENT NUMBER
C
10	CONTINUE
	UNIT=6
C
        IF(USE_LOOKUP) THEN
           CALL INPNUM('Enter agent number (E - Exit, P - Print) ',
     *                 AGT,1,9999999,EXT)
           IF(EXT.EQ.-1) RETURN
           DO I=1,NUMAGT
              IF(AGT.EQ.LOOKUP(I)) THEN
                 TER = I
                 GOTO 11
              ENDIF
           ENDDO
	   TYPE*,IAM(),'Agent ',AGT,' does not exist!' 
           GOTO 10
        ELSE
           CALL INPNUM('Enter terminal number (E - Exit, P - Print) ',
     *                 TER,1,NUMAGT,EXT)
           IF(EXT.EQ.-1) RETURN
        ENDIF
11	CONTINUE
C
C CHECK FOR PRINT COMMAND
C
	IF(EXT.EQ.-2) THEN
	  CALL CLRSCR(5)
	  UNIT=7
          OPEN(UNIT, FILE='HASF.REP', STATUS='NEW', DISP='PRINT/DELETE')
	  GOTO 15
	ENDIF
C
C READ AGENTS RECORD
C
	CALL CLRSCR(5)
	CALL READW(FDB,TER,ASFREC,ST)
	IF(ST.NE.0) THEN
	  CALL CLRSCR(5)
	  CALL FILERR(SCFSFN(1,ASF),2,ST,TER)
	  GOTO 10
	ENDIF
C
C DISPLAY AGENT ID INFORMATION
C
15	CONTINUE
	WRITE(UNIT,901) TER,MNAME(MODE+1)
	IND1=MODE*29
C
	DO 100 I=1,13
	IND1=IND1+1
	WRITE(LINE1,902)
	WRITE(LINE2,906)
	IF(FLDBEG(IND1).EQ.0) GOTO 30
	DO 20 J=FLDBEG(IND1),FLDEND(IND1)
	IF(ASFBYT(J).EQ.CZERO) ASFBYT(J)=' '
20	CONTINUE
	WRITE(LINE1,903) FLDNAM(IND1),
     *	                  (ASFBYT(K),K=FLDBEG(IND1),FLDEND(IND1))
C
C
30	CONTINUE
	IF(ASFLGR(LGRCDC,I).NE.0) THEN
	  DATE(5)=ASFLGR(LGRCDC,I)
	  CALL LCDATE(DATE)
	  LNUM=MOD(ASFLGR(LGRCOD,I),10000)
	  CN=ASFLGR(LGRCOD,I)/10000+1
	  WRITE(LINE2,904) (DATE(K),K=7,13),CODE(CN),LNUM,
     *                      CSMONYI8(ASFLGR(LGRAMTU,I),12,BETUNIT)
	ENDIF
C
	WRITE(UNIT,905) LINE1,LINE2
100	CONTINUE
C
C
	WRITE(LINE2,906)
	DO 200 I=14,29
	IND1=IND1+1
	IF(FLDBEG(IND1).EQ.0) GOTO 200
	DO 110 J=FLDBEG(IND1),FLDEND(IND1)
	IF(ASFBYT(J).EQ.CZERO) ASFBYT(J)=' '
110	CONTINUE
	WRITE(LINE1,902)
	WRITE(LINE1,903) FLDNAM(IND1),
     *	                  (ASFBYT(K),K=FLDBEG(IND1),FLDEND(IND1))
	WRITE(UNIT,905) LINE1,LINE2
200	CONTINUE
	IF(EXT.EQ.-2) CLOSE(6)
	GOTO 10
C
C
901	FORMAT('1  Terminal ',I5,1X,A20,T50,
     *	       11X,'Monetary transactions',/)
902	FORMAT(47(' '))
903	FORMAT(1X,A8,' > ',55A1)
904	FORMAT(7A2,1X,A1,I4.4,A12,1X)
905	FORMAT(A47,A33)
906	FORMAT(33(' '))
	END
