C LSYS.FOR
C
C V05 09-FEB-95 CAR CHECK EXISTENCE OF SYSTEM NUMBER BEFORE DISPLAY
C V04 01-FEB-94 MTK REMOVED EXTENDED SYSTEM DEFINITIONS
C V03 24-JAN-94 MTK DEFINE EXTENED SYSTEM FOR EVERY NORMAL SYSTEM
C V02 19-JAN-94 LXR MADE INTO SUBROUTINE TO BE CALLED BY BLDSYS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C+
C+   LSYS.FTN
C+
C+      V1.0   APR-14-98 WS INITIAL RELEASE
C+
C+    THIS PROGRAM ALLOWS TO ENTER SYSTEM BETS IN SYSCHK.FIL
C+    AND IT SERVES TO ANALYZE SYSTEM BETS DISTRIBUTION
C+
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
C Copyright 1991, 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE LSYS(FILE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LSYSCOM.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4  BET_LENGTH, LANG
	PARAMETER (BET_LENGTH=LMXMARK)
C
	INTEGER*4 SYSNR
	INTEGER*4 FDB(7),FILE(5)
	INTEGER*4 SHARE(LMXMARK),BONUS_SHARE(LMXMARK)
	INTEGER*4 BET_MASK(BET_LENGTH)
	INTEGER*4 BET_DEF(LMXMARK)
	INTEGER*4  EDBET, WINMSK, TIMES, MATCH, BONUS_OFF, BONUS
	INTEGER*4 SYSTEM, BONUSBET, FROM, CHOSE, COUNT, OFF, I
	INTEGER*4 MRK, BET, BASE_BETS, POINTER, MARKS, NBET, ATR, GAME
	INTEGER*4 ENDSYS, STARTSYS, STATUS, CMD, ST
	INTEGER*4 MAXFULSYS, ESYSNR, K, SMARKS, EMARKS,NUMSYS,BEGSYS
	INTEGER*4 LIMIT, FLAG
C
        INTEGER*4 SPANISH  /2/
C
	LANG=1
C
C OPEN LOTTO SYSTEM FILE
C
5	CONTINUE
	CALL OPENQW(3,FILE,4,0,0,ST)
	IF(ST.NE.0) THEN
	  CALL FILERR(FILE,1,ST,0)
	  CALL GPAUSE
	  GOTO 5
	ENDIF
C
	CALL IOQINIT(FDB,3,1*256)
	CALL READQIO(FDB,1,LSYS_ATR,LSYS_COMMON_LENGTH*4,ST)
	IF (ST.NE.0) THEN
	   CALL FILERR(FILE,2,ST,1)
	   CALL CLOSEQFIL(FDB)
	   RETURN
	ENDIF
C
10	CONTINUE
	CALL CLRSCR(5)
	IF(LANG.EQ.SPANISH) THEN
	    TYPE *,' **** OPCIONES PARA ARCHIVOS DE LOTO ****'
	    TYPE *,' 1 - To display system bet'
	    TYPE *,' 2 - Analyze distribution of winnings'
       TYPE *,' 3 - To add system bet, to modify a bet, delete it first'
	    TYPE *,'                         and then reenter it'
	    TYPE *,' 4 - To add a game, you can always reenter new game'
	    TYPE *,'                    instead for old one'
	    TYPE *,' 5 - Save image in the file'
	    TYPE *,' 6 - To display LSYS_GAMSHR(*,I1,I2,I3,I4) '
	    TYPE *,' 7 - To call lsyschk n times'
	    TYPE *,' 8 - To delete system bet'
	    TYPE *,' 9 - Edit boards in reduced system bet'
	    TYPE *,'10 - Auto define full system bets'
	    CALL INPNUM('Ingrese comando: ',CMD,1,10,ST)
	ELSE
	    TYPE *,' **** LOTTO SYSTEM FILE DEFINITION OPTIONS ****'
	    TYPE *,' 1 - To display system bet'
	    TYPE *,' 2 - Analyze distribution of winnings'
      TYPE *,' 3 - To add system bet, to modify a bet, delete it first'
	    TYPE *,'                         and then reenter it'
	    TYPE *,' 4 - To add a game, you can always reenter new game'
	    TYPE *,'                    instead for old one'
	    TYPE *,' 5 - Save image in the file'
	    TYPE *,' 6 - To display LSYS_GAMSHR(*,I1,I2,I3,I4) '
	    TYPE *,' 7 - To call lsyschk n times'
	    TYPE *,' 8 - To delete system bet'
	    TYPE *,' 9 - Edit boards in reduced system bet'
	    TYPE *,'10 - Auto define full system bets'
	    CALL INPNUM('Enter command: ',CMD,1,10,ST)
	ENDIF
	IF (ST.LT.0) THEN
	  CALL CLOSEQFIL(FDB)
	  RETURN
	ENDIF
	GOTO (100,200,300,400,500,600,700,800,900,1000) CMD
	TYPE *,'Invalid'
	GOTO 10
C
C     DISPLAY BET
C
100	CONTINUE
	IF(LANG.EQ.SPANISH) THEN
	    CALL INPNUM('Ingrese nr de sistema [0 - tudo] ',
     *			    SYSNR,0,LSYSMAX,ST)
	ELSE
	    CALL INPNUM('Enter system nr [0 - all] ',SYSNR,0,LSYSMAX,ST)
	ENDIF
	IF (ST.LT.0) GOTO 10
	IF (SYSNR.NE.0) THEN
	   IF (LSYS_GAME(SYSNR).NE.0) THEN
	     CALL LSYSDSP(SYSNR,STATUS)
	     IF(LANG.EQ.SPANISH) THEN
	       TYPE *,'Quieres continuar? [Y/N] :'
	     ELSE
	       TYPE *,'Do you want to continue? [Y/N] :'
	     ENDIF
	     CALL YESNO(FLAG)
	     IF(FLAG.EQ.1) GOTO 100
	   ELSE
	     TYPE *,'System number does not exist'
	     CALL XWAIT(2,2,STATUS)
	   ENDIF
 	ELSE
	   DO 110, SYSNR=1,LSYSMAX
	      IF (LSYS_GAME(SYSNR).NE.0) THEN
		CALL LSYSDSP(SYSNR,STATUS)
		IF(LANG.EQ.SPANISH) THEN
		    TYPE *,'Quieres continuar? [Y/N] :'
		ELSE
		    TYPE *,'Do you want to continue? [Y/N] :'
		ENDIF
		CALL YESNO(FLAG)
		IF(FLAG.NE.1) GOTO 10
              ENDIF
110	   CONTINUE
	ENDIF
	GOTO 10
C
C     ANALYZE BET
C
200	CONTINUE
	IF(LANG.EQ.SPANISH) THEN
	    CALL INPNUM('Ingrese PRIMO numero de sistema (0=tudo) ',
     *		  STARTSYS,0,LSYSMAX,ST)
	ELSE
	    CALL INPNUM('Enter FIRST system number (0=all) ',
     *		  STARTSYS,0,LSYSMAX,ST)
	ENDIF
	IF (ST.LT.0) GOTO 10
	IF(STARTSYS.EQ.0) THEN
	   STARTSYS=1
	   ENDSYS=LSYSMAX
	ELSE
	   IF(LANG.EQ.SPANISH) THEN
	       CALL INPNUM('Ingrese ULTIMO numero de sistema ',
     *			  ENDSYS,STARTSYS,LSYSMAX,ST)
	   ELSE
	       CALL INPNUM('Enter LAST system number ',ENDSYS,STARTSYS,
     *	       LSYSMAX,ST)
	   ENDIF
	   IF(ST.LT.0) GOTO 10
	ENDIF
C
	DO 210, SYSNR=STARTSYS,ENDSYS
	   IF (LSYS_GAME(SYSNR).NE.0) CALL LSYSANL(SYSNR)
210	CONTINUE
	GOTO 10
C
C     ENTER SYSTEM BET
C
300	CONTINUE
	IF(LANG.EQ.SPANISH) THEN
	    CALL INPNUM('Ingrese # del sistema: ',SYSNR,1,LSYSMAX,ST)
	ELSE
	    CALL INPNUM('Enter system #: ',SYSNR,1,LSYSMAX,ST)
	ENDIF
	IF (ST.LT.0) GOTO 10
C
	IF (LSYS_GAME(SYSNR).GT.0) GOTO 10
C
	IF(LANG.EQ.SPANISH) THEN
	    CALL INPNUM('Ingrese # del juego : ',GAME,1,LSYS_MAXGAM,ST)
	    IF (ST.LT.0) GOTO 10
	    TYPE *, ' Tipo completo de sitema:',LSYS_FULL,
     *		    ' reduzido:',LSYS_REDUCED
	    CALL INPNUM('Ingrese atributo: ',ATR,LSYS_FULL,
     *			LSYS_REDUCED,ST)
	    IF (ST.LT.0) GOTO 10
	    IF (ATR.EQ.LSYS_FULL) THEN
		NBET=1
	    ELSE
		CALL INPNUM('Ingrese # de combinaciones:',NBET,1,999,ST)
		IF (ST.LT.0) GOTO 10
	    ENDIF
	    CALL INPNUM('Ingrese  # de marcas: ',MARKS,1,LMXMARK,ST)
	    IF (ST.LT.0) GOTO 10
	ELSE
	    CALL INPNUM('Enter game index : ',GAME,1,LSYS_MAXGAM,ST)
	    IF (ST.LT.0) GOTO 10
	    TYPE *, ' Full system type:',LSYS_FULL,
     *		    ' reduced:',LSYS_REDUCED
	    CALL INPNUM('Enter atribute: ',ATR,LSYS_FULL,LSYS_REDUCED,ST)
	    IF (ST.LT.0) GOTO 10
	    IF (ATR.EQ.LSYS_FULL) THEN
		NBET=1
	    ELSE
		CALL INPNUM('Enter # of bets:',NBET,1,999,ST)
		IF (ST.LT.0) GOTO 10
	    ENDIF
	    CALL INPNUM('Enter # of marks: ',MARKS,1,LMXMARK,ST)
	    IF (ST.LT.0) GOTO 10
	ENDIF
	POINTER=LSYS_FREEPTR+1
C
	IF (ATR.NE.LSYS_FULL) THEN
	    IF(LANG.EQ.SPANISH) THEN
		CALL INPNUM('Ingrese # de combinaciones simples:',
     *	                BASE_BETS,1,10000,ST)
	    ELSE
		CALL INPNUM('Enter # of corresponding simple bets:',
     *	                BASE_BETS,1,10000,ST)
	   ENDIF
	   IF (ST.LT.0) GOTO 10
	   DO 350, BET=1,NBET
	     IF(LANG.EQ.SPANISH) THEN
		TYPE *,'Ingreso de combinacion ',BET
		CALL INPNUM('Ingresse # de marcas ',MRK,1,LMXMARK,ST)
	     ELSE
		TYPE *,'Entering bet ',BET
		CALL INPNUM('Enter # of marks ',MRK,1,LMXMARK,ST)
	     ENDIF
	     IF (ST.LT.0) GOTO 10
310	     CONTINUE
	     LSYS_TAB(POINTER+1)=0
	     IF(LANG.EQ.SPANISH) THEN
		TYPE *,'Ingrese marcas'
	     ELSE
		TYPE *,'Enter marks'
	     ENDIF
	     ACCEPT *,(BET_DEF(I),I=1,MRK)
	     DO 320, OFF=1,MRK
	       IF (BET_DEF(OFF).GT.LMXMARK.OR.BET_DEF(OFF).LE.0.OR.
     *	           BET_DEF(OFF).GT.MARKS) THEN
	         TYPE *,'Invalid bet'
	         GOTO 310
	       ENDIF
	       CALL BSET(LSYS_TAB(POINTER+1),BET_DEF(OFF)-1)
320	     CONTINUE
C
	     CALL BITCNT(LSYS_TAB(POINTER+1),L_SYSBYTES,COUNT)
	     IF (COUNT.NE.MRK) THEN
	       IF(LANG.EQ.SPANISH) THEN
		    TYPE *,'Definicion de la combinacion invalida'
	       ELSE
		    TYPE *,'Invalid bet definition, please reenter bet '
	       ENDIF
	       GOTO 310
	     ENDIF
C
	     LSYS_TAB(POINTER)=MRK
	     POINTER=POINTER+2
350	   CONTINUE
C
	ELSE
	   CALL SYSPRICE(LSYS_GAMCHOSE(GAME),LSYS_GAMFROM(GAME),MARKS,
     *	                 BASE_BETS)
	   LSYS_TAB(POINTER)=MARKS
	   DO 360, OFF=0,MARKS-1
	      CALL BSET(LSYS_TAB(POINTER+1),OFF)
360	   CONTINUE
	   POINTER=POINTER+2
	ENDIF
C
	LSYS_PTR(SYSNR)=LSYS_FREEPTR+1
	LSYS_FREEPTR=POINTER-1
	LSYS_NUMBET(SYSNR)=NBET
	LSYS_NUMMRK(SYSNR)=MARKS
	LSYS_GAME(SYSNR)=GAME
	LSYS_ATR(SYSNR)=ATR
	LSYS_BOARD(SYSNR)=BASE_BETS
        GOTO 10
C
C     GAME INFO
C
400	CONTINUE
	IF(LANG.EQ.SPANISH) THEN
	    CALL INPNUM('Ingrese # del juego ',GAME,1,LSYS_MAXGAM,ST)
	    IF (ST.LT.0) GOTO 10
	    CALL INPNUM('Ingrese numero de numeros en la combinacion '
     *			,CHOSE,1,LMXMARK,ST)
	    IF (ST.LT.0) GOTO 10
	    LSYS_GAMCHOSE(GAME)=CHOSE
	    CALL INPNUM('Ingrese major numero de conbinacion ',
     *			  FROM,1,100,ST)
	    IF (ST.LT.0) GOTO 10
	    LSYS_GAMFROM(GAME)=FROM
	    CALL INPNUM('Ingrese # de numeros de bonus ',BONUSBET,0,
     *			  LMXBONUS,ST)
	    IF (ST.LT.0) GOTO 10
	    LSYS_BONUSBET(GAME)=BONUSBET
	    CALL FASTSET(0,LSYS_GAMDIV(0,GAME),LMXSHR+1)
	    CALL FASTSET(0,LSYS_GAMBON(0,GAME),LMXSHR+1)
	    TYPE *,'Enter match with no bonus divisions '
	    ACCEPT *,(LSYS_GAMDIV(I,GAME),I=0,CHOSE)
C
	    IF (BONUSBET.NE.0) THEN
		TYPE *,'Ingrese divisiones de bonus '
		ACCEPT *,(LSYS_GAMBON(I,GAME),I=0,CHOSE)
	    ENDIF
	ELSE
	    CALL INPNUM('Enter game # ',GAME,1,LSYS_MAXGAM,ST)
	    IF (ST.LT.0) GOTO 10
	    CALL INPNUM('Enter number of numbers in base bet ',CHOSE,
     *	             1,LMXMARK,ST)
	    IF (ST.LT.0) GOTO 10
	    LSYS_GAMCHOSE(GAME)=CHOSE
	    CALL INPNUM('Enter highest number bet ',FROM,1,100,ST)
	    IF (ST.LT.0) GOTO 10
	    LSYS_GAMFROM(GAME)=FROM
	    CALL INPNUM('Enter # of bonus numbers ',BONUSBET,0,
     *			  LMXBONUS,ST)
	    IF (ST.LT.0) GOTO 10
	    LSYS_BONUSBET(GAME)=BONUSBET
	    CALL FASTSET(0,LSYS_GAMDIV(0,GAME),LMXSHR+1)
	    CALL FASTSET(0,LSYS_GAMBON(0,GAME),LMXSHR+1)
	    TYPE *,'Enter match with no bonus divisions '
	    ACCEPT *,(LSYS_GAMDIV(I,GAME),I=0,CHOSE)
C
	    IF (BONUSBET.NE.0) THEN
		TYPE *,'Enter bonus divisions '
		ACCEPT *,(LSYS_GAMBON(I,GAME),I=0,CHOSE)
	    ENDIF
	ENDIF
C
C     GENERATE SHARES TABLE
C
	MAXFULSYS=MIN(LMXFULLMARK,FROM)
	DO 490, SYSTEM=1,MAXFULSYS
	   DO 480, BONUS=0,BONUSBET
	      BONUS_OFF=2*BONUS
	      DO 470, MATCH=0,CHOSE
	         CALL SYSDIV(CHOSE,FROM,SYSTEM,MATCH,BONUS,BONUSBET,
     *	 LSYS_GAMBON(0,GAME),LSYS_GAMSHR(0,MATCH,BONUS_OFF,SYSTEM,GAME)
     *	       ,LSYS_GAMSHR(0,MATCH,BONUS_OFF+1,SYSTEM,GAME))
C
	         DO 450, OFF=0,CHOSE
	            LSYS_GAMSHR(OFF,MATCH,BONUS_OFF,SYSTEM,GAME)=
     *	            LSYS_GAMSHR(OFF,MATCH,BONUS_OFF,SYSTEM,GAME)
     *	           *LSYS_GAMDIV(OFF,GAME)
450	         CONTINUE
	         IF (BONUS.NE.0.OR.SYSTEM.LT.CHOSE) THEN
	         DO 460, OFF=0,CHOSE
	            LSYS_GAMSHR(OFF,MATCH,BONUS_OFF+1,SYSTEM,GAME)=
     *	            LSYS_GAMSHR(OFF,MATCH,BONUS_OFF+1,SYSTEM,GAME)
     *	           *LSYS_GAMBON(OFF,GAME)
460	         CONTINUE
	         ENDIF
C***  TYPE 465,SYSTEM,MATCH,BONUS,(LSYS_GAMSHR(I,MATCH,BONUS,SYSTEM
C*** *                                        ,GAME),I=0,CHOSE)
C***465   FORMAT(1H ,'System ',I2,' match ',I2,' b ',I1,' shares '
C*** *   ,10I5)
C
470	      CONTINUE
480	   CONTINUE
490	CONTINUE
        GOTO 10
C
C     UPDATE LSYSCHK.FIL
C
500	CONTINUE
	CALL WRITEQIO(FDB,1,LSYS_ATR,LSYS_COMMON_LENGTH*4,ST)
	GOTO 10
C
C     DISPLAY LSYS_GAMSHR TABLE
C
600	CONTINUE
	IF(LANG.EQ.SPANISH) THEN
	    TYPE *,'Pantaja de LSYS_GAMSHR(*,I1,I2,I3,I4)'
	    CALL INPNUM(' Ingrese I1 (match) ',MATCH,0,LMXSHR,ST)
	    IF (ST.LT.0) GOTO 10
	    CALL INPNUM(' Ingrese I2 (bonus off) ',BONUS,0,
     *			LMXBONUS*2+1,ST)
	    IF (ST.LT.0) GOTO 10
	    CALL INPNUM(' Ingrese I3 (system)',SYSTEM,0,LMXMARK,ST)
	    IF (ST.LT.0) GOTO 10
	    CALL INPNUM(' Ingrese I4 (game)  ',GAME,1,LSYS_MAXGAM,ST)
	    IF (ST.LT.0) GOTO 10
	    TYPE *,(LSYS_GAMSHR(I,MATCH,BONUS,SYSTEM,GAME),I=0,LMXSHR)
	ELSE
	    TYPE *,'Displaying LSYS_GAMSHR(*,I1,I2,I3,I4)'
	    CALL INPNUM(' Enter I1 (match) ',MATCH,0,LMXSHR,ST)
	    IF (ST.LT.0) GOTO 10
	    CALL INPNUM(' Enter I2 (bonus off) ',BONUS,0,
     *			    LMXBONUS*2+1,ST)
	    IF (ST.LT.0) GOTO 10
	    CALL INPNUM(' Enter I3 (system)',SYSTEM,0,LMXMARK,ST)
	    IF (ST.LT.0) GOTO 10
	    CALL INPNUM(' Enter I4 (game)  ',GAME,1,LSYS_MAXGAM,ST)
	    IF (ST.LT.0) GOTO 10
	    TYPE *,(LSYS_GAMSHR(I,MATCH,BONUS,SYSTEM,GAME),I=0,LMXSHR)
	ENDIF
	GOTO 10
C
C     PERFORM TIMING BENCHMARK (CALL LSYSCHK N TIMES)
C
700	CONTINUE
	IF(LANG.EQ.SPANISH) THEN
	    CALL INPNUM('hamar cuantas veces ',TIMES,1,100000,ST)
	    IF (ST.LT.0) GOTO 10
	    CALL INPNUM('Ingrese sysnr ',SYSNR,1,LSYSMAX,ST)
	ELSE
	    CALL INPNUM('How many times to call ',TIMES,1,100000,ST)
	    IF (ST.LT.0) GOTO 10
	    CALL INPNUM('Enter sysnr ',SYSNR,1,LSYSMAX,ST)
	ENDIF
	IF (ST.LT.0) GOTO 10
C
	IF (LSYS_GAME(SYSNR).LE.0) GOTO 10
	CALL FASTSET(0,BET_MASK,BET_LENGTH)
C
C
C     SET THE BET_MASK
C
	GAME=LSYS_GAME(SYSNR)
	MARKS=LSYS_NUMMRK(SYSNR)
	CHOSE=LSYS_GAMCHOSE(GAME)
C
	DO 710, OFF=1,MARKS
	   CALL SETNIBLE(1,BET_MASK,OFF)
710	CONTINUE
C
	IF(LANG.EQ.SPANISH) THEN
	    TYPE *,'Ingrese mascara vencedora en hex'
	    ACCEPT 715,WINMSK
	    CALL INPNUM('Ingrese # del bonus ',BONUS,-1,100,ST)
	    IF (ST.LT.0) GOTO 10
C
	    TYPE*,'toma contabilidad ahora'
	ELSE
	    TYPE *,'Enter winning mask in hex'
	    ACCEPT 715,WINMSK
	    CALL INPNUM('Enter bonus # ',BONUS,-1,100,ST)
	    IF (ST.LT.0) GOTO 10
C
	    TYPE*,'take accounting now'
	ENDIF
715	FORMAT(Z)
	CALL GPAUSE
C
	DO 720, OFF=1,TIMES
	   CALL LSYSCHK(BET_MASK,SYSNR,WINMSK,BONUS,SHARE,BONUS_SHARE)
720	CONTINUE
C
	IF(LANG.EQ.SPANISH) THEN
	    TYPE*,' ola, acorda'
	ELSE
	    TYPE*,' hello, wake up'
	ENDIF
	CALL GPAUSE
	GOTO 10
C
C     DELETE SYSTEM BET, DELETE IS NOT VERY SOFISTICATED, SHOULD
C     SHRINK LSYS_TAB
C
800	CONTINUE                 !DELETE SYSTEM BET
	CALL INPNUM('Enter system # to delete ',SYSNR,1,LSYSMAX,ST)
	IF (ST.LT.0) GOTO 10
	LSYS_PTR(SYSNR)=0
	LSYS_NUMBET(SYSNR)=0
	LSYS_NUMMRK(SYSNR)=0
	LSYS_GAME(SYSNR)=0
	LSYS_ATR(SYSNR)=0
	LSYS_BOARD(SYSNR)=0
	GOTO 10
C
C EDIT SYSTEM BETS
C
900	CONTINUE
	IF(LANG.EQ.SPANISH) THEN
	    CALL INPNUM('Ingrese # del sistema: ',SYSNR,1,LSYSMAX,ST)
	    IF (ST.LT.0) GOTO 10
	    IF(LSYS_GAME(SYSNR).LE.0) THEN
		TYPE*,' Sistema no definido '
		GOTO 10
	    ENDIF
	    GAME=LSYS_GAME(SYSNR)
	    ATR=LSYS_ATR(SYSNR)
	    IF(ATR.EQ.LSYS_FULL) THEN
		TYPE*,' Usted tien que reingresar toda la combinacion'
		GOTO 10
	    ENDIF
	ELSE
	    CALL INPNUM('Enter system #: ',SYSNR,1,LSYSMAX,ST)
	    IF (ST.LT.0) GOTO 10
	    IF(LSYS_GAME(SYSNR).LE.0) THEN
		TYPE*,' System not defined '
		GOTO 10
	    ENDIF
	    GAME=LSYS_GAME(SYSNR)
	    ATR=LSYS_ATR(SYSNR)
	    IF(ATR.EQ.LSYS_FULL) THEN
		TYPE*,' You must reenter full system bets '
		GOTO 10
	    ENDIF
	ENDIF
	MARKS=LSYS_NUMMRK(SYSNR)
	NBET=LSYS_NUMBET(SYSNR)
C
C
910	CONTINUE
	IF(LANG.EQ.SPANISH) THEN
	    CALL INPNUM('Ingrese # de la combinacion que editar(E-exit)'
     *			    ,EDBET,1,NBET,ST)
	    IF(ST.LT.0) GOTO 10
	    POINTER=LSYS_PTR(SYSNR)+(EDBET-1)*2
C
	    TYPE *,'Entering bet ',EDBET
	    CALL INPNUM('Ingrese # de impressiones',MRK,1,MARKS,ST)
	    IF (ST.LT.0) GOTO 10
	ELSE
	    CALL INPNUM('Enter bet # to edit (E-exit) ',EDBET,1,NBET,ST)
	    IF(ST.LT.0) GOTO 10
	    POINTER=LSYS_PTR(SYSNR)+(EDBET-1)*2
C
	    TYPE *,'Entering bet ',EDBET
	    CALL INPNUM('Enter # of marks ',MRK,1,MARKS,ST)
	    IF (ST.LT.0) GOTO 10
	ENDIF
	LSYS_TAB(POINTER)=MRK
C
920	   CONTINUE
	   IF(LANG.EQ.SPANISH) THEN
	    TYPE *,'Ingrese impresion'
	   ELSE
	    TYPE *,'Enter marks'
	   ENDIF
	   ACCEPT *,(BET_DEF(I),I=1,MRK)
	   LSYS_TAB(POINTER+1)=0
	   DO 930, OFF=1,MRK
	     IF (BET_DEF(OFF).GT.LMXMARK.OR.BET_DEF(OFF).LE.0.OR.
     *	         BET_DEF(OFF).GT.MARKS) THEN
	       IF(LANG.EQ.SPANISH) THEN
		TYPE *,'Combinacion invalida'
	       ELSE
		TYPE *,'Invalid bet'
	       ENDIF
	       GOTO 920
	     ENDIF
	     CALL BSET(LSYS_TAB(POINTER+1),BET_DEF(OFF)-1)
930	   CONTINUE
C
	   CALL BITCNT(LSYS_TAB(POINTER+1),L_SYSBYTES,COUNT)
	    IF (COUNT.NE.MRK) THEN
	      IF(LANG.EQ.SPANISH) THEN
	        TYPE *,'Definiciaon de combinacion invalida'
	      ELSE
	        TYPE *,'Invalid bet definition, please reenter bet '
	      ENDIF
	      GOTO 920
	   ENDIF
	GOTO 910
C
C auto define full system bets
C
1000    CONTINUE
	IF(LANG.EQ.SPANISH) THEN
      	  CALL INPNUM('Ingrese numero del juego: ',GAME,1,LSYS_MAXGAM,ST)
      	  IF (ST.LT.0) GOTO 10
	  IF(LSYS_GAMFROM(GAME).EQ.0) THEN
	    TYPE*,'Juego no definido '
	    GOTO 1000
	  ENDIF
	  FROM=LSYS_GAMFROM(GAME)
	  CHOSE=LSYS_GAMCHOSE(GAME)
 	  LIMIT=MIN(FROM,LMXMARK)
	  CALL INPNUM('Ingrese numero de sistema de partida ',
     *			BEGSYS,1,LSYSMAX,ST)
	  IF(ST.NE.0) GOTO 1000
	  CALL INPNUM('Ingrese numeros de sistema seleccionados ',
     *			SMARKS,1,LIMIT,ST)
	  IF(ST.NE.0) GOTO 1000
	  CALL INPNUM('Ingrese numeros de terminacion seleccionados ',
     *			EMARKS,SMARKS,LIMIT,ST)
	  IF(ST.NE.0) GOTO 1000
	ELSE
      	  CALL INPNUM('Enter game number: ',GAME,1,LSYS_MAXGAM,ST)
      	  IF (ST.LT.0) GOTO 10
	  IF(LSYS_GAMFROM(GAME).EQ.0) THEN
	    TYPE*,'Game not defined '
	    GOTO 1000
	  ENDIF
	  FROM=LSYS_GAMFROM(GAME)
	  CHOSE=LSYS_GAMCHOSE(GAME)
 	  LIMIT=MIN(FROM,LMXMARK)
	  CALL INPNUM('Enter starting system number ',BEGSYS,
     *			1,LSYSMAX,ST)
	  IF(ST.NE.0) GOTO 1000
	  CALL INPNUM('Enter starting numbers picked ',SMARKS,
     *			1,LIMIT,ST)
	  IF(ST.NE.0) GOTO 1000
	  CALL INPNUM('Enter ending numbers picked ',EMARKS,
     *			SMARKS,LIMIT,ST)
	  IF(ST.NE.0) GOTO 1000
	ENDIF
C
C CHECK IF ENOUGH FREE SYSTEMS AVAILABLE
C
	NUMSYS=EMARKS-SMARKS+1
	DO 1010 SYSNR=BEGSYS,BEGSYS+NUMSYS-1
	IF(LANG.EQ.SPANISH) THEN
	  IF(SYSNR.GT.LSYSMAX) THEN
	    TYPE*,' No hay espacio en la tabela del sistema'
	    GOTO 10
	  ENDIF
	  IF(LSYS_GAME(SYSNR).NE.0) THEN
	    TYPE*,'sistema ',SYSNR,' ya assignalado'
	    GOTO 1000
	  ENDIF
	ELSE
	  IF(SYSNR.GT.LSYSMAX) THEN
	    TYPE*,'Sorry not enough room in system table'
	    GOTO 10
	  ENDIF
	  IF(LSYS_GAME(SYSNR).NE.0) THEN
	    TYPE*,'Sorry system ',SYSNR,' already assigned'
	    GOTO 1000
	  ENDIF
	ENDIF
1010	CONTINUE
C
C SHOW SYSTEM ASSIGNMENTS
C
	MARKS=SMARKS
	DO 1020 SYSNR=BEGSYS,BEGSYS+NUMSYS-1
	  WRITE(5,960) SYSNR,CHOSE,FROM,MARKS
960       FORMAT(1X,'SYSTEM ',I2,' DEFINED AS ',I2,'/',I2,' PICK ',I2)
	  MARKS=MARKS+1
1020	CONTINUE
	CALL WIMG(5,'Is this correct [Y/N] ')
	CALL YESNO(ST)
	IF(ST.NE.1) GOTO 1000
C
C DEFINE SYSTEMS
C
        ATR=LSYS_FULL
        NBET=1
	MARKS=SMARKS-1
	DO 1500 SYSNR=BEGSYS,BEGSYS+NUMSYS-1
	MARKS=MARKS+1
	TYPE*,'BUILDING SYSTEM ',SYSNR,' PICK ',MARKS
        POINTER=LSYS_FREEPTR+1
        CALL SYSPRICE(LSYS_GAMCHOSE(GAME),LSYS_GAMFROM(GAME),MARKS,
     *                BASE_BETS)
        LSYS_TAB(POINTER)=MARKS
        DO 1360, OFF=0,MARKS-1
          CALL BSET(LSYS_TAB(POINTER+1),OFF)
1360    CONTINUE
        POINTER=POINTER+2
C
        LSYS_PTR(SYSNR)=LSYS_FREEPTR+1
        LSYS_FREEPTR=POINTER-1
        LSYS_NUMBET(SYSNR)=NBET
        LSYS_NUMMRK(SYSNR)=MARKS
        LSYS_GAME(SYSNR)=GAME
        LSYS_ATR(SYSNR)=ATR
        LSYS_BOARD(SYSNR)=BASE_BETS
1500	CONTINUE
        GOTO 10
	END
