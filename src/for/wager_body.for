C WAGER_BODY.FOR
C
C V09 13-DEC-2010 HXK LOTTO 2 CHANGES - ADDED LUCKY NUMBER
C V08 30-NOV-2010 FJG TWEMSER/TWEMCHK replaced by TWLNKSER/TWLNKCHK
C V07 14-APR-2010 RXK Bet details for ePassive added.
C v06 03-apr-2009 TRG ONLY INCLUDE EM SERIAL FOR KICKER
c VO5 25-MAR-2009 LRS EM SERIAL FOR JOKER INCLUDED
C V04 15-JUN-2000 OXK CLEANUP W/ WARNINGS=ALL
C V03 01-FEB-2000 UXN TNFRAC added.
C V02 13-OCT-1999 RXK World Tour added.
C V01 08-JUL-1999 UXN Initial release.
C
C This subroutine builds output message for reprint of wagers, for reprint of
C exchanged tickets, for TEBE inquiry and for the fractions. 
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE WAGER_BODY(TRABUF,OUTTAB,IND)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:NBRCOM.DEF'
	INCLUDE 'INCLIB:TSLCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	BYTE      OUTTAB(*)             ! outgoing message to the terminal
	INTEGER*4 IND                   ! offset in OUTTAB
C
	INTEGER*4 OPTION
	INTEGER*4 KOFFBEG,KOFFEND
	INTEGER*4 I4TEMP
	BYTE      I1TEMP(4)
	EQUIVALENCE (I1TEMP,I4TEMP)
	INTEGER*4 AMT,ROW,SEL
        INTEGER*4 NUM_HOME_IND
        INTEGER*4 NUM_HOME,NUM_AWAY,TRAOFF
	INTEGER*4 OFFBEG,OFFEND,R,CHECK
	INTEGER*4 I,POOL,OFF
	INTEGER*4  N3POL(10)     !
        INTEGER*4  N4POL(16)     !
        INTEGER*4  N3MULT(10)    !
        INTEGER*4  N4MULT(16)    !

        DATA N3POL/1,1,1,17,18,18,19,19,16,16/
        DATA N4POL/1,1,1,17,18,18,18,18,19,19,19,19,16,16,16,16/
        DATA N3MULT/1,1,1,1,1,1,1,1,6,3/
        DATA N4MULT/1,1,1,1,1,1,1,1,1,1,1,1,24,12,6,4/

C
C
C 1RST PART - FROM HOST-->TERMINAL WAGER MESSAGE
C ----------------------------------------------
        CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),I4TEMP,CHECK)
        OUTTAB(IND+0)=I1TEMP(3)   !#7   External (wager) serial #
        OUTTAB(IND+1)=I1TEMP(2)   !#8
        OUTTAB(IND+2)=I1TEMP(1)   !#9
        IND=IND+3

        OUTTAB(IND)=CHECK                !#10
        IND=IND+1

        CALL GETOFF(TRABUF,OFFBEG,OFFEND)
        I4TEMP = OFFBEG
        OUTTAB(IND+0) = I1TEMP(2)        ! #11
        OUTTAB(IND+1) = I1TEMP(1)        ! #12

        I4TEMP = OFFEND
        OUTTAB(IND+2) = I1TEMP(2)        ! #13
        OUTTAB(IND+3) = I1TEMP(1)        ! #14
        IND=IND+4
C
C SET OFFSETS TO FIRST - SECOND WEEK / YEAR DATES
C
        CALL PUT_WEEK_YEAR_DRAWS(TRABUF, OUTTAB, IND)
C
C       GET TIME
C       --------
        CALL PUTIME(TRABUF(TTIM),OUTTAB,IND)
C
C GET OUTPUT MESSAGE OPTIONS
C
        CALL OGETOPT(TRABUF,OPTION)
        CALL KOFFGET(TRABUF,KOFFBEG,KOFFEND)
C
        IF(OFFBEG.EQ.KOFFBEG.AND.OFFEND.EQ.KOFFEND)   !Additional Kicker offsets
     *     OPTION = IAND(OPTION,'7F'X)                !are not needed.
C
        OUTTAB(IND+0) = OPTION
        IND = IND + 1
C
C       SET JOKER OFFSETS
C       -----------------
        IF(IAND(OPTION,'80'X).NE.0) THEN
            I4TEMP = KOFFBEG
            OUTTAB(IND) = I1TEMP(1)      !

            I4TEMP = KOFFEND
            OUTTAB(IND+1) = I1TEMP(1)    !
C
	    OUTTAB(IND+2) = TRABUF(TWKDUR)
            IND = IND + 3
        END IF

C       SET JOKER 1 NUMBER
C       ------------------
        IF(IAND(OPTION,'40'X).NE.0) THEN
              I4TEMP = TRABUF(TWKICK)
              OUTTAB(IND+0) = I1TEMP(4)
              OUTTAB(IND+1) = I1TEMP(3)
              OUTTAB(IND+2) = I1TEMP(2)
              OUTTAB(IND+3) = I1TEMP(1)
              IND=IND+4
        ENDIF

C       SET JOKER 2 NUMBER
C       ------------------
        IF(IAND(OPTION,'20'X).NE.0) THEN
              I4TEMP = TRABUF(TWKICK2)
              OUTTAB(IND+0) = I1TEMP(4)
              OUTTAB(IND+1) = I1TEMP(3)
              OUTTAB(IND+2) = I1TEMP(2)
              OUTTAB(IND+3) = I1TEMP(1)
              IND=IND+4
        ENDIF
 
C       SET NUMBER OF FRACTIONS
C       -----------------------
        IF(IAND(OPTION,'10'X).NE.0) THEN
	    OUTTAB(IND) = TRABUF(TNFRAC)
            IND=IND+1
        ENDIF
C
C SPORTS CANCELLATIONS EVENT BITMAP
C
        IF(IAND(OPTION,'02'X) .NE. 0 .AND. TRABUF(TGAMTYP) .EQ. TSPT) THEN
           I4TEMP          = TRABUF(TWCEBM)
           OUTTAB(IND + 0) = I1TEMP(4)  ! Data is comming in The High I4 Byte Part
           OUTTAB(IND + 1) = I1TEMP(3)
           IND = IND + 2
        ENDIF
C
C       SEND BINGO FULL HOUSE AND LUCKY NUMBER
C       ------------------------------------------------
        IF(TRABUF(TGAMTYP).EQ.TBNG) THEN

           DO I=1,BGONBB
              DO R=1,BGOCOL
                 I4TEMP = TRABUF(TWBBFH1+(I-1)*BGOROW+(R-1))
                 OUTTAB(IND+0) = I1TEMP(2)
                 OUTTAB(IND+1) = I1TEMP(1)
                 IND=IND+2
              ENDDO
           ENDDO

C           SET LUCKY NUMBER AND BASE
C           -------------------------
            I4TEMP = TRABUF(TWBBAS)
            OUTTAB(IND+0) = I1TEMP(2)
            OUTTAB(IND+1) = I1TEMP(1)
            IND=IND+2
            I4TEMP = TRABUF(TWBLUK)
            OUTTAB(IND+0) = I1TEMP(4)
            OUTTAB(IND+1) = I1TEMP(3)
            OUTTAB(IND+2) = I1TEMP(2)
            OUTTAB(IND+3) = I1TEMP(1)
            IND=IND+4

        ENDIF        
C
C 2ND PART - FROM TERMINAL-->HOST WAGER MESSAGE
C ---------------------------------------------

C	GET GAME INDEX & SYSTEM TYPE
C	-------------------------
	I4TEMP=ISHFT(TRABUF(TGAMIND),4)
        IF(TRABUF(TGAMTYP).EQ.TPAS) THEN
           I4TEMP=I4TEMP+TRABUF(TWEPOP) !game index & operation request
        ELSE   
           I4TEMP=I4TEMP+TRABUF(TWSYST) !game index & syst type
        ENDIF
	OUTTAB(IND)=I4TEMP
	IND=IND+1

C	GET DURATION & NUMBER OF BOARDS
C	-------------------------------
	I4TEMP=ISHFT(TRABUF(TWDUR),4)+TRABUF(TWNBET) !duration & number of boards
	OUTTAB(IND)=I4TEMP
	IND=IND+1

C	GET INPUT MESSAGE OPTIONS
C	-------------------------
	CALL IGETOPT(TRABUF,OPTION)

	I4TEMP = OPTION
	OUTTAB(IND)  =I1TEMP(2)
	OUTTAB(IND+1)=I1TEMP(1)

	IND = IND + 2

C       GET QUICK PICK FLAG 
C       -------------------
        IF(IAND(OPTION,'0200'X).NE.0) THEN   !QUICK PICK FLAGS
           I4TEMP=TRABUF(TWQPF)
           OUTTAB(IND+0)=I1TEMP(2)
           OUTTAB(IND+1)=I1TEMP(1)
           IND=IND+2
        ENDIF

C       GET SYSTEM NUMBER ( SPORTS / RESULTS GAME SHOULD BE NUMBER OF WAGERS ) 
C       ----------------------------------------------------------------------
        IF(IAND(OPTION,'0100'X).NE.0) THEN   !SYSTEM NUMBER
	  I4TEMP = TRABUF(TWSYSN)
          IF(TRABUF(TGAMTYP) .EQ. TSPT .OR. TRABUF(TGAMTYP) .EQ. TTGL) THEN
            I4TEMP = TRABUF(TWSIMP)
          ENDIF
          OUTTAB(IND+0)=I1TEMP(2)
          OUTTAB(IND+1)=I1TEMP(1)
          IND=IND+2
	ENDIF

C       GET BANK NUMBER & BANK ACCOUNT NUMBER
C       -------------------------------------
        IF(IAND(OPTION,'0008'X).NE.0) THEN   
	  I4TEMP = TRABUF(TWBNKID)
          OUTTAB(IND+0)=I1TEMP(4)
          OUTTAB(IND+1)=I1TEMP(3)
          OUTTAB(IND+2)=I1TEMP(4)
          OUTTAB(IND+3)=I1TEMP(1)
          IND=IND+4

	  I4TEMP = TRABUF(TWBNKNM)
          OUTTAB(IND+0)=I1TEMP(4)
          OUTTAB(IND+1)=I1TEMP(3)
          OUTTAB(IND+2)=I1TEMP(2)
          OUTTAB(IND+3)=I1TEMP(1)
          IND=IND+4
	ENDIF

C       GET LINKED WAGER SERIAL NUMBER
C       -------------------------------------
        
        IF(TRABUF(TGAMTYP) .EQ. TKIK) THEN    ! v06
         IF(IAND(OPTION,'0004'X).NE.0) THEN   
	   I4TEMP = TRABUF(TWLNKSER)
           OUTTAB(IND+0)=I1TEMP(3)
           OUTTAB(IND+1)=I1TEMP(2)
           OUTTAB(IND+2)=I1TEMP(1)
           IND=IND+3 

	   I4TEMP = TRABUF(TWLNKCHK)
           OUTTAB(IND+0)=I1TEMP(1)
           IND=IND+1
	 ENDIF
	ENDIF 

        IF(IAND(OPTION,'0001'X).NE.0) THEN   
          IND=IND+4
        ENDIF

C       SET MONDAY FLAG INDICATOR ( ONLY FOR LOTTO )
C       -------------------------
        IF(TRABUF(TGAMTYP) .EQ. TLTO) THEN
          I4TEMP = TRABUF(TWLMFI)
          OUTTAB(IND) = I1TEMP(1)
          IND = IND + 1
        ENDIF

C       SET LUCKY NUMBER ( ONLY FOR LOTTO )
C       -------------------------
        IF(TRABUF(TGAMTYP) .EQ. TLTO) THEN
          I4TEMP = TRABUF(TWLUCK)
          OUTTAB(IND) = I1TEMP(1)
          IND = IND + 1
        ENDIF

C 	LOTTO/SPORTS BET DETAILS
C	------------------------
	IF(TRABUF(TGAMTYP).EQ.TLTO.OR.
     *	   TRABUF(TGAMTYP).EQ.TSPT.OR.
     *     TRABUF(TGAMTYP).EQ.TTGL) THEN

	  DO I=TWBORD,TWBEND
	     IF(TRABUF(I).NE.0) THEN
	        CALL MOVBYT(TRABUF(I),1,OUTTAB,IND,4)
	        IND=IND+4
             ENDIF
	  ENDDO
	  GOTO 4000
	ENDIF

C 	EPASSIVE BET DETAILS
C	------------------------
	IF(TRABUF(TGAMTYP).EQ.TPAS) THEN
           IF(TRABUF(TWEPOP).EQ.EPASSAL) THEN
              OUTTAB(IND) = TRABUF(TWEPWK)
              IND = IND + 1
              OUTTAB(IND) = TRABUF(TWEPYR)
              IND = IND + 1
              I4TEMP = TRABUF(TWEPSN)
              OUTTAB(IND+0) = I1TEMP(4)
              OUTTAB(IND+1) = I1TEMP(3)
              OUTTAB(IND+2) = I1TEMP(2)
              OUTTAB(IND+3) = I1TEMP(1)
              IND=IND+4
              OUTTAB(IND) = ISHFT(TRABUF(TWEPSS),4) + TRABUF(TWEPSF)
              IND=IND+1
           ENDIF           
	   GOTO 4000
	ENDIF

C       NUMBERS BETS DETAILS
C       --------------------
	IF(TRABUF(TGAMTYP).EQ.TNBR) THEN
      	   DO 1040 I=0,TRABUF(TWNBET)-1
      	      POOL=TRABUF(TWNPOL1+I*3)
      	      AMT=TRABUF(TWNAMT1+I*3)/NBRPRC(TRABUF(TGAMIND))

      	      IF(TRABUF(TWNTYP).EQ.NB3TYP) THEN
      		AMT=AMT/N3MULT(POOL)
      		I4TEMP=N3POL(POOL)
      	      ELSE
      		AMT=AMT/N4MULT(POOL)
      		I4TEMP=N4POL(POOL)
      	      ENDIF

      	      OUTTAB(IND)=I4TEMP
      	      OUTTAB(IND+1)=AMT
      	      IND=IND+2

      	      CALL NUMBCD(TRABUF(TWNTYP),POOL,TRABUF(TWNNUM1+I*3),I4TEMP)
	      OUTTAB(IND+0)=I1TEMP(2)
	      OUTTAB(IND+1)=I1TEMP(1)
      	      IND=IND+2
 1040      CONTINUE

	  GOTO 4000
	ENDIF

C 	KICKER BETS DETAILS 
C	--------------------
	IF(TRABUF(TGAMTYP).EQ.TKIK) GOTO 4000
C
C 	SCORE GAME BETS DETAILS (ODDSETS  MATCH)
C	----------------------------------------
	IF(TRABUF(TGAMTYP).EQ.TSCR) THEN
	  I4TEMP = TRABUF(TWBEG)
	  OUTTAB(IND+0) = I1TEMP(2)
	  OUTTAB(IND+1) = I1TEMP(1)
	  IND = IND + 2

	  IF(TRABUF(TWSYST).EQ.0) THEN
	     DO 1100 I = 0,TRABUF(TWNBET) - 1
                OUTTAB(IND+0) = 1   ! # homes
                OUTTAB(IND+1) = 1   ! # aways
                IND = IND + 2
		OUTTAB(IND+0) = TRABUF(TWSSCR1+I*TWSBLEN)
		OUTTAB(IND+1) = TRABUF(TWSSCR2+I*TWSBLEN)
		IND = IND + 2
		I4TEMP = TRABUF(TWSAMT+I*TWSBLEN)
		IF(TRABUF(TFAMTFLG).EQ.1) I4TEMP = I4TEMP/TRABUF(TNFRAC)
		OUTTAB(IND+0) = I1TEMP(4)
		OUTTAB(IND+1) = I1TEMP(3)
		OUTTAB(IND+2) = I1TEMP(2)
		OUTTAB(IND+3) = I1TEMP(1)
		IND = IND + 4
1100	     CONTINUE
	  ELSE

             NUM_HOME_IND = IND

             IND = IND + 2

             NUM_HOME = 0
             DO 1112 I = 0,TRABUF(TWNBET)-1
                IF(TRABUF(TWSSCR1+I*TWSBLEN).EQ.'FF'X) GOTO 1112
                OUTTAB(IND+0) = TRABUF(TWSSCR1+I*TWSBLEN)
                NUM_HOME = NUM_HOME + 1
                IND = IND+1
1112         CONTINUE

             NUM_AWAY = 0
             DO 1114 I = 0,TRABUF(TWNBET)-1
                IF(TRABUF(TWSSCR2+I*TWSBLEN).EQ.'FF'X) GOTO 1114
                OUTTAB(IND+0) = TRABUF(TWSSCR2+I*TWSBLEN)
                NUM_AWAY = NUM_AWAY + 1
                IND = IND+1
1114         CONTINUE

             OUTTAB(NUM_HOME_IND+0) = NUM_HOME   ! # homes
             OUTTAB(NUM_HOME_IND+1) = NUM_AWAY   ! # aways
 
	     I4TEMP = TRABUF(TWSAMT)
	     IF(TRABUF(TFAMTFLG).EQ.1) I4TEMP = I4TEMP/TRABUF(TNFRAC)

	     OUTTAB(IND+0) = I1TEMP(4)
	     OUTTAB(IND+1) = I1TEMP(3)
	     OUTTAB(IND+2) = I1TEMP(2)
	     OUTTAB(IND+3) = I1TEMP(1)
	     IND = IND + 4
	  ENDIF	

	  GOTO 4000
	ENDIF

C 	WINNERS TIP BETS DETAILS (ODDSETS  TOPPEN)
C	------------------------------------------
	IF(TRABUF(TGAMTYP).EQ.TWIT) THEN
	  I4TEMP = TRABUF(TWBEG)				!Draw Number.
	  OUTTAB(IND+0) = I1TEMP(2)
	  OUTTAB(IND+1) = I1TEMP(1)
	  IND = IND + 2

	  DO 1200 I = 0,TRABUF(TWNBET) - 1
	    OUTTAB(IND+0) = TRABUF(TWWROW+I*TWWBLEN)	!Row# bet on.
	    IND = IND + 1
	    I4TEMP = TRABUF(TWWAMT+I*TWWBLEN)		!Amount bet.
	    IF(TRABUF(TFAMTFLG).EQ.1) I4TEMP = I4TEMP/TRABUF(TNFRAC)
	    OUTTAB(IND+0) = I1TEMP(4)
	    OUTTAB(IND+1) = I1TEMP(3)
	    OUTTAB(IND+2) = I1TEMP(2)
	    OUTTAB(IND+3) = I1TEMP(1)
	    IND = IND + 4
1200	  CONTINUE

	  GOTO 4000
	ENDIF

C 	TOTO SELECT BETS DETAILS (ODDSETS  LANGEN)
C	------------------------------------------
	IF(TRABUF(TGAMTYP).EQ.TTSL) THEN
	  DO 1300 I = 0,TRABUF(TWNBET) - 1
	    TRAOFF = (I*TWTBLEN*TWTRMAX) + (I*TWTHLEN)
	    I4TEMP = TRABUF(TWTAMT1+TRAOFF)		
	    IF(TRABUF(TFAMTFLG).EQ.1) I4TEMP = I4TEMP/TRABUF(TNFRAC)
	    OUTTAB(IND+0) = TRABUF(TWTSEL1+TRAOFF)	!# SELECTIONS
	    OUTTAB(IND+1) = I1TEMP(4)			!AMOUNT BET
	    OUTTAB(IND+2) = I1TEMP(3)			!
	    OUTTAB(IND+3) = I1TEMP(2)			!
	    OUTTAB(IND+4) = I1TEMP(1)			!
	    IND = IND + 5

	    DO 1310 R = 0,TRABUF(TWTSEL1+TRAOFF)-1
	      ROW = TRABUF(TWTROW1+TRAOFF+R*TWTBLEN)	!ROW BET
	      SEL = TRABUF(TWTPOL1+TRAOFF+R*TWTBLEN)	!SELECTION
	      SEL = ISHFT(ROW,2) + SEL 			!ROW/SELECTION
	      OUTTAB(IND) = SEL
	      IND = IND + 1
1310	    CONTINUE
1300	  CONTINUE	  

	  GOTO 4000
	ENDIF

C 	SUPER DOUBLE GAME BETS DETAILS (ODDSETS  MATCH)
C	-----------------------------------------------

	IF(TRABUF(TGAMTYP).EQ.TDBL) THEN
	  OUTTAB(IND+0) = TRABUF(TWDBCOUPID) ! COUPON ID
	  IND = IND + 1
	  I4TEMP = TRABUF(TWBEG)
	  OUTTAB(IND+0) = I1TEMP(2)
	  OUTTAB(IND+1) = I1TEMP(1)
	  IND = IND + 2

	  IF(TRABUF(TWSYST).EQ.0) THEN
	     DO 1400 I = 0,TRABUF(TWNBET) - 1
                OUTTAB(IND+0) = 1   ! # 1st
                OUTTAB(IND+1) = 1   ! # 2nd
                IND = IND + 2
		OUTTAB(IND+0) = TRABUF(TWDBROW1+I*TWDBBLEN)
		OUTTAB(IND+1) = TRABUF(TWDBROW2+I*TWDBBLEN)
		IND = IND + 2
		I4TEMP = TRABUF(TWDBAMT+I*TWDBBLEN) 
		IF(TRABUF(TFAMTFLG).EQ.1) I4TEMP = I4TEMP/TRABUF(TNFRAC)
		OUTTAB(IND+0) = I1TEMP(4)
		OUTTAB(IND+1) = I1TEMP(3)
		OUTTAB(IND+2) = I1TEMP(2)
		OUTTAB(IND+3) = I1TEMP(1)
		IND = IND + 4
1400	     CONTINUE
	  ELSE
             NUM_HOME_IND = IND
             IND = IND + 2
             NUM_HOME = 0
             DO 1412 I = 0,TRABUF(TWNBET)-1
                IF(TRABUF(TWDBROW1+I*TWDBBLEN).EQ.'FF'X) GOTO 1412
                OUTTAB(IND+0) = TRABUF(TWDBROW1+I*TWDBBLEN)
                NUM_HOME = NUM_HOME + 1
                IND = IND+1
1412         CONTINUE
             NUM_AWAY = 0
             DO 1414 I = 0,TRABUF(TWNBET)-1
                IF(TRABUF(TWDBROW2+I*TWDBBLEN).EQ.'FF'X) GOTO 1414
                OUTTAB(IND+0) = TRABUF(TWDBROW2+I*TWDBBLEN)
                NUM_AWAY = NUM_AWAY + 1
                IND = IND+1
1414         CONTINUE

             OUTTAB(NUM_HOME_IND+0) = NUM_HOME   ! # 1st
             OUTTAB(NUM_HOME_IND+1) = NUM_AWAY   ! # 2nd
 
	     I4TEMP = TRABUF(TWDBAMT)
	     IF(TRABUF(TFAMTFLG).EQ.1) I4TEMP = I4TEMP/TRABUF(TNFRAC)
	     OUTTAB(IND+0) = I1TEMP(4)
	     OUTTAB(IND+1) = I1TEMP(3)
	     OUTTAB(IND+2) = I1TEMP(2)
	     OUTTAB(IND+3) = I1TEMP(1)
	     IND = IND + 4
	  ENDIF	

	  GOTO 4000
	ENDIF


C 	TODAYS COUPLE GAME BETS DETAILS (ODDSETS  MATCH)
C	------------------------------------------------

	IF(TRABUF(TGAMTYP).EQ.TCPL) THEN
          OUTTAB(IND+0) = TRABUF(TWCPCOUPID)
	  IND = IND + 1
	  I4TEMP = TRABUF(TWBEG)
	  OUTTAB(IND+0) = I1TEMP(2)
	  OUTTAB(IND+1) = I1TEMP(1)
	  IND = IND + 2

	  IF(TRABUF(TWSYST).EQ.0) THEN
	     DO 1500 I = 0,TRABUF(TWNBET) - 1
                OUTTAB(IND+0) = 1   ! # event 1
                OUTTAB(IND+1) = 1   ! # event 2
                IND = IND + 2
		OUTTAB(IND+0) = TRABUF(TWCPROW1+I*TWCPBLEN)
		OUTTAB(IND+1) = TRABUF(TWCPROW2+I*TWCPBLEN)
		IND = IND + 2
		I4TEMP = TRABUF(TWCPAMT+I*TWCPBLEN)
		IF(TRABUF(TFAMTFLG).EQ.1) I4TEMP = I4TEMP/TRABUF(TNFRAC)
		OUTTAB(IND+0) = I1TEMP(4)
		OUTTAB(IND+1) = I1TEMP(3)
		OUTTAB(IND+2) = I1TEMP(2)
		OUTTAB(IND+3) = I1TEMP(1)
		IND = IND + 4
1500	     CONTINUE
	  ELSE
             NUM_HOME_IND = IND
             IND = IND + 2
             NUM_HOME = 0
             DO 1512 I = 0,TRABUF(TWNBET)-1
                IF(TRABUF(TWCPROW1+I*TWCPBLEN).EQ.'FF'X) GOTO 1512
                OUTTAB(IND+0) = TRABUF(TWCPROW1+I*TWCPBLEN)
                NUM_HOME = NUM_HOME + 1
                IND = IND+1
1512         CONTINUE
             NUM_AWAY = 0
             DO 1514 I = 0,TRABUF(TWNBET)-1
                IF(TRABUF(TWCPROW2+I*TWCPBLEN).EQ.'FF'X) GOTO 1514
                OUTTAB(IND+0) = TRABUF(TWCPROW2+I*TWCPBLEN)
                NUM_AWAY = NUM_AWAY + 1
                IND = IND+1
1514         CONTINUE

             OUTTAB(NUM_HOME_IND+0) = NUM_HOME   ! # homes
             OUTTAB(NUM_HOME_IND+1) = NUM_AWAY   ! # aways
 
	     I4TEMP = TRABUF(TWCPAMT) 
	     IF(TRABUF(TFAMTFLG).EQ.1) I4TEMP = I4TEMP/TRABUF(TNFRAC)
	     OUTTAB(IND+0) = I1TEMP(4)
	     OUTTAB(IND+1) = I1TEMP(3)
	     OUTTAB(IND+2) = I1TEMP(2)
	     OUTTAB(IND+3) = I1TEMP(1)
	     IND = IND + 4
	  ENDIF	

	  GOTO 4000
	ENDIF

C
C Super Score bet details.
C
	IF(TRABUF(TGAMTYP).EQ.TSSC) THEN
	  I4TEMP = TRABUF(TWBEG)
	  OUTTAB(IND+0) = I1TEMP(2)
	  OUTTAB(IND+1) = I1TEMP(1)
	  IND = IND + 2
	  OUTTAB(IND) = TRABUF(TWSSHM1)
	  IND = IND + 1
	  OUTTAB(IND) = TRABUF(TWSSAW1)
	  IND = IND + 1
	  OUTTAB(IND) = TRABUF(TWSSHM2)
	  IND = IND + 1
	  OUTTAB(IND) = TRABUF(TWSSAW2)
	  IND = IND + 1
	  OUTTAB(IND) = TRABUF(TWSSHM3)
	  IND = IND + 1
	  OUTTAB(IND) = TRABUF(TWSSAW3)
	  IND = IND + 1
C
	  OFF = 0
	  DO I = 0, TRABUF(TWSSHM1)-1
	    OUTTAB(IND+I) = TRABUF(TWSSBET+I+OFF)
	  ENDDO
C
	  IND = IND + TRABUF(TWSSHM1)
	  OFF = OFF + TRABUF(TWSSHM1)
C
	  DO I = 0, TRABUF(TWSSAW1)-1
	    OUTTAB(IND+I) = TRABUF(TWSSBET+I+OFF)
	  ENDDO
C
	  IND = IND + TRABUF(TWSSAW1)
	  OFF = OFF + TRABUF(TWSSAW1)
C
	  DO I = 0, TRABUF(TWSSHM2)-1
	    OUTTAB(IND+I) = TRABUF(TWSSBET+I+OFF)
	  ENDDO
C
	  IND = IND + TRABUF(TWSSHM2)
	  OFF = OFF + TRABUF(TWSSHM2)
C
	  DO I = 0, TRABUF(TWSSAW2)-1
	    OUTTAB(IND+I) = TRABUF(TWSSBET+I+OFF)
	  ENDDO
C
	  IND = IND + TRABUF(TWSSAW2)
	  OFF = OFF + TRABUF(TWSSAW2)
C
	  DO I = 0, TRABUF(TWSSHM3)-1
	    OUTTAB(IND+I) = TRABUF(TWSSBET+I+OFF)
	  ENDDO
C
	  IND = IND + TRABUF(TWSSHM3)
	  OFF = OFF + TRABUF(TWSSHM3)
C
	  DO I = 0, TRABUF(TWSSAW3)-1
	    OUTTAB(IND+I) = TRABUF(TWSSBET+I+OFF)
	  ENDDO
C
	  IND = IND + TRABUF(TWSSAW3)
C
	  I4TEMP = TRABUF(TWSSAMT)
	  IF(TRABUF(TFAMTFLG).EQ.1) I4TEMP = I4TEMP/TRABUF(TNFRAC)
C
	  OUTTAB(IND+0) = I1TEMP(4)
	  OUTTAB(IND+1) = I1TEMP(3)
	  OUTTAB(IND+2) = I1TEMP(2)
	  OUTTAB(IND+3) = I1TEMP(1)
	  IND = IND + 4
C
	  GOTO 4000
	ENDIF
C
C Todays Trio bet details...
C
	IF(TRABUF(TGAMTYP).EQ.TTRP) THEN
	  OUTTAB(IND+0) = TRABUF(TWTTCOUPID) !COUPON ID...
	  IND = IND + 1
	  I4TEMP = TRABUF(TWBEG)
	  OUTTAB(IND+0) = I1TEMP(2)
	  OUTTAB(IND+1) = I1TEMP(1)
	  IND = IND + 2
	  OUTTAB(IND) = TRABUF(TWTTMA)
	  IND = IND + 1
	  OUTTAB(IND) = TRABUF(TWTTMB)
	  IND = IND + 1
	  OUTTAB(IND) = TRABUF(TWTTMC)
	  IND = IND + 1
	  OFF = 0
	  DO I = 0, TRABUF(TWTTMA)-1
	    OUTTAB(IND+I) = TRABUF(TWTTBET+I+OFF)
	  ENDDO
C
	  IND = IND + TRABUF(TWTTMA)
	  OFF = OFF + TRABUF(TWTTMA)
C
	  DO I = 0, TRABUF(TWTTMB)-1
	    OUTTAB(IND+I) = TRABUF(TWTTBET+I+OFF)
	  ENDDO
C
	  IND = IND + TRABUF(TWTTMB)
	  OFF = OFF + TRABUF(TWTTMB)
C
	  DO I = 0, TRABUF(TWTTMC)-1
	    OUTTAB(IND+I) = TRABUF(TWTTBET+I+OFF)
	  ENDDO
C
	  IND = IND + TRABUF(TWTTMC)
C
	  I4TEMP = TRABUF(TWTTAMT)
	  IF(TRABUF(TFAMTFLG).EQ.1) I4TEMP = I4TEMP/TRABUF(TNFRAC)
	  OUTTAB(IND+0) = I1TEMP(4)
	  OUTTAB(IND+1) = I1TEMP(3)
	  OUTTAB(IND+2) = I1TEMP(2)
	  OUTTAB(IND+3) = I1TEMP(1)
	  IND = IND + 4
C
	  GOTO 4000
	ENDIF	  
C
C Super Triple bet details...
C
	IF(TRABUF(TGAMTYP).EQ.TSTR) THEN
	  OUTTAB(IND+0) = TRABUF(TWSTCOUPID) !COUPON ID...
	  IND = IND + 1
	  I4TEMP = TRABUF(TWBEG)
	  OUTTAB(IND+0) = I1TEMP(2)
	  OUTTAB(IND+1) = I1TEMP(1)
	  IND = IND + 2
	  OUTTAB(IND) = TRABUF(TWSTM1)
	  IND = IND + 1
	  OUTTAB(IND) = TRABUF(TWSTM2)
	  IND = IND + 1
	  OUTTAB(IND) = TRABUF(TWSTM3)
	  IND = IND + 1
	  OFF = 0
	  DO I = 0, TRABUF(TWSTM1)-1
	    OUTTAB(IND+I) = TRABUF(TWSTBET+I+OFF)
	  ENDDO
C
	  IND = IND + TRABUF(TWSTM1)
	  OFF = OFF + TRABUF(TWSTM1)
C
	  DO I = 0, TRABUF(TWSTM2)-1
	    OUTTAB(IND+I) = TRABUF(TWSTBET+I+OFF)
	  ENDDO
C
	  IND = IND + TRABUF(TWSTM2)
	  OFF = OFF + TRABUF(TWSTM2)
C
	  DO I = 0, TRABUF(TWSTM3)-1
	    OUTTAB(IND+I) = TRABUF(TWSTBET+I+OFF)
	  ENDDO
C
	  IND = IND + TRABUF(TWSTM3)
C
	  I4TEMP = TRABUF(TWSTAMT)
	  IF(TRABUF(TFAMTFLG).EQ.1) I4TEMP = I4TEMP/TRABUF(TNFRAC)
	  OUTTAB(IND+0) = I1TEMP(4)
	  OUTTAB(IND+1) = I1TEMP(3)
	  OUTTAB(IND+2) = I1TEMP(2)
	  OUTTAB(IND+3) = I1TEMP(1)
	  IND = IND + 4
C
	  GOTO 4000
	ENDIF
C
4000	CONTINUE
	RETURN
	END
