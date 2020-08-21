C
C SUBROUTINE SALINV
C
C SALINV.FOR
C
C V38 27-MAR-2017 HXK Omit Joker details if Joker removed > 90 days
C V37 02-JAN-2011 HXK ADD LOTTO3 AND LOTTO4 AS ONE GAME (TOTOLOTO)
C V36 14-SEP-2010 FJG ePassive project. BUG with AM and LN
C     15-SEP-2010 FJG Revert the online calculation.
C V35 27-APR-1999 UXN CLEANED UP VERSION. PARTIALLY REWRITTEN.
C V34 07-APR-1997 HXK Fix for unintended rounding of pennies for Online amounts
C V33 06-MAR-1997 RXK Send validation amount and commission with '-'sign 
C V32 27-FEB-1997 HXK Modified for instant games
C V31 18-FEB-1997 HXK Cleaned up hack for AGTXFR
C V30 07-FEB-1997 RXK Fix for instant part length
C V29 04-FEB-1997 RXK Fix for adjustment, commission and amtdue
C V28 16-DEC-1996 GPK Modified to print Instant Game Sales along with Online
C V27 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting, 
C                     Instant Pass Thru Phase 1
C V26 29-JAN-1996 HXK FFix for Couple game
C V25 23-NOV-1995 PXB Couple and Double games added
C V24 17-AUG-1995 HXK Fix for Ravi game MDS being set
C V23 13-DEC-1994 HXK Fix for Tulos, Voittaja
C V22 15-OCT-1994 HXK Adding /developing Bingo (15.Oct.94)
C V20 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V19 17-JUN-1994 HXK FIX WEEK BUG.
C V18 16-JUN-1994 HXK FIX FOR WEEK, YEAR.
C V17 16-JUN-1994 HXK KLUDGE FIX FOR WEEK, YEAR
C V16 21-MAY-1994 HXK USE DAYCDC INSTEAD OF INVOICE DATE AS DATE TO APPEAR ON 
C                     INVOICE REPORT.
C V15 07-FEB-1994 HXK SAME CHANGE AS 1.10, BUT WHICH DID NOT 'TAKE'!!!!
C V14 07-FEB-1994 HXK No change.
C V13 31-JAN-1994 HXK CHANGED TIME.
C V12 20-DEC-1993 HXK SUM TOTALS FOR TSCR, TWIT GAMES. (more to follow ....)
C V11 17-OCT-1993 HXK Type seg number problem.
C V10 10-OCT-1993 GXA Corrected Net sales amount calculation. 
C                     Removed Type statements.
C V09 30-SEP-1993 GXA Added Refunds to Validation totals.
C V08 25-SEP-1993 GXA Changed check for priviliged terminals, to get the 
C                     terminal # out of the message and check it accordingly.
C V07 06-SEP-1993 SXH For LOTGEN (still in test)
C V06 16-AUG-1993 SXH Debugged
C V05 21-JUN-1993 HXK CHANGED FOR FINLAND VAX CONVERSION
C V04 10-JUN-1993 HXK Changed AGTINF.DEF, AGTCOM.DEF includes
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 10-APR-1992 GCAN ADDED PAYMENTS TO ADJUSTMENT FIELD.
C V01 18-NOV-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C
C CALLING SEQUENCE:
C     CALL SALINV(TRABUF,MESTAB,OUTLEN)
C INPUT
C
C OUTPUT
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE SALINV(TRABUF,MESTAB,OUTLEN)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:SPECOM.DEF'

	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'

        ! arguments
        BYTE	   MESTAB(*)                 !

	INTEGER*2  OUTLEN                    !

        ! variables
	INTEGER*4  SALES(MAXGAM*MAXMLTD_SEL) ! 40 * 6
	INTEGER*4  MYCHKSUM                  !
	INTEGER*4  CHKLEN                    !
	INTEGER*4  IND                       !
	INTEGER*4  CLASS                     !
	INTEGER*4  SUBCLASS                  !
	INTEGER*4  TER                       !
	INTEGER*4  RTER                      !
	INTEGER*4  ST                        !
        INTEGER*4  I                         !
	INTEGER*4  BEGSALOFF                 !
	INTEGER*4  ENDSALOFF                 !
	INTEGER*4  ERRTYP                    !
	INTEGER*4  REPPNUM                   !
	INTEGER*4  MESS(EDLEN)               !
	INTEGER*4  REPTYP                    !
        INTEGER*4  SIND                      !
	INTEGER*4  GNUM                      !
	INTEGER*4  GTYP                      !
        INTEGER*4  GIND                      !
	INTEGER*4  TMP_GNUM                  ! used to add Lotto4 to Lotto3
        INTEGER*4  DEFINED_GAMES             !
        INTEGER*4  SALIND                    !
        INTEGER*4  SEGNO                     !
        INTEGER*4  NO_ROWS                   !
        INTEGER*4  LAST_SEG(NUMAGT)          !
        INTEGER*4  CDC                       !
        INTEGER*4  WEEK                      !
        INTEGER*4  YEAR                      !
	INTEGER*4  INDEX(MAXGAM)             ! SEND INFORMATION BY GAME
        INTEGER*4  AGT                       ! REPORT FROM AGENT NUMBER 
	INTEGER*4  TRANSP,CERR,STTRANSP
C
C Added fields for INSTANT Games
C
        INTEGER*4  ITS_ROWS                  !INSTANT NUMBER OF ROWS
        INTEGER*4  ITS_BYTES                 !INSTANT NUMBER OF STORAGE BYTES
        INTEGER*4  ONL_BYTES                 !ONLINE
        INTEGER*4  TOTAL_BYTES               !ONLINE + INSTANT
        INTEGER*4  BINDX                     !
        INTEGER*4  SBINDX                    !
        INTEGER*4  BYTE_OFFSET               !
        INTEGER*4  BYTES_TO_MOVE             !

	BYTE       BSALES(MAXGAM * 4 +       ! ONLINE GAMES  40 * 4 BYTES
     *                    AITGAM * 4 +       ! INSTANT GAMES 20 * 4
     *                    AITGAM + 1)        !             + 20 + 1  
C                                            ! TOTAL 261 BYTES


	INTEGER*4  I4TEMP                    !

	INTEGER*2  I2TEMP(2)                 !

	BYTE	   I1TEMP(4)                 !
	LOGICAL    PRIV                      !
	LOGICAL    MANAGER                   !
	LOGICAL    POST                      !
        LOGICAL    HEAD                      !

        !functions
        LOGICAL    NO_JOK_DET
        EXTERNAL   NO_JOK_DET
                           
	EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)

	DATA ERRTYP /Z90/
C
C START CODE FOR SALINV ( SET VARIABLES WITH DEFAULT VALUES )
C
        DEFINED_GAMES = 0
C
C GET INVOICE REPORT OPTIONS
C
	RTER=0
	CLASS    = ZEXT( MESTAB(5) )
	SUBCLASS = ZEXT( MESTAB(6) )
	TRABUF(TSDT1)=CLASS
	TRABUF(TSDT2)=SUBCLASS
C
C GET REPORT TYPE
C
	REPTYP = ZEXT( MESTAB(7) )
C
C GET REPORT PASS NUMBER
C
	I4TEMP=0
	I1TEMP(2)=MESTAB(8)
	I1TEMP(1)=MESTAB(9)
  	REPPNUM=I4TEMP
C
	TER=TRABUF(TTER)

C
C CHECK IF INVOICE REPORTS ARE SUPRESSED
C

	IF(TSBIT(P(SUPRPT),INVREP)) THEN
	    TRABUF(TERR)=SUPR
	    GOTO 8000
	ENDIF
C
C CHECK FOR PRIVILEGED TERMINAL
C IF PRIVILEGED TERMINAL GET REPORT AGENT NUMBER ( REPORT FROM AGENT )
C
	TER=TRABUF(TTER)
	PRIV=.FALSE.
 	IF(TSBIT(AGTTAB(AGTTYP,TER),AGTPRV)) PRIV=.TRUE.
 	IF(PRIV) THEN
C
C GET AGENT NUMBER TO GET INVOICE REPORT
C
            CALL TERM_TO_HOST(MESTAB(10), AGT, 4)
C
C IF AGENT NUMBER IT'S ZERO, GET ACTUAL TERMINAL AGENT NUMBER
C
            IF(AGT .LE. 0) THEN
              RTER = TER 
              GOTO 2000
            ENDIF
C
C SEARCH TERMINAL NUMBER FOR AGENT USER ASKED FOR
C
            DO RTER = 1, NUMAGT
              IF(AGT .EQ. AGTTAB(AGTNUM, RTER) .AND. AGT .NE. 0) GOTO 2000 
            ENDDO
C
C TERMINAL NUMBER FOR AGENT THAT USER ASKED FOR, IT'S NOT FOUND
C
            TRABUF(TERR) = INVL
            GOTO 8000
C
C SET TERMINAL NUMBER FOR INVOICE REPORT
C
2000        CONTINUE
	    TRABUF(TSDT4)=RTER
	ELSE
	    RTER=TER
	ENDIF

        ! get segment number
        SEGNO = ZEXT(MESTAB(14))
        IF (SEGNO.LT.0) THEN
             TYPE *,IAM(),'segment number problem ',segno
	     TRABUF(TERR)=INVL
	     GOTO 8000
	ENDIF

C
C CHECK FOR HEAD OF CHAIN TERMINAL
C
	HEAD=.FALSE. !HEAD is always false as Finland has no chains
	TER=TRABUF(TTER)
	IF(.NOT.PRIV) THEN
	    IF(HEAD.AND.(REPTYP.EQ.1.OR.REPTYP.EQ.3)) THEN
	        I1TEMP(4) = ZEXT( MESTAB(10) )
	        I1TEMP(3) = ZEXT( MESTAB(11) )
	        I1TEMP(2) = ZEXT( MESTAB(12) )
	        I1TEMP(1) = ZEXT( MESTAB(13) )
		RTER      = I4TEMP
	        IF(RTER.EQ.0) RTER=TER
		IF(RTER.LT.1.OR.RTER.GT.NUMAGT) THEN
		   TRABUF(TERR)=INVL
		   GOTO 8000
		ENDIF
C
		IF(AGTTAB(AGTNUM,RTER).EQ.0) THEN
	           TRABUF(TERR) = INVL
	           GOTO 8000
		ENDIF
		TRABUF(TSDT4) = RTER
C
	        IF(AGTHTB(ACHCOD,TER).NE.AGTHTB(ACHCOD,RTER)) THEN
	            TRABUF(TERR)=INVL
	            GOTO 8000
	        ENDIF
	    ELSE
	        RTER=TER
	    ENDIF
	ENDIF
C
C CHECK FOR ASSOCIATED POST
C
	POST=.FALSE.
	TER=TRABUF(TTER)
	IF(.NOT.PRIV.AND..NOT.HEAD) THEN
	    IF(TSBIT(AGTTAB(AGTTYP,TER),AGTPST)) POST=.TRUE.
	    IF(POST.AND.REPTYP.EQ.1) THEN
	        RTER=AGTHTB(ASSTER,TER)
	        TRABUF(TSDT4)=RTER
	        IF(RTER.LT.1.OR.RTER.GT.NUMAGT) THEN
	            TRABUF(TERR)=INVL
	            GOTO 8000
	        ENDIF
	    ELSE
	        RTER=TER
	    ENDIF
	ENDIF
C
C REJECT BAD REPORT TYPE
C
	IF(.NOT.HEAD.AND.REPTYP.EQ.2) TRABUF(TERR)=INVL
	IF(.NOT.HEAD.AND.REPTYP.EQ.3) TRABUF(TERR)=INVL
	IF(.NOT.PRIV.AND..NOT.HEAD.AND.REPTYP.EQ.1) TRABUF(TERR)=INVL
	IF(.NOT.PRIV.AND..NOT.HEAD.AND.REPTYP.EQ.2) TRABUF(TERR)=INVL
	IF(TRABUF(TERR).NE.NOER) GOTO 8000
C
C CHECK TO SEE IF MANAGER IS SIGNED ON.
C
	MANAGER=.FALSE.
	IF(AGTTAB(AGTNCL,RTER).EQ.1) MANAGER=.TRUE.
C
C READ CLERK FILE
C
	IF(MANAGER.AND.(SUBCLASS.EQ.0.OR.
     *	   SUBCLASS.EQ.8)) THEN
	    IF(P(SUPFIL).EQ.1) THEN
	        TRABUF(TERR)=SUPR
	        GOTO 8000
	    ENDIF
	    CALL READW(CLRKFDB,RTER,CLRKREC,ST)
	    IF(ST.NE.0) THEN
	        MESS(1)=SPE
	        MESS(2)=TEGEN
	        MESS(3)=4
	        CALL FASTMOV(SFNAMES(1,CLK),MESS(4),5)
	        MESS(9)=RTER
	        CALL QUEMES(MESS)
	        GOTO 8000
	    ENDIF
	ENDIF
C
C READ ASF FOR INVOICE
C
	IF(P(SUPFIL).EQ.1) THEN
	    TRABUF(TERR)=SUPR
	    GOTO 8000
	ENDIF

	CALL READW(ASFFDB,RTER,ASFREC,ST)
	IF(ST.NE.0) THEN
	    MESS(1)=SPE
	    MESS(2)=TEGEN
	    MESS(3)=4
	    CALL FASTMOV(SFNAMES(1,ASF),MESS(4),5)
	    MESS(9)=RTER
	    CALL QUEMES(MESS)
	    GOTO 8000
	ENDIF

C
C PUT TIME IN OUTPUT MESSAGE(hours, mins, secs)
C
	IND = 5
        CALL PUTIME(TRABUF(TTIM), MESTAB, IND)
C
C PUT CLASS IN OUTPUT MESSAGE
C
	MESTAB(IND) = CLASS                   !IND=8
	IND=IND+1
C
C PUT WEEK IN OUTPUT MESSAGE
C
        CDC=ASFINV(ASFEND,1)
        CALL FIGWEK(CDC, WEEK, YEAR)
        I4TEMP = MOD(YEAR, 100)
        MESTAB(IND) = I4TEMP                  !IND=9
	IND=IND+1
C
C PUT YEAR IN OUTPUT MESSAGE
C
	IF(AGTHTB(ACHLNK,RTER).EQ.-1) THEN
	    MESTAB(IND) = 4                   !IND=10
	ELSE
	    I4TEMP = WEEK
            MESTAB(IND) = I4TEMP 
	ENDIF
	IND=IND+1

C
C SET CDC DATE IN OUTPUT MESSAGE
C
        I4TEMP=DAYCDC                         !rev 1.13
	MESTAB(IND+0) = I1TEMP(2)             !IND=11
	MESTAB(IND+1) = I1TEMP(1)             !IND=12
	IND=IND+2

C
C PUT AGENT NUMBER IN OUTPUT MESSAGE
C
	I4TEMP = AGTTAB(AGTNUM,RTER)
	MESTAB(IND+0) = I1TEMP(4)             !IND=13
	MESTAB(IND+1) = I1TEMP(3)             !IND=14
	MESTAB(IND+2) = I1TEMP(2)             !IND=15
	MESTAB(IND+3) = I1TEMP(1)             !IND=16
	IND=IND+4
C
	BEGSALOFF = 0
        ENDSALOFF = 0
C
        IF (SEGNO .EQ. 0) THEN
C
C PREPARE THE FIRST SEGMENT - HEADER INFORMATION AND SALES SUMMARY	
C
            ! init some segmentation variables
            LAST_SEG(TER) = 0
            ! zero out SALES array
            CALL FASTSET(0,SALES,MAXGAM*MAXMLTD_SEL) 
            
            ! set options byte
            MESTAB(IND) = '08'X			  !17 OPTIONS 
            IND = IND+1 

            ! store IND for # games defined
            SIND=IND				  !18
            IND = IND+1
C
C EURO MIL PROJECT - INCLUDE EURO MIL GAME TYPE AND INDEX
C
            MESTAB(IND + 0) = TEUM !(EURO MIL GAME TYPE)
            MESTAB(IND + 1) = 15
            IND = IND + 2
C            
	    CALL FASTSET(0, INDEX, MAXGAM)
            ! get active game types and indices
            DO 100 GNUM=1,MAXGAM
                GTYP=GNTTAB(GAMTYP,GNUM)
                GIND=GNTTAB(GAMIDX,GNUM)

                IF(GTYP .LE. 0 .OR. GTYP .GT. MAXTYP) GOTO 100
		IF(GIND .LT. 1 .OR. GIND .GT. MAXIND) GOTO 100
	        IF(GNUM.EQ.7) GOTO 100   !include Lotto4 (7)  with Lotto3 (6)

                IF(GTYP.EQ.TKIK.AND.NO_JOK_DET()) GOTO 100

		IF(INDEX(GNUM).EQ.0) THEN
		   INDEX(GNUM) = 1

                   MESTAB(IND+0)=GTYP
                   MESTAB(IND+1)=GIND

                   IND=IND+2
                   DEFINED_GAMES=DEFINED_GAMES+1
		ENDIF
C
100         CONTINUE
C
C C EURO MIL PROJECT - EURO NEED TO SUM ONE MORE GAME
C
C            MESTAB(SIND) = ZEXT(DEFINED_GAMES)   !# GAMES SENT
            MESTAB(SIND) = ZEXT(DEFINED_GAMES + 1)    !# GAMES SENT
            
	    DO 910 GNUM=1,MAXGAM
	        GTYP=GNTTAB(GAMTYP,GNUM)
   	        GIND=GNTTAB(GAMIDX,GNUM)
                
	        IF(GTYP .LE. 0 .OR. GTYP .GT. MAXTYP) GOTO 910
                IF(GIND .LT. 1 .OR. GIND .GT. MAXIND) GOTO 910
                IF(GTYP .EQ. TPAS) GOTO 910   ! LN skipped for AM

                IF(GTYP.EQ.TKIK.AND.NO_JOK_DET()) GOTO 910

C Get ONLINE Summary

                SALIND=1		  	           !1
	        SALES(SALIND) = SALES(SALIND)+
     *                          ASFBIL(GCCNT,GNUM,1)       !CANCELS CNT
                SALIND=SALIND+1				   !2
	        SALES(SALIND) = SALES(SALIND)+
     *                          ASFBIL(GCAMT,GNUM,1)       !CANCELS AMT
                SALIND=SALIND+1				   !3

	        SALES(SALIND) = SALES(SALIND)+             !NET SALES CNT
     *                          ASFBIL(GSCNT,GNUM,1)-   
     *                          ASFBIL(GCCNT,GNUM,1)
                SALIND=SALIND+1				   !4
	        SALES(SALIND) = SALES(SALIND)+             !NET SALES AMT
     *                          ASFBIL(GSAMT,GNUM,1)-
     *                          ASFBIL(GCAMT,GNUM,1)
                SALIND=SALIND+1				   !5

	        SALES(SALIND) = SALES(SALIND)+
     *                          ASFBIL(GVCNT,GNUM,1)+      !VALIDATIONS CNT
     *		                ASFBIL(GRCNT,GNUM,1)
                SALIND=SALIND+1				   !6
	        SALES(SALIND) = SALES(SALIND)+
     *                          ASFBIL(GVAMT,GNUM,1)+      !VALIDATIONS AMT
     *		   	         ASFBIL(GRAMT,GNUM,1)
910	    CONTINUE

	    SALES(6) = SALES(6) + ASFINV(ASFOFFPAY,1)      !offline validation
C
C TRANSPORTES ( ONLY IF WE HAVE ACTIVITY )
C
	    TRANSP = 0
	    IF (ASFINV(ASFACT,1) .GT. 0) THEN
               CALL ASCBIN(ASFINF,SSTTP,LSTTP,STTRANSP,CERR)
               IF (STTRANSP.EQ.1) THEN
                  TRANSP = P(VALCT1)
               ELSEIF (STTRANSP.EQ.2) THEN
                  TRANSP = P(VALCT2)      
               ELSEIF (STTRANSP.EQ.3) THEN
                  TRANSP = P(VALCT3)       
               ENDIF
	    ENDIF

            SALIND = SALIND+1				   !7 Adjusments

            ! ADJUSTMENTS AMOUNT
	    CALL ADDI8I8(SALES(SALIND),ASFINV(ASFADJU,1),BETUNIT)
            
	    ! TRANSPORTE WE SEND WITH ADJUSTMENTS
	    CALL SUBI8I4(SALES(SALIND),TRANSP,BETUNIT)

            CALL REVAMT(SALES(SALIND))
	    SALIND = SALIND+2		                   !9 Comissions
C
            ! Online Sales commission
	    CALL ADDI8I8(SALES(SALIND),ASFINV(ASFSCMU,1),BETUNIT)
            ! + Online validations commission
	    CALL ADDI8I8(SALES(SALIND),ASFINV(ASFVCMU,1),BETUNIT) 
            ! + Online wins commission
	    CALL ADDI8I8(SALES(SALIND),ASFINV(ASFWCMU,1),BETUNIT)
            CALL REVAMT(SALES(SALIND)) 
	    SALIND = SALIND+2                              !11 Ticket Charge

            !TICKET CHARGE
            SALES(SALIND) = ASFINV(ASFTKC,1)                     
            SALIND=SALIND+1				   !12 Service Charge

            !SERVICE CHARGE
            SALES(SALIND) = ASFINV(ASFSRV,1)                     
            SALIND=SALIND+1				   !13 Amount DUE

            !Online amount due
C===============================================================================
            CALL ADDI8I8(SALES(SALIND),ASFINV(ASFDUEU,1),BETUNIT)
C===============================================================================
C           Instead of using AMOUNT DUE, LETs CALCULATE FOR BUG
C           CALL ADDI8I4(SALES(SALIND),SALES(4),BETUNIT)
C           CALL SUBI8I4(SALES(SALIND),SALES(6),BETUNIT)                        
C           CALL ADDI8I8(SALES(SALIND),ASFINV(ASFADJU,1),BETUNIT)
C           CALL SUBI8I4(SALES(SALIND),TRANSP,BETUNIT)
C           CALL SUBI8I8(SALES(SALIND),ASFINV(ASFSCMU,1),BETUNIT)
C           CALL SUBI8I8(SALES(SALIND),ASFINV(ASFVCMU,1),BETUNIT) 	    
C           CALL SUBI8I8(SALES(SALIND),ASFINV(ASFWCMU,1),BETUNIT)
C===============================================================================
            CALL REVAMT(SALES(SALIND)) 
	    SALIND = SALIND+2				   !15 Last useless offset
           
C
C Get INSTANT Summary ( DON NOT USED IN PORTUGAL )
C
C	    SALES(SALIND) = SALES(SALIND)+ASFITINV(ASFITSCNT)  !PACKS SOLD CNT
C           SALIND=SALIND+1				       !15
C
C	    SALES(SALIND) = SALES(SALIND)+ASFITINV(ASFITSAMT)  !PACKS SOLD AMT
C           SALIND=SALIND+1				       !16
C
C	    SALES(SALIND) = SALES(SALIND)+ASFITINV(ASFITVCNT)  !VALIDATION CNT
C           SALIND=SALIND+1				       !17
C
C	    SALES(SALIND) = SALES(SALIND)+ASFITINV(ASFITVAMT)  !VALIDATION AMT
C           SALIND=SALIND+1				       !18
C
C	    SALES(SALIND) = SALES(SALIND)+ASFITINV(ASFITRAMT)  !RETURNS AMT
C           SALIND=SALIND+1				       !19
C
C           ! sales commission
C           CALL ADDI8I4(SALES(SALIND),ASFITINV(ASFITSCM),BETUNIT) 
C           ! prizes commission
C	    CALL ADDI8I4(SALES(SALIND),ASFITINV(ASFITPCM),BETUNIT) 
C            SALIND=SALIND+2				       !21
C	    ! adjustments
C	    CALL ADDI8I4(SALES(SALIND),ASFITINV(ASFITADJ),BETUNIT) 
C           CALL SUBI8I8(SALES(SALIND),ASFINV(ASFPADU,1),BETUNIT)
C           SALIND=SALIND+2				       !23
C           ! instant amount due
C	    CALL ADDI8I4(SALES(SALIND),ASFITINV(ASFITDUE),BETUNIT) 
C           SALIND=SALIND+2				       !25
C
C ONLINE AND INSTANT TOGETHER ( DO NOT USED IN PORTUGAL )
C
C           !ONLINE + INSTANT AMOUNT DUE
C	    CALL ADDI8I8(SALES(SALIND),ASFINV(ASFDUEU,1),BETUNIT)
C	    CALL ADDI8I4(SALES(SALIND),ASFITINV(ASFITDUE),BETUNIT) 
C           SALIND=SALIND+2				       !27
C
C
C PUT SALES INFO HEADER INTO OUTPUT MESSAGE BUFFER
C
            SALIND = SALIND-1
	    DO I = 1, SALIND
                IF(I.EQ.6.OR.I.EQ.7) THEN
	           I4TEMP = -SALES(I)      !validation amount and commission
C                                          NOTE: Comissions are in 9 but it is like 
C                                          this for 10 years so better let it this way
                ELSE
	           I4TEMP = SALES(I)
                ENDIF
	        MESTAB(IND+0) = I1TEMP(4)
	        MESTAB(IND+1) = I1TEMP(3)
	        MESTAB(IND+2) = I1TEMP(2)
	        MESTAB(IND+3) = I1TEMP(1)
	        IND=IND+4
            ENDDO
C
C EURO MIL PROJECT RELEASE 2
C SPLIT INVOICE (FIRST MILLENNIUM AND THEN EURO MIL)
C
C EM CANCELS COUNT
C
            MESTAB(IND+0) = 0
            MESTAB(IND+1) = 0
            MESTAB(IND+2) = 0
            MESTAB(IND+3) = 0
            IND = IND + 4
C
C EM CANCELS AMOUNT (WAGER UNITS)
C
            MESTAB(IND+0) = 0
            MESTAB(IND+1) = 0
            MESTAB(IND+2) = 0
            MESTAB(IND+3) = 0
            IND = IND + 4
C
C EM NET SALES COUNT
C
            MESTAB(IND+0) = 0
            MESTAB(IND+1) = 0
            MESTAB(IND+2) = 0
            MESTAB(IND+3) = 0
            IND = IND + 4
C
C EM NET SALES AMOUNT
C
            MESTAB(IND+0) = 0
            MESTAB(IND+1) = 0
            MESTAB(IND+2) = 0
            MESTAB(IND+3) = 0
            IND = IND + 4
C
C EM CASH COUNT (WAGER UNITS)
C
            MESTAB(IND+0) = 0
            MESTAB(IND+1) = 0
            MESTAB(IND+2) = 0
            MESTAB(IND+3) = 0
            IND = IND + 4
C
C EM CASH AMOUNT (VALIDATION UNITS, INCLUDING REFUNDS)
C
            MESTAB(IND+0) = 0
            MESTAB(IND+1) = 0
            MESTAB(IND+2) = 0
            MESTAB(IND+3) = 0
            IND = IND + 4
C
C EM ADJUSTEMENTS AMOUNT (18 WAGER UNITS)
C
            MESTAB(IND+0) = 0
            MESTAB(IND+1) = 0
            MESTAB(IND+2) = 0
            MESTAB(IND+3) = 0
            MESTAB(IND+4) = 0
            MESTAB(IND+5) = 0
            MESTAB(IND+6) = 0
            MESTAB(IND+7) = 0
            IND = IND + 8
C
C EM COMMISSION AMOUNT, CASHES (18 WAGER UNITS)
C
            MESTAB(IND+0) = 0
            MESTAB(IND+1) = 0
            MESTAB(IND+2) = 0
            MESTAB(IND+3) = 0
            MESTAB(IND+4) = 0
            MESTAB(IND+5) = 0
            MESTAB(IND+6) = 0
            MESTAB(IND+7) = 0
            IND = IND + 8
C
C EM TICKET CHARGE
C
            MESTAB(IND+0) = 0
            MESTAB(IND+1) = 0
            MESTAB(IND+2) = 0
            MESTAB(IND+3) = 0
            IND = IND + 4
C
C EM SERVICE CHARGE
C
            MESTAB(IND+0) = 0
            MESTAB(IND+1) = 0
            MESTAB(IND+2) = 0
            MESTAB(IND+3) = 0
            IND = IND + 4
C
C EM AMOUNT DUE (18 WAGER UNITS)
C
            MESTAB(IND+0) = 0
            MESTAB(IND+1) = 0
            MESTAB(IND+2) = 0
            MESTAB(IND+3) = 0
            MESTAB(IND+4) = 0
            MESTAB(IND+5) = 0
            MESTAB(IND+6) = 0
            MESTAB(IND+7) = 0
            IND = IND + 8
C
C END OF RELEASE 2 FOR EURO MIL PORJECT
C                        
	    OUTLEN = IND-1
            LAST_SEG(TER) = 1
C 
C END OF SEGMENT 1
C
        ELSE
C
C GET SALES DETAILS FOR ALL GAMES 
C
            ! zero out SALES array            
            CALL FASTSET(0,SALES,MAXGAM*MAXMLTD_SEL) 
	    CALL FASTSET(0,BSALES,
     *           MAXGAM + AITGAM + (AITGAM + 1)/4 )
            CALL FASTSET(0, INDEX, MAXGAM)
            ! get net sales for  multi-week options
            SALIND  = 0
            NO_ROWS = 0
            DO 920 GNUM=1,MAXGAM
                GTYP=GNTTAB(GAMTYP,GNUM)
                GIND=GNTTAB(GAMIDX,GNUM)

                IF(GTYP.LE.0.OR.GTYP.EQ.TINS.OR.GTYP.GT.MAXTYP) GOTO 920
                IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 920
                IF(GTYP .EQ. TPAS) GOTO 920                
                IF(GTYP.EQ.TKIK.AND.NO_JOK_DET()) GOTO 920

                DO I = AGAMLEN+1,AGAMLEN+MAXMLTD_SEL !MAXIMUM # ITERATIONS REQUIRED
	           IF(GNUM.NE.7) THEN
		     IF(INDEX(GNUM).EQ.0) THEN
		       SALIND  = SALIND  + 1
		       NO_ROWS = NO_ROWS + 1
		       INDEX(GNUM) = SALIND	
		     ENDIF
	             TMP_GNUM = GNUM
	           ELSE
	             TMP_GNUM = 6
	           ENDIF
                   SALES(INDEX(TMP_GNUM)) = SALES(INDEX(TMP_GNUM)) + 
     *                                      ASFBIL(I,GNUM,1)
                ENDDO
920         CONTINUE

C
C MOVE ONLINE INFORMATION TO THE BSALES BUFFER
C
            BINDX  = 0
	    DO I = 1, SALIND
	        I4TEMP = SALES(I)
	        BSALES(BINDX+1) = I1TEMP(4)
	        BSALES(BINDX+2) = I1TEMP(3)
	        BSALES(BINDX+3) = I1TEMP(2)
	        BSALES(BINDX+4) = I1TEMP(1)
	        BINDX=BINDX+4
            ENDDO
            ONL_BYTES = BINDX
C
C MOVE INSTANT INFORMATION TO THE BSALES BUFFER
C

            ITS_ROWS = 0
            ITS_BYTES  = 0
            BINDX = BINDX + 1
            SBINDX = BINDX                               !INST GAME ROWS

            DO GNUM=1,AITGAM
                IF (ASFITGSAL(AITGNUM,GNUM).GT.0) THEN
                    ITS_ROWS = ITS_ROWS+1
                    BINDX = BINDX + 1
                    BSALES(BINDX) = ASFITGSAL(AITGNUM,GNUM)     !GAME NUMBER
	            I4TEMP = ASFITGSAL(AITGAMT,GNUM)            !SALES
	            BSALES(BINDX+1) = I1TEMP(4)
	            BSALES(BINDX+2) = I1TEMP(3)
	            BSALES(BINDX+3) = I1TEMP(2)
	            BSALES(BINDX+4) = I1TEMP(1)
	            BINDX=BINDX+4
                    ITS_BYTES = ITS_BYTES + 5
                ENDIF
            ENDDO

            BSALES(SBINDX) = ITS_ROWS
            ITS_BYTES = ITS_BYTES + 1        ! byte for # of instant games

            ! calculate number of segments required
            ! -------------------------------------
            ! we will build each segment (after the first) to have 56 
            ! I*4 sales amounts plus the 'header' of 18 bytes
            ! I.E we can send 224 bytes of information at a time

            TOTAL_BYTES = ONL_BYTES + ITS_BYTES
            IF (TOTAL_BYTES .LE. 224) THEN      ! Only 1 Segment needed
                                                ! set OPTIONS BYTE to FF
                MESTAB(IND)   = 'FF'X 
                BYTES_TO_MOVE = TOTAL_BYTES
            ELSE                                ! More segments needed
                BYTES_TO_MOVE = TOTAL_BYTES - (LAST_SEG(TER) - 1)
                IF (BYTES_TO_MOVE .LE. 224) THEN
                    MESTAB(IND)   = 'FF'X
                ELSE
                    BYTES_TO_MOVE = 224
                    MESTAB(IND)   = '08'X
                ENDIF
            ENDIF
            IND = IND+1 

            BYTE_OFFSET   = LAST_SEG(TER)
            LAST_SEG(TER) = LAST_SEG(TER) + BYTES_TO_MOVE
            ! set number of rows
            MESTAB(IND) = NO_ROWS
            IND=IND+1

            CALL MOVBYT(BSALES,BYTE_OFFSET,MESTAB,IND,BYTES_TO_MOVE)
            OUTLEN = IND + BYTES_TO_MOVE - 1
            
        ENDIF

	IF(TRABUF(TERR).NE.NOER) GOTO 8000

	GOTO 9000
C
C ERROR IN REPORT REQUEST FROM TERMINAL
C
8000	CONTINUE
	TRABUF(TSTAT)=REJT
	MESTAB(2) = ERRTYP
	MESTAB(5) = TRABUF(TERR)
        MESTAB(6) = 0
	OUTLEN=6
C
C CALCULATE CHECKSUM FOR MESSAGE BACK TO TERMINAL
C
9000	CONTINUE
	I4CCITT = TRABUF(TCHK)
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)
        CHKLEN=OUTLEN-1
        CALL GETCCITT(MESTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)	
C
	RETURN
C
	END
