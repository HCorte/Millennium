C
C SUBROUTINE CHKRED
C $Log:   GXAFIP:[GOLS]CHKRED.FOV  $
C  
C     Rev 1.1   18 Mar 1997 13:43:26   RXK
C  If redmin for agent has been set then use this value
C  
C     Rev 1.0   17 Apr 1996 12:33:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.6   03 Nov 1993 13:15:46   HXK
C  CHANGE THE OTHER WAY AT CUSTOMERS REQUEST.
C  
C     Rev 1.5   02 Nov 1993 22:42:14   HXK
C  DO NOT EXCLUDE TOTAMT VALUES .EQ. RMAX !!!
C  TOOK OUT DYN_VALUNIT FOR NOW!
C  
C     Rev 1.4   01 Nov 1993 18:37:50   HXK
C  CHANGE CHECK ON RMAX.
C  
C     Rev 1.3   15 Oct 1993 14:56:16   GXA
C  Make sure refund only transactions have the type of TREF, but ensure
C  that refund amount is calculated with for REDMIN/REDMAX checks.
C  
C     Rev 1.2   09 Oct 1993 20:36:22   GXA
C  Added Refunds to total amount for REDMAX/REDMIN check and
C  removed type statements.
C  
C     Rev 1.1   22 Aug 1993 20:16:06   GXA
C  Released for Finland Dec Conversion / Oddset.
C  
C     Rev 1.0   21 Jan 1993 15:50:58   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - valupd.for **
C
C V02 12-OCT-2013 SCML New Validation Messages
C                      VPRPAY prizes can only be paid by privileged terminals
C V01 16-JAN-2001 EPH Fill Trabuf with OP amount
C
C SUBROUTINE TO DETERMINE IF A TICKET CAN BE CASHED.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CHKRED(TRABUF,VALREC,REGAMT,KIKAMT,REFAMT,
     *                    EFLAG,CLAIMED)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
C
        INTEGER*4 TER                   !Terminal #
        INTEGER*4 RMAX                  !Redemption Maximum used.
        INTEGER*4 RMIN                  !Redemption Minimum used.
        INTEGER*4 GAME                  !Game Number Validated.
        INTEGER*4 REGAMT                !REGULAR GAMES WINNINGS
        INTEGER*4 KIKAMT                !KICKER GAMES WINNINGS
        INTEGER*4 REFAMT                !REFUND AMOUNT
        INTEGER*4 TOTAMT                !TOTAL WINNINGS
        INTEGER*4 VALTYP                !Validation Type.
C
        LOGICAL EFLAG                   !Exchange Ticket Flag.
        LOGICAL CLAIMED                 !Claim Ticket Flag.
C
C
        GAME = TRABUF(TGAM)
        TRABUF(TVCODE) = NOWIN
        TRABUF(TVPAY) = REGAMT
        TRABUF(TVKPAY) = KIKAMT
        TRABUF(TVREF) = REFAMT
        TRABUF(TVOPPAY)  = VALREC(VOPSAMT)        !v01
        TRABUF(TVKOPPAY) = VALREC(VKOPSAMT)       !V01
        TER = TRABUF(TTER)
        VALTYP = TRABUF(TVTYPE)
C
C
C GET REDEMPTION MINIMUM AND MAXIMUM (AGENT VALUES HAVE PRIORITY IF SET).
C
        IF(AGTTAB(AGTRMN,TER).GT.0) THEN
           RMIN = AGTTAB(AGTRMN,TER)
        ELSE
           RMIN = REDMIN(GAME)
        ENDIF
        IF(AGTTAB(AGTRMX,TER).GT.0) THEN
           RMAX = AGTTAB(AGTRMX,TER)
        ELSE
           RMAX = REDMAX(GAME)
        ENDIF
C
C MAKE SURE REFUND TYPE IS SET WHEN ONLY REFUNDS APPLY, BUT INSURE THAT 
C TOTAL AMOUNT IS CALCULATED INCLUDING REFUNDS FOR REDMIN/REDMAX CHECKING.
C
        TOTAMT=TRABUF(TVPAY)+TRABUF(TVKPAY)
        IF(TOTAMT.EQ.0.AND.REFAMT.NE.0) TRABUF(TTYP)=TREF
        TOTAMT = TOTAMT + TRABUF(TVREF)
C
C CHECK FOR BANK VALIDATIONS
C
        IF(VALTYP.EQ.VPTB) THEN
           IF(VALREC(VBNKID).NE.0.OR.VALREC(VBNKNUM).NE.0) THEN
              TRABUF(TVCODE) = BASET
           ELSE
              CALL UPVREC(TRABUF,VALREC,EFLAG)
           ENDIF
           RETURN
        ENDIF
C
C CHECK IF PRIV TERMINAL CASH.
C
C (EVEN PRIVILIGED AGENTS CAN RUN OUT OF MONEY??, MAKE SURE THEY HAVE SOME!)
C
        IF(TSBIT(AGTTAB(AGTTYP,TER),AGTPRV)) THEN
C----+------------------------------------------------------------------
C V02| Adding New Validation Messages
C----+------------------------------------------------------------------
C           IF(TOTAMT.GT.RMIN.AND.VALTYP.NE.VMID) THEN
           IF(    TOTAMT .GT. RMIN
     *     .AND. (VALTYP .NE. VMID
     *     .AND.  VALTYP .NE. VNDON
     *     .AND.  VALTYP .NE. VNBNK)) THEN
C----+------------------------------------------------------------------
C V02| Adding New Validation Messages
C----+------------------------------------------------------------------
              TRABUF(TVCODE) = VMINQ
              TRABUF(TERR) = VINQ
           ELSE
              CALL UPVREC(TRABUF,VALREC,EFLAG)
           ENDIF
           RETURN
        ENDIF
C
C REGULAR TERMINAL CASH.
C 
C CHECK FOR VALIDATION INQUIRY SITUATIONS
C
C----+------------------------------------------------------------------
C V02| VPRPAY prizes can only be paid by privileged terminals
C----+------------------------------------------------------------------
    
        IF(VALREC(VSTAT).EQ.VPRPAY) THEN 
          TRABUF(TVCODE) = RCLAM
          RETURN
        ENDIF
C----+------------------------------------------------------------------
C V02| VPRPAY prizes can only be paid by privileged terminals
C----+------------------------------------------------------------------

        IF(TOTAMT.GT.RMIN.AND.TOTAMT.LE.RMAX) THEN
C----+------------------------------------------------------------------
C V02| Adding New Validation Messages
C----+------------------------------------------------------------------
           IF(   VALTYP .NE. VMID
     *     .AND. VALTYP .NE. VNDON
     *     .AND. VALTYP .NE. VNBNK) THEN
C----+------------------------------------------------------------------
C V02| Adding New Validation Messages
C----+------------------------------------------------------------------
              TRABUF(TVCODE) = VMINQ
              TRABUF(TERR) = VINQ
           ELSE
              CALL UPVREC(TRABUF,VALREC,EFLAG)
           ENDIF
           RETURN
        ENDIF
C
C CHECK IF LESS THAN GLOBAL REDMIN
C
        IF(TOTAMT.LE.RMIN) THEN
           CALL UPVREC(TRABUF,VALREC,EFLAG)
           RETURN
        ENDIF
C
C CHECK IF HIGH TIER CASH FOR AGENT
C
        IF(TOTAMT.GT.RMAX) THEN
           TRABUF(TVCODE) = RCLAM 
           RETURN
        ENDIF
C
        RETURN
        END
