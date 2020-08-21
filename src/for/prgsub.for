C SUBROUTINE PRGSUB
C
C V10 17-JAN-2001 EPH Update amount purged for VOPSAMT and VKOPSAMT
C V09 16-JAN-2001 EPH VK2PAMT REMOVED
C V08 14-MAY-1999 UXN Super Triple added.
C V07 19-MAY-1996 HXK Rita's update form Finland
C V06 13-FEB-1996 RXK Fix for Pitka
C V05 23-NOV-1995 HXK Merge of post 65 stuff; changes for Double/Couple
C V04 30-OCT-1995 RXK For oddset games added check of drawing date
C V03 28-SEP-1993 GXA Added refund Amount to Total Amount check.
C V02 18-AUG-1993 SXH Released for Finland
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE PRGSUB(PDRAWS,VALREC,SVALREC,PVALREC,PURGE,SAVE,PARTIAL,
     *             PURTAB)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VDETAIL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'

        ! arguments
	INTEGER*4  PDRAWS(MAXGAM)               !
	INTEGER*4  SVALREC(VALLEN)              !
	INTEGER*4  PVALREC(VALLEN)              !

	LOGICAL  PURGE                          !
	LOGICAL  SAVE                           !
	LOGICAL  PARTIAL                        !


        ! variables
	INTEGER*4  SDETAIL(VPLEN,VMAX)          !
	INTEGER*4  PDETAIL(VPLEN,VMAX)          !

	INTEGER*4  I                            !
	INTEGER*4  IND                          !
	INTEGER*4  AMOUNT                       !
	INTEGER*4  GAME                         !
	INTEGER*4  NUMDAY                       !
        INTEGER*4  FRCS                         !
        INTEGER*4  TOTAMT                       !

	LOGICAL    LATE_DRAW
	LOGICAL    IN_PURTAB
	INTEGER*4  MAXPRG
	PARAMETER  (MAXPRG=200)
	INTEGER*4  PURTAB(MAXGAM,MAXPRG)
        INTEGER*4  GTYP,K           
C
C
	SAVE  = .FALSE.
	PURGE = .FALSE.
        LATE_DRAW = .FALSE.
        IN_PURTAB = .FALSE.
C
C
	IF(.NOT.PARTIAL) THEN
            GAME=VALREC(VGAM)
            NUMDAY=PRGDAY(GAME)
            IF(VALREC(VWCDC).LT.DAYCDC-NUMDAY.AND.NUMDAY.NE.0) THEN
	        PURGE=.TRUE.
	        CALL FASTMOV(VALREC,PVALREC,VALLEN)
	    ELSE
	        SAVE=.TRUE.
	        CALL FASTMOV(VALREC,SVALREC,VALLEN)
	    ENDIF
	    RETURN
	ENDIF
C
C PARTIAL PURGE
C
        CALL FASTMOV(VALREC,PVALREC,VALLEN)
	CALL FASTMOV(VALREC,SVALREC,VALLEN)

C                                                                               
C PURGE IF PRIZE VALUE IS ZERO                                                  
C                                                                               
        TOTAMT = VALREC(VPAMT) + VALREC(VKPAMT) + VALREC(VRAMT)
        IF(VALREC(VSTAT).EQ.VUNCSH.AND.TOTAMT.EQ.0) THEN                          
            PURGE=.TRUE.                                                            
            RETURN                                                                  
        ENDIF                                                                     

C
C CHECK IF INDIVIDUAL PRIZES HAVE TO BE PURGED
C
	CALL DLOGVAL(VALREC,VDETAIL)
	CALL FASTSET(0,SDETAIL,VMAX*VPLEN)
	CALL FASTSET(0,PDETAIL,VMAX*VPLEN)
C
C FOR CANCEL WAGERS SET TOTAL WIN AMOUNT TO ZERO
C
        IF(VALREC(VSTAT) .EQ. VCXL .OR. VALREC(VSTAT) .EQ. VDEL) THEN
          SVALREC(VPAMT) = 0
          SVALREC(VKPAMT) = 0
          SVALREC(VRAMT) = 0
        ENDIF
C
C INITIATE PARTIAL PURGE VARIABLES
C
	PVALREC(VPAMT)=0
	PVALREC(VKPAMT)=0
	PVALREC(VOPSAMT)=0       !V10
	PVALREC(VKOPSAMT)=0      !V10
	PVALREC(VPZOFF)=0
	SVALREC(VPZOFF)=0
C
	GAME=VALREC(VGAM)
        GTYP=GNTTAB(GAMTYP,GAME)
C
	DO 1000 I=1,VALREC(VPZOFF)
C
        GAME=VALREC(VGAM)
        IF(GTYP.EQ.TWIT .OR. GTYP.EQ.TSCR .OR. GTYP.EQ.TCPL .OR.
     *     GTYP.EQ.TDBL .OR. GTYP.EQ.TSSC .OR. GTYP.EQ.TTRP .OR.
     *     GTYP.EQ.TSTR) THEN
           IF(VDETAIL(VDRW,I).GE.PDRAWS(GAME)) GOTO 100
           DO K=1,MAXPRG
              IF(PURTAB(GAME,K) .EQ.0) GOTO 100
              IF(VDETAIL(VDRW,I) .EQ. PURTAB(GAME,K)) IN_PURTAB= .TRUE.
              IF(IN_PURTAB) GOTO 100              
           ENDDO
100        CONTINUE
           LATE_DRAW = .NOT.IN_PURTAB 
        ENDIF
C
        IF(LATE_DRAW) THEN   ! for oddset games only
	    SAVE=.TRUE.
	    SVALREC(VPZOFF)=SVALREC(VPZOFF)+1
	    IND=SVALREC(VPZOFF)
	    CALL FASTMOV(VDETAIL(1,I),SDETAIL(1,IND),VPLEN)
            GOTO 1000
        ENDIF

        FRCS=VALREC(VFRAC)

	IF (VDETAIL(VKIK,I).NE.0 .OR.VDETAIL(VKI2,I).NE.0) GAME = VALREC(VKGME)

C*	IF (VDETAIL(VKIK,I).NE.0 .OR.VDETAIL(VKI2,I).NE.0) THEN
C*	   IF (VDETAIL(VOP,I).EQ.0) THEN
C*              GAME=VALREC(VKGME)
C*           ELSE
C
C             IT IS A KICKER OP IT MUST PURGE WITH MAIN GAME
C             SINCE I HAVE ONLY 1 DATE FOR PURGING EVERYTHING,
C             WRITTEN TO THE OP AND THIS DATE IS THE PURGING OF 
C             MAIN GAME (VARIABLE "GAME" HERE)
C
C*              CALL GETWEEK (VDETAIL(VDRW,I), WEEK, YEAR ?????????)
C*              DRAW = GETDRW ( YEAR, WEEK,        GAME)    ??????
C*           ENDIF
C*        ENDIF




        IF(VDETAIL(VDRW,I).GE.PDRAWS(GAME)) THEN
	   SAVE=.TRUE.
	   SVALREC(VPZOFF)=SVALREC(VPZOFF)+1
	   IND=SVALREC(VPZOFF)
	   CALL FASTMOV(VDETAIL(1,I),SDETAIL(1,IND),VPLEN)
	ELSE
	   PURGE=.TRUE.
	   PVALREC(VPZOFF)=PVALREC(VPZOFF)+1
	   IND=PVALREC(VPZOFF)
           CALL FASTMOV(VDETAIL(1,I),PDETAIL(1,IND),VPLEN)
	   CALL GETPRIZE(PDRAWS,GAME,FRCS,VDETAIL(1,I),AMOUNT)

           ! ONLY SET AMOUNT IF WAGER IS NOT CANCEL
           IF(VALREC(VSTAT) .NE. VCXL .AND. VALREC(VSTAT) .NE. VDEL) THEN
  	     IF(VDETAIL(VKIK,I).EQ.0 .AND.VDETAIL(VKI2,I).EQ.0) THEN
	       PVALREC(VPAMT)=PVALREC(VPAMT)+AMOUNT
	       SVALREC(VPAMT)=SVALREC(VPAMT)-AMOUNT
               IF (VDETAIL(VOP,I).EQ.1) THEN                 !V10
	         PVALREC(VOPSAMT)=PVALREC(VOPSAMT)+AMOUNT   !V10
	         SVALREC(VOPSAMT)=SVALREC(VOPSAMT)-AMOUNT   !V10
               ENDIF                                         !V10
	     ELSE
	       PVALREC(VKPAMT)=PVALREC(VKPAMT)+AMOUNT
               SVALREC(VKPAMT)=SVALREC(VKPAMT)-AMOUNT
               IF(VDETAIL(VOP,I).EQ.1) THEN                    !V10
	         PVALREC(VKOPSAMT)=PVALREC(VKOPSAMT)+AMOUNT    !V10
                 SVALREC(VKOPSAMT)=SVALREC(VKOPSAMT)-AMOUNT    !V10
               ENDIF					       !V10
C	       IF(VDETAIL(VKI2,I).NE.0) THEN       !V09
C                SVALREC(VK2PAMT)=SVALREC(VK2PAMT)-AMOUNT !V09
C              ENDIF
	     ENDIF
           ENDIF
        ENDIF
1000	CONTINUE
C
C
	CALL DVALLOG(PVALREC,PDETAIL)
	CALL DVALLOG(SVALREC,SDETAIL)

	RETURN

	END
