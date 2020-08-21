C
C SUBROUTINE TMREPORT
C
C V17 17-APR-2000 UXN  Initialization for VAKIO added.
C V16 01-FEB-2000 UXN  Only GOOD and FRAC wagers counted.
C V15 15-NOV-1999 OXK  Added SYSNUM -checking
C V14 09-SEP-1998 RXK  Changed due RECREP.def containing QP values and Kicker
C                      number direction 
C V13 13-NOV-1997 UXN  TMFREP.DEF
C V12 18-AUG-1995 RXK  Spede excluded
C V11 24-JUL-1995 PXB  Changed top ten coupons to be dimensioned by game index.
C V10 17-JUL-1995 PXB  V5 Changes. Added extra price range and also top ten 
C                      coupon list.
C V09 07-OCT-1993 HXK  Changed RAVI price ranging for VAX conversion.
C V08 03-SEP-1993 SXH  Fix DCL error, copy=1
C V07 26-AUG-1993 HXN  When it's a sport full system bet, test on GTYP and not 
C                      TRABUF(TGAM).
C V06 18-AUG-1993 HXN  Used OPENQW instead of OPENW.
C V05 04-JUN-1993 HXN  Initial revision.
C V04 21-JAN-1993 DAB  Initial Release
C                      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                      DEC Baseline
C V03 04-JUN-1993 HHN  INITIAL RELEASE FOR VAX FINLAND
C V02 23-FEB-1992 GCAN FIXED SYSTEM #'S AND GRAND TOTALS. 
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C
C SUBROUTINE TO EXTRACT SALES INFORMATION FROM THE TM FILE AND
C UPDATE OFFLINE DATABASE FOR REPORT GENERATION PURPOSES.
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE TMREPORT
        IMPLICIT NONE


        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:RECREP.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:TMFREP.DEF'
C
	INTEGER*4   LUN
C
C     REP.FIL FILE UPDATE TEMPORARY HOLD VARIABLES
C     --------------------------------------------
      INTEGER*2 DATE(12)
      INTEGER*4 FDBREP(7)
      INTEGER*4 TMPN1W(MAXCMB,GAMLEN,SYSTYPS,QPTYPS,MAXDAY)
      INTEGER*4 TMPT1W(JOKLEN,5)
     
      INTEGER*4 TABLE(TCMD,2)
 
      LOGICAL FIRST    /.TRUE./

      INTEGER*4 BEGCDC,ENDCDC
      SAVE      BEGCDC,ENDCDC
      INTEGER*4 GTYP, GIND,ST
      INTEGER*4 I,J,K,L,M,OFF,
     *          INDEX,SYSNUM,
     *          STYPE

C BEGIN CODE -----------------------------------------------------


	IF (FIRST) THEN
	    WRITE (5,501) IAM(),(SFNAMES(ST,REP),ST=1,5)
501         FORMAT(1X,A18,'Opening & Reading Distribution file...',5A4,'...')

	    FIRST = .FALSE.

      	    CALL FASTSET(0,TABLE,TCMD*2)
      	    CALL FASTSET(0,TMPN1W,MAXCMB*GAMLEN*SYSTYPS*QPTYPS*MAXDAY)
      	    CALL FASTSET(0,TMPT1W,JOKLEN*5)


C           GET TODAYS DATE AND FIGURE OUT BEGINING AND ENDING OF WEEK
C 	    ----------------------------------------------------------
      	    DATE(VCDC)=DAYCDC
      	    CALL CDATE(DATE)

      	    BEGCDC=DAYCDC-DATE(VDOW)+1
      	    ENDCDC=BEGCDC+6


C 	    ON SUNDAYS CLEAR REPORT FILE TO START FRESH FOR THE WEEK
C 	    --------------------------------------------------------
      	    IF(DATE(VDOW).EQ.SUNDAY)THEN                   ! V05
      	       CALL CRTFIL(SFNAMES(1,REP),SFSIZES(REP),ST)
      	       IF(ST.NE.0) THEN
      		  WRITE(5,8002) IAM(),(SFNAMES(K,REP),K=1,5),ST
      		  CALL GPAUSE
      	       ENDIF
      	    ENDIF


C 	    OPEN OFFLINE REPORT FILE (REP.FIL)
C 	    ----------------------------------
	    LUN = 7 
	    CALL GETLUN(LUN)
      	    CALL OPENQW (LUN,SFNAMES(1,REP),4,0,0,ST)
      	    IF(ST.NE.0) CALL FILERR(SFNAMES(1,REP),1,ST,0)
      	    CALL IOQINIT(FDBREP,LUN,REPSEC*256)        !in bytes, not in sectors
      	    CALL READQIO (FDBREP,1,REPREC,REPSEC*256,ST)
      	    IF(ST.NE.0) CALL FILERR(SFNAMES(1,REP),2,ST,0)
C
C Initialize VAKIO if first sales day
C
	    DO 10 I=1, NUMSPT
	       IF(SPTBSD(I).NE.DAYCDC) GOTO 10
	       DO J=1, MAXCMB
	          DO K=1, SYSTYPS
		     DO L=1, QPTYPS
			DO M=1, MAXDAY
			   REPN1W(J,NUMLTO+I,K,L,M) = 0
			   REPN2W(J,NUMLTO+I,K,L,M) = 0
			   REPN3W(J,NUMLTO+I,K,L,M) = 0
			   REPN5W(J,NUMLTO+I,K,L,M) = 0
			   REPNAW(J,NUMLTO+I,K,L,M) = 0
                        ENDDO
                     ENDDO
                  ENDDO
               ENDDO
10           CONTINUE

	    RETURN
	ENDIF   !FIRST

	IF (EOF) GOTO 1000       !WRITE TO REPORT FILE
C
C 	GET GAME TYPE AND INDEX
C       -----------------------
        IF(TRABUF(TGAM).LT.1.OR.TRABUF(TGAM).GT.MAXGAM) THEN
      	   TYPE*,IAM(),'Invalid game number found, serial: ',TRABUF(TGAM),
     *                 TRABUF(TSER)
      	   RETURN
        ENDIF


        GTYP = GNTTAB(GAMTYP,TRABUF(TGAM))
        GIND = GNTTAB(GAMIDX,TRABUF(TGAM))

      	IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) THEN
	   TYPE*,IAM(),' Invalid Game type :',GTYP,' .Game #, Ser # :',
     *                 TRABUF(TGAM),TRABUF(TSER)
	   RETURN
	ENDIF

      	IF(GIND.LT.1.OR.GIND.GT.MAXIND) THEN 
	   TYPE*,IAM(),' Invalid Game ind :',GIND,' .Game #, Ser # :',
     *                 TRABUF(TGAM),TRABUF(TSER)
	   RETURN
	ENDIF
C
	IF(TRABUF(TWFFLG).NE.0) RETURN
	IF(TRABUF(TSTAT).NE.GOOD.AND.TRABUF(TSTAT).NE.FRAC) RETURN 


C   FOR LOTTO,SPORTS VAKIO,RAVI AND JOKER CHECK THE DURATION AND UPDATE
C   OFFLINE REPORT FILE VARIABLES                                     
C   -----------------------------                                     

      	IF(GTYP.EQ.TLTO.OR.GTYP.EQ.TSPT.OR.GTYP.EQ.TKIK) THEN             

C   Determine SYSNUM & INDEX
C   ------------------------
      	   IF(GTYP.EQ.TLTO)THEN                                           
      	      INDEX  = GIND                                               
	      SYSNUM = TRABUF(TWSYSN)             
              IF(SYSNUM.EQ.0) SYSNUM = TRABUF(TWNBET)

      	   ELSEIF (GTYP.EQ.TSPT) THEN                                          
      	      INDEX=NUMLTO+GIND                                           
      	      SYSNUM = TRABUF(TWSIMP)                               
      	      IF(TRABUF(TWSYST).EQ.REDSYS) SYSNUM = TRABUF(TWSYSN)             

      	   ELSEIF (GTYP.EQ.TKIK) THEN                                          
      	      INDEX=NUMLTO+NUMSPT+GIND
      	      SYSNUM = TRABUF(TWSYSN)                              
              IF(SYSNUM.EQ.0) SYSNUM = 1
              IF(TRABUF(TWNBET).GT.1) SYSNUM=TRABUF(TWNBET)
      	   ENDIF                                                          

           IF(SYSNUM.EQ.0) SYSNUM = 1

C          IF IT IS A SPORT FULL SYSTEM BET THEN STORE BY NUMBER        
C          OF BOARDS AND NOT BY SYSTEM NUMBER SINCE SYSTEM NUMBER       
C          WILL ALWAYS BE THE SAME (1).                                 
C          --------------------------

      	   IF(GTYP.EQ.TSPT.AND.SYSNUM.GT.256) THEN             
      	      CALL BIGFRSYS(SYSNUM,0)
      	   ENDIF                                                        


C	   Determine STYPE
C	   ---------------
      	   STYPE = TRABUF(TWSYST) +1  !DETERMINE SYSTEM TYPE 
                                      !(1=NOSYS,2-FULL,3=REDUCED)

      	   IF(TRABUF(TWKAMT).NE.0.AND.TRABUF(TWKGME).NE.0) THEN      
      	     IF(TRABUF(TWKFLG).EQ.0.AND.TRABUF(TWKFLG2).EQ.0) THEN
      		TYPE*,IAM(),' Invalid Jokeri flags ser ',TRABUF(TSER)
      		RETURN                                            
      	     ENDIF                                                   
      	   ENDIF                                                     
C
C GET OFFSET FOW NUMBER OF WEEKS
C --------------------------------
           IF(TRABUF(TWDUR).GT.0.AND.TRABUF(TWDUR).LT.4) THEN
              OFF = TRABUF(TWDUR)
      	   ELSEIF(TRABUF(TWDUR).EQ.5) THEN
              OFF = 4
      	   ELSEIF(TRABUF(TWDUR).EQ.10) THEN
              OFF = 5
	   ELSE
	      TYPE*,IAM(),' Invalid TRABUF(TWDUR) : ',TRABUF(TWDUR)
      	   ENDIF                              
C
C CALCULATE TOTALS
C ------------------
           IF(TRABUF(TGAMTYP).NE.TKIK) THEN
              TMPT1W(INDEX,OFF) = TMPT1W(INDEX,OFF) + 1  
           ELSE
              TMPT1W(INDEX,OFF) = TMPT1W(INDEX,OFF) + SYSNUM
           ENDIF
C
C QUICK PICKS 
C -------------------
           IF(TRABUF(TWQPF).EQ.0) THEN                                   
              TMPN1W(SYSNUM,INDEX,STYPE,1,OFF) =
     *        TMPN1W(SYSNUM,INDEX,STYPE,1,OFF) + 1
           ELSE
      	      IF(TRABUF(TWWEQP).EQ.0) THEN
                 TMPN1W(SYSNUM,INDEX,STYPE,2,OFF) =
     *           TMPN1W(SYSNUM,INDEX,STYPE,2,OFF) + 1
              ELSE
                 TMPN1W(SYSNUM,INDEX,STYPE,3,OFF) =
     *           TMPN1W(SYSNUM,INDEX,STYPE,3,OFF) + 1
              ENDIF
           ENDIF
C
C KICKER WITH OTHER GAMES
C ------------------------          
100        CONTINUE
        ENDIF
	RETURN
C--------------------------------------------

 1000 CONTINUE

C LOOP THROUGH TMP VARIABLES AND UPDATE REPORT FILE                             
C -------------------------------------------------
      TYPE *,IAM(),'Updating Report Statistics file'

      CALL FASTMOV(TMPN1W(1,1,1,1,1),
     *             REPN1W(1,1,1,1,DATE(VDOW)),
     *             MAXCMB*GAMLEN*SYSTYPS*QPTYPS)                  !1 WEEK BETS
      CALL FASTMOV(TMPN1W(1,1,1,1,2),
     *             REPN2W(1,1,1,1,DATE(VDOW)),
     *             MAXCMB*GAMLEN*SYSTYPS*QPTYPS)                  !2 WEEK BETS
      CALL FASTMOV(TMPN1W(1,1,1,1,3),
     *             REPN3W(1,1,1,1,DATE(VDOW)),
     *             MAXCMB*GAMLEN*SYSTYPS*QPTYPS)                  !3 WEEK BETS
      CALL FASTMOV(TMPN1W(1,1,1,1,4),
     *             REPN5W(1,1,1,1,DATE(VDOW)),
     *             MAXCMB*GAMLEN*SYSTYPS*QPTYPS)                  !5 WEEK BETS
      CALL FASTMOV(TMPN1W(1,1,1,1,5),
     *             REPNAW(1,1,1,1,DATE(VDOW)),
     *             MAXCMB*GAMLEN*SYSTYPS*QPTYPS)                  !10 WEEK BETS


      CALL FASTMOV(TMPT1W(1,1),REPT1W(1,DATE(VDOW)),JOKLEN)
      CALL FASTMOV(TMPT1W(1,2),REPT2W(1,DATE(VDOW)),JOKLEN)
      CALL FASTMOV(TMPT1W(1,3),REPT3W(1,DATE(VDOW)),JOKLEN)
      CALL FASTMOV(TMPT1W(1,4),REPT5W(1,DATE(VDOW)),JOKLEN)
      CALL FASTMOV(TMPT1W(1,5),REPTAW(1,DATE(VDOW)),JOKLEN)

C
C WRITE UPDATED RECORD BACK TO FILE                                             
C ---------------------------------
      REPBEG = BEGCDC                                                          
      REPEND = ENDCDC                                                          
      REPUPD = DAYCDC 

      TYPE*,IAM(),' Writing to Distribution file'
      CALL WRITEQIO (FDBREP,1,REPREC,REPSEC*256,ST)   
      IF(ST.NE.0) CALL FILERR(SFNAMES(1,REP),3,ST,0)  
      CALL CLOSEQFILE (FDBREP)                        

 8002   FORMAT(1X,A4,' Error while allocating: ',5A4,' > ',I4)
	RETURN
	END
