C
C V13 21-MAY-2010 RXK Changes for ePassive
C
C SUBROUTINE SALSUM
C $Log:   GXAFXT:[GOLS]SALSUM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:49:54   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.11   26 Jan 1994 12:24:04   JXP
C  Show total af all clerks - last revision was in error
C  
C     Rev 1.9   30 Sep 1993 18:26:50   GXA
C  Added Refund Amount to Validations.
C  
C     Rev 1.8   25 Sep 1993 18:48:16   GXA
C  Corrected previous correction. (Subtract cancell amount NOT count).
C  
C     Rev 1.7   24 Sep 1993 22:07:56   GXA
C  Do not include cancels in total count and amount.
C  
C     Rev 1.6   16 Aug 1993 17:34:42   SXH
C  Debugged
C  
C     Rev 1.5   13 Aug 1993 14:11:12   SXH
C  Debugging...
C  
C     Rev 1.4   03 Aug 1993 15:01:50   HXK
C  FIXED CDC BUG
C  
C     Rev 1.3   21 Jun 1993 16:23:14   HXK
C  CHANGED SALSUM.FCC FOR FINLAND VAX CONVERSION
C  
C     Rev 1.2   10 Jun 1993 19:06:50   HXK
C  Changed AGTINF.DEF, AGTCOM.DEF includes.
C  
C     Rev 1.1   10 Feb 1993 17:41:24   EBD
C  Changed dimension of sales array to wildcard, such that
C  calling routine dimensions the array
C  
C     Rev 1.0   21 Jan 1993 17:33:30   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - spe_salsum.for **
C
C SALSUM.FOR
C
C V05 24-MAR-2017 HXK  OMIT JOKER DETAILS IF JOKER REMOVED > 90 DAYS
C V04 24-JUN-2015 SCML Correction of SALSUM for Portugal
C V03 13-JUL-92 WLM CHECKED AND RELEASED FOR THE NETHERLANDS
C V02 27-JUN-91 JPJ UPDATED TO DISPLAY CORRECT WEEKLY TOTAL SALES
C V01 15-MAR-91 JPJ INITIAL RELEASE FOR MARYLAND
C
C CALLING SEQUENCE:
C     CALL SALSUM(SALES,SUBCLASS,MANAGER,SALOFF,CDC,TRABUF)
C INPUT
C     SUBCLASS - REPORT SUBCLASS
C     MANAGER  - MANAGER FLAG
C
C OUTPUT
C     SALES    - SALES INFO FOR REPORT
C     SALOFF   - TOTAL WORDS USED
C     CDC      - CDC OF REPORT
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
C----+------------------------------------------------------------------
C V04| Correction of SALSUM for Portugal
C----+------------------------------------------------------------------
C       SUBROUTINE SALSUM(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
        SUBROUTINE SALSUM_OLD(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
C----+------------------------------------------------------------------
C V04| Correction of SALSUM for Portugal
C----+------------------------------------------------------------------

        IMPLICIT NONE


        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SPECOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'


        !arguments
        INTEGER*4  SALES(*)          !
        INTEGER*4  SUBCLASS          !
        INTEGER*4  SALOFF            !
        INTEGER*4  CDC               !
        INTEGER*4  RTER              !

        LOGICAL    MANAGER           !

        !variables
        INTEGER*4  GNUM              !
        INTEGER*4  GTYP              !
        INTEGER*4  CLERK             !
        INTEGER*4  I                 !
        INTEGER*4  J                 !
        INTEGER*4  OFF               !
        INTEGER*4  SALIND            !
        INTEGER*4  WORK_SALIND       !
        INTEGER*4  TMPCNT            !
        INTEGER*4  TMPAMT(2)         !
        INTEGER*4  CLRKNUM

        !function(s)
        LOGICAL  NO_JOK_DET  ! no Jok details if Joker dead > 90 days
        EXTERNAL NO_JOK_DET 

        TMPCNT    = 0
        TMPAMT(1) = 0
        TMPAMT(2) = 0


C
C TOTAL SUMMARY REPORT
C
        IF(SUBCLASS.EQ.0.OR.SUBCLASS.EQ.8) THEN  !TODAY OR W. T. D.
            DO 100 GNUM=1,MAXGAM
                GTYP=GNTTAB(GAMTYP,GNUM)
		!IF (GTYP.EQ.TPAS) THEN
		!   SALIND = 5       
                !   GOTO 110              !ONLY VALIDATIONS
	        !ENDIF        
                IF(GTYP.LE.0.OR.GTYP.EQ.TINS) THEN
                  SALIND=7
                  GOTO 100
                ENDIF
                IF(GTYP.EQ.TKIK.AND.NO_JOK_DET()) GOTO 100
                SALIND=1
C
C GROSS SALES = GROSS SALES   !NOT USED IN FINLAND
C
C***            SALES(SALIND)=SALES(SALIND)+AGTGAM(GSCNT,GNUM,RTER)    ! CNT
C***            SALIND=SALIND+1
C***            SALES(SALIND)=SALES(SALIND)+AGTGAM(GSAMT,GNUM,RTER)    ! AMT
C***            SALIND=SALIND+1


C
C CANCELLATIONS = CANCELLATIONS
C
                SALES(SALIND) = SALES(SALIND) + AGTGAM(GCCNT,GNUM,RTER) ! AMT
                SALIND = SALIND+1
C***            TMPCNT = TMPCNT+AGTGAM(GCCNT,GNUM,RTER)
                SALES(SALIND)=SALES(SALIND)+AGTGAM(GCAMT,GNUM,RTER)     ! CNT
                SALIND=SALIND+1
             

C
C NET SALES = GROSS SALES - CANCELLATIONS
C
                SALES(SALIND)=SALES(SALIND)+AGTGAM(GSCNT,GNUM,RTER)-   ! CNT
     *                                      AGTGAM(GCCNT,GNUM,RTER)
                SALIND=SALIND+1
                TMPCNT=TMPCNT+AGTGAM(GSCNT,GNUM,RTER)-
     *                        AGTGAM(GCCNT,GNUM,RTER)
                SALES(SALIND)=SALES(SALIND)+AGTGAM(GSAMT,GNUM,RTER)-   ! AMT
     *                                      AGTGAM(GCAMT,GNUM,RTER)
                SALIND=SALIND+1
                CALL ADDI8I4(TMPAMT,AGTGAM(GSAMT,GNUM,RTER),BETUNIT)
                CALL SUBI8I4(TMPAMT,AGTGAM(GCAMT,GNUM,RTER),BETUNIT)

C
C CASH = VALIDATION + REFUNDS
C
110		CONTINUE
                SALES(SALIND)=SALES(SALIND)+AGTGAM(GVCNT,GNUM,RTER) + ! AMT
     *                                      AGTGAM(GRCNT,GNUM,RTER)
                SALIND=SALIND+1
                TMPCNT=TMPCNT+AGTGAM(GVCNT,GNUM,RTER) +
     *                        AGTGAM(GRCNT,GNUM,RTER)
                SALES(SALIND)=SALES(SALIND)+AGTGAM(GVAMT,GNUM,RTER) + ! CNT
     *                                      AGTGAM(GRAMT,GNUM,RTER)
                SALIND=SALIND+1
                CALL SUBI8I4(TMPAMT,AGTGAM(GVAMT,GNUM,RTER),VALUNIT)
                CALL SUBI8I4(TMPAMT,AGTGAM(GRAMT,GNUM,RTER),VALUNIT)
100         CONTINUE


C
C Instant sales not in use...
C
	     CAll FASTSET(0,SALES(SALIND),2)
	     SALIND = SALIND + 2

C
C Instant validations.
C
            CLRKNUM = AGTHTB(AGTPASOFF,RTER)
	    IF(CLRKNUM.LE.0) CLRKNUM = 1
            SALES(SALIND)=SALES(SALIND)+AGTMIS(CLRKNUM,1,RTER)
            TMPCNT=TMPCNT+AGTMIS(CLRKNUM,1,RTER)
	    SALIND = SALIND + 1
            SALES(SALIND)=SALES(SALIND)+AGTMIS(CLRKNUM,2,RTER)
            SALIND=SALIND+1
            CALL SUBI8I4(TMPAMT,AGTMIS(CLRKNUM,2,RTER),VALUNIT)

C
C MISC SALES TXNS.
C
            OFF=PRM_NUMINS*2
            DO I=1,PRM_NUMISAL
                WORK_SALIND=SALIND
                SALES(WORK_SALIND)=SALES(WORK_SALIND)+AGTMIS(I+OFF,1,RTER)
                WORK_SALIND=WORK_SALIND+1
                TMPCNT=TMPCNT+AGTMIS(I+OFF,1,RTER)
                SALES(WORK_SALIND)=SALES(WORK_SALIND)+AGTMIS(I+OFF,2,RTER)
                WORK_SALIND=WORK_SALIND+1
                CALL ADDI8I4(TMPAMT,AGTMIS(I+OFF,2,RTER),BETUNIT)
            END DO
            SALIND=WORK_SALIND


C
C PETTY CASH TXNS.
C
            OFF=PRM_NUMINS*2+PRM_NUMISAL
            DO I=1,PRM_NUMIPTY
                WORK_SALIND=SALIND
                SALES(WORK_SALIND)=SALES(WORK_SALIND)+AGTMIS(I+OFF,1,RTER)
                WORK_SALIND=WORK_SALIND+1
                TMPCNT=TMPCNT+AGTMIS(I+OFF,1,RTER)     
                SALES(WORK_SALIND)=SALES(WORK_SALIND)+AGTMIS(I+OFF,2,RTER)
                WORK_SALIND=WORK_SALIND+1
                CALL SUBI8I4(TMPAMT,AGTMIS(I+OFF,2,RTER),BETUNIT)
            END DO
            SALIND=WORK_SALIND
            CDC=DAYCDC

        ENDIF


C
C IF SHOP OWNER GET EVERYONE OF HIS CLERKS ACCOUNTS
C
C       IF(MANAGER.AND.P(CLRKACT).EQ.0)  THEN   !GET CLERKS ACCOUNTS ALSO
        IF(MANAGER) THEN   !GET CLERKS ACCOUNTS ALSO
            DO 130 CLERK = 2, 8
                IF(AGTTAB((APSNUM+CLERK)-1,RTER).EQ.0) GOTO 130
                DO 132 GNUM=1,MAXGAM
                    GTYP=GNTTAB(GAMTYP,GNUM)
		    !IF (GTYP.EQ.TPAS) THEN
		    !   SALIND = 5       
                    !   GOTO 133              !ONLY VALIDATIONS
	            !ENDIF        
                    IF(GTYP.LE.0.OR.GTYP.EQ.TINS) THEN
                      SALIND=7
                      GOTO 132
                    ENDIF
                    IF(GTYP.EQ.TKIK.AND.NO_JOK_DET()) GOTO 132
                    SALIND=1


C***            SALES(SALIND)=SALES(SALIND)+CLRKDAY(GSCNT,GNUM,CLERK) !NO GROSS
C***            SALIND=SALIND+1                                       !SALES IN
C***            SALES(SALIND)=SALES(SALIND)+CLRKDAY(GSAMT,GNUM,CLERK) !FINLAND
C***            SALIND=SALIND+1


                    SALES(SALIND)=SALES(SALIND)+CLRKDAY(GCCNT,GNUM,CLERK)
C***                TMPCNT=TMPCNT+CLRKDAY(GCCNT,GNUM,CLERK)
                    SALIND=SALIND+1
                    SALES(SALIND)=SALES(SALIND)+CLRKDAY(GCAMT,GNUM,CLERK)
                    SALIND=SALIND+1


                    SALES(SALIND)=SALES(SALIND)+CLRKDAY(GSCNT,GNUM,CLERK)-
     *                                          CLRKDAY(GCCNT,GNUM,CLERK)
                    TMPCNT=TMPCNT+CLRKDAY(GSCNT,GNUM,CLERK)-
     *                            CLRKDAY(GCCNT,GNUM,CLERK)
                    SALIND=SALIND+1
                    SALES(SALIND)=SALES(SALIND)+CLRKDAY(GSAMT,GNUM,CLERK)-
     *                                          CLRKDAY(GCAMT,GNUM,CLERK)
                    CALL ADDI8I4(TMPAMT,CLRKDAY(GSAMT,GNUM,CLERK),BETUNIT)
                    CALL SUBI8I4(TMPAMT,CLRKDAY(GCAMT,GNUM,CLERK),BETUNIT)
                    SALIND=SALIND+1

133		    CONTINUE
                    SALES(SALIND)=SALES(SALIND)+CLRKDAY(GVCNT,GNUM,CLERK) +
     *                                          CLRKDAY(GRCNT,GNUM,CLERK)
                    TMPCNT=TMPCNT+CLRKDAY(GVCNT,GNUM,CLERK) +
     *                            CLRKDAY(GRCNT,GNUM,CLERK)
                    SALIND=SALIND+1
                    SALES(SALIND)=SALES(SALIND)+CLRKDAY(GVAMT,GNUM,CLERK) +
     *                                          CLRKDAY(GRAMT,GNUM,CLERK)
                    CALL SUBI8I4(TMPAMT,CLRKDAY(GVAMT,GNUM,CLERK),VALUNIT)
                    CALL SUBI8I4(TMPAMT,CLRKDAY(GRAMT,GNUM,CLERK),VALUNIT)
                    SALIND=SALIND+1
132             CONTINUE


C
C Instant sales not in use....
C
		CALL FASTSET(0,SALES(SALIND),2)
	        SALIND = SALIND + 2
C
C Instant validations
C
                SALES(SALIND)=SALES(SALIND)+CLRKMIS(CLERK,1,CLERK)
                TMPCNT=TMPCNT+CLRKMIS(CLERK,1,CLERK)
                SALIND=SALIND+1
                SALES(SALIND)=SALES(SALIND)+CLRKMIS(CLERK,2,CLERK)
                CALL SUBI8I4(TMPAMT,CLRKMIS(CLERK,2,CLERK),VALUNIT)
                SALIND=SALIND+1

                OFF=PRM_NUMINS*2
                DO I=1,PRM_NUMISAL  !MISCELLANEOUS ITEMS
                    WORK_SALIND=SALIND
                    SALES(WORK_SALIND)=SALES(WORK_SALIND)+
     *                                 CLRKMIS(I+OFF,1,CLERK)
                    TMPCNT=TMPCNT+CLRKMIS(I+OFF,1,CLERK)
                    WORK_SALIND=WORK_SALIND+1
                    SALES(WORK_SALIND)=SALES(WORK_SALIND)+
     *                                 CLRKMIS(I+OFF,2,CLERK)
                    CALL ADDI8I4(TMPAMT,CLRKMIS(I+OFF,2,CLERK),BETUNIT)
                    WORK_SALIND=WORK_SALIND+1
                END DO
                SALIND=WORK_SALIND


                OFF=OFF+PRM_NUMISAL
                DO I=1,PRM_NUMIPTY  !PETTY CASH
                    WORK_SALIND=SALIND
                    SALES(WORK_SALIND)=SALES(WORK_SALIND)+
     *                                 CLRKMIS(I+OFF,1,CLERK)
                    TMPCNT=TMPCNT+CLRKMIS(I+OFF,1,CLERK)
                    WORK_SALIND=WORK_SALIND+1
                    SALES(WORK_SALIND)=SALES(WORK_SALIND)+
     *                                 CLRKMIS(I+OFF,2,CLERK)
                    CALL SUBI8I4(TMPAMT,CLRKMIS(I+OFF,2,CLERK),BETUNIT)
                    WORK_SALIND=WORK_SALIND+1
                END DO
                SALIND=WORK_SALIND

130         CONTINUE

            CDC=DAYCDC

        ENDIF


C
C IF WEEK TO DATE ALSO ACCUMULATE WEEK TO DATE
C
        IF(SUBCLASS.EQ.8) THEN
            DO 170 I= 1,9
                IF(ASFDAT(ASFCDC,I).LE.ASFINV(ASFEND,1)) GOTO 170
                DO 150 GNUM=1,MAXGAM
                    GTYP=GNTTAB(GAMTYP,GNUM)
		    !IF (GTYP.EQ.TPAS) THEN
		    !   SALIND = 5       
                    !   GOTO 140              !ONLY VALIDATIONS
	            !ENDIF        
                    IF(GTYP.LE.0.OR.GTYP.EQ.TINS) THEN
                      SALIND=7
                      GOTO 150
                    ENDIF
                    IF(GTYP.EQ.TKIK.AND.NO_JOK_DET()) GOTO 150
                    SALIND=1


C***            SALES(SALIND)=SALES(SALIND)+ASFDAY(GSCNT,GNUM,I)  !NO GROSS
C***            SALIND=SALIND+1                                   !SALES IN
C***            SALES(SALIND)=SALES(SALIND)+ASFDAY(GSAMT,GNUM,I)  !FINLAND
C***            SALIND=SALIND+1


                    SALES(SALIND)=SALES(SALIND)+ASFDAY(GCCNT,GNUM,I)
C***                TMPCNT=TMPCNT+ASFDAY(GCCNT,GNUM,I)
                    SALIND=SALIND+1
                    SALES(SALIND)=SALES(SALIND)+ASFDAY(GCAMT,GNUM,I)
                    SALIND=SALIND+1


                    SALES(SALIND)=SALES(SALIND)+ASFDAY(GSCNT,GNUM,I)-
     *                                          ASFDAY(GCCNT,GNUM,I)
                    TMPCNT=TMPCNT+ASFDAY(GSCNT,GNUM,I)-
     *                            ASFDAY(GCCNT,GNUM,I)
                    SALIND=SALIND+1
                    SALES(SALIND)=SALES(SALIND)+ASFDAY(GSAMT,GNUM,I)-
     *                                          ASFDAY(GCAMT,GNUM,I)
                    CALL ADDI8I4(TMPAMT,ASFDAY(GSAMT,GNUM,I),BETUNIT)
                    CALL SUBI8I4(TMPAMT,ASFDAY(GCAMT,GNUM,I),BETUNIT)
                    SALIND=SALIND+1

140		    CONTINUE
                    SALES(SALIND)=SALES(SALIND)+ASFDAY(GVCNT,GNUM,I) +
     *                                          ASFDAY(GRCNT,GNUM,I)
                    TMPCNT=TMPCNT+ASFDAY(GVCNT,GNUM,I) +
     *                            ASFDAY(GRCNT,GNUM,I)
                    SALIND=SALIND+1
                    SALES(SALIND)=SALES(SALIND)+ASFDAY(GVAMT,GNUM,I) +
     *                                          ASFDAY(GRAMT,GNUM,I)
                    CALL SUBI8I4(TMPAMT,ASFDAY(GVAMT,GNUM,I),VALUNIT)
                    CALL SUBI8I4(TMPAMT,ASFDAY(GRAMT,GNUM,I),VALUNIT)
                    SALIND=SALIND+1
150             CONTINUE
C
C Instant sales not in use....
C
		CALL FASTSET(0,SALES(SALIND),2)
	        SALIND = SALIND + 2
C
C Instant validations
C
                DO CLERK=1,NUMCLERK
                   IF(AGTTAB((APSNUM+CLERK)-1,RTER).EQ.0) GOTO 160
                   SALES(SALIND)=SALES(SALIND)+ASFMIS(CLERK,1,I)
                   TMPCNT=TMPCNT+ASFMIS(CLERK,1,I)
                   SALES(SALIND+1)=SALES(SALIND+1)+ASFMIS(CLERK,2,I)
                   CALL SUBI8I4(TMPAMT,ASFMIS(CLERK,2,I),VALUNIT)
160                CONTINUE
                ENDDO
                SALIND=SALIND+2
C
                OFF=2*PRM_NUMINS
                DO J=1,PRM_NUMISAL  !MISCELLANEOUS ITEMS
                    WORK_SALIND=SALIND
                    SALES(WORK_SALIND)=SALES(WORK_SALIND)+ASFMIS(J+OFF,1,I)
                    TMPCNT=TMPCNT+ASFMIS(J+OFF,1,I)
                    WORK_SALIND=WORK_SALIND+1
                    SALES(WORK_SALIND)=SALES(WORK_SALIND)+ASFMIS(J+OFF,2,I)
                    CALL ADDI8I4(TMPAMT,ASFMIS(J+OFF,2,I),BETUNIT)
                    WORK_SALIND=WORK_SALIND+1
                END DO
                SALIND=WORK_SALIND


                OFF=OFF+PRM_NUMISAL
                DO J=1,PRM_NUMIPTY  !PETTY CASH
                    WORK_SALIND=SALIND
                    SALES(WORK_SALIND)=SALES(WORK_SALIND)+ASFMIS(J+OFF,1,I)
                    TMPCNT=TMPCNT+ASFMIS(J+OFF,1,I)
                    WORK_SALIND=WORK_SALIND+1
                    SALES(WORK_SALIND)=SALES(WORK_SALIND)+ASFMIS(J+OFF,2,I)
                    CALL SUBI8I4(TMPAMT,ASFMIS(J+OFF,2,I),BETUNIT)
                    WORK_SALIND=WORK_SALIND+1
                END DO
               SALIND=WORK_SALIND

170         CONTINUE

            CDC=DAYCDC

        ENDIF


C
C PROCESS FOR DAY REQUESTED
C
        IF(SUBCLASS.GE.1.AND.SUBCLASS.LE.7) THEN
            DO I= 1,9
                IF(ASFDAT(ASFDOW,I).EQ.SUBCLASS) GOTO 180
            END DO
            TRABUF(TERR)=INVL
            GOTO 8000
C
180         CONTINUE
            DO 182 GNUM=1,MAXGAM
                GTYP=GNTTAB(GAMTYP,GNUM)
		!IF (GTYP.EQ.TPAS) THEN
		!   SALIND = 5       
                !   GOTO 181              !ONLY VALIDATIONS
	        !ENDIF        
                IF(GTYP.LE.0.OR.GTYP.EQ.TINS) THEN
                  SALIND=7
                  GOTO 182
                ENDIF
                IF(GTYP.EQ.TKIK.AND.NO_JOK_DET()) GOTO 182
                SALIND=1


C***         SALES(SALIND)=SALES(SALIND)+ASFDAY(GSCNT,GNUM,I)  !NO GROSS SALES
C***         SALIND=SALIND+1                                   !SALES FOR
C***         SALES(SALIND)=SALES(SALIND)+ASFDAY(GSAMT,GNUM,I)  !FINLAND
C***         SALIND=SALIND+1


                SALES(SALIND)=SALES(SALIND)+ASFDAY(GCCNT,GNUM,I)
C***            TMPCNT=TMPCNT+ASFDAY(GCCNT,GNUM,I)
                SALIND=SALIND+1
                SALES(SALIND)=SALES(SALIND)+ASFDAY(GCAMT,GNUM,I)
                SALIND=SALIND+1


                SALES(SALIND)=SALES(SALIND)+ASFDAY(GSCNT,GNUM,I)-
     *                                      ASFDAY(GCCNT,GNUM,I)
                TMPCNT=TMPCNT+ASFDAY(GSCNT,GNUM,I)-
     *                        ASFDAY(GCCNT,GNUM,I)
                SALIND=SALIND+1
                SALES(SALIND)=SALES(SALIND)+ASFDAY(GSAMT,GNUM,I)-
     *                                      ASFDAY(GCAMT,GNUM,I)
                CALL ADDI8I4(TMPAMT,ASFDAY(GSAMT,GNUM,I),BETUNIT)
                CALL SUBI8I4(TMPAMT,ASFDAY(GCAMT,GNUM,I),BETUNIT)
                SALIND=SALIND+1

181		CONTINUE
                SALES(SALIND)=SALES(SALIND)+ASFDAY(GVCNT,GNUM,I) +
     *                                      ASFDAY(GRCNT,GNUM,I)
                TMPCNT=TMPCNT+ASFDAY(GVCNT,GNUM,I) +
     *                        ASFDAY(GRCNT,GNUM,I)
                SALIND=SALIND+1
                SALES(SALIND)=SALES(SALIND)+ASFDAY(GVAMT,GNUM,I) +
     *                                      ASFDAY(GRAMT,GNUM,I)
                CALL SUBI8I4(TMPAMT,ASFDAY(GVAMT,GNUM,I),VALUNIT)
                CALL SUBI8I4(TMPAMT,ASFDAY(GRAMT,GNUM,I),VALUNIT)
                SALIND=SALIND+1
182         CONTINUE

C
C Instant sales not in use....
C
	    CALL FASTSET(0,SALES(SALIND),2)
	    SALIND = SALIND + 2
C
C Instant validations
C
            DO CLERK=1,NUMCLERK
               IF(AGTTAB((APSNUM+CLERK)-1,RTER).EQ.0) GOTO 190
               SALES(SALIND)=SALES(SALIND)+ASFMIS(CLERK,1,I)
               TMPCNT=TMPCNT+ASFMIS(CLERK,1,I)
               SALES(SALIND+1)=SALES(SALIND+1)+ASFMIS(CLERK,2,I)
               CALL SUBI8I4(TMPAMT,ASFMIS(CLERK,2,I),VALUNIT)
190            CONTINUE
            ENDDO
            SALIND=SALIND+2
C
            OFF=2*PRM_NUMINS
            DO J=1,PRM_NUMISAL  !MISCELLANEOUS SALES
                WORK_SALIND=SALIND
                SALES(WORK_SALIND)=SALES(WORK_SALIND)+ASFMIS(J+OFF,1,I)
                TMPCNT=TMPCNT+ASFMIS(J+OFF,1,I)
                WORK_SALIND=WORK_SALIND+1
                SALES(WORK_SALIND)=SALES(WORK_SALIND)+ASFMIS(J+OFF,2,I)
                CALL ADDI8I4(TMPAMT,ASFMIS(J+OFF,2,I),BETUNIT)
                WORK_SALIND=WORK_SALIND+1
            END DO
            SALIND=WORK_SALIND


            OFF=OFF+PRM_NUMISAL
            DO J=1,PRM_NUMIPTY  !PETTY CASH
                WORK_SALIND=SALIND
                SALES(WORK_SALIND)=SALES(WORK_SALIND)+ASFMIS(J+OFF,1,I)
                TMPCNT=TMPCNT+ASFMIS(J+OFF,1,I)
                WORK_SALIND=WORK_SALIND+1
                SALES(WORK_SALIND)=SALES(WORK_SALIND)+ASFMIS(J+OFF,2,I)
                CALL SUBI8I4(TMPAMT,ASFMIS(J+OFF,2,I),BETUNIT)
                WORK_SALIND=WORK_SALIND+1
            END DO
            SALIND=WORK_SALIND

            CDC=ASFDAT(ASFCDC,I)
        ENDIF


C
C TOTAL SALES COUNT AND AMOUNT 
C
        SALES(SALIND)=SALES(SALIND)+TMPCNT  !TOTAL CASH COUNT
        SALIND=SALIND+1
        CALL ADDI8I8(SALES(SALIND),TMPAMT,BETUNIT)
C
C CHANGE TOTAL SALES AMOUNT ( FIRST MORE SIGNIFICANT VALUE )
C
	I = SALES(SALIND)
        SALES(SALIND) = SALES(SALIND + 1)
        SALES(SALIND + 1) = I
C
C SET OUTPUT VARIABLES
C
        SALIND=SALIND+2
        SALOFF=SALIND-1

8000    CONTINUE

        RETURN

        END








C----+------------------------------------------------------------------
C V04| Correction of SALSUM for Portugal
C----+------------------------------------------------------------------
        
        SUBROUTINE I8ADDI8I8(I8RES, I8A, I8B)
        IMPLICIT NONE
        
        INTEGER*8 I8RES
        INTEGER*8 I8A, I8B
        
        I8RES = I8A + I8B
        
        RETURN
        END

        SUBROUTINE I8ADDI8I4(I8RES, I8A, I4B)
        IMPLICIT NONE
        
        INTEGER*8 I8RES
        INTEGER*4 I4B
        INTEGER*8 I8A, I8B
        
        CALL I4TOI8(I4B,I8B)
        
        I8RES = I8A + I8B
        
        RETURN
        END

        SUBROUTINE I8SUBI8I4(I8RES, I8A, I4B)
        IMPLICIT NONE
        
        INTEGER*8 I8RES
        INTEGER*4 I4B
        INTEGER*8 I8A, I8B
        
        CALL I4TOI8(I4B,I8B)
        
        I8RES = I8A - I8B
        
        RETURN
        END

        SUBROUTINE I8TOI4(I8VAL, I4VAL)
        IMPLICIT NONE
        
        INTEGER*4 I4VAL
        INTEGER*8 I8VAL
        
        INTEGER*8 I8TMP
        
        INTEGER*4 SIGN
        
        SIGN = 1
        IF(I8VAL .LT. 0) THEN
            SIGN = -1
        ENDIF
        
        I8TMP = KIABS(I8VAL)
        I4VAL = INT4(KMOD(I8TMP,KZEXT(2147483647))) ! MOD(I8TMP,2^31) - 
                ! remember, most significant bit is a sign bit
        I4VAL = SIGN * I4VAL
        RETURN
        END


        SUBROUTINE I4TOI8(I4VAL, I8VAL)
        IMPLICIT NONE
        
        INTEGER*8 I8VAL
        INTEGER*4 I4VAL
        
        INTEGER*8 I8TMP
        
        INTEGER*8 SIGN
        
        SIGN = 1
        IF(I4VAL .LT. 0) THEN
            SIGN = -1
        ENDIF
        
        I8TMP = INT8(IABS(I4VAL))
        I8VAL = SIGN * I8TMP
        RETURN
        END


        SUBROUTINE SALSUM(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
        IMPLICIT NONE


        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SPECOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'


        !arguments
        INTEGER*4  SALES(*)          !
        INTEGER*4  SUBCLASS          !
        INTEGER*4  SALOFF            !
        INTEGER*4  CDC               !
        INTEGER*4  RTER              !

        LOGICAL    MANAGER           !

        !variables
        
        INTEGER*4  GNUM              !
        INTEGER*4  GTYP              !
        INTEGER*4  CLERK             !
        INTEGER*4  I                 !
        INTEGER*4  J                 !
        INTEGER*4  OFF               !
        INTEGER*4  SALIND            !
        INTEGER*4  WORK_SALIND       !
        INTEGER*4  TMPCNT            !
        INTEGER*4  TMPAMT(2)         !
        INTEGER*4  CLRKNUM

        ! Auxiliary variables for 8-byte integer
        INTEGER*8 I8TMPAMT
        INTEGER*4 I4TMPCNT
        
        ! function(s)
        LOGICAL  NO_JOK_DET  ! omit Jok detail if Jok dead > 90 days
        EXTERNAL NO_JOK_DET 

  
        I8TMPAMT = KZEXT(0)
        I4TMPCNT = 0

        ! to delete...
        TMPCNT    = 0
        TMPAMT(1) = 0
        TMPAMT(2) = 0

C
C TOTAL SUMMARY REPORT
C
        IF(SUBCLASS .EQ. 0 .OR. SUBCLASS .EQ. 8) THEN  !TODAY OR W. T. D.
            DO 100 GNUM = 1, MAXGAM
                GTYP = GNTTAB(GAMTYP,GNUM)
                !IF (GTYP.EQ.TPAS) THEN
                !   SALIND = 5       
                !   GOTO 110              !ONLY VALIDATIONS
                !ENDIF        
                IF(GTYP .LE. 0 .OR. GTYP .EQ. TINS) THEN
                  SALIND = 7
                  GOTO 100
                ENDIF
                IF(GTYP.EQ.TKIK.AND.NO_JOK_DET()) GOTO 100
                SALIND = 1
C
C GROSS SALES = GROSS SALES   !NOT USED IN FINLAND
C
C***            SALES(SALIND)=SALES(SALIND)+AGTGAM(GSCNT,GNUM,RTER)    ! CNT
C***            SALIND=SALIND+1
C***            SALES(SALIND)=SALES(SALIND)+AGTGAM(GSAMT,GNUM,RTER)    ! AMT
C***            SALIND=SALIND+1


C
C CANCELLATIONS = CANCELLATIONS
C
                SALES(SALIND) = SALES(SALIND) + AGTGAM(GCCNT,GNUM,RTER) ! AMT
                SALIND = SALIND+1
C***            TMPCNT = TMPCNT+AGTGAM(GCCNT,GNUM,RTER)
                SALES(SALIND)=SALES(SALIND)+AGTGAM(GCAMT,GNUM,RTER)     ! CNT
                SALIND=SALIND+1
             

C
C NET SALES = GROSS SALES - CANCELLATIONS
C
                SALES(SALIND)=SALES(SALIND)+AGTGAM(GSCNT,GNUM,RTER)-   ! CNT
     *                                      AGTGAM(GCCNT,GNUM,RTER)
                SALIND=SALIND+1
                TMPCNT=TMPCNT+AGTGAM(GSCNT,GNUM,RTER)-
     *                        AGTGAM(GCCNT,GNUM,RTER)
                SALES(SALIND)=SALES(SALIND)+AGTGAM(GSAMT,GNUM,RTER)-   ! AMT
     *                                      AGTGAM(GCAMT,GNUM,RTER)
                SALIND=SALIND+1
C               CALL ADDI8I4(TMPAMT,AGTGAM(GSAMT,GNUM,RTER),BETUNIT)
C               CALL SUBI8I4(TMPAMT,AGTGAM(GCAMT,GNUM,RTER),BETUNIT)
                CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,AGTGAM(GSAMT,GNUM,RTER))
                CALL I8SUBI8I4(I8TMPAMT,
     *                         I8TMPAMT,AGTGAM(GCAMT,GNUM,RTER))

C
C CASH = VALIDATION + REFUNDS
C
110             CONTINUE
                SALES(SALIND)=SALES(SALIND)+AGTGAM(GVCNT,GNUM,RTER) + ! AMT
     *                                      AGTGAM(GRCNT,GNUM,RTER)
                SALIND=SALIND+1
                TMPCNT=TMPCNT+AGTGAM(GVCNT,GNUM,RTER) +
     *                        AGTGAM(GRCNT,GNUM,RTER)
                SALES(SALIND)=SALES(SALIND)+AGTGAM(GVAMT,GNUM,RTER) + ! CNT
     *                                      AGTGAM(GRAMT,GNUM,RTER)
                SALIND=SALIND+1
C               CALL SUBI8I4(TMPAMT,AGTGAM(GVAMT,GNUM,RTER),VALUNIT)
C               CALL SUBI8I4(TMPAMT,AGTGAM(GRAMT,GNUM,RTER),VALUNIT)
                CALL I8SUBI8I4(I8TMPAMT,
     *                         I8TMPAMT,AGTGAM(GVAMT,GNUM,RTER))
                CALL I8SUBI8I4(I8TMPAMT,
     *                         I8TMPAMT,AGTGAM(GRAMT,GNUM,RTER))
100         CONTINUE


C
C Instant sales not in use...
C
             CAll FASTSET(0,SALES(SALIND),2)
             SALIND = SALIND + 2

C
C Instant validations.
C
            CLRKNUM = AGTHTB(AGTPASOFF,RTER)
            IF(CLRKNUM.LE.0) CLRKNUM = 1
            SALES(SALIND)=SALES(SALIND)+AGTMIS(CLRKNUM,1,RTER)
            TMPCNT=TMPCNT+AGTMIS(CLRKNUM,1,RTER)
            SALIND = SALIND + 1
            SALES(SALIND)=SALES(SALIND)+AGTMIS(CLRKNUM,2,RTER)
            SALIND=SALIND+1
C            CALL SUBI8I4(TMPAMT,AGTMIS(CLRKNUM,2,RTER),VALUNIT)
            CALL I8SUBI8I4(I8TMPAMT,
     *                     I8TMPAMT,AGTMIS(CLRKNUM,2,RTER))

C
C MISC SALES TXNS.
C
            OFF=PRM_NUMINS*2
            DO I=1,PRM_NUMISAL
                WORK_SALIND=SALIND
                SALES(WORK_SALIND)=SALES(WORK_SALIND)+AGTMIS(I+OFF,1,RTER)
                WORK_SALIND=WORK_SALIND+1
                TMPCNT=TMPCNT+AGTMIS(I+OFF,1,RTER)
                SALES(WORK_SALIND)=SALES(WORK_SALIND)+AGTMIS(I+OFF,2,RTER)
                WORK_SALIND=WORK_SALIND+1
C               CALL ADDI8I4(TMPAMT,AGTMIS(I+OFF,2,RTER),BETUNIT)
                CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,AGTMIS(I+OFF,2,RTER))
            END DO
            SALIND=WORK_SALIND


C
C PETTY CASH TXNS.
C
            OFF=PRM_NUMINS*2+PRM_NUMISAL
            DO I=1,PRM_NUMIPTY
                WORK_SALIND=SALIND
                SALES(WORK_SALIND)=SALES(WORK_SALIND)+AGTMIS(I+OFF,1,RTER)
                WORK_SALIND=WORK_SALIND+1
                TMPCNT=TMPCNT+AGTMIS(I+OFF,1,RTER)     
                SALES(WORK_SALIND)=SALES(WORK_SALIND)+AGTMIS(I+OFF,2,RTER)
                WORK_SALIND=WORK_SALIND+1
C               CALL SUBI8I4(TMPAMT,AGTMIS(I+OFF,2,RTER),BETUNIT)
                CALL I8SUBI8I4(I8TMPAMT,
     *                         I8TMPAMT,AGTMIS(I+OFF,2,RTER))
            END DO
            SALIND=WORK_SALIND
            CDC=DAYCDC

        ENDIF


C
C IF SHOP OWNER GET EVERYONE OF HIS CLERKS ACCOUNTS
C
C       IF(MANAGER.AND.P(CLRKACT).EQ.0)  THEN   !GET CLERKS ACCOUNTS ALSO
        IF(MANAGER) THEN   !GET CLERKS ACCOUNTS ALSO
            DO 130 CLERK = 2, 8
                IF(AGTTAB((APSNUM+CLERK)-1,RTER).EQ.0) GOTO 130
                DO 132 GNUM=1,MAXGAM
                    GTYP=GNTTAB(GAMTYP,GNUM)
                    !IF (GTYP.EQ.TPAS) THEN
                    !   SALIND = 5       
                    !   GOTO 133              !ONLY VALIDATIONS
                    !ENDIF        
                    IF(GTYP.LE.0.OR.GTYP.EQ.TINS) THEN
                      SALIND=7
                      GOTO 132
                    ENDIF
                    IF(GTYP.EQ.TKIK.AND.NO_JOK_DET()) GOTO 132
                    SALIND=1


C***            SALES(SALIND)=SALES(SALIND)+CLRKDAY(GSCNT,GNUM,CLERK) !NO GROSS
C***            SALIND=SALIND+1                                       !SALES IN
C***            SALES(SALIND)=SALES(SALIND)+CLRKDAY(GSAMT,GNUM,CLERK) !FINLAND
C***            SALIND=SALIND+1


                    SALES(SALIND)=SALES(SALIND)+CLRKDAY(GCCNT,GNUM,CLERK)
C***                TMPCNT=TMPCNT+CLRKDAY(GCCNT,GNUM,CLERK)
                    SALIND=SALIND+1
                    SALES(SALIND)=SALES(SALIND)+CLRKDAY(GCAMT,GNUM,CLERK)
                    SALIND=SALIND+1


                    SALES(SALIND)=SALES(SALIND)+CLRKDAY(GSCNT,GNUM,CLERK)-
     *                                          CLRKDAY(GCCNT,GNUM,CLERK)
                    TMPCNT=TMPCNT+CLRKDAY(GSCNT,GNUM,CLERK)-
     *                            CLRKDAY(GCCNT,GNUM,CLERK)
                    SALIND=SALIND+1
                    SALES(SALIND)=SALES(SALIND)+CLRKDAY(GSAMT,GNUM,CLERK)-
     *                                          CLRKDAY(GCAMT,GNUM,CLERK)
C                   CALL ADDI8I4(TMPAMT,CLRKDAY(GSAMT,GNUM,CLERK),BETUNIT)
C                   CALL SUBI8I4(TMPAMT,CLRKDAY(GCAMT,GNUM,CLERK),BETUNIT)
                    CALL I8ADDI8I4(I8TMPAMT,
     *                             I8TMPAMT,CLRKDAY(GSAMT,GNUM,CLERK))
                    CALL I8SUBI8I4(I8TMPAMT,
     *                             I8TMPAMT,CLRKDAY(GCAMT,GNUM,CLERK))
                    SALIND=SALIND+1

133                 CONTINUE
                    SALES(SALIND)=SALES(SALIND)+CLRKDAY(GVCNT,GNUM,CLERK) +
     *                                          CLRKDAY(GRCNT,GNUM,CLERK)
                    TMPCNT=TMPCNT+CLRKDAY(GVCNT,GNUM,CLERK) +
     *                            CLRKDAY(GRCNT,GNUM,CLERK)
                    SALIND=SALIND+1
                    SALES(SALIND)=SALES(SALIND)+CLRKDAY(GVAMT,GNUM,CLERK) +
     *                                          CLRKDAY(GRAMT,GNUM,CLERK)
C                   CALL SUBI8I4(TMPAMT,CLRKDAY(GVAMT,GNUM,CLERK),VALUNIT)
C                   CALL SUBI8I4(TMPAMT,CLRKDAY(GRAMT,GNUM,CLERK),VALUNIT)
                    CALL I8SUBI8I4(I8TMPAMT,
     *                             I8TMPAMT,CLRKDAY(GVAMT,GNUM,CLERK))
                    CALL I8SUBI8I4(I8TMPAMT,
     *                             I8TMPAMT,CLRKDAY(GRAMT,GNUM,CLERK))
                    SALIND=SALIND+1
132             CONTINUE


C
C Instant sales not in use....
C
                CALL FASTSET(0,SALES(SALIND),2)
                SALIND = SALIND + 2
C
C Instant validations
C
                SALES(SALIND)=SALES(SALIND)+CLRKMIS(CLERK,1,CLERK)
                TMPCNT=TMPCNT+CLRKMIS(CLERK,1,CLERK)
                SALIND=SALIND+1
                SALES(SALIND)=SALES(SALIND)+CLRKMIS(CLERK,2,CLERK)
C               CALL SUBI8I4(TMPAMT,CLRKMIS(CLERK,2,CLERK),VALUNIT)
                CALL I8SUBI8I4(I8TMPAMT,
     *                         I8TMPAMT,CLRKMIS(CLERK,2,CLERK))
                SALIND=SALIND+1

                OFF=PRM_NUMINS*2
                DO I=1,PRM_NUMISAL  !MISCELLANEOUS ITEMS
                    WORK_SALIND=SALIND
                    SALES(WORK_SALIND)=SALES(WORK_SALIND)+
     *                                 CLRKMIS(I+OFF,1,CLERK)
                    TMPCNT=TMPCNT+CLRKMIS(I+OFF,1,CLERK)
                    WORK_SALIND=WORK_SALIND+1
                    SALES(WORK_SALIND)=SALES(WORK_SALIND)+
     *                                 CLRKMIS(I+OFF,2,CLERK)
C                   CALL ADDI8I4(TMPAMT,CLRKMIS(I+OFF,2,CLERK),BETUNIT)
                    CALL I8ADDI8I4(I8TMPAMT,
     *                             I8TMPAMT,CLRKMIS(I+OFF,2,CLERK))
                    WORK_SALIND=WORK_SALIND+1
                END DO
                SALIND=WORK_SALIND


                OFF=OFF+PRM_NUMISAL
                DO I=1,PRM_NUMIPTY  !PETTY CASH
                    WORK_SALIND=SALIND
                    SALES(WORK_SALIND)=SALES(WORK_SALIND)+
     *                                 CLRKMIS(I+OFF,1,CLERK)
                    TMPCNT=TMPCNT+CLRKMIS(I+OFF,1,CLERK)
                    WORK_SALIND=WORK_SALIND+1
                    SALES(WORK_SALIND)=SALES(WORK_SALIND)+
     *                                 CLRKMIS(I+OFF,2,CLERK)
C                   CALL SUBI8I4(TMPAMT,CLRKMIS(I+OFF,2,CLERK),BETUNIT)
                    CALL I8ADDI8I4(I8TMPAMT,
     *                             I8TMPAMT,CLRKMIS(I+OFF,2,CLERK))
                    WORK_SALIND=WORK_SALIND+1
                END DO
                SALIND=WORK_SALIND

130         CONTINUE

            CDC=DAYCDC

        ENDIF


C
C IF WEEK TO DATE ALSO ACCUMULATE WEEK TO DATE
C
        IF(SUBCLASS.EQ.8) THEN
            DO 170 I= 1,9
                IF(ASFDAT(ASFCDC,I).LE.ASFINV(ASFEND,1)) GOTO 170
                DO 150 GNUM=1,MAXGAM
                    GTYP=GNTTAB(GAMTYP,GNUM)
                    !IF (GTYP.EQ.TPAS) THEN
                    !   SALIND = 5       
                    !   GOTO 140              !ONLY VALIDATIONS
                    !ENDIF        
                    IF(GTYP.LE.0.OR.GTYP.EQ.TINS) THEN
                      SALIND=7
                      GOTO 150
                    ENDIF
                    IF(GTYP.EQ.TKIK.AND.NO_JOK_DET()) GOTO 150
                    SALIND=1


C***            SALES(SALIND)=SALES(SALIND)+ASFDAY(GSCNT,GNUM,I)  !NO GROSS
C***            SALIND=SALIND+1                                   !SALES IN
C***            SALES(SALIND)=SALES(SALIND)+ASFDAY(GSAMT,GNUM,I)  !FINLAND
C***            SALIND=SALIND+1


                    SALES(SALIND)=SALES(SALIND)+ASFDAY(GCCNT,GNUM,I)
C***                TMPCNT=TMPCNT+ASFDAY(GCCNT,GNUM,I)
                    SALIND=SALIND+1
                    SALES(SALIND)=SALES(SALIND)+ASFDAY(GCAMT,GNUM,I)
                    SALIND=SALIND+1


                    SALES(SALIND)=SALES(SALIND)+ASFDAY(GSCNT,GNUM,I)-
     *                                          ASFDAY(GCCNT,GNUM,I)
                    TMPCNT=TMPCNT+ASFDAY(GSCNT,GNUM,I)-
     *                            ASFDAY(GCCNT,GNUM,I)
                    SALIND=SALIND+1
                    SALES(SALIND)=SALES(SALIND)+ASFDAY(GSAMT,GNUM,I)-
     *                                          ASFDAY(GCAMT,GNUM,I)
C                   CALL ADDI8I4(TMPAMT,ASFDAY(GSAMT,GNUM,I),BETUNIT)
C                   CALL SUBI8I4(TMPAMT,ASFDAY(GCAMT,GNUM,I),BETUNIT)
                    CALL I8ADDI8I4(I8TMPAMT,
     *                             I8TMPAMT,ASFDAY(GSAMT,GNUM,I))
                    CALL I8SUBI8I4(I8TMPAMT,
     *                             I8TMPAMT,ASFDAY(GCAMT,GNUM,I))
                    SALIND=SALIND+1

140                 CONTINUE
                    SALES(SALIND)=SALES(SALIND)+ASFDAY(GVCNT,GNUM,I) +
     *                                          ASFDAY(GRCNT,GNUM,I)
                    TMPCNT=TMPCNT+ASFDAY(GVCNT,GNUM,I) +
     *                            ASFDAY(GRCNT,GNUM,I)
                    SALIND=SALIND+1
                    SALES(SALIND)=SALES(SALIND)+ASFDAY(GVAMT,GNUM,I) +
     *                                          ASFDAY(GRAMT,GNUM,I)
C                   CALL SUBI8I4(TMPAMT,ASFDAY(GVAMT,GNUM,I),VALUNIT)
C                   CALL SUBI8I4(TMPAMT,ASFDAY(GRAMT,GNUM,I),VALUNIT)
                    CALL I8SUBI8I4(I8TMPAMT,
     *                             I8TMPAMT,ASFDAY(GVAMT,GNUM,I))
                    CALL I8SUBI8I4(I8TMPAMT,
     *                             I8TMPAMT,ASFDAY(GRAMT,GNUM,I))
                    SALIND=SALIND+1
150             CONTINUE
C
C Instant sales not in use....
C
                CALL FASTSET(0,SALES(SALIND),2)
                SALIND = SALIND + 2
C
C Instant validations
C
                DO CLERK=1,NUMCLERK
                   IF(AGTTAB((APSNUM+CLERK)-1,RTER).EQ.0) GOTO 160
                   SALES(SALIND)=SALES(SALIND)+ASFMIS(CLERK,1,I)
                   TMPCNT=TMPCNT+ASFMIS(CLERK,1,I)
                   SALES(SALIND+1)=SALES(SALIND+1)+ASFMIS(CLERK,2,I)
C                  CALL SUBI8I4(TMPAMT,ASFMIS(CLERK,2,I),VALUNIT)
                   CALL I8SUBI8I4(I8TMPAMT,
     *                             I8TMPAMT,ASFMIS(CLERK,2,I))
160                CONTINUE
                ENDDO
                SALIND=SALIND+2
C
                OFF=2*PRM_NUMINS
                DO J=1,PRM_NUMISAL  !MISCELLANEOUS ITEMS
                    WORK_SALIND=SALIND
                    SALES(WORK_SALIND)=SALES(WORK_SALIND)+ASFMIS(J+OFF,1,I)
                    TMPCNT=TMPCNT+ASFMIS(J+OFF,1,I)
                    WORK_SALIND=WORK_SALIND+1
                    SALES(WORK_SALIND)=SALES(WORK_SALIND)+ASFMIS(J+OFF,2,I)
C                   CALL ADDI8I4(TMPAMT,ASFMIS(J+OFF,2,I),BETUNIT)
                    CALL I8ADDI8I4(I8TMPAMT,
     *                             I8TMPAMT,ASFMIS(J+OFF,2,I))
                    WORK_SALIND=WORK_SALIND+1
                END DO
                SALIND=WORK_SALIND


                OFF=OFF+PRM_NUMISAL
                DO J=1,PRM_NUMIPTY  !PETTY CASH
                    WORK_SALIND=SALIND
                    SALES(WORK_SALIND)=SALES(WORK_SALIND)+ASFMIS(J+OFF,1,I)
                    TMPCNT=TMPCNT+ASFMIS(J+OFF,1,I)
                    WORK_SALIND=WORK_SALIND+1
                    SALES(WORK_SALIND)=SALES(WORK_SALIND)+ASFMIS(J+OFF,2,I)
C                   CALL SUBI8I4(TMPAMT,ASFMIS(J+OFF,2,I),BETUNIT)
                    CALL I8SUBI8I4(I8TMPAMT,
     *                             I8TMPAMT,ASFMIS(J+OFF,2,I))
                    WORK_SALIND=WORK_SALIND+1
                END DO
               SALIND=WORK_SALIND

170         CONTINUE

            CDC=DAYCDC

        ENDIF


C
C PROCESS FOR DAY REQUESTED
C
        IF(SUBCLASS.GE.1.AND.SUBCLASS.LE.7) THEN
            DO I= 1,9
                IF(ASFDAT(ASFDOW,I).EQ.SUBCLASS) GOTO 180
            END DO
            TRABUF(TERR)=INVL
            GOTO 8000
C
180         CONTINUE
            DO 182 GNUM=1,MAXGAM
                GTYP=GNTTAB(GAMTYP,GNUM)
                !IF (GTYP.EQ.TPAS) THEN
                !   SALIND = 5       
                !   GOTO 181              !ONLY VALIDATIONS
                !ENDIF        
                IF(GTYP.LE.0.OR.GTYP.EQ.TINS) THEN
                  SALIND=7
                  GOTO 182
                ENDIF
                IF(GTYP.EQ.TKIK.AND.NO_JOK_DET()) GOTO 182
                SALIND=1


C***         SALES(SALIND)=SALES(SALIND)+ASFDAY(GSCNT,GNUM,I)  !NO GROSS SALES
C***         SALIND=SALIND+1                                   !SALES FOR
C***         SALES(SALIND)=SALES(SALIND)+ASFDAY(GSAMT,GNUM,I)  !FINLAND
C***         SALIND=SALIND+1


                SALES(SALIND)=SALES(SALIND)+ASFDAY(GCCNT,GNUM,I)
C***            TMPCNT=TMPCNT+ASFDAY(GCCNT,GNUM,I)
                SALIND=SALIND+1
                SALES(SALIND)=SALES(SALIND)+ASFDAY(GCAMT,GNUM,I)
                SALIND=SALIND+1


                SALES(SALIND)=SALES(SALIND)+ASFDAY(GSCNT,GNUM,I)-
     *                                      ASFDAY(GCCNT,GNUM,I)
                TMPCNT=TMPCNT+ASFDAY(GSCNT,GNUM,I)-
     *                        ASFDAY(GCCNT,GNUM,I)
                SALIND=SALIND+1
                SALES(SALIND)=SALES(SALIND)+ASFDAY(GSAMT,GNUM,I)-
     *                                      ASFDAY(GCAMT,GNUM,I)
C               CALL ADDI8I4(TMPAMT,ASFDAY(GSAMT,GNUM,I),BETUNIT)
C               CALL SUBI8I4(TMPAMT,ASFDAY(GCAMT,GNUM,I),BETUNIT)
                CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,ASFDAY(GSAMT,GNUM,I))
                CALL I8SUBI8I4(I8TMPAMT,
     *                         I8TMPAMT,ASFDAY(GCAMT,GNUM,I))
                SALIND=SALIND+1

181             CONTINUE
                SALES(SALIND)=SALES(SALIND)+ASFDAY(GVCNT,GNUM,I) +
     *                                      ASFDAY(GRCNT,GNUM,I)
                TMPCNT=TMPCNT+ASFDAY(GVCNT,GNUM,I) +
     *                        ASFDAY(GRCNT,GNUM,I)
                SALIND=SALIND+1
                SALES(SALIND)=SALES(SALIND)+ASFDAY(GVAMT,GNUM,I) +
     *                                      ASFDAY(GRAMT,GNUM,I)
C               CALL SUBI8I4(TMPAMT,ASFDAY(GVAMT,GNUM,I),VALUNIT)
C               CALL SUBI8I4(TMPAMT,ASFDAY(GRAMT,GNUM,I),VALUNIT)
                CALL I8SUBI8I4(I8TMPAMT,
     *                         I8TMPAMT,ASFDAY(GVAMT,GNUM,I))
                CALL I8SUBI8I4(I8TMPAMT,
     *                         I8TMPAMT,ASFDAY(GRAMT,GNUM,I))
                SALIND=SALIND+1
182         CONTINUE

C
C Instant sales not in use....
C
            CALL FASTSET(0,SALES(SALIND),2)
            SALIND = SALIND + 2
C
C Instant validations
C
            DO CLERK=1,NUMCLERK
               IF(AGTTAB((APSNUM+CLERK)-1,RTER).EQ.0) GOTO 190
               SALES(SALIND)=SALES(SALIND)+ASFMIS(CLERK,1,I)
               TMPCNT=TMPCNT+ASFMIS(CLERK,1,I)
               SALES(SALIND+1)=SALES(SALIND+1)+ASFMIS(CLERK,2,I)
C              CALL SUBI8I4(TMPAMT,ASFMIS(CLERK,2,I),VALUNIT)
               CALL I8SUBI8I4(I8TMPAMT,
     *                        I8TMPAMT,ASFMIS(CLERK,2,I))
190            CONTINUE
            ENDDO
            SALIND=SALIND+2
C
            OFF=2*PRM_NUMINS
            DO J=1,PRM_NUMISAL  !MISCELLANEOUS SALES
                WORK_SALIND=SALIND
                SALES(WORK_SALIND)=SALES(WORK_SALIND)+ASFMIS(J+OFF,1,I)
                TMPCNT=TMPCNT+ASFMIS(J+OFF,1,I)
                WORK_SALIND=WORK_SALIND+1
                SALES(WORK_SALIND)=SALES(WORK_SALIND)+ASFMIS(J+OFF,2,I)
C               CALL ADDI8I4(TMPAMT,ASFMIS(J+OFF,2,I),BETUNIT)
                CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,ASFMIS(J+OFF,2,I))
                WORK_SALIND=WORK_SALIND+1
            END DO
            SALIND=WORK_SALIND


            OFF=OFF+PRM_NUMISAL
            DO J=1,PRM_NUMIPTY  !PETTY CASH
                WORK_SALIND=SALIND
                SALES(WORK_SALIND)=SALES(WORK_SALIND)+ASFMIS(J+OFF,1,I)
                TMPCNT=TMPCNT+ASFMIS(J+OFF,1,I)
                WORK_SALIND=WORK_SALIND+1
                SALES(WORK_SALIND)=SALES(WORK_SALIND)+ASFMIS(J+OFF,2,I)
C               CALL SUBI8I4(TMPAMT,ASFMIS(J+OFF,2,I),BETUNIT)
                CALL I8SUBI8I4(I8TMPAMT,
     *                         I8TMPAMT,ASFMIS(J+OFF,2,I))
                WORK_SALIND=WORK_SALIND+1
            END DO
            SALIND=WORK_SALIND

            CDC=ASFDAT(ASFCDC,I)
        ENDIF


C
C TOTAL SALES COUNT AND AMOUNT 
C
        SALES(SALIND)=SALES(SALIND)+TMPCNT  !TOTAL CASH COUNT
        SALIND=SALIND+1
C       CALL ADDI8I8(SALES(SALIND),TMPAMT,BETUNIT)
        CALL I8ADDI8I8(SALES(SALIND),
     *                 SALES(SALIND),I8TMPAMT)
C
C CHANGE TOTAL SALES AMOUNT ( FIRST MORE SIGNIFICANT VALUE )
C
        I = SALES(SALIND)
        SALES(SALIND) = SALES(SALIND + 1)
        SALES(SALIND + 1) = I
C
C SET OUTPUT VARIABLES
C
        SALIND=SALIND+2
        SALOFF=SALIND-1

8000    CONTINUE

        RETURN

        END
C----+------------------------------------------------------------------
C V04| Correction of SALSUM for Portugal
C----+------------------------------------------------------------------


C
C V05 NO_JOK_DET function
C
C Omit details of Joker financials if > 90 days since Joker deactivated
C
	LOGICAL FUNCTION NO_JOK_DET
        IMPLICIT NONE
       
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'


        INTEGER*4 KGAM, ST, FDB(7)

        LOGICAL FIRST/.TRUE./
        LOGICAL STATUS /.FALSE./

        KGAM = GTNTAB(TKIK,1)

        IF(DAYDRW(KGAM).EQ.0.AND.FIRST) THEN
           CALL OPENW(60,GFNAMES(1,KGAM),4,0,0,ST)
           CALL IOINIT(FDB,60,DKKSEC*256)
           IF(ST.NE.0) THEN
              CALL FILERR(GFNAMES(1,KGAM),1,ST,0)
              RETURN
           ENDIF
           CALL READW(FDB,DAYHDR(KGAM),DKKREC,ST)  !read last ever Jok draw
           IF(ST.NE.0) THEN
              CALL FILERR(GFNAMES(1,KGAM),2,ST,DAYHDR(KGAM))
              RETURN
           ENDIF

           ! check if today > last draw date was + purge days
           IF(DAYCDC.GT.DKKDAT(CURDRW)+PRGDAY(KGAM)) THEN
              STATUS = .TRUE.
           ENDIF
           CALL CLOSEFIL(FDB)

        ENDIF
        
        FIRST = .FALSE.

        NO_JOK_DET = STATUS

        END

