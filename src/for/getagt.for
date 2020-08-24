C
C GETAGT.FOR
C
C V13 04-DEZ-2014 SCML MXTerminals DESFLG not depends on ASF (set AGTDES)
C V12 31-MAY-2011 FJG MXTerminals haven't default: DESFLG depends on ASF
C V11 12-APR-2011 FJG MXTerminals withput DESFLG
C V10 29-NOV-2010 FRP Lotto2 Changes
C     08-FEB-2011 FJG REDMAX changes
C V09 01-JAN-10 FJG ePassive - AGTFLGs for Passive
C V08 15-MAR-10 RXK  OFFSETS AGTEPS,AGTEPC INITIALIZED
C V07 23-MAR-01 CS   INCLUDED SAP CONTROL
C V06 13-MAR-01 EPH  Fix IN_WEEK_INTERVAL
C V05 07-MAR-01 EPH  Fill AGTSAP with SAP number
C V04 13-FEB-01 EPH  Treat suspension and new flags for SCML
C V03 02-FEB-92 GCAN ADDED SETTING OF LANGUAGE CODE
C V02 07-OCT-91 MTK  INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C $Log:   GXAFIP:[GOLS]GETAGT.FOV  $
C
C     Rev 1.3   03 Feb 1997 21:02:08   WPW
C  Changes for downloading GVTs.
C  
C     Rev 1.2   13 Jan 1997 16:44:26   RXK
C  GVT id online change added
C  
C X2X Upgrade: 03-MAR-96 wsm Revert X2CHKDATE() arguments to x2x baseline ver.
C
C     Rev 1.1   30 May 1996  9:27:08   HXK
C  Further update from Mike Pindrik, Wojtek
C  
C     Rev 1.0   17 Apr 1996 13:18:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C
C     Rev 1.9   12 Feb 1996 12:48:14   RXK
C  Automatic invoice flag now dependent on weekly update count ASFWCT
C
C     Rev 1.8   24 Apr 1995 17:01:38   HXK
C  Merge of V5 development with March 10th 1995 bible
C  
C     Rev 1.8   18 Feb 1995 20:06:18   HXK
C  Changes for V5 game
C  
C     Rev 1.7   15 Oct 1994 16:37:04   HXK
C  Adding /developing Bingo (15.Oct.94)
C  
C     Rev 1.6   04 Nov 1993 12:11:08   HXK
C  CHANGE FOR 5 PENNY UNITS FOR REDMAX.
C  
C     Rev 1.5   18 Sep 1993  1:19:36   JWE
C  Change calling parameters of X2CHKDATE
C  
C     Rev 1.4   17 Sep 1993 21:07:38   GXA
C  Initialize Second Kicker Number to -1.
C  
C     Rev 1.3   26 Aug 1993 18:27:00   SXH
C  Set Agent REDMAX to Default REDMAX if not set.
C  
C     Rev 1.2   16 Jun 1993 16:24:18   SXH
C  Released for Finland
C  
C     Rev 1.1   11 Jun 1993 17:38:46   HXK
C  MOVED AGTINF.DEF
C  
C     Rev 1.0   21 Jan 1993 16:24:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C SUBROUTINE TO EXTRACT AGENT INFORMATION FROM THE ASF FOR RUNSYS.
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C====== OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE GETAGT(UNIT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
C
C
        ! argument
        INTEGER*4  UNIT               !

        ! variables
	INTEGER*4  I                  !
	INTEGER*4  LANG               !
	INTEGER*4  RMAX               !
	INTEGER*4  RMIN               !
	INTEGER*4  ANUM               !
	INTEGER*4  PASNUM             !
	INTEGER*4  CERR               !
	INTEGER*4  ATYPE              !
	INTEGER*4  J                  !
	INTEGER*4  ST                 !
	INTEGER*4  AGT                !
        INTEGER*4  CARTEL             !
	INTEGER*4  SAPNUM

	INTEGER*4  GAM
	INTEGER*4  AGAME
	INTEGER*4  GTYPE

	INTEGER*4  BEGW, ENDW
	INTEGER*4  MESS_CNT

	LOGICAL    IN_CDC_INTERVAL   !FUNCTION

	LOGICAL    MUTUAS_OUT, PASSIVE_OUT
	LOGICAL    SUPMUT, UNSUPMUT, 
     *             SUPPAS, UNSUPPAS
C
	CHARACTER X2FILNAM*20	    !FILENAME FUNCTION
	CHARACTER CZERO/Z0/

	INTEGER*4 X2XADR1, X2XADR2
C
	CHARACTER   AGENT_STRING*(ALENGTH)
	EQUIVALENCE (AGENT_STRING, ASFBYT)

	MESS_CNT = 0
C
C READ ALL AGENTS
C
	AGT_LOOKUP_CNT = 0	    !initialize counter		
C
	TYPE*,IAM(),' Loading Agent information'
	CALL OPENASF(UNIT)
	CALL OPENX2X(X2FILNAM(XTER),UNIT+1)
C
	DO 100 AGT = 1, NUMAGT
	    CALL READASF(AGT,ASFREC,ST)
	    IF(ST.NE.0) GOTO 110
	    IF(MOD(AGT,1000).EQ.0) TYPE*,IAM(),AGT,' agents processed'
C
C CHECK IF ACTIVE AGENT
C
	    DO J = SAGNO, EAGNO
	        IF(ASFBYT(J).NE.CZERO.AND.ASFBYT(J).NE.' ') GOTO 30
            END DO

	    CALL ASCBIN(ASFINF,STTYP,LTTYP,ATYPE,CERR)
	    IF(CERR.EQ.0) AGTTAB(AGTTYP,AGT)=ATYPE
C           CALL BCLR(AGTTAB(AGTTYP,AGT),AGTDES)
	    CALL BSET(AGTTAB(AGTTYP,AGT),AGTDES)
	    GOTO 100

30	    CONTINUE
	    PASNUM=0
	    ANUM=0
	    ATYPE=0
	    AGTHTB(AOPSTS,AGT) =SIGNOF
	    AGTHTB(ACHKSM,AGT) =-1
	    AGTTAB(AGTLKN,AGT) =-1
	    AGTTAB(AGTLKN2,AGT)=-1
            AGTTAB(AGTBSED,AGT)=-1
            AGTTAB(AGTEPS,AGT)=-1
            AGTTAB(AGTEPC,AGT)=0
C
C EXTRACT PASS NUMBERS AND AGENT NUMBER AND AGENT TYPE
C
	    CALL ASCBIN(ASFINF,SPAS1,LPAS1,PASNUM,CERR)
	    IF(CERR.LT.0) WRITE(5,903) IAM(),AGT
	    AGTTAB(APSNUM,AGT)=PASNUM
	    PASNUM=0

	    IF(P(CLRKACT).EQ.0) THEN
                CALL ASCBIN(ASFINF,SPAS2,LPAS2,PASNUM,CERR)   !CLERK 2
                IF(CERR.LT.0) WRITE(5,903) IAM(),AGT,2
                AGTTAB(APSNUM+1,AGT)=PASNUM

                PASNUM=0
	        CALL ASCBIN(ASFINF,SPAS3,LPAS3,PASNUM,CERR)   !CLERK 3
	        IF(CERR.LT.0) WRITE(5,903) IAM(),AGT,3
	        AGTTAB(APSNUM+2,AGT)=PASNUM

	        PASNUM=0
	        CALL ASCBIN(ASFINF,SPAS4,LPAS4,PASNUM,CERR)   !CLERK 4
	        IF(CERR.LT.0) WRITE(5,903) IAM(),AGT,4
	        AGTTAB(APSNUM+3,AGT)=PASNUM

	        PASNUM=0
	        CALL ASCBIN(ASFINF,SPAS5,LPAS5,PASNUM,CERR)   !CLERK 5
	        IF(CERR.LT.0) WRITE(5,903) IAM(),AGT,5
	        AGTTAB(APSNUM+4,AGT)=PASNUM

	        PASNUM=0
	        CALL ASCBIN(ASFINF,SPAS6,LPAS6,PASNUM,CERR)   !CLERK 6
	        IF(CERR.LT.0) WRITE(5,903) IAM(),AGT,6
	        AGTTAB(APSNUM+5,AGT)=PASNUM

	        PASNUM=0
	        CALL ASCBIN(ASFINF,SPAS7,LPAS7,PASNUM,CERR)   !CLERK 7
	        IF(CERR.LT.0) WRITE(5,903) IAM(),AGT,7
	        AGTTAB(APSNUM+6,AGT)=PASNUM

	        PASNUM=0
	        CALL ASCBIN(ASFINF,SPAS8,LPAS8,PASNUM,CERR)   !CLERK 8
	        IF(CERR.LT.0) WRITE(5,903) IAM(),AGT,8
	        AGTTAB(APSNUM+7,AGT)=PASNUM
	    ENDIF
C
C
	    CALL ASCBIN(ASFINF,SAGNO,LAGNO,ANUM,CERR)
	    IF(CERR.LT.0) WRITE(5,904) IAM(),AGT
C
	    IF(ANUM.NE.0) THEN
	      AGT_LOOKUP_CNT=AGT_LOOKUP_CNT+1
	      AGT_LOOKUP_TER(AGT_LOOKUP_CNT)=AGT
	      AGT_LOOKUP_AGT(AGT_LOOKUP_CNT)=ANUM
	    ENDIF
C
	    CALL ASCBIN(ASFINF,STTYP,LTTYP,ATYPE,CERR)
	    AGTTAB(AGTNUM,AGT)=ANUM
	    AGTTAB(AGTTYP,AGT)=ATYPE
C===========IF NOT MXT PUT AGTDES. IF MXT LEAVE ASF CONFIG======================
C	    IF(.NOT.BTEST(AGTTAB(AGTTYP,AGT),AGTMXT)) THEN
C	      CALL BSET(AGTTAB(AGTTYP,AGT),AGTDES)
C	    ENDIF
C===========V42=================================================================
	    CALL BSET(AGTTAB(AGTTYP,AGT),AGTDES) !V13
	    
C           CALL BCLR(AGTTAB(AGTTYP,AGT),AGTTOI)            !V08
            CALL BSET(AGTTAB(AGTTYP,AGT),AGTTOI)            !V08
C
C GET SAP NUMBER
C
	    CALL ASCBIN(ASFINF,SSAPN,LSAPN,SAPNUM,CERR)
            IF (CERR.EQ.0 .AND. SAPNUM.GT.0) THEN
               AGTSAP(AGT) = SAPNUM
	    ELSE
	       WRITE(5,905) IAM(),AGT
            ENDIF
C
C IPS CHANGES
C
	    CALL BCLR(AGTTAB(AGTTYP,AGT),AGTISF)
	    AGTHTB(AGTCDC,AGT)=ASFGVT
	    AGTTAB(ADLTIM,AGT)=ASFGVTIM
	    AGTHTB(AGTNCDC,AGT)=ASFNCDC
	    AGTHTB(AGTLCDC,AGT)=ASFLCDC
	    IF(AGTHTB(AGTNCDC,AGT).NE.0) AGTHTB(AGTCBT,AGT)=ASFGVTIM
C
	    RMAX=0
	    CALL ASCBIN(ASFINF,SARED,LARED,RMAX,CERR) ! THIS IS IN MARKS
	    IF(RMAX.EQ.999999) RMAX = 21470000        ! SET TO THE VERY MAXIMUM FJG
            RMAX=RMAX*(100/DYN_VALUNIT)  ! CHANGE TO 5 PENNY UNITS
      	    AGTTAB(AGTRMX,AGT)=RMAX
C
C SET DEFAULT REDMAX IF AGENT'S IS NOT SET
C
C	    IF(RMAX.LE.0) AGTTAB(AGTRMX,AGT) = P(REDDEF)*(100/DYN_VALUNIT)
	    IF(RMAX.LE.0) AGTTAB(AGTRMX,AGT) = P(REDDEF)
C
C
	    RMIN=0
	    CALL ASCBIN(ASFINF,SARMN,LARMN,RMIN,CERR)
            RMIN=RMIN*(100/DYN_VALUNIT)   !CHANGE TO 5 PENNY UNITS
	    AGTTAB(AGTRMN,AGT)=RMIN
C
C
	    LANG=0
	    CALL ASCBIN(ASFINF,SLANG,LLANG,LANG,CERR)
	    AGTHTB(AGTLANG,AGT)=LANG
C
C
	    CARTEL=0
            CALL ASCBIN(ASFINF,SCHAN,LCHAN,CARTEL,CERR)
            IF(CERR.LT.0) WRITE(5,906) IAM(),AGT
            AGTCAR(AGT)=CARTEL
C
C GET GAME FLAGS
C
	    DO I = 1, MAXGAM
	        AGTGAM(GFLAGS,I,AGT)=ASFGFL(I)
            END DO
C
C CHECK FOR    BEGIN/END DATE    AND     BEGIN/END SUPRESSION DATE  FOR GAMES
C SET AGTWAG, AGTCAN, AGTVAL FLAGS ACCORDINGLY                              !v04
C
            MUTUAS_OUT = .FALSE. 
            CALL ASCBIN(ASFINF,SWBSU,LWBSU,BEGW,CERR)  
            CALL ASCBIN(ASFINF,SWESU,LWESU,ENDW,CERR)  
	    IF (IN_CDC_INTERVAL(DAYCDC,BEGW,ENDW)) THEN
               MUTUAS_OUT = .TRUE.
            ENDIF
            CALL ASCBIN(ASFINF,SWBSA,LWBSA,BEGW,CERR)  
            CALL ASCBIN(ASFINF,SWESA,LWESA,ENDW,CERR)  
	    IF (.NOT. IN_CDC_INTERVAL(DAYCDC,BEGW,ENDW)) THEN
               MUTUAS_OUT = .TRUE.
            ENDIF

            PASSIVE_OUT = .FALSE. 
            CALL ASCBIN(ASFINF,SPBSU,LPBSU,BEGW,CERR)  
            CALL ASCBIN(ASFINF,SPESU,LPESU,ENDW,CERR)  
	    IF (IN_CDC_INTERVAL(DAYCDC,BEGW,ENDW)) THEN
               PASSIVE_OUT = .TRUE.
            ENDIF
            CALL ASCBIN(ASFINF,SPBSA,LPBSA,BEGW,CERR)  
            CALL ASCBIN(ASFINF,SPESA,LPESA,ENDW,CERR)  
	    IF (.NOT. IN_CDC_INTERVAL(DAYCDC,BEGW,ENDW)) THEN
               PASSIVE_OUT = .TRUE.
            ENDIF

	    SUPMUT   = .FALSE.
            SUPPAS   = .FALSE.
            UNSUPMUT = .FALSE.
            UNSUPPAS = .FALSE.

	    DO GAM=1,MAXGAM

               GTYPE = GNTTAB(GAMTYP,GAM)
               AGAME = AGTGAM(GFLAGS, GAM, AGT)

               IF (GTYPE.EQ.TLTO .OR. GTYPE.EQ.TSPT .OR. GTYPE.EQ.TTGL) THEN
	          CALL UPDATE_GAME_FLAGS (MUTUAS_OUT, AGAME, GTYPE)
                  IF (AGAME.NE.AGTGAM(GFLAGS,GAM,AGT)) THEN
                     IF (MUTUAS_OUT) THEN
                        SUPMUT = .TRUE.
                     ELSE
                        UNSUPMUT = .TRUE.
                     ENDIF
                  ENDIF
               ENDIF
               IF (GTYPE.EQ.TPAS) THEN
	          CALL UPDATE_GAME_FLAGS (PASSIVE_OUT, AGAME, GTYPE)
                  IF (AGAME.NE.AGTGAM(GFLAGS,GAM,AGT)) THEN
                     IF (PASSIVE_OUT) THEN
                        SUPPAS = .TRUE.
                     ELSE
			UNSUPPAS = .TRUE.
                     ENDIF
                  ENDIF
               ENDIF

               AGTGAM(GFLAGS, GAM, AGT) = AGAME   !UPDATE WITH CHANGES THAT MAY OCCUR ON UPDATE_GAME_FLAGS

            ENDDO

C
C	    SHOW ONLY FOR ONLINE AGENTS (MAX 40 MESSAGES)
C
	    IF (MESS_CNT.LE.40 .AND. TSBIT(AGTTAB(AGTTYP,AGT),AGTTON)) THEN    
	    IF (SUPMUT) THEN
	       TYPE*,IAM(),' Suspending Mutuas for agent : ', AGTTAB(AGTNUM,AGT)
	       MESS_CNT = MESS_CNT + 1
            ENDIF
	    IF (UNSUPMUT) THEN
	       TYPE*,IAM(),' End of Mutuas Suspension for agent : ', AGTTAB(AGTNUM,AGT)
	       MESS_CNT = MESS_CNT + 1
            ENDIF
	    IF (SUPPAS) THEN
	       TYPE*,IAM(),' Suspending Passive for agent : ', AGTTAB(AGTNUM,AGT)
	       MESS_CNT = MESS_CNT + 1
            ENDIF
	    IF (UNSUPPAS) THEN
	       TYPE*,IAM(),' End of Passive Suspension for agent : ', AGTTAB(AGTNUM,AGT)
	       MESS_CNT = MESS_CNT + 1
            ENDIF
	    ENDIF

C
C GET INVOICE REPORT FLAG
C
CCC	    IF(ASFINV(ASFEND,1).EQ.DAYCDC-1.AND.
CCC  *         ASFINV(ASFEND,1).NE.0) AGTHTB(AINRPT,AGT)=1
	    IF(ASFWCT.EQ.0) AGTHTB(AINRPT,AGT) = 1
C
C CHECK THE START/END DATE TO DETERMINE WHETHER THE TERMINAL
C SHOULD BE ACTIVATED OR DISABLED.
C
C TEST FOR ACTIVATION ONLY IF IT IS AN ONLINE (NOT OFFLINE) TERMINAL
C OR HAS A X2XADDRES 
C
	    CALL ASCBIN(ASFINF,SXADR,7      ,X2XADR1,CERR)
	    CALL ASCBIN(ASFINF,SXADR,LXADR-7,X2XADR2,CERR)

	    IF ( TSBIT(AGTTAB(AGTTYP,AGT),AGTTON) .OR. X2XADR1.GT.0 .OR. X2XADR2.GT.0) THEN
	       CALL X2CHKDATE(UNIT+1,AGT,ASFINF)
	    ENDIF
C
C GET AGENT GVT ID NUMBERS
C
            CALL ATOH(ASFBYT(SGSER),1,LGSER,AGTTAB(AGTGVT1,AGT),CERR)
C
C
100	CONTINUE
110	CONTINUE

        CALL CLOSASF(UNIT)
C
C CLOSE THE X2X FILE.
C
	CALL CLOSX2X(UNIT+1)
C
C SORT THE AGENT LOOKUP TABLE
C
	IF(AGT_LOOKUP_CNT.NE.0) THEN
	  CALL I4SHELL(AGT_LOOKUP_TER,AGT_LOOKUP_CNT,AGT_LOOKUP_AGT,1)
	ENDIF
C
	RETURN
C
C
903	FORMAT(1X,A18,' Terminal ',I5,' invalid pass number ',I2)
904	FORMAT(1X,A18,' Terminal ',I5,' invalid agent number')
905     FORMAT(1X,A18,' Terminal ',I5,' invalid SAP number')
906     FORMAT(1X,A18,' Terminal ',I5,' invalid cartel number')
	END




C	******************************************************
	SUBROUTINE UPDATE_GAME_FLAGS (GAME_OUT, AGAME, GTYPE)
C	******************************************************
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'

        LOGICAL    GAME_OUT    !(INPUT)
        INTEGER*4  AGAME       !(INPUT/OUTPUT)    
	INTEGER*4  GTYPE

        IF (GAME_OUT) THEN
C          IF (.NOT.TSBIT(AGAME,AGTOUT2)) THEN    
C
C	      IT IS OUT FROM NOW ON (WASN'T BEFORE)
C
C	      SAVE  FLAGS
C
C             CALL BSET(AGAME,AGTOUT2)  !SHOW THAT THERE ARE FLAGS SAVED DUE TO SUPPRESSION
C             IF (TSBIT(AGAME,AGTWAG)) CALL BSET(AGAME,AGTWAG2)
C             IF (TSBIT(AGAME,AGTVAL)) CALL BSET(AGAME,AGTVAL2)
C             IF (TSBIT(AGAME,AGTCAN)) CALL BSET(AGAME,AGTCAN2)
C
C	      SUPPRESS FLAGS 
C
          IF (.NOT.TSBIT(AGAME,AGTWAG)) CALL BSET(AGAME,AGTWAG)  
	  IF (.NOT.TSBIT(AGAME,AGTCAN)) CALL BSET(AGAME,AGTCAN)                        
	  IF (GTYPE.NE.TPAS) THEN        ! FOR PASSIVE, NEVER SUPPRESS VALIDATIONS
            IF (.NOT.TSBIT(AGAME,AGTVAL)) CALL BSET(AGAME,AGTVAL)      
          ENDIF
C          ENDIF
        ELSE
          IF (TSBIT(AGAME,AGTWAG)) CALL BCLR(AGAME,AGTWAG)
          IF (TSBIT(AGAME,AGTCAN)) CALL BCLR(AGAME,AGTCAN)          
          IF (TSBIT(AGAME,AGTVAL)) CALL BCLR(AGAME,AGTVAL)
C          
C           IF (TSBIT(AGAME,AGTOUT2)) THEN    
C
C	      IT IS BACK TO WORK FROM NOW ON (WAS SUPPRESSED BEFORE)
C
C	      RETURN WITH FLAGS STATUSES BEFORE SUPPRESSION
C
C             CALL BCLR(AGAME,AGTWAG)  
C             CALL BCLR(AGAME,AGTVAL)                        
C             CALL BCLR(AGAME,AGTCAN)                        
C             IF (TSBIT(AGAME,AGTWAG2)) CALL BSET(AGAME,AGTWAG)
C             IF (TSBIT(AGAME,AGTVAL2)) CALL BSET(AGAME,AGTVAL)
C             IF (TSBIT(AGAME,AGTCAN2)) CALL BSET(AGAME,AGTCAN)
C
C	      CLEAR AREA FOR SAVED FLAGS
C
C             CALL BCLR(AGAME,AGTOUT2)  !NOTHING MORE SAVED HERE  
C             CALL BCLR(AGAME,AGTWAG2)  
C             CALL BCLR(AGAME,AGTVAL2)                        
C             CALL BCLR(AGAME,AGTCAN2)                        
C          ENDIF
        ENDIF   !GAME_OUT

	RETURN
	END                                    



C	********************************************************
	LOGICAL FUNCTION IN_CDC_INTERVAL (CDC, DAT1, DAT2)
C	********************************************************
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'

	INTEGER*4 CDC              !(INPUT)
	INTEGER*4 DAT1, DAT2      !(INPUT)

        INTEGER*2 DATES(12)
        INTEGER*4 DD,MM,YY,TT
        INTEGER*4 LDAT,LDAT1,LDAT2

	IN_CDC_INTERVAL = .FALSE.

	IF (DAT1.EQ.0 .AND. DAT2.EQ.0) THEN
           RETURN
        ENDIF

        DATES(VCDC) = CDC
        CALL CDATE(DATES)
        DD = DATES(VDAY)
        MM = DATES(VMON)
        IF(DATES(VYEAR) .GT. 40) THEN  !Consider from 1900 dates after year 40
          YY = DATES(VYEAR) + 1900
        ELSE
          YY = DATES(VYEAR) + 2000
        ENDIF
        LDAT = YY*10000 + MM*100 + DD

        DD = DAT1/10000
        TT = MOD(DAT1,10000)
        MM = TT/100
        YY = MOD(TT,100)
        IF(YY .GT. 40) THEN  !Consider from 1900 dates after year 40
          YY = YY + 1900
        ELSE
          YY = YY + 2000
        ENDIF
        LDAT1 = YY*10000 + MM*100 + DD

        DD = DAT2/10000
        TT = MOD(DAT2,10000)
        MM = TT/100
        YY = MOD(TT,100)
        IF(YY .GT. 40) THEN  !Consider from 1900 dates after year 40
          YY = YY + 1900
        ELSE
          YY = YY + 2000
        ENDIF
        LDAT2 = YY*10000 + MM*100 + DD

        IF (DAT1.NE.0 .AND. DAT2.EQ.0) THEN
C          I have just a begin date, so test just for begin date
           IF (LDAT.GE.LDAT1) THEN
              IN_CDC_INTERVAL = .TRUE.
           ENDIF
        ELSE
           IF (LDAT.GE.LDAT1 .AND. LDAT.LE.LDAT2) THEN
              IN_CDC_INTERVAL = .TRUE.
           ENDIF
        ENDIF

	RETURN
	END

C	********************************************************
	LOGICAL FUNCTION IN_WEEK_INTERVAL (CDC, DAT1, DAT2)
C	********************************************************
	IMPLICIT NONE

	INTEGER*4 CDC              !(INPUT)
	INTEGER*4 DAT1, DAT2      !(INPUT)

	INTEGER*4 YEARWEEK, 
     *            YEAR,  WEEK
	
	IN_WEEK_INTERVAL = .FALSE.

	IF (DAT1.EQ.0 .AND. DAT2.EQ.0) THEN
           RETURN
        ENDIF

	CALL FIGWEK(CDC, WEEK, YEAR)

	YEARWEEK = YEAR*100 + WEEK

        IF (DAT1.NE.0 .AND. DAT2.EQ.0) THEN
C          I have just a begin date, so test just for begin date
           IF (YEARWEEK.GE.DAT1) THEN
              IN_WEEK_INTERVAL = .TRUE.
           ENDIF
        ELSE
           IF (YEARWEEK.GE.DAT1 .AND. YEARWEEK.LE.DAT2) THEN
              IN_WEEK_INTERVAL = .TRUE.
           ENDIF
        ENDIF

	RETURN
	END
