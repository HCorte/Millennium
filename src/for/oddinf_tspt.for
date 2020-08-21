C ODDINF_TSPT.FOR
C 
C V06 22-MAR-2000 OXK SHR_ID added; DSPDAT fixed
C V05 01-MAR-2000 OXK More winning division parameters
C V04 18-FEB-2000 UXN Winning division parameters added.
C V03 15-FEB-2000 OXK Report layout changes (Vakio changes)
C V02 28-JAN-2000 OXK Vakio changes
C V01 14-MAY-1998 UXN Initial release.
C
C This subroutine reads ODDINF file and updates VAKIO game file and DAF.
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
	SUBROUTINE ODDINF_TSPT(LUN,UPDATE,STATUS)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
	INCLUDE 'INCLIB:ODDINF.DEF'
C
	INTEGER*4	LUN,STATUS
	LOGICAL*4	UPDATE
C
	COMMON		SCFREC
	INTEGER*4	ST,DRAW,FDB(7),DFDB(7)
	INTEGER*4	GIND,GNUM,FLAG,HRS,MINS
	INTEGER*4	REV1,REV2,REV3,REV4,PREV3
	INTEGER*2	I2DATE(LDATE_LEN),I2DATE2(LDATE_LEN)
	INTEGER*4	I,J,K,ROW
	INTEGER*4	I4TV,COUNT,TEAM
	CHARACTER*4	TV
	EQUIVALENCE	(TV,I4TV)
	CHARACTER*80	TNAME
	INTEGER*4	I4TNAME(20)
	EQUIVALENCE	(TNAME,I4TNAME)
	INTEGER*4	SPT_NAME_LEN
	PARAMETER	(SPT_NAME_LEN=14)
	LOGICAL		SKIP,FIRST	    
	INTEGER*4	REPLUN,PAGE
	CHARACTER*40	TITL_NAME
	CHARACTER*20	FILNAM
	CHARACTER*9	HDR_DATE
	CHARACTER*7	HDR_TIME
	INTEGER*4	HDR_WEEK,HDR_YEAR	
	INTEGER*4	EVHDR_ROWPRC, SHR_ID
C
	SKIP = .FALSE.
	FIRST = .TRUE.
C
C Read file header
C
	READ(UNIT=LUN,IOSTAT=ST,FMT='(190A)') INF.INLINE
	IF(ST.NE.0) THEN    
	    WRITE(6,913) IAM(),GTNAMES(TSPT),ST    
	    GOTO 9999
	ENDIF
	IF(INF.HDR_TYPE.NE.'0') THEN
	    TYPE*,IAM(),'Invalid record type in the file header ...'
	    GOTO 9999
	ENDIF
	IF(INF.HDR_GAME(2:4).NE.'102') THEN
	    TYPE*,IAM(),'Invalid game code in the file header ...'
	    GOTO 9999
	ENDIF
	HDR_DATE = INF.HDR_DATE
	HDR_TIME = INF.HDR_TIME
	READ(INF.HDR_WEEK,I4FMT) HDR_WEEK
	READ(INF.HDR_YEAR(2:),I4FMT) HDR_YEAR
C
C Read header record.
C	
	READ(UNIT=LUN,IOSTAT=ST,FMT='(190A)') INF.INLINE
	IF(ST.NE.0) THEN    
	    WRITE(6,913) IAM(),GTNAMES(TSPT),ST    
	    GOTO 9999
	ENDIF
C
C Get event header record..
C
	IF(INF.EVHDR_TYPE.NE.'1') THEN
	    TYPE*,IAM(),'Invalid record type in the event header  ...'
	    GOTO 9999
	ENDIF
C
	IF(INF.EVHDR_GAME(2:4).NE.'102') THEN
	    TYPE*,IAM(),'Invalid game code in the event header  ...'
	    GOTO 9999
	ENDIF
C
50 	CONTINUE
C
C Read match record. 
C
	INF.INLINE=' '
	READ(UNIT=LUN,IOSTAT=ST,FMT='(190A)') INF.INLINE
	IF(ST.NE.0) THEN    
	    WRITE(6,913) IAM(),GTNAMES(TSPT),ST    
	    GOTO 9999
	ENDIF
C	
	IF(INF.SPT_TYPE.EQ.'9') GOTO 1000   ! end record.
	IF(SKIP.AND.INF.SPT_TYPE.EQ.'4') GOTO 50  ! skip this record.
	SKIP = .FALSE.
	IF(INF.SPT_TYPE.NE.'3') THEN
	    TYPE*,IAM(),'Invalid record type ...'
	    GOTO 9999
	ENDIF
C
	IF(INF.SPT_GAME(2:4).NE.'102') THEN
	    TYPE*,IAM(),'Invalid game code ...'
	    GOTO 9999
	ENDIF
C
C Get game index
C
	READ(INF.SPT_GAME_IND,I4FMT) GIND
	IF(GIND.LE.0.OR.GIND.GT.NUMSPT) THEN
	    WRITE(6,912) IAM(),GTNAMES(TSPT),GIND
	    GOTO 9999
	ENDIF	    
C
C Open game and DAF file.
C
	GNUM = SCFGTN(TSPT,GIND)    
	IF(GNUM.LE.0.OR.GNUM.GT.MAXGAM) THEN
	    WRITE(6,900) IAM(),GTNAMES(TSPT),GIND
 	    GOTO 9999
	ENDIF
	CALL OPENW(1,SCFSFN(1,DAF),4,0,0,ST)
	CALL IOINIT(DFDB,1,DAFSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFSFN(1,DAF),1,ST,0)
	CALL OPENW(2,SCFGFN(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,2,DSPSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)	
C
C Read draw number
C
	READ(INF.SPT_DRAW_NO,I4FMT) DRAW
	IF(DRAW.LE.0) THEN
	    WRITE(6,909) IAM(),GTNAMES(TSPT),GIND,DRAW
	    GOTO 9999
	ENDIF
C
C
C Read the file for the previous draw
C
	PREV3 = 0
        IF((DRAW.GT.1).AND.UPDATE) THEN
	    CALL READW(FDB,DRAW-1,DSPREC,ST)
	    IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW-1)
	    CALL ILBYTE(PREV3,DSPREV,2)		! Get previous text checksum
	    IF(DSPSTS.EQ.0) THEN
		WRITE(6,901) IAM(),GTNAMES(TSPT),GIND,DRAW-1
		GOTO 9999
	    ENDIF	    
	ENDIF
C
C Read file for the current draw
C
	CALL READW(FDB,DRAW,DSPREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
C Checking if draw is closed.
C
        IF(DSPSTS.GT.GAMOPN) THEN
           WRITE(6,903),IAM(),GTNAMES(TSPT),GIND,DRAW
           GOTO 9999
	ENDIF
C
C Checking if draw is not already set
C
        IF(DSPNMS(1,1,1).NE.0.AND.DSPNMS(1,1,1).NE.'    ') THEN
           WRITE(6,902),IAM(),GTNAMES(TSPT),GIND,DRAW
	   IF(UPDATE) THEN
             CALL INPYESNO('Do you want to overwrite [Y/N] ?',FLAG)
             IF(FLAG.NE.1) THEN
	       SKIP = .TRUE.
	       GOTO 1000
	     ENDIF
	   ENDIF
        ENDIF
C
C Initialize some variables...
C
	CALL LIB$MOVC5(0,0,ICHAR(' '),SIZEOF(DSPNMS),DSPNMS)
C
C Read game start date
C
	I2DATE(VCDC) = 0
	READ(INF.SPT_SDATE(4:5),I2FMT) I2DATE(VYEAR)
	READ(INF.SPT_SDATE(6:7),I2FMT) I2DATE(VMON)
	READ(INF.SPT_SDATE(8:9),I2FMT) I2DATE(VDAY)
	CALL BDATE(I2DATE)
	DSPBSD = I2DATE(VCDC)
C
C Get draw end date
C
	I2DATE(VCDC) = 0
	READ(INF.SPT_EDATE(4:5),I2FMT) I2DATE(VYEAR)
	READ(INF.SPT_EDATE(6:7),I2FMT) I2DATE(VMON)
	READ(INF.SPT_EDATE(8:9),I2FMT) I2DATE(VDAY)
	CALL BDATE(I2DATE)
	DSPESD = I2DATE(VCDC)
C
C Get draw date
C
	I2DATE(VCDC) = 0
	READ(INF.SPT_DRAW_DATE(4:5),I2FMT) I2DATE(VYEAR)
	READ(INF.SPT_DRAW_DATE(6:7),I2FMT) I2DATE(VMON)
	READ(INF.SPT_DRAW_DATE(8:9),I2FMT) I2DATE(VDAY)
	CALL BDATE(I2DATE)
	DSPDAT(CURDRW) = I2DATE(VCDC)

C
C Draw closing time.
C
	READ(INF.SPT_CLOS_TIME(2:3),I2FMT) HRS
	READ(INF.SPT_CLOS_TIME(4:5),I2FMT) MINS
	DSPTIM = HRS*3600+MINS*60
C
C Number of matches
C 
	READ(INF.SPT_MATCHES_CNT,I4FMT) COUNT
	IF(COUNT.GT.SPGNBR) THEN
	    WRITE(6,914) IAM(),GTNAMES(TSPT),GIND,COUNT
	    GOTO 9999
	ENDIF
	DSPMAX = COUNT
C
C Row price
C
	READ(INF.SPT_ROW_PRICE,I6FMT) EVHDR_ROWPRC
	DSPPRC = EVHDR_ROWPRC/DYN_BETUNIT
C
C Get share parameter index, checking will be done in SPTDIV
C
	READ(INF.SPT_SHR_ID,I4FMT) SHR_ID
C
C Event name
C
        CALL STR$UPCASE(TNAME,INF.SPT_EVENT_NAME)
	IF(TNAME.EQ.'                              ') THEN
	    WRITE(TNAME,990)HDR_WEEK,HDR_YEAR,GIND
	    TYPE*,IAM(),'No event name in VAKIO.FIL, setting ',TNAME
	ENDIF
        CALL STR$TRANSLATE(TNAME,TNAME,TRANSLATE_TABLE,MATCH_TABLE)
        CALL MOVBYT(I4TNAME,1,DSPEVN(1),1,SPEVN_LEN)
C
C Get home and away team names (2*COUNT)
C
	DO I=1,2*COUNT
	  READ(UNIT=LUN,IOSTAT=ST,FMT='(190A)') INF.INLINE
	  IF(ST.NE.0) THEN    
	    WRITE(6,913) IAM(),GTNAMES(TSPT),ST    
	    GOTO 9999
	  ENDIF
C	
	  IF(INF.SPT_TYPE.NE.'4') THEN
	    TYPE*,IAM(),'Invalid record type ...'   
	    GOTO 9999
	  ENDIF
C
	  IF(INF.SPT_GAME(2:4).NE.'102') THEN
	    TYPE*,IAM(),'Invalid game code ...'   
	    GOTO 9999
	  ENDIF
C
C Match number.
C
	  READ(INF.SPT_ROW,I4FMT) ROW
	  IF(ROW.LE.0.OR.ROW.GT.COUNT) THEN
	    WRITE(6,915) IAM(),GTNAMES(TSPT),GIND,ROW
	    GOTO 9999
	  ENDIF
C
C Home or away team ??
C
	  READ(INF.SPT_TEAM,I4FMT) TEAM
	  IF(TEAM.NE.1.AND.TEAM.NE.2) THEN
	    WRITE(6,916) IAM(),GTNAMES(TSPT),GIND,TEAM
	    GOTO 9999
	  ENDIF
C
C Team name
C
          CALL STR$UPCASE(TNAME,INF.SPT_NAME)
          CALL STR$TRANSLATE(TNAME,TNAME,TRANSLATE_TABLE,MATCH_TABLE)
          CALL MOVBYT(I4TNAME,1,DSPNMS(1,TEAM,ROW),1,SPNMS_LEN)
	ENDDO
C
	DSPSTS = GAMOPN
	DSPDRW = DRAW
C
C Calculate new checksum.
C
	BUFIDX = 1
	DO I=1,SPGNBR
	  DO J=1,2
	    CALL MOVBYT(DSPNMS(1,J,I),1,BYTTAB,BUFIDX,SPNMS_LEN)
            BUFIDX=BUFIDX+SPNMS_LEN
	  ENDDO
	ENDDO
        BUFIDX = BUFIDX - 1

        CALL CHECKSUM(BYTTAB,1,BUFIDX,REV4)
        CALL ILBYTE(REV1,DSPREV,0)
        IF(DSPDRW.EQ.M251-1) THEN
           REV1 = MOD(REV1+DSPDRW,(M251-10)) + 1
        ELSE
           REV1 = MOD(REV1+DSPDRW,M251) + 1
        ENDIF
        REV2 = MOD(DSPDRW,255)
        CALL ILBYTE(REV3,DSPREV,2)          !GET PREVIOUS TEXT REV #
        REV3 = MOD(PREV3 + REV3,255) + 1
        CALL ISBYTE(REV1,DSPREV,0)          !CONTROL REV BYTE (SEQUENCE#)
        CALL ISBYTE(REV2,DSPREV,1)          !DRAW REV BYTE
        CALL ISBYTE(REV3,DSPREV,2)          !TEXT REV # BYTE  (SEQUENCE#)
        CALL ISBYTE(REV4,DSPREV,3)          !TEXT CHECKSUM BYTE
C
C Select winning divisions
C
C	IF(GIND.GT.1) THEN
C	   TYPE*
C	   WRITE(6,991) IAM(),(SCFLGN(I,GNUM),I=1,4),DSPDRW
C	   TYPE*
C	   CALL SPTDIV(DSPREC,SHR_ID)
C	ENDIF
C
C Verify all data.
C
        IF(DSPBSD.GT.DSPESD) THEN
           TYPE*,IAM(),'Beginning sales date greater then ending sales date'
	   GOTO 9999
        ENDIF
        IF(DSPBSD.EQ.0) THEN
           TYPE*,IAM(),'Beginning sales date not set '
	   GOTO 9999
        ENDIF
        IF(DSPESD.EQ.0) THEN
           TYPE*,IAM(),'Ending sales date not set'
           GOTO 9999
        ENDIF
        IF(DSPSPR.EQ.0) THEN
           TYPE*,IAM(),'Pool percentage not set '
           GOTO 9999
        ENDIF
	IF(DSPMAX.LT.10) THEN
	   TYPE*,IAM(),'Incorrect # of matches set'
	   GOTO 9999
	ENDIF
	IF(DSPPRC.LE.0) THEN
	   TYPE*,IAM(),'Incorrect row price set'
	   GOTO 9999
	ENDIF
C
	DO I=DSPBSD,DSPESD
          CALL READW(DFDB,I,DAFREC,ST)
          IF(ST.NE.0) CALL FILERR(SCFSFN(1,DAF),2,I,ST)
          I2DATE(5)=I
          CALL LCDATE(I2DATE)
          IF(DAFSTS.NE.DNOSAL.AND.DAFSTS.NE.DSOPEN) THEN
            WRITE(6,907) IAM(),(I2DATE(K),K=7,13)
            CALL INPYESNO('Do you want to overwrite [Y/N] ?',FLAG)
            IF(FLAG.NE.1) THEN 
	      GOTO 9999
	    ENDIF
          ENDIF
          IF(DAFDRW(GNUM).NE.0.AND.DAFDRW(GNUM).NE.DRAW) THEN
           WRITE(6,906) IAM(),(I2DATE(K),K=7,13),GTNAMES(TSPT),GIND,DAFDRW(GNUM)
	   IF(UPDATE) THEN
             CALL INPYESNO('Do you want to overwrite [Y/N] ?',FLAG)
             IF(FLAG.NE.1) THEN 
	       SKIP = .TRUE.
	       GOTO 1000
	     ENDIF
	   ENDIF
          ENDIF
	ENDDO

C
C
	IF(UPDATE) THEN
          DO I=DSPBSD,DSPESD
           CALL READW(DFDB,I,DAFREC,ST)
           IF(ST.NE.0) CALL FILERR(SCFSFN(1,DAF),2,ST,I)
           DAFDRW(GNUM)=DRAW
           CALL WRITEW(DFDB,I,DAFREC,ST)
           IF(ST.NE.0) CALL FILERR(SCFSFN(1,DAF),3,ST,I)
	   WRITE(6,910) IAM(),(SCFSFN(K,DAF),K=1,5),I
	  ENDDO
C
	  CALL WRITEW(FDB,DRAW,DSPREC,ST)
	  IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
	  WRITE(6,911) IAM(),(SCFGFN(K,GNUM),K=1,5),DRAW

	  WRITE(6,908) IAM(),GTNAMES(TSPT),GIND,DRAW
	ENDIF
C
C Generating report...
C
	IF(FIRST) THEN
          FIRST = .FALSE.
	  WRITE(FILNAM,917) 
	  REPLUN = 7
	  CALL ROPEN(FILNAM,REPLUN,ST)
          IF(ST.NE.0) THEN
            TYPE*,IAM(),'Error opening ',FILNAM,' status=',ST
	    GOTO 9999
          ENDIF
	  PAGE = 0
	  WRITE(TITL_NAME,918) HDR_WEEK,HDR_YEAR 
	  CALL TITLE(TITL_NAME,FILNAM,1,REPLUN,PAGE,DAYCDC)
	  WRITE(REPLUN,923)
	  WRITE(REPLUN,919) HDR_DATE(8:9),HDR_DATE(6:7),HDR_DATE(2:5),
     *        HDR_TIME(2:3),HDR_TIME(4:5),HDR_TIME(6:7)
	ENDIF
	I2DATE(VCDC) = DSPBSD
	CALL LCDATE(I2DATE)
	I2DATE2(VCDC) = DSPESD
	CALL LCDATE(I2DATE2)
	HRS = DSPTIM/3600
	MINS= (DSPTIM-HRS*3600)/60
	WRITE(REPLUN,929) (DSPEVN(K),K=1,4),GIND,DSPDRW
	WRITE(REPLUN,927) (I2DATE(K),K=9,13)
	WRITE(REPLUN,928) (I2DATE2(K),K=9,13),HRS,MINS
	WRITE(REPLUN,926) CMONY(DSPPRC,5,BETUNIT)
	WRITE(REPLUN,930) DSPDIV, DISPER(DSPSPR),
     *                    (DSPMAT(K),K,DISPER(DSPPER(K)),K=1,DSPDIV) 
	DO I=1,DSPMAX
	  WRITE(REPLUN,925) I,(DSPNMS(K,1,I),K=1,4),(DSPNMS(K,2,I),K=1,4)
	ENDDO
	WRITE(REPLUN,920)
	WRITE(REPLUN,922)
C
C Close game files and DAF
C
1000	CONTINUE
	CALL CLOSEFIL(FDB)			  
	CALL CLOSEFIL(DFDB)
C
C Get next record.
C
	IF(INF.SPT_TYPE.NE.'9')	GOTO 50	! Get next record.
	STATUS = 0
	CLOSE(REPLUN)
	RETURN
C
C ODDINF file processed with errors....
C
9999	CONTINUE
	CALL CLOSEFIL(FDB)			  
	CALL CLOSEFIL(DFDB)
	STATUS = -1
	CLOSE(REPLUN)
	TYPE*,IAM(),'********** ERRORS FOUND ****************'
	TYPE*,INF.INLINE
	TYPE*,IAM(),'********** ERRORS FOUND ****************'
	RETURN
C
C Format statements.
C
900     FORMAT(1X,A,A8,1X,I1,' draw ',I4,' is not correct')
901     FORMAT(1X,A,A8,1X,I1,' draw ',I4,' draw not defined')
902     FORMAT(1X,A,A8,1X,I1,' draw ',I4,' data has been already entered')
903     FORMAT(1X,A,A8,1X,I1,' draw ',I4,' has already been closed')
904	FORMAT(4X,':S',I1,'P',I4.4,'.FIL   ')
905     FORMAT(1X,A,'Pool file name is ',A20)
906     FORMAT(1X,A,7A2,' is already active for ',A8,1X,I1,' event # ',I4)
907     FORMAT(1X,A,7A2,' - Day has already been closed')
908     FORMAT(1X,A,A8,1X,I1,' event ',I4,' verify complete')
909	FORMAT(1X,A,A8,1X,I1,' invalid draw number >',I4)
910	FORMAT(1X,A,1X,5A4,' updated for CDC  ',I4.4)
911	FORMAT(1X,A,1X,5A4,' updated for draw ',I4.4)
912	FORMAT(1X,A,A8,' invalid game index >',I4)	
913	FORMAT(1X,A,'Error reading ',A8,' ODDINF file, status = ',I4)
914	FORMAT(1X,A,A8,1X,I1,' invalid number of matches >',I4)
915	FORMAT(1X,A,A8,1X,I1,' invalid match number >',I4)
916	FORMAT(1X,A,A8,1X,I1,' invalid team number  >',I4,'. Should be 1 or 2')
917	FORMAT('VAODDINF.REP')
918	FORMAT('VAKIO KOHTEET VIIKOLLA ',I2.2,'/',I4.4)
919	FORMAT(1X,'Tiedosto ',A2,'.',A2,'.',A4,2X,A2,':',A2,':',A2,/)
920	FORMAT(132X)
922	FORMAT(1X,98('-'))
923	FORMAT(132('='))
925	FORMAT(2X,I2,2X,4A4,' - ',4A4)
926	FORMAT(1X,'Rivin hinta',1X,A5,/)
927	FORMAT(1X,'Alkupäivä ',2X,5A2)
928	FORMAT(1X,'Loppupäivä',2X,5A2,2X,'Time',1X,I2.2,':',I2.2,/)
929	FORMAT(1X,4A4,1X,'Index',2X,I1,2X,'Draw',2X,I4,/)
930	FORMAT(1X,'Winning division parameters:',/,
     *         4X,'Number of divisions : ', I2,/,
     *         4X,'Pool % of sales     : ', F6.2,/,
     *         <DSPDIV>(4X,'Match ',I2,' wins division ',I2,1X,
     *                  'Pool % ',F6.2,/),/) 
940	FORMAT(1X,'Rolling parameters:',/,
     *         <DSPDIV>(4X,'Division ',I2,1X,
     *                  A30,1X,
     *                  'Possible rollover goes to div',I2,/),/) 
990	FORMAT('VAKIO ',I2,'/',I4,' - ',I1)
991	FORMAT(1X,A,'Selecting winning divisions for ',4A4,' draw ', I4)
	END

