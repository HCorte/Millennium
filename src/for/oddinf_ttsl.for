C
C This subroutine reads ODDINF file and updates PITKA game file,
C verification file and DAF.
C
C V02 09-FEB-1999 UXN Row status set to GAMINF if odds are 0.
C V01 12-MAY-1998 UXN Initial release.
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
	SUBROUTINE ODDINF_TTSL(LUN,UPDATE,STATUS)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DTSREC.DEF'
	INCLUDE 'INCLIB:ODDINF.DEF'
C
	INTEGER*4   LUN,STATUS
	LOGICAL*4   UPDATE
C
	COMMON	    SCFREC
	INTEGER*4   ST,DRAW,GIND
	INTEGER*4   TSL_NAME_LEN
	PARAMETER   (TSL_NAME_LEN=14)
	INTEGER*4   FDB(7),VFDB(7),DFDB(7)
	INTEGER*4   HRS,MINS,ROW,ROWS,ANSS
	INTEGER*4   REV1,REV2,REV3,REV4,PREV3
	INTEGER*4   I,J,K,GNUM,TSUM
	INTEGER*4   I4TV,REPLUN
	CHARACTER*4 TV
	EQUIVALENCE (I4TV,TV)
	CHARACTER*80 TNAME
	INTEGER*4    I4TNAME(20)
	EQUIVALENCE (I4TNAME,TNAME)
	CHARACTER*20 CDTSPFN,FILNAM
	EQUIVALENCE  (DTSPFN,CDTSPFN)
	INTEGER*2   I2DATE(LDATE_LEN),I2DATE2(LDATE_LEN)
	INTEGER*4   PAGE
	LOGICAL	    SKIP,FIRST
	CHARACTER*40 TITL_NAME
	CHARACTER*9 HDR_DATE
	CHARACTER*7 HDR_TIME
	INTEGER*4   HDR_WEEK,HDR_YEAR,DUMMY
	CHARACTER*6 ROWTYP(0:3)/'??????','single','tupla ','      '/
C
	SKIP = .FALSE.
	FIRST=.TRUE.
C
C Read file header
C
	READ(UNIT=LUN,IOSTAT=ST,FMT='(190A)') INF.INLINE
	IF(ST.NE.0) THEN    
	    WRITE(6,914) IAM(),GTNAMES(TTSL),ST    
	    GOTO 9999
	ENDIF
	IF(INF.HDR_TYPE.NE.'0') THEN
	    TYPE*,IAM(),'Invalid record type in the file header ...'   
	    GOTO 9999
	ENDIF
	IF(INF.HDR_GAME(4:4).NE.'1') THEN
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
40	CONTINUE
	READ(UNIT=LUN,IOSTAT=ST,FMT='(190A)') INF.INLINE
	IF(ST.NE.0) THEN    
	    WRITE(6,914) IAM(),GTNAMES(TTSL),ST    
	    GOTO 9999
	ENDIF
C
C Get event header record..
C
50 	CONTINUE
	IF(SKIP.AND.INF.EVHDR_TYPE.EQ.'3') GOTO 40
	SKIP = .FALSE.
	IF(INF.EVHDR_TYPE.EQ.'9') GOTO 1000 ! end record.
	IF(INF.EVHDR_TYPE.NE.'1') THEN
	    TYPE*,IAM(),'Invalid event header ...'   
	    GOTO 9999
	ENDIF
C
C Open game, verification and DAF file.
C
	GIND = 1		
	GNUM = SCFGTN(TTSL,GIND)    
	IF(GNUM.LE.0.OR.GNUM.GT.MAXGAM) THEN
	    WRITE(6,900) IAM(),GTNAMES(TTSL),GIND
	    GOTO 9999
	ENDIF
	CALL OPENW(1,SCFSFN(1,DAF),4,0,0,ST)
	CALL IOINIT(DFDB,1,DAFSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFSFN(1,DAF),1,ST,0)
	CALL OPENW(2,SCFGFN(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,2,DTSSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)	
	CALL OPENW(3,SCFGVN(1,GNUM),4,0,0,ST)
	CALL IOINIT(VFDB,3,DTSSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFGVN(1,GNUM),1,ST,0)	
C
C Get draw number
C
	READ(INF.EVHDR_DRAW,I4FMT) DRAW
	TYPE*,IAM(),'PITKA DRAW ',DRAW	
	IF(DRAW.LE.0) THEN
	    WRITE(6,911) IAM(),GTNAMES(TTSL),GIND,DRAW
	    GOTO 9999
	ENDIF
C
C Read the file for the previous draw
C
	PREV3 = 0
        IF((DRAW.GT.1).AND.UPDATE) THEN
	    CALL READW(FDB,DRAW-1,DTSREC,ST)
	    IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW-1)
	    CALL ILBYTE(PREV3,DTSREV,2)		! Get previous text checksum
	    IF(DTSSTS.EQ.0) THEN
		WRITE(6,901) IAM(),GTNAMES(TTSL),GIND,DRAW,DRAW-1
		GOTO 9999
	    ENDIF	    
	ENDIF
C
C Read file for the current draw
C
	CALL READW(FDB,DRAW,DTSREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
C Checking if draw is not already set
C
        IF(DTSSTS.EQ.GAMINF.OR.DTSSTS.EQ.GAMOPN) THEN
           WRITE(6,902) IAM(),GTNAMES(TTSL),GIND,DRAW
	   IF(UPDATE) THEN
             CALL INPYESNO('Do you want to overwrite [Y/N] ?',ANSS)
             IF(ANSS.NE.1) THEN
	       SKIP = .TRUE.
	       GOTO 1000
	     ENDIF
	   ENDIF
        ENDIF
C
C Checking if draw is closed.
C
        IF(DTSSTS.GT.GAMOPN) THEN
           WRITE(6,9011) IAM(),GTNAMES(TTSL),GIND,DRAW
           GOTO 9999
	ENDIF
C
C Checking if some rows are already closed.
C
	DO I=1,MAXSRW
	   IF(DTSSTA(I).GT.GAMOPN.AND.(DTSODS(1,I).NE.0.OR.
     *        DTSODS(2,I).NE.0.OR.DTSODS(3,I).NE.0)) THEN
             WRITE(6,906) IAM(),GTNAMES(TTSL),GIND,I
             GOTO 9999
	   ENDIF
        ENDDO
C
C Get draw start date
C
	READ(INF.EVHDR_SDATE(4:5),I2FMT) I2DATE(VYEAR)
	READ(INF.EVHDR_SDATE(6:7),I2FMT) I2DATE(VMON)
	READ(INF.EVHDR_SDATE(8:9),I2FMT) I2DATE(VDAY)
	CALL BDATE(I2DATE)
	DTSBSD = I2DATE(VCDC)
C
C Get draw end date
C
	READ(INF.EVHDR_EDATE(4:5),I2FMT) I2DATE(VYEAR)
	READ(INF.EVHDR_EDATE(6:7),I2FMT) I2DATE(VMON)
	READ(INF.EVHDR_EDATE(8:9),I2FMT) I2DATE(VDAY)
	CALL BDATE(I2DATE)
	DTSESD = I2DATE(VCDC)
	DTSDTE = I2DATE(VCDC)
C
C Set week number
C
	CALL FIGWEK(DTSESD-WEEK_OFFSET,DTSWEK,DUMMY)	
C
C Get number of matches in event
C
	READ(INF.EVHDR_MATCHES,I4FMT) ROWS
	IF(ROWS.LE.0.OR.ROWS.GT.MAXSRW) THEN
           WRITE(6,903) IAM(),GTNAMES(TTSL),GIND,ROWS,DRAW
	   GOTO 9999
	ENDIF
C
C Initialize som variables.
C
	DO I=1,MAXSRW
	    DTSSTA(I) = 0
	    DTSROWTYP(I) = 3
	ENDDO
C
	CALL LIB$MOVC5(0,0,ICHAR(' '),SIZEOF(DTSNMS),DTSNMS)
C
C Get match record for each row...
C
100	CONTINUE
	READ(UNIT=LUN,IOSTAT=ST,FMT='(190A)') INF.INLINE
	IF(ST.NE.0) THEN    
	    WRITE(6,914) IAM(),GTNAMES(TTSL),ST    
	    GOTO 9999
	ENDIF
	IF(INF.TSL_TYPE2.EQ.'9') GOTO 200	! end record.
	IF(INF.TSL_TYPE2.EQ.'1') GOTO 200	! header record for next event.
	IF(INF.TSL_TYPE2.NE.'3') THEN
	    TYPE*,IAM(),'Invalid match record >',INF.TSL_TYPE2
	    GOTO 9999
	ENDIF
C
C Match number
C
	READ(INF.TSL_MATCH_NO,I4FMT) ROW
	IF(ROW.LE.0.OR.ROW.GT.ROWS) THEN
	    WRITE(6,904) IAM(),GTNAMES(TTSL),GIND,ROW
	    GOTO 9999
	ENDIF
C
	DTSSTA(ROW) = GAMINF
C
C Row draw date.
C
	READ(INF.TSL_RESULT_DATE(4:5),I2FMT) I2DATE(VYEAR)
	READ(INF.TSL_RESULT_DATE(6:7),I2FMT) I2DATE(VMON)
	READ(INF.TSL_RESULT_DATE(8:9),I2FMT) I2DATE(VDAY)
	CALL BDATE(I2DATE)
	DTSDAT(ROW) = I2DATE(VCDC)
C
C Row closing time.
C
	READ(INF.TSL_CLOS_TIME(2:3),I2FMT) HRS
	READ(INF.TSL_CLOS_TIME(4:5),I2FMT) MINS
	DTSTIM(ROW) = HRS*3600+MINS*60
C
C TV-channel
C
	TV = INF.TSL_TV
	DTSTVC(1,ROW) = I4TV
C
C Row status (match type)
C	
	READ(INF.TSL_MATCH_TYPE,I4FMT) DTSROWTYP(ROW)
        IF(DTSROWTYP(ROW).LE.0.OR.DTSROWTYP(ROW).GT.3) THEN
          WRITE(6,905) IAM(),GTNAMES(TTSL),GIND,DTSROWTYP(ROW),ROW
          GOTO 9999
        ENDIF
C
C Get fixed odds.
C    
	READ(INF.TSL_ODDS1,I6FMT) DTSODS(1,ROW)
        READ(INF.TSL_ODDS2,I6FMT) DTSODS(2,ROW)
        READ(INF.TSL_ODDSX,I6FMT) DTSODS(3,ROW)
C
C Get home and away team name.
C
        CALL STR$UPCASE(TNAME,INF.TSL_HOME_NAME)
        CALL STR$TRANSLATE(TNAME,TNAME,TRANSLATE_TABLE,MATCH_TABLE)
        CALL MOVBYT(I4TNAME,1,DTSNMS(1,1,ROW),1,TSL_NAME_LEN)
C
        CALL STR$UPCASE(TNAME,INF.TSL_AWAY_NAME)
        CALL STR$TRANSLATE(TNAME,TNAME,TRANSLATE_TABLE,MATCH_TABLE)
        CALL MOVBYT(I4TNAME,1,DTSNMS(1,2,ROW),1,TSL_NAME_LEN)
C
	GOTO 100    ! get next record.
200	CONTINUE
C
C Verify all data
C
	IF(DTSRWS.LE.0) THEN	
          WRITE(6,907) IAM(),GTNAMES(TTSL),GIND
	  DTSRWS = MAXSRW
	ENDIF
C
C Set pool file name
C
        WRITE (CDTSPFN,912) GIND,DRAW
        DTSPFN(1) = FILEPACK
        IF(UPDATE) WRITE(6,908) IAM(),CDTSPFN
C
	DTSSTS = GAMOPN
	DTSDRW = DRAW
C
C Calculate new checksum
C
	BUFIDX = 1
	DO I=1,DTSRWS
	  DO J=1,3
	    CALL REVBYT(DTSODS(J,I),TODS_LEN,BYTTAB,BUFIDX,TODS_LEN)
            BUFIDX = BUFIDX + TODS_LEN
	  ENDDO
	  DO J=1,2
	    CALL MOVBYT(DTSNMS(1,J,I),1,BYTTAB,BUFIDX,TSL_NAME_LEN)
            BUFIDX = BUFIDX + TSL_NAME_LEN
	  ENDDO
        ENDDO
	BUFIDX = BUFIDX - 1
	CALL CHECKSUM(BYTTAB,0,BUFIDX,TSUM)
	CALL ILBYTE(REV1,DTSREV,0)
        IF(DTSDRW.EQ.M251-1) THEN
          REV1 = MOD(REV1+DTSDRW,(M251-10)) + 1
        ELSE
          REV1 = MOD(REV1+DTSDRW,M251) + 1
        ENDIF
        REV2 = MOD(DTSDRW,255)
        CALL ILBYTE(REV3,DTSREV,2)          !get previous text rev #
        REV3 = MOD(PREV3 + REV3,255) + 1
        CALL ISBYTE(REV1,DTSREV,0)          !control rev byte  (sequence #)
        CALL ISBYTE(REV2,DTSREV,1)          !draw rev byte
        CALL ISBYTE(REV3,DTSREV,2)          !text rev # byte   (sequence #)
        CALL ISBYTE(REV4,DTSREV,3)          !text checksum byte
C
        IF(DTSBSD.GT.DTSESD) THEN
           TYPE*,IAM(),'Begining sales date greater then ending sales date'
	   GOTO 9999
        ENDIF
        IF(DTSBSD.EQ.0) THEN
           TYPE*,IAM(),'Begining sales date not set '
	   GOTO 9999
        ENDIF
        IF(DTSESD.EQ.0) THEN
           TYPE*,IAM(),'Ending sales date not set'
           GOTO 9999
        ENDIF
        IF(DTSDTE.EQ.0) THEN
           TYPE*,IAM(),'Event draw date not set'
           GOTO 9999
        ENDIF
        IF(DTSDTE.LT.DTSESD) THEN
           TYPE*,IAM(),'Event draw date is less then ending sales date'
           GOTO 9999
        ENDIF
        IF(DTSPRC.LE.0) THEN
           TYPE*,IAM(),'Base price not set'
           GOTO 9999
        ENDIF
        DO 1010 I=1,DTSRWS
          IF(DTSSTA(I).EQ.0) THEN
	    DTSSTA(I) = GAMBFD		! Close empty rows.
	    GOTO 1010
	  ELSE
	    DTSSTA(I) = GAMOPN
	  ENDIF
          IF(DTSDAT(I).LT.DTSBSD) THEN
            TYPE*,IAM(),'Date for row ',I,' is less than begining sales date'
	    GOTO 9999
          ENDIF
	  IF(DTSDAT(I).GT.DTSDTE) THEN
            TYPE*,IAM(),'Date for row ',I,' is greater than event draw date'
            GOTO 9999
          ENDIF
          IF(DTSODS(1,I).EQ.0) THEN
            TYPE*,IAM(),'Home odds for row ',I,' not set'
            DTSSTA(I) = GAMINF
          ENDIF
          IF(DTSODS(2,I).EQ.0) THEN
            TYPE*,IAM(),'Away odds for row ',I,' not set'
            DTSSTA(I) = GAMINF
          ENDIF
          IF(DTSODS(3,I).EQ.0) THEN
            TYPE*,IAM(),'Tie odds for row  ',I,' not set'
            DTSSTA(I) = GAMINF
          ENDIF
1010    CONTINUE
C
	DO I = DTSBSD,DTSESD
	   CALL READW(DFDB,I,DAFREC,ST)
	   IF(ST.NE.0) CALL FILERR(SCFSFN(1,DAF),2,ST,I)
	   IF(DAFDRW(GNUM).NE.0.AND.DAFDRW(GNUM).NE.DRAW) THEN
	     I2DATE(VCDC) = I
	     CALL LCDATE(I2DATE)
	     WRITE(6,909) IAM(),(I2DATE(K),K=7,13),GTNAMES(TTSL),DAFDRW(GNUM)
	     IF(UPDATE) THEN
               CALL INPYESNO('Do you want to overwrite [Y/N] ? ',ANSS)
               IF(ANSS.NE.1) THEN
	        SKIP = .TRUE.
		GOTO 1000
	       ENDIF
	     ENDIF
	   ENDIF	      
	ENDDO
C
	IF(UPDATE) THEN
	  DO I=DTSBSD,DTSESD
	   CALL READW(DFDB,I,DAFREC,ST)
	   IF(ST.NE.0) CALL FILERR(SCFSFN(1,DAF),2,ST,I)
	   DAFDRW(GNUM)=DRAW
	   CALL WRITEW(DFDB,I,DAFREC,ST)
	   IF(ST.NE.0) CALL FILERR(SCFSFN(1,DAF),3,ST,I)
	  ENDDO
	  WRITE(6,913) IAM(),(SCFSFN(K,DAF),K=1,5)

	  CALL WRITEW(FDB,DRAW,DTSREC,ST)
	  IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
 	  WRITE(6,913) IAM(),(SCFGFN(K,GNUM),K=1,5)
	  CALL WRITEW(VFDB,DRAW,DTSREC,ST)
	  IF(ST.NE.0) CALL FILERR(SCFGVN(1,GNUM),3,ST,DRAW)
	  WRITE(6,913) IAM(),(SCFGVN(K,GNUM),K=1,5)
C
	  WRITE(6,910) IAM(),GTNAMES(TTSL),GIND,DRAW
	ENDIF
C
C Generating report.
C
	IF(FIRST) THEN
	  FIRST = .FALSE.
	  WRITE(FILNAM,916) 	
	  REPLUN = 7
	  CALL ROPEN(FILNAM,REPLUN,ST)
          IF(ST.NE.0) THEN
            TYPE*,IAM(),'Error opening ',FILNAM,' status=',ST
	    GOTO 9999
          ENDIF
	  PAGE = 0
	  WRITE(TITL_NAME,917) HDR_WEEK,HDR_YEAR 
	  CALL TITLE(TITL_NAME,FILNAM,1,REPLUN,PAGE,DAYCDC)
	  WRITE(REPLUN,923)
	  WRITE(REPLUN,918) HDR_DATE(8:9),HDR_DATE(6:7),HDR_DATE(2:5),
     *      HDR_TIME(2:3),HDR_TIME(4:5),HDR_TIME(6:7)
	ENDIF
	WRITE(REPLUN,925) DTSDRW
	WRITE(REPLUN,919)
	I2DATE(VCDC) = DTSBSD
	CALL LCDATE(I2DATE)
	WRITE(REPLUN,920) (I2DATE(K),K=9,13)	
	WRITE(REPLUN,919)
	WRITE(REPLUN,921)
	WRITE(REPLUN,922)
	DO I=1,ROWS
	   I2DATE2(VCDC) = DTSDAT(I)
	   CALL LCDATE(I2DATE2)
	   HRS = DTSTIM(I)/3600
	   MINS = (DTSTIM(I)-HRS*3600)/60
	   WRITE(REPLUN,924) I,(DTSNMS(K,1,I),K=1,4),(DTSNMS(K,2,I),K=1,4),
     *        ROWTYP(DTSROWTYP(I)),
     *        (I2DATE(K),K=9,13),(I2DATE2(K),K=9,13),HRS,MINS,DTSTVC(1,I),
     *        DTSODS(1,I)/100,MOD(DTSODS(1,I),100),
     *        DTSODS(3,I)/100,MOD(DTSODS(3,I),100),
     *        DTSODS(2,I)/100,MOD(DTSODS(2,I),100)
        ENDDO
C
C Close game files and DAF
C
1000	CONTINUE
	CALL CLOSEFIL(FDB)			  
	CALL CLOSEFIL(VFDB)			  
	CALL CLOSEFIL(DFDB)
C
	IF(SKIP) GOTO 40
	IF(INF.TSL_TYPE2.NE.'9') GOTO 50 ! Get next draw definition.
	STATUS = 0
        CLOSE(REPLUN)
	RETURN
C
C ODDINF file processed with errors....
C
9999	CONTINUE
	CALL CLOSEFIL(FDB)			  
	CALL CLOSEFIL(VFDB)			  
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
9011    FORMAT(1X,A,A8,1X,I1,' draw ',I4,' has already been completed')
902     FORMAT(1X,A,A8,1X,I1,' draw ',I4,' data has been already entered')
903     FORMAT(1X,A,A8,1X,I1,' invalid number of matches ',I4, ' for draw ',I4)
904	FORMAT(1X,A,A8,1X,I1,' invalid match number >',I4)
905	FORMAT(1X,A,A8,1X,I1,' invalid row type >',I4,' for row >',I4)
906     FORMAT(1X,A,A8,1X,I1,' row ',I4,' has already been closed')
907     FORMAT(1X,A,A8,1X,I1,' number of rows was not set ')
908     FORMAT(1X,A,'Pool file name is ',A20)
909     FORMAT(1X,A,7A2,' is already active for ',A8,' event # ',I4)
910     FORMAT(1X,A,A8,1X,I1,' event ',I4,' verify complete')
911	FORMAT(1X,A,A8,1X,I1,' invalid draw number >',I4)
912	FORMAT(4X,':TS',I1,'P',I4.4,'.FIL  ')
913	FORMAT(1X,A,1X,5A4,' updated.')
914	FORMAT(1X,A,'Error reading ',A8,' ODDINF file, status = ',I4)
915	FORMAT(1X,A,A8,1X,I1,' report file for draw ',I4,' is ',A20)
916	FORMAT('PIODDINF.REP')
917	FORMAT('PITKÄVETOKOHTEET VIIKOLLA ',I2.2,'/',I4.4)
918	FORMAT(1X,'File ',A2,'.',A2,'.',A4,2X,A2,':',A2,':',A2)
919	FORMAT(132X)
920	FORMAT(1X,'Alkupäivä',2X,5A2)
921	FORMAT(1X,'N:o Target',T53,'Start Date    End Date      Time     TV',
     *            '       1      X      2 ')
922	FORMAT(1X,114('-'))
923	FORMAT(132('='))
924	FORMAT(2X,I2.2,1X,4A4,' - ',4A4,2X,A6,2X,T52,5A2,4X,5A2,4X,
     *         I2.2,':',I2.2,4X,A4,4X,I2,'.',I2.2,2X,I2,'.',I2.2,
     *         2X,I2,'.',I2.2,2X)
925	FORMAT(/,1X,'Draw',3X,I4)
	END

