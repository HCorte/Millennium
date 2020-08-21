C GVTDWN.FOR
C
C V07 21-MAY-1999 UXN TSTR CHANGED TO TEMP
C V06 16-MAR-1997 WPW Fix for clearing the download times from ASF.
C V05 13-MAR-1997 WPW End time for download added, additional checking for 
C                     agttoi flag enabled.
C V04 05-FEB-1997 WPW Initial revision.
C V03 21-DEC-1993 SYSTEM Initial revision.
C V02 07-OCT-1993 TZK CHANGED TERMINAL TYPE DETERMINATION(GVT/OI).
C V01 12-JUN-1993 MCM RELEASED FOR GEORGIA
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
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM GVTDWN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
C
        INTEGER*4    ANS, EXT, COUNT, PAGE, LINCNT/70/
        INTEGER*4    CERR, ST, TER, REPLU/7/
        INTEGER*4    PHNLIN, DOWNTIM, BEGTIM, CLASS, TEMPTIM
        INTEGER*4    AGTNBR, PHNOFF/0/, LOADTIM, BITMAP, TEMP(2)
        INTEGER*4    FSTTER, STIM, LOADINHR, HOUR, MIN
        CHARACTER    REPNAM*12
        CHARACTER*12 ASFGVTID
        CHARACTER*12 REPTITLE(3)/'PRELIMINARY ','SET TIMES   ',
     *                           'CLEAR TIMES '/
        CHARACTER    HEAD*36
        EQUIVALENCE (ASFBYT(SGSER),ASFGVTID)
C
        CALL COPYRITE
C
        TYPE*,' '
        TYPE*,'          GVTDWN << GVT Download Program >> V01'
        TYPE*,' '
C
C OPEN THE ASF.FIL
C
        CALL OPENASF(ASF)
C
        COUNT   = 0
        LOADTIM = 0
        FSTTER  = 1
C
        WRITE(5,9000) IAM(),IAM(),IAM()
        CALL PRMNUM('Enter GVT download option ',ANS,1,3,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
        IF(ANS.EQ.1.OR.ANS.EQ.2) THEN

          CALL PRMNUM('Enter the 1st terminal to process:                 ',
     *                 FSTTER,1,6144,EXT)
          IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)

          CALL PRMNUM('Enter number of available ATT phone lines:         ',
     *                 PHNLIN,0,100,EXT)
          IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)

          CALL PRMNUM('Enter the amount of time to download a GVT in min: ',
     *                 DOWNTIM,0,600,EXT)
          IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)

          CALL PRMNUM('Enter in min after midnight the start of download: ',
     *                 BEGTIM,0,1440,EXT)
          IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)

          CALL PRMNUM('Enter the time HHMM to stop download:              ',
     *                 STIM,0,1440,EXT)
          IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
        ENDIF
C
C OPEN REPORT FILE
C
        WRITE (REPNAM,8000) ANS
        CALL ROPEN(REPNAM,REPLU,ST)
        IF(ST.NE.0)THEN
          TYPE *,' Error openning ',REPNAM,' > ',ST
          CALL GPAUSE
        ENDIF
C
C WRITE TITLE
C
        WRITE(HEAD,8001) REPTITLE(ANS)
        IF(LINCNT.GT.50) THEN
          CALL TITLE(HEAD,REPNAM(1:8),1,7,PAGE,DAYCDC)
          WRITE(REPLU,9050) PHNLIN,DOWNTIM,BEGTIM,STIM
          LINCNT=LINCNT+7
          WRITE(REPLU,9100)
          LINCNT=15
        ENDIF
C
C GET PRELIMINARY DOWNLOAD TIMES
C
        LOADTIM=BEGTIM-DOWNTIM
        DO 200 TER=FSTTER,NUMAGT
           CALL READASF(TER,ASFREC,ST)
           IF(ST.NE.0) CALL FILERR(GFNAMES(1,ASF),2,ST,TER)
           IF(MOD(TER,1000).EQ.0) THEN
             TYPE*,IAM(),TER,' retailers processed'
             TYPE*,IAM(),COUNT,' GVTs'
           ENDIF
C           
C CHECK TERMINAL TYPE FOR GVT
C
	   BITMAP=0
	   CALL ASCBIN(ASFINF,STTYP,LTTYP,BITMAP,ST)
	   IF(BTEST(BITMAP,AGTTOI)) GOTO 200

           CALL ATOH(ASFGVTID,1,LGSER,TEMP,ST)
           IF(TEMP(1).EQ.0.OR.TEMP(2).EQ.0) GOTO 200

           CALL ASCBIN(ASFINF,SSCLS,LSCLS,CLASS,ST)
           IF(CLASS.NE.9) GOTO 200
C
C GET RETAILER NUMBER
C
           AGTNBR=0
           CALL ASCBIN(ASFINF,SAGNO,LAGNO,AGTNBR,CERR)
           IF(AGTNBR.EQ.0) GOTO 200
C
C CALCULATE DOWNLOAD TIME FOR GVT
C
           IF(ANS.NE.3) THEN
             IF(MOD(COUNT,PHNLIN).EQ.0) LOADTIM=LOADTIM+DOWNTIM
           ENDIF
C
C MAKE SURE WE DO NOT EXCEED THE STOPPING TIME FOR DOWNLOAD
C
           IF(ANS.NE.3) THEN
	     TEMPTIM = LOADTIM + DOWNTIM
	     HOUR = TEMPTIM / 60
	     MIN  = MOD(TEMPTIM,60)
	     LOADINHR = (HOUR*100) + MIN
             IF(LOADINHR.GT.STIM) GOTO 4000
           ENDIF
C
C UPDATE NUMBER OF GVTS TO DOWNLOAD
C
           COUNT=COUNT+1
C
C CHECK IF PAGE IS FULL IF SO GOTO NEW PAGE AND PRINT HEADER
C
           IF(LINCNT.GT.50) THEN
             CALL TITLE(HEAD,REPNAM(1:8),1,7,PAGE,DAYCDC)
             WRITE(REPLU,9100)
             LINCNT=7
           ENDIF
C
C WRITE RETAILER TO REPORT
C
           WRITE(REPLU,9200) TER,AGTNBR,PHNOFF,DISTIM(LOADTIM*60)
           LINCNT=LINCNT+1
C
C WRITE/CLEAR DOWNLOAD TIMES TO THE ASF
C
           IF(ANS.NE.1) THEN
             IF(ANS.EQ.2) THEN                  !WRITE DOWNLOAD TIME IN SEC
               ASFGVTIM=LOADTIM*60
             ELSEIF(ANS.EQ.3) THEN              !CLEAR DOWNLOAD TIME
               ASFGVTIM=0
             ENDIF
             CALL WRITASF(TER,ASFREC,ST)
           ENDIF
200     CONTINUE
        WRITE(REPLU,9300) COUNT
C
4000    CONTINUE
C
        CALL CLOSASF
        IF (COUNT.GT.0) THEN
	  TYPE*,IAM(),TER-1,' Last retailer processed'
	  TYPE*,IAM(),COUNT,' GVTs'
	  WRITE(REPLU,9400) TER-1
	ENDIF
        CALL GSTOP(GEXIT_SUCCESS)
C
C       FORMAT STATEMENTS
C
8000    FORMAT('GVTDWN',I2.2,'.REP')
8001    FORMAT('GVT DOWNLOAD SCHEDULE - ',A12)
9000    FORMAT(1X,A,' 1 -  Preliminary download times ',/,
     *         1X,A,' 2 -  Set download times is the ASF ',/,
     *         1X,A,' 3 -  Clear download times ',/)
9050    FORMAT(//,2X,'Number of ATT phone lines used: ',8X,I6,/,
     *            2X,'Number of minutes to download a GVT: ',3X,I6,/,
     *            2X,'Minutes after midnight to start download: ',I4,/,
     *            2X,'Time in HHMM to stop download: ',11X,I4.4,/)
9100    FORMAT(2X,'TERMINAL',4X,' AGENT  ',7X,' PHONE',5X,
     *         'DOWNLOAD',/,4X,'NUMBER',6X,
     *         'NUMBER',7X,'OFFSET',9X,'TIME',9X,/)
9200    FORMAT(5X,I5.5,5X,I7.7,12X,I1,5X,A8)
9300    FORMAT(/,1X,I5,' GVTs processed')
9400    FORMAT(/,1X,' End time reached ... Last terminal ',I5.5)
	END
