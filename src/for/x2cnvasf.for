C CNVASF.FOR
C
C V01 01-MAR-96 wsm Finland stn cls conversion.
C
C PROGRAM TO CONVERT AGENT RECORD WITH STATION CLASS=10 TO 1,
C STATION CLASS TO BE EXACTLY 2 CHARACTERS ("X" TO "0X"), 
C STATION NUMBER TO BE EXACTLY 5 CHARACTERS ("XXX" TO "00XXX"),
C RELAY GROUP TO BE EXACTLY 3 CHARACTERS ("X" TO "00X").
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, West Greenwich, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright (c) 1996 GTECH Corporation. All Rights Reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM CNVASF
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
C
C
        INTEGER*4    J, K, ST, I
        INTEGER*4    AGT, FDB(7), CLASS, STN, GROUP
        INTEGER*4    SCFNAM(5), SCFFDB(7)
        DATA         SCFNAM/'SCF.','FIL ',3*'    '/
        CHARACTER    CZERO/Z0/
	CHARACTER   AGENT_STRING*(ALENGTH)
        EQUIVALENCE (AGENT_STRING, ASFBYT)
C
C
        TYPE*,' '
        TYPE*,' '
        TYPE*,'       ASF.FIL Conversion for Finland'
        TYPE*,'   -> converts records with stn cls=10 to 1'
        TYPE*,' '
        TYPE*,' '
        TYPE*,'           <<< CNVASF.FOR >>>'
        TYPE*,' '
        TYPE*,' '
C
C READ SCF RECORD
C 
      CALL OPENW(1,SCFNAM,4,0,0,ST)
      IF(ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)

      CALL IOINIT(SCFFDB,1,SCFSEC*256)
      IF(ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)

      CALL READW(SCFFDB,1,SCFREC,ST)
      IF(ST.NE.0) CALL FILERR(SCFNAM,2,ST,1)

      CALL CLOSEFIL(SCFFDB)

      DO 5 I=1,MAXFIL
         IF(SCFSFN(1,I).EQ.'    ') CALL SYSVOL(SCFSFN(1,I))
5     CONTINUE
C
C OPEN ASF.FIL
C
        CALL OPENW(1,SCFSFN(1,ASF),0,0,0,ST)
        IF (ST .NE. 0)THEN
          CALL FILERR(SCFSFN(1,ASF),1,ST,0)
          TYPE *,'***ERROR ON OPEN OF FILE OR DEVICE -- # ',ST
          CALL GSTOP(GEXIT_OPABORT)
        ENDIF
        CALL IOINIT(FDB,1,ASFSEC*256)
        IF (ST.NE.0) THEN
          CALL USRCLOS1(1)
          CALL GSTOP(GEXIT_OPABORT)
        ENDIF
C
C READ ALL AGENTS
C
        TYPE*,' '
        TYPE*,IAM(),' Converting Agent Information'
        TYPE*,' '
C
        DO 200 AGT=1,NUMAGT
          CALL READW(FDB,AGT,ASFREC,ST)
          IF (ST.NE.0) GOTO 500
          IF (MOD(AGT,1000).EQ.0) TYPE*,IAM(),AGT,' agents processed'
C
C ONLY CHANGE RECORDS WITH VALID AGENT NUMBER
C
          DO 10 J=SAGNO,EAGNO
            IF (ASFBYT(J).NE.CZERO .AND. ASFBYT(J).NE.' ') GOTO 20
10        CONTINUE
          GOTO 200
C
20        CONTINUE
C
C ONLY CHANGE RECORDS WITH VALID STATION NUMBER
C
          DO 15 J=SXSTN,EXSTN
            IF (ASFBYT(J).NE.CZERO .AND. ASFBYT(J).NE.' ') GOTO 25
15        CONTINUE
          GOTO 200
C
25	  CONTINUE	  
C
C MODIFY STATION CLASS VALUE BASED ON ACTUAL FIELD SIZE
C
	  READ(UNIT = AGENT_STRING(SSCLS:ESCLS), FMT = '(BN, I)',
	1      IOSTAT=ST) CLASS
	  IF (ST .NE. 0 .AND. ST .NE. 64) WRITE(5,901) IAM(),AGT
C
C         CHANGE STATION CLASS VALUE IF CLASS=10
C
          IF (CLASS.NE.0 .AND. CLASS.EQ.10) CLASS=1
          CALL BINASC(ASFINF,SSCLS,LSCLS,CLASS)
C
C MODIFY STATION VALUE BASED ON ACTUAL FIELD SIZE
C
	  READ(UNIT = AGENT_STRING(SXSTN:EXSTN), FMT = '(BN, I)',
	1      IOSTAT = ST) STN
          IF (ST .NE. 0) WRITE(5,902) IAM(), AGT
	  
          CALL BINASC(ASFINF,SXSTN,LXSTN,STN)
C
C MODIFY RELAY GROUP VALUE BASED ON ACTUAL FIELD SIZE
C SKIP RECORDS WITH NO/BLANK RELAY GROUP
C
          DO 30 J=SXGRP,EXGRP
            IF (ASFBYT(J).NE.CZERO .AND. ASFBYT(J).NE.' ') GOTO 40
30        CONTINUE
          GOTO 100
C
40        CONTINUE
	  READ(UNIT = AGENT_STRING(SXGRP:EXGRP), FMT = '(BN, I)',
	1      IOSTAT=ST) GROUP 
	  IF (ST .NE. 0) WRITE(5,903) IAM(),AGT
          CALL BINASC(ASFINF,SXGRP,LXGRP,GROUP)
C
C
C WRITE BACK MODIFIED AGENT RECORD TO ASF.FIL
C
100       CONTINUE
          CALL WRITEW(FDB,AGT,ASFREC,ST)
          IF (ST.NE.0) THEN
            WRITE(5,904) IAM(),(SFNAMES(K,ASF),K=1,5)
            GOTO 200
          ENDIF
C
200     CONTINUE
C
C CLOSE THE ASF FILES
C
500     CONTINUE
        CALL USRCLOS1(1)
        CALL GSTOP(GEXIT_SUCCESS)
C
C
C FORMAT STATEMENTS
C
900     FORMAT(1X,' Error creating file ',A7 )      
901     FORMAT(1X,A18,' Terminal ',I5,' invalid stn class number')      
902     FORMAT(1X,A18,' Terminal ',I5,' invalid station number')      
903     FORMAT(1X,A18,' Terminal ',I5,' invalid group number')      
904     FORMAT(1X,A,1X,5A4,' not updated')
        END      
