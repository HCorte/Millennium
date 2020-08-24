C
C AGTPSW.for
C
C $Log:   GXAFIP:[GOLS]AGTPSW.FOV  $
C  
C     Rev 1.1   07 Mar 1997 13:37:22   RXK
C  Pick up passwords of GVT agents only
C  
C     Rev 1.0   06 Mar 1997 13:52:22   RXK
C  Initial revision.
C
C
C PROGRAM TO CREATE FILE OF AGENTS PASSWORDS 
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, W.Greenwich, Rhode
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

C=======OPTIONS/CHECK=NOOVERFLOW/EXT
	PROGRAM AGTPSW
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'

        INTEGER*4  K,SRTCNT, SORT(NUMAGT)
        INTEGER*4  ST,PASSLUN/7/,REC,XREC,FDB(7),AGENT,BITMAP
        CHARACTER  CZERO/Z0/

C       OPEN OUTPUT FILE
C       ----------------
        CALL DFILX('AGTPASS.FIL',0,0,ST)
        OPEN (UNIT=PASSLUN, FILE='AGTPASS.FIL', IOSTAT=ST,
     *       STATUS='NEW', DISP='KEEP', ACCESS='SEQUENTIAL',
     *       FORM='FORMATTED', RECORDTYPE='FIXED',
     *       RECL=144, CARRIAGECONTROL='LIST')
        IF(ST.NE.0) THEN
           TYPE*,IAM(),'CRKACTPC.FIL Open error  st - ',ST
           CALL USRCLOS1(10)
           CALL GSTOP(GEXIT_FATAL)
        ENDIF

C       OPEN ASF.FIL
C       ------------
        CALL OPENW(1,SFNAMES(1,ASF),0,0,0,ST)
        CALL IOINIT(FDB,1,ASFSEC*256)
        IF(ST.NE.0) THEN
           CALL USRCLOS1(1)
        ENDIF

C       READ AND SORT BY AGENT NUMBER
C       -----------------------------
        CALL SRTFLD(1,1,SORT,SRTCNT)

C       READ ALL AGENTS BY AGENT NUMBER
C       -------------------------------
        DO 100 REC=1,SRTCNT
           XREC = SORT(REC)

           CALL READW(FDB,XREC,ASFREC,ST)
           IF(ST.NE.0) CALL USRCLOS1(1)

           AGENT=0
           CALL ASCBIN(ASFINF,SAGNO,LAGNO,AGENT,ST)
           IF(AGENT.EQ.0.OR.ST.NE.0) GOTO 100

           BITMAP=0
           CALL ASCBIN(ASFINF,STTYP,LTTYP,BITMAP,ST)
           IF(TSBIT(BITMAP,AGTTOI).NE.0 .OR. ST.NE.0) GOTO 100    !GVTs only

           DO 150 K=1,512
              IF(ASFBYT(K).EQ.CZERO) ASFBYT(K)=' '
150        CONTINUE

           WRITE(PASSLUN,900) (ASFBYT(SAGNO+K-1),K=1,LAGNO-1),
     *                  ASFBYT(LAGNO),
     *                  (ASFBYT(SNAME+K-1),K=1,LNAME),
     *                  (ASFBYT(SSTRT+K-1),K=1,LSTRT),
     *                  (ASFBYT(SCITY+K-1),K=1,LCITY),
     *                  (ASFBYT(SZIPC+K-1),K=1,LZIPC),
     *                  (ASFBYT(STELE+K-1),K=1,LTELE),
     *                  (ASFBYT(SPAS1+K-1),K=1,LPAS1),
     *                  (ASFBYT(SPAS2+K-1),K=1,LPAS2),
     *                  (ASFBYT(SPAS3+K-1),K=1,LPAS3),
     *                  (ASFBYT(SPAS4+K-1),K=1,LPAS4),
     *                  (ASFBYT(SPAS5+K-1),K=1,LPAS5),
     *                  (ASFBYT(SPAS6+K-1),K=1,LPAS6),
     *                  (ASFBYT(SPAS7+K-1),K=1,LPAS7),
     *                  (ASFBYT(SPAS8+K-1),K=1,LPAS8) 


100     CONTINUE    ! read next agent

        CALL USRCLOS1(1)
        CLOSE(UNIT=PASSLUN)
        CALL GSTOP(GEXIT_SUCCESS)
C
900     FORMAT(6A,'-',1A,27A,30A,18A,9A,12A,8(1X,4A))
        END
