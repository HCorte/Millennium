C  GXSRC:CNVBNG.FOR
C
C  $Log:   GXAFIP:[GOLS]CNVBNG.FOV  $
C  
C
C  PROGRAM TO CONVERT BINGO B1F.FIL 
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXTEND
        PROGRAM CNVBNG
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DBNREC.DEF'
        INCLUDE 'INCLIB:OLD_DBNREC.DEF'
C
C
        INTEGER*4   ST,EXT,I,J,K,DRAW,RECNO
        INTEGER*4   FDB(7),OLD_FDB(7)

        INTEGER*4   FILNAME(5)
        CHARACTER   CFILNAME(20)
        EQUIVALENCE (CFILNAME,FILNAME)
C
        CALL COPYRITE
C
        TYPE *,'Old B1F length = ',ODBNLEN, '  # of sectors = ',ODBNSEC
        TYPE *,'New B1F length = ',DBNLEN,  '  # of sectors = ',DBNSEC
        TYPE *,'Old B1F is supposed to be FILE:B1F.FIL'
C
100     CONTINUE
        CALL WIMG(5,'Enter file name (VOLN:FILNAME) for new B1F')
        READ(5,9000) FILNAME
        IF (FILNAME(1).EQ.'    ') GOTO 100
        IF(CFILNAME(1).EQ.'E '.OR.CFILNAME(1).EQ.'e ')
     *     CALL GSTOP(GEXIT_OPABORT)
C
        CALL OPENW(3,FILNAME,0,0,0,ST)
        CALL IOINIT(FDB,3,DBNSEC*256)
        IF(ST.NE.0) THEN
           TYPE *,' Opening new B1F, status =',ST
           CALL USRCLOS1(3)
           CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
        CALL OPENW(1,'FILE:B1F.FIL',0,0,0,ST)
        CALL IOINIT(OLD_FDB,1,ODBNSEC*256)
        IF(ST.NE.0) THEN
           TYPE *,'Opening old B1F, status =',ST
           CALL USRCLOS1(1)
           CALL GSTOP(GEXIT_FATAL)
        ENDIF
        CALL PRMNUM('Enter number of last completed draw ', 
     *               DRAW,1,9999,EXT)
        IF(EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
C
C Read first record from file
C
        RECNO = 1

        CALL READW(OLD_FDB,RECNO,ODBNREC,ST)
        IF(ST.NE.0) THEN
           TYPE *,'Reading first record, old B1F status =',ST
           CALL USRCLOS1(1)
           CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
        DO WHILE(ST.EQ.0)
C
           CALL FASTMOV(ODBNSTS, DBNSTS, ODBNSHV_OFF-1)
           CALL FASTMOV(ODBNWIN, DBNWIN, ODBNMAT_OFF-ODBNWIN_OFF)
           CALL FASTMOV(ODBNFRG, DBNFRG, ODBNMAP_OFF-ODBNFRG_OFF)
           DBNSPR = ODBNSPR
           DBNREV = ODBNREV
           DBNLOB = DRAW
C
           DO I=1,8            !old bgodiv
              DO K=1,BGOSUB
                 DBNSHV(I,K) = ODBNSHV(I,K)
                 DBNSHR(I,K) = ODBNSHR(I,K)
                 DBNPOL(I,K) = ODBNPOL(I,K)
                 DBNPAD(I,K) = ODBNPAD(I,K)
                 DBNPRG(I,K) = ODBNPRG(I,K)
                 DBNANU(I,K) = ODBNANU(I,K)
                 DBNBRK(I,K) = ODBNBRK(I,K)
                 DBNOSV(I,K) = ODBNOSV(I,K)
                 DBNFRZ(I,K) = ODBNFRZ(I,K)
                 DBNPER(I,K) = ODBNPER(I,K)
                 DBNTSR(I,K) = ODBNTSR(I,K)
                 DBNASH(I,K) = ODBNASH(I,K)
             ENDDO
           ENDDO

           DO I=1,15           !old bgomaxmap
              DO K=1,BGOSUB
                 DBNMAP(I,K) = ODBNMAP(I,K)
                 DBNMPF(I,K) = ODBNMPF(I,K)
             ENDDO
           ENDDO

           DO I=1,BGOPHS
              DBNPMT(1,I) = ODBNPMT(1,I)
              DBNPMT(2,I) = ODBNPMT(2,I)
           ENDDO     

           DO I=1,8               !old bgodiv
              DO J=1,15           !old bgomaxmap
                 DO K=1,BGOSUB
                    DBNAMP(I,J,K) = ODBNAMP(I,J,K)
                 ENDDO
             ENDDO
           ENDDO

           DO I=1,15      
              DO K=1,BGOSUB
                 DBNMAT(I,K) = ODBNMAT(I,K)
             ENDDO
           ENDDO
           DO K=1,BGOSUB
              DBNMAT(BGOMAXMAP+1,K) = ODBNMAT(15+1,K)      !worst
           ENDDO
           DO I=1, BGOCOL*BGOROW
              DO K=1,BGOSUB
                 DBNMAT(BGOMAXMAP+I+2,K) = ODBNMAT(15+I+1,K)
              ENDDO   
           ENDDO   

           IF(RECNO.LE.DRAW) THEN

              DO I=1,3                    ! AB
                 DBNOTH(I,1) = I
              ENDDO

              DO I=1,6
                 DBNOTH(I,2) = I          ! FH
              ENDDO
C
C FOR FULLHOUSE POINT ON DIVISION NUMBERS IN LOTTERY 1,...,20 SCALE
C
              DBNDNR(1) = 13  
              DBNDNR(2) = 14  
              DBNDNR(3) = 6  
              DBNDNR(4) = 4  
              DBNDNR(5) = 2  
              DBNDNR(6) = 19  
           ENDIF
C
C Write record to file
C
           CALL WRITEW(FDB,RECNO,DBNREC,ST)
           IF(ST.NE.0) THEN
             TYPE *,'Error - new B1F status =',ST
             TYPE *,'Writing record number',RECNO
             CALL USRCLOS1(3)

             CALL GSTOP(GEXIT_FATAL)
           ENDIF
C
C Read next records from file
C
           RECNO = RECNO + 1 
           CALL READW(OLD_FDB,RECNO,ODBNREC,ST)
           IF(ST.NE.0) THEN
              TYPE *,'Total number of records read',RECNO-1
           ENDIF
C
        ENDDO
C
        TYPE *,'Last converted record',RECNO-1
C
C
        CALL USRCLOS1(3)
        CALL USRCLOS1(1)
C
        TYPE*,'ATTENSION! Remember to rename new B1F to B1F.FIL and',
     *         ' move to FILE:'
        CALL GSTOP(GEXIT_SUCCESS)
C       
9000    FORMAT(5A4)
        END

