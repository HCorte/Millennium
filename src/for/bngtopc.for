C  GXSRC:BNGTOPC.FOR
C  
C V12 19-APR-2000 UXN Fix for GIND and DRAW
C V11 01-Feb-2000 RXK Output date fixed
C V10 11-Nov-1999 RXK Changed for ALPHA (UXN).
C V09 09-Feb-1996 HXK Bug fix
C V08 14-Sep-1995 HXK Allow for no cancels
C V07 31-Mar-1995 HXK Fix for grand total
C V06 21-Mar-1995 HXK Fix so that share value reported for Bingo A,B division 
C                     one is that which is expected by TV Studio
C V05 12-Jan-1995 PXB added screen display and fixed errors in checksum.
C V04 20-Dec-1994 HXK Amendments made per customer request
C V03 13-Dec-1994 JXP First record in temporary draw & tm scan files, 
C                     is now the first good seed in draw range.
C V02 09-Dec-1994 JXP Applying PVCS header for automatic revision history
C V01 09-Dec-1994 JXP Initial revision.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        
        PROGRAM  BNGTOPC

        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF' 
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:RECDAF.DEF'

        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'

        INTEGER*2   DATE(12)
        INTEGER*4   GNUM,GIND,ST,I,K,EXT
        INTEGER*4   FLAG,LUN,OPT,TLUN
        INTEGER*4   DRAW,BYTLEN
        INTEGER*4   FIRST_IND(2),LAST_IND(2),LAST_SOLD,FIRST_SOLD
        INTEGER*4   CAN_NO
        INTEGER*4   WEK
        INTEGER*4   MAX_NO
        INTEGER*4   MONTH,YEAR,DAY
        INTEGER*4   YEAR2
        INTEGER*4   VALCNT(2),CANCNT(2),SALAMT(2)
        INTEGER*4   TOTVAL
        INTEGER*4   TOTSAL
        INTEGER*4   OUTLEN
        INTEGER*4   TOTCAN  ! TOTCAN IS NO. OF MISSING SEEDS BETWEEN DRAW AND
                            ! TM TEMP FILES

        INTEGER*4   LAST_SEED,CUR_SEED

        CHARACTER*8     CTIME
        CHARACTER*80    OUTBUF
        CHARACTER*120   TINBUF

        CHARACTER*5     C5_BLANK
        CHARACTER*7     C7_BLANK
        CHARACTER*8     HOLD_CHECKSUM
        CHARACTER*12    C12_BLANK
        INTEGER*4       RPLU /4/
        INTEGER*4       COPY 
        INTEGER*4       PAGE
        INTEGER*4       TOTCHK
        CHARACTER*50    HEAD /' '/
 
        CHARACTER*(*)   I8FMT, I4FMT, I2FMT, I20FMT
        PARAMETER       (I20FMT='(I20)')
        PARAMETER       (I8FMT='(I8)')
        PARAMETER       (I4FMT='(I4)')
        PARAMETER       (I2FMT='(I2)')
 
        CHARACTER*20 CFILNAM
        INTEGER*4    FILNAM(5)
        EQUIVALENCE  (FILNAM,CFILNAM)

        CHARACTER*20 TFILNAM(2)
   
        DATA            MAX_NO/2147483647/ 

        BYTE            B_CHECKSUM(80)

        EQUIVALENCE (B_CHECKSUM,OUTBUF)

        LOGICAL     FIRST
        CHARACTER*10 C_DATE
C
C SET / CLEAR VARIABLES
C
        C5_BLANK = ' '
        C7_BLANK = ' '
        C12_BLANK = ' '
        CAN_NO=0  
        TOTCHK = 0      

	GIND = 1
        GNUM = GTNTAB(TBNG,GIND)
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
            TYPE*,IAM(),'Invalid game number specified ',GNUM,GIND
            CALL GSTOP(GEXIT_FATAL)
        ENDIF

        DRAW=DAYDRW(GNUM)
        IF(DRAW.EQ.0) DRAW=DAYHDR(GNUM)
        IF(DRAW.LE.0) THEN
            TYPE *,' '
            CALL INPNUM('Enter draw number ',DRAW,1,99999,EXT)
            TYPE *,' '
            IF(EXT.LT.0) GOTO 9999
        ENDIF

        IF(DAYSTS.NE.DSOPEN) THEN
           IF(BNGSTS(GIND).NE.GAMBFD) THEN
              WRITE(6,904) IAM(),GTNAMES(TBNG),GIND,DRAW,BNGSTS(GIND)
              CALL GPAUSE
           ENDIF
        ENDIF
C

100     CONTINUE
C       CALL CLRSCR(5)
        WRITE(6,900)
        CALL INPNUM('Enter option ',OPT,1,1,EXT)
        IF(EXT.LT.0) GOTO 9999

C       
C       OPEN FTP TRANSFER FILE
C       ----------------------
        CFILNAM='FILE:BFHTOPC.FIL'
        LUN=7
        CALL GETLUN(LUN)
        BYTLEN=56
        CALL FTP_OPEN(FILNAM,LUN,BYTLEN,ST)
        IF(ST.NE.0) THEN
            CALL FILERR(FILNAM,1,ST,0)
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
        TYPE*,IAM(),' Creating ',CFILNAM
C
C SET UP HEADER RECORD
C
        OUTLEN = 56
        DATE(VCDC)=DAYCDC
        CALL FIGWEK(DAYCDC,WEK,YEAR2)
        CALL GDATE(MONTH,DAY,YEAR)
        IF(YEAR.LT.77) THEN
          YEAR = YEAR + 2000
        ELSE
          YEAR = YEAR + 1900
        ENDIF
        CALL TIME(CTIME)
        WRITE(C_DATE,915) DAY,MONTH,YEAR
        WRITE(OUTBUF,910) YEAR2,WEK,DRAW,C_DATE,CTIME
        WRITE(UNIT=LUN,FMT=902,IOSTAT=ST) OUTBUF(1:OUTLEN)
        IF(ST.NE.0) THEN
          CALL FILERR(FILNAM,3,ST,0)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
        CALL BINGO_CHECKSUM(B_CHECKSUM,TOTCHK,OUTLEN)
C
C       Fullhouse seeds and cancels to TV studio
C
1000    CONTINUE
        TOTSAL=0
        TOTVAL=0
        TOTCAN=0
C
C Cancelled index record ( record type = 12 )
C
        OUTLEN = 32
C Read DRWTMP.FTP and TMFTMP.FTP file

        TFILNAM(1)='FILE:DRWTMP.FTP'
        TFILNAM(2)='FILE:TMFTMP.FTP'
        DO I = 1,2
          SALAMT(I) = 0
          VALCNT(I) = 0
          CANCNT(I) = 0
          TLUN = 7
          CALL GETLUN(TLUN)  
          OPEN(UNIT=TLUN,FILE=TFILNAM(I),IOSTAT=ST,
     *              STATUS='OLD',BLOCKSIZE=512,
     *              ORGANIZATION='SEQUENTIAL',
     *              ACCESS='SEQUENTIAL')
          IF (ST .NE. 0) THEN
             TYPE*,IAM(),'Error opening ',TFILNAM(I),' status=',ST
             CALL GSTOP(GEXIT_FATAL)
          END IF
          READ(UNIT=TLUN,FMT=903,IOSTAT=ST) TINBUF
          IF (ST .NE. 0) THEN
             TYPE*,IAM(),'Error reading ',TFILNAM(I),' status=',ST
             CALL GSTOP(GEXIT_FATAL)
          END IF
C
C Scan through DRWTMP.FTP (output from DFTPSCAN) and TMFTMP:FTP (output from
C TFTPSCAN) - both tasks run from FTP_SCAN.
C
C Layout of above temp filse is
C 
C First record -> first good seed of range used
C
C      Many bad seeds
C
C Last record -> Total sales, valid seed count, bad seed count, first seed,
C                last seed in range.
C
C Check for missing seeds between draw files scan and TMF scan.
C
C
          FIRST=.TRUE.
          DO WHILE(ST.EQ.0)
           IF(TINBUF(1:5).EQ.'TOTAL') THEN
             READ(TINBUF( 6:25),I20FMT) SALAMT(I)
             READ(TINBUF(26:45),I20FMT) VALCNT(I)
             READ(TINBUF(46:65),I20FMT) CANCNT(I)
             READ(TINBUF(66:85),I20FMT) FIRST_IND(I)
             READ(TINBUF(86:105),I20FMT) LAST_IND(I)
           ELSEIF(FIRST) THEN
             READ(TINBUF(1:20),I20FMT) FIRST_IND(I)
             FIRST=.FALSE.
             IF(I.EQ.2) THEN
               IF(FIRST_IND(2).NE.LAST_IND(1)+1) THEN ! missing seeds may exist
                 LAST_SEED=LAST_IND(1)
                 CUR_SEED=FIRST_IND(2) 
C
                 IF(LAST_SEED.NE.CUR_SEED-1) THEN ! missing seeds do exist
                   IF(LAST_SEED.LT.CUR_SEED-1) THEN
                     DO K=LAST_SEED+1,CUR_SEED-1
                        CAN_NO=CAN_NO+1
                        WRITE(OUTBUF,912) CAN_NO,K
                        WRITE(UNIT=LUN,FMT=902,IOSTAT=ST) OUTBUF(1:OUTLEN)
                        IF(ST.NE.0) THEN
                           CALL FILERR(FILNAM,3,ST,0)
                           CALL GSTOP(GEXIT_FATAL)
                        ENDIF
                        TOTCAN=TOTCAN+1
                        CALL BINGO_CHECKSUM(B_CHECKSUM,TOTCHK,OUTLEN)
                      ENDDO
                   ELSE
                      DO K=LAST_SEED+1,MAX_NO
                        CAN_NO=CAN_NO+1
                        WRITE(OUTBUF,912) CAN_NO,K
                        WRITE(UNIT=LUN,FMT=902,IOSTAT=ST) OUTBUF(1:OUTLEN)
                        IF(ST.NE.0) THEN
                          CALL FILERR(FILNAM,3,ST,0)
                          CALL GSTOP(GEXIT_FATAL)
                        ENDIF
                        TOTCAN=TOTCAN+1
                        CALL BINGO_CHECKSUM(B_CHECKSUM,TOTCHK,OUTLEN)
                      ENDDO
                      DO K=0,CUR_SEED-1
                        CAN_NO=CAN_NO+1
                        WRITE(OUTBUF,912) CAN_NO,K
                        WRITE(UNIT=LUN,FMT=902,IOSTAT=ST) OUTBUF(1:OUTLEN)
                        IF(ST.NE.0) THEN
                           CALL FILERR(FILNAM,3,ST,0)
                           CALL GSTOP(GEXIT_FATAL)
                        ENDIF
                        TOTCAN=TOTCAN+1
                        CALL BINGO_CHECKSUM(B_CHECKSUM,TOTCHK,OUTLEN)
                      ENDDO
                   ENDIF
                 ENDIF
               ENDIF
             ENDIF
           ELSE
             CAN_NO=CAN_NO+1
             READ(TINBUF(1:20),I20FMT) CUR_SEED
             WRITE(OUTBUF,912) CAN_NO,CUR_SEED
             WRITE(UNIT=LUN,FMT=902,IOSTAT=ST) OUTBUF(1:OUTLEN)
             IF(ST.NE.0) THEN
               CALL FILERR(FILNAM,3,ST,0)
               CALL GSTOP(GEXIT_FATAL)
             ENDIF
             CALL BINGO_CHECKSUM(B_CHECKSUM,TOTCHK,OUTLEN)
           ENDIF
           READ(UNIT=TLUN,FMT=903,IOSTAT=ST) TINBUF
          ENDDO
          CALL USRCLOS1(TLUN)
        ENDDO
C
C Index info record ( record type = 11 )
C
        OUTLEN=44
        FIRST_SOLD=FIRST_IND(1)
C
C Set the correct valid FIRST_SOLD
C
        IF(VALCNT(1).EQ.0) FIRST_SOLD=FIRST_IND(2)  
        LAST_SOLD=LAST_IND(2)
C
C Set the correct valid LAST_SOLD
C
        IF(VALCNT(2).EQ.0) LAST_SOLD=LAST_IND(1)
        WRITE(OUTBUF,911) FIRST_SOLD,LAST_SOLD
        CALL BINGO_CHECKSUM(B_CHECKSUM,TOTCHK,OUTLEN)
        WRITE(UNIT=LUN,FMT=902,IOSTAT=ST) OUTBUF(1:OUTLEN)
        IF(ST.NE.0) THEN
          CALL FILERR(FILNAM,3,ST,0)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C Sum record ( record type = 19 )
C
        OUTLEN = 32

        TOTSAL=SALAMT(1)+SALAMT(2)
        TOTVAL=VALCNT(1)+VALCNT(2)  
        TOTCAN=TOTCAN+CANCNT(1)+CANCNT(2)
        WRITE(OUTBUF,913) TOTVAL,TOTCAN,TOTSAL
        CALL BINGO_CHECKSUM(B_CHECKSUM,TOTCHK,OUTLEN)
        WRITE(UNIT=LUN,FMT=902,IOSTAT=ST) OUTBUF(1:OUTLEN)
        IF(ST.NE.0) THEN
          CALL FILERR(FILNAM,3,ST,0)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C End record ( record type = 99 )
C
        OUTLEN = 12
        WRITE(OUTBUF,914) TOTCHK
        WRITE(UNIT=LUN,FMT=902,IOSTAT=ST) OUTBUF(1:OUTLEN)
        IF(ST.NE.0) THEN
          CALL FILERR(FILNAM,3,ST,0)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C Display collected information for  confirmation.
C
        WRITE(6,*) IAM()
        WRITE(6,*) IAM()//'Bingo Seeds and Cancels from Online to TV-Studio'
        WRITE(6,*) IAM()
        WRITE(6,*) IAM()//'------------------------------------------------------'
        WRITE(6,*) IAM()
        WRITE(6,916) IAM(),'Round:              ',WEK,YEAR2
        WRITE(6,917) IAM(),'Draw:               ',DRAW
        WRITE(6,918) IAM(),'Time:               ',C_DATE,CTIME
        WRITE(6,*) IAM()
        WRITE(6,*) IAM()//'Sales Information  Bingo Fullhouse'
        WRITE(6,*) IAM()
        WRITE(6,917) IAM(),'First Seed:         ',FIRST_SOLD
        WRITE(6,917) IAM(),'Last Seed:          ',LAST_SOLD
        WRITE(6,917) IAM(),'Number Cancels:     ',TOTCAN
        WRITE(6,*) IAM()
        WRITE(6,917) IAM(),'Number Tickets:     ',TOTVAL
        WRITE(6,919) IAM(),'Sales mk:           ',TOTSAL
        WRITE(6,*) IAM()
        WRITE(6,*) IAM()//'------------------------------------'

        CALL WIMG(5,'Is this correct [Y/N] ')

        CALL YESNO(FLAG)

        IF (FLAG .EQ. 1) THEN         !--- Answer was Yes.
          CALL USRCLOS1(LUN)          !--- Close FTP file.
          TYPE*,IAM(),'*** Created file:    ',CFILNAM(6:20)
          TYPE*,IAM(),'    Checksum:        ',HOLD_CHECKSUM

C---- Now create report.

          HEAD = 'Bingo Fullhouse Sales Information Transfer' 

          CALL ROPEN('BFHTOPC.REP',RPLU,ST)
          IF (ST .NE. 0) THEN
            TYPE *,IAM(),'BFHTOPC.REP',' report file open error > ',ST
            GOTO 100
          END IF
          CALL TITLE(HEAD,'  BFHTOPC', 1, RPLU, PAGE, DAYCDC)
          WRITE (RPLU,8000)
          WRITE (RPLU,8001) CFILNAM(6:20),WEK,YEAR2,DRAW,
     *                      C_DATE,CTIME,
     *                      FIRST_SOLD,LAST_SOLD,TOTCAN,
     *                      TOTVAL,TOTSAL,
     *                      TOTCHK
          CLOSE(UNIT=RPLU)
          COPY = 1
          CALL SPOOL('BFHTOPC.REP',COPY,EXT)
          GOTO 100
        ELSE                                   !--- Answer was No.
          CLOSE (UNIT=LUN,STATUS='DELETE')     !--- Close and delete ftp file.
          GOTO 100
        END IF
C--------------  Format Statements  -----------------------------------
        
900     FORMAT(///,T5,' Bingo Online to TV studio functions:',
     *   //,T5,'1',5X,'- Fullhouse seeds and cancels to TV studio',
     *   //,T5,'E',5X,'- Exit',//)
901     FORMAT(A2)
902     FORMAT(A<OUTLEN>)
903     FORMAT(A120)
904     FORMAT(1X,A,1X,A8,I1,' draw ',I4,' invalid game status> ',I4)

910     FORMAT('  10','   1',I8,I8,I8,A12,A12)  ! Header record.
911     FORMAT('  11',I20,I20)                  ! Index info record.
912     FORMAT('  12',I8,I20)                   ! Cancelled index record.
913     FORMAT('  19',I8,I8,I12)                ! Sum record.
914     FORMAT('  99',I8)                       ! End record.
915     FORMAT(I2.2,'.',I2.2,'.',I4.4)          ! date format.
916     FORMAT(1X,A18,A20,8X,I2.2,'/',I4.4)
917     FORMAT(1X,A18,A20,I8)
918     FORMAT(1X,A18,A20,8X,A10,' ',A8)
919     FORMAT(1X,A18,A20,I8,'.00')
C---- Report Format Statements.

C---- Fullhouse.

8000    FORMAT (//,4X,'Bingo Tayskasi ',4X,'Myyntitietojen siirto',///)

8001    FORMAT (4X,'Tiedosto:      ',8X,A20,//,
     *          4X,'Kierros:       ',16X,I2.2,'/',I4.4,/,
     *          4X,'Draw:          ',16X,I8,/,
     *          4X,'Aika:          ',6X,A8,2X,A8,//,
     *          4X,'Myyntitiedot',//,
     *          4X,'Alku seed:     ',4X,I20,/,
     *          4X,'Loppu seed:    ',4X,I20,/,
     *          4X,'Mitatoityja:   ',16X,I8,//,
     *          4X,'Tositteita:    ',16X,I8,/,
     *          4X,'Vaihto mk:     ',9X,I12,'.00',//,
     *          4X,'Tarkistussumma:',16X,I8)

C---- Bingo AB

8002    FORMAT (//,4X,'Voittoennuste  ',4X,'BingoLotto A-B',//)

8003    FORMAT (4X,'Tiedosto:      ',8X,A20,//,
     *          4X,'Kierros:       ',16X,A8,/,
     *          4X,'Draw:          ',16X,A8,/,
     *          4X,'Aika:          ',6X,A8,2X,A8,//,
     *          4X,'Ennuste:       ',4X,'Voittoluokka',5X,'Kpl',13X,'mk',/,
     *          23X,'-----------------------------------',/,
     *          23X,'Superbingo',2X,A8,4X,A8,'.00',/,
     *          23X,'Tuplabingo',2X,A8,4X,A8,'.00',/,
     *          23X,'Bingo     ',2X,A8,4X,A8,'.00',/,
     *          23X,'===================================',/,
     *          23X,'Yht.      ',2X,A8,A12,'.00',///,
     *          4X,'Tarkistussumma:',16X,A8)

9999    CONTINUE        
        END
