C  GXSRC:BNGFHPRG.FOR
C  
C V12 28-JAN-2000 RXK Format of input file changed.
C V11 13-JAN-2000 RXK Def-file for Bingo division names added
C V10 27-APR-1999 RXK Call of WIMG/YESNO replaced with call of PRMYESNO.
C V09 09-SEP-1997 UXN Removing BINGOFTP.DEF, significant changes for new BINGO.
C V08 12-JAN-1995 PXB Add screen display and checksumming.
C V07 08-JAN-1995 HXK Allow for 5 penny units on Central
C V06 08-JAN-1995 HXK WRITE DBN 'holds' TO FILE
C V05 06-JAN-1995 HXK Use B_WRKSHR which is defined in WINCOM
C V04 19-DEC-1994 HXK Rearranged SHRs
C V03 19-DEC-1994 HXK Cosmetic changes
C V02 13-DEC-1994 JXP Do not write share counts to file 
C V01 09-DEC-1994 JXP Initial revision.
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

        PROGRAM  BNGFHPRG

        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:DBNREC.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:BNGDNAM.DEF'

        INTEGER*4  FDB(7)
        INTEGER*4  GNUM,GIND,ST,I,J,K
        INTEGER*4  FLAG,LUN
        INTEGER*4  YEAR,WEEK,DRAW
        
        CHARACTER*8     TEMP_ROUND
        CHARACTER*12    TEMP_DATE
        CHARACTER*12    TEMP_TIME
        CHARACTER*8     HOLD_TOTSHR
        CHARACTER*12    HOLD_TOTAMT
        CHARACTER*12    TEMP_CHECK

        CHARACTER*20 CFILNAM
        INTEGER*4    FILNAM(5)
        EQUIVALENCE  (FILNAM,CFILNAM)

        COMMON SCFREC
        INTEGER*4 SCFNAM(5)
        DATA SCFNAM/'SCF.','FIL ',3*'    '/

        CHARACTER*200   INPUT_LINE
        BYTE            BYTE_INP(200)
        EQUIVALENCE     (INPUT_LINE,BYTE_INP)
        INTEGER*4       RECTYPE,XLEN,TMP
        INTEGER*4       TOTCHK          ! checksum...

        INTEGER*4     LOT_DIV(BGOLOT)
C       
        CALL COPYRITE
        TYPE *, IAM()
        TYPE *, IAM(),
     *          '<<<<< Fullhouse Prognosis and win share to online >>>>>'
        TYPE *, IAM()

C
C READ SCF RECORD
C
        CALL OPENW(1,SCFNAM,4,0,0,ST)
        CALL IOINIT(FDB,1,SCFSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)
        CALL READW(FDB,1,SCFREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFNAM,2,ST,1)
        CALL CLOSEFIL(FDB)

C        CALL CLRSCR(5)

C
C PROCESS INPUT FILE
C
        CFILNAM='FILE:PCTOBFH.FIL'
        LUN=7
        CALL GETLUN(LUN)
C
C The longest record in PCTOBFH.FIL is 116 bytes ...    
C

        OPEN(UNIT=LUN, FILE=CFILNAM,
     *          IOSTAT=ST,
     *          STATUS='OLD',
     *          ORGANIZATION='SEQUENTIAL',
     *          ACCESS='SEQUENTIAL',
     *          RECL=116)
C
        IF(ST.NE.0) THEN
             TYPE*,IAM(),'Error opening ',CFILNAM,' status=',ST
             CALL GSTOP(GEXIT_FATAL)
        ENDIF

        GIND=1
        GNUM=SCFGTN(TBNG,GIND)
        CALL OPENW(1,SCFGFN(1,GNUM),4,0,0,ST)
        CALL IOINIT(FDB,1,DBNSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
C
C Initialize variables
C
        TOTCHK = 0

C
        CALL READ_LINE(LUN,CFILNAM,INPUT_LINE)
        RECTYPE = CTOI(INPUT_LINE(1:4),XLEN)
C
C File header : rectype = 10
C WPW changed draw input line from 27:35 to 27:34
C
        IF(RECTYPE.EQ.10) THEN
            YEAR = CTOI(INPUT_LINE(09:17),XLEN)
            WEEK = CTOI(INPUT_LINE(18:26),XLEN)
            DRAW = CTOI(INPUT_LINE(27:34),XLEN)
            
C
C Check that draw number in the file is equal to current draw number
C
            IF(BNGDRW(GIND).NE.DRAW) THEN
              TYPE*,IAM(),'DRAW NUMBER IN ',CFILNAM,'> ',DRAW
              TYPE*,IAM(),'IS DIFFERENT FROM CURRENT DRAW     > ',
     *                    BNGDRW(GIND)
              CALL GPAUSE
            ENDIF
            CALL READW(FDB,DRAW,DBNREC,ST)
            IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
C Initialize Lottery divisions.
C
            CALL FASTSET(0,LOT_DIV,BGOLOT)
            DO K=1,BGODIV
              IF(DBNDNR(K).NE.0) LOT_DIV(DBNDNR(K)) = K
            ENDDO
C
            TEMP_ROUND = INPUT_LINE(23:24)//'/'//INPUT_LINE(13:16)
            TEMP_DATE  = INPUT_LINE(33:44)
            TEMP_TIME  = INPUT_LINE(45:56)
            CALL BINGO_CHECKSUM(BYTE_INP,TOTCHK,56)  ! calculate checksum...
        ELSE
            TYPE*,IAM(),'Error - incorrect record type - ',RECTYPE
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
        CALL READ_LINE(LUN,CFILNAM,INPUT_LINE)
        RECTYPE = CTOI(INPUT_LINE(1:4),XLEN)
C
C Number of drawn numbers record for BINGO ( rectype = 50 )
C
        IF(RECTYPE.EQ.50) THEN
C
C Numbers for subphases
C
            DBNSPHH(1) = CTOI(INPUT_LINE(49:52),XLEN)   !these go to file 
            DBNSPHH(2) = CTOI(INPUT_LINE(53:56),XLEN)
            DBNSPHH(3) = CTOI(INPUT_LINE(57:60),XLEN)
            DBNSPHH(4) = CTOI(INPUT_LINE(61:64),XLEN)
            DBNSPHH(5) = CTOI(INPUT_LINE(65:68),XLEN)

            DBNPHSH(1) = DBNSPHH(4)
            DBNPHSH(2) = DBNSPHH(5)
C
C Worst and second worst hit amount.
C
            DBNWSTH    = CTOI(INPUT_LINE(41:44),XLEN)
            DBNWS2H    = CTOI(INPUT_LINE(45:48),XLEN)
C
C First singles, doubles, triples, quadruples
C
            CALL FASTSET(0,DBNNDFH,BGODIV)
            DO J = 0, 3
              IF(LOT_DIV(J*2+1).NE.0) THEN
                DBNNDFH(LOT_DIV(J*2+1)) = CTOI(INPUT_LINE(J*4+5:J*4+9),XLEN)
              ENDIF
            ENDDO       
            CALL BINGO_CHECKSUM(BYTE_INP,TOTCHK,68)
        ELSE
            TYPE*,IAM(),'Error - incorrect record type - ',RECTYPE
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
        CALL READ_LINE(LUN,CFILNAM,INPUT_LINE)
        RECTYPE = CTOI(INPUT_LINE(1:4),XLEN)
C
C Number of winners record ( rectype = 51 )
C
        IF(RECTYPE.EQ.51) THEN
C
            CALL FASTSET(0,B_WRKSHR,BGODIV)
C
C Read lottery divisions 1-8
C
            DO J = 0,7
              TMP = CTOI(INPUT_LINE(J*8+5:J*8+12),XLEN)
              IF(LOT_DIV(J+1).NE.0) B_WRKSHR(LOT_DIV(J+1)) = TMP
            ENDDO
C
C Read Fullhouse - fullhouse-4
C
            DO J = 0,4
              TMP = CTOI(INPUT_LINE(101-J*8:108-J*8),XLEN)
              IF(LOT_DIV(J+13).NE.0) B_WRKSHR(LOT_DIV(J+13)) = TMP
            ENDDO
C
C Read Worst
C       
            TMP = CTOI(INPUT_LINE(109:116),XLEN)
            IF(LOT_DIV(19).NE.0) B_WRKSHR(LOT_DIV(19)) = TMP
C  
            CALL BINGO_CHECKSUM(BYTE_INP,TOTCHK,116)
        ELSE
            TYPE*,IAM(),'Error - incorrect record type - ',RECTYPE
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
        CALL READ_LINE(LUN,CFILNAM,INPUT_LINE)
        RECTYPE = CTOI(INPUT_LINE(1:4),XLEN)
C
C Win amounts record ( rectype = 52 ) 
C
        IF(RECTYPE.EQ.52) THEN
C
            CALL FASTSET(0,DBNSHV(1,BGOFHS),BGODIV)
C
C Read lottery divisions 1-8
C
            DO J = 0,7
              TMP = CTOI(INPUT_LINE(J*8+5:J*8+12),XLEN)
              IF(LOT_DIV(J+1).NE.0) DBNSHV(LOT_DIV(J+1),BGOFHS) = TMP*20
            ENDDO
C
C Read Fullhouse - fullhouse-4
C
            DO J = 0,4
              TMP = CTOI(INPUT_LINE(101-J*8:108-J*8),XLEN)
              IF(LOT_DIV(J+13).NE.0) DBNSHV(LOT_DIV(J+13),BGOFHS) = TMP*20
            ENDDO
C
C Read Worst
C       
            TMP = CTOI(INPUT_LINE(109:116),XLEN)
              IF(LOT_DIV(19).NE.0) DBNSHV(LOT_DIV(19),BGOFHS) = TMP*20
C  
            CALL BINGO_CHECKSUM(BYTE_INP,TOTCHK,116)
        ELSE
            TYPE*,IAM(),'Error - incorrect record type - ',RECTYPE
            CALL GSTOP(GEXIT_FATAL)
        ENDIF

        CALL READ_LINE(LUN,CFILNAM,INPUT_LINE)
        RECTYPE = CTOI(INPUT_LINE(1:4),XLEN)
C
C Speculative record ( rectype = 53 )
C
        IF(RECTYPE.EQ.53) THEN
            CALL BINGO_CHECKSUM(BYTE_INP,TOTCHK,72)
        ELSE
            TYPE*,IAM(),'Error - incorrect record type - ',RECTYPE
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
        CALL READ_LINE(LUN,CFILNAM,INPUT_LINE)
        RECTYPE = CTOI(INPUT_LINE(1:4),XLEN)
C
C Sum record ( rectype = 59 )
C 
        IF(RECTYPE.EQ.59) THEN  
            HOLD_TOTSHR = INPUT_LINE(5:12)
            HOLD_TOTAMT = INPUT_LINE(13:24)
            CALL BINGO_CHECKSUM(BYTE_INP,TOTCHK,24)
        ELSE
            TYPE*,IAM(),'Error - incorrect record type - ',RECTYPE
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
        CALL READ_LINE(LUN,CFILNAM,INPUT_LINE)
        RECTYPE = CTOI(INPUT_LINE(1:4),XLEN)
C
C End record ( rectype = 99 )
C
        IF(RECTYPE.EQ.99) THEN
            IF (CTOI(INPUT_LINE(5:12),XLEN) .NE. TOTCHK) THEN
              TEMP_CHECK = '*** NOT OK'
            ELSE
              TEMP_CHECK = 'OK'
            END IF
        ELSE
            TYPE*,IAM(),'Error - incorrect record type - ',RECTYPE
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C Display records and ask for confirmation.
C
        WRITE(6,*) IAM()//'Bingo Fullhouse Prognosis Update'
        WRITE(6,*) IAM()
        WRITE(6,*) IAM()//'------------------------------------------------------'
        WRITE(6,*) IAM()
        WRITE(6,*) IAM()//'Read file:          '//CFILNAM
        WRITE(6,*) IAM()
        WRITE(6,*) IAM()//'Round:              '//TEMP_ROUND
        WRITE(6,900) IAM(),'Draw:          ',DRAW
        WRITE(6,*) IAM()//'Time:               '//TEMP_DATE(3:12)//' '//
     *                                              TEMP_TIME(5:12)
        WRITE(6,*) IAM()
        WRITE(6,*) IAM()//'Prognosis Bingo Fullhouse'
        WRITE(6,*) IAM()
        WRITE(6,900) IAM(),'Number of numbers:  Phase 1:     ',DBNSPHH(1)
        WRITE(6,900) IAM(),'                    Phase 2:     ',DBNSPHH(2)
        WRITE(6,900) IAM(),'                    Phase 3:     ',DBNSPHH(3)
        WRITE(6,900) IAM(),'                    Phase 4:     ',DBNSPHH(4)
        WRITE(6,900) IAM(),'                    Phase 5(FH): ',DBNSPHH(5)
        IF(LOT_DIV(19).NE.0) THEN
          WRITE(6,900) IAM(),'                    Worst match: ',DBNWSTH
        ENDIF
        IF(LOT_DIV(20).NE.0) THEN
          WRITE(6,900) IAM(),'                    Second worst:',DBNWS2H
        ENDIF
        DO I=1,BGODIV
          IF(DBNFST(I,BGOFHS).NE.0) THEN
            WRITE(6,902) IAM(),BNGDNAMES(DBNDNR(I)),DBNNDFH(I)
          ENDIF
        ENDDO
        WRITE(6,*) IAM()
        WRITE(6,*) IAM()//'Prognosis           Division     #wins      '//
     *                      'Amount mk'
        WRITE(6,*) IAM()//'                    ------------------------'//
     *                      '---------'
        DO I=1,BGODIV
           IF(DBNDNR(I).NE.0) THEN 
             WRITE(6,901) IAM(),BNGDNAMES(DBNDNR(I)),
     *                    B_WRKSHR(I),DBNSHV(I,BGOFHS)/20
           ENDIF
        ENDDO 
        WRITE(6,*) IAM()//'                    ============'//
     *               '====================='
        WRITE(6,*) IAM()//'                    Total     '//HOLD_TOTSHR//
     *                    HOLD_TOTAMT//'.00'
        WRITE(6,*) IAM()
        WRITE(6,900) IAM(),'Checksum:           '//TEMP_CHECK
        WRITE(6,*) IAM()//'------------------------------------------------------'

        CALL PRMYESNO('Is this correct [Y/N] > ',FLAG)

        IF (FLAG .EQ. 1) THEN
          CALL WRITEW(FDB,DRAW,DBNREC,ST)
          IF (ST .NE. 0) THEN
              CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
          ELSE
              TYPE*,IAM(),' *** Bingo Fullhouse Prognosis entered'
          END IF
        ELSE
           CALL GSTOP(GEXIT_OPABORT)
        ENDIF

900     FORMAT(1X,A18,A,I8)
901     FORMAT(1X,A18,A29,':',I8,4X,I8,'.00')
902     FORMAT(1X,A18,20X,A8,5X,I8)
        END
C*************************************************
C
C Subroutine for reading a text line from the file.
C
C*************************************************
        SUBROUTINE READ_LINE(LUN,FILE_NAME,OUTPUT)
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSDEFINE.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INTEGER*4       LUN     
        CHARACTER*20    FILE_NAME
        CHARACTER*200   OUTPUT
C
        INTEGER*4       ST
C
        READ (LUN,IOSTAT=ST,FMT='(A)') OUTPUT
        IF(ST.NE.0) THEN
            TYPE*,IAM(),'Error reading ',FILE_NAME,' status=',ST
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
        END
        
         
