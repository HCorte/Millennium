C PROGRAM ENAPRM
C
C V03 16-JUN-2000 OXK Cleanup w/ WARNINGS=ALL
C V02 28-FEB-2000 RXK Cleaned up and implemented new promo type
C                     "if X weeks paid then add 1 free additional week"
C V01 20-MAR-1991 JPJ INITIAL RELEASE FOR MARYLAND
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
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM ENAPRM
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:RECPRM.DEF'
C
        INTEGER*4  PRMFDB(7), ST, FLAG, GTYP, GNUM, GIND
        INTEGER*4  I, OPT, EXT,K
	INTEGER*4  TER, NUMW

CV03        INTEGER*4  LIST(NUMAGT), COUNT, 
CV03        CHARACTER*5 CHRCNT
CV03        CHARACTER*20 PROMOTXT(1)   ! so far one type only
CV03        DATA PROMOTXT /'Add free week promo '/
CV03        LOGICAL    BROADCAST 

C
C
        CALL COPYRITE

        IF (.NOT.ISSUBPROC()) THEN
            TYPE*,IAM(),
     *            'This program can be run only from PROMOTION'
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C READ SCF RECORD
C
	CALL GETSCONF(SCFREC,ST)
C
C OPEN PROMO FILE
C
        CALL OPENW(1,SCFSFN(1,PRMO),4,0,0,ST)
        CALL IOINIT(PRMFDB,1,PRMSEC*256)
        IF(ST.NE.0) THEN
          CALL CLOSEFIL(PRMFDB)
          CALL FILERR(SCFSFN(1,PRMO),1,ST,0)
        ENDIF
C
C READ PROMO FILE
C
        CALL READW(PRMFDB,1,PRMREC,ST)
        IF(ST.NE.0) THEN
           CALL FILERR(SCFSFN(1,PRMO),2,ST,0)
        ENDIF
C
C GET OPTION
C
100     CONTINUE
        WRITE(6,900) IAM(),IAM(),IAM(),IAM()
        CALL PRMNUM('Enter option [E-Exit] ',OPT,1,3,EXT)
        IF(EXT.NE.0) GOTO 8000
        GOTO (1000,2000,8000) OPT
        GOTO 100
C
C CHANGE A GAMES STATUS
C
1000    CONTINUE
C
        TYPE *,IAM(),' Available game types:'
        TYPE *,IAM(),'----------------------'

        DO 1100 GNUM=1,MAXGAM
           GTYP=SCFGNT(GAMTYP,GNUM)
           GIND=SCFGNT(GAMIDX,GNUM)
           IF(GTYP.NE.TLTO .AND. GTYP.NE.TKIK) GOTO 1100
           WRITE(6,901) IAM(),GNUM,(SCFLGN(K,GNUM),K=1,4)
1100    CONTINUE
C
        CALL PRMNUM('Enter Game number you wish to change [E-Exit] ',
     *               GNUM,1,MAXGAM,EXT)
        IF(EXT.NE.0) GOTO 100

        GTYP=SCFGNT(GAMTYP,GNUM)
        GIND=SCFGNT(GAMIDX,GNUM)
        IF(.NOT.(GTYP.EQ.TLTO.AND.GIND.EQ.1.OR.GTYP.EQ.TKIK)) THEN
           TYPE *,IAM(),' Game not available'
           GOTO 100
        ENDIF
C
C DISABLE OR ENABLE
C
        IF(PRMTAB(PRMTYP,GNUM).NE.0) THEN
           CALL PRMYESNO('Game has a promo do you wish disablee it (Y/N)',FLAG)
           IF(FLAG.EQ.1) THEN
              IF(PRMTAB(PRMTYP,GNUM).EQ.PRMFRW) THEN
                 PRMTAB(PRMIFX,GNUM) = 0
              ENDIF
              PRMTAB(PRMTYP,GNUM) = 0
              DO TER=1,NUMAGT
                 CALL BCLR(PRMAFL(1,TER),GNUM)
              ENDDO
              TYPE *,IAM(),' Promotion file will be updated'
           ELSE
              TYPE *,IAM(),' Game not changed'
           ENDIF
           GOTO 100
        ELSE
           CALL PRMYESNO('Do you wish to add a promo (Y/N)',FLAG)
           IF(FLAG.NE.1) THEN
              TYPE *,IAM(),' Game not changed'
              GOTO 100
           ENDIF
        ENDIF
C
C SO FAR ONLY 1 TYPE OF PROMOTIONS IMPLEMENTED, SET IT WITHOUT ASKING FOR TYPE
C
        PRMTAB(PRMTYP,GNUM) = PRMFRW
        CALL PRMNUM('Enter number of weeks deserving a free draw [E-Exit]',
     *               NUMW,1,20,EXT)
        IF(EXT.NE.0) GOTO 100   
        PRMTAB(PRMIFX,GNUM) = NUMW
        TYPE *,IAM(),' Promotion file will be updated'
        GOTO 100
C
C CHANGE AN AGENTS STATUS
C
2000    CONTINUE

        TYPE *,IAM()
        TYPE *,IAM(),' Promotions per agent in Finland not used'
        CALL WIMG(6,'Hit return to continue')
        READ(5,904) K
        GOTO 100

CV032001    CONTINUE
CV03        CALL ASELECT(3,LIST,COUNT,BROADCAST,EXT)
CV03        IF(EXT.LT.0) GOTO 100
CV03        IF(COUNT.EQ.0) THEN
CV03          TYPE*,IAM(),' No agents found with field entered, try again'
CV03          GOTO 2000
CV03        ELSE
CV03          TYPE*,IAM(),COUNT,' agents selected'
CV03        ENDIF
CV03
CV03        TYPE *,IAM(),' Available game types:'
CV03        TYPE *,IAM(),'----------------------'
CV03        DO 2100 GNUM=1,MAXGAM
CV03           GTYP=SCFGNT(GAMTYP,GNUM)
CV03           GIND=SCFGNT(GAMIDX,GNUM)
CV03           IF(GTYP.NE.TLTO .AND. GTYP.NE.TKIK .OR.
CV03     *        GTYP.EQ.TLTO.AND.GIND.NE.1) GOTO 2100       
CV03           WRITE(6,901) IAM(),GNUM,(SCFLGN(K,GNUM),K=1,4)
CV032100    CONTINUE
CV03C
CV03        CALL PRMNUM('Enter Game number you wish to change for selected agents',
CV03     *               GNUM,1,MAXGAM,EXT)
CV03        IF(EXT.NE.0) GOTO 100
CV03C
CV03C DISABLE OR ENABLE
CV03C
CV03        CALL PRMNUM('Enter option [1=enable, 2=disable, E=exit]',
CV03     *              OPT,1,2,EXT)
CV03        IF(EXT.LT.0) GOTO 100
CV03        DO 3010 I=1,COUNT
CV03           TER=LIST(I)
CV03           IF(COUNT.EQ.NUMAGT) TER=I
CV03           IF(OPT.EQ.1) CALL BSET(PRMAFL(1,TER),GNUM)
CV03           IF(OPT.EQ.2) CALL BCLR(PRMAFL(1,TER),GNUM)
CV033010    CONTINUE
CV03        IF(COUNT.EQ.NUMAGT) THEN
CV03           WRITE(CHRCNT,904) ' all'
CV03        ELSE
CV03           WRITE(CHRCNT,905) COUNT
CV03        ENDIF
CV03        IF(OPT.EQ.1) WRITE(6,902) IAM(),PROMOTXT(PRMTYP),' enabled',
CV03     *                            (SCFLGN(K,GNUM),K=1,4),CHRCNT
CV03        IF(OPT.EQ.2) WRITE(6,902) IAM(),PROMOTXT(PRMTYP),' disabled',
CV03     *                            (SCFLGN(K,GNUM),K=1,4),CHRCNT
CV03        GOTO 100
C
C WRITE PROMO FILE
C
8000    CONTINUE
        CALL WRITEW(PRMFDB,1,PRMREC,ST)
        IF(ST.NE.0) THEN
           WRITE(6,903) IAM(),(SCFSFN(I,PRMO),I=1,5),ST,1
           CALL GPAUSE
        ENDIF
C
C CLOSE PROMO FILE
C
        CALL CLOSEFIL(PRMFDB)
        CALL GSTOP(GEXIT_SUCCESS)
C
C FORMAT SECTION
C
900     FORMAT(1X,A,1X,/,
     *         1X,A,1X,'1. Enable/disable a promo by GAME ',/,
     *         1X,A,1X,'2. Enable/disable a promo by AGENT ',/,
     *         1X,A,1X,'3. Exit ',/,
     *         1X,A)
901     FORMAT(1X,A,1X,I2,2X,4(A4))
902     FORMAT(1X,A,1X,A20,A,' for ',4(A4),' and ',A4,' agents')
904     FORMAT(A4)
905     FORMAT(I4)
903     FORMAT(1X,A,1X,5A4,' write error ',I4,' record ',I4)
        END
