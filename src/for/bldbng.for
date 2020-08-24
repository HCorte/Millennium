C
C SUBROUTINE BLDBNG
C  
C V10 12-JUN-2000 UXN Cleaned up.
C V09 08-MAR-2000 RXK Fixes for input/output.
C V08 13-JAN-2000 RXK Quadruples added to list of available divisions. Def-file
C                     for division names added.
C V07 16-JUL-1995 HXK Allow subgame value to be set to 0 (for Lucky Number)
C V06 03-FEB-1995 HXK Added time to "small" build
C V05 08-JAN-1995 HXK Allow price/prizes to be set seperately
C V04 11-NOV-1994 HXK Update PMT field, fixed diplay function
C V03 25-OCT-1994 HXK Updated for winning bitmaps
C V02 15-OCT-1994 GXA Re-wrote routine to use subgames better. Added winning 
C                   bitmap updating and related fields.
C V01 10-OCT-1994 HXK  Initial revision.
C  
C
C BLDBNG.FOR
C
C SUBROUTINE TO DEFINE BINGO GAME PARAMETERS.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE BLDBNG(FILE,GNAME)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DBNREC.DEF'
        INCLUDE 'INCLIB:BNGDNAM.DEF'
C
        INTEGER*4 FDB(7),DPER(BGODIV,BGOSUB)
        INTEGER*4 WINTAB(BGOMAXMAP+2+(BGOCOL*BGOROW),BGOSUB)
        INTEGER*4 YN(2),FILE(5)
        INTEGER*4 GNAME(4),SHRVAL(BGODIV,BGOSUB)
        INTEGER*4 WINOTH(BGODIV,BGOSUB)
        INTEGER*4 WINFST(BGODIV,BGOSUB)
        INTEGER*4 WINDNR(BGODIV)
C
        INTEGER*4 ST, DRAW, IND, TOTPER, I, SPER, SUBGM(BGOSUB), SUBPH
        INTEGER*4 TIME, PRICE, MULTI, EXT, FLAG
        INTEGER*4 OPT, ANS, I4_MDS, PREVIOUS_I4_MDS, K, X, J, Y, L, S
        INTEGER*4 DIVS2
        INTEGER*4 MAPNUM, BINGOTYPE
        INTEGER*4 TOTAL_MDS_CHOSEN
        INTEGER*4 MIND
        INTEGER*4 REV1                      ! Control Rev Sequence #
        INTEGER*4 REV2                      ! Control Rev (checksum)
        INTEGER*4 WINMAPS(BGOMAXMAP,BGOSUB) ! Winning bitmaps
        INTEGER*4 OVERLAP(BGOMAXMAP,BGOSUB) ! Overlap and Win Flags
C        INTEGER*4 SORT(5,BGOMAXMAP+2+(BGOCOL*BGOROW)) !Sort Array
        INTEGER*4 CUTOFF_PMT(3,BGOPHS)
        INTEGER*4 TOTDIV
        INTEGER*4 BNGOFF, NUMBNG, FIGOFF, NUMFIG, MINMAP, MAXMAP, DIV
        INTEGER*4 BNGDIV/1/        !divisions with bingo bitmaps are 1,...,8
        INTEGER*4 FIGDIV/9/        !divisions with figure bitmaps are 9,10
        INTEGER*4 NUMDIV/11/       !divisions with numbers to match are 11,..,15
        BYTE BYTE_MDS(MAXMLTD_AVL)     
C
        CHARACTER*65 STRING1
        CHARACTER*35 STRING3
        CHARACTER*63 STRING4
        CHARACTER*52 DISPLAY(200)
        CHARACTER*2  MATTAB(BGOCOL)
        CHARACTER*12 SUBNAME(3)
        CHARACTER*2  SSUBNAME(3)
        CHARACTER*1  CONT

        LOGICAL SETPRZ
        LOGICAL SETPAT
        LOGICAL SETALL
        LOGICAL SELECTION

        DATA YN/'No  ','Yes '/
        DATA SUBNAME/'Bingo AB    ','Full House  ','Lucky Number'/
        DATA SSUBNAME/'AB','FH','LN'/
C
        CHARACTER*8 NEWNAMES(BGOLOT)            
        INTEGER*4   INEWNAMES(2*BGOLOT)
        EQUIVALENCE (NEWNAMES,INEWNAMES)
        

        INTEGER*4 PRENUM                    ! Number of predefined bitmaps
        PARAMETER(PRENUM=16)
        INTEGER*4 PRE_WINMAPS(PRENUM)       ! Predefined Winning bitmaps 
        CHARACTER*8 WINMAP_NAMES(PRENUM)    ! Predefined Winning bitmap names 
C                                                
        DATA PRE_WINMAPS(1) /'0000003E'X/ ! Row 1    
        DATA PRE_WINMAPS(2) /'000007C0'X/ ! Row 2    
        DATA PRE_WINMAPS(3) /'0000F800'X/ ! Row 3    
        DATA PRE_WINMAPS(4) /'001F0000'X/ ! Row 4    
        DATA PRE_WINMAPS(5) /'03E00000'X/ ! Row 5    
        DATA PRE_WINMAPS(6) /'000E7380'X/ ! Block
        DATA PRE_WINMAPS(7) /'03F18C7E'X/ ! Border
        DATA PRE_WINMAPS(8) /'022A22A2'X/ ! Cross
        DATA PRE_WINMAPS(9) /'00210842'X/ ! Col 1
        DATA PRE_WINMAPS(10)/'00421084'X/ ! Col 2
        DATA PRE_WINMAPS(11)/'00842108'X/ ! Col 3
        DATA PRE_WINMAPS(12)/'01084210'X/ ! Col 4
        DATA PRE_WINMAPS(13)/'02108420'X/ ! Col 5
        DATA PRE_WINMAPS(14)/'02082082'X/ ! Diag 1
        DATA PRE_WINMAPS(15)/'00222220'X/ ! Diag 2
        DATA PRE_WINMAPS(16)/'02200022'X/ ! Corners
        DATA WINMAP_NAMES/'Row 1   ','Row 2   ','Row 3   ','Row 4   ',  
     *                    'Row 5   ','Block   ','Border  ','Cross   ',  
     *                    'Col 1   ','Col 2   ','Col 3   ','Col 4   ',  
     *                    'Col 5   ','Diag 1  ','Diag 2  ','Corners '/
C
C
5       CONTINUE
        CALL CLRSCR(6)
        CALL FASTMOV(IBNGDNAMES,INEWNAMES,2*BGOLOT)
        TYPE*,' '
        WRITE (STRING4,800) GNAME
        CALL PRMNUM(STRING4,OPT,1,2,EXT)
        IF(EXT.NE.0) GOTO 1000
        IF(OPT.EQ.2) GOTO 2000
C
C
10      CONTINUE
        CALL CLRSCR(6)
        CALL FASTSET(0,DPER,BGODIV*BGOSUB)
        CALL FASTSET(0,WINTAB,(BGOMAXMAP+2+(BGOCOL*BGOROW))*BGOSUB)
        CALL FASTSET(0,WINMAPS,BGOSUB*BGOMAXMAP)
        CALL FASTSET(0,OVERLAP,BGOSUB*BGOMAXMAP)
        CALL FASTSET(0,SHRVAL,BGODIV*BGOSUB)
        CALL FASTSET(0,SUBGM,BGOSUB)
        CALL FASTSET(0,CUTOFF_PMT,3*BGOPHS)
        CALL FASTSET(0,WINOTH,BGODIV*BGOSUB)
        CALL FASTSET(0,WINFST,BGODIV*BGOSUB)
        CALL FASTSET(0,WINDNR,BGODIV)
        SUBPH=0
        SETPRZ = .FALSE.
        SETPAT = .FALSE.
        SETALL = .TRUE.
        WRITE(6,917) GNAME
C
C
        CALL WIMG(6,'Do you want to set Winning Patterns only [Y/N] ')
        CALL YESNO(FLAG)
        IF(FLAG.EQ.1) THEN 
           SETPAT = .TRUE.
           SETALL = .FALSE.
        ENDIF
        IF(SETALL) THEN
           CALL WIMG(6,'Do you want to set Price/Prizes/Time only [Y/N] ')
           CALL YESNO(FLAG)
           IF(FLAG.EQ.1) THEN 
              SETPRZ = .TRUE.
              SETALL = .FALSE.
           ENDIF
        ENDIF
C
        IF(SETPAT.OR.SETPRZ) THEN 
20         CONTINUE
           CALL PRMNUM(' Enter first draw to change: ',DRAW,1,99999,EXT)
           IF(EXT.NE.0) GOTO 1000
           CALL OPENW(2,FILE,4,0,0,ST)
           CALL IOINIT(FDB,2,DBNSEC*256)
           IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)
           CALL READW(FDB,DRAW,DBNREC,ST)
           IF(ST.NE.0) CALL FILERR(FILE,2,ST,DRAW)
           CALL CLOSEFIL(FDB)
           CALL CLRSCR(6)

           IF(DBNSTS.GT.GAMOPN) THEN
              TYPE*,'Game already closed for draw ',DRAW
              CALL GPAUSE
              GOTO 20
           ENDIF
C
           CALL FASTMOV(DBNDIV,SUBGM,BGOSUB)
           SUBPH=DBNNSP
           TOTDIV = 0
           DO I=1,BGOSUB
              TOTDIV = TOTDIV + SUBGM(I)
           ENDDO
           IF(TOTDIV.LE.0) THEN 
              TYPE*,'THIS DRAW IS NOT PREVIOUSLY SET !'
              CALL XWAIT(2,2,ST)
              GOTO 5
           ENDIF

           MULTI=DBNMLT
           DO I=1,MAXMLTD_AVL
              BYTE_MDS(I)=DBNMDS(I)
           ENDDO

           IF(SETPAT) THEN
              SPER=DBNSPR
              TIME=DBNTIM
              PRICE=DBNPRC
           ENDIF

        ENDIF
    
        IF(SETPRZ) THEN
           CALL FASTMOV(DBNMAP(1,1),WINMAPS(1,1),BGOMAXMAP*BGOSUB)
           CALL FASTMOV(DBNMPF(1,1),OVERLAP(1,1),BGOMAXMAP*BGOSUB)
           CALL FASTMOV(DBNMAT(1,1),WINTAB(1,1),
     *                 (BGOMAXMAP+2+BGOCOL*BGOROW)*BGOSUB)
           CALL FASTMOV(DBNPMT(1,1),CUTOFF_PMT(1,1),3*BGOPHS)
           CALL FASTMOV(DBNFST(1,1),WINFST(1,1),BGODIV*BGOSUB)
           CALL FASTMOV(DBNOTH(1,1),WINOTH(1,1),BGODIV*BGOSUB)
           CALL FASTMOV(DBNDNR(1),WINDNR(1),BGODIV)
           SUBPH=DBNNSP
        ENDIF

        IF(SETPRZ.OR.SETALL) THEN
           CALL PRMMONY('Enter ticket price/bet ',PRICE,BETUNIT,EXT)
           IF(EXT.LT.0) GOTO 1000

           CALL PRMPER('Enter pool percentage of sales (0=fixed payout)',
     *                  SPER,EXT)
           IF(EXT.LT.0) GOTO 1000

           CALL PRMTIM('Enter pool close time HH:MM:SS ',TIME,EXT)
           IF(EXT.LT.0) GOTO 1000
        ENDIF
C
C POOL PERCENTAGES OR FIXED AMOUNTS
C
50      CONTINUE
        IF(SETPRZ) THEN 
           DO J = 1,BGOSUB
             IF(SUBGM(J).GT.0) THEN
              WRITE(6,903) IAM(),SUBNAME(J)
              DO DIV = 1, NUMDIV-2,2
                 IF(WINFST(DIV,J).NE.0) THEN
                    WRITE (STRING1,940) DIV,NEWNAMES(WINDNR(DIV))
                    CALL PRMPER(STRING1,DPER(DIV,J),EXT)
                    IF(EXT.LT.0) GOTO 1000
                    IF(DPER(DIV,J).EQ.0) THEN
                       WRITE(STRING3,904) DIV
                       CALL PRMMONY(STRING3,SHRVAL(DIV,J),VALUNIT,EXT)
                       IF(EXT.LT.0) GOTO 1000
                    ENDIF
                 ENDIF 
                 IF(WINOTH(DIV,J).NE.0) THEN
                    WRITE (STRING1,940) DIV+1,NEWNAMES(WINDNR(DIV+1))
                    CALL PRMPER(STRING1,DPER(DIV+1,J),EXT)
                    IF(EXT.LT.0) GOTO 1000
                    IF(DPER(DIV+1,J).EQ.0) THEN
                       WRITE(STRING3,904) DIV+1
                       CALL PRMMONY(STRING3,SHRVAL(DIV+1,J),VALUNIT,EXT)
                       IF(EXT.LT.0) GOTO 1000
                    ENDIF
                 ENDIF 
              ENDDO
              DO I = BGOMAXMAP+1,BGOMAXMAP+2+BGOROW*BGOCOL 
                 DIV = WINTAB(I,J)
                 IF(DIV.GT.0) THEN
                    WRITE (STRING1,940) DIV,NEWNAMES(WINDNR(DIV))
                    CALL PRMPER(STRING1,DPER(DIV,J),EXT)
                    IF(EXT.LT.0) GOTO 1000
                    IF(DPER(DIV,J).EQ.0) THEN
                       WRITE(STRING3,904) DIV
                       CALL PRMMONY(STRING3,SHRVAL(DIV,J),VALUNIT,EXT)
                       IF(EXT.LT.0) GOTO 1000
                    ENDIF
                 ENDIF
              ENDDO       
             ENDIF
           ENDDO
         
C
C CHECK POOL PERCENTAGES FOR ACCURACY
C
           DO J = 1,BGOSUB
              IF(SUBGM(J).GT.0) THEN
                 TOTPER=0
                 DO I = 1,SUBGM(J)
                    IF(DPER(I,J).GT.0) TOTPER=TOTPER+DPER(I,J)
                 ENDDO
                 IF(TOTPER.GT.0.AND.SPER.EQ.0) THEN
                    CALL PRMPER(
     *                 'Pool percentage of sales not set, enter it',
     *                 SPER,EXT)
                    IF(EXT.LT.0) GOTO 1000
                 ENDIF
                 IF(TOTPER.GT.100000) THEN
                    WRITE(6,903) IAM(),SUBNAME(J)
                    WRITE(6,901) DISPER(TOTPER)
                    GOTO 50
                 ENDIF
                 IF(TOTPER.LT.100000) THEN
                    WRITE(6,903) IAM(),SUBNAME(J)
                    WRITE(6,902) DISPER(TOTPER)
                    CALL WIMG(6,'Is this correct [Y/N] ')
                    CALL YESNO(FLAG)
                    IF(FLAG.NE.1) GOTO 50
                 ENDIF
              ENDIF
           ENDDO
        ENDIF
C
C SET MULTIDRWAS
C
100      CONTINUE
      IF(SETALL) THEN 
        MULTI=1
        PREVIOUS_I4_MDS=0
        DO I=1,MAXMLTD_AVL
           BYTE_MDS(I)=0
        ENDDO
        BYTE_MDS(1)=1          !single week wagering always allowed
        CALL WIMG(6,'Is there multi-draw wagering [Y/N] ')
        CALL YESNO(FLAG)
        IF(FLAG.EQ.1) THEN
          CALL PRMNUM('Enter maximum number of draws ',MULTI,1,15,EXT)
          IF(EXT.LT.0) GOTO 100
          TYPE*,'Enter multi-draws allowed '
          TYPE*,'(e.g. Lotto =2,3,5 and 10)'
          I=2
150        CONTINUE
          CALL PRMNUM('Enter multi-draw allowed [E=finished] ',
     *                I4_MDS,2,MAXMLTD_AVL,EXT)
          IF(EXT.EQ.0) THEN
            IF(I.GT.MAXMLTD_SEL-1) THEN   !prevent more than max allowed
              TYPE*,'Too many multi-draws selected.'
              TYPE*,'total cannot exceed ',MAXMLTD_SEL-1  !1 drw already chosen
              CALL XWAIT(2,2,ST)
              GOTO 100
            ENDIF
            IF(BYTE_MDS(I4_MDS).NE.0) THEN  !prevent writing over values
              TYPE*,'Draw multiple already chosen '
              TYPE*,'Re-enter data '
              CALL XWAIT(2,2,ST)
              GOTO 100
            ENDIF
            IF(I4_MDS.LE.PREVIOUS_I4_MDS) THEN
              TYPE*,'Draw multiples MUST be entered in '
              TYPE*,'ascending order'
              CALL XWAIT(2,2,ST)
              GOTO 100
            ENDIF
            BYTE_MDS(I4_MDS)=I
            PREVIOUS_I4_MDS=I4_MDS
            I=I+1
            GOTO 150
          ENDIF
          IF(EXT.EQ.-1) THEN
            TOTAL_MDS_CHOSEN=I-1
          ELSE
            TYPE*,'Incorrect data entered '
            GOTO 150
          ENDIF
        ENDIF
      ENDIF
C
C 
C SET WINNING BITMAPS AND CORRESPOND PRIZES
C
      IF(SETPAT.OR.SETALL) THEN
         DO I = 1,BGOSUB
            WRITE(STRING1,927) SUBNAME(I)
            CALL PRMNUM(STRING1,SUBGM(I),0,BGODIV,EXT)
            IF(EXT.LT.0) GOTO 1000
         ENDDO
         WRITE(STRING1,929) SUBNAME(BGOFHS)
         CALL PRMNUM(STRING1,SUBPH,0,BGOSPH,EXT)
         IF(EXT.LT.0) GOTO 1000
      ENDIF
C
C ASK FOR WINNING BITMAPS TO BE CREATED WITH PREDEFINED BITMAPS
C
      IF(SETPAT.OR.SETALL) THEN
200      CONTINUE

         DO 400 S = 1,BGOSUB-1           !DON'T DO LUCKY NUMBER

           CALL FASTSET(0,DPER(1,S),BGODIV*BGOSUB)
           CALL FASTSET(0,WINTAB(1,S),BGOMAXMAP+2+BGOCOL*BGOROW)
           CALL FASTSET(0,WINMAPS(1,S),BGOMAXMAP)
           CALL FASTSET(0,SHRVAL(1,S),BGODIV)
           CALL FASTSET(0,WINOTH(1,S),BGODIV)
           CALL FASTSET(0,WINFST(1,S),BGODIV)
           CALL FASTSET(0,WINDNR,BGODIV)

           IF(SUBGM(S).NE.0) THEN 
              MAPNUM = 1
              DO J=1,BGOMAXMAP
                 WINMAPS(J,S)=0
              ENDDO

              BNGOFF = 0
              NUMBNG = 0
              FIGOFF = 0
              NUMFIG = 0
   
              WRITE(6,905)
              WRITE(6,937) SUBNAME(S)
              CALL PRMNUM('Enter option ',BINGOTYPE,1,3,EXT)
              IF(EXT.LT.0) GOTO 400
 
              DO I=1,5
                 WINMAPS(I,S) = PRE_WINMAPS(I)         ! horizontal bingo
              ENDDO 
              BNGOFF=1 
              NUMBNG=5 

              IF(BINGOTYPE.GE.2) THEN
                 K=1                                  
                 DO I=9,13
                    WINMAPS(5+K,S) = PRE_WINMAPS(I)    ! vertical bingo  
                    K=K+1
                 ENDDO 
                 NUMBNG=10 
              ENDIF

              IF(BINGOTYPE.EQ.3) THEN
                 WINMAPS(11,S) = PRE_WINMAPS(14)       ! diagonal bingo
                 WINMAPS(12,S) = PRE_WINMAPS(15)       ! diagonal bingo
                 NUMBNG=12 
              ENDIF

              WRITE(6,905)
              TYPE*,IAM(),'Bitmaps selected :'
              CALL DSPMAP (BNGOFF,BNGOFF+NUMBNG-1,WINMAPS,S)
C
C ASK WHOLE INFORMATION FOR BINGO MATCH DIVISIONS(BINGO,DOUBLE,TRIPLE,QUADRUPLE)
C
              DO DIV=1,8
                WRITE(6,905)
                IF(MOD(DIV,2).NE.0) THEN
                  WRITE(STRING1,'(A35,A8,1X,A7)') 
     *            'Do you want set division for FIRST ',BNGDNAMES(DIV),'[Y/N] ?'
                ELSE
                  WRITE(STRING1,'(A35,A8,1X,A7)') 
     *            'Do you want set division for OTHER ',BNGDNAMES(DIV),'[Y/N] ?'
                ENDIF
                CALL WIMG(6,STRING1)
                CALL YESNO(FLAG)
                IF(FLAG.EQ.1) THEN
                   WINDNR(DIV) = DIV
                   WINFST(DIV,S) = MOD(DIV,2)
                   WINOTH(DIV+2*MOD(DIV,2)-1,S) = DIV
                   IF(DIV.EQ.1.OR.(DIV.EQ.2.AND.WINFST(1,S).EQ.0)) THEN
                     DO I = BNGOFF,BNGOFF+NUMBNG-1
                       WINTAB(I,S) = DIV
                     ENDDO
                   ENDIF
                   WRITE (STRING1,941) BNGDNAMES(WINDNR(DIV))
                   CALL PRMPER(STRING1,DPER(DIV,S),EXT)
                   IF(EXT.LT.0) GOTO 1000
                   IF(DPER(DIV,S).EQ.0) THEN
                    WRITE(STRING3,904) DIV
                    CALL PRMMONY(STRING3,SHRVAL(DIV,S),VALUNIT,EXT)
                    IF(EXT.LT.0) GOTO 1000
                   ENDIF
                ENDIF
            ENDDO
C
            WRITE(6,905)
            TYPE*,'Continuation of the selection of the bitmaps'
            WRITE(6,905)
            DO MAPNUM=1,(PRENUM+1)/2
               WRITE(6,935) MAPNUM,WINMAP_NAMES(MAPNUM),
     *             MAPNUM+(PRENUM+1)/2,WINMAP_NAMES(MAPNUM+(PRENUM+1)/2)
            ENDDO
C
C ASK WHOLE INFORMATION FOR FIGURE MATCH DIVISIONS (BLOCK,BORDER AND CROSS)
C
            FIGOFF=BNGOFF+NUMBNG 
            NUMFIG=0

250         CONTINUE
            SELECTION = .FALSE.
            CALL PRMNUM('Enter map number (E-end)',
     *                   MAPNUM,1,PRENUM,EXT) 
            IF(EXT.LT.0) GOTO 300
            IF(MAPNUM.NE.6.AND.MAPNUM.NE.7.AND.MAPNUM.NE.8) THEN
               TYPE*,'You can select Border, Block or Cross'
               GOTO 250
            ENDIF

            WINMAPS(FIGOFF+NUMFIG,S) = PRE_WINMAPS(MAPNUM) 
C
            DO I=2*(MAPNUM-2)-1,2*(MAPNUM-2) 
              IF(MOD(I,2).NE.0) THEN
                WRITE(STRING1,'(A35,A8,1X,A7)') 
     *            'Do you want set division for FIRST ',BNGDNAMES(I),'[Y/N] ?'
              ELSE
                  WRITE(STRING1,'(A35,A8,1X,A7)') 
     *            'Do you want set division for OTHER ',BNGDNAMES(I),'[Y/N] ?'
              ENDIF
               CALL WIMG(6,STRING1)
               CALL YESNO(FLAG)
               IF(FLAG.EQ.1) THEN
                  DIV = FIGDIV+2*NUMFIG+MOD(I+1,2)
                  WINDNR(DIV)=I
                  IF(MOD(I,2).EQ.0) THEN
                    IF(WINFST(DIV-1,S).EQ.0) WINTAB(NUMBNG+NUMFIG+1,S) = DIV
                  ELSE
                    WINTAB(NUMBNG+NUMFIG+1,S) = DIV
                  ENDIF
                  WINFST(DIV,S) = MOD(I,2)
                  WINOTH(DIV+2*MOD(I,2)-1,S) = DIV
                  WRITE (STRING1,941) BNGDNAMES(WINDNR(DIV))
                  CALL PRMPER(STRING1,DPER(DIV,S),EXT)
                  IF(EXT.LT.0) GOTO 1000
                  IF(DPER(DIV,S).EQ.0) THEN
                     WRITE(STRING3,904) DIV
                     CALL PRMMONY(STRING3,SHRVAL(DIV,S),VALUNIT,EXT)
                     IF(EXT.LT.0) GOTO 1000
                  ENDIF
               ENDIF
               IF(FLAG.EQ.1) SELECTION = .TRUE.
              ENDDO
C
             IF(SELECTION) NUMFIG = NUMFIG + 1
             IF(NUMFIG.LT.2) GOTO 250   
C
C DISPLAY VALUES SET UP UNTIL THIS POINT
C
300         CONTINUE
            IF(NUMFIG.GT.0) THEN
               WRITE(6,905)
               TYPE*,'Bitmaps selected :'
               WRITE(6,905)
               CALL DSPMAP (BNGOFF,NUMBNG+NUMFIG,WINMAPS,S)
            ENDIF
C
C LIST SELECTED BITMAPS
C
            WRITE(6,934)
            DO MAPNUM=1,BGOMAXMAP
               IF(WINMAPS(MAPNUM,S).NE.0) THEN
                  DO I=1,PRENUM
                     IF(PRE_WINMAPS(I).EQ.WINMAPS(MAPNUM,S)) 
     *               WRITE(6,933) WINMAP_NAMES(I),SUBNAME(S)
                  ENDDO
               ENDIF
            ENDDO
            CALL WIMG(6,'Is this correct [Y/N] ')
            CALL YESNO(FLAG)
            IF(FLAG.NE.1) GOTO 200
C
C SET NUMBERS TO MATCH DIVISION TABLES
C
            DIVS2 = 0
            DO I = BGOCOL*BGOROW, 20, -1                !Divs for #'s Matched
               WRITE(STRING1,936) I
               CALL WIMG(6,STRING1)
               CALL YESNO(FLAG)
               IF(FLAG.EQ.1) THEN
                  DIV = NUMDIV + DIVS2
                  WINTAB(BGOMAXMAP+2+I,S) = DIV
                  WINDNR(DIV)=13+BGOCOL*BGOROW-I
                  WRITE (STRING1,941) BNGDNAMES(WINDNR(DIV))
                  CALL PRMPER(STRING1,DPER(DIV,S),EXT)
                  IF(EXT.LT.0) GOTO 1000
                  IF(DPER(DIV,S).EQ.0) THEN
                     WRITE(STRING3,904) DIV
                     CALL PRMMONY(STRING3,SHRVAL(DIV,S),VALUNIT,EXT)
                     IF(EXT.LT.0) GOTO 1000
                  ENDIF
                  DIVS2 = DIVS2+1
                  IF(DIVS2.GE.5) GOTO 400
               ENDIF
            ENDDO

            CALL WIMG(6,
     *         'Do you want set division for worst match [Y/N] ?')
            CALL YESNO(FLAG)
            IF(FLAG.EQ.1) THEN
               DIV = NUMDIV + DIVS2
               WINDNR(DIV) = 19
               WINTAB(BGOMAXMAP+1,S) = DIV
               WRITE (STRING1,941) BNGDNAMES(WINDNR(DIV))
               CALL PRMPER(STRING1,DPER(DIV,S),EXT)
               IF(EXT.LT.0) GOTO 1000
               IF(DPER(DIV,S).EQ.0) THEN
                  WRITE(STRING3,904) DIV
                  CALL PRMMONY(STRING3,SHRVAL(DIV,S),VALUNIT,EXT)
                  IF(EXT.LT.0) GOTO 1000
               ENDIF
               DIVS2 = DIVS2+1
               IF(DIVS2.GE.5) GOTO 400
            ENDIF

            CALL WIMG(6,
     *         'Do you want set division for second worst match [Y/N] ?')
            CALL YESNO(FLAG)
            IF(FLAG.EQ.1) THEN
               DIV = NUMDIV + DIVS2
               WINDNR(DIV) = 20
               WINTAB(BGOMAXMAP+2,S) = DIV
               WRITE (STRING1,941) BNGDNAMES(WINDNR(DIV))
               CALL PRMPER(STRING1,DPER(DIV,S),EXT)
               IF(EXT.LT.0) GOTO 1000
               IF(DPER(DIV,S).EQ.0) THEN
                  WRITE(STRING3,904) DIV
                  CALL PRMMONY(STRING3,SHRVAL(DIV,S),VALUNIT,EXT)
                  IF(EXT.LT.0) GOTO 1000
               ENDIF
               DIVS2 = DIVS2+1
            ENDIF


          ENDIF
400     CONTINUE        
C
C CHECK POOL PERCENTAGES FOR ACCURACY
C
        DO J = 1,BGOSUB
            IF(SUBGM(J).GT.0) THEN
                TOTPER=0
                DO I = 1,SUBGM(J) 
                    IF(DPER(I,J).GT.0) TOTPER=TOTPER+DPER(I,J)
                ENDDO
C
                IF(TOTPER.GT.100000) THEN
                    WRITE(6,903) IAM(),SUBNAME(J)
                    WRITE(6,901) DISPER(TOTPER)
                    GOTO 200
                ENDIF
C
                IF(TOTPER.GT.0.AND.SPER.EQ.0) THEN
                   CALL PRMPER(
     *                'Pool percentage of sales not set, enter it',
     *                 SPER,EXT)
                   IF(EXT.LT.0) GOTO 1000
                ENDIF

                IF(TOTPER.LT.100000) THEN
                    WRITE(6,903) IAM(),SUBNAME(J)
                    WRITE(6,902) DISPER(TOTPER)
                    CALL WIMG(6,'Is this correct [Y/N] ')
                    CALL YESNO(FLAG)
                    IF(FLAG.NE.1) GOTO 200
                ENDIF
            ENDIF
        ENDDO

C
C SET CUTOFFS
C
        IF(SUBGM(BGOFHS).GT.0) THEN      !Full House only

           CUTOFF_PMT(1,1) = NUMBNG+NUMFIG
           CUTOFF_PMT(2,1) = 0
           CUTOFF_PMT(3,1) = NUMFIG
           CUTOFF_PMT(1,2) = 0
           CUTOFF_PMT(2,2) = DIVS2
           CUTOFF_PMT(3,2) = 0

        ENDIF

      ENDIF  
C
C MAPS AND VALUES ARE SET. DISPLAY CURRENT SETTING.
C
        IND=1
        MIND=1
        IF(MULTI.GT.1) MIND=2

        DO I=1,100
           WRITE (DISPLAY(I),905)
        ENDDO

        DO J = 1,BGOSUB
           IF(SUBGM(J).GT.0) THEN

              DO DIV=BNGDIV,NUMDIV-2,2
                 IF(WINFST(DIV,J).NE.0) THEN 
                    IF(DPER(DIV,J).EQ.0) THEN
                       WRITE(DISPLAY(IND),938) SSUBNAME(J),DIV,
     *                      BNGDNAMES(WINDNR(DIV)),
     *                      CMONY(SHRVAL(DIV,J),12,VALUNIT)
                       IND = IND +1
                    ELSE
                       WRITE(DISPLAY(IND),939) SSUBNAME(J),DIV,
     *                      BNGDNAMES(WINDNR(DIV)),
     *                      DISPER(DPER(DIV,J))
                       IND = IND +1
                    ENDIF
                 ENDIF
                 IF(WINOTH(DIV,J).NE.0) THEN
                    IF(DPER(DIV+1,J).EQ.0) THEN
                       WRITE(DISPLAY(IND),938) SSUBNAME(J),DIV+1,
     *                      BNGDNAMES(WINDNR(DIV+1)),
     *                      CMONY(SHRVAL(DIV+1,J),12,VALUNIT)
                       IND = IND +1
                    ELSE
                       WRITE(DISPLAY(IND),939) SSUBNAME(J),DIV+1,
     *                      BNGDNAMES(WINDNR(DIV+1)),
     *                      DISPER(DPER(DIV+1,J))
                       IND = IND +1
                    ENDIF
                 ENDIF
              ENDDO

              DO I = BGOMAXMAP+1,BGOMAXMAP+2+BGOROW*BGOCOL 
                 DIV = WINTAB(I,J)
                 IF(DIV.GT.0.AND.DPER(DIV,J).EQ.0) THEN
                    WRITE(DISPLAY(IND),938) SSUBNAME(J),DIV,
     *                   BNGDNAMES(WINDNR(DIV)),
     *                   CMONY(SHRVAL(DIV,J),12,VALUNIT)
                    IND = IND +1
                 ELSEIF(DIV.GT.0) THEN
                     WRITE(DISPLAY(IND),939) SSUBNAME(J),DIV,
     *                   BNGDNAMES(WINDNR(DIV)),DISPER(DPER(DIV,J))
                     IND = IND +1
                 ENDIF
              ENDDO       

           ENDIF
        ENDDO
C
C
        WRITE(6,908) YN(MIND),DISPLAY(1)
        WRITE(6,909) CMONY(PRICE,6,BETUNIT),DISPLAY(2)
        WRITE(6,910) DISPER(SPER),DISPLAY(3)
        WRITE(6,911) (SUBNAME(J),SUBGM(J),DISPLAY(3+J),J = 1,BGOSUB)
        J = 3 + BGOSUB + 1
        WRITE(6,912) DISTIM(TIME),DISPLAY(J)
        WRITE(6,913) DISPLAY(J+1)
        J = J + 1
        DO I = 2,MAXMLTD_AVL
          IF(BYTE_MDS(I).NE.0) THEN
            J=J+1
            WRITE(6,919) I,DISPLAY(J)
          ENDIF
        ENDDO 
        J = J + 1
        WRITE(6,920) TOTAL_MDS_CHOSEN,DISPLAY(J)
        I=1   
        J = J + 1
        WRITE(6,921) 'BN',CUTOFF_PMT(1,I),CUTOFF_PMT(2,I),
     *               CUTOFF_PMT(3,I),
     *               DISPLAY(J)
        I=2   
        J = J + 1
        WRITE(6,921) 'FH',CUTOFF_PMT(1,I),CUTOFF_PMT(2,I),
     *               CUTOFF_PMT(3,I),
     *               DISPLAY(J)
        J = J + 1
        WRITE(6,922) SUBPH, 
     *               DISPLAY(J)
C
        DO I = J+1,IND-1
           IF(MOD(I,20).EQ.0) THEN
              CALL WIMG(6,'Hit < Return > to continue ')
              READ(5,928) CONT
           ENDIF
           WRITE(6,914) DISPLAY(I)
        ENDDO
C
        TYPE*,'    '
        CALL WIMG(6,'Are these values correct [Y/N] ')
        CALL YESNO(FLAG)
        IF(FLAG.NE.1) GOTO 10
C
C SORT OVERLAP ARRAY AND MOVE OTHER RELATED ARRAYS (FOR WINSEL AND SCAN)
C STORE IN REVERSED SORTED ORDER DUE TO OVERLAP FLAGS VALUES ARE 1 IF OVERLAP 
C IS NOT ALOWED AND WE WANT THESE TO BE LAST IN THE LIST.
C
       GOTO 500    !skip sort
C501    CONTINUE 
C        DO J = 1,BGOSUB
C            DO I = 1,BGOMAXMAP
C                SORT(1,I) = OVERLAP(I,J)
C                SORT(2,I) = WINMAPS(I,J)
C                SORT(3,I) = 0 
C                SORT(4,I) = 0
C                SORT(5,I) = I
C            ENDDO
C            CALL ISORT5(SORT,I-1,1)
C            IND = 0
C            DO I = BGOMAXMAP,1,-1
C                IND = IND + 1
C                OVERLAP(IND,J) = SORT(1,I)
C                WINMAPS(IND,J) = SORT(2,I)
C            ENDDO
C        ENDDO
C
500     CONTINUE
        CALL CLRSCR(6)
        IF(SETALL) THEN
           CALL PRMNUM(
     *     'Enter first draw for this game description ',
     *      DRAW,1,10000,EXT)
           IF(EXT.LT.0) GOTO 1000
        ENDIF
C
C UPDATE BINGO GAME FILE
C
        WRITE(6,916) FILE
        CALL OPENW(2,FILE,4,0,0,ST)
        CALL IOINIT(FDB,2,DBNSEC*256)
        IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)
C
550     CONTINUE
        CALL READW(FDB,DRAW,DBNREC,ST)
C
        IF(ST.EQ.144) THEN
          TYPE*,'Last draw initialized - ',DRAW-1
          CALL CLOSEFIL(FDB)
          CALL XWAIT(2,2,ST)
          RETURN
        ENDIF
        IF(ST.NE.0) CALL FILERR(FILE,2,ST,DRAW)
C
        IF(DBNSTS.GT.GAMOPN) THEN
          TYPE*,'Game already closed for draw ',DRAW
          CALL GPAUSE
          CALL CLOSEFIL(FDB)
          GOTO 500
        ENDIF
C
        DBNPRC=PRICE
        DBNSPR=SPER
        DBNTIM=TIME
        DBNMLT=MULTI
        DBNNSP=SUBPH
        DO I=1,MAXMLTD_AVL
           DBNMDS(I)=BYTE_MDS(I)
        ENDDO

        CALL FASTMOV(SUBGM,DBNDIV,BGOSUB)
        CALL FASTMOV(WINMAPS(1,1),DBNMAP(1,1),BGOMAXMAP*BGOSUB)
        CALL FASTMOV(OVERLAP(1,1),DBNMPF(1,1),BGOMAXMAP*BGOSUB)
        CALL FASTMOV(WINTAB(1,1),DBNMAT(1,1),
     *               (BGOMAXMAP+2+BGOCOL*BGOROW)*BGOSUB)
        CALL FASTMOV(SHRVAL(1,1),DBNSHV(1,1),BGOSUB*BGODIV)
        CALL FASTMOV(DPER(1,1),DBNPER(1,1),BGOSUB*BGODIV)
        CALL FASTMOV(CUTOFF_PMT(1,1),DBNPMT(1,1),3*BGOPHS)
        CALL FASTMOV(WINFST(1,1),DBNFST(1,1),BGODIV*BGOSUB)
        CALL FASTMOV(WINOTH(1,1),DBNOTH(1,1),BGODIV*BGOSUB)
        CALL FASTMOV(WINDNR(1),DBNDNR(1),BGODIV)
C
C UPDATE CONTROL REVISION NUMBERS
C
        CALL ILBYTE(REV1,DBNREV,0)
        CALL ILBYTE(REV2,DBNREV,1)
        IF(REV1.NE.0.AND.REV2.NE.0) THEN
           REV1 = REV1 + 1
        ELSE
           REV1 = 1
           REV2 = MOD(DRAW,255)
        ENDIF
C
        CALL ISBYTE(REV1,DBNREV,0)
        CALL ISBYTE(REV2,DBNREV,1)
C
C
        CALL WRITEW(FDB,DRAW,DBNREC,ST)
        IF(ST.NE.0) CALL FILERR(FILE,3,ST,DRAW)
        DRAW=DRAW+1
        GOTO 550
C
C
1000    CONTINUE
        CALL CLRSCR(6)
        RETURN
C
C
C OPTION 2 - DISPLAY SETTINGS FOR PARTICULAR DRAW FROM FILE
C
2000    CONTINUE
        CALL PRMNUM(' Enter draw to display: ',DRAW,1,99999,EXT)
        IF(EXT.NE.0) GOTO 1000
C
        CALL OPENW(2,FILE,4,0,0,ST)
        CALL IOINIT(FDB,2,DBNSEC*256)
        IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)
C
        CALL READW(FDB,DRAW,DBNREC,ST)
        IF(ST.NE.0) CALL FILERR(FILE,2,ST,DRAW)
        CALL CLOSEFIL(FDB)
C
        CALL CLRSCR(6)
        IND = 1
        MIND=1
        IF(DBNMLT.GT.1) MIND=2

        DO I=1,100
            WRITE(DISPLAY(I),905)
        ENDDO
C
C
        PRICE=DBNPRC
        MULTI=DBNMLT
        CALL FASTMOV(DBNDIV,SUBGM,BGOSUB)
        CALL FASTMOV(DBNMAP(1,1),WINMAPS(1,1),BGOMAXMAP*BGOSUB)
        CALL FASTMOV(DBNMPF(1,1),OVERLAP(1,1),BGOMAXMAP*BGOSUB)
        CALL FASTMOV(DBNMAT(1,1),WINTAB(1,1),
     *               (BGOMAXMAP+2+BGOCOL*BGOROW)*BGOSUB)
        CALL FASTMOV(DBNSHV(1,1),SHRVAL(1,1),BGOSUB*BGODIV)
        CALL FASTMOV(DBNPER(1,1),DPER(1,1),BGOSUB*BGODIV)
        CALL FASTMOV(DBNPMT(1,1),CUTOFF_PMT(1,1),3*BGOPHS)
        CALL FASTMOV(DBNFST(1,1),WINFST(1,1),BGODIV*BGOSUB)
        CALL FASTMOV(DBNOTH(1,1),WINOTH(1,1),BGODIV*BGOSUB)
        CALL FASTMOV(DBNDNR(1),WINDNR(1),BGODIV)
        SPER=DBNSPR
        SUBPH=DBNNSP 
        TIME=DBNTIM
        DO I=1,MAXMLTD_AVL
           BYTE_MDS(I)=DBNMDS(I)
        ENDDO
C
C LIST SET BITMAPS 
C
        DO J = 1,BGOSUB
         IF(SUBGM(J).GT.0) THEN
              DO I = 1, BGOMAXMAP                       !bitmaps to match     
                 DIV = WINTAB(I,J)
                 IF(DIV.GT.0) THEN
                    WRITE(DISPLAY(IND),906) I,SSUBNAME(J),DIV
                    IND = IND + 1
                 ENDIF
              ENDDO
              IND=IND+1
C
C DISPALY OF OLD BINGO AB AND FH IS HERE HARDCODED
C                                      
           IF(SUBGM(BGOBAB).NE.0) THEN
              K=1
              IF(J.EQ.BGOBAB) THEN
                 L=3
                 WINDNR(1)=6
                 WINDNR(2)=4
                 WINDNR(3)=2
              ENDIF
              IF(J.EQ.BGOFHS) THEN 
                 L=6
                 WINDNR(1)=13
                 WINDNR(2)=14
                 WINDNR(3)=6
                 WINDNR(4)=4
                 WINDNR(5)=2
                 WINDNR(6)=19
              ENDIF
           ELSE
              K=2
              L=10
           ENDIF            
C
C DISPLAY DIVISION INFO
C
           DO DIV = 1,L,K        !divisions for maps to match
              IF(WINFST(DIV,J).NE.0) THEN
                 IF(DPER(DIV,J).EQ.0) THEN
                    WRITE(DISPLAY(IND),938) SSUBNAME(J),DIV,
     *                   BNGDNAMES(WINDNR(DIV)),
     *                   CMONY(SHRVAL(DIV,J),12,VALUNIT)
                    IND=IND+1
                 ELSE
                    WRITE(DISPLAY(IND),939) SSUBNAME(J),DIV,
     *                   BNGDNAMES(WINDNR(DIV)),
     *                   DISPER(DPER(DIV,J))
                    IND=IND+1
                 ENDIF
              ENDIF 
              IF(WINOTH(DIV,J).NE.0) THEN
                 IF(DPER(DIV+K-1,J).EQ.0) THEN
                    WRITE(DISPLAY(IND),938) SSUBNAME(J),DIV+K-1,
     *                   BNGDNAMES(WINDNR(DIV+K-1)),
     *                   CMONY(SHRVAL(DIV+K-1,J),12,VALUNIT)
                    IND=IND+1
                 ELSE
                    WRITE(DISPLAY(IND),939) SSUBNAME(J),DIV+K-1,
     *                   BNGDNAMES(WINDNR(DIV+K-1)),
     *                   DISPER(DPER(DIV+K-1,J))
                    IND=IND+1
                 ENDIF
              ENDIF 

           ENDDO
           IND=IND+1

           IF(SUBGM(BGOBAB).EQ.0) THEN    !for old draws dispalyed above already
            DO I = BGOMAXMAP+1,BGOMAXMAP+2+BGOROW*BGOCOL
              DIV = WINTAB(I,J)             !numbers to match
              IF(DIV.GT.0.AND.DPER(DIV,J).EQ.0) THEN
                 WRITE(DISPLAY(IND),938) SSUBNAME(J),DIV,
     *                   BNGDNAMES(WINDNR(DIV)),
     *                   CMONY(SHRVAL(DIV,J),12,VALUNIT)
                 IND=IND+1
              ELSEIF(DIV.GT.0) THEN
                 WRITE(DISPLAY(IND),939) SSUBNAME(J),DIV,
     *                   BNGDNAMES(WINDNR(DIV)),DISPER(DPER(DIV,J))
                 IND=IND+1
              ENDIF
            ENDDO       
            IND=IND+1
           ENDIF
         ENDIF
        ENDDO
C
C
        WRITE(6,908) YN(MIND),DISPLAY(1)
        WRITE(6,909) CMONY(PRICE,6,BETUNIT),DISPLAY(2)
        WRITE(6,910) DISPER(SPER),DISPLAY(3)
        WRITE(6,911) (SUBNAME(J),SUBGM(J),DISPLAY(3+J),J = 1,BGOSUB)
        J = 3 + BGOSUB + 1
        WRITE(6,912) DISTIM(TIME),DISPLAY(J)
        WRITE(6,913) DISPLAY(J+1)
        J = J + 1
        DO I = 2,MAXMLTD_AVL
          IF(BYTE_MDS(I).NE.0) THEN
            J=J+1
            WRITE(6,919) I,DISPLAY(J)
          ENDIF
        ENDDO 
        J = J + 1
        WRITE(6,920) TOTAL_MDS_CHOSEN,DISPLAY(J)
        I=1
        J = J + 1
        WRITE(6,921) 'BN',CUTOFF_PMT(1,I),CUTOFF_PMT(2,I),
     *               CUTOFF_PMT(3,I),
     *               DISPLAY(J)
        I=2
        J = J + 1
        WRITE(6,921) 'FH',CUTOFF_PMT(1,I),CUTOFF_PMT(2,I),
     *               CUTOFF_PMT(3,I),
     *               DISPLAY(J)
        J = J + 1
        WRITE(6,922) SUBPH, 
     *               DISPLAY(J)
C
C
        DO I = J+1,IND-1
           IF(MOD(I,20).EQ.0) THEN
              CALL WIMG(6,'Hit < Return > to continue ')
              READ(5,928) CONT
           ENDIF
           WRITE(6,914) DISPLAY(I)
        ENDDO
C
C DISPLAY EXISTING WINNING BITMAPS
C
        CALL WIMG(6,'Do you want to see actual winning maps [Y/N] ')
        CALL YESNO(FLAG)
        IF(FLAG.EQ.1) THEN
           DO J = 1,BGOSUB-1            !DON'T DO LUCKY NUMBER
              IF(SUBGM(J).GT.0) THEN
                 MINMAP=999
                 MAXMAP=0
                 DO MAPNUM=1,BGOMAXMAP
                    IF(WINMAPS(MAPNUM,J).NE.0.AND.MAPNUM.LT.MINMAP)
     *                 MINMAP=MAPNUM
                    IF(WINMAPS(MAPNUM,J).NE.0.AND.MAPNUM.GT.MAXMAP)
     *                 MAXMAP=MAPNUM
                 ENDDO
                 IF(MAXMAP.GT.0) CALL DSPMAP(MINMAP,MAXMAP,WINMAPS,J)
              ENDIF
           ENDDO
        ENDIF 
C
C DISPLAY EXISTING PREDEFINED BITMAPS
C
        CALL WIMG(6,'Do you want to see predefined winning maps [Y/N] ')
        CALL YESNO(FLAG)
        IF(FLAG.EQ.1) THEN
           CALL CLRSCR(6)
3000       CONTINUE
           WRITE(6,905)
           DO MAPNUM=1,(PRENUM+1)/2
              WRITE(6,935) MAPNUM,WINMAP_NAMES(MAPNUM),
     *            MAPNUM+(PRENUM+1)/2,WINMAP_NAMES(MAPNUM+(PRENUM+1)/2)
           ENDDO
           CALL PRMNUM(' Enter map number (E-exit) : ',J,1,PRENUM,EXT)
           IF(EXT.NE.0) GOTO 4000


           IF(PRE_WINMAPS(J).NE.0) THEN
              DO X = 1,BGOCOL
                 MATTAB(X) = '  '
              ENDDO
              WRITE(6,931) J,PRE_WINMAPS(J)
              WRITE(6,924) (X,X=1,BGOCOL)
              DO Y = 1,BGOROW
                 DO X = 1,BGOCOL
                    IF(BTEST(PRE_WINMAPS(J),(Y-1)*BGOROW+X)) THEN
                       MATTAB(X) = ' X'
                    ELSE
                       MATTAB(X) = ' -'
                    ENDIF
                 ENDDO 
                 WRITE(6,930) Y,(MATTAB(K),K=1,BGOCOL)
              ENDDO
           ENDIF
           GOTO 3000
        ENDIF 
4000    CONTINUE

        TYPE*,' '
        CALL WIMG(6,' Hit <RETURN> to continue ')
C
        READ(5,918) ANS
        RETURN
C
C
800     FORMAT(' 1 - To Set  2 - To Display ',4A4,' parameters (E)xit')
901     FORMAT(' Total percentage entered ',F7.3,
     *          ' cannot exceed 100.00')
902     FORMAT(' Total percentage entered ',F7.3,
     *         ' is less than 100.00')
904     FORMAT('Enter division ',I2,' share value     ')
903     FORMAT(1X,A,'*** Entry of subgame ',A12,' parameters ***',/)
905     FORMAT(50(' '))
906     FORMAT('Match  Map ',I2,' wins',A3,' div ',I2)
908     FORMAT(1X,'Multi-draw....... ',A4,6X,A50)
909     FORMAT(1X,'Price/board...... ',A6,4X,A50)
910     FORMAT(1X,'Pool %(sales).... ',F7.3,3X,A50)
911     FORMAT(1X,'Divs ',A12,1X,I2.2,8X,A50)
912     FORMAT(1X,'Pool close time.. ',A8,2X,A50)
913     FORMAT(1X,'                  ',10X,A50)
914     FORMAT(29X,A50)
916     FORMAT(' Updating ',5A4,' with game parameters')
917     FORMAT(1X,4A4,' game parameter entry')
918     FORMAT(A4)
919     FORMAT(1X,' multidraw:      -> ',I2.2,' drws ',A50)   
920     FORMAT(1X,' -> ',I2,' multidraw(s) chosen  ',A50)
921     FORMAT(1X,'Phs ',A2,' Bmp:',I2,' Num:',I2,' Fig:',I1,2X,A50)
922     FORMAT(1X,I2,' subphases',T30,A50)
924     FORMAT(8X,' Column',<BGOCOL>(I2))
927     FORMAT('Enter number of divisions for ',A12)
928     FORMAT(A1)
929     FORMAT('Enter number of subphases (incl.FH) for ',A12)
930     FORMAT(10X,'Row ',I1,<BGOCOL>A2)
931     FORMAT(/10X,'*** Predefined Winning Bitmap Display for '
     *         'Bitmap ',I2,',',1X,Z8.8,' ***',/)
933     FORMAT(10X,'Winning Pattern Map   ',A8,' for ',A12,'  set')
934     FORMAT(/10X,'Selected Maps',/)
935     FORMAT(1X,2(9X,'Map ',I2,3X,A8))
936     FORMAT('Do you want set division for match ',I2,' [Y/N] ?')
937     FORMAT(10X,'Use for ',A12,' bingo',/
     *         10x,'             1-horizontals only',/
     *         10x,'             2-horizontals & verticals',/
     *         10x,'             3-horizontals & verticals & diagonals')
938     FORMAT(A2,' Div ',I2,2X,A8,' wins',1X,A10,' (fxd)')
939     FORMAT(A2,' Div ',I2,2X,A8,' wins',1X,F10.2,' (%pool)')
940     FORMAT('Enter division ',I2,1X,A8,' pool percentage [0=Fixed]   ')
941     FORMAT('Enter pool percentage for ',A8,'  [0=Fixed]   ')
        END

C
C SUBROUTINE DO DISPLAY A SET OF MAPS 
C
       SUBROUTINE DSPMAP(MINMAP,MAXMAP,WINMAPS,S)
       IMPLICIT NONE
       
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'

       INTEGER*4 MINMAP                            ! Lowest map #
       INTEGER*4 MAXMAP                            ! Hihgest map #
       INTEGER*4 WINMAPS(BGOMAXMAP,BGOSUB)         ! Winning bitmaps
       INTEGER*4 S                                 ! Subgame 

       INTEGER*4 BEGNUM,ENDNUM
       INTEGER*4 J,Y,K,I,LOOPS,L
       CHARACTER*13 ROWSTR(BGOROW,6)
       CHARACTER*2  MATTAB(BGOCOL)
       CHARACTER*12 SUBNAME(3)
       DATA SUBNAME/'Bingo AB    ','Full House  ','Lucky Number'/

       WRITE(6,942) SUBNAME(S)
       LOOPS=(MAXMAP-MINMAP+1)/6
       IF(MOD(MAXMAP-MINMAP+1,6).NE.0) LOOPS=LOOPS+1        
       BEGNUM=1
       IF(LOOPS.GT.1) THEN 
          ENDNUM=6
       ELSE
          ENDNUM=MAXMAP-MINMAP+1
       ENDIF

       DO L=1,LOOPS
          DO J=BEGNUM,ENDNUM
             if(WINMAPS(MINMAP+(L-1)*6+j-1,S).ne.0) then         
                DO Y = 1,BGOROW
                  DO K = 1,BGOCOL
                    IF(BTEST(WINMAPS(MINMAP+(L-1)*6+j-1,S),
     *                      (Y-1)*BGOROW+K)) THEN
                       MATTAB(K) = ' X'
                    ELSE
                       MATTAB(K) = ' -'
                    ENDIF
                  ENDDO 
                  WRITE(ROWSTR(Y,J),939) (MATTAB(K),K=1,BGOCOL)
                ENDDO
             ELSE
                DO Y = 1,BGOROW
                   DO K = 1,BGOCOL
                         MATTAB(K) = '  '
                   ENDDO 
                   WRITE(ROWSTR(Y,J),939) (MATTAB(K),K=1,BGOCOL)
                ENDDO
             endif
          enddo
          WRITE(6,940)((L-1)*6+MINMAP+I,I=0,ENDNUM-1)
          WRITE(6,941)((ROWSTR(Y,J),J=BEGNUM,ENDNUM),Y=1,BGOROW)

          BEGNUM=1
          ENDNUM=6
          IF(MAXMAP-MINMAP+1.LT.6*L+6) ENDNUM=MAXMAP-MINMAP+1-6*L

       ENDDO
       RETURN

939    FORMAT(3X,<BGOCOL>A2)
940    FORMAT(/5X,<ENDNUM-BEGNUM+1>('Map',I4,6X))
941    FORMAT(1X,<ENDNUM-BEGNUM+1>A13)
942    FORMAT(/3X,'Bitmaps for 'A12) 

       END
