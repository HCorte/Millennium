C
C     FILE   : BLDTGL.DEF
C     AUTHOR : J.H.R
C     VERSION: 01            DATE: 28 / 11 / 2000
C
C
C V01 JHR SUBROUTINE TO DEFINE RESULTS GAME PARAMETERS ( TOTOGOLO PORTUGAL ).
C
C     **************************************************************************
C
C        THIS ITEM IS THE PROPERTY OF GTECH CORPORATION, POVIDENCE, RHODE
C     ISLAND, AND CONTAINS CONFIDENTIAL AND TRADE SECRET INFORMATION. IT MAY
C     NOT BE TRANSFERRED FROM THE CUSTODY OR CONTROL OF GTECH EXCEPT AS AUTO -
C     RIZED IN WRITING BY AN OFFICER OF GTECH. NEITHER THIS ITEM NOR THE
C     INFORMATION IT CONTAINS MAY BE USED, TRANSFERRED, REPRODUCED, PUBLISHED
C     OR DISCLOSED, IN WHOLE OR IN PART, AND DIRECTLY OR INDIRECTLY, EXCEPT AS
C     EXPRESSLY AUTHORIZED BY AN OFFICER OR GTECH, PURSUANT TO WRITTEN AGREEMENT
C
C     Copyright 2000 GTECH Corporation. All Rigth Reserved
C
C     **************************************************************************
C
C THIS ROUTINE ALLOW UPDATE RESULTS ( TOTOGOLO ) GAME PARAMETERS
C
C=======OPTIONS /CHECK = NOOVERFLOW /EXT
      SUBROUTINE BLDTGL(FILE, GNAME)
      IMPLICIT NONE
C
C INCLUDE DEFINITION TO UPDATE RESULTS GAME
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:DTGREC.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'
      INCLUDE 'INCLIB:DESPAR.DEF'
C
C ARGUMENTS DEFINITION TO UPDATE RESULTS GAME
C
      INTEGER * 4 FILE(5)                  ! FILE GAME NAME
      INTEGER * 4 GNAME(4)                 ! GAME LONG NAME 
C
C VARIABLES DEFINITION TO UPDATE RESULTS GAME
C
	INTEGER*4  FDB(7)                   !
	INTEGER*4  DPER(TGGDIV)             !
	INTEGER*4  WINTAB(TGGDIV)           !
	INTEGER*4  YN(2)                    !
	INTEGER*4  ST                       !
	INTEGER*4  DRAW                     !
	INTEGER*4  IND                      !
	INTEGER*4  NUM                      !
	INTEGER*4  TOTPER                   !
	INTEGER*4  I                        !
	INTEGER*4  SPER                     !
	INTEGER*4  DIV                      !
	INTEGER*4  TIME                     !
	INTEGER*4  PRICE                    !
	INTEGER*4  MULTI                    !
	INTEGER*4  EXT                      !
        INTEGER*4  OPT                      !
        INTEGER*4  ANS                      !
	INTEGER*4  MROWS                    !
	INTEGER*4  FLAG                     !
	INTEGER*4  MIND                     !
        INTEGER*4  BEST                     !
        INTEGER*4 I4_MDS                    ! multi-draw value
        INTEGER*4 PREVIOUS_I4_MDS           ! previous multi-draw value
        INTEGER*4 TOTAL_MDS_CHOSEN          ! total multi-draws chosen
	INTEGER*4 REV1			    ! Control Rev Sequence #
	INTEGER*4 REV2			    ! Control Rev (checksum)
        INTEGER*4 EXTRA                     ! EXTRAORDINARY GAME
C
        BYTE      BYTE_MDS(MAXMLTD_AVL)     !

C
	CHARACTER*37  STRING1               !
	CHARACTER*41  STRING2               !
	CHARACTER*63  STRING4               !
	CHARACTER*42  DISPLAY(20)           !

	DATA YN/'No  ','Yes '/
C
        COMMON SCFREC
C
	TOTAL_MDS_CHOSEN = 0

	CALL CLRSCR(6)
	WRITE (STRING4,800) GNAME
        CALL INPNUM(STRING4,OPT,1,2,EXT)
        IF(EXT.NE.0) GOTO 1000          
        IF(OPT.EQ.2) GOTO 2000          
C
C
10	CONTINUE
	CALL CLRSCR(6)
	CALL FASTSET(0,DPER,TGGDIV)
	CALL FASTSET(0,WINTAB,TGGDIV)
	WRITE(6,917) GNAME
C
C
	CALL INPNUM('Enter number of rows/bet ',MROWS,1,TGGNBR,EXT)
	IF(EXT.LT.0) GOTO 1000

15      CONTINUE
	MULTI = 1
        PREVIOUS_I4_MDS = 0
        DO I = 1, MAXMLTD_AVL
          BYTE_MDS(I)=0
        END DO
        BYTE_MDS(1)=1          !single week wagering always allowed
C
C SET FLAG FOR EXTRORDINARY GAMES
C
        EXTRA = 0             
        CALL PRMYESNO('Is This Game An Extraordinary Game [Y/N]', FLAG)
        IF(FLAG .EQ. 1) EXTRA = 1
C
	CALL PRMYESNO('Is there multi-draw wagering [Y/N] ',FLAG)
	IF (FLAG.EQ.1) THEN
	    CALL INPNUM('Enter maximum number of draws ',MULTI,1,15,EXT)
	    IF(EXT.LT.0) GOTO 15

	    WRITE(6,*)'Enter multi-draws allowed '
	    WRITE(6,*)'(e.g. Lotto =2,3,5 and 10)'
            I = 2
20          CONTINUE

            CALL INPNUM('Enter multi-draw allowed [E=finished] ',
     *                   I4_MDS,2,MAXMLTD_AVL,EXT)
            IF(EXT.EQ.0) THEN
                IF (I.GT.MAXMLTD_SEL-1) THEN   !prevent more than max allowed
                    TYPE*,'Too many multi-draws selected.'
                    TYPE*,'total cannot exceed ',MAXMLTD_SEL-1  
							!1 drw already chosen
                    CALL XWAIT(2,2,ST)
                    GOTO 15
                ENDIF

                IF(BYTE_MDS(I4_MDS).NE.0) THEN  !prevent writing over values
                    TYPE*,'Draw multiple already chosen '
                    TYPE*,'Re-enter data '
                    CALL XWAIT(2,2,ST)
                    GOTO 15
                ENDIF

                IF(I4_MDS.LE.PREVIOUS_I4_MDS) THEN
                    TYPE*,'Draw multiples MUST be entered in '
                    TYPE*,'ascending order'
                    CALL XWAIT(2,2,ST)
                    GOTO 15
                ENDIF

                BYTE_MDS(I4_MDS)=I
                PREVIOUS_I4_MDS=I4_MDS
                I = I + 1
                GOTO 20
            ENDIF

            IF(EXT.EQ.-1) THEN
                TOTAL_MDS_CHOSEN=I-1
            ELSE
                TYPE*,'Incorrect data entered '
                GOTO 20
            ENDIF

        ENDIF

C
        CALL INPNUM('Enter ticket price/bet (without decimal point) ',
     *               PRICE,1,9999999,EXT)
	IF(EXT.LT.0) GOTO 1000
C
C
	CALL INPTIM('Enter pool close time HH:MM:SS ',TIME,EXT)
	IF(EXT.LT.0) GOTO 1000
C
C
	CALL INPNUM('Enter number of divisions ',DIV,1,TGGDIV,EXT)
	IF(EXT.LT.0) GOTO 1000
C
C
	CALL INPPER('Enter pool percentage of sales ',SPER,EXT)
	IF(EXT.LT.0) GOTO 1000
C
C
100	CONTINUE
	DO I = 1, DIV
	    WRITE (STRING1,900) I
	    CALL INPPER(STRING1,DPER(I),EXT)
	    IF(EXT.LT.0) GOTO 1000
        END DO
C
	TOTPER=0
	DO I = 1, DIV
	    IF(DPER(I).GT.0) TOTPER=TOTPER+DPER(I)
        END DO

	IF(TOTPER.GT.100000) THEN
	    WRITE(6,901) DISPER(TOTPER)
	    GOTO 100
	ENDIF

	IF(TOTPER.LT.100000) THEN
	    WRITE(6,902) DISPER(TOTPER)
	    CALL PRMYESNO('Is this correct [Y/N] ',FLAG)
	    IF(FLAG.NE.1) GOTO 100
	ENDIF
C
C
        BEST = 0
	CALL FASTSET(0,WINTAB,TGGDIV)
        CALL PRMYESNO('Is this a "best match" game (Y/N)',FLAG)

        IF(FLAG.NE.1) THEN       
	    DO I = 1, DIV
	        WRITE (STRING2,903) I
	        CALL INPNUM(STRING2,NUM,1,MROWS,EXT)
	        IF(EXT.LT.0) GOTO 1000
	        WINTAB(I)=NUM
            END DO
        ELSE
            BEST=1               
            DO I = 1, TGGDIV     
                NUM = MROWS - I + 1
                IF(NUM.GT.0) WINTAB(I)=NUM
            END DO
        ENDIF                             
C       

C
C DISPLAY OPTIONS
C
	MIND=1
	IF(MULTI.GT.1) MIND=2

	CALL CLRSCR(6)
	IND=0

	DO I = 1, 20
	    WRITE (DISPLAY(I),905)
        END DO

	DO I = 1, DIV
	    IND=IND+1
	    IF(BEST.EQ.0) THEN
	        WRITE (DISPLAY(IND),906) WINTAB(I),I,DISPER(DPER(I))
	    ELSE
	        WRITE (DISPLAY(IND),9061) DISPER(DPER(I))
	    ENDIF
        END DO
C
C
	WRITE(6,907) MROWS,DISPLAY(1)
	WRITE(6,908) YN(MIND),DISPLAY(2)
	WRITE(6,930) ( DFLOAT(PRICE)/(SCFPAR(PRFACTOR)*DOLL_BASE) ),DISPLAY(3)
	WRITE(6,910) DISPER(SPER),DISPLAY(4)
	WRITE(6,911) DIV,DISPLAY(5)
	WRITE(6,912) DISTIM(TIME),DISPLAY(6)
        WRITE(6,915) YN(EXTRA + 1)
	IND=8

        DO I = 2, MAXMLTD_AVL
            IF(BYTE_MDS(I).NE.0) THEN
	        WRITE(6,9190) I, DISPLAY(IND)
                IND = IND + 1
            ENDIF
        END DO

        WRITE(6,9200) TOTAL_MDS_CHOSEN,DISPLAY(IND)

	IF(IND.LE.20) THEN
	    DO I = IND+1, 20
	        WRITE(6,914) DISPLAY(IND)
            END DO
	ENDIF
C
C
	TYPE*,'    '
	CALL PRMYESNO('Are these values correct [Y/N] ',FLAG)
	IF(FLAG.NE.1) GOTO 10
C
C
500	CONTINUE
	CALL CLRSCR(6)
	CALL INPNUM(
     *	 'Enter first draw for this game description ',
     *	 DRAW,1,10000,EXT)
	IF(EXT.LT.0) GOTO 1000
C
C UPDATE RESULTS GAME FILE
C
	WRITE(6,916) FILE
	CALL OPENW(2,FILE,4,0,0,ST)
	CALL IOINIT(FDB,2,DTGSEC*256)
	IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)
C
C
510	CONTINUE
	CALL READW(FDB,DRAW,DTGREC,ST)
	IF(ST.EQ.144) THEN
	    TYPE*,'Last draw initialized - ',DRAW-1
	    CALL CLOSEFIL(FDB)
	    CALL XWAIT(2,2,ST)
	    RETURN
	ENDIF
	IF(ST.NE.0) CALL FILERR(FILE,2,ST,DRAW)
C
C
	IF(DTGSTS.GT.GAMOPN) THEN
	    TYPE*,'Game already closed for draw ',DRAW
	    CALL GPAUSE
            CALL CLOSEFIL(FDB)
            GOTO 500
	ENDIF
C
C
	DTGPRC = PRICE
	DTGMAX = MROWS
        DTGEXT = EXTRA
	DTGMLT = MULTI
	DTGDIV = DIV
	DTGSPR = SPER
	DTGTIM = TIME
        DTGBST = BEST
	DO I = 1, TGGDIV
	    DTGMAT(I) = WINTAB(I)
	    DTGPER(I) = DPER(I)
        END DO
        DO I = 1, MAXMLTD_AVL
            DTGMDS(I) = BYTE_MDS(I)
        END DO

C
C UPDATE CONTROL REVISION NUMBERS
C
	CALL ILBYTE(REV1,DTGREV,0)
	CALL ILBYTE(REV2,DTGREV,1)
	IF(REV1.NE.0.AND.REV2.NE.0) THEN
	   REV1 = REV1 + 1
	ELSE
	   REV1 = 1
	   REV2 = MOD(DRAW,255)
	ENDIF
C
	CALL ISBYTE(REV1,DTGREV,0)
	CALL ISBYTE(REV2,DTGREV,1)
C
C
	CALL WRITEW(FDB,DRAW,DTGREC,ST)
	IF(ST.NE.0) CALL FILERR(FILE,3,ST,DRAW)
	DRAW = DRAW + 1
	GOTO 510
C
C
C
1000	CONTINUE
	CALL CLRSCR(6)

	RETURN
C                                                                               
C DISPLAY GAME PARAMETERS                                                       
C                                                                               
2000    CONTINUE                                                    
        CALL INPNUM(' Enter Draw to display: ',DRAW,1,99999,EXT)    
        IF(EXT.NE.0) GOTO 1000                                      
C                                                                               
        CALL OPENW(2,FILE,4,0,0,ST)                                 
        CALL IOINIT(FDB,2,DTGSEC*256)                               
        IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)                        
C                                                                               
	CALL FASTSET(0,DTGREC,DTGLEN)
        CALL READW(FDB,DRAW,DTGREC,ST)                              
        IF(ST.NE.0) CALL FILERR(FILE,2,ST,DRAW)                     
        CALL CLOSEFIL(FDB)                                          
C                                                                               
        CALL CLRSCR(6)                                              
        IND=0                                                       

        MIND=1
        IF(DTGMLT.GT.1) MIND=2

        DO I=1,20
            WRITE (DISPLAY(I),905)
        END DO

	DO I = 1, DTGDIV
	    IND=IND+1
	    IF(DTGBST.EQ.0) THEN
	        WRITE (DISPLAY(IND),906) DTGMAT(I),I, DISPER(DTGPER(I))
	    ELSE
	        WRITE (DISPLAY(IND),9061) DISPER(DTGPER(I))
	    ENDIF
        END DO
C
C
	WRITE(6,907) DTGMAX,DISPLAY(1)
	WRITE(6,908) YN(MIND),DISPLAY(2)
	WRITE(6,930) ( DFLOAT(DTGPRC)/(SCFPAR(PRFACTOR)*DOLL_BASE) ),DISPLAY(3)
	WRITE(6,910) DISPER(DTGSPR),DISPLAY(4)
	WRITE(6,911) DTGDIV,DISPLAY(5)
	WRITE(6,912) DISTIM(DTGTIM),DISPLAY(6)
        WRITE(6,915) YN(DTGEXT + 1)
	IND=8

        DO I = 2, MAXMLTD_AVL
            IF(DTGMDS(I).NE.0) THEN
	        WRITE(6,9190) I, DISPLAY(IND)
                IND = IND + 1
            ENDIF
        END DO

        TYPE*,' '                                      
        CALL PRMTEXT('Hit <RETURN> to continue ',ANS,I)


C
C
800     FORMAT(' 1 - To SET  2 - To DISPLAY ',4A4,' parameters (E)xit')
C800     FORMAT(' Are you sure you want to set ',4A4,' parameters (Y/N)')
900	FORMAT('Enter division ',I2,' percentage of pool ')
901	FORMAT(' Total percentage entered ',F7.3,
     *	        ' cannot exceed 100.00')
902	FORMAT(' Total percentage entered ',F7.3,
     *	       ' is less than 100.00')
903	FORMAT('Enter number of rows to match for div ',I2)
905	FORMAT(42(' '))
C906	FORMAT('Match ',I2.2,' wins division ',I2.2)
C9061	FORMAT('Match ?? wins division ',I2.2)
906     FORMAT('Match ',I2.2,' wins division ',I2.2,' Pool % ',F7.3)
9061    FORMAT('Match ?? wins division ?? Pool % ',F7.3)            
907	FORMAT(1X,'# rows/bet....... ',I2.2,8X,A42)
908	FORMAT(1X,'Multi-draw....... ',A4,6X,A42)
C909	FORMAT(1X,'Price/board...... ',A6,4X,A42)
910	FORMAT(1X,'Pool %(sales).... ',F7.3,3X,A42)
911	FORMAT(1X,'Divisions........ ',I2.2,8X,A42)
912	FORMAT(1X,'Pool close time.. ',A8,2X,A42)
913	FORMAT(1X,'Div ',I2.2,' %(pool)... ',F7.3,3X,A42)
9131	FORMAT(1X,'Div ',I2.2,' free ticket',10X,A42)
914	FORMAT(29X,A42)
915     FORMAT(1X,'Extra Game....... ', A)
916	FORMAT(' Updating ',5A4,' with game parameters')
917	FORMAT(1X,4A4,' game parameter entry')
919     FORMAT(A4)                                                  
9190    FORMAT(1X,' multidraw:      -> ',I2.2,' drws ',A42)   
9200    FORMAT(1X,' -> ',I2,' multidraw(s) chosen  ',A42)
930     FORMAT(1X,'Price/board...... ',F8.4,2X,A42)

	END
