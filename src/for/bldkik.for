C
C SUBROUTINE BLDKIK
C
C V11 12-DEC-2000 EPH Base price up to 4 decimals
C V10 06-DEC-2000 EPH/ANG INPUT DKKMIN (MINIMUM JACKPOT) VALUE
C V09 06-DEC-2000 EPH/ANG INSERT INPUT FOR MIXED FIXED/SHARED DIVISIONS
C V08 12-JUN-2000 UXN Cleaned up.
C V07 05-NOV-1993 GXA Changed Revision#.
C V06 10-OCT-1993 HXK Fix for MDS for invoicing.
C V05 07-JUL-1993 GXA Added Control Revision # updating during file updates.
C V04 18-JUN-1993 SXH Changed logic of IF(MULTI.NE.1)MIND=2 to
C                     IF(MULTI.GT.1)MIND=2
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 07-OCT-1991 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C SUBROUTINE TO DEFINE KICKER GAME PARAMETERS.
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE BLDKIK(FILE,GNAME)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DKKREC.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DESPAR.DEF'

        ! arguments
        INTEGER*4  FILE(5)                  !
        INTEGER*4  GNAME(4)                 !

        ! variables
        INTEGER*4  YN(2)                    !
	INTEGER*4  FDB(7)                   !
	INTEGER*4  DPER(KIGDIV)             !
	INTEGER*4  WINTAB(3,KIGDIV)         !
	INTEGER*4  OCTAL(8)                 !
	INTEGER*4  PRIZES(KIGDIV)           !
	INTEGER*4  ST                       !
	INTEGER*4  DRAW                     !
	INTEGER*4  IND                      !
	INTEGER*4  NUM                      !
	INTEGER*4  J                        !
	INTEGER*4  TOTPER                   !
	INTEGER*4  I                        !
	INTEGER*4  SPER                     !
	INTEGER*4  DIV                      !
	INTEGER*4  TIME                     !
	INTEGER*4  MAX                      !
	INTEGER*4  EXT                      !
        INTEGER*4  OPT                      !
	INTEGER*4  PRICE                    !
	INTEGER*4  FLAG                     !
        INTEGER*4  ANS                      !
	INTEGER*4  OCTDIG/0/                !
        INTEGER*4  MULTI                    !
        INTEGER*4  MIND                     !
        INTEGER*4  TMPDFF                   ! double joker flag
        INTEGER*4  I4_MDS                   ! multi-draw value
        INTEGER*4  PREVIOUS_I4_MDS          ! previous multi-draw value
        INTEGER*4  TOTAL_MDS_CHOSEN         ! total multi-draws chosen
        INTEGER*4 REV1                      ! Control Rev Sequence #
        INTEGER*4 REV2                      ! Control Rev (checksum)
C
	INTEGER*4 JACKMIN

        BYTE       BYTE_MDS(MAXMLTD_AVL)    !

	CHARACTER*80  STRING1               !
	CHARACTER*60  STRING2               !
	CHARACTER*3   FST(3)                !
	CHARACTER*63  STRING4               !
	CHARACTER*30  STRING5               !
	CHARACTER*50  DISPLAY(20)           !

	DATA YN/'No  ','Yes '/
	DATA OCTAL/7,63,511,4095,32767,262143,2097151,16777215/
	DATA FST/'1st','2nd','3rd'/

	COMMON SCFREC

C
C
	CALL CLRSCR(6)

	WRITE (STRING4,800) GNAME
	CALL INPNUM(STRING4,OPT,1,2,EXT)
        IF(EXT.NE.0) GOTO 1000                                                    
        IF(OPT.EQ.2) GOTO 2000                                                    
C
10	CONTINUE
	CALL CLRSCR(6)
	CALL FASTSET(0, DPER,   KIGDIV)
	CALL FASTSET(0, WINTAB, KIGDIV)
	CALL FASTSET(0, PRIZES, KIGDIV)
	WRITE(6,917) GNAME
C
C
	CALL INPNUM('Enter ticket price/bet Without decimal point ',PRICE,1,9999999,EXT)
	IF(EXT.LT.0) GOTO 1000
C
C
	CALL INPNUM('Enter highest number to issue ',MAX,99,
     *	      9999999,EXT)
	IF(EXT.LT.0) GOTO 1000
C
C
15      CONTINUE
	MULTI = 1
        PREVIOUS_I4_MDS = 0
        DO I = 1, MAXMLTD_AVL
          BYTE_MDS(I)=0
        END DO
        BYTE_MDS(1)=1          !single week wagering always allowed

	CALL INPYESNO('Is there multi-draw wagering [Y/N]', FLAG)
	IF (FLAG.EQ.1) THEN
	    CALL INPNUM('Enter maximum number of draws ',MULTI,1,15,EXT)
	    IF(EXT.LT.0) GOTO 15

            CALL WIMG(6,'Enter multi-draws allowed ')
            CALL WIMG(6,'(e.g. Lotto =2,3,5 and 10)')
            I = 2
20          CONTINUE

            CALL INPNUM('Enter multi-draw allowed [E=finished] ',
     *                   I4_MDS,2,MAXMLTD_AVL,EXT)
            IF(EXT.EQ.0) THEN
                IF (I.GT.MAXMLTD_SEL-1) THEN   !prevent more than max allowed
                    TYPE*,'Too many multi-draws selected.'
                    TYPE*,'total cannot exceed ',MAXMLTD_SEL-1  !1 drw already chosen
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


	CALL INPTIM('Enter pool close time HH:MM:SS ',TIME,EXT)
	IF(EXT.LT.0) GOTO 1000


	CALL INPMONY('Enter minimum Jackpot Value (0 = No minimum value) ',JACKMIN,VALUNIT,EXT)
	IF(EXT.LT.0) GOTO 1000

	CALL INPNUM('Enter number of divisions ',DIV,1,KIGDIV,EXT)
	IF(EXT.LT.0) GOTO 1000
C
C
C
	CALL INPPER('Enter pool percentage of sales (0 = All divs fixed payout) ',
     *               SPER,EXT)
	IF(EXT.LT.0) GOTO 1000
C
C
	IF(SPER.NE.0) THEN
100	    CONTINUE
	    DO I = 1, DIV
	        WRITE (STRING1,900) I
	        CALL INPPER(STRING1,DPER(I),EXT)
	        IF(EXT.LT.0) GOTO 1000
                IF(DPER(I).EQ.0) THEN
	           WRITE(STRING5,820) I
	           CALL INPMONY(STRING5,PRIZES(I),VALUNIT,EXT)
	           IF(EXT.LT.0) GOTO 1000
                ENDIF
            END DO

	    TOTPER=0
	    DO 120 I = 1, DIV
	        IF(DPER(I).GT.0) TOTPER=TOTPER+DPER(I)
120	    CONTINUE

	    IF(TOTPER.GT.100000) THEN
	        WRITE(6,901) DISPER(TOTPER)
	        GOTO 100
	    ENDIF

	    IF(TOTPER.LT.100000) THEN
	        WRITE(6,902) DISPER(TOTPER)
	        CALL INPYESNO('Is this correct [Y/N] ', FLAG)
	        IF(FLAG.NE.1) GOTO 100
	    ENDIF
	ELSE
	    DO I = 1, DIV
	        WRITE(STRING5,820) I
	        CALL INPMONY(STRING5,PRIZES(I),VALUNIT,EXT)
	        IF(EXT.LT.0) GOTO 1000
            END DO
	ENDIF
C
C
	CALL FASTSET(0,WINTAB,KIGDIV*3)
	DO 210 I = 1, DIV
	    DO  J = 1, 3
	        WRITE (STRING2,903) FST(J),I
	        CALL INPNUM(STRING2,NUM,1,9999999,EXT)
	        IF(EXT.EQ.-8) GOTO 210
	        IF(EXT.LT.0) GOTO 1000
	        WINTAB(J,I)=NUM
            END DO
210	CONTINUE
C
C GET NUMBER OF OCTAL DIGITS FOR RANDOMIZER
C
	DO I = 8, 1, -1
	    IF(MAX.LE.OCTAL(I)) OCTDIG=I
        END DO
C                                                                               
C GET DOUBLE JOKERI FREE FLAG                                                   
C                                                                               
        TMPDFF=0                                           
        CALL INPYESNO('Is there Free Double Jokeri?', FLAG)
        IF(FLAG.EQ.1) TMPDFF=1                             
C
C DISPLAY OPTIONS
C
	CALL CLRSCR(6)

        MIND = 1
        IF (MULTI.GT.1) MIND = 2
	IND=0
	DO I = 1, 20
	    WRITE (DISPLAY(I),905)
        END DO

	DO I = 1, DIV
	    IND = IND + 1
	    WRITE (DISPLAY(IND),906) (WINTAB(J,I),J=1,3),I

C	    IF(DPER(I).EQ.0) THEN
C                WRITE (DISPLAY(IND),9061) (WINTAB(J,I),J=1,3),I,
C     *                                     CMONY(PRIZES(I),11,VALUNIT)
C	    ELSE
C	        WRITE (DISPLAY(IND),906) (WINTAB(J,I),J=1,3),I
C	    ENDIF
        END DO
C
C
	WRITE(6,907) MAX,                                   DISPLAY(1)
	WRITE(6,908) DFLOAT(PRICE)/(SCFPAR(PRFACTOR)*DOLL_BASE), DISPLAY(2)
	WRITE(6,909) DISPER(SPER),                          DISPLAY(3)
	WRITE(6,910) DIV,                                   DISPLAY(4)
	WRITE(6,911) DISTIM(TIME),                          DISPLAY(5)
        WRITE(6,920) YN(MIND),                              DISPLAY(6)
        WRITE(6,921) YN(TMPDFF+1),                          DISPLAY(7)
        WRITE(6,922) CMONY(JACKMIN,12,BETUNIT),             DISPLAY(8)
	IND=9
	IF(SPER.NE.0) THEN
	    DO I = 1, DIV
                IF (DPER(I).GT.0) THEN
	           IF(IND.LE.20) THEN
	              WRITE(6,912) I,DISPER(DPER(I)),DISPLAY(IND)
	              IND=IND+1
	           ELSE
	              WRITE(6,912) I,DISPER(DPER(I))
	           ENDIF
                ELSE
                   IF(IND.LE.20) THEN
                      WRITE(6,914) I,CMONY(PRIZES(I),11,VALUNIT),DISPLAY(IND)
                      IND = IND + 1
                   ELSE
                      WRITE(6,915) I,CMONY(PRIZES(I),11,VALUNIT)
                   ENDIF                                        
                ENDIF
            END DO
	ENDIF

        DO I = 2, MAXMLTD_AVL
            IF(BYTE_MDS(I).NE.0) THEN
	        WRITE(6,9190) I, DISPLAY(IND)
                IND = IND + 1
            ENDIF
        END DO

        WRITE(6,9200) TOTAL_MDS_CHOSEN,DISPLAY(IND)

	IF(IND.LE.20) THEN
	    DO I = IND+1, 20
	        WRITE(6,913) DISPLAY(IND)
            END DO
	ENDIF
C
C
	CALL INPYESNo('Are these values correct [Y/N]',FLAG)
	IF(FLAG.NE.1) GOTO 10
C
C
500	CONTINUE

	CALL CLRSCR(6)
	CALL INPNUM(
     *	  'Enter first draw for this game description ',
     *	  DRAW,1,10000,EXT)
	IF(EXT.LT.0) GOTO 1000
C
C UPDATE KICKER GAME FILE
C
	WRITE(6,916) FILE
	CALL OPENW(2,FILE,4,0,0,ST)
	CALL IOINIT(FDB,2,DKKSEC*256)
	IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)
C
C
510	CONTINUE
	CALL READW(FDB,DRAW,DKKREC,ST)
	IF(ST.EQ.144) THEN
	    TYPE*,'Last draw initialized - ',DRAW-1
	    CALL CLOSEFIL(FDB)
	    CALL XWAIT(2,2,ST)
	    RETURN
	ENDIF
	IF(ST.NE.0) CALL FILERR(FILE,2,ST,DRAW)
C
C
	IF(DKKSTS.GT.GAMOPN) THEN
	    TYPE*,'Game already closed for draw ',DRAW
	    CALL GPAUSE
	    CALL CLOSEFIL(FDB)
	    GOTO 500
	ENDIF
C

	DKKPRC = PRICE
	DKKMAX = MAX
	DKKDIV = DIV
	DKKSPR = SPER
	DKKTIM = TIME
        DKKMIN = JACKMIN
	DKKOCT = OCTDIG
        DKKMLT = MULTI
        DKKDFF = TMPDFF
	CALL FASTMOV(PRIZES, DKKSHV, KIGDIV)
	DO I = 1, KIGDIV
	    DO J = 1, 3
	        DKKMAT(J,I) = WINTAB(J,I)
	        DKKPER(I)   = DPER(I)
            END DO
        END DO

        DO I = 1, MAXMLTD_AVL
            DKKMDS(I) = BYTE_MDS(I)
        END DO
C
C UPDATE CONTROL REVISION NUMBERS
C
        CALL ILBYTE(REV1,DKKREV,0)
	CALL ILBYTE(REV2,DKKREV,1)
        IF(REV1.NE.0.AND.REV2.NE.0) THEN
           REV1 = REV1 + 1
        ELSE
	   REV1 = 1
           REV2 = MOD(DRAW,255)
        ENDIF
C
        CALL ISBYTE(REV1,DKKREV,0)
        CALL ISBYTE(REV2,DKKREV,1)
C
C
	CALL WRITEW(FDB,DRAW,DKKREC,ST)
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
C
C                                                                               
C DISPLAY GAME PARAMETERS                                                       
C                                                                               

2000    CONTINUE                                                    
        CALL INPNUM(' Enter Draw to display: ',DRAW,1,99999,EXT)    
        IF(EXT.NE.0) GOTO 1000                                      
C                                                                               
        CALL OPENW(2,FILE,4,0,0,ST)                                 
        CALL IOINIT(FDB,2,DKKSEC*256)                               
        IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)                        
C                                                                               
	CALL FASTSET(0,DKKREC,DKKRECLEN)
        CALL READW(FDB,DRAW,DKKREC,ST)                              
        IF(ST.NE.0) CALL FILERR(FILE,2,ST,DRAW)                     
        CALL CLOSEFIL(FDB)                                          
C                                                                               
        CALL CLRSCR(6)                                              
        IND=0                                                       

        MIND = 1
        IF (DKKMLT.GT.1)MIND=2

	DO I = 1, 20
	    WRITE (DISPLAY(I),905)
        END DO

	DO I = 1, DKKDIV
	    IND = IND + 1
	    WRITE (DISPLAY(IND),906) (DKKMAT(J,I),J=1,3),I
C	    IF(DKKPER(I).EQ.0) THEN
C                WRITE (DISPLAY(IND),9061) (DKKMAT(J,I),J=1,3),I,
C     *                                     CMONY(DKKSHV(I),11,VALUNIT)
C	    ELSE
C	        WRITE (DISPLAY(IND),906) (DKKMAT(J,I),J=1,3),I
C	    ENDIF
        END DO
C
C
	WRITE(6,907) DKKMAX,                                   DISPLAY(1)
	WRITE(6,908) DFLOAT(DKKPRC) / (SCFPAR(PRFACTOR)*DOLL_BASE), DISPLAY(2)
	WRITE(6,909) DISPER(DKKSPR),                           DISPLAY(3)
	WRITE(6,910) DKKDIV,                                   DISPLAY(4)
	WRITE(6,911) DISTIM(DKKTIM),                           DISPLAY(5)
        WRITE(6,920) YN(MIND),                                 DISPLAY(6)
        WRITE(6,921) YN(DKKDFF+1),                             DISPLAY(7)
        WRITE(6,922) CMONY(DKKMIN,12,BETUNIT),                 DISPLAY(8)

	IND=9
	IF(DKKSPR.NE.0) THEN
	    DO I = 1, DKKDIV
                IF (DKKPER(I).GT.0) THEN
	           IF(IND.LE.20) THEN
	              WRITE(6,912) I,DISPER(DKKPER(I)),DISPLAY(IND)
	              IND=IND+1
	           ELSE
	              WRITE(6,912) I,DISPER(DKKPER(I))
	           ENDIF
                ELSE
	           IF(IND.LE.20) THEN
	              WRITE(6,914) I,CMONY(DKKSHV(I),11,VALUNIT),DISPLAY(IND)
		      IND = IND + 1
                   ELSE
		      WRITE(6,915) I,CMONY(DKKSHV(I),11,VALUNIT)
	           ENDIF					
                ENDIF
            END DO
	ENDIF

        DO I = 2, MAXMLTD_AVL
            IF(DKKMDS(I).NE.0) THEN
	        WRITE(6,9190) I, DISPLAY(IND)
                IND = IND + 1
            ENDIF
        END DO


	IF(IND.LE.20) THEN
	    DO I = IND, 20
	        WRITE(6,913) DISPLAY(IND)
            END DO
	ENDIF
C
        TYPE*,' '                                                                 
        CALL WIMG(6,'Hit <RETURN> to continue ')                                  
        READ(5,923) ANS                                                           

        RETURN                                                                    



C800     FORMAT(' Are you sure you want to set ',4A4,' parameters (Y/N)')
800     FORMAT(' 1 - To SET  2 - To DISPLAY ',4A4,' parameters (E)xit')           
820     FORMAT('Enter division ',I1,' share value ')
900	FORMAT('Enter division ',I2,' percentage of pool (0 = Fixed division)')
901	FORMAT(' Total percentage entered ',F7.3,
     *	        ' cannot exceed 100.00')
902	FORMAT(' Total percentage entered ',F7.3,
     *	       ' is less than 100.00')
903	FORMAT('Enter ',A3,' winmap (9-mat,1-nomat) ',
     *	       'for div ',I2,' (N - next div)')
905	FORMAT(50(' '))
906     FORMAT('Mat ',I7.0'/',I7.0,'/',I7.0,' wins div ',I2)
9061	FORMAT('Mat ',I7.0'/',I7.0,'/',I7.0,' wins div ',I2,A11)
907	FORMAT(1X,'Max number..... ',I7,2X,A50)
908	FORMAT(1X,'Price/board.... ',F8.4,1X,A50)
909	FORMAT(1X,'Pool %(sales).. ',F7.3,2X,A50)
910	FORMAT(1X,'Divisions...... ',I2.2,7X,A50)
911	FORMAT(1X,'Pool close time ',A8,1X,A50)
912	FORMAT(1X,'Div ',I2.2,' %(pool). ',F7.3,2X,A50)
913	FORMAT(27X,A50)
914	FORMAT(1X,'Div ',I2.2,' Fix. Prize ',A11,2X,A50)
915	FORMAT(1X,'Div ',I2.2,' Fix. Prize ',A11)
916	FORMAT(' Updating ',5A4,' with game parameters')
917	FORMAT(1X,4A4,' game parameter entry')
9190    FORMAT(1X,' multidraw:      -> ',I2.2,' drws ',A50)   
9200    FORMAT(1X,' -> ',I2,' multidraw(s) chosen  ',A50)
921     FORMAT(1X,'2nd Jokeri Free ',A4,5X,A50)                                     
922     FORMAT(1X,'Minimum Jackpot ',A12,1X,A50)                                     
920	FORMAT(1X,'Multi-draw..... ',A4,5X,A50)	
923     FORMAT(A4)
        END

