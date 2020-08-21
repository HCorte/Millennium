C
C SUBROUTINE BLDLTO
C
C V14 23-NOV-2010 MAC LUCKY NUMBER
C     30-NOV-2010 FJG JACKPOT POST GAME
C V13 01-AUG-2009 FJG Portugal Fiscal Legislation changes
C V12 11-DEC-2000 EPH Read base price without decimal point so that it can 
C                     contain how many decimals needed
C V11 12-JUN-2000 UXN Cleaned up
C V10 05-NOV-1993 GXA Changed Revision#.
C V09 10-OCT-1993 HXK Fix for MDS for invoicing.
C V08 07-JUL-1993 GXA Added Control Revision # updating during file updates.
C V07 17-JUN-1993 SXH Changed logic of IF(MULTI.NE.1)MIND=2 to
C                     IF(MULTI.GT.1)MIND=2
C V06 09-JUN-1993 SXH Added multi-draw options
C V05 04-MAY-1993 STUART Added sales percentage DLTPAW
C V04 03-MAY-1993 STUART Added Viking Lotto
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 07-OCT-1991 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C SUBROUTINE TO DEFINE LOTTO GAME PARAMETERS.
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE BLDLTO(FILE,GNAME,GIND)	
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DLTREC.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DESPAR.DEF'

	COMMON SCFREC
C
        ! Arguments
        INTEGER*4 FILE(5)            !
        INTEGER*4 GNAME(4)           !
        INTEGER*4 GIND               !

        ! Variables
                                     !
	INTEGER*4 FDB(7)             !
	INTEGER*4 DPER(LTGDIV)       !
	INTEGER*4 YN(2)              !
	INTEGER*4 PRIZE(LTGDIV)      !
        INTEGER*4 WINTAB(LTGNBR,LTGBON+1,LTGBET) !
C
	INTEGER*4 ST                 !
	INTEGER*4 DRAW               !
	INTEGER*4 IND                !
	INTEGER*4 DWON               !
	INTEGER*4 J                  !
	INTEGER*4 TOTPER             !
	INTEGER*4 ANS                !
	INTEGER*4 I                  !
	INTEGER*4 SPER               !
	INTEGER*4 DIV                !
	INTEGER*4 PRICE              !
	INTEGER*4 TIME               !
	INTEGER*4 MULTI              !
	INTEGER*4 BONUS              !
	INTEGER*4 PICKED             !
	INTEGER*4 EXT                !
	INTEGER*4 MAXNUM             !
	INTEGER*4 K                  !
	INTEGER*4 FLAG               !
	INTEGER*4 MIND               !
	INTEGER*4 ADV                !
	INTEGER*4 BTYPES             !
	INTEGER*4 BTTAB(LTGBET)      !
	INTEGER*4 BET                !
        INTEGER*4 OPT                !
        INTEGER*4 SPAW               ! sales percentage applied to wins 
        INTEGER*4 I4_MDS             ! multi-draw value
        INTEGER*4 PREVIOUS_I4_MDS    ! previous multi-draw value
        INTEGER*4 TOTAL_MDS_CHOSEN   ! total multi-draws chosen
        INTEGER*4 REV1               ! Control Rev Sequence #
        INTEGER*4 REV2               ! Control Rev (checksum)      
	INTEGER*4 JACKMIN        
	INTEGER*4 JACKPOS            !V14 FJG
        INTEGER*4 LUCKYN             !V14 LUCKY NUMBER enable flag (max Lucky Number as well)
        INTEGER*4 LUCKYDV            !V14 Lucky Number refund division number
        INTEGER*4 LUCKYCNV(2,LTGDIV) !V14 LUCKY NUMBER division conversion table
C                          1 - WITH    LUCKY NUMBER
C                          2 - WITH NO LUCKY NUMBER
C
        BYTE      BYTE_MDS(MAXMLTD_AVL) !

	CHARACTER*53 STRING1         !
	CHARACTER*58 STRING2,STRING3 !
	CHARACTER*63 STRING4         !
	CHARACTER*48 DISPLAY(20)     !
	CHARACTER*30 STRING5         !
	CHARACTER*2  PLUS(2)         !


	DATA YN/'No  ','Yes '/
	DATA PLUS/'  ','+ '/
C
        ! start of code

	CALL CLRSCR(6)
	WRITE (STRING4,800) GNAME
        CALL INPNUM(STRING4,OPT,1,2,EXT)                                          
        IF(EXT.NE.0) GOTO 1000                                                    
        IF(OPT.EQ.2) GOTO 2000                                                    

C	CALL WIMG(5,STRING4)
C	CALL YESNO(FLAG)
C	IF(FLAG.NE.1) GOTO 1000
C
10	CONTINUE
	CALL CLRSCR(6)
	CALL FASTSET(0,DPER,LTGDIV)
	CALL FASTSET(0,WINTAB,LTGNBR*LTGBET*(LTGBON+1))
	CALL FASTSET(0,BTTAB,LTGBET)
	CALL FASTSET(0,PRIZE,LTGDIV)
	CALL FASTSET(0,LUCKYCNV,2*LTGDIV)  !V14
        LUCKYN=0                           !V14
        LUCKYDV=0                          !V14
	WRITE(6,918) GNAME
C
C
	CALL INPNUM('Enter highest number allowed to bet ',
     *	            MAXNUM,1,64,EXT)
	IF(EXT.LT.0) GOTO 1000
C
C
	CALL INPNUM('Enter number of numbers drawn ',PICKED,
     *	            1,MAXNUM,EXT)
	IF(EXT.LT.0) GOTO 1000
C
C
	BONUS=0
	CALL INPYESNO('Is there a bonus number drawn [Y/N]',FLAG)
	IF(FLAG.EQ.1) THEN
	    CALL INPNUM('Enter number of bonus numbers ',BONUS,1,3,EXT)
	    IF(EXT.LT.0) GOTO 1000
	ENDIF
C
C
	CALL INPYESNO('Is there a lucky number drawn [Y/N]',FLAG) !V14...
	IF(FLAG.EQ.1) THEN
	    CALL INPNUM('Enter lucky number max value ',LUCKYN,1,LTGLUC,EXT)
	    IF(EXT.LT.0) GOTO 1000
	ENDIF                                                     !...V14
C
C
	CALL INPNUM('Enter number of bet types ',BTYPES,1,LTGBET,EXT)
	IF(EXT.LT.0) GOTO 1000
C
C
	DO I = 1, BTYPES
	    WRITE(STRING1,810) I
	    CALL INPNUM(STRING1,BET,1,PICKED,EXT)
	    IF(EXT.LT.0) GOTO 1000
	    BTTAB(I)=BET
        END DO
C
C
15      CONTINUE
	MULTI = 1
        PREVIOUS_I4_MDS = 0
        DO I = 1, MAXMLTD_AVL
          BYTE_MDS(I)=0
        END DO
        BYTE_MDS(1)=1          !single week wagering always allowed

	CALL INPYESNO('Is there multi-draw wagering [Y/N] ',FLAG)
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
C
C
C
	ADV=0
	CALL INPYESNO('Is there advance wagering [Y/N]',FLAG)
	IF(FLAG.EQ.1) ADV=1
C
C
	CALL INPTIM('Enter pool close time HH:MM:SS ',TIME,EXT)
	IF(EXT.LT.0) GOTO 1000
C===============================================================================
C INI V13
C===============================================================================
	CALL INPMONY('Enter minimum Jackpot Value (0 = No minimum value) ',JACKMIN,VALUNIT,EXT)
	IF(EXT.LT.0) GOTO 1000	
C===============================================================================
C FIN V13
C===============================================================================
        CALL INPYESNO('Is the JACKPOT linked with other Lotto game [Y/N]',FLAG)
        IF(FLAG.EQ.1) THEN
30        CONTINUE          
	  CALL INPNUM('Enter Lotto Index to join Jackpot ',JACKPOS,1,MAXIND,EXT)
	  IF(EXT.LT.0) GOTO 1000          
	  IF(JACKPOS.EQ.GIND) THEN
            TYPE*,'Index cannot be the same',JACKPOS
            CALL XWAIT(2,2,ST)
            GOTO 30	    
	  ENDIF
        ELSE
          JACKPOS = 0         
        ENDIF
C=======V14 FJG=================================================================
	CALL INPNUM('Enter ticket price/board (Without decimal ppoint) ',PRICE,1,9999999, EXT)
	IF(EXT.LT.0) GOTO 1000
C
C
	CALL INPNUM('Enter number of divisions ',DIV,1,LTGDIV,EXT)
	IF(EXT.LT.0) GOTO 1000
C
C
	CALL INPPER('Enter pool percentage of sales (0 = fixed payout) ',
     *               SPER,EXT)
	IF(EXT.LT.0) GOTO 1000
C
        CALL INPPER('Enter sales percentage applied to wins ',SPAW,        
     *               EXT)                                                  
        IF(EXT.LT.0) GOTO 1000                                             

C
	IF(SPER.NE.0) THEN
100	    CONTINUE
	    DO 110 I=1,DIV
	        WRITE (STRING1,900) I
	        CALL INPPER(STRING1,DPER(I),EXT)
	        IF(EXT.LT.0) GOTO 1000
110	    CONTINUE
	  
            TOTPER=0
	    DO 120 I=1,DIV
	        IF(DPER(I).GT.0) TOTPER=TOTPER+DPER(I)
120	    CONTINUE

	    IF(TOTPER.GT.100000) THEN
	        WRITE(5,901) DISPER(TOTPER)
	        GOTO 100
	    ENDIF

	    IF(TOTPER.LT.100000) THEN
	        WRITE(6,902) DISPER(TOTPER)
	        CALL INPYESNO('Is this correct [Y/N]',FLAG)
	        IF(FLAG.NE.1) GOTO 100
	    ENDIF
	ELSE
	    DO 130 I=1,DIV
	        WRITE(STRING5,820) I
	        CALL INPMONY(STRING5,PRIZE(I),VALUNIT,EXT)
	        IF(EXT.LT.0) GOTO 1000
130	    CONTINUE
	ENDIF
C
C
	DO 200 I=1,LTGNBR
	    DO 200 J=1,LTGBON+1
	        DO 200 K=1,LTGBET
	            WINTAB(I,J,K)=0
200	CONTINUE
C
	DO 220 K=1,BTYPES
	    DO 210 I=1,BTTAB(K)
	        WRITE (STRING2,903) BTTAB(K),I
	        CALL INPNUM(STRING2,DWON,0,DIV,EXT)
	        IF(EXT.LT.0) GOTO 1000
	        WINTAB(I,1,K)=DWON
	        IF(BONUS.GE.1) THEN
	            WRITE (STRING3,904) BTTAB(K),I,PLUS(2)
	            CALL INPNUM(STRING3,DWON,0,DIV,EXT)
	            IF(EXT.LT.0) GOTO 1000
	            WINTAB(I,2,K)=DWON
	        ENDIF
210	    CONTINUE
220	CONTINUE
c
        IF (LUCKYN.GT.0)THEN                    !V14...
          DO I=1,LTGDIV
            WRITE (STRING3,9041) I
            CALL INPNUM(STRING3,LUCKYCNV(1,I),0,LTGDIV,EXT)
            WRITE (STRING3,9042) I
            CALL INPNUM(STRING3,LUCKYCNV(2,I),0,LTGDIV,EXT)
            IF(EXT.LT.0) GOTO 1000
          ENDDO
          WRITE (STRING3,9043)
          CALL INPNUM(STRING3,LUCKYDV,1,LTGDIV,EXT)
          IF(EXT.LT.0) GOTO 1000
        ENDIF                                   !...V14
C
C DISPLAY OPTIONS
C
	MIND=1
	IF(MULTI.GT.1) MIND=2
	CALL CLRSCR(6)
	IND=0
	DO 300 I=1,20
	    WRITE (DISPLAY(I),905)
300	CONTINUE

	DO 310 K=1,LTGBET
	    DO I=1,LTGNBR
	        DO J=1,2
	            IF(WINTAB(I,J,K).NE.0) THEN
	                IND=IND+1
	                IF(SPER.EQ.0) THEN
	                    WRITE (DISPLAY(IND),906) BTTAB(K),I,PLUS(J),
     *                             WINTAB(I,J,K),
     *                             CMONY(PRIZE(WINTAB(I,J,K)),11,VALUNIT)
	                ELSE
                            WRITE (DISPLAY(IND),906) BTTAB(K),I,PLUS(J),
     *                             WINTAB(I,J,K)
	                ENDIF
	            ENDIF
                END DO
            END DO
310	CONTINUE
C
C
	WRITE(6,907) PICKED,MAXNUM,DISPLAY(1)
	WRITE(6,908) BONUS,DISPLAY(2)
	WRITE(6,909) YN(MIND),DISPLAY(3)
	WRITE(6,910) (DFLOAT(PRICE)/(DOLL_BASE*SCFPAR(PRFACTOR))),DISPLAY(4)
	IF(SPER.NE.0) THEN
	    WRITE(6,911) DISPER(SPER),DISPLAY(5)
	ELSE
	    WRITE(6,9111) DISPLAY(5)
	ENDIF
	WRITE(6,912) DIV,DISPLAY(6)
	WRITE(6,913) DISTIM(TIME),DISPLAY(7)
	WRITE(6,914) YN(ADV+1),DISPLAY(8)
C===============================================================================
C INI V13-V14
C===============================================================================	
        WRITE(6,920) CMONY(JACKMIN,12,BETUNIT),DISPLAY(9)	
        IF(JACKPOS.GT.0) THEN
          WRITE(6,921) JACKPOS,DISPLAY(10)	        
        ELSE
          WRITE(6,922) DISPLAY(10)	                  
        ENDIF
	IND=11
C===============================================================================
C FIN V13-V14
C===============================================================================
	IF(SPER.NE.0) THEN
	    DO 400 I=1,DIV
	        IF(IND.LE.20) THEN
	            WRITE(6,915) I,DISPER(DPER(I)),DISPLAY(IND)
	            IND=IND+1
	        ELSE
	            WRITE(6,915) I,DISPER(DPER(I))
	        ENDIF
400	    CONTINUE
	ENDIF
        WRITE(6,9152) DISPER(SPAW),DISPLAY(IND)                              
        IND=IND+1                                                            

        DO I = 2, MAXMLTD_AVL
            IF(BYTE_MDS(I).NE.0) THEN
	        WRITE(6,9190) I, DISPLAY(IND)
                IND = IND + 1
            ENDIF
        END DO

        WRITE(6,9200) TOTAL_MDS_CHOSEN,DISPLAY(IND)

	IF(IND.LE.20) THEN
	    DO I = IND+1, 20
	        WRITE(6,916) DISPLAY(I)
            END DO
	ENDIF
C
        IF (LUCKYN.GT.0) THEN               !V14...
          WRITE(6,9081) LUCKYN,(LUCKYCNV(1,I),I=1,LTGDIV) 
          WRITE(6,9082) LUCKYDV,(LUCKYCNV(2,I),I=1,LTGDIV)
        ENDIF                               !...V14 
C
	TYPE*,'    '
	CALL INPYESNO('Are these values correct [Y/N]',FLAG)
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
C UPDATE LOTTO GAME FILE
C
	WRITE(6,917) FILE
	CALL OPENW(2,FILE,4,0,0,ST)
	CALL IOINIT(FDB,2,DLTSEC*256)
	IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)
C
C
510	CONTINUE
	CALL READW(FDB,DRAW,DLTREC,ST)
	IF(ST.EQ.144) THEN
	    TYPE*,'Last draw initialized - ',DRAW-1
	    CALL CLOSEFIL(FDB)
	    CALL XWAIT(2,2,ST)
	    RETURN
	ENDIF
	IF(ST.NE.0) CALL FILERR(FILE,2,ST,DRAW)
C
C
	IF(DLTSTS.GT.GAMOPN) THEN
	    TYPE*,'Game already closed for draw ',DRAW
	    CALL GPAUSE
	    CALL CLOSEFIL(FDB)
	    GOTO 500
	ENDIF
C
C
	DLTPRC = PRICE
	DLTMAX = MAXNUM
	DLTBFL = BONUS
	DLTNUM = PICKED
	DLTMLT = MULTI
	DLTADW = ADV
	DLTDIV = DIV
	DLTJPG = JACKPOS                     !V14 FJG
        DLTLFL = LUCKYN                      !V14...
        DLTLDV = LUCKYDV
        DO I=1,LTGDIV
          DO J=1,2
            DLTLNC(J,I)=LUCKYCNV(J,I)
          ENDDO
        ENDDO                                !...V14
	DLTSPR = SPER
	DLTTIM = TIME
C===============================================================================
C INI V13
C===============================================================================	
        DLTMIN = JACKMIN	
C===============================================================================
C FIN V13
C===============================================================================        
	DO I=1,LTGNBR
	    DO J=1,LTGBON+1
	        DO K=1,LTGBET
	            DLTWTB(I,J,K) = WINTAB(I,J,K)
                END DO
            END DO
        END DO

	DO I=1,LTGDIV
	    DLTPER(I)=DPER(I)
	    DLTSHV(I,1)=PRIZE(I)
	END DO
	CALL FASTMOV(BTTAB,DLTBET,LTGBET)

        DLTPAW=SPAW

        DO I = 1, MAXMLTD_AVL
            DLTMDS(I) = BYTE_MDS(I)
        END DO
C
C UPDATE CONTROL REVISION NUMBERS
C
        CALL ILBYTE(REV1,DLTREV,0)
	CALL ILBYTE(REV2,DLTREV,1)
        IF(REV1.NE.0.AND.REV2.NE.0) THEN
           REV1 = REV1 + 1
        ELSE
	   REV1 = 1
           REV2 = MOD(DRAW,255)
        ENDIF
C
        CALL ISBYTE(REV1,DLTREV,0)
        CALL ISBYTE(REV2,DLTREV,1)
C
C
	CALL WRITEW(FDB,DRAW,DLTREC,ST)
	IF(ST.EQ.144) THEN
	    TYPE*,'last draw initialized ',DRAW
	    CALL XWAIT(2,2,ST)
	    GOTO 1000
	ENDIF
	IF(ST.NE.0) CALL FILERR(FILE,3,ST,DRAW)
	DRAW=DRAW+1
	GOTO 510
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
        CALL IOINIT(FDB,2,DLTSEC*256)                               
        IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)                        
C                                                                               
	CALL FASTSET(0,DLTREC,DLTLEN)
        CALL READW(FDB,DRAW,DLTREC,ST)                              
        IF(ST.NE.0) CALL FILERR(FILE,2,ST,DRAW)                     
        CALL CLOSEFIL(FDB)                                          
C                                                                               
        CALL CLRSCR(6)                                              
        IND=0                                                       

        MIND=1
        IF(DLTMLT.GT.1) MIND=2

        DO I=1,20
            WRITE (DISPLAY(I),905)
        END DO

        DO K=1,LTGBET
	    DO I=1,LTGNBR
	        DO J=1,2
	            IF(DLTWTB(I,J,K).NE.0) THEN
	                IND=IND+1
	                IF(DLTSPR.EQ.0) THEN
	                    WRITE (DISPLAY(IND),906) DLTBET(K),I,PLUS(J),
     *                             DLTWTB(I,J,K),
     *                             CMONY(DLTSHV(DLTWTB(I,J,K),1),11,VALUNIT)
	                ELSE
                            WRITE (DISPLAY(IND),906) DLTBET(K),I,PLUS(J),
     *                             DLTWTB(I,J,K)
	                ENDIF
	            ENDIF
                END DO
            END DO
        END DO
C
C
	WRITE(6,907) DLTNUM,DLTMAX,DISPLAY(1)
	WRITE(6,908) DLTBFL,DISPLAY(2)
	WRITE(6,909) YN(MIND),DISPLAY(3)
	WRITE(6,910) (DFLOAT(DLTPRC)/(DOLL_BASE*SCFPAR(PRFACTOR))),DISPLAY(4)
	IF(DLTSPR.NE.0) THEN
	    WRITE(6,911) DISPER(DLTSPR),DISPLAY(5)
	ELSE
	    WRITE(6,9111) DISPLAY(5)
	ENDIF
	WRITE(6,912) DLTDIV,DISPLAY(6)
	WRITE(6,913) DISTIM(DLTTIM),DISPLAY(7)
	WRITE(6,914) YN(ADV+1),DISPLAY(8)
C===============================================================================
C INI V13
C===============================================================================	
        WRITE(6,920) CMONY(DLTMIN,12,BETUNIT),DISPLAY(9)	
        IF(DLTJPG.GT.0) THEN
          WRITE(6,921) DLTJPG,DISPLAY(10)	        
        ELSE
          WRITE(6,922) DISPLAY(10)	                  
        ENDIF
	IND=11
C===============================================================================
C FIN V13
C===============================================================================
C
	IF(DLTSPR.NE.0) THEN
	    DO I=1,DLTDIV
	        IF(IND.LE.20) THEN
	            WRITE(6,915) I,DISPER(DLTPER(I)),DISPLAY(IND)
	            IND=IND+1
	        ELSE
	            WRITE(6,915) I,DISPER(DLTPER(I))
	        ENDIF
	    END DO
	ENDIF
        WRITE(6,9152) DISPER(DLTPAW),DISPLAY(IND)                              
        IND=IND+1                                                            

        DO I = 2, MAXMLTD_AVL
            IF(DLTMDS(I).NE.0) THEN
                WRITE(6,9190) I,DISPLAY(IND)
                IND = IND + 1
            ENDIF
        END DO

	IF(IND.LE.20) THEN
	    DO I=IND,20
	        WRITE(6,916) DISPLAY(I)
	    END DO
	ENDIF
C
        IF (DLTLFL.GT.0) THEN               !V14...
          WRITE(6,9081) DLTLFL,(DLTLNC(1,I),I=1,LTGDIV) 
          WRITE(6,9082) DLTLDV,(DLTLNC(2,I),I=1,LTGDIV) 
        ENDIF                               !...V14
C

      TYPE*,' '                                                                 
      CALL WIMG(6,'Hit <RETURN> to continue ')                                  
      READ(5,919) ANS                                                           

      RETURN                                                                    


C
C
C800   FORMAT(' Are you sure you want to set ',4A4,' parameters (Y/N)')
800   FORMAT(' 1 - To SET  2 - To DISPLAY ',4A4,' parameters (E)xit')           
810   FORMAT('Enter number of numbers picked for type ',I1)
820   FORMAT('Enter division ',I1,' share value ')
900   FORMAT('Enter division ',I2,' percentage of pool (0 = fixed) ')
901	FORMAT(' Total percentage entered ',F7.3,
     *	        ' cannot exceed 100.00')
902	FORMAT(' Total percentage entered ',F7.3,
     *	       ' is less than 100.00')
903	FORMAT('Enter division won for picking ',I2,
     *           ' matching ',I2,'  [0-nowin]')
904	FORMAT('Enter division won for picking ',I2,
     *         ' matching ',I2,A2,'[0-nowin]')
9041	FORMAT('Division ',I2, ' with    Lucky Number should be division: ' )    !V14
9042	FORMAT('Division ',I2, ' with NO Lucky Number should be division: ' )    !V14
9043	FORMAT('Enter Lucky Number refund division number ' )                    !V14
905	FORMAT(48(' '))
906	FORMAT(' Pick ',I2,' Match ',I2.2,A1,' wins division ',I2.2,1X,A11)
907	FORMAT(1X,'Lotto............ ',I2.2,'/',I2.2,5X,A48)
908	FORMAT(1X,'Bonus number(s).. ',I1,9X,A48)
9081	FORMAT(1X,'MAX Lucky Number. ',I2,' Division conversion table with    lucky number ',<LTGDIV>I2)      !V14
9082	FORMAT(1X,'LN refund div.    ',I2,' Division conversion table with NO lucky number ',<LTGDIV>I2)      !V14
909	FORMAT(1X,'Multi-draw....... ',A4,6X,A48)
910	FORMAT(1X,'Price/board...... ',F8.4,2X,A48)
911	FORMAT(1X,'Pool %(sales).... ',F7.3,3X,A48)
9111    FORMAT(1X,'Pool %(sales).... ','FIXED  ',3X,A48)
912	FORMAT(1X,'Divisions........ ',I2.2,8X,A48)
913	FORMAT(1X,'Pool close time.. ',A8,2X,A48)
914     FORMAT(1X,'Advance wagering. ',A4,6X,A48)
915	FORMAT(1X,'Div ',I2.2,' %(pool)... ',F7.3,3X,A48)
9151    FORMAT(1X,'Div ',I2.2,' free ticket',10X,A48)                             
9152    FORMAT(1X,'%Sales applied .. ',F7.3,3X,A48)      
920     FORMAT(1X,'Minimum Jackpot.. ',A12,1X,A48)                                                    
921     FORMAT(1X,'Jackpot Linked to Lotto Index',I2,A48)    
922     FORMAT(1X,'Jackpot NOT Linked',13X,A48)    
916	FORMAT(29X,A48)
917	FORMAT(' Updating ',5A4,' with game parameters')
918	FORMAT(1X,4A4,' game parameter entry')
919     FORMAT(A4)                                                                
9190    FORMAT(1X,' multidraw:      -> ',I2.2,' drws ',A48)   
9200    FORMAT(1X,' -> ',I2,' multidraw(s) chosen  ',A48)

	END
