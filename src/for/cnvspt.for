C CNVSPT.FOR
C
C V03 22-NOV-2010 FJG Avoid Warnings
C V02 03-MAR-2000 OXK Sharecalc params addeed
C V01 10-FEB-2000 OXK Initial revision for Vakio changes
C
C This program converts old Sports game file S1F.FIL to new format.
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

	OPTIONS /CHECK/EXT
	PROGRAM CNVSPT
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
	INCLUDE 'INCLIB:DSPREC_OLD.DEF'

        INTEGER*4   DRAW
        INTEGER*4   ST, I, J, K, OLD_FDB(7), FDB(7)
        INTEGER*4   YESNO
        INTEGER*4   MAXDRW_CNV          ! MAX NUMBER OF DRAWS IN FILE.
C
        INTEGER*4   FILE_SIZE           ! NEW FILE SIZE
        CHARACTER*20 NEW_FILE,OLD_FILE
        INTEGER*4    I4NEW_FILE(5),I4OLD_FILE(5)
        EQUIVALENCE (NEW_FILE,I4NEW_FILE)
        EQUIVALENCE (OLD_FILE,I4OLD_FILE)
        INTEGER*4   SIZE

C Begin Code ------------------------------------------------

        CALL COPYRITE
C
C
        NEW_FILE = 'FILE:S1F.NEW'
        OLD_FILE = 'FILE:S1F.FIL'
C
        CALL OPENX(1,OLD_FILE,4,0,0,ST)
        CALL VAXGETFSIZ(1,SIZE)
        CLOSE(1)
C
        MAXDRW_CNV = SIZE/(ODSPSEC/2)
        FILE_SIZE  = MAXDRW_CNV*DSPSEC/2
        TYPE*,IAM(),'Old file size = ', MAXDRW_CNV*ODSPSEC/2
        TYPE*,IAM(),'New file size = ', MAXDRW_CNV*DSPSEC/2
        TYPE*,IAM()
        TYPE*,IAM(),'Number of draws to convert >',MAXDRW_CNV
        TYPE*,IAM()
        CALL PRMYESNO('Are you sure you want to convert VAKIO 1 file',YESNO)
        IF(YESNO.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
        CALL CRTFIL(I4NEW_FILE,FILE_SIZE,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
        CALL OPENW(3,I4NEW_FILE,0,0,0,ST)
        CALL IOINIT(FDB,3,DSPSEC*256)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,1,ST,0)
C
        CALL OPENW(4,I4OLD_FILE,0,0,0,ST)
        CALL IOINIT(OLD_FDB,4,ODSPSEC*256)
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,1,ST,0)
C
        TYPE*,IAM(),'STARTING...'
C
C       Read record from file

        DRAW=0
        DO WHILE(ST.EQ.0)
           DRAW = DRAW + 1
           CALL READW(OLD_FDB,DRAW,ODSPREC,ST)
           IF(ST.EQ.144) GOTO 100
           IF(ST.NE.0) CALL FILERR(I4OLD_FILE,2,ST,DRAW)
C

	CALL FASTSET (0,DSPREC,DSPLEN)

	DSPSTS = ODSPSTS			
	DSPCTM = ODSPCTM			
	DSPTIM = ODSPTIM			
	DSPDRW = ODSPDRW			
	DSPBSD = ODSPBSD			
	DSPESD = ODSPESD			
	DSPPUP = ODSPPUP			
	DSPUPD = ODSPUPD			
	DO I=1,DATLEN
	    DSPDAT(I) = ODSPDAT(I)	
	ENDDO
	DO I=1,NUMADV
	    DSPADV(I) =	ODSPADV(I)	
	ENDDO
	DO I=1,SPGENT
	    DSPSAL(I) = ODSPSAL(I)	
	ENDDO
	DO I=1,SPGDIV
	    DSPSHV(I) = ODSPSHV(I)	
	    DSPSHR(I) = ODSPSHR(I)	
	    DSPPOL(I) = ODSPPOL(I)	
	    DSPPAD(I) = ODSPPAD(I)	
	    DSPPRG(I) = ODSPPRG(I)	
C           DSPANU(I) = ODSPANU(I)	
C           DSPBRK(I) = ODSPBRK(I)	
            DSPOSV(I) = ODSPOSV(I)	
C           DSPFRZ(I) = ODSPFRZ(I)	
	ENDDO
	DO I=1,OSPGNBR
	    DSPWIN(I) =	ODSPWIN(I)	
	    DSPHLD(I) = ODSPHLD(I)	
	ENDDO
	DSPPRP = ODSPPRP		
	DSPPRN = ODSPPRN		
					
	DSPTAX = ODSPTAX		
	DO I=1,2
	    DSPRES(I)= ODSPRES(I)	      
	ENDDO
	DSPAPL = ODSPAPL		
	DSPMIN = ODSPMIN		
	DSPSER = ODSPSER		
	DSPOPA = ODSPOPA		
	DSPPRC = ODSPPRC		
	DSPMAX = ODSPMAX	    
	DSPMLT = ODSPMLT	    
	DO I=1,MAXMLTD_AVL
	    DSPMDS(I) = ODSPMDS(I)  
	ENDDO
	DSPDIV = ODSPDIV			
	DO I=1,SPGDIV
	    DSPMAT(I) = ODSPMAT(I)		 
	    DSPPER(I) = ODSPPER(I)		 
	ENDDO
	DSPSPR = ODSPSPR			
	DO I=1,SPGDIV
	    DSPTSR(I) = ODSPTSR(I)		 
	ENDDO
	DSPREV = ODSPREV			

	DO I=1,OSPGNBR
	    DO J=1,2
		DO K=1,SPNMS_LEN/4
		    DSPNMS(K,J,I) = ODSPNMS(K,J,I)
		ENDDO
	    ENDDO
	ENDDO

	DO I=1,25
	    DSPCLB(I) = ODSPCLB(I)		     
	ENDDO
	DO I=1,SPGDIV
	    DSPASH(I) = ODSPASH(I)		 
	ENDDO
	DSPFRG = ODSPFRG			
	DO I=1,POSTED
	    DSPWRF(I) = ODSPWRF(I)		 
	ENDDO
	DSPBST = ODSPBST			
	DO I=1,MAXDRW
	    DSPBAL(I) = ODSPBAL(I)		 
	ENDDO

C New data that is not found in the old version:
C
	DSPEVN(1) = '    '
	DSPEVN(2) = '    '
	DSPEVN(3) = '    '
	DSPEVN(4) = '    '
	DO I=1,2
	    DSPROD(I) = 0
	ENDDO

C          Write record to file
C
           CALL WRITEW(FDB,DRAW,DSPREC,ST)
           IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,DRAW)

           IF(MOD(DRAW,500).EQ.0) TYPE*,IAM(),DRAW,' records converted...'
        ENDDO
100     CONTINUE
C
        TYPE*,IAM(),DRAW-1,' records converted in total.'
C
        CALL CLOSEFIL(FDB)
        CALL CLOSEFIL(OLD_FDB)
C
        TYPE*,IAM(),'Renaming FILE:S1F.FIL to FILE:S1F.OLD'
        CALL LIB$RENAME_FILE('FILE:S1F.FIL','FILE:S1F.OLD')

        TYPE*,IAM(),'Renaming FILE:S1F.NEW to FILE:S1F.FIL'
        CALL LIB$RENAME_FILE('FILE:S1F.NEW','FILE:S1F.FIL')
C
        TYPE*,IAM(),'VAKIO 1 game file converted succesfully!'
        CALL GSTOP(GEXIT_SUCCESS)
        END
