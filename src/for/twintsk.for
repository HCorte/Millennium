C
C TWINTSK.FOR
C  
C V07 14-DEC-1999 OXK MULTIWIN changes.
C V06 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C V05 30 Jan 1994 HXK call VLF2VLC to speed up winsel
C V04 17 Oct 1993 GXA Added call to WINRPT for Toto Select Share /win report 
C		      generation.
C V03 21 Jan 1993 DAB Initial Release
C  		      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  		      DEC Baseline
C V02 12-NOV-1991 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C
C PROGRAM TWINTSK
C TOTO SELECT WINNER SELECTION PROCEDURE CONTROL TASK
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT

	PROGRAM TWINTSK
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
C
	INTEGER*4 FDB(7)
	INTEGER*4 FLAG, ST, K
        INTEGER*4 STATUS,TSKSTS

C
	CALL COPYRITE
C
        IF (STOPMOD.NE.WINMANUAL) THEN
           TYPE*,IAM(),'Use MULTIWIN instead.'
           CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C
	CALL OPENX(1,'SCF.FIL',4,0,0,ST)
	CALL IOINIT(FDB,1,SCFSEC*256)
	IF(ST.NE.0) THEN
	  TYPE*,IAM(),'SCF.FIL open error > ',ST
	  CALL GPAUSE
	ENDIF
	CALL READW(FDB,1,SCFREC,ST)
	IF(ST.NE.0) THEN
	  TYPE*,IAM(),'SCF.FIL read error > ',ST
	  CALL GPAUSE
	ENDIF
	CALL CLOSEFIL(FDB)
	CALL PRMYESNO(
     *   'Are you sure you want Toto Select winner selection [Y/N]? ',
     *   FLAG)
	IF(FLAG.NE.1) CALL GSTOP(GEXIT_OPABORT)



C
C CREATE NEW FILES
C
        IF(SFNAMES(1,VLF).NE.SFNAMES(1,VLC)) THEN
          TYPE*,IAM(),'Validation copy file must have same volume as vlf'
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
        IF(SFSIZES(VLC).NE.SFSIZES(VLF)) THEN
          TYPE*,IAM(),'Validation copy file must be same size as vlf'
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C
C COPY VLF TO VLC
C
        TYPE*,IAM(),'Running VLF2VLC to copy VLF.FIL to VLC.FIL'
        CALL NRUNTSK(8HVLF2VLC )

C
C CREATE NEW FILES
C
	CALL CRTFIL(SCFSFN(1,VLW),SCFFSZ(VLW),ST)
	IF(ST.NE.0) THEN
	  WRITE(5,903) IAM(),(SFNAMES(K,VLW),K=1,5)
	  CALL GPAUSE
	ENDIF
C
C MAKE SURE DRAWING PACK IS MOUNTED
C
	CALL PRMYESNO('Are the drawing packs mounted [Y/N]? ',FLAG)
	IF(FLAG.NE.1) THEN
	  WRITE(5,900) IAM()
	  CALL GPAUSE
	ENDIF
C
C LOAD AND START TWINSEL TASK
C
	WRITE(5,901) IAM(),'TWINSEL  '
	CALL RUNTSK(8HTWINSEL )
	WRITE(5,902) IAM()
C
        WRITE(5,901) IAM(),'WINRPT  '
        CALL RUNTSK(8HWINRPT  )
C
	CALL PRMYESNO(' Do you want to do file merge (Y/N) ',FLAG)
	IF(FLAG.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
C
C MAKE SURE VLF2VLC IS DONE
C
710     CONTINUE
        CALL XWAIT(2,2,STATUS)
        CALL STTSK(8HVLF2VLC ,TSKSTS,STATUS)
        IF (STATUS.NE.4) THEN
           GOTO 710
        ENDIF
C
C
C LOAD AND START VLF MERGE PROGRAM
C
	WRITE(5,901) 'MRGVLF  '
	CALL RUNTSK(8HMRGVLF  )
	CALL PRMYESNO('Did MRGVLF run ok [Y/N]? ',FLAG)
	IF(FLAG.NE.1) THEN
	  WRITE(5,904) IAM(),(SFNAMES(K,VLC),K=1,5)
	  CALL GPAUSE
	ENDIF
	CALL FMAINT(VLF,VLC,ST)
	WRITE(5,905) IAM()
C
C LOAD AND START REPORT PROGRAMS
C
	WRITE(5,901) IAM(),'BKKREP  '
	CALL RUNTSK(8HBKKREP  )
C
	CALL GSTOP(GEXIT_SUCCESS)
C
900	FORMAT(1X,A,1X,' Mount drawing packs and continue TWINTSK ')
901	FORMAT(1X,A,1X,' Begining execution of ',A8)
902	FORMAT(1X,A,1X,' Toto Select winner selection complete ')
903	FORMAT(1X,A,1X,' Error while allocating ',5A4)
904	FORMAT(1X,A,1X,' Initialize ',5A4,' and rerun MRGVLF')
905	FORMAT(1X,A,1X,' Validation file merge complete')
	END
