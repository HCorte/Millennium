C
C PROGRAM DWINTSK
C
C V04 15-FEB-2000 UXN Drawing pack mounted question removed.
C V03 14-DEC-1999 OXK MULTIWIN changes.
C V02 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C                     Instant Pass Thru Phase 1
C V01 23 Nov 1995 PXB Initial revision.
C
C DWINTSK.FOR
C
C SUPER DOUBLE WINNER SELECTION PROCEDURE CONTROL TASK
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT

	PROGRAM DWINTSK

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'

C---- Local Variables.

	INTEGER*4 FDB(7)
	INTEGER*4 FLAG, ST, K
        INTEGER*4 STATUS,TSKSTS


C---------------------------- Start of code ------------------------------

	CALL COPYRITE

        IF (STOPMOD.NE.WINMANUAL) THEN
           TYPE*,IAM(),'Use MULTIWIN instead.'
           CALL GSTOP(GEXIT_FATAL)
        ENDIF

	CALL OPENX(1,'SCF.FIL',4,0,0,ST)

	CALL IOINIT(FDB,1,SCFSEC*256)

	IF (ST .NE. 0) THEN
	  TYPE*,IAM(),'SCF.FIL open error > ',ST
	  CALL GPAUSE
	END IF

	CALL READW(FDB,1,SCFREC,ST)

	IF (ST .NE. 0) THEN
	  TYPE*,IAM(),'SCF.FIL read error > ',ST
	  CALL GPAUSE
	END IF

	CALL CLOSEFIL(FDB)

	CALL PRMYESNO(
     *   'Are you sure you want Super Double winner selection [Y/N]? ',
     *   FLAG)

	IF (FLAG .NE. 1) CALL GSTOP(GEXIT_OPABORT)


C---- Create new files

        IF (SFNAMES(1,VLF) .NE. SFNAMES(1,VLC)) THEN
          TYPE*,IAM(),'Validation copy file must have same volume as vlf'
          CALL GSTOP(GEXIT_FATAL)
        END IF

        IF (SFSIZES(VLC) .NE. SFSIZES(VLF)) THEN
          TYPE*,IAM(),'Validation copy file must be same size as vlf'
          CALL GSTOP(GEXIT_FATAL)
        END IF

C---- Copy vlf to vlc

        TYPE*,IAM(),'Running VLF2VLC to copy VLF.FIL to VLC.FIL'

        CALL NRUNTSK(8HVLF2VLC )


C---- Create new files

	CALL CRTFIL(SCFSFN(1,VLW),SCFFSZ(VLW),ST)

	IF (ST .NE. 0) THEN
	  WRITE(5,903) IAM(),(SFNAMES(K,VLW),K=1,5)
	  CALL GPAUSE
	END IF

C---- Load and start dwinsel task

	WRITE(5,901) IAM(),'DWINSEL  '

	CALL RUNTSK(8HDWINSEL )

	WRITE(5,902) IAM()

        WRITE(5,901) IAM(),'WINRPT  '

        CALL RUNTSK(8HWINRPT  )

	CALL PRMYESNO(' Do you want to do file merge (Y/N) ',FLAG)

	IF (FLAG .NE. 1) CALL GSTOP(GEXIT_OPABORT)

C---- Make sure vlf2vlc is done

710     CONTINUE

        CALL XWAIT(2,2,STATUS)

        CALL STTSK(8HVLF2VLC ,TSKSTS,STATUS)

        IF (STATUS .NE. 4) THEN
           GOTO 710
        END IF

C---- Load and start vlf merge program.

	WRITE(5,901) IAM(),'MRGVLF  '

	CALL RUNTSK(8HMRGVLF  )

	CALL PRMYESNO('Did MRGVLF run ok [Y/N]? ',FLAG)

	IF (FLAG .NE. 1) THEN
	  WRITE(5,904) IAM(),(SFNAMES(K,VLC),K=1,5)
	  CALL GPAUSE
	END IF

	CALL FMAINT(VLF,VLC,ST)

	WRITE(5,905) IAM()

C---- Load and start report programs

	WRITE(5,901) IAM(),'BKKREP  '

	CALL RUNTSK(8HBKKREP  )

	CALL GSTOP(GEXIT_SUCCESS)


C------------------------- Format Statements --------------------------

900	FORMAT(1X,A,' Mount drawing packs and continue DWINTSK ')

901	FORMAT(1X,A,' Begining execution of ',A8)

902	FORMAT(1X,A,' Super Double winner selection complete ')

903	FORMAT(1X,A,' Error while allocating ',5A4)

904	FORMAT(1X,A,' Initialize ',5A4,' and rerun MRGVLF')

905	FORMAT(1X,A,' Validation file merge complete')


	END
