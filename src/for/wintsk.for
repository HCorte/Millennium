C
C PROGRAM WINTSK
C
C V10 03-DEC-2000 UXN JPSBLD removed.
C V09 14-DEC-1999 OXK MULTIWIN changes.
C V08 09-SEP-1998 RXK Call of JPSBLD added
C V07 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C V06 13 Dec 1994 PXB Added VKSBLD (Vakio Build) program.
C V05 30 Jan 1994 HXK call VLF2VLC to speed up winsel
C V04 02 Sep 1993 HXK Call MRGTSK for VLF, TCF file merges.
C V03 21 Jan 1993 DAB Initial Release
C  		      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  		      DEC Baseline
C V02 12-NOV-91 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C WINNER SELECTION PROCEDURE CONTROL TASK
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT


	PROGRAM WINTSK
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'

	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'

	INTEGER*4 K, FLAG, ST
        INTEGER*4 STATUS,TSKSTS

	LOGICAL VAKIO_STATS_BUILD
	REAL*8  UPDLST(2)

	DATA UPDLST /'VKSBLD  ','        '/
C
C
	CALL COPYRITE
C
        IF (STOPMOD.NE.WINMANUAL) THEN
           TYPE*,IAM(),'Use MULTIWIN instead.'
           CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
        CALL GETSCONF(SCFREC,ST)
        IF (ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
C
	CALL PRMYESNO('Are you sure you want winner selection [Y/N]? ',FLAG)
	IF(FLAG.NE.1) CALL GSTOP(GEXIT_SUCCESS)
C
C COPY VLF TO VLC
C
        TYPE*,IAM(),'Running VLF2VLC to copy VLF.FIL to VLC.FIL'
        CALL NRUNTSK(8HVLF2VLC )
C
C CLEAR TCC
C
        TYPE*,IAM(),'Running CRTTCC to allocate and clear TCC.FIL'
        CALL NRUNTSK(8HCRTTCC  )
C
C CLEAR TCC
C
        TYPE*,IAM(),'Running CRTTCW to allocate and clear TCW.FIL'
        CALL NRUNTSK(8HCRTTCW  )
C
C CREATE NEW FILES
C
	CALL CRTFIL(SCFSFN(1,VLW),SCFFSZ(VLW),ST)
	IF(ST.NE.0) THEN
	  WRITE(6,903) IAM(),(SCFSFN(K,VLW),K=1,5)
	  CALL GPAUSE
	ENDIF
C
C***	CALL CRTFIL(SCFSFN(1,TCW),SCFFSZ(TCW),ST)
C***	IF(ST.NE.0) THEN
C***	  WRITE(6,903) IAM(),(SCFSFN(K,TCW),K=1,5)
C***	  CALL GPAUSE
C***	ENDIF
C

C
C MAKE SURE CRTTCW IS DONE
C
100     CONTINUE
        CALL XWAIT(2,2,STATUS)
        CALL STTSK(8HCRTTCW  ,TSKSTS,STATUS)
        IF (STATUS.NE.4) THEN
           GOTO 100
        ENDIF
C
C
C MAKE SURE DRAWING PACK IS MOUNTED
C
	CALL PRMYESNO('Are the drawing packs mounted [Y/N]? ',FLAG)
	IF(FLAG.NE.1) THEN
	  WRITE(6,900) IAM()
	  CALL GPAUSE
	ENDIF
C
C LOAD AND START WINSEL TASK
C
	WRITE(6,901) IAM(),'WINSEL  '
	CALL RUNTSK(8HWINSEL  )
C
C CREATE WINRPT
C
	WRITE(6,901) IAM(),'WINRPT  '
	CALL RUNTSK(8HWINRPT  )
C
C
	WRITE(6,902)
C
C MAKE SURE VLF2VLC IS DONE
C
200     CONTINUE
        CALL XWAIT(2,2,STATUS)
        CALL STTSK(8HVLF2VLC ,TSKSTS,STATUS)
        IF (STATUS.NE.4) THEN
           GOTO 200
        ENDIF
C
C MAKE SURE CRTTCC IS DONE
C
300     CONTINUE
        CALL XWAIT(2,2,STATUS)
        CALL STTSK(8HCRTTCC  ,TSKSTS,STATUS)
        IF (STATUS.NE.4) THEN
           GOTO 300
        ENDIF
C
C
	CALL PRMYESNO(
     *     'Do you want to run Validation and Carryover file merges (Y/N)? ',
     *     FLAG)
	IF(FLAG.NE.1) CALL GSTOP(GEXIT_SUCCESS)

        WRITE(6,901) 'MRGTSK  '
        CALL RUNTSK(8HMRGTSK  )

        VAKIO_STATS_BUILD=.FALSE.

        IF (SPTDAT(CURDRW,1) .EQ. DAYCDC) VAKIO_STATS_BUILD = .TRUE.
C
        IF (VAKIO_STATS_BUILD) THEN
          WRITE(6,901) UPDLST(1)
          CALL RUNTSK(UPDLST(1))
          WRITE(6,907)
        END IF
C
	CALL GSTOP(GEXIT_SUCCESS)
C
C
900	FORMAT(1X,A,' Mount drawing packs and continue WINTSK ')

901	FORMAT(1X,A,' Begining execution of ',A8)

902	FORMAT(1X,A,' Winner selection complete ')

903	FORMAT(1X,A,' Error while allocating ',5A4)

907     FORMAT(1X,A,' Building Vakio Stats for next draw complete')



	END


