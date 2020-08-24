C PROGRAM UPDTSK
C
C V07 30-MAY-2000 OXK UPDTSK_RUN_AGAIN set to 1 when problems w/ WINUPD
C V06 11-APR-2000 UXN UPDTSK_RUN_AGAIN added.
C V05 27-APR-1999 RXK Call of WIMG/YESNO replaced with call of PRMYESNO.
C V04 02-SEP-1993 HXK Removed superfluous report again.
C V03 10-MAY-1993 SXH CHANGED NAME FROM SHRTSK TO UPDTSK
C V02 12-NOV-1991 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C
C UPDTSK.FOR
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM UPDTSK
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
C
	INTEGER*4     K,FLAG, ST
C
C
	CALL COPYRITE
C
	CALL PRMYESNO('Are you sure you want share update [Y/N]? ',FLAG)
	IF(FLAG.NE.1) CALL GSTOP(GEXIT_SUCCESS)
	CALL PRMYESNO('Do you want to change/enter share values (Y/N) ',FLAG)
	IF(FLAG.NE.1) GOTO 100
C
C LOAD AND START SHARE ENTRY PROGRAM
C
	WRITE(6,901) IAM(),'SHAREUPD '
	CALL RUNTSK(8HSHAREUPD)

C	WRITE(6,901) IAM(),'WINRPT'      !commented out for Finland
C	CALL RUNTSK(8HWINRPT  )

	TYPE*,IAM(),' Share update complete'
C
C
	TYPE*,IAM(),' When Ready to post prizes to the VLF,',
     *		    ' please continue '
	CALL GPAUSE
C
C
100	CONTINUE
C
	UPDTSK_RUN_AGAIN = 0
C
	CALL PRMYESNO('Do you want to post prize values to the VLF (Y/N) ',
     *       FLAG)
	IF(FLAG.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
C CREATE VLF COPY FILE
C
	CALL CRTFIL(SFNAMES(1,VLC),SFSIZES(VLC),ST)
	IF(ST.NE.0) THEN
	  WRITE(6,900) IAM(),(SFNAMES(K,VLC),K=1,5)
	  CALL GPAUSE
	ENDIF
C
C LOAD AND START WINUPD TASK
C
	WRITE(6,901) IAM(),'WINUPD  '
	CALL RUNTSK(8HWINUPD  )
C
C RENAME FILES
C
	CALL PRMYESNO('Did WINUPD run ok [Y/N]? ',FLAG)
	IF(FLAG.NE.1) THEN
	    UPDTSK_RUN_AGAIN = 1
	    CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
	CALL FMAINT(VLF,VLC,ST)
	IF(ST.NE.0) THEN
	  TYPE*,IAM(),'Problem renaming files'
	  CALL GPAUSE
	ENDIF
C
C
	TYPE*,IAM(),' Validation posting complete'
	CALL GSTOP(GEXIT_SUCCESS)
C
C FORMAT AREA
C
901	FORMAT(1X,A,' Begining execution of ',A8)
900	FORMAT(1X,A,' Error while allocating ',4A4)
	END
