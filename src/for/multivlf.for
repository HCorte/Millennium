C
C PROGRAM MULTIVLF
C
C V02 14-DEC-1999 OXK GETSCONF added & general cleaning.
C V01 29-APR-1999 RXK INITIAL RELEASE FOR FINLAND
C
C PROGRAM TO CALL VLF MULTI-FILE MERGE (SUBRUN)
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM MULTIVLF
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'

        INTEGER*4 I, K, FLAG, ST

        LOGICAL*4 ISSUB

	INTEGER*4 NBRTSK
	EXTERNAL  NBRTSK

        CALL COPYRITE

        ISSUB = ISSUBPROC()
        IF(.NOT.ISSUB) THEN
         CALL PRMYESNO('Are you sure you want run (not SUBRUN) MULTIVLF [Y/N]?',
     *                 FLAG)
         IF(FLAG.NE.1) CALL GSTOP(GEXIT_OPABORT)
        ENDIF

	CALL GETSCONF(SCFREC,ST)
	IF (ST.NE.0) CALL GSTOP(GEXIT_FATAL)

        CALL PRMYESNO(
     *     'Are you sure you want VLF multi-file merge [Y/N]? ',FLAG)
        IF(FLAG.NE.1) CALL GSTOP(GEXIT_SUCCESS)
C
C START VLF MULTI-FILE MERGE PROGRAM
C
        WRITE(6,901) IAM(),'MLMRGVLF'
        CALL RUNTSK(8HMLMRGVLF)

        CALL PRMYESNO('Did MLMRGVLF run ok [Y/N]? ',FLAG)
        IF(FLAG.NE.1) THEN
          WRITE(6,903) IAM(),(SCFSFN(K,VLC),K=1,5)
          CALL GSTOP(GEXIT_OPABORT)
        ELSE
          CALL FMAINT(VLF,VLC,ST)
          IF(ST.EQ.0) WRITE(6,907) IAM()
        ENDIF

	DO I=1,MAXGAM
      	    K=NBRTSK(I)
	    IF (DRWSTS(MLWININD,I).EQ.WINSOK) VLWSTS(K)=WMRG
	ENDDO

        VLCSTS = WMRG

        CALL GSTOP(GEXIT_SUCCESS)

901     FORMAT(1X,A,'Begining execution of ',A8)
903     FORMAT(1X,A,'Initialize ',5A4,' and rerun MULTIVLF')
907     FORMAT(1X,A,'VLF multi-file merge complete')

        END
