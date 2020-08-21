C
C PROGRAM MULTITCF
C
C V07 20-OCT-2000 UXN ALPHA BASELINE RELEASE.
C V06 10-APR-2000 UXN VKSBLD moved to MULTIWIN.
C V06 08-FEB-2000 UXN POOLBLD removed.
C V05 28-JAN-2000 OXK GIND=1 hardcoding remover for Sport (Vakio changes)
C V04 27-DEC-1999 OXK Call to NBRTCW added.
C V04 13-DEC-1999 OXK MULTIWIN changes.
C V03 21-JUL-1999 UXN Start VKSBLD only after VAKIO WINSEL and JPSBLD olny
C                     after JOKERI WINSEL.
C V02 29-APR-1999 RXK Status WMRG added, check of run as subprocess.
C V01 11-JAN-1999 GLS INITIAL RELEASE FOR FINLAND
C
C MAIN CONTROL PROGRAM FOR TCF MULTI-FILE MERGE (SUBRUN)
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
        PROGRAM MULTITCF
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
C
        INTEGER*4 K, FLAG, ST, I, II
        INTEGER*4 NBRTSK                !winner selection task # in multiwinsel
        INTEGER*4 NBRTCW                !TCW # in multiwinsel
	EXTERNAL NBRTSK,NBRTCW

        LOGICAL*4 ISSUB
C
        CALL COPYRITE
C
C Check that program was subrun
C
        ISSUB = ISSUBPROC()
        IF(.NOT.ISSUB) THEN
         CALL PRMYESNO('Are you sure you want run (not SUBRUN) MULTITCF [Y/N]?',
     *                 FLAG)
         IF(FLAG.NE.1) CALL GSTOP(GEXIT_OPABORT)
        ENDIF

	CALL GETSCONF(SCFREC,ST)
        IF (ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
        II=0
        DO I=1,MAXWTSK                        !FIND # OF MRGTCF TODAY
          IF(TCWNBR(I).NE.0) II=II+1
        ENDDO
C
        IF(II.EQ.0) THEN
          TYPE *
          TYPE *,IAM(),'No TCF multi-file merge today'
          TYPE *
          TCCSTS = WMRG
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF

        CALL PRMYESNO(
     *     'Are you sure you want TCF multi-file merge [Y/N]? ',FLAG)
        IF(FLAG.NE.1) CALL GSTOP(GEXIT_SUCCESS)
C
C CHECK FILES
C
        IF(SCFSFN(1,TCF).NE.SCFSFN(1,TCC)) THEN
          TYPE*,'Carryover copy file must have same volume as tcf'
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
        IF(SCFFSZ(VLC).NE.SCFFSZ(VLF)) THEN
          TYPE*,'Validation copy file must be same size as vlf'
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
C
C
        IF(TCCSTS.NE.WCLR) THEN
          WRITE(6,905) IAM(),(SCFSFN(K,TCC),K=1,5)
          CALL GPAUSE
        ENDIF
C
        TCCSTS=WUSE
C 
        DO I=1,MAXWTSK
          TSKTCFSTS(I)=0
        ENDDO
C
C START TCF MULTI-FILE MERGE PROGRAM
C
        WRITE(6,901) IAM(),'MLMRGTCF'
        CALL RUNTSK(8HMLMRGTCF)
C
        CALL PRMYESNO('Did MLMRGTCF run ok [Y/N]? ',FLAG)
        IF(FLAG.NE.1) THEN
          WRITE(6,903) IAM(),(SCFSFN(K,TCC),K=1,5)
          CALL GSTOP(GEXIT_OPABORT)
        ELSE
          CALL FMAINT(TCF,TCC,ST)
          IF(ST.EQ.0) WRITE(6,902) IAM()
        ENDIF
C
C RUN REPORTING PROGRAMS
C
        WRITE(6,901) IAM(),'TCFSUM  '
        CALL RUNTSK(8HTCFSUM  )
C
        WRITE(6,907) IAM()
C
        DO I=1,WINCNT
            IF (DRWSTS(MLWININD,I).EQ.WINSOK) THEN
	        K=NBRTCW(I)
	        IF (K.NE.0) TCWSTS(K)=WMRG
	     ENDIF
        ENDDO

        TCCSTS = WMRG
        CALL GSTOP(GEXIT_SUCCESS)
C
901     FORMAT(1X,A,'Begining execution of ',A8)
902     FORMAT(1X,A,'TCF multi-file merge complete')
903     FORMAT(1X,A,'Initialize ',5A4,' and rerun MULTITCF')
905     FORMAT(1X,A,'Clear ',5A4,' wait until finished, type CONT') 
907     FORMAT(1X,A,'MULTITCF complete') 
        END
