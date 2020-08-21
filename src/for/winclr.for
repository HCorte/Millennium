C
C PROGRAM WINCLR
C
C V03 07-JAN-2000 OXK Only VLWs in use are cleared (TSKCNT.NE.0)
C V02 14-SEP-1999 UXN Clearing VLC, TCC separately added.
C V01 25-JAN-1999 GPW Clearing of VLC, TCC, VLWnn and TCWnn files
C
C WINNER SELECTION CLEARING FILES PROGRAM
C
C TO CLEAR VLC.FIL, TCC.FIL, VLW01.FIL, VLW02.FIL,...,TCW01.FIL, TCW02.FIL,...
C TO BE RUN ANY TIME AFTER RUNSYS AND BEFORE WINSEL WHENEVER SYSTEM IS NOT BUSY
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
C
	PROGRAM WINCLR
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
C
	INTEGER*4 K, ST, I,OPT,EXT
        CHARACTER * 20 FILNAM
C
	CALL COPYRITE
C

   10   CONTINUE
        TYPE*,' '
        WRITE(*,2000)
        TYPE*,' '
        CALL INPNUM('Enter option #',OPT,1,6,EXT)
        IF(EXT.EQ.-1) CALL GSTOP(GEXIT_SUCCESS)
C
        GO TO (100,200,300,400,500,600) , OPT
  100   CONTINUE
  200   CONTINUE
  400   CONTINUE
C
C CLEAR VLC
C
           VLCSTS=WUSE
           CALL RUNTSK(8HCRTVLC  )
           VLCSTS=WCLR
C
        IF(OPT.EQ.2) GO TO 10
C
 300	CONTINUE
C
C CLEAR TCC
C
           TCCSTS=WUSE
           CALL RUNTSK(8HCRTTCC  )
           TCCSTS=WCLR
C
        IF(OPT.EQ.3) GO TO 10
        IF(OPT.EQ.4) GO TO 10	
C
 500    CONTINUE
C
C CLEAR VLWnn FILES
C
            DO I=1,MAXMVLF
              VLWSTS(I)=WUSE
            ENDDO
C

            DO 510 I=1,MAXMVLF
	       IF (TSKCNT(I).EQ.0) GOTO 510                         
               FILNAM(1:20)=' '
               WRITE(FILNAM,1000) (VLWNAM(K,I),K=1,5)
	       CALL SUBCLRFIL(1,FILNAM,ST)
	       IF(ST.NE.0) THEN
                   WRITE(5,903) IAM(),(VLWNAM(K,I),K=1,5)
                   CALL GPAUSE
               ELSE
                   VLWSTS(I)=WCLR
               ENDIF
510	    CONTINUE
C
        IF(OPT.EQ.5) GO TO 10
C
  600   CONTINUE
C
C CLEAR TCWnn FILES
C
             DO I=1,MAXMTCF
                TCWSTS(I)=WUSE
             ENDDO
C
             DO I=1,MAXMTCF
	         FILNAM(1:20)=' '
                 WRITE(FILNAM,1000) (TCWNAM(K,I),K=1,5)
                 CALL SUBCLRFIL(1,FILNAM,ST)
	         IF(ST.NE.0) THEN
	              WRITE(5,903) IAM(),(TCWNAM(K,I),K=1,5)
	              CALL GPAUSE
                 ELSE
                      TCWSTS(I)=WCLR
	         ENDIF
             ENDDO
C
        IF(OPT.EQ.6) GO TO 10
C
	CALL GSTOP(GEXIT_SUCCESS)
C
C
903	FORMAT(1X,A,' Error while clearing ',5A4)
1000    FORMAT(5A4)
2000    FORMAT(1X,'  1 - Clearing all files'/
     *         1X,'  2 - Clearing VLC file'/
     *         1X,'  3 - Clearing TCC file'/
     *         1X,'  4 - Clearing VLC & TCC files'/
     *         1X,'  5 - Clearing VLWnn files'/
     *         1X,'  6 - Clearing TCWnn files')
	END


