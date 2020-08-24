C
C SUBROUTINE X2CLRCON.FOR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CLRCON.FOV                                 $
C  $Date::   17 Apr 1996 16:13:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C     X2CLRCON.FTN
C     ____________
C
C V03 05-DEC-94 SCD Integrate UK changes into X2X Baseline
C V02 16-FEB-94 JWE Port from Concurrent
C
C     BUILD THE MESSAGE TO CLEAR CONNECTION
C
C     CALL X2CLRCON(STATION_NO,CONN_ID,SAP)
C     IN:
C     STATION_NO
C     CONN_ID                  CONNECTION ID THAT SHOULD BE SET
C                              IN THE MESSAGE
C     SAP                      SAP FOR THIS CONNECTION
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE X2CLRCON(STATION_NO,CONN_ID,SAP)
      IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
C
C
      INTEGER*4      STATION_NO
      INTEGER*4      CONN_ID
      INTEGER*4      SAP
C
      INTEGER*4      BUF, LENGTH
C
C     TRY TO GET A BUFFER IF CANNOT GET ONE EXIT
C
      CALL GETBUF(BUF)
      IF (BUF.LE.0)  RETURN
C
C     FILL SAP NO
C
C     ***** Start V03 changes *****
      IF (X2X_I4_STATION) THEN
      	  PRO(LINENO,BUF)=SAP
      ELSE
      	  HPRO(LINENO,BUF)=SAP
      ENDIF
C     ***** End V03 changes *****
C
C     FILL THE BUFFER WITH DATA
C
      CALL X2DISCFE(STATION_NO,CONN_ID,PRO(INPTAB,BUF),LENGTH)
C
C     SET BUFFER LENGTH
C
      HPRO(INPLEN,BUF)=LENGTH
      HPRO(X2X_DEST,BUF)=X2DEST_FE
C
      CALL X2ADDPRO(BUF)
C
      RETURN
      END
