C
C SUBROUTINE X2DLLNAME
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2DLLNAME.FOV                                $
C  $Date::   17 Apr 1996 16:15:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2dllname.for;1 **
C
C X2DLLNAME.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This function will return a text description for a
C Data Link command received from LANPRO.
C
C Input parameters:
C
C	PARAM	    Integer*4	    DLL command number
C	TXTNAME	    Character*40    Text description
C	STATUS	    Integer*4	    Return status code
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2DLLNAME(PARAM,TXTNAME,STATUS)
	IMPLICIT NONE 
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4	PARAM		!DLL command from LANPRO
	INTEGER*4	STATUS		!Status of call
	CHARACTER*40    TXTNAME		!Text name returned
	CHARACTER*1	BELL /'07'X/	!Ring bell
C
	STATUS=0
	IF(PARAM.EQ.CCONREQ) THEN
	  TXTNAME = 'Activate Connection Request'
	ELSE IF(PARAM.EQ.CDISREQ) THEN
	  TXTNAME = 'Deactivate Connection Request'
	ELSE IF(PARAM.EQ.CCONNIND) THEN
	  TXTNAME = 'Activate Connection Indication'
	ELSE IF(PARAM.EQ.CDISIND) THEN 
	  TXTNAME = 'Deactivate Connection Indication'//BELL//BELL
	ELSE IF (PARAM.EQ.CACTETH) THEN
	  TXTNAME = 'Activate Ethernet'
	ELSE IF (PARAM.EQ.CDISETH) THEN
	  TXTNAME = 'Deactivate Ethernet'
	ELSE IF (PARAM.EQ.COPEN) THEN
	  TXTNAME = 'Open SAP command'
	ELSE IF (PARAM.EQ.CCLOSE) THEN
	  TXTNAME = 'Close SAP command'//BELL//BELL
	ELSE IF (PARAM.EQ.CACTSTA) THEN
	  TXTNAME = 'Activate station command'
	ELSE IF (PARAM.EQ.CDISSTA) THEN
	  TXTNAME = 'Deactivate station command'
	ELSE IF (PARAM.EQ.CGOLAN) THEN
	  TXTNAME = 'Activate LAN command'
	ELSE IF (PARAM.EQ.CSTOPLAN) THEN
	  TXTNAME = 'Stop LAN command'
	ELSE
	  STATUS=-1
	ENDIF
C
	RETURN
	END
