C
C FUNCTION X2MAP_CONN_TYPE.FOR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2MAP_CONN_TYPE.FOV                          $
C  $Date::   17 Apr 1996 16:23:14                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
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
C This function maps the many connection types to the 5 network port
C types.  For example, X2XSCT_ASYNC should use a X2XPT_ASYNC network
C port.
C
	INTEGER*4 FUNCTION X2MAP_CONN_TYPE(CONN_TYPE)
	IMPLICIT NONE
C
	INCLUDE	'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
	INCLUDE	'INCLIB:X2XCOM.DEF'
C
	INTEGER*4   CONN_TYPE
C
	IF(CONN_TYPE .EQ. X2XSCT_X21SWC) THEN
	    X2MAP_CONN_TYPE = X2XPT_X21
	ELSE IF(CONN_TYPE .EQ. X2XSCT_X25SVC .OR.
	1   CONN_TYPE .EQ. X2XSCT_X28PAD .OR.
	2   CONN_TYPE .EQ. X2XSCT_X25FSL .OR.
	3   CONN_TYPE .EQ. X2XSCT_X25_DIAL .OR.
	4   CONN_TYPE .EQ. X2XSCT_X25_X32) THEN
	    X2MAP_CONN_TYPE = X2XPT_X25
	ELSE IF(CONN_TYPE .EQ. X2XSCT_ASYPVC) THEN
	    X2MAP_CONN_TYPE = X2XPT_ASYNC
	ELSE IF(CONN_TYPE .EQ. X2XSCT_GTECH_DIAL) THEN
	    X2MAP_CONN_TYPE = X2XPT_DIALUP
	ELSE
	    X2MAP_CONN_TYPE = 0
	ENDIF
	RETURN
	END
