C
C MSGSCAN.FOR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSGSCAN.FOV                                  $
C  $Date::   17 Apr 1996 14:08:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C V03 12-FEB-96 DAS REMOVED SUBROUTINES INANYL AND INANYR
C V02 19-SEP-95 DAS INCORPORATED LEIPZIG BACKGROUND LOAD CHANGES
C V01 ??-???-91 ??? RELEASE FOR VAX BASELINE.
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
C=======OPTIONS /CHECK=NOOVERFLOW
	INTEGER*4 FUNCTION MSGSCAN(BUF,INDEX)
	IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER BUF(20),LBUF(20)
	INTEGER*4 I4BUF(5), I, INDEX, KEYNUM
	EQUIVALENCE (LBUF,I4BUF)
	REAL*8 KEYS(10)
C
	DATA KEYS/'INIt    ','ENter   ','DIsplay ','DElete  ','Save    ',
     *		  'EDit    ','Load    ','Move    ','DLL     ','Quit    '/
C
C
	DO 10 I=1,20
	LBUF(I)=BUF(I)
10	CONTINUE
C
C
	INDEX=1
	CALL KEY(I4BUF,KEYS,10,INDEX,KEYNUM)
	MSGSCAN=KEYNUM-1
	RETURN
	END
