C GUIPASS_006.FOR
C
C V02 13-NOV-2000 UXN Functions renames to GUIRPC_%%%
C V01 XX-XXX-XXXX XXX Initial release.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This subroutine accepts Path Thru message(s) from GUIRECV routine, 
C and passes them to the appropriate RPC function.
C
C Input parameters:
C
C	BYTE	  MESSAGE(*)	- message
C
C Output parameters:
C
C	INTEGER*4 MES_LEN		- MESSAGE LENGTH
C	INTEGER*4 RET_CODE:
C		   0		-  no error, message accepted;
C		 value >= 11	-  error number to be sent to Client.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUIPASS_006(MESSAGE, MES_LEN, RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
C
	BYTE	    MESSAGE(*)  ! message
	INTEGER*4   MES_LEN, RET_CODE
C
	INTEGER*4 RPC_FUNCTION
C
	RET_CODE = 0
C
C  GET RPC FUNCTION
C
	RPC_FUNCTION = MESSAGE(9)
	GOTO (     10, 20, 30, 40, 50, 60, 70, 80, 90,
     *        100,110,120,130,140,150,160,170,180,190,
     *        200,210,220,230,240,250,260,270,280,290) RPC_FUNCTION
C
C INVALID FUNCTION
C
	GOTO 999
C
C  GET_TERM_SNP
C
10	CONTINUE
	CALL GUIRPC_001(MESSAGE,MES_LEN,RET_CODE)
	RETURN
20	CONTINUE
	CALL GUIRPC_002(MESSAGE,MES_LEN,RET_CODE)
	RETURN
30	CONTINUE
	CALL GUIRPC_003(MESSAGE,MES_LEN,RET_CODE)
	RETURN
40	CONTINUE
	CALL GUIRPC_004(MESSAGE,MES_LEN,RET_CODE)
	RETURN
50	CONTINUE
	GOTO 999
60	CONTINUE
	CALL GUIRPC_006(MESSAGE,MES_LEN,RET_CODE)
	RETURN
70	CONTINUE
80	CONTINUE
90	CONTINUE
100	CONTINUE
110	CONTINUE
120	CONTINUE
130	CONTINUE
140	CONTINUE
150	CONTINUE
	GOTO 999
160	CONTINUE
	CALL GUIRPC_016(MESSAGE,MES_LEN,RET_CODE)
	RETURN
170	CONTINUE
	CALL GUIRPC_017(MESSAGE,MES_LEN,RET_CODE)
	RETURN
180	CONTINUE
	CALL GUIRPC_018(MESSAGE,MES_LEN,RET_CODE)
	RETURN
190     CONTINUE
200     CONTINUE
210     CONTINUE
220     CONTINUE
230	CONTINUE
240	CONTINUE
250	CONTINUE
260	CONTINUE
270	CONTINUE
280	CONTINUE
290	CONTINUE
	GOTO 999
C
999	CONTINUE
	RET_CODE=11
	RETURN
C
	END
