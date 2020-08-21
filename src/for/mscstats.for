C
C *** PROGRAM MSCSTATS ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSCSTATS.FOV                                 $
C  $Date::   17 Apr 1996 14:06:42                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - mscstats.for ***
C
C V03 07-MAR-95 SCD USE SYSID VECTOR TO DISPLAY LAT_NAMES FOR ACTUAL 
C		    NUMBER OF SYSTEMS (DC).
C V02 08-DEC-92 RRB NO MORE READ/WRITE QUEUES
C V01 22-JAN-91 RRB VAX INITIAL RELEASE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	PROGRAM MSCSTATS
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:MSCCOM.DEF'
C
	INTEGER*4 I, QUECNT, ST
C
	CHARACTER*5 HOM
	CHARACTER*4 SYSID(MAX_LAT_PORTS)/'SYSA','SYSB',		! V03
     *                                 'SYSC','SYSD','SYSE'/	! V03
	CHARACTER*3 CLS
C
	CLS(1:1) = CHAR(155)
	CLS(2:3) = '2J'
C
	HOM(1:1) = CHAR(155)
	HOM(2:5) = '1;1H'
C
	TYPE*,CLS,IAM()
100	CONTINUE
	TYPE*,HOM,IAM()
	TYPE*,IAM(),'MSCSTS       = ',MSCSTS
	TYPE*,IAM(),'FREE BUFFERS = ',QUECNT(MSCFREE)
	TYPE*,IAM(),'EXEC BUFFERS = ',QUECNT(MSCEXEC)
	TYPE*,IAM(),'TRAP COUNTS  = ',(MSC_TIMCNT(I),I=1,MSC_NUM_TIMTRAPS)
	TYPE*,IAM(),'LAST SENT TM = ',MSC_LAST_SENT_TIME
	TYPE*,IAM(),'LAST RECV TM = ',MSC_LAST_RECV_TIME
	TYPE*,IAM(),'READ IN PROG = ',MSC_READ_IN_PROG
	TYPE*,IAM(),'WRIT IN PROG = ',MSC_WRITE_IN_PROG
	TYPE*,IAM(),'SWITCH CNT   = ',MSC_CONF_INFO(SWITCH_CNT)
C
	DO 200 I = 0, MSC_CONF_INFO(SWITCH_CNT) - 1
	  TYPE*,IAM(),'SWITCH NAME  = ',CMSC_CONF_INFO(SWITCH_NAME + I)
200	CONTINUE
C
	TYPE*,IAM(),'USER ID      = ',CMSC_CONF_INFO(USER_ID)
	TYPE*,IAM(),'PASSWORD     = ',CMSC_CONF_INFO(PASSWORD)
	TYPE*,IAM(),'NET_PORT_ID  = ',CMSC_CONF_INFO(NET_PORT_ID)
	TYPE*,IAM(),'LOC_PORT_ID  = ',CMSC_CONF_INFO(LOC_PORT_ID)
	TYPE*,IAM(),'AUX_PORT_CNT = ',MSC_CONF_INFO(AUX_PORT_CNT)

	DO 300 I = 1,NUMSYS					!V03
	   TYPE*,IAM(),SYSID(I),' PORT    = ',LAT_NAME(I)	!V03
  300	CONTINUE						!V03

	CALL XWAIT(250, 1, ST)
	GOTO 100
C
	END
