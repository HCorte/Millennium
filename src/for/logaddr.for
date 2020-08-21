C
C SUBROUTINE LOGADDR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFIP:[GOLS]LOGADDR.FOV                                  $
C  $Date::   18 Mar 1997 13:24:02                                         $
C  $Revision::   1.1                                                      $
C  $Author::   RXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2decode.for;1 **
C
C LOGADDR
C
C V02 12-DEC-94 GPR Integrate UK changes into X2X Baseline
C V01 12-NOV-92 MF Original
C
C CHECK ADDRES LENGTH AND MOVE ADDRESS BYTES INTO TRABUF FIELDS
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
        SUBROUTINE LOGADDR(ADRLEN,MESSAGE,M_OFF,TRABUF,T_OFF)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'					  !V02
        INCLUDE 'INCLIB:SYSEXTRN.DEF'					  !V02
        INCLUDE 'INCLIB:GLOBAL.DEF'					  !V02
        INCLUDE 'INCLIB:X2XCOM.DEF'					  !V02
C
	INTEGER*4 ADRLEN,MESSAGE(*),M_OFF,T_OFF       ! Input
	INTEGER*4 TRABUF(*)                           ! Output
	INTEGER*4 CHKVAL                              ! Function
	INTEGER*4 ALEN, I                             ! Work 
C
	ALEN = (ADRLEN+1)/2
C
C	***** Start V02 changes *****
C
        IF (IAND(X2X_DEBUG,1024).NE.0)
     *        TYPE *,'ADRLEN, M_OFF ',ADRLEN,M_OFF,ALEN
        IF (IAND(X2X_DEBUG,1024).NE.0)
     *        TYPE 9000,(MESSAGE(I),I=1,7)
9000    FORMAT(1H ,7(Z8.8,1X))
C
C	***** End V02 changes *****
C
	IF(CHKVAL(ADRLEN,0,16,' X2XADRLEN').EQ.0) THEN
	  CALL MOVBYT(MESSAGE,M_OFF,TRABUF(T_OFF),1,ALEN)
        ELSE		
          DO 1 I=1,8                ! CLEAR  TXFDAD1,2 OR TXFALT1,2
            TRABUF(T_OFF+I-1)=0   
1	  CONTINUE
        ENDIF
C
        RETURN  
        END
