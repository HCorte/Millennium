C
C  GXSRC:X2STSPRO.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2STSPRO.FOV                                 $
C  $Date::   17 Apr 1996 16:37:32                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C X2STSPRO.FTN  -  BCST STATISTICS REPORT PROCESSING
C
C V03 12-DEC-94 DAS Integrate UK changes into X2X Baseline
C V02 19-JUL-94 WS MULTINETWORK CHANGES
C V01  9-MAR-94 JWE Port from Sweden 
C
C V06 18-JAN-93 WS ADDED NO OF TIMES BCST STATUS CHANGED
C V05 14-JUN-92 JAN TOOK OUT SOME CONSOLE WARNINGS
C V04 07-FEB-92 MF ORIGINAL
C
C This subroutine updates counts based on the BCST statistics
C reports which have been sent by the terminal.
C
C Calling Sequence:
C
C     CALL X2STSPRO(STN,NUM_BCST,BITS,LEN1,ADDR1,LEN2,ADDR2)
C
C Input parameters:
C
C     STN                  Int*4    Station number
C     NUM_BCST             Int*4    Num reported BCST connections
C     BITS                 Int*4    BCST connections bit-flags (stn)
C     LEN1,LEN2            Int*4    DTE addr LEN of stn conn
C     ADDR1(2),ADDR2(2)    Int*4(2) DTE addr of stn connection 1 and 2
C
C Output (updated X2XCOM variables):
C
C     X2X_BCST_ACTIVE_CNT  Int*4(2) Count of BCST1/BCST2 connections
C     X2XS_BCST_ACTIVE_BITMAP(STN)  BCST connections bit-flags (host)
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
C Copyright 1992, 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE X2STSPRO(STN,NUM_BCST,BITS,LEN1,ADDR1,LEN2,ADDR2)
      IMPLICIT NONE
C
	INCLUDE	'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE	'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C

      INTEGER*4   STN                  !Station number
      INTEGER*4   NUM_BCST             !Num reported BCST connections
      INTEGER*4   BITS                 !reported BCST connections bit-flags
      INTEGER*4   LEN1,LEN2            !DTE addr LEN of stn conn
      INTEGER*4   ADDR1(2),ADDR2(2)    !DTE addr of stn connection 1 and 2
C
      INTEGER*4   STN_BCST_PORT_IDX1/0/!conn1: BCST STN port number if ENABLE
      INTEGER*4   STN_BCST_PORT_IDX2/0/
      INTEGER*4   BCST_IDX1/0/         !conn1: which BCST
      INTEGER*4   BCST_IDX2/0/
      INTEGER*4   CHANGED_BITS         !BCSt connections bit-flags
      INTEGER*4	  CLASS		       !STATION CLASS -V02
      INTEGER*4   PORT1, PORT2	       !V02
C
        CHANGED_BITS = IEOR(BITS,X2XS_BCST_ACTIVE_BITMAP(STN)) !changed sts
C**************** TEST PHASE WARNINGS *******************************
C
C       Check HOST and STN BCST conf errors and warnings
C
	CLASS = X2XS_STNCLS(STN)			!V02
	IF (CLASS.LE.0) RETURN				!V02

	PORT1=X2XC_BCST_NET_PORT1(CLASS)		!V02
	PORT2=X2XC_BCST_NET_PORT2(CLASS)		!V02
	IF (PORT1.LE.0) PORT1=X2X_NETWORK_PORTS
	IF (PORT2.LE.0) PORT2=X2X_NETWORK_PORTS

        IF( NUM_BCST .EQ. 0) THEN                       !chg and not cfg
            CALL OPS('ERR-no BCSTs, STN', STN,0)
        ELSEIF( NUM_BCST .GT. 2) THEN                   !too many conn
            CALL OPS('WARN-TOO MANY BCSTs, STN', STN,0)
            NUM_BCST = 2                                !clamp to max
        ENDIF
C
        IF (CHANGED_BITS .EQ.0 ) THEN                   !ok if periodic
C***            CALL OPS('WARN-NO BCST BITS CHANGE, STN', STN,0)
        ELSEIF( ishft(ISHFT(BITS,-NUM_BCST),num_bcst) .NE. 0) THEN !xtra bits
            CALL OPS('WARN-UNEXPECTED BCST BITS, STN', STN,0)
        ENDIF
C
C
        IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *   TYPE *,'BCST PRO: STN, NUM,CHG_BITS: ',
     *   STN,NUM_BCST,CHANGED_BITS
C
C       Process BCST connection status report
C
C       Connection 1
C
        IF (NUM_BCST .GE. 1 .AND.
     * IAND(CHANGED_BITS,X2STMES_STSBCST_CONN1_MASK) .NE. 0) THEN   ! chged
          CALL X2PNCMP(LEN1,ADDR1,STN,STN_BCST_PORT_IDX1 ) !STN port/enable
          CALL X2BCSTX(STN_BCST_PORT_IDX1,STN,BCST_IDX1)  !which BCST
          IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *    TYPE *,'BCST1 PRO: STN, PIDX, BIDX: ',
     *                  STN, STN_BCST_PORT_IDX1, BCST_IDX1
          IF ( BCST_IDX1 .GT. 0) THEN                     !valid BCST
            IF (IAND(BITS,X2STMES_STSBCST_CONN1_MASK) .NE.0) THEN !conn1 up
              X2XPN_BCST_ACTIVE_CNT(PORT1) =			 !V02
     *		  X2XPN_BCST_ACTIVE_CNT(PORT1) + 1		 !V02
	      X2XPN_BCST_STATUS_CHANGES(PORT1) =		 !V02
     *		  X2XPN_BCST_STATUS_CHANGES(PORT1) +1		 !V02
C***          CALL OPS('BCST CONN. UP FROM STN ',STN,BCST_IDX1)  ! TEST only
            ELSE                                               ! conn1 down
              X2XPN_BCST_ACTIVE_CNT(PORT1) =			 !V02
     *		  X2XPN_BCST_ACTIVE_CNT(PORT1) - 1		 !V02
	      X2XPN_BCST_STATUS_CHANGES(PORT1) =		 !V02
     *		  X2XPN_BCST_STATUS_CHANGES(PORT1) +1		 !V02
C***          CALL OPS('BCST CONN. DOWN FROM STN ',STN,BCST_IDX1)! TEST only
            ENDIF
          ENDIF
        ENDIF
C
C       Connection 2 - ignore BCST_IDX1 .eq. BCST_IDX2
C                      (i.e. ignore same stn port idx or net port type)
C
        IF (NUM_BCST .GT. 1 .AND.
     * IAND(CHANGED_BITS,X2STMES_STSBCST_CONN2_MASK) .NE. 0) THEN ! chged conn2
          CALL X2PNCMP(LEN2,ADDR2,STN,STN_BCST_PORT_IDX2 )      ! STN port
          CALL X2BCSTX(STN_BCST_PORT_IDX2,STN,BCST_IDX2)       ! BCST
          IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *    TYPE *,'BCST2 PRO: STN, PIDX, BIDX: ',
     *                  STN, STN_BCST_PORT_IDX2, BCST_IDX2
          IF ( BCST_IDX2 .GT. 0 ) THEN
            IF (BCST_IDX1 .EQ. BCST_IDX2  ) THEN           ! *** TEST ONLY ***
              CALL OPS('WARN-Two conn to a BCST,STN ',STN,BCST_IDX1)! TEST only
            ELSEIF (IAND(BITS,X2STMES_STSBCST_CONN2_MASK) .NE. 0) THEN !con2 up
              X2XPN_BCST_ACTIVE_CNT(PORT2) =			 !V02
     *		  X2XPN_BCST_ACTIVE_CNT(PORT2) + 1		 !V02
	      X2XPN_BCST_STATUS_CHANGES(PORT2) =		 !V02
     *		  X2XPN_BCST_STATUS_CHANGES(PORT2) +1		 !V02
C***          CALL OPS('BCST CONN. UP FROM STN ',STN,BCST_IDX2)  ! TEST only
            ELSE                                            ! conn. down
              X2XPN_BCST_ACTIVE_CNT(PORT2) =			 !V02
     *		  X2XPN_BCST_ACTIVE_CNT(PORT2) - 1		 !V02
	      X2XPN_BCST_STATUS_CHANGES(PORT2) =		 !V02
     *		  X2XPN_BCST_STATUS_CHANGES(PORT2) +1		 !V02
C***          CALL OPS('BCST CONN. DOWN FROM STN ',STN,BCST_IDX2)! TEST only
            ENDIF
          ENDIF
        ENDIF
C
C****   X2XPN_CONNECTION_CNT(X2XS_BCST_NET_PORT1(STN)) !NOT USED (FUTURE ???)
C
        X2XS_BCST_ACTIVE_BITMAP(STN) = BITS           ! update stn bitmap
****   IF (CHANGED_BITS.NE.0) X2XS_BCST_STATUS_CHANGES(STN)=
**** *                X2XS_BCST_STATUS_CHANGES(STN)+1
       X2XS_BCST_STATUS_CHANGES(STN)=
     *                X2XS_BCST_STATUS_CHANGES(STN)+1
        IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *   TYPE *,'BCST PRO RETURN: STN, NUM,BITS,#ACT: ',
     *                  STN, NUM_BCST,BITS,
     *      X2XPN_BCST_ACTIVE_CNT(PORT1),X2XPN_BCST_ACTIVE_CNT(PORT2)	!V02
C
C ROUTINE EXIT.
C
      RETURN
      END
