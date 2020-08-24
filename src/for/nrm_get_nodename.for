C 
C Subroutine GET_NODENAME returns the node name and the 
C node name length.
C
C V01 18-AUG-1997 UXN Initial release.
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
	SUBROUTINE GET_NODENAME(NODENAME,	    ! output
     *                          NODENAME_LEN)	    ! output
	IMPLICIT NONE
C
	CHARACTER*15	NODENAME
	INTEGER*2	NODENAME_LEN
C	
	INTEGER*4   SYS$GETSYIW
	EXTERNAL    SYI$_NODENAME
C
	INTEGER*4   ST
C
	STRUCTURE /JPISTRUC/
	  UNION
	   MAP
	    INTEGER*2	BUFLEN
	    INTEGER*2	ITMCOD
	    INTEGER*4	BUFADR
	    INTEGER*4	LENADR
	   END MAP
	   MAP
	    INTEGER*4	END_LIST
	   END MAP
	  END UNION
	END STRUCTURE
C
	RECORD /JPISTRUC/ ITEMLIST(2)
C
	STRUCTURE /IOSBLK/  
	    INTEGER*4	STS,RESERVED
	END STRUCTURE
C
	RECORD /IOSBLK/ IOSB
C
	ITEMLIST(1).BUFLEN = 15
	ITEMLIST(1).ITMCOD = %LOC(SYI$_NODENAME)
	ITEMLIST(1).BUFADR = %LOC(NODENAME)
	ITEMLIST(1).LENADR = %LOC(NODENAME_LEN)
	ITEMLIST(2).END_LIST = 0
	ST = SYS$GETSYIW(,,,ITEMLIST,IOSB,,)
	IF(ST) ST = IOSB.STS
	IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
	END
