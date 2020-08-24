C
C $Log:   GXAFXT:[GOLS]GET_BCC.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:23:54   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   13 Dec 1993 16:05:56   SXH
C  Initial revision.
C SUBROUTINE GET_BCC
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE GET_BCC(CTEXT)
        IMPLICIT NONE
C


        ! arguments
        CHARACTER*43  CTEXT


        ! variables
        INTEGER*4  I                   ! counter
        INTEGER*4  TEMP1               ! temporary store variable

        CHARACTER*43  CEQTEXT          ! copy CTEXT into here

        BYTE  TEXT(43)                 ! equivalenced with CEQTEXT
        BYTE  TOTAL                    ! value of BCC 

        EQUIVALENCE(CEQTEXT,TEXT)


        !*** start of code ***!


        ! copy the CTEXT argument into CEQTEXT so we can use the equivalenced 
        ! array

        WRITE(CEQTEXT,200)CTEXT


C        write(5,100)(CTEXT)
C100     format(1X,'CTEXT ',A43) 
C        write(5,105)(CEQTEXT)
C105     format(1X,'CTEXT ',A43) 
C        write(5,110)(TEXT(I),I=1,43)
C110     format(1X,'TEXT ',43A) 




        TOTAL = 0
        TEMP1 = 0

        DO I = 1, 42
            TOTAL = TOTAL + TEXT(I)
C        type *, 'total = ',total
        END DO



        ! AND to 7 bits
        TEMP1 = ZEXT(TOTAL)
        TOTAL = IAND(TEMP1,'7F'X)

C        type *, 'total = ',total
        ! set ODD parity
        !CALL SET_ODDPARITY(TOTAL)

C        type *, 'total = ',total

        TEXT(43) = TOTAL
C        type *, 'text 43 =  ',text(43)

        ! copy CEQTEXT back into CTEXT for return
        WRITE(CTEXT,200)CEQTEXT


        RETURN

200     FORMAT(A43)

        END
