C
C V01 01-JAN-2010 FJG ePassive
C
C Function to check if a file already exist in the disk
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
C Copyright 2010 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C=======OPTIONS /check=nooverflow
	logical function filexist(nam)
	implicit none
!
        character    nam*(*)
        logical      isf
!
        isf=.false.
        inquire(file=nam,exist=isf)
        filexist=isf
!
        return
        end
