C NRM_GDATE.FOR
C
C V02 24-JAN-2011 RXK Change for DATE_AND_TIME.
C V01 13-JUN-2000 OXK Initial release
C
C DUMMY INTERFACE FOR OLD IDATE CALLS TO AVOID Y2K-ERRORS
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
C
	SUBROUTINE GDATE(MONTH,DAY,YEAR)
	IMPLICIT NONE

C ARGUMENTS
	INTEGER*4 MONTH, DAY, YEAR  ! NOTE: 2-DIGIT YEAR

C INTERNAL VARIABLES
	INTEGER*4 TIM(8)
        CHARACTER*12 CLOCK(3)
C
C	values (1) is the 4-digit year
C	values (2) is the month of the year
C	values (3) is the day of the year
C	values (4) is the time difference with respect to
C		    Coordinated Universal Time (UTC) in minutes
C	values (5) is the hour of the day (range 0 to 23)
C	values (6) is the minutes of the hour (range 0 to 59).
C	values (7) is the seconds of the minute (range 0 to 59).
C	values (8) is the milliseconds of the second (range 0 to 999).


	CALL DATE_AND_TIME(CLOCK(1),CLOCK(2),CLOCK(3),TIM)

	YEAR     = MOD(TIM(1),100)
	MONTH    = TIM(2)
	DAY      = TIM(3)

	RETURN
	END
