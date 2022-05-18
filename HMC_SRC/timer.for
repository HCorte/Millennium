! Example use of LIB$SHOW_TIMER to time an HP Fortran program
      PROGRAM TIMER

        INTEGER TIMER_CONTEXT
        DATA TIMER_CONTEXT /0/

        ! Initialize default timer stats to 0
        CALL LIB$INIT_TIMER
        ! Sample first section of code to be timed
        DO I=1,100
            CALL MOM
        ENDDO

        ! Display stats
        TYPE *,'Stats for first section'
        CALL LIB$SHOW_TIMER
        
        ! Zero second timer context
        CALL LIB$INIT_TIMER (TIMER_CONTEXT)
        ! Sample second section of code to be timed
        DO I=1,1000
            CALL MOM
        ENDDO

        ! Display stats
            TYPE *,'Stats for second section'
            CALL LIB$SHOW_TIMER (TIMER_CONTEXT)
            TYPE *,'Accumulated stats for two sections'
            CALL LIB$SHOW_TIMER

        ! Re-Initialize second timer stats to 0
            CALL LIB$INIT_TIMER (TIMER_CONTEXT)

        ! Sample Third section of code to be timed
        DO I=1,1000
            CALL MOM
        ENDDO

        ! Display stats
        
        TYPE *,'Stats for third section'
        CALL LIB$SHOW_TIMER (TIMER_CONTEXT)
        TYPE *,'Accumulated stats for all sections'
        CALL LIB$SHOW_TIMER
      END PROGRAM TIMER

      ! Sample subroutine performs enough processing so times aren't all 0.0
      SUBROUTINE MOM
        COMMON BOO(10000)
        DOUBLE PRECISION BOO
        BOO = 0.5 ! Initialize all array elements to 0.5

        DO I=2,10000
            BOO(I) = 4.0+(BOO(I-1)+1)*BOO(I)*COSD(BOO(I-1)+30.0)
            BOO(I-1) = SIND(BOO(I)**2)
        ENDDO

        RETURN
      END SUBROUTINE MOM