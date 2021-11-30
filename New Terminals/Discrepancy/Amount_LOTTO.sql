-------------------------- LOTTO -----------------------------------------------------------------------------------------------

--select TO_TIMESTAMP('21.11.02 00:00:00','YY-MM-DD HH24::SS') as time

select sum(wager_amount) as total_LOTTO,count(wager_id) AS wagers_count
FROM tl_t_wager 
WHERE 
wager_date >= TO_TIMESTAMP('21.11.02 00:00:00','YY-MM-DD HH24::SS') AND 
wager_date <= TO_TIMESTAMP('21.11.02 23:59:59','YY-MM-DD HH24::SS') AND
CANCEL_DATE IS NULL AND --CANCEL_SERIAL_NUMBER IS NOT NULL AND
draw_number = 88 --88 (Totoloto de Quarta) --89 (Totoloto de Sabado)

--                          COUNT            TOTAL AMT
-- TOTAL Sales              168              995,4  (Hades query a tl_t_wager draw_number=88)
-- Total sales              147              693.00 (MILL Totoloto de Quarta GIND=4) --> LOTQ-02-11-2021.DAT;1
-- Conclução discrepância   21 apostas       302,4 euros 

--select sum(wager_amount) as total_LOTTO,count(wager_id) AS wagers_count
--FROM tl_t_wager 
--WHERE 
--wager_date >= '21.11.02 00:00:00' AND 
--wager_date <= '21.11.02 23:59:59' AND
--CANCEL_DATE IS NULL AND --CANCEL_SERIAL_NUMBER IS NOT NULL AND
--draw_number = 89 -- 88 (Totoloto de Quarta) --89 (Totoloto de Sabado)

--                          COUNT            TOTAL AMT
-- TOTAL Sales              126              5890,5  (Hades query a tl_t_wager draw_number=89) (wagers total without taking into account cancellations)
-- TOTAL Sales              119              5884,2  (Hades query a tl_t_wager draw_number=89)
-- Total sales              119              5884.20 (MILL Totoloto de Quarta GIND=4) --> LOTS-02-11-2021.DAT;2
-- Conclução os valores batem correctamente

-------------------------- SPORTS ----------------------------------------------------------------------------------------------

--select sum(wager_amount) as total_LOTTO,count(wager_id) AS wagers_count
--FROM tb_t_wager 
--WHERE 
--wager_date >= '21.11.02 00:00:00' AND 
--wager_date <= '21.11.02 23:59:59' AND
--CANCEL_DATE IS NULL AND --CANCEL_SERIAL_NUMBER IS NOT NULL AND
--draw_number = 45

--                           COUNT           TOTAL AMT
-- Total sales               49              147.00      (MILL  totobola normal)
-- Total sales               49              147         (Hades totobola normal)      --> TOTO-02-11-2021.DAT;2


--rownum <= 10

-- sum(wager_amount) as total_LOTTO

--select * from tl_t_wager where ROWNUM < 10

