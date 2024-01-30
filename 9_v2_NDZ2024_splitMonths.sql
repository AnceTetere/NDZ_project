----- Sadalām v2_NDZ2024 pa mēnešiem, lai savienotu ar Irenai_yyyymm

SELECT [periodT],[ss_kod], [ds_kod], [rs_kod], [zkod_sak], [SDD], [zkod_beidz], [SSD1], [prof], [zkod_I555], [ND_sanemsanas_datums]
INTO v2_NDZ202401
FROM v2_NDZ2024
WHERE periodT = '202401';

SELECT *
FROM v2_NDZ202401

ALTER TABLE v2_NDZ202401
ADD last_date DATE;

UPDATE v2_NDZ202401
SET last_date = '2024-01-31'; 

--------------------------------------------Skatām visus kuri vienā un tanī pašā mēnesī ir gan sākušies, gan beigušies vienā un tanī pat datumā
TODO Izstrādā apstrādes kodu apakštabulām.

