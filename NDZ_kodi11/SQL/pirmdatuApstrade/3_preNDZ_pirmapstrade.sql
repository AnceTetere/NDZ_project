-- Tabulā preNDZ_20XX ir daudz tukšo rindu
-- Vispirms izņem tās.
DELETE FROM [dbo].[preNDZ_2021]
WHERE NM_code IS NULL AND NMR_ATB_PK_ALG IS NULL AND 
	  DN_code IS NULL AND 
	  DN_ATB_PK_ALG IS NULL AND 
	  sandat = '""' AND
	  zinkod IS NULL AND 
	  valsts IS NULL AND
	  prof IS NULL AND
	  period IS NULL;  
	  
-- Tad izformē ailes

-- 1) Izformē aili NM_code
-- Noņem pēdiņas
UPDATE preNDZ_2021
SET NM_code = REPLACE(NM_code, '"', ''); 

-- Nomaini ailes platumu.
ALTER TABLE preNDZ_2021
ALTER COLUMN NM_code VARCHAR(11);
-- Pārbauda uz NULL
SELECT *
FROM preNDZ_2021
WHERE NM_code IS NULL;

-- 2) Izformē aili NMR_ATB_PK_ALG
-- Noņem pēdiņas
UPDATE preNDZ_2021
SET NMR_ATB_PK_ALG = REPLACE(NMR_ATB_PK_ALG, '"', '');

-- Nomaini ailes platumu.
ALTER TABLE preNDZ_2021
ALTER COLUMN NMR_ATB_PK_ALG VARCHAR(1);
-- Pārbaudu uz NULL
SELECT *
FROM preNDZ_2021
WHERE NM_code IS NULL;  --Neuzrāda nevienu

--3) Izformē aili DN_code
-- Noņem pēdiņas
UPDATE preNDZ_2021
SET DN_code = REPLACE(DN_code, '"', '');
-- Nomaini ailes platumu.
ALTER TABLE preNDZ_2021
ALTER COLUMN DN_code VARCHAR(11);
-- Pārbaudu uz NULL
SELECT *
FROM preNDZ_2021
WHERE DN_code IS NULL;  --Neuzrāda nevienu

--4) Izformē aili DN_ATB_PK_ALG
-- Noņem pēdiņas
UPDATE preNDZ_2021
SET DN_ATB_PK_ALG = REPLACE(DN_ATB_PK_ALG, '"', '');
-- Nomaini ailes platumu.
ALTER TABLE preNDZ_2021
ALTER COLUMN DN_ATB_PK_ALG VARCHAR(1);
-- Pārbaudu uz NULL
SELECT *
FROM preNDZ_2021
WHERE DN_ATB_PK_ALG IS NULL;  --Neuzrāda nevienu

--5) Izformē aili sandat
-- Noņem pēdiņas
UPDATE preNDZ_2021
SET sandat = REPLACE(sandat, '"', '');

-- Pārbaudu uz NULL
SELECT *
FROM preNDZ_2021
WHERE sandat IS NULL;  --Neuzrāda nevienu

-- Nomaini ailes tipu uz datumu
ALTER TABLE preNDZ_2021 ADD NewsanDat DATE;
UPDATE preNDZ_2021 SET NewsanDat = CONVERT(DATE, sandat, 104);

ALTER TABLE preNDZ_2021 DROP COLUMN sandat;
EXEC sp_rename 'preNDZ_2021.NewsanDat', 'sandat', 'COLUMN';

--6) Izformē aili zinkod
-- Noņem pēdiņas
UPDATE preNDZ_2021
SET zinkod = REPLACE(zinkod, '"', '');

-- Nomaini ailes platumu.
ALTER TABLE preNDZ_2021
ALTER COLUMN zinkod VARCHAR(2);

-- Pārbaudu uz NULL
SELECT *
FROM preNDZ_2021
WHERE zinkod IS NULL;  --Neuzrāda nevienu 

--7) Izformē aili valsts
-- Noņem pēdiņas
UPDATE preNDZ_2021
SET valsts = REPLACE(valsts, '"', '');

-- Nomaini ailes platumu.
ALTER TABLE preNDZ_2021
ALTER COLUMN valsts VARCHAR(2);

-- Pārbaudu uz NULL
SELECT *
FROM preNDZ_2021
WHERE valsts IS NULL;  --Neuzrāda nevienu

--8) Izformē aili prof
-- Noņem pēdiņas
UPDATE preNDZ_2021
SET prof = REPLACE(prof, '"', '');

-- Nomaini ailes platumu.
ALTER TABLE preNDZ_2021
ALTER COLUMN prof VARCHAR(7);

-- Pārbaudu uz NULL
SELECT *
FROM preNDZ_2021
WHERE prof IS NULL;  --Neuzrāda nevienu

--9) Izformē aili period
-- Noņem pēdiņas
UPDATE preNDZ_2021
SET period = REPLACE(period, '"', '');

-- Nomaini ailes platumu.
ALTER TABLE preNDZ_2021
ALTER COLUMN period VARCHAR(6);

-- Pārbaudu uz NULL
SELECT *
FROM preNDZ_2021
WHERE period IS NULL;  --Neuzrāda nevienu

/*10) No SS nākušajos datos ir rindas, kur saņemšanas mēnesi no norādītā perioda atšķiras.
     Citreiz pat par vairākiem mēnešiem starpā. 

LĒMUMS: Tiek pieņemts lēmums tos pārdefinēt atbilstoši periodam, ko nes NDZ saņemšanas datums.*/
SELECT zinkod, sandat, period, DATEPART(MONTH, sandat) AS sandat_month, SUBSTRING(period, 5, 2) as period_month
FROM preNDZ_2021
WHERE DATEPART(MONTH, sandat) != SUBSTRING(period, 5, 2);

-- Par cik mēs visām citām vērtībām rēķinām datumus mēnesī no sandat, tāpēc te atšķirības ailē [period] tiks piemērotas ailes [sandat] mēnesim.
--Apskati:
SELECT sandat, [period] = FORMAT(DATEPART(YEAR, sandat), '0000') + FORMAT(DATEPART(MONTH, sandat), '00')
FROM preNDZ_2021
WHERE DATEPART(MONTH, sandat) != SUBSTRING([period], 5, 2)
ORDER BY sandat;

--Izmaini
UPDATE preNDZ_2021
SET [period] = FORMAT(DATEPART(YEAR, sandat), '0000') + FORMAT(DATEPART(MONTH, sandat), '00')
FROM preNDZ_2021
WHERE DATEPART(MONTH, sandat) != SUBSTRING([period], 5,2); 

SELECT *
FROM [dbo].[preNDZ_2021];

--11) Tagad noliec atsevišķā tabulā tās vērtības, kurām sandat ir no iepriekšējā gada
SELECT *
FROM preNDZ_2021
WHERE LEFT([period], 4) = CAST(RIGHT('preNDZ_2021', 4) AS INT) - 1
ORDER BY sandat;

--11.1 Izveido pagaidu tabulu ar 2020. gada vērtībām no tabulas preNDZ_2021.
CREATE TABLE p2020_noNDZ2021 (
    NM_code VARCHAR(11),
    NMR_ATB_PK_ALG VARCHAR(1),
    DN_code VARCHAR(11),
    DN_ATB_PK_ALG VARCHAR(1),
    zinkod VARCHAR(2),
	valsts VARCHAR(2),
	prof VARCHAR(7),
	[period] VARCHAR(6),
	sandat DATE);

--11.2 Ievieto rindas no iepriekšējā gada jaunizveidotajā tabulā
INSERT INTO p2020_noNDZ2021 (NM_code, NMR_ATB_PK_ALG, DN_code, DN_ATB_PK_ALG, zinkod, valsts, prof, [period], sandat)
SELECT NM_code, NMR_ATB_PK_ALG, DN_code, DN_ATB_PK_ALG, zinkod, valsts, prof, [period], sandat
FROM preNDZ_2021
WHERE LEFT([period], 4) = CAST(RIGHT('preNDZ_2021', 4) AS INT) - 1
ORDER BY sandat;

--11.3 Izdzēs rindas, ko tikko pārvietoji uz jaunizveidoto tabulu.
DELETE FROM preNDZ_2021
WHERE LEFT([period], 4) = CAST(RIGHT('preNDZ_2021', 4) AS INT) - 1;

--11.4 Pārbaudi
SELECT *
FROM preNDZ_2021
WHERE LEFT([period], 4) = CAST(RIGHT('preNDZ_2021', 4) AS INT) - 1;

--12) Tabulā [dbo].[preNDZ_2021] pārsauc aili [DN_code] uz [PS_code].
EXEC sp_rename 'preNDZ_2021.DN_code', 'PS_code', 'COLUMN';

--12) Pārbaudu, vai ir kādi lieki PS
SELECT *
FROM [dbo].[preNDZ_2021]
WHERE LEFT(PS_code, 1) != 'P' AND LEFT(PS_code, 1) != '9' AND LEFT(PS_code, 2) != '32' AND LEFT(PS_code, 2) != '38' AND PS_code != '00000000003' AND LEFT(PS_code, 1) != 'T'
ORDER BY PS_code; 

