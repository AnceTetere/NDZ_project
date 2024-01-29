-- Tabulā preNDZ_2024 ir daudz tukšo rindu
-- Vispirms izņem tās.
USE [Zinas ND];

DELETE FROM [dbo].[preNDZ_2024]
WHERE rkod IS NULL AND TB_PK_ALG IS NULL AND 
	  perk IS NULL AND 
	  TB_PK_ALG IS NULL AND 
	  sandat = '""' AND
	  zkod IS NULL AND 
	  valsts IS NULL AND
	  pro IS NULL AND
	  periodT IS NULL;

-- Tad izformē ailes

-- 1) Izformē aili rkod
-- Noņem pēdiņas
UPDATE preNDZ_2024
SET rkod = REPLACE(rkod, '"', ''); 
-- Nomaini ailes platumu.
ALTER TABLE preNDZ_2024
ALTER COLUMN rkod VARCHAR(11);
-- Pārbaudu uz NULL
SELECT *
FROM preNDZ_2024
WHERE rkod IS NULL;

-- 2) Izformē aili TB_PK_ALG
-- Noņem pēdiņas
UPDATE preNDZ_2024
SET TB_PK_ALG = REPLACE(TB_PK_ALG, '"', '');
-- Nomaini ailes platumu.
ALTER TABLE preNDZ_2024
ALTER COLUMN TB_PK_ALG VARCHAR(1);
-- Pārbaudu uz NULL
SELECT *
FROM preNDZ_2024
WHERE rkod IS NULL;  --Neuzrāda nevienu

--3) Izformē aili perk
-- Noņem pēdiņas
UPDATE preNDZ_2024
SET perk = REPLACE(perk, '"', '');
-- Nomaini ailes platumu.
ALTER TABLE preNDZ_2024
ALTER COLUMN perk VARCHAR(11);
-- Pārbaudu uz NULL
SELECT *
FROM preNDZ_2024
WHERE perk IS NULL;  --Neuzrāda nevienu

--4) Izformē aili TB_PK_ALG
-- Noņem pēdiņas
UPDATE preNDZ_2024
SET TB_PK_ALG = REPLACE(TB_PK_ALG, '"', '');
-- Nomaini ailes platumu.
ALTER TABLE preNDZ_2024
ALTER COLUMN TB_PK_ALG VARCHAR(1);
-- Pārbaudu uz NULL
SELECT *
FROM preNDZ_2024
WHERE TB_PK_ALG IS NULL;  --Neuzrāda nevienu

--5) Izformē aili sandat
-- Noņem pēdiņas
UPDATE preNDZ_2024
SET sandat = REPLACE(sandat, '"', '');
-- Pārbaudu uz NULL
SELECT *
FROM preNDZ_2024
WHERE sandat IS NULL;  --Neuzrāda nevienu
-- Nomaini ailes tipu uz datumu
ALTER TABLE preNDZ_2024 ADD NewsanDat DATE;
UPDATE preNDZ_2024 SET NewsanDat = CONVERT(DATE, sandat, 104);
ALTER TABLE preNDZ_2024 DROP COLUMN sandat;
EXEC sp_rename 'preNDZ_2024.NewsanDat', 'sandat', 'COLUMN';

--6) Izformē ailizkod
-- Noņem pēdiņas
UPDATE preNDZ_2024
SETzkod = REPLACE(zinkod, '"', '');
-- Nomaini ailes platumu.
ALTER TABLE preNDZ_2024
ALTER COLUMNzkod VARCHAR(2);
-- Pārbaudu uz NULL
SELECT *
FROM preNDZ_2024
WHEREzkod IS NULL;  --Neuzrāda nevienu


----------------turpini te izstrādāt tabulas----
---!!!!   Atceries, ka sandat jau ir izstrādāta - tā pārnesās uz beigām, kad mainīji tipu no varchar uz date
----kad pabeidz ar ailēm, nomaini tabulas nosaukumu


--7) Izformē aili VEIDS
-- Noņem pēdiņas
UPDATE NVD_2022
SET VEIDS = REPLACE(VEIDS, '"', '');
-- Nomaini ailes platumu.
ALTER TABLE NVD_2022
ALTER COLUMN VEIDS VARCHAR(20);

SELECTzkod, LEN(zinkod) AS lengthy
FROM preNDZ_2024
ORDER BY lengthy DESC;

SELECT *
FROM preNDZ_2024




--8) Izformē aili SECIBAS_NUMURS
-- Noņem pēdiņas
UPDATE NVD_2022
SET SECIBAS_NUMURS = REPLACE(SECIBAS_NUMURS, '"', '');
-- Nomaini ailes platumu.
ALTER TABLE NVD_2022
ALTER COLUMN SECIBAS_NUMURS VARCHAR(3);

--9) Izformē aili STATUSS
--Noņem pēdiņas
UPDATE NVD_2022
SET STATUSS = REPLACE(STATUSS, '"', '');
-- Nomaini ailes platumu.
ALTER TABLE NVD_2022
ALTER COLUMN STATUSS VARCHAR(20);

--10) Izformē aili SAKUMA DATUMS
--Noņem pēdiņas un pārveido datu tipu uz DATE
UPDATE NVD_2022 SET SAKUMA_DATUMS = CONVERT(DATE, REPLACE(SAKUMA_DATUMS, '"', ''), 104);
-- šī rindiņa noņem pēdiņas un pārveido varchar mainīgo uz DATE
-- !! TAS, KAS NOTIEK IR, ka tas vienlaicīgi tukšās šūnas pārveido uz 1900-01-01
-- Domāju, ka sekojošais kods to nedarīs: SET SAKUMA_DATUMS = CASE WHEN SAKUMA_DATUMS <> '' THEN CONVERT(DATE, REPLACE(SAKUMA_DATUMS, '"', ''), 104) END;
-- taču tas nedarbojas un uz doto brīdi nav laika čakarēties, tāpēc nomainu šūnas ar 1900-01-01 atpakaļ uz tukšumiem.
-- kad sanāk laika, optimizē šo.
UPDATE NVD_2022 SET SAKUMA_DATUMS = '' WHERE SAKUMA_DATUMS = '1900-01-01'; 

--11) Izformē aili BEIGU_DATUMS
--Noņem pēdiņas un pārveido datu tipu uz DATE, izlaižot tukšās šūnas
UPDATE NVD_2022 SET BEIGU_DATUMS = CONVERT(DATE, REPLACE(BEIGU_DATUMS, '"', ''), 104); -- skat. iepriekšējo komentāru 
UPDATE NVD_2022 SET BEIGU_DATUMS = '' WHERE BEIGU_DATUMS = '1900-01-01'; 

--12) Izformē aili DARB_DATUMS
--Noņem pēdiņas un pārveido datu tipu uz DATE
UPDATE NVD_2022 SET DARB_DATUMS = CONVERT(DATE, REPLACE(DARB_DATUMS, '"', ''), 104); 
UPDATE NVD_2022 SET DARB_DATUMS = '' WHERE DARB_DATUMS = '1900-01-01';
	   
--13) Izformē aili TURP_DATUMS
--Noņem pēdiņas un pārveido datu tipu uz DATE
UPDATE NVD_2022 SET TURP_DATUMS = CONVERT(DATE, REPLACE(TURP_DATUMS, '"', ''), 104); 
UPDATE NVD_2022 SET TURP_DATUMS = '' WHERE TURP_DATUMS = '1900-01-01'; 

--14) Izformē aili ANUL_DATUMS 
--Noņem pēdiņas un pārveido datu tipu uz DATE
UPDATE NVD_2022 SET ANUL_DATUMS = CONVERT(DATE, REPLACE(ANUL_DATUMS, '"', ''), 104);
UPDATE NVD_2022 SET ANUL_DATUMS = '' WHERE ANUL_DATUMS = '1900-01-01'; 

--15) Izformē aili periodT
-- Noņem pēdiņas
UPDATE NVD_2022 SET periodT = REPLACE(periodT, '"', '');
-- Nomaini ailes platumu.
ALTER TABLE NVD_2022 ALTER COLUMN periodT VARCHAR(6);
