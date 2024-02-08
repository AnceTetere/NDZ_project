------------------------------------PA DIVIEM --------------------------------------------

--1) Te no tabulas v2_NDZ202401_Z1Z2 izvelk tos, kas uzrādās divreiz un saglabā tos tabulā v2_NDZ202401_Z1Z2_paDivi.

SELECT t1.*
INTO v2_NDZ202401_Z1Z2_paDiviem
FROM [dbo].[v2_NDZ202401_Z1Z2] t1
JOIN (
    SELECT PS_code, NM_code
    FROM [dbo].[v2_NDZ202401_Z1Z2]
    GROUP BY PS_code, NM_code
    HAVING COUNT(*) = 2
) t2 ON t1.PS_code = t2.PS_code AND t1.NM_code = t2.NM_code
ORDER BY PS_code, [NDZ_date];


-- uzreiz no [dbo].[v2_NDZ202401_Z1Z2] izdzēs šīs rindas
DELETE FROM [dbo].[v2_NDZ202401_Z1Z2]
WHERE EXISTS (
    SELECT 1
    FROM [dbo].[v2_NDZ202401_Z1Z2_paDiviem] t2
    WHERE [dbo].[v2_NDZ202401_Z1Z2].PS_code = t2.PS_code
      AND [dbo].[v2_NDZ202401_Z1Z2].NM_code = t2.NM_code
      AND [dbo].[v2_NDZ202401_Z1Z2].prof = t2.prof
      AND [dbo].[v2_NDZ202401_Z1Z2].zinkod = t2.zinkod
      AND [dbo].[v2_NDZ202401_Z1Z2].NDZ_date = t2.NDZ_date
);

--3) Aptrādā divniekus
   SELECT PS_code, NM_code
    -- FROM [dbo].[v2_NDZ202401_Z1Z2_paDiviem]
	FROM [dbo].[v2_NDZ202401_Z1Z2_papildusDivnieki]
    GROUP BY PS_code, NM_code
    HAVING COUNT(*) = 2;

--3.1. Savienojam duplikātu rindas tā, lai sākuma un beigu datumi ir vienā ailē
SELECT
    periodT,
    PS_code,
    ND_code,
    NM_code,
	MAX(prof) AS prof,
	MAX(zinkod_sak) AS zinkod_sak,
    MAX(sak_darbu) AS sak_darbu,
	MAX(zinkod_beidz) AS zinkod_beidz,
    MAX(beidz_darbu) AS beidz_darbu,
    MAX(last_date) AS last_date
-- FROM [dbo].[v2_NDZ202401_Z1Z2_paDiviem]
FROM [dbo].[v2_NDZ202401_Z1Z2_papildusDivnieki]
GROUP BY
    periodT,
    PS_code,
    ND_code,
    NM_code
	ORDER BY PS_code;

/* 3.2 Te sekojošās vērtības pēc rindu apvienošanas nāk ar NULLs beidz_darbu vai sak_darbu ailēs.
   skatīt LĒMUMS I.

	SELECT *
	FROM [dbo].[v2_NDZ202401_Z1Z2_paDiviem]
--	WHERE PS_code = '...'
--	WHERE PS_code = '...'
--	WHERE PS_code = '...'
--  WHERE PS_code = '...'
--  WHERE PS_code = '..'

-- 3.3. Izveido tabulu v2_NDZ202401_Z1Z2_paDiviem_combined

CREATE TABLE v2_NDZ202401_Z1Z2_paDiviem_combined (
    periodT VARCHAR(6), 
	PS_code VARCHAR(11), 
	ND_code VARCHAR(11), 
	NM_code VARCHAR(11), 
	prof VARCHAR(7), 
	zinkod_sak VARCHAR(2), 
	sak_darbu DATE, 
	zinkod_beidz VARCHAR(2), 
	beidz_darbu DATE, 
	last_date DATE)

-- 3.4 Savienotās rindas ieliek tabulā [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined]
INSERT INTO [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined] (periodT, PS_code, ND_code, NM_code, prof, zinkod_sak, sak_darbu, zinkod_beidz, beidz_darbu, last_date)
SELECT
    periodT,
    PS_code,
    ND_code,
    NM_code,
	MAX(prof) AS prof,
	MAX(zinkod_sak) AS zinkod_sak,
    MAX(sak_darbu) AS sak_darbu,
	MAX(zinkod_beidz) AS zinkod_beidz,
    MAX(beidz_darbu) AS beidz_darbu,
    MAX(last_date) AS last_date
FROM [dbo].[v2_NDZ202401_Z1Z2_paDiviem]
GROUP BY
    periodT,
    PS_code,
    ND_code,
    NM_code;

--3.5 OBLIGĀTI izdzēs tabulu [dbo].[v2_NDZ202401_Z1Z2_paDiviem]!
DROP TABLE [dbo].[v2_NDZ202401_Z1Z2_paDiviem]

--4. Tagad, sekojot lēmumam 1 punktā 3.2. izlasām    

--4.1. No tabulas [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined] izlasi rindas, kur ailēs [sak_darbu] vai [beidz_darbu] ir NULLs.
SELECT *
FROM [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined]
WHERE [sak_darbu] IS NULL OR [beidz_darbu] IS NULL;

-- 4.2. Izveido šiem atsevišķu tabulu
CREATE TABLE v2_NDZ202401_Z1Z2_papildus (
    periodT VARCHAR(6), 
	PS_code VARCHAR(11), 
	ND_code VARCHAR(11), 
	NM_code VARCHAR(11),  
	zinkod_sak VARCHAR(2), 
	sak_darbu DATE, 
	zinkod_beidz VARCHAR(2), 
	beidz_darbu DATE, 
	prof VARCHAR(7),
	last_date DATE)

--4.3. Tabulā v2_NDZ202401_Z1Z2_papildus ievieto rindas no tabulas [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined], kur ailēs [sak_darbu] vai [beidz_darbu] ir NULLs.

INSERT INTO [dbo].[v2_NDZ202401_Z1Z2_papildus] (periodT, PS_code, ND_code, NM_code, zinkod_sak, sak_darbu, zinkod_beidz, beidz_darbu, prof, last_date)
SELECT periodT, PS_code, ND_code, NM_code, zinkod_sak, sak_darbu, zinkod_beidz, beidz_darbu, prof, last_date
FROM [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined]
WHERE [sak_darbu] IS NULL OR [beidz_darbu] IS NULL;

--4.4. Rindas, kuras tika ieliktas tabulā Tabulā v2_NDZ202401_Z1Z2_papildus izdzēs no [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined]
DELETE FROM [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined]
WHERE [sak_darbu] IS NULL OR [beidz_darbu] IS NULL;
-- Tabulā [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined] bija 1020 rindas, izņēmu 35, palika 985.

-- 4.5. Tabula [dbo].[v2_NDZ202401_Z1Z2_papildus] aiziet uz apstrādi 11_kodiZ1Z2_paVienam.sql un tiek piešūta piešuj galā tabulai.[dbo].[v2_NDZ202401_Z1Z2_paVienam]

-- 5 Turpini ar dienu sarēķiniem tabulā [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined]

ALTER TABLE [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined]
ADD CDD INT,
	CDD1 INT,
	CDD2 INT


-- 5.1.1. Sarēķina CDD un  saglabā ailē CDD1.
SELECT [sak_darbu], [last_date], DATEDIFF(day, sak_darbu, last_date) AS CDD1
FROM [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined]
WHERE [beidz_darbu] < [sak_darbu];

-- Veic tabulā izmaiņas
UPDATE v2_NDZ202401_Z1Z2_paDiviem_combined
SET CDD1 = DATEDIFF(day, sak_darbu, last_date)
WHERE [beidz_darbu] < [sak_darbu];

-- 5.1.2. Sarēķina CDD starp beigu datumu [beidz_darbu] no mēneša sākuma '2024-01-01'; saglabā tās ailē CDD2.
SELECT beidz_darbu, DATEDIFF(day, '2024-01-01', beidz_darbu)  AS CDD2
FROM [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined]
WHERE [beidz_darbu] < [sak_darbu];

-- Veic tabulā izmaiņas
UPDATE v2_NDZ202401_Z1Z2_paDiviem_combined
SET CDD2 = DATEDIFF(day, '2024-01-01', beidz_darbu)
WHERE [beidz_darbu] < [sak_darbu];

-- 5.1.3 Sarēķinam CDD, summējot CDD1 + CDD2
SELECT PS_code, CDD1, CDD2, CDD1 + CDD2 as CDD
FROM [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined]
WHERE [beidz_darbu] < [sak_darbu];

UPDATE v2_NDZ202401_Z1Z2_paDiviem_combined
SET CDD = CDD1 + CDD2
WHERE [beidz_darbu] < [sak_darbu];

-- 5.1.4 Izdzēs pagaidu ailes
ALTER TABLE [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined]
DROP COLUMN CDD1, CDD2;

-- 5.2. Pārējiem aprēķinām CDD un darba CDD
-- 5.2.1. Sarēķina CDD un saglabā kalendārās CDD ailē CDD.
SELECT  [sak_darbu], [beidz_darbu], DATEDIFF(day, sak_darbu, beidz_darbu) AS CDD
FROM [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined]
WHERE [beidz_darbu] > [sak_darbu];

--5.2.2. Veic tabulā izmaiņas
UPDATE v2_NDZ202401_Z1Z2_paDiviem_combined
SET CDD = DATEDIFF(day, sak_darbu, beidz_darbu)
WHERE [beidz_darbu] > [sak_darbu];

-- 5.3. Ja sak_darbu = beidz_darbu, tad CDD = 1
-- 5.3.1. Sarēķina CDD starp sak_darbu un beidz_darbu, un saglabā kalendārās CDD ailē CDD.
SELECT  [sak_darbu], [beidz_darbu], DATEDIFF(day, sak_darbu, beidz_darbu) AS CDD
FROM v2_NDZ202401_Z1Z2_paDiviem_combined
WHERE [beidz_darbu] = [sak_darbu];

--5.3.2. Veic tabulā izmaiņas
UPDATE v2_NDZ202401_Z1Z2_paDiviem_combined
SET CDD = 1
WHERE [beidz_darbu] = [sak_darbu];

select *
from [dbo].[v2_NDZ202401_Z1Z2_paDiviem_combined]
WHERE [beidz_darbu] = [sak_darbu];
