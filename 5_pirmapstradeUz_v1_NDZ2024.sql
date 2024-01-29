-- No preNDZ_2024 pievilkt profesijas kodu 

--1) Noņemt pēdiņas
SELECT *
FROM [dbo].[preNDZ_2024]
ORDER BY periodT;  -- 4 492 073 rows

UPDATE [dbo].[preNDZ_2024]
SET periodT = REPLACE(periodT, '"', '');

UPDATE [dbo].[preNDZ_2024]
SET prof = REPLACE(prof, '"', '');

--2) Tabulā [dbo].[preNDZ_2024] pārsauc aili [nperk] uz [P_perk], jo tas saskan ar TI, kur aile [nperk] tiek lietota PP kodiem.
EXEC sp_rename 'preNDZ_2024.nperk', 'P_perk', 'COLUMN';

--3) Pārbaudu, vai ir kādi lieki PP kodi
SELECT *
FROM [dbo].[preNDZ_2024]
WHERE LEFT(P_perk, 1) != 'P' AND LEFT(P_perk, 1) != '9' AND LEFT(P_perk, 2) != '32' AND LEFT(P_perk, 2) != '38' AND P_perk != '00000000003'
ORDER BY P_perk;

--4) Izveidojam tabulu v1_NDZ_2024

CREATE TABLE v1_NDZ2024 (
    periodT varchar(6),
    sanemsanas_datums date,
    P_perk varchar(11),
	rkod varchar(11),
	zkod varchar(2),
	prof varchar(7)
);

--5) Noņemam visas rindas, kur P_perk ir '00000000003' un ievieto to tabulā v1_NDZ2024

INSERT INTO v1_NDZ2024 (periodT, sanemsanas_datums, P_perk, rkod, zkod, prof)
SELECT [periodT], [sandat], [P_perk], [rkod], [zkod], [prof]
FROM [dbo].[preNDZ_2024]
WHERE P_perk != '00000000003'; -- 4 485 661 rows

SELECT *
FROM [dbo].[v1_NDZ2024]
ORDER BY periodT;
