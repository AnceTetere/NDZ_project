--1.1. Nošķiram konkrētus kodus
-- kodi Z1, Z2
SELECT [periodT],[PS_code], [DS_code], [NM_code], [Z_kods_sak], [sak_darbu], [Z_kods_beidz], [beidz_darbu], [prof], [Z_kods], [NDZ_sanemsanas_datums], last_date
INTO v2_NDZ202401_Z1Z2
FROM v2_NDZ202401
WHERE Z_kods = 'Z1' OR Z_kods = 'Z2';

------------------------------------PA VIENAM --------------------------------------------
-- Šis kļūst miskastīgi ļoti ātri tātad uzreiz nodala tos, kuri mēnesī ir pa vienam.

--2) Te ir tabulas v2_NDZ202401_Z1Z2 vienības, kas uzrādās vienu reizi - saglabātas tabulā v2_NDZ202401_Z1Z2_paVienam.
-- šiem papildus analīzi nevajag un uzreiz var veikt aprēķinus.

SELECT t1.*
INTO v2_NDZ202401_Z1Z2_paVienam
FROM [dbo].[v2_NDZ202401_Z1Z2] t1
JOIN (
    SELECT PS_code, NM_code
    FROM [dbo].[v2_NDZ202401_Z1Z2]
    GROUP BY PS_code, NM_code
    HAVING COUNT(*) = 1
) t2 ON t1.PS_code = t2.PS_code AND t1.NM_code = t2.NM_code
ORDER BY PS_code, [NDZ_sanemsanas_datums];


-- uzreiz no [dbo].[v2_NDZ202401_Z1Z2] izdzēšu šīs rindas
DELETE FROM [dbo].[v2_NDZ202401_Z1Z2]
WHERE EXISTS (
    SELECT 1
    FROM [dbo].[v2_NDZ202401_Z1Z2_paVienam] t2
    WHERE [dbo].[v2_NDZ202401_Z1Z2].PS_code = t2.PS_code
      AND [dbo].[v2_NDZ202401_Z1Z2].NM_code = t2.NM_code
      AND [dbo].[v2_NDZ202401_Z1Z2].prof = t2.prof
      AND [dbo].[v2_NDZ202401_Z1Z2].Z_kods = t2.Z_kods
      AND [dbo].[v2_NDZ202401_Z1Z2].NDZ_sanemsanas_datums = t2.NDZ_sanemsanas_datums
);

--3) Aptrāde
--3.1 Izdzēs ailes
ALTER TABLE v2_NDZ202401_Z1Z2_paVienam
DROP COLUMN Z_kods, NDZ_sanemsanas_datums;

--3.2 Izveido jaunas ailes
-- ALTER TABLE v2_NDZ202401_Z1Z2_paVienam
ALTER TABLE v2_NDZ202401_Z1Z2_papildusVieninieki
ADD CD INT;


--3.3 Aprēķini CD no sak_darbu līdz mēneša pēdējai dienai, un ievieto tās ailē CD
SELECT [sak_darbu], [last_date], DATEDIFF(day, sak_darbu, last_date) AS CD
-- FROM v2_NDZ202401_Z1Z2_paVienam
FROM v2_NDZ202401_Z1Z2_papildusVieninieki
WHERE sak_darbu IS NOT NULL;

-- UPDATE v2_NDZ202401_Z1Z2_paVienam
UPDATE v2_NDZ202401_Z1Z2_papildusVieninieki
SET CD = DATEDIFF(day, sak_darbu, last_date)
WHERE sak_darbu IS NOT NULL;

--3.4 Un tagad to pašu ar Z_kods_beidz = Z2: aprēķini CD no mēneša pirmā datuma līdz beidz_darbu
SELECT [beidz_darbu], DATEDIFF(day, '2021-12-31', beidz_darbu) AS CD
FROM v2_NDZ202401_Z1Z2_paVienam
--FROM v2_NDZ202401_Z1Z2_papildusVieninieki
WHERE Z_kods_beidz = 'Z2';

-- Veic tabulā izmaiņas
--UPDATE v2_NDZ202401_Z1Z2_paVienam
UPDATE v2_NDZ202401_Z1Z2_papildusVieninieki
SET CD = DATEDIFF(day, '2021-12-31', beidz_darbu)
WHERE Z_kods_beidz = 'Z2';

-- Izlabo, kur [sak_darbu] = [last_date], tur CD ir 1
SELECT *
FROM v2_NDZ202401_Z1Z2_paVienam
--FROM v2_NDZ202401_Z1Z2_papildusVieninieki
WHERE [sak_darbu] = [last_date];

-- UPDATE v2_NDZ202401_Z1Z2_paVienam
UPDATE v2_NDZ202401_Z1Z2_papildusVieninieki
SET CD = 1
WHERE [sak_darbu] = [last_date];
---------------------------	PAPILDUS VIENINIEKI --------------------------

-- 4. Tabulai v2_NDZ202401_Z1Z2_paVienam galā piešuj rindas no tabulas v2_NDZ202401_Z1Z2_papildusVieninieki
INSERT INTO v2_NDZ202401_Z1Z2_paVienam (periodT, PS_code, DS_code, NM_code, Z_kods_sak, sak_darbu, Z_kods_beidz, beidz_darbu, prof, last_date, CD)
SELECT periodT, PS_code, DS_code, NM_code, Z_kods_sak, sak_darbu, Z_kods_beidz, beidz_darbu, prof, last_date, CD
FROM v2_NDZ202401_Z1Z2_papildusVieninieki;

-- 5. Izdzēs tabulu [dbo].[v2_NDZ202401_Z1Z2_papildusVieninieki]
DROP TABLE [dbo].[v2_NDZ202401_Z1Z2_papildusVieninieki]
