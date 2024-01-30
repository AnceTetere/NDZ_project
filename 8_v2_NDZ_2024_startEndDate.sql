--1) Tabulai [dbo.v2_NDZ2024] pievieno ailes [sak_darbu] un [beidz_darbu]

ALTER TABLE [dbo].[v2_NDZ2024]
ADD sak_darbu DATE, 
    beidz_darbu DATE;

--2) Aizpildi tabulas dbo.v2_NDZ2024 aili [sak_darbu] ar datumiem no ailes [ND_sanemsanas_datums], kur ziņojuma kodi ailē ss_kods ir 11, 14, 41, 51, 54, 55, 61, 71, 81, 92.
--   Tie apzīmē darba sākšanas datumu.

SELECT ND_sanemsanas_datums, ss_kods
FROM [dbo].[v2_NDZ2024]
WHERE ss_kods IN ('11', '14', '41', '51', '54', '55', '61', '71', '81', '92')
ORDER BY ss_kods;

UPDATE [dbo].[v2_NDZ2024]
SET [sak_darbu] = ND_sanemsanas_datums
WHERE ss_kods IN ('11', '14', '41', '51', '54', '55', '61', '71', '81', '92');

SELECT ND_sanemsanas_datums, ss_kods, sak_darbu, beidz_darbu
FROM [dbo].[v2_NDZ2024]
ORDER BY ss_kods;

--3) Aizpildi tabulas dbo.v2_NDZ2024 aili [beidz_darbu] ar datumiem no ailes [ND_sanemsanas_datums], kur ziņojuma kodi ailē ss_kods ir 13, 21, 22, 23, 24, 25, 26, 40, 50, 53, 56, 72, 82, 91.

SELECT ND_sanemsanas_datums, ss_kods
FROM [dbo].[v2_NDZ2024]
WHERE ss_kods IN ('13', '21', '22', '23', '24', '25', '26', '40', '50', '53', '56', '72', '82', '91')
ORDER BY ss_kods;

UPDATE [dbo].[v2_NDZ2024]
SET [beidz_darbu] = ND_sanemsanas_datums
WHERE ss_kods IN ('13', '21', '22', '23', '24', '25', '26', '40', '50', '53', '56', '72', '82', '91');

SELECT ND_sanemsanas_datums, ss_kods, sak_darbu, beidz_darbu
FROM [dbo].[v2_NDZ2024]
ORDER BY ss_kods;

--4) Tabulai [dbo.v2_NDZ2024] pievieno ailes [ss_kods_sak] un [ss_kods_beidz], kas apzīmē aili, 
-- kur uzrādās darba kodi un darbu beigšanas kodi 

ALTER TABLE [dbo].[v2_NDZ2024]
ADD ss_kods_sak varchar(2), 
    ss_kods_beidz varchar(2);

--5) Aizpildi tabulas dbo.v2_NDZ2024 aili [ss_kods_sak] ar kodiem no ailes [ss_kods], t.i.: 11, 14, 41, 51, 54, 55, 61, 71, 81, 92.

SELECT ND_sanemsanas_datums, ss_kods
FROM [dbo].[v2_NDZ2024]
WHERE ss_kods IN ('11', '14', '41', '51', '54', '55', '61', '71', '81', '92')
ORDER BY ss_kods;

UPDATE [dbo].[v2_NDZ2024]
SET ss_kods_sak =  ss_kods
WHERE ss_kods IN ('11', '14', '41', '51', '54', '55', '61', '71', '81', '92');

SELECT ss_kods_sak, ss_kods, sak_darbu, beidz_darbu
FROM [dbo].[v2_NDZ2024]
ORDER BY ss_kods;

--6) Aizpildi tabulas dbo.v2_NDZ2024 aili [ss_kods_sak] ar kodiem no ailes [ss_kods], t.i.: 13, 21, 22, 23, 24, 25, 26, 40, 50, 53, 56, 72, 82, 91.

SELECT ND_sanemsanas_datums, ss_kods, beidz_darbu, ss_kods_beidz
FROM [dbo].[v2_NDZ2024]
WHERE ss_kods IN ('13', '21', '22', '23', '24', '25', '26', '40', '50', '53', '56', '72', '82', '91')
ORDER BY ss_kods;

UPDATE [dbo].[v2_NDZ2024]
SET [ss_kods_beidz] =  ss_kods
WHERE ss_kods IN ('13', '21', '22', '23', '24', '25', '26', '40', '50', '53', '56', '72', '82', '91');

SELECT ND_sanemsanas_datums, ss_kods, beidz_darbu, ss_kods_beidz
FROM [dbo].[v2_NDZ2024]
ORDER BY ss_kods;

-- Tālāk veiksi aprēķinu vai starp datumiem un dd dienas to starpā.
