USE [N D Z]
GO

----------------------NDZ202310_11 eksportam

--1. Nošķiram konkrētus kodus, 11, 13, 14, 61, 21, 22, 23, 24, 25, 26, 29

SELECT [period], [PS_code], [DN_code], [NM_code], [sak], [sak_DATE], [beidz], [beidz_DATE], [prof], [zinkod], [NDZ_sanemsanas_datums], [last_date]
INTO NDZ202310_11
FROM v2_NDZ202310
WHERE zinkod = '11' OR zinkod = '13' OR zinkod = '14' OR zinkod = '61' OR zinkod = '21' OR zinkod = '22' OR zinkod = '23' OR zinkod = '24' OR zinkod = '25' OR zinkod = '26' OR zinkod = '29';

--2. Pievieno aili [dienas]
ALTER TABLE [dbo].[NDZ202310_11]
ADD dienas INT;

--3. Eksportē uz ...\data\originals\YYYY

--4. Izdzēs
DROP TABLE [dbo].[NDZ202310_11];

----------------------NDZ202310_40 eksportam

--1. Nošķiram kodus, 40, 41, 91, 92
SELECT [period], [PS_code], [DN_code], [NM_code], [sak], [sak_DATE], [beidz], [beidz_DATE], [prof], [zinkod], [NDZ_sanemsanas_datums], [last_date]
INTO NDZ202310_40
FROM v2_NDZ202310
WHERE zinkod = '40' OR zinkod = '41' OR zinkod = '91' OR zinkod = '92';

--2. Pievieno aili [dienas]
ALTER TABLE [dbo].[NDZ202310_40]
ADD dienas INT;

--3. Eksportē uz ...\data\originals\YYYY


--4. Izdzēs
DROP TABLE [dbo].[NDZ202310_40];

----------------------NDZ202310_50 eksportam

--1. Nošķiram kodus, 50 un 51
SELECT [period], [PS_code], [DN_code], [NM_code], [sak], [sak_DATE], [beidz], [beidz_DATE], [prof], [zinkod], [NDZ_sanemsanas_datums], [last_date]
INTO NDZ202310_50
FROM v2_NDZ202310
WHERE zinkod = '50' OR zinkod = '51'; 

--2. Pievieno aili [dienas]
ALTER TABLE [dbo].[NDZ202310_50]
ADD dienas INT;

--3. Eksportē uz ...\data\originals\YYYY

--4. Izdzēs
DROP TABLE [dbo].[NDZ202310_50];

----------------------NDZ202310_53 eksportam

--1. Nošķiram kodus, 53 un 54
SELECT [period], [PS_code], [DN_code], [NM_code], [sak], [sak_DATE], [beidz], [beidz_DATE], [prof], [zinkod], [NDZ_sanemsanas_datums], [last_date]
INTO NDZ202310_53
FROM v2_NDZ202310
WHERE zinkod = '53' OR zinkod = '54'; 

--2. Pievieno aili [dienas]
ALTER TABLE [dbo].[NDZ202310_53]
ADD dienas INT;

--3. Eksportē uz ...\data\originals\YYYY

--4. Izdzēs
DROP TABLE [dbo].[NDZ202310_53];



----------------------NDZ202310_55 eksportam
-- IZŅEMTS
--1. Nošķiram kodus, 55 un 56
-- SELECT [period], [PS_code], [DN_code], [NM_code], [sak], [sak_DATE], [beidz], [beidz_DATE], [prof], [zinkod], [NDZ_sanemsanas_datums], [last_date]
-- INTO NDZ202310_55
-- FROM v2_NDZ202310
-- WHERE zinkod = '55' OR zinkod = '56';

--2. Pievieno aili [dienas]
-- ALTER TABLE [dbo].[NDZ202310_55]
-- ADD dienas INT;

--3. Eksportē uz ...\data\originals\YYYY

------!!! Ieliku kopā ar 40 un 41. Abi BKA: bieži tiek miksēti, tāpēc izņēmu šo.
----------------------NDZ202310_91 eksportam

--1. Nošķiram kodus, 91 un 92
--SELECT [period], [PS_code], [DN_code], [NM_code], [sak], [sak_DATE], [beidz], [beidz_DATE], [prof], [zinkod], [NDZ_sanemsanas_datums], [last_date]
-- INTO NDZ202310_91
-- FROM v2_NDZ202310
-- WHERE zinkod = '91' OR zinkod = '92';

--2. Pievieno aili [dienas]
-- ALTER TABLE [dbo].[NDZ202310_91]
-- ADD dienas INT;

--3. Eksportē uz ...\data\originals\YYYY

----------------------NDZ202310_81 eksportam
-- IZŅEMTS, jo forma
--1. Nošķiram kodus, 81 un 82
--SELECT [period], [PS_code], [DN_code], [NM_code], [sak], [sak_DATE], [beidz], [beidz_DATE], [prof], [zinkod], [NDZ_sanemsanas_datums], [last_date]
--INTO NDZ202310_81
--FROM v2_NDZ202310
--WHERE zinkod = '81' OR zinkod = '82';

--2. Pievieno aili [dienas]
--ALTER TABLE [dbo].[NDZ202310_81]
--ADD dienas INT;

--3. Eksportē uz ...\data\originals\YYYY
-- NDZ202310_81: ... rindas

--4. Izdzēs
-- DROP TABLE [dbo].[NDZ202310_81];
