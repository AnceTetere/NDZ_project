USE [N D Z]
GO
  
----- No gada v2_NDZ2023 izdali vajadzīgo mēnesi

SELECT [period], [PS_code], [DN_code], [NM_code], [zinkod_sak], [sak], [zinkod_beidz], [beidz], [prof], [zinkod], [NDZ_sanemsanas_datums]
INTO v2_NDZ202311
FROM v2_NDZ2023
WHERE period = '202311';

SELECT *
FROM [dbo].[v2_NDZ202311];

ALTER TABLE v2_NDZ202311
ADD last_date DATE;

UPDATE v2_NDZ202311
SET last_date = '2023-11-30'; 

----------------------NDZ202311_11 eksportam

--1. Nošķiram konkrētus kodus, 11, 13, 14, 61, 21, 22, 23, 24, 25, 26, 29

SELECT [period], [PS_code], [DN_code], [NM_code], [zinkod_sak], [sak], [zinkod_beidz], [beidz], [prof], [zinkod], [NDZ_sanemsanas_datums], [last_date]
INTO NDZ202311_11
FROM v2_NDZ202311
WHERE zinkod = '11' OR zinkod = '13' OR zinkod = '14' OR zinkod = '61' OR zinkod = '21' OR zinkod = '22' OR zinkod = '23' OR zinkod = '24' OR zinkod = '25' OR zinkod = '26' OR zinkod = '29';  

--2. Pievieno aili [dienas]
ALTER TABLE [dbo].[NDZ202311_11]
ADD dienas INT;

--3. Eksportē uz ...\data\originals\YYYY

--4. Izdzēs
DROP TABLE [dbo].[NDZ202311_11];

----------------------NDZ202311_40 eksportam

--1. Nošķiram kodus, 40, 41, 91, 92
SELECT [period], [PS_code], [DN_code], [NM_code], [zinkod_sak], [sak], [zinkod_beidz], [beidz], [prof], [zinkod], [NDZ_sanemsanas_datums], [last_date]
INTO NDZ202311_40
FROM v2_NDZ202311
WHERE zinkod = '40' OR zinkod = '41' OR zinkod = '91' OR zinkod = '92';

--2. Pievieno aili [dienas]
ALTER TABLE [dbo].[NDZ202311_40]
ADD dienas INT;

--3. Eksportē uz ...\data\originals\YYYY

--4. Izdzēs
DROP TABLE [dbo].[NDZ202311_40];

----------------------NDZ202311_50 eksportam

--1. Nošķiram kodus, 50 un 51
SELECT [period], [PS_code], [DN_code], [NM_code], [zinkod_sak], [sak], [zinkod_beidz], [beidz], [prof], [zinkod], [NDZ_sanemsanas_datums], [last_date]
INTO NDZ202311_50
FROM v2_NDZ202311
WHERE zinkod = '50' OR zinkod = '51';

--2. Pievieno aili [dienas]
ALTER TABLE [dbo].[NDZ202311_50]
ADD dienas INT;

--3. Eksportē uz ...\data\originals\YYYY

--4. Izdzēs
DROP TABLE [dbo].[NDZ202311_50];

----------------------NDZ202311_53 eksportam

--1. Nošķiram kodus, 53 un 54
SELECT [period], [PS_code], [DN_code], [NM_code], [zinkod_sak], [sak], [zinkod_beidz], [beidz], [prof], [zinkod], [NDZ_sanemsanas_datums], [last_date]
INTO NDZ202311_53
FROM v2_NDZ202311
WHERE zinkod = '53' OR zinkod = '54'; 

--2. Pievieno aili [dienas]
ALTER TABLE [dbo].[NDZ202311_53]
ADD dienas INT;

--3. Eksportē uz ...\data\originals\YYYY

--4. Izdzēs
DROP TABLE [dbo].[NDZ202311_53];
