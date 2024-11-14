USE [NDZ_DB]
GO

-- 1 Izmantojot NDZyyyy_1NF atribūtu [period], tiek izdalīts mēnesis un pārnests tabulā NDZyyyymm
SELECT [period], [PS_code], [DN_code], [NM_code], [sak_beidz], [NDZ_sanemsanas_datums], [zinkod]
INTO NDZ202201 FROM [dbo].[NDZ2022_1NF]
WHERE [period] = '202201'; -- 2021. janv.: 84 670 rindas
                           -- 2022. janv.: 103 932 rindas

-- 2 Mēneša tabulai NDZyyyymm izveido aili last_date, kas nes dotā mēneša pēdējo datumu izmantojamu aprēķinos.
ALTER TABLE NDZ202201
ADD last_date DATE;

UPDATE NDZ202201
SET last_date = EOMONTH(CAST(SUBSTRING([period], 1, 4) + SUBSTRING([period], 5, 2) + '01' AS DATE));

-- 3 NDZyyyymm_11 eksportam
-- 3.1 Nošķir konkrētus kodus, 11, 14, 16, 61, 21, 22, 23, 24, 25, 26, 29
SELECT [period], [PS_code], [DN_code], [NM_code], [sak_beidz], [NDZ_sanemsanas_datums], [zinkod], [last_date]
INTO NDZ202201_11
FROM NDZ202201
WHERE zinkod = '11' OR zinkod = '14' OR zinkod = '16' OR zinkod = '61' OR zinkod = '21' OR zinkod = '22' OR zinkod = '23' OR zinkod = '24' OR zinkod = '25' OR zinkod = '26' OR zinkod = '29';

/*3.2 Eksportē uz ..\NDZ_codes\data\originals\YYYY
   01/21 kods11: 70 408 rindas
   01/22 kods11: 82 376 rindas
   10/22 kods11:        rindas
   10/23 kods11:        rindas
   11/23 kods11:        rindas
   12/23 kods11:        rindas*/

-- 3.3 Izdzēs
DROP TABLE [dbo].[NDZ202201_11];

-- 4 NDZyyyymm_40 eksportam
-- 4.1 Nošķir kodus 40, 41, 91 un 92
SELECT [period], [PS_code], [DN_code], [NM_code], [sak_beidz], [NDZ_sanemsanas_datums], [zinkod], [last_date]
INTO NDZ202201_40
FROM NDZ202201
WHERE zinkod = '40' OR zinkod = '41' OR zinkod = '91' OR zinkod = '92'; 

/*4.2 Eksportē uz ..\NDZ_codes\data\originals\YYYY
   01/21 kods40: 4 953 rindas
   01/22 kods40: 5 095 rindas
   10/22 kods40:       rindas
   10/23 kods40:       rindas
   11/23 kods40:       rindas
   12/23 kods40:       rindas*/

-- 4.3 Izdzēs
DROP TABLE [dbo].[NDZ202201_40];

-- 5 NDZyyyymm_50 eksportam
-- 5.1 Nošķir kodus 50 un 51
SELECT [period], [PS_code], [DN_code], [NM_code], [sak_beidz], [NDZ_sanemsanas_datums], [zinkod], [last_date]
INTO NDZ202201_50
FROM NDZ202201
WHERE zinkod = '50' OR zinkod = '51'; 

/*5.2 Eksportē uz ..\NDZ_codes\data\originals\YYYY
       01/21 kods50:  9 309 rindas
       01/22 kods50: 16 013 rindas
       10/22 kods50:        rindas
       10/23 kods50:        rindas
       11/23 kods50:        rindas
       12/23 kods50:        rindas*/

-- 5.3 Izdzēs
DROP TABLE [dbo].[NDZ202201_50];

-- 6 NDZ202202_53 eksportam
-- 6.1 Nošķir kodus 53 un 54
SELECT [period], [PS_code], [DN_code], [NM_code], [sak_beidz], [NDZ_sanemsanas_datums], [zinkod], [last_date]
INTO NDZ202201_53
FROM NDZ202201
WHERE zinkod = '53' OR zinkod = '54';

/* 6.2 Eksportē uz ..\NDZ_codes\data\originals\YYYY
       01/21 kods53:     0 rindas
       01/22 kods53:   448 rindas
       10/22 kods53:       rindas
       10/23 kods53:       rindas
       11/23 kods53:       rindas
       12/23 kods53:       rindas*/

-- 6.3 Izdzēs
DROP TABLE [dbo].[NDZ202201_53];
