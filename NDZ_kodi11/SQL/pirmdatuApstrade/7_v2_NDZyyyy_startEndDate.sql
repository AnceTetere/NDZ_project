--1) Tabulai v2_NDZ2021 pievieno ailes [sak] un [beidz]
ALTER TABLE v2_NDZ2021
ADD sak DATE, beidz DATE;

/*2) Aizpilda tabulas v2_NDZ2021 aili [sak] ar datumiem no ailes [NDZ_sanemsanas_datums], 
     kur ziņojuma kodi ailē zinkod ir 11, 13, 14, 41, 51, 54, 55, 61, 71, 81, 92.*/
SELECT NDZ_sanemsanas_datums, zinkod
FROM v2_NDZ2021
WHERE zinkod IN ('11', '13', '14', '41', '51', '54', '55', '61', '71', '81', '92')
ORDER BY zinkod; 

UPDATE v2_NDZ2021
SET [sak] = NDZ_sanemsanas_datums
WHERE zinkod IN ('11', '13', '14', '41', '51', '54', '55', '61', '71', '81', '92');

SELECT NDZ_sanemsanas_datums, zinkod, sak, beidz
FROM v2_NDZ2021
ORDER BY zinkod;

/*3) Aizpilda tabulas v2_NDZ2021 aili [beidz] ar datumiem no ailes [NDZ_sanemsanas_datums], 
     kur ziņojuma kodi ailē zinkod ir 21, 22, 23, 24, 25, 26, 40, 50, 53, 56, 72, 82, 91.*/
SELECT NDZ_sanemsanas_datums, zinkod
FROM v2_NDZ2021
WHERE zinkod IN ('21', '22', '23', '24', '25', '26', '29', '40', '50', '53', '56', '72', '82', '91')
ORDER BY zinkod; 

UPDATE v2_NDZ2021
SET [beidz] = NDZ_sanemsanas_datums
WHERE zinkod IN ('21', '22', '23', '24', '25', '26', '29', '40', '50', '53', '56', '72', '82', '91');

SELECT NDZ_sanemsanas_datums, zinkod, sak, beidz
FROM v2_NDZ2021
ORDER BY zinkod; 

/*4) Tabulai v2_NDZ2021 pievieno ailes [zinkod_sak] un [zinkod_beidz]*/ 
ALTER TABLE v2_NDZ2021
ADD zinkod_sak VARCHAR(2), zinkod_beidz VARCHAR(2);

/*5) Aizpilda tabulas v2_NDZ2021 aili [zinkod_sak] ar kodiem no ailes [zinkod]*/
SELECT NDZ_sanemsanas_datums, zinkod
FROM v2_NDZ2021
WHERE zinkod IN ('11', '13', '14', '41', '51', '54', '55', '61', '71', '81', '92')
ORDER BY zinkod; 

UPDATE v2_NDZ2021
SET zinkod_sak =  zinkod
WHERE zinkod IN ('11', '13', '14', '41', '51', '54', '55', '61', '71', '81', '92');

SELECT zinkod_sak, zinkod, sak, beidz
FROM v2_NDZ2021
ORDER BY zinkod;

/*6) Aizpilda tabulas v2_NDZ2021 aili [zinkod_sak] ar kodiem no ailes [zinkod].*/
SELECT NDZ_sanemsanas_datums, zinkod, beidz, zinkod_beidz
FROM [dbo].[v2_NDZ2021]
WHERE zinkod IN ('21', '22', '23', '24', '25', '26', '29', '40', '50', '53', '56', '72', '82', '91')
ORDER BY zinkod;

UPDATE [dbo].[v2_NDZ2021]
SET [zinkod_beidz] =  zinkod
WHERE zinkod IN ('21', '22', '23', '24', '25', '26', '29', '40', '50', '53', '56', '72', '82', '91');

SELECT NDZ_sanemsanas_datums, zinkod, beidz, zinkod_beidz
FROM [dbo].[v2_NDZ2021]
ORDER BY zinkod;

/*7) No tabulas izslēdz tos, kam nav ziņojuma koda ailē [zinkod]*/
DELETE FROM v2_NDZ2021
WHERE zinkod = '?';
