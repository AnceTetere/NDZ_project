--1) Izveido tabulu v1_NDZ2021
CREATE TABLE v1_NDZ2021 (
    [period] varchar(6),
    NDZ_sanemsanas_datums date,
    PS_code varchar(11),
	NM_code varchar(11),
	zinkod varchar(2),
	prof varchar(7));

/* LĒMUMS 
2) Noņemu visas rindas, kur PS_code ir '00000000003', jo izskatās

2.1 Dzēšu šīs rindas un sekojošās tiek ievietotas tabulā v1_NDZ2021. */
INSERT INTO v1_NDZ2021 ([period], NDZ_sanemsanas_datums, PS_code, NM_code, zinkod, prof)
SELECT [period], sandat, PS_code, NM_code, zinkod, prof
FROM preNDZ_2021
WHERE PS_code = '00000000003'; 

--2.2 Pārbaude
SELECT *
FROM v1_NDZ2021
ORDER BY [period]; --Atceries, ka te vēl viss gads ir kopā.
