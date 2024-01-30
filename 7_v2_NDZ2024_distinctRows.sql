-- Izveidojam tabulu v2_NDZ_2024, kas izslēdz no v1_NDZ_2024 dubultniekus

--1) Izveido tabulu v2_NDZ2024
CREATE TABLE v2_NDZ2024 (
    periodT varchar(6),
    NDZ_sanemsanas_datums date,
    rs_kod varchar(11),
	ds_kod varchar(11),
	rn_kod varchar(11),
	z_kod varchar(2),
	prof varchar(7)
);

--2) Pārbaudi atlasi
SELECT DISTINCT
    [periodT],
    [NDZ_sanemsanas_datums],
    [rs_kod],
	[ds_kod],
	[rn_kod],
	[z_kod],
	[prof]
FROM[dbo].[v1_NDZ2024]
ORDER BY periodT; --1 271 279 rows

--3) No tabulas v1_NDZ2024 pārnes DISTINCT datus uz v2_NDZ2024
INSERT INTO v2_NDZ2024 (periodT, NDZ_sanemsanas_datums, rs_kod, ds_kod, rn_kod, z_kod, prof)
SELECT DISTINCT
    [periodT],
    [NDZ_sanemsanas_datums],
    [rs_kod],
	[ds_kod],
	[rn_kod],
	[z_kod],
	[prof]
FROM[dbo].[v1_NDZ2024];    -- tabulai v1_NDZ2024 ir 1 271 279 rindas

/*
 Uz savienošanu pārsaucam aili tabulas [v1_NDZ2024] aili sanemsanas_datums par [NDZ_sanemsanas_datums], jo citādi I555 tabulā nesapratīs, 
 kas tas par datumu un no korienes tas nāk.
---- Pēc savienošanas nomaini atpakaļ 
	EXEC sp_rename 'v1_NDZ2024.sanemsanas_datums', 'NDZ_sanemsanas_datums', 'COLUMN';
*/

