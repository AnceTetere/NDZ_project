-- Tabula v2_NDZ2021 izslēdz v1_NDZ2021 dubultniekus

--1) Izveido tabulu v2_NDZ2021
CREATE TABLE v2_NDZ2021 (
	[period] VARCHAR(6),
	NDZ_sanemsanas_datums DATE,
	PS_code VARCHAR(11),
	dnperk VARCHAR(11),
	NM_code VARCHAR(11),
	zinkod VARCHAR(2),
	prof VARCHAR(7));

--2) Pārbaudi atlasi
SELECT DISTINCT [period],
                NDZ_sanemsanas_datums, 
				PS_code,
				dnperk,
				NM_code,
				zinkod,
				prof
FROM v1_NDZ2021
ORDER BY [period]; 

--3) No tabulas v1_NDZ2021 pārnes DISTINCT datus uz v2_NDZ2021
INSERT INTO v2_NDZ2021([period], PS_code, dnperk, NM_code, zinkod, prof, NDZ_sanemsanas_datums)
SELECT DISTINCT [period], PS_code, dnperk, NM_code, zinkod, prof, NDZ_sanemsanas_datums
FROM v1_NDZ2021; 
